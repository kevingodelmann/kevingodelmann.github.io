#!/usr/bin/env Rscript
# ===========================================================================
# Fly-brain bridge server v5
#
# v4 was anatomically real but computationally hollow. Four things were wrong,
# and this version fixes each:
#
#   1. INPUT WAS ONE BIT. All 200 LC10 cells got the same scalar, and the only
#      asymmetry was hemisphere, so target position collapsed to sign(x) before
#      reaching a synapse. Now each visual neuron has a real receptive field
#      (from its dendritic centroid in the lobula -- see
#      build_lc10_receptive_fields.R) and is driven by a Gaussian on the
#      distance from its RF centre to the target. The population response is
#      genuinely graded and the real synaptic weights have something to compute
#      on. Measured: synaptic weight covaries with RF azimuth
#      (Spearman rho = -0.45, p = 0.009 for DNa10_R), so this is signal, not
#      decoration.
#
#   2. OUTPUT WAS SATURATED ~10x. turn*1.2 was hard-clipped to 1, so every bit
#      of weight structure was crushed and the controller was bang-bang. Now
#      the drive is calibrated at startup against its own dynamic range and
#      passed through tanh, which compresses smoothly instead of clipping.
#
#   3. LEARNING WAS A NO-OP. It multiplied an already-saturated value, so it
#      provably could not change behaviour. Now the mushroom body SELECTS THE
#      ACTION -- which stroke to play and where to place it -- which is what
#      MBONs actually do. Learning changes what the fly does, not how hard it
#      pushes a clipped number.
#
#   4. WEIGHTS PINNED AT THEIR LIMITS. No decay term meant every exercised bin
#      sat at its ceiling or floor permanently (measured: aversive weights at
#      exactly 0.0, appetitive at the 3.0 cap), so nothing could be learned
#      after the first few minutes. Now plasticity is depression-only in both
#      directions (which is the actual biology) and is balanced by homeostatic
#      recovery toward baseline, so the fly can track a changing opponent.
#
# Also fixed: context_bin's `urg_level` term made half of all bins structurally
# unreachable, because the callers always sent urgency > 0.5.
# ===========================================================================

.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(httpuv))
suppressMessages(library(jsonlite))

ROOT <- "C:/Users/kevin/Documents/Flybrain"

# ---------------------------------------------------------------------------
# Circuit loading (unchanged topology, now carrying receptive fields)
# ---------------------------------------------------------------------------

# Real L/R synapse counts for a DN pair are rarely symmetric (DNp10_R's
# total_out came out ~2.4x DNp10_L's here). Used directly, that anatomical
# magnitude imbalance becomes a fixed steering bias regardless of input. The
# pair's combined strength is real and worth keeping as a gain; which side is
# "stronger" is reconstruction noise. So both sides get the same magnitude and
# direction comes purely from the input-driven differential between the eyes.
normalize_by_side_pair <- function(total_out, type) ave(total_out, type, FUN = sum)

build_matrix <- function(e, rows, cols) {
  M <- matrix(0, nrow = length(rows), ncol = length(cols),
              dimnames = list(as.character(rows), as.character(cols)))
  for (k in seq_len(nrow(e))) {
    fr <- as.character(e$from[k]); to <- as.character(e$to[k])
    if (fr %in% rownames(M) && to %in% colnames(M)) M[fr, to] <- e$weight[k]
  }
  cs <- colSums(M); cs[cs == 0] <- 1
  sweep(M, 2, cs, "/")
}

motor_targets <- function(motor_meta, T_dn_mn) {
  is_ant  <- grepl("anterior|extensor|flight|^b[0-9] MN$", motor_meta$type, ignore.case = TRUE)
  is_post <- grepl("posterior|flexor", motor_meta$type, ignore.case = TRUE)
  rowSums(T_dn_mn[, is_ant, drop = FALSE]) - rowSums(T_dn_mn[, is_post, drop = FALSE])
}

load_motor_circuit <- function(path) {
  c_ <- fromJSON(path)
  lc_ids <- c_$lc_meta$bodyid; dn_ids <- c_$dn_meta$bodyid
  in_ids <- unique(c_$interneuron_ids); mn_ids <- c_$motor_meta$bodyid
  W0 <- build_matrix(c_$edges[c_$edges$layer == "lc_to_dn", ], lc_ids, dn_ids)
  W1 <- build_matrix(c_$edges[c_$edges$layer == "dn_to_in", ], dn_ids, in_ids)
  W2 <- build_matrix(c_$edges[c_$edges$layer == "in_to_mn", ], in_ids, mn_ids)
  T_dn_mn <- W1 %*% W2
  list(lc_meta = c_$lc_meta, dn_meta = c_$dn_meta, W0 = W0,
       total_out = normalize_by_side_pair(rowSums(T_dn_mn), c_$dn_meta$type),
       side_sign = ifelse(c_$dn_meta$side == "R", 1, -1),
       ap_out = motor_targets(c_$motor_meta, T_dn_mn),
       lc_side_sign = ifelse(c_$lc_meta$side == "R", 1, -1))
}

load_dn_circuit <- function(path) {
  c_ <- fromJSON(path)
  dn_ids <- c_$dn_meta$bodyid
  in_ids <- unique(c_$interneuron_ids); mn_ids <- c_$motor_meta$bodyid
  W1 <- build_matrix(c_$edges[c_$edges$layer == "dn_to_in", ], dn_ids, in_ids)
  W2 <- build_matrix(c_$edges[c_$edges$layer == "in_to_mn", ], in_ids, mn_ids)
  T_dn_mn <- W1 %*% W2
  list(dn_meta = c_$dn_meta,
       total_out = normalize_by_side_pair(rowSums(T_dn_mn), c_$dn_meta$type),
       side_sign = ifelse(c_$dn_meta$side == "R", 1, -1),
       ap_out = motor_targets(c_$motor_meta, T_dn_mn))
}

load_mb_circuit <- function(path) {
  mb <- fromJSON(path)
  kc_ids <- mb$kc_meta$bodyid; mbon_ids <- mb$mbon_meta$bodyid
  W <- matrix(0, nrow = length(kc_ids), ncol = length(mbon_ids),
              dimnames = list(as.character(kc_ids), as.character(mbon_ids)))
  for (k in seq_len(nrow(mb$edges))) {
    fr <- as.character(mb$edges$from[k]); to <- as.character(mb$edges$to[k])
    if (fr %in% rownames(W) && to %in% colnames(W)) W[fr, to] <- mb$edges$weight[k]
  }
  list(kc_ids = kc_ids, mbon_ids = mbon_ids, W_init = W, W = W)
}

esc  <- load_motor_circuit(file.path(ROOT, "circuit_v2_rf.json"))
app  <- load_motor_circuit(file.path(ROOT, "circuit_appetitive_rf.json"))
idle <- load_dn_circuit(file.path(ROOT, "circuit_idle.json"))
idle_t0 <- Sys.time()

# MBON01 is the approach channel, MBON11 the avoidance channel.
#
# These use the full 1342-cell KCg-m population rather than v4's 300-cell
# sample. With 81 (context, action) conjunctions to keep apart, a 300-cell pool
# made the random sparse ensembles overlap so heavily that training one
# conjunction bled into its neighbours and learning showed up as a weak global
# drift instead of a decision. Separation here comes from the real population
# size, which is what it comes from in the fly.
mb_app <- load_mb_circuit(file.path(ROOT, "mb_circuit_appetitive_large.json"))
mb_esc <- load_mb_circuit(file.path(ROOT, "mb_circuit_large.json"))

# ===========================================================================
# 1. GRADED RETINAL INPUT
# ===========================================================================
#
# Each eye covers a hemifield, overlapping in a binocular zone straight ahead.
# A neuron's preferred azimuth is its RF position mapped into its own eye's
# hemifield; its drive is a Gaussian on the angular distance to the target.
#
# This is what makes the output graded AND what produces steering: direction
# comes from the interocular difference in drive (the eye that sees the target
# drives its DNs harder), which is how fly course control actually works. We
# verified that azimuth alone cannot do it -- DNa10_L and DNa10_R sample
# statistically indistinguishable azimuth ranges (Wilcoxon p = 0.37) -- so the
# binocular comparison is doing real work here, not decoration.

EYE_OFFSET <- 0.45   # hemifield centre, in normalised visual-field units
EYE_HALF   <- 0.75   # how far each eye's RFs spread around that centre
SIGMA_AZ   <- 0.45   # RF tuning width, azimuth
SIGMA_EL   <- 0.70   # RF tuning width, elevation (LC cells are broader here)

retinal_drive <- function(circ, tx, ty, urgency) {
  lc <- circ$lc_meta
  pref_az <- circ$lc_side_sign * EYE_OFFSET + lc$rf_az * EYE_HALF
  pref_el <- lc$rf_el * 0.8
  urgency * exp(-((pref_az - tx)^2) / (2 * SIGMA_AZ^2)) *
            exp(-((pref_el - ty)^2) / (2 * SIGMA_EL^2))
}

# `turn_types` restricts steering to DN pairs verified to differentiate left
# from right. DNp10_L receives zero LC10 drive in this subgraph -- a
# topological gap in the extraction, not something magnitude normalisation can
# repair -- so including it would reintroduce a fixed one-sided bias.
circuit_drive <- function(circ, tx, ty, urgency, turn_types = NULL) {
  d_lc <- retinal_drive(circ, tx, ty, urgency)
  d_dn <- as.vector(d_lc %*% circ$W0)
  names(d_dn) <- paste0(circ$dn_meta$type, "_", circ$dn_meta$side)
  mask <- if (is.null(turn_types)) rep(TRUE, length(d_dn)) else circ$dn_meta$type %in% turn_types
  list(turn = sum(circ$side_sign[mask] * d_dn[mask] * circ$total_out[mask]),
       fwdback = sum(d_dn * circ$ap_out),
       d_dn = d_dn)
}

# ===========================================================================
# 2. CALIBRATED, UNSATURATED OUTPUT
# ===========================================================================
#
# Sweep the full input range once at startup and measure the drive's actual
# dynamic range, then scale so a typical target lands in the responsive part of
# tanh. tanh compresses smoothly rather than clipping, so extreme inputs still
# differ from moderate ones instead of both pinning at 1.

calibrate <- function(circ, turn_types = NULL) {
  grid <- seq(-1, 1, by = 0.05)
  tv <- sapply(grid, function(v) circuit_drive(circ, v, 0, 1, turn_types)$turn)
  fv <- sapply(grid, function(v) circuit_drive(circ, 0, v, 1, turn_types)$fwdback)
  list(turn = max(abs(tv)), fwd = max(abs(fv)), profile = tv)
}

cal_app <- calibrate(app, turn_types = "DNa10")
cal_esc <- calibrate(esc)

# Scale so peak drive maps to tanh(1.6) ~ 0.92: near-full deflection at the
# edge of the visual field, but still strictly increasing all the way there.
TANH_PEAK <- 1.6
scale_turn_app <- TANH_PEAK / max(1e-9, cal_app$turn)
scale_turn_esc <- TANH_PEAK / max(1e-9, cal_esc$turn)
scale_fwd_app  <- TANH_PEAK / max(1e-9, cal_app$fwd)
scale_fwd_esc  <- TANH_PEAK / max(1e-9, cal_esc$fwd)

# ===========================================================================
# 3. THE MUSHROOM BODY SELECTS THE ACTION
# ===========================================================================
#
# In the fly, KCs encode the sensory context as a sparse code, and the MBONs
# read it out as a valence that biases action selection; dopamine adjusts the
# KC->MBON synapse for whichever KCs were active. So the natural way to make
# learning behaviourally real is to evaluate each candidate action in the
# current context and let the MBON readout choose.
#
# The KC code is therefore over (context, action) conjunctions. An efference
# copy of the action under consideration is a real feature of MB models --
# the fly evaluates "what if I did this" -- and it is what lets a valence
# signal pick between options rather than just scale one.

STROKES    <- c("drive", "loop", "push")
PLACEMENTS <- c("left", "centre", "right")
ACTIONS <- expand.grid(stroke = STROKES, placement = PLACEMENTS, stringsAsFactors = FALSE)
N_ACTIONS <- nrow(ACTIONS)

# Context: how far the ball is off to the side x how high it is arriving.
#
# The second axis was originally the incoming spin, which turned out to be a
# dead dimension: the opponent always plays the same topspin drive, so `spin`
# never left one bucket and 6 of 9 contexts were unreachable -- the same class
# of bug as v4's `urg_level`, which made half its bins unreachable for exactly
# the same reason. Arrival height genuinely varies AND is what should decide
# between lifting a loop and pushing under the ball, so it is both reachable
# and relevant. Verified reachable in play rather than assumed this time.
N_XSEC <- 3; N_HEIGHT <- 3
N_CONTEXT <- N_XSEC * N_HEIGHT

context_id <- function(x, height) {
  xs <- if (x < -0.33) 1 else if (x > 0.33) 3 else 2
  hs <- if (height < -0.33) 1 else if (height > 0.33) 3 else 2
  (xs - 1) * N_HEIGHT + hs
}

N_CELLS <- N_CONTEXT * N_ACTIONS
cell_id <- function(ctx, act) (ctx - 1) * N_ACTIONS + act

# Sparse random KC subsets. Random PN->KC connectivity is the one genuinely
# principled abstraction here: the fly's KC odour/context code really is a
# random sparse expansion, not something the connectome dictates per stimulus.
make_kc_sets <- function(n_kc, n_sets, seed, active_frac = 0.05) {
  n_active <- max(2, round(active_frac * n_kc))
  lapply(seq_len(n_sets), function(i) { set.seed(seed + i); sample(seq_len(n_kc), n_active) })
}
kc_sets_app <- make_kc_sets(length(mb_app$kc_ids), N_CELLS, 5000)
kc_sets_esc <- make_kc_sets(length(mb_esc$kc_ids), N_CELLS, 1000)

# Each ensemble is normalised against ITS OWN untrained strength, so every
# channel reads 1.0 before learning and decays toward 0 as it is depressed.
# Normalising against the whole population instead (as v4 did) made the value
# scale both tiny and dependent on how many KCs happened to be sampled, which
# left the softmax temperature meaning nothing in particular.
base_app <- vapply(kc_sets_app, function(s) sum(colSums(mb_app$W_init[s, , drop = FALSE])), numeric(1))
base_esc <- vapply(kc_sets_esc, function(s) sum(colSums(mb_esc$W_init[s, , drop = FALSE])), numeric(1))
base_app[base_app == 0] <- 1
base_esc[base_esc == 0] <- 1

approach_drive <- function(cell) sum(mb_app$W[kc_sets_app[[cell]], , drop = FALSE]) / base_app[cell]
avoid_drive    <- function(cell) sum(mb_esc$W[kc_sets_esc[[cell]], , drop = FALSE]) / base_esc[cell]

# Both channels start at 1, so an untrained value is 0 and no action is
# favoured. Range is [-1, +1]: +1 means "pure approach, never punished here".
action_value <- function(ctx, act) {
  cell <- cell_id(ctx, act)
  approach_drive(cell) - avoid_drive(cell)
}

# Softmax exploration: the MBON readout is noisy, not an argmax. At this
# temperature a value gap of ~0.2 gives the better action roughly 5:1 odds,
# which keeps the fly exploring instead of locking onto its first success.
TEMPERATURE <- 0.12

select_action <- function(ctx) {
  vals <- vapply(seq_len(N_ACTIONS), function(a) action_value(ctx, a), numeric(1))
  p <- exp((vals - max(vals)) / TEMPERATURE); p <- p / sum(p)
  a <- sample(seq_len(N_ACTIONS), 1, prob = p)
  list(action = a, values = vals, probs = p,
       stroke = ACTIONS$stroke[a], placement = ACTIONS$placement[a])
}

# ===========================================================================
# 4. DEPRESSION-ONLY PLASTICITY WITH HOMEOSTATIC RECOVERY
# ===========================================================================
#
# The canonical fly result (Aso, Hige, Cohn) is that dopamine coincident with
# KC activity DEPRESSES that KC->MBON synapse. Reward depresses the avoidance
# channel; punishment depresses the approach channel. Both directions of
# learning therefore come from the same, real, one-signed rule -- v4's "LTP on
# reward with an arbitrary 3.0 ceiling" was neither.
#
# Recovery toward baseline is what stops the weights pinning at zero and lets
# the fly re-learn when the opponent changes. Without it, learning is a
# one-way trip -- which is exactly what the v4 logs showed.

ETA <- 0.20   # depression per dopamine event
RHO <- 0.02   # homeostatic recovery per event, toward W_init

recover <- function() {
  mb_app$W <<- mb_app$W + RHO * (mb_app$W_init - mb_app$W)
  mb_esc$W <<- mb_esc$W + RHO * (mb_esc$W_init - mb_esc$W)
}

last_decision <- NULL
last_choice <- list(stroke = "drive", placement = "centre", ctx = 0, value = 0)
hits <- matrix(0L, N_CONTEXT, N_ACTIONS)
rewards <- matrix(0L, N_CONTEXT, N_ACTIONS)

# Frozen-weights control. The point of an A/B here is that it is now capable of
# telling us something: in v4 a frozen control was indistinguishable from the
# learning condition BY CONSTRUCTION, because learning multiplied a saturated
# output and could not change behaviour either way.
plasticity_on <- TRUE

reset_memory <- function() {
  mb_app$W <<- mb_app$W_init
  mb_esc$W <<- mb_esc$W_init
  hits <<- matrix(0L, N_CONTEXT, N_ACTIONS)
  rewards <<- matrix(0L, N_CONTEXT, N_ACTIONS)
  last_decision <<- NULL
}

# `strength` scales the depression, so the same synapse can be taught by two
# reinforcement timescales: a dense, immediate, well-attributed signal (did the
# stroke I chose actually land?) and a sparse, delayed, goal-aligned one (did I
# go on to win the point?). Neither alone is enough -- immediate-only optimises
# for the safest stroke, which lands every time and wins nothing, while
# delayed-only blames a stroke for a rally decided several shots later.
# Multiple dopaminergic timescales converging on one compartment is a real
# feature of the mushroom body, not a convenience.
reinforce <- function(won, strength = 1) {
  if (is.null(last_decision)) return(NULL)
  strength <- max(0, min(1, strength))
  eta <- ETA * strength
  if (!plasticity_on) {
    # Still record what happened, so the control arm's statistics are
    # comparable -- just don't let it change any synapse.
    hits[last_decision$ctx, last_decision$action] <<-
      hits[last_decision$ctx, last_decision$action] + 1L
    if (won) rewards[last_decision$ctx, last_decision$action] <<-
      rewards[last_decision$ctx, last_decision$action] + 1L
    return(list(frozen = TRUE, won = won))
  }
  ctx <- last_decision$ctx; act <- last_decision$action
  cell <- cell_id(ctx, act)
  before <- action_value(ctx, act)
  if (won) {
    # PAM reward dopamine depresses the AVOIDANCE channel, leaving approach
    # dominant for this context+action.
    mb_esc$W[kc_sets_esc[[cell]], ] <<- mb_esc$W[kc_sets_esc[[cell]], ] * (1 - eta)
    rewards[ctx, act] <<- rewards[ctx, act] + 1L
  } else {
    # PPL1 punishment dopamine depresses the APPROACH channel.
    mb_app$W[kc_sets_app[[cell]], ] <<- mb_app$W[kc_sets_app[[cell]], ] * (1 - eta)
  }
  hits[ctx, act] <<- hits[ctx, act] + 1L
  recover()
  after <- action_value(ctx, act)
  log_event(if (won) "reward" else "punish", ctx, act, before, after)
  list(ctx = ctx, action = act, stroke = ACTIONS$stroke[act],
       placement = ACTIONS$placement[act], value_before = before, value_after = after)
}

# ---------------------------------------------------------------------------
# Motor output
# ---------------------------------------------------------------------------

last_state <- list()

compute_move <- function(ex, ey, urgency, gx, gy, gurgency) {
  urgency <- max(0, min(1, urgency)); gurgency <- max(0, min(1, gurgency))

  e <- circuit_drive(esc, ex, ey, urgency)
  a <- circuit_drive(app, gx, gy, gurgency, turn_types = "DNa10")

  # Idle/exploratory walking (DNg13): a real DN->interneuron->motor pathway
  # driven by an internal rhythm rather than vision, standing in for
  # spontaneous locomotion during rest -- a real, documented phenomenon.
  t_idle <- as.numeric(difftime(Sys.time(), idle_t0, units = "secs"))
  w_idle <- 2 * pi / 5.5
  d_idle <- c((sin(w_idle * t_idle) + 1) * 0.5, (sin(w_idle * t_idle + pi) + 1) * 0.5) * 0.35
  idle_turn <- sum(idle$side_sign * d_idle * idle$total_out)
  idle_fwd  <- sum(d_idle * idle$ap_out) + 0.15

  gate_esc  <- 1 - urgency                      # pursuit yields to a real threat
  gate_idle <- (1 - urgency) * (1 - gurgency)   # idle only when nothing else is up

  # Escape steers AWAY from the looming stimulus; pursuit steers TOWARD the
  # target. That difference is circuit identity (DNp01's giant-fibre escape vs
  # DNa10's smooth pursuit), not a fudge factor -- but the sign still has to be
  # stated somewhere, and this is it.
  turn <- -e$turn * scale_turn_esc +
           a$turn * scale_turn_app * gate_esc +
           idle_turn * gate_idle * 0.3
  fwd  <- -e$fwdback * scale_fwd_esc +
           a$fwdback * scale_fwd_app * gate_esc +
           idle_fwd * gate_idle * 0.3

  mx <- tanh(turn); my <- tanh(-fwd)

  last_state <<- list(ex = ex, ey = ey, urgency = urgency,
                      gx = gx, gy = gy, gurgency = gurgency,
                      esc_turn = e$turn, esc_fwd = e$fwdback,
                      app_turn = a$turn, app_fwd = a$fwdback,
                      idle_turn = idle_turn, idle_fwd = idle_fwd, idle_gate = gate_idle,
                      turn_raw = turn, mx = mx, my = my,
                      d_dn_esc = as.list(e$d_dn), d_dn_app = as.list(a$d_dn),
                      d_dn_idle = list(DNg13_L = d_idle[1], DNg13_R = d_idle[2]))
  list(x = mx, y = my)
}

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------

LOG_PATH <- file.path(ROOT, "learning_log_v5.csv")
t0 <- Sys.time()
if (file.exists(LOG_PATH)) file.remove(LOG_PATH)
write.csv(data.frame(t = numeric(0), kind = character(0), ctx = integer(0),
                     action = integer(0), value_before = numeric(0),
                     value_after = numeric(0)), LOG_PATH, row.names = FALSE)

log_event <- function(kind, ctx, act, before, after) {
  row <- data.frame(t = as.numeric(difftime(Sys.time(), t0, units = "secs")),
                    kind = kind, ctx = ctx, action = act,
                    value_before = before, value_after = after)
  write.table(row, LOG_PATH, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
}

# ---------------------------------------------------------------------------
# HTTP
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a
num <- function(params, k, d = "0") as.numeric(params[[k]] %||% d)

parse_qs <- function(qs) {
  if (!nzchar(qs)) return(list())
  qs <- sub("^\\?", "", qs)
  out <- list()
  for (p in strsplit(qs, "&")[[1]]) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    if (length(kv) == 2) out[[utils::URLdecode(kv[1])]] <- utils::URLdecode(kv[2])
  }
  out
}

json_ok <- function(x, headers) list(status = 200L, headers = headers,
                                     body = toJSON(x, auto_unbox = TRUE, digits = 6))

httpd_app <- list(call = function(req) {
  path <- req$PATH_INFO
  params <- parse_qs(req$QUERY_STRING)
  headers <- list("Content-Type" = "application/json", "Access-Control-Allow-Origin" = "*")
  if (identical(req$REQUEST_METHOD, "OPTIONS"))
    return(list(status = 200L, headers = c(headers, list("Access-Control-Allow-Headers" = "*")), body = ""))

  if (path == "/move") {
    mv <- compute_move(num(params, "ex"), num(params, "ey"), num(params, "urgency"),
                       num(params, "gx"), num(params, "gy"), num(params, "gurgency"))
    return(json_ok(mv, headers))
  }

  # Steering AND action choice in one round trip, so the game needs a single
  # request per decision point.
  if (path == "/decide") {
    tx <- num(params, "tx"); ty <- num(params, "ty")
    urg <- num(params, "urgency", "1")
    mv <- compute_move(0, 0, 0, tx, ty, urg)
    ctx <- context_id(tx, ty)
    sel <- select_action(ctx)
    last_decision <<- list(ctx = ctx, action = sel$action)
    # Surfaced through /brain_state so the dashboard can show what the
    # mushroom body just chose, and how strongly it preferred it.
    last_choice <<- list(stroke = sel$stroke, placement = sel$placement,
                         ctx = ctx, value = sel$values[sel$action])
    return(json_ok(list(x = mv$x, y = mv$y, ctx = ctx, action = sel$action,
                        stroke = sel$stroke, placement = sel$placement,
                        values = sel$values, probs = sel$probs,
                        value = sel$values[sel$action]), headers))
  }

  if (path == "/outcome") {
    won <- num(params, "won") > 0.5
    r <- reinforce(won, num(params, "strength", "1"))
    return(json_ok(if (is.null(r)) list(ok = FALSE, reason = "no decision pending") else r, headers))
  }

  if (path == "/policy") {
    V <- matrix(0, N_CONTEXT, N_ACTIONS)
    for (c_ in seq_len(N_CONTEXT)) for (a_ in seq_len(N_ACTIONS)) V[c_, a_] <- action_value(c_, a_)
    return(json_ok(list(values = V, hits = hits, rewards = rewards,
                        strokes = ACTIONS$stroke, placements = ACTIONS$placement,
                        n_context = N_CONTEXT, n_actions = N_ACTIONS), headers))
  }

  if (path == "/reset") { reset_memory(); return(json_ok(list(ok = TRUE), headers)) }
  if (path == "/plasticity") {
    plasticity_on <<- num(params, "on", "1") > 0.5
    return(json_ok(list(plasticity = plasticity_on), headers))
  }
  if (path == "/brain_state")  return(json_ok(c(last_state, list(choice = last_choice)), headers))
  if (path == "/calibration")  return(json_ok(list(
      turn_peak_app = cal_app$turn, turn_peak_esc = cal_esc$turn,
      scale_turn_app = scale_turn_app, scale_turn_esc = scale_turn_esc,
      turn_profile_app = cal_app$profile), headers))
  if (path == "/health") return(list(status = 200L, headers = headers, body = '{"ok":true,"version":5}'))
  list(status = 404L, headers = headers, body = '{"error":"not found"}')
})

cat("fly-brain bridge v5 on http://0.0.0.0:8723\n")
cat("  graded retinal input:", nrow(app$lc_meta), "LC10 +", nrow(esc$lc_meta), "LC4/LC6 with real RFs\n")
cat("  turn drive peak (pursuit):", round(cal_app$turn, 4), "-> scale", round(scale_turn_app, 2), "\n")
cat("  MB policy:", N_CONTEXT, "contexts x", N_ACTIONS, "actions =", N_CELLS, "KC ensembles\n")
runServer("0.0.0.0", 8723, httpd_app)

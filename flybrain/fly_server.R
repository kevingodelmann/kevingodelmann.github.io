.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(httpuv))
suppressMessages(library(jsonlite))

# Real L/R synapse counts for a DN pair are rarely symmetric (e.g. DNp10_R's
# total_out came out ~2.4x DNp10_L's in this reconstruction). Used directly
# with side_sign, that anatomical magnitude imbalance becomes a fixed bias
# that steers one direction almost regardless of input. The pair's combined
# strength is real and worth keeping as an overall gain, but which side is
# "stronger" is reconstruction noise, not signal — so both sides of a pair
# are given the SAME magnitude (their sum). Direction then comes purely from
# the input-driven differential between d_dn_L and d_dn_R (via ipsi_boost).
normalize_by_side_pair <- function(total_out, type) {
  ave(total_out, type, FUN = sum)
}

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

load_motor_circuit <- function(path) {
  c_ <- fromJSON(path)
  lc_ids <- c_$lc_meta$bodyid; dn_ids <- c_$dn_meta$bodyid
  in_ids <- unique(c_$interneuron_ids); mn_ids <- c_$motor_meta$bodyid
  e0 <- c_$edges[c_$edges$layer == "lc_to_dn", ]
  e1 <- c_$edges[c_$edges$layer == "dn_to_in", ]
  e2 <- c_$edges[c_$edges$layer == "in_to_mn", ]
  W0 <- build_matrix(e0, lc_ids, dn_ids)
  W1 <- build_matrix(e1, dn_ids, in_ids)
  W2 <- build_matrix(e2, in_ids, mn_ids)
  T_dn_mn <- W1 %*% W2
  total_out <- normalize_by_side_pair(rowSums(T_dn_mn), c_$dn_meta$type)
  side_sign <- ifelse(c_$dn_meta$side == "R", 1, -1)
  is_ant  <- grepl("anterior|extensor|flight|^b[0-9] MN$", c_$motor_meta$type, ignore.case = TRUE)
  is_post <- grepl("posterior|flexor", c_$motor_meta$type, ignore.case = TRUE)
  ap_out <- rowSums(T_dn_mn[, is_ant, drop = FALSE]) - rowSums(T_dn_mn[, is_post, drop = FALSE])
  lc_side_sign <- ifelse(c_$lc_meta$side == "R", 1, -1)
  list(lc_ids = lc_ids, dn_meta = c_$dn_meta, W0 = W0, total_out = total_out,
       side_sign = side_sign, ap_out = ap_out, lc_side_sign = lc_side_sign)
}

load_dn_circuit <- function(path) {
  c_ <- fromJSON(path)
  dn_ids <- c_$dn_meta$bodyid
  in_ids <- unique(c_$interneuron_ids); mn_ids <- c_$motor_meta$bodyid
  e1 <- c_$edges[c_$edges$layer == "dn_to_in", ]
  e2 <- c_$edges[c_$edges$layer == "in_to_mn", ]
  W1 <- build_matrix(e1, dn_ids, in_ids)
  W2 <- build_matrix(e2, in_ids, mn_ids)
  T_dn_mn <- W1 %*% W2
  total_out <- normalize_by_side_pair(rowSums(T_dn_mn), c_$dn_meta$type)
  side_sign <- ifelse(c_$dn_meta$side == "R", 1, -1)
  is_ant  <- grepl("anterior|extensor|flight|^b[0-9] MN$", c_$motor_meta$type, ignore.case = TRUE)
  is_post <- grepl("posterior|flexor", c_$motor_meta$type, ignore.case = TRUE)
  ap_out <- rowSums(T_dn_mn[, is_ant, drop = FALSE]) - rowSums(T_dn_mn[, is_post, drop = FALSE])
  list(dn_meta = c_$dn_meta, total_out = total_out, side_sign = side_sign, ap_out = ap_out)
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

make_bin_tables <- function(n_kc, seed_offset, n_bins = 17, active_frac = 0.10) {
  n_active <- round(active_frac * n_kc)
  lapply(seq_len(n_bins), function(b) { set.seed(seed_offset + b); sample(seq_len(n_kc), n_active) })
}

context_bin <- function(x, y, urgency, n_bins = 17) {
  if (urgency < 0.05) return(n_bins)
  ang <- atan2(y, x)
  sector <- floor(((ang + pi) / (2 * pi)) * 8) %% 8
  urg_level <- if (urgency > 0.5) 1 else 0
  sector * 2 + urg_level + 1
}

# ---------------------------------------------------------------------------
# Escape (aversive): vision (LC4/LC6 looming) -> DNp01 etc -> ... -> motor
# Pursuit (appetitive): vision (LC10 small-target) -> DNa10/DNp10 -> ... -> motor
# ---------------------------------------------------------------------------
esc <- load_motor_circuit("C:/Users/kevin/Documents/Flybrain/circuit_v2.json")
app <- load_motor_circuit("C:/Users/kevin/Documents/Flybrain/circuit_appetitive.json")
idle <- load_dn_circuit("C:/Users/kevin/Documents/Flybrain/circuit_idle.json")
idle_t0 <- Sys.time()
mb_esc <- load_mb_circuit("C:/Users/kevin/Documents/Flybrain/mb_circuit.json")           # KC -> MBON11 (aversive brake)
mb_app <- load_mb_circuit("C:/Users/kevin/Documents/Flybrain/mb_circuit_appetitive.json") # KC -> MBON01 (appetitive drive)

ceiling_esc <- colSums(mb_esc$W_init); ceiling_esc[ceiling_esc == 0] <- 1
ceiling_app <- colSums(mb_app$W_init); ceiling_app[ceiling_app == 0] <- 1

N_BINS <- 17
bins_esc <- make_bin_tables(length(mb_esc$kc_ids), 1000)
bins_app <- make_bin_tables(length(mb_app$kc_ids), 5000)
hits_esc <- integer(N_BINS)
hits_app <- integer(N_BINS)

ETA_ESC <- 0.22   # LTD (depression) rate on damage
ETA_APP <- 0.28   # LTP (potentiation) rate on reward
APP_LTP_CEILING <- 3.0  # cap so appetitive weights can't grow unboundedly

last_bin_esc <- N_BINS
last_bin_app <- N_BINS
last_state <- list()

suppression_of <- function(bin) {
  active <- bins_esc[[bin]]
  sig <- colSums(mb_esc$W[active, , drop = FALSE])
  min(0.8, max(0, mean(sig / ceiling_esc) * 2.2))
}
potentiation_of <- function(bin) {
  active <- bins_app[[bin]]
  sig <- colSums(mb_app$W[active, , drop = FALSE])
  max(0, mean(sig / ceiling_app))   # unbounded-ish, scaled at use site
}

drive_from_lc <- function(circ, x, y, urgency, wander, turn_types = NULL) {
  d_lc <- numeric(length(circ$lc_ids))
  ipsi_boost <- 0.5 + 0.5 * (circ$lc_side_sign * sign(ifelse(x == 0, 0, -x)))
  d_lc[] <- wander + urgency * (0.6 + 0.4 * ipsi_boost)
  d_dn <- as.vector(d_lc %*% circ$W0)
  names(d_dn) <- paste0(circ$dn_meta$type, "_", circ$dn_meta$side)
  # Some DN pairs in a given subgraph sample receive essentially zero drive
  # on one side (a topological gap in that specific extracted subgraph, not
  # fixable by magnitude normalization) — that turns into a one-sided bias
  # that pushes a fixed direction regardless of input. turn_types lets the
  # caller restrict steering to DN pairs verified to actually differentiate
  # left vs right; all DNs still contribute to fwdback either way.
  turn_mask <- if (is.null(turn_types)) rep(TRUE, length(d_dn)) else circ$dn_meta$type %in% turn_types
  turn <- sum(circ$side_sign[turn_mask] * d_dn[turn_mask] * circ$total_out[turn_mask])
  fwdback <- sum(d_dn * circ$ap_out) + y * urgency * 0.5
  list(turn = turn, fwdback = fwdback, d_dn = d_dn)
}

compute_move <- function(ex, ey, urgency, gx, gy, gurgency) {
  urgency <- max(0, min(1, urgency)); gurgency <- max(0, min(1, gurgency))
  bin_e <- context_bin(ex, ey, urgency); last_bin_esc <<- bin_e
  bin_a <- context_bin(gx, gy, gurgency); last_bin_app <<- bin_a

  esc_d <- drive_from_lc(esc, ex, ey, urgency, wander = 0)
  suppression <- suppression_of(bin_e)
  esc_turn <- esc_d$turn * (1 - suppression)
  esc_fwd  <- esc_d$fwdback * (1 - suppression)

  app_d <- drive_from_lc(app, gx, gy, gurgency, wander = 0, turn_types = "DNa10")
  potentiation <- potentiation_of(bin_a)
  pursuit_gain <- min(2.5, 1 + potentiation * 4)   # learned reward strengthens pursuit
  # drive_from_lc's ipsi_boost convention was tuned for escape (move AWAY
  # from ex/ey); pursuit needs the opposite sense (move TOWARD gx/gy), so
  # its turn/fwd contribution is negated here.
  app_turn <- -app_d$turn * pursuit_gain
  app_fwd  <- -app_d$fwdback * pursuit_gain

  # Idle/exploratory walking (DNg13): a real DN->interneuron->motor pathway
  # driven by an internal rhythm rather than vision, standing in for
  # spontaneous locomotion during rest states (a real, well-documented
  # phenomenon) instead of a hand-coded constant "wander" nudge.
  t_idle <- as.numeric(difftime(Sys.time(), idle_t0, units = "secs"))
  w_idle <- 2 * pi / 5.5
  d_dn_idle <- c((sin(w_idle * t_idle) + 1) * 0.5, (sin(w_idle * t_idle + pi) + 1) * 0.5) * 0.35
  idle_turn <- sum(idle$side_sign * d_dn_idle * idle$total_out)
  idle_fwd  <- sum(d_dn_idle * idle$ap_out) + 0.15   # small constant forward bias

  gate_esc <- 1 - urgency        # pursuit backs off when an escape is actually urgent
  gate_idle <- (1 - urgency) * (1 - gurgency)   # idle only shows when nothing else is going on

  turn <- esc_turn + app_turn * gate_esc + idle_turn * gate_idle
  fwdback <- esc_fwd + app_fwd * gate_esc + idle_fwd * gate_idle

  mx <- turn * 1.2; my <- -fwdback * 1.2
  m <- sqrt(mx^2 + my^2)
  if (m > 1) { mx <- mx / m; my <- my / m }

  last_state <<- list(ex = ex, ey = ey, urgency = urgency, gx = gx, gy = gy, gurgency = gurgency,
                       esc_turn = esc_turn, esc_fwd = esc_fwd, app_turn = app_turn, app_fwd = app_fwd,
                       idle_turn = idle_turn, idle_fwd = idle_fwd, idle_gate = gate_idle,
                       suppression = suppression, potentiation = potentiation,
                       d_dn_esc = as.list(esc_d$d_dn), d_dn_app = as.list(app_d$d_dn),
                       d_dn_idle = list(DNg13_L = d_dn_idle[1], DNg13_R = d_dn_idle[2]),
                       mx = mx, my = my, bin_esc = bin_e, bin_app = bin_a)
  list(x = mx, y = my, suppression = suppression, potentiation = potentiation, bin = bin_e, bin_app = bin_a)
}

apply_damage <- function() {
  bin <- last_bin_esc
  before <- suppression_of(bin)
  active <- bins_esc[[bin]]
  mb_esc$W[active, ] <<- mb_esc$W[active, ] * (1 - ETA_ESC)
  hits_esc[bin] <<- hits_esc[bin] + 1
  after <- suppression_of(bin)
  log_event("damage", bin, hits_esc[bin], before, after)
  list(bin = bin, hits = hits_esc[bin], suppression_now = after)
}

apply_reward <- function() {
  bin <- last_bin_app
  before <- potentiation_of(bin)
  active <- bins_app[[bin]]
  mb_app$W[active, ] <<- pmin(mb_app$W_init[active, , drop=FALSE] * APP_LTP_CEILING,
                               mb_app$W[active, , drop = FALSE] * (1 + ETA_APP))
  hits_app[bin] <<- hits_app[bin] + 1
  after <- potentiation_of(bin)
  log_event("reward", bin, hits_app[bin], before, after)
  list(bin = bin, hits = hits_app[bin], potentiation_now = after)
}

LOG_PATH <- "C:/Users/kevin/Documents/Flybrain/learning_log.csv"
learning_log <- data.frame(t = numeric(0), kind = character(0), bin = integer(0), hits = integer(0),
                            value_before = numeric(0), value_after = numeric(0))
t0 <- Sys.time()
if (file.exists(LOG_PATH)) file.remove(LOG_PATH)
write.csv(learning_log, LOG_PATH, row.names = FALSE)

log_event <- function(kind, bin, hits, before, after) {
  row <- data.frame(t = as.numeric(difftime(Sys.time(), t0, units = "secs")), kind = kind,
                     bin = bin, hits = hits, value_before = before, value_after = after)
  learning_log <<- rbind(learning_log, row)
  write.table(row, LOG_PATH, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
}

RUN_LOG_PATH <- "C:/Users/kevin/Documents/Flybrain/run_log.csv"
run_log <- data.frame(t = numeric(0), survival_s = numeric(0), level = integer(0), kills = integer(0),
                       total_hits = integer(0), total_rewards = integer(0),
                       mean_suppression = numeric(0), mean_potentiation = numeric(0))
if (file.exists(RUN_LOG_PATH)) file.remove(RUN_LOG_PATH)
write.csv(run_log, RUN_LOG_PATH, row.names = FALSE)

log_run_end <- function(survival_s, level, kills) {
  mean_supp <- mean(sapply(seq_len(N_BINS), suppression_of))
  mean_pot  <- mean(sapply(seq_len(N_BINS), potentiation_of))
  row <- data.frame(t = as.numeric(difftime(Sys.time(), t0, units = "secs")),
                     survival_s = survival_s, level = level, kills = kills,
                     total_hits = sum(hits_esc), total_rewards = sum(hits_app),
                     mean_suppression = mean_supp, mean_potentiation = mean_pot)
  run_log <<- rbind(run_log, row)
  write.table(row, RUN_LOG_PATH, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  row
}

`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a
parse_qs <- function(qs) {
  if (!nzchar(qs)) return(list())
  qs <- sub("^\\?", "", qs)
  pairs <- strsplit(qs, "&")[[1]]
  out <- list()
  for (p in pairs) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    if (length(kv) == 2) out[[utils::URLdecode(kv[1])]] <- utils::URLdecode(kv[2])
  }
  out
}

httpd_app <- list(
  call = function(req) {
    path <- req$PATH_INFO
    params <- parse_qs(req$QUERY_STRING)
    headers <- list("Content-Type" = "application/json", "Access-Control-Allow-Origin" = "*")
    if (identical(req$REQUEST_METHOD, "OPTIONS")) {
      return(list(status = 200L, headers = c(headers, list("Access-Control-Allow-Headers" = "*")), body = ""))
    }
    if (path == "/move") {
      ex <- as.numeric(params$ex %||% "0"); ey <- as.numeric(params$ey %||% "0")
      urgency <- as.numeric(params$urgency %||% "0")
      gx <- as.numeric(params$gx %||% "0"); gy <- as.numeric(params$gy %||% "0")
      gurgency <- as.numeric(params$gurgency %||% "0")
      mv <- compute_move(ex, ey, urgency, gx, gy, gurgency)
      return(list(status = 200L, headers = headers, body = toJSON(mv, auto_unbox = TRUE)))
    }
    if (path == "/run_end") {
      survival_s <- as.numeric(params$t %||% "0"); level <- as.integer(params$level %||% "1")
      kills <- as.integer(params$kills %||% "0")
      return(list(status = 200L, headers = headers, body = toJSON(log_run_end(survival_s, level, kills), auto_unbox = TRUE)))
    }
    if (path == "/run_log") return(list(status = 200L, headers = headers, body = toJSON(run_log, dataframe = "rows")))
    if (path == "/damage") return(list(status = 200L, headers = headers, body = toJSON(apply_damage(), auto_unbox = TRUE)))
    if (path == "/reward") return(list(status = 200L, headers = headers, body = toJSON(apply_reward(), auto_unbox = TRUE)))
    if (path == "/brain_state") return(list(status = 200L, headers = headers, body = toJSON(last_state, auto_unbox = TRUE)))
    if (path == "/learning_log") return(list(status = 200L, headers = headers, body = toJSON(learning_log, dataframe = "rows")))
    if (path == "/learning_stats") {
      frac_esc <- sapply(seq_len(N_BINS), function(b) mean(colSums(mb_esc$W[bins_esc[[b]], , drop=FALSE]) / colSums(mb_esc$W_init[bins_esc[[b]], , drop=FALSE])))
      frac_app <- sapply(seq_len(N_BINS), function(b) mean(colSums(mb_app$W[bins_app[[b]], , drop=FALSE]) / colSums(mb_app$W_init[bins_app[[b]], , drop=FALSE])))
      return(list(status = 200L, headers = headers,
                  body = toJSON(list(hits_esc = hits_esc, remaining_fraction_esc = frac_esc,
                                      hits_app = hits_app, potentiation_fraction_app = frac_app), auto_unbox = TRUE)))
    }
    if (path == "/health") return(list(status = 200L, headers = headers, body = '{"ok":true}'))
    list(status = 404L, headers = headers, body = '{"error":"not found"}')
  }
)

cat("Starting fly-brain bridge server v4 (escape + pursuit + dual learning) on http://0.0.0.0:8721 ...\n")
cat("Escape DN:", paste(esc$dn_meta$type, esc$dn_meta$side, collapse=", "), "\n")
cat("Pursuit DN:", paste(app$dn_meta$type, app$dn_meta$side, collapse=", "), "\n")
runServer("0.0.0.0", 8721, httpd_app)

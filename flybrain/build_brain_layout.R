.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(malecns))
suppressMessages(library(neuprintr))
suppressMessages(library(jsonlite))

get_pos <- function(ids) {
  ids <- unique(ids)
  meta <- mcns_neuprint_meta(ids)
  cols <- colnames(meta)
  loc_col <- if ("somaLocation" %in% cols) "somaLocation" else if ("location" %in% cols) "location" else NA
  if (is.na(loc_col)) stop("no location column found: ", paste(cols, collapse=","))
  out <- data.frame(bodyid = meta$bodyid, loc = meta[[loc_col]])
  parts <- strsplit(as.character(out$loc), ",")
  out$x <- sapply(parts, function(p) as.numeric(p[1]))
  out$y <- sapply(parts, function(p) as.numeric(p[2]))
  out$z <- sapply(parts, function(p) as.numeric(p[3]))
  out[, c("bodyid","x","y","z")]
}

invisible(mcns_neuprint_meta("/DNp01$"))  # establishes the neuprint connection malecns manages

esc <- fromJSON("C:/Users/kevin/Documents/Flybrain/circuit_v2.json")
app <- fromJSON("C:/Users/kevin/Documents/Flybrain/circuit_appetitive.json")
mb_e <- fromJSON("C:/Users/kevin/Documents/Flybrain/mb_circuit.json")
mb_a <- fromJSON("C:/Users/kevin/Documents/Flybrain/mb_circuit_appetitive.json")

cat("Fetching soma positions for escape circuit...\n")
pos_lc_e <- get_pos(esc$lc_meta$bodyid)
pos_dn_e <- get_pos(esc$dn_meta$bodyid)
pos_in_e <- get_pos(esc$interneuron_ids)
pos_mn_e <- get_pos(esc$motor_meta$bodyid)

cat("Fetching soma positions for pursuit circuit...\n")
pos_lc_a <- get_pos(app$lc_meta$bodyid)
pos_dn_a <- get_pos(app$dn_meta$bodyid)
pos_in_a <- get_pos(app$interneuron_ids)
pos_mn_a <- get_pos(app$motor_meta$bodyid)

cat("Fetching soma positions for mushroom body (KC sampled, MBON)...\n")
set.seed(99)
kc_sample_e <- sample(mb_e$kc_meta$bodyid, 60)
kc_sample_a <- sample(mb_a$kc_meta$bodyid, 60)
pos_kc_e <- get_pos(kc_sample_e)
pos_kc_a <- get_pos(kc_sample_a)
pos_mbon_e <- get_pos(mb_e$mbon_meta$bodyid)
pos_mbon_a <- get_pos(mb_a$mbon_meta$bodyid)

cat("Fetching a broad neuron sample for the brain outline...\n")
broad <- mcns_neuprint_meta("/DNp01|DNp02|DNp03|DNp04|DNp11|DNa10|DNp10|LC4|LC6|LC10.*|KCg-m|MBON01|MBON11")
set.seed(1)
broad_sample <- broad[sample(nrow(broad), min(1500, nrow(broad))), ]
pos_broad <- get_pos(broad_sample$bodyid)

save_all <- list(
  lc_e = pos_lc_e, dn_e = pos_dn_e, in_e = pos_in_e, mn_e = pos_mn_e,
  lc_a = pos_lc_a, dn_a = pos_dn_a, in_a = pos_in_a, mn_a = pos_mn_a,
  kc_e = pos_kc_e, kc_a = pos_kc_a, mbon_e = pos_mbon_e, mbon_a = pos_mbon_a,
  outline_sample = pos_broad
)
saveRDS(save_all, "C:/Users/kevin/Documents/Flybrain/brain_positions_raw.rds")
cat("Saved raw positions.\n")
for (n in names(save_all)) cat(n, ":", nrow(save_all[[n]]), "\n")

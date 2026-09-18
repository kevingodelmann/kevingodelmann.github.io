#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Rebuild both mushroom-body circuits with a much larger Kenyon cell pool.
#
# The v4 circuits sampled 300 KCg-m. That was enough to demonstrate plasticity
# on a single context, but the v5 server needs the KC code to SEPARATE 81
# (context, action) conjunctions, and with only 300 cells the random sparse
# ensembles overlap so heavily that training one conjunction bleeds into its
# neighbours -- learning shows up as a weak global drift instead of a decision.
#
# The real mushroom body has ~2000 KCs per hemisphere precisely because sparse
# high-dimensional codes are what make many memories separable. Sampling closer
# to the true population size is therefore the principled fix, not a tuning
# knob: it buys separation from the biology rather than from a hyperparameter.
# ---------------------------------------------------------------------------

.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(malecns))
suppressMessages(library(neuprintr))
suppressMessages(library(jsonlite))

ROOT <- "C:/Users/kevin/Documents/Flybrain"
conn <- mcns_neuprint()

N_KC <- 1500

build <- function(mbon_pattern, dan_pattern, out, seed) {
  cat("\n===", out, "\n")
  set.seed(seed)
  kc_all <- mcns_neuprint_meta("/KCg-m")
  cat("KCg-m available:", nrow(kc_all), "\n")
  n <- min(N_KC, nrow(kc_all))
  kc <- kc_all[sample(nrow(kc_all), n), c("bodyid", "type")]
  cat("sampled:", nrow(kc), "\n")

  mbon <- mcns_neuprint_meta(mbon_pattern)[, c("bodyid", "type", "somaSide")]
  dan  <- mcns_neuprint_meta(dan_pattern)[, c("bodyid", "type", "somaSide")]
  cat("MBON:", nrow(mbon), " DAN:", nrow(dan), "\n")

  cat("Fetching KC -> MBON connections...\n")
  ct <- neuprint_connection_table(kc$bodyid, prepost = "POST", by.roi = FALSE, conn = conn)
  ct <- ct[ct$partner %in% mbon$bodyid, ]
  cat("KC->MBON edges:", nrow(ct),
      sprintf(" (%.0f%% of sampled KCs connect)\n",
              100 * length(unique(ct$bodyid)) / nrow(kc)))

  circuit <- list(kc_meta = kc, mbon_meta = mbon, dan_meta = dan,
                  edges = data.frame(from = ct$bodyid, to = ct$partner, weight = ct$weight))
  write_json(circuit, file.path(ROOT, out), auto_unbox = TRUE)
  cat("saved", out, "\n")
}

# MBON01 / PAM = the approach (reward) channel; MBON11 / PPL1 = avoidance.
build("/MBON01", "/PAM01|PAM02", "mb_circuit_appetitive_large.json", 43)
build("/MBON11", "/PPL101",      "mb_circuit_large.json",            44)
cat("\nDone.\n")

#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Give every visual projection neuron in the circuits a real receptive-field
# address, so the connectome has spatial information to actually compute on.
#
# Why this exists: the v4 server drove all 200 LC10 neurons with one identical
# scalar, so layer 0 carried no spatial information whatsoever and the target's
# position collapsed to a single bit (which hemisphere) before it touched a
# synapse. The real weights downstream then had nothing to compute on.
#
# LC neurons are retinotopic: each cell's dendrites sample one patch of the
# lobula, and the lobula preserves the eye's spatial map. So the centroid of a
# cell's INPUT synapses is a genuine proxy for where in the visual field it
# looks. That is what we extract here.
#
# We reuse the exact neuron set and edge list from the existing circuits, so
# the graph is unchanged -- this script only ADDS coordinates.
#
# Note on method: neuprint's `assignedOlHex1/2` columns would be the ideal
# retinotopic address, but they are unassigned (all NA) for LC neurons in this
# reconstruction -- they are populated for columnar cells like Tm/Mi. The
# dendritic centroid is the honest second-best.
# ---------------------------------------------------------------------------

.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(malecns))
suppressMessages(library(neuprintr))
suppressMessages(library(jsonlite))

ROOT <- "C:/Users/kevin/Documents/Flybrain"

# neuprintr needs an explicit connection; malecns supplies the configured one.
# (A bare neuprint_* call errors with "you must specify a neuprint server".)
conn <- mcns_neuprint()

build_rf <- function(IN, OUT) {
  circ <- fromJSON(file.path(ROOT, IN))
  lc <- circ$lc_meta
  cat("\n===", IN, "--", nrow(lc), "visual neurons\n")

  cat("Fetching synapse coordinates...\n")
  syn <- neuprint_get_synapses(lc$bodyid, conn = conn)

  # prepost == 1 is postsynaptic: the cell's INPUTS, i.e. its dendrites in the
  # lobula. Presynaptic terminals sit in the central brain and carry no
  # retinotopy, so including them would smear the map.
  dend <- syn[syn$prepost == 1, ]
  cat("Postsynaptic sites:", nrow(dend), "across",
      length(unique(dend$bodyid)), "neurons\n")

  centroid <- aggregate(cbind(x, y, z) ~ bodyid, data = dend, FUN = median)
  names(centroid) <- c("bodyid", "cx", "cy", "cz")
  lc <- merge(lc, centroid, by = "bodyid", all.x = TRUE)

  # A neuron with no recovered dendrite lands at the population centre, which
  # makes it an uninformative rather than a misleading unit.
  for (v in c("cx", "cy", "cz")) lc[[v]][is.na(lc[[v]])] <- median(lc[[v]], na.rm = TRUE)

  # -------------------------------------------------------------------------
  # Turn 3D centroids into a 2D visual-field map, per hemisphere.
  #
  # The lobula is a curved sheet, so its two directions of greatest spread ARE
  # its retinotopic axes. PCA per side recovers them without us assuming which
  # anatomical axis is azimuth. Rank-normalising to [-1,1] gives a uniform
  # tiling of the visual field regardless of how densely we happened to sample.
  # -------------------------------------------------------------------------
  lc$rf_az <- NA_real_
  lc$rf_el <- NA_real_
  for (sd in c("L", "R")) {
    idx <- which(lc$side == sd)
    if (length(idx) < 3) next
    pc <- prcomp(as.matrix(lc[idx, c("cx", "cy", "cz")]), center = TRUE, scale. = FALSE)
    rank01 <- function(v) (rank(v, ties.method = "average") - 0.5) / length(v)
    lc$rf_az[idx] <- rank01(pc$x[, 1]) * 2 - 1
    lc$rf_el[idx] <- rank01(pc$x[, 2]) * 2 - 1
    cat(sprintf("  side %s: n=%3d  PC1,PC2 explain %.0f%%, %.0f%% of variance\n",
                sd, length(idx),
                100 * pc$sdev[1]^2 / sum(pc$sdev^2),
                100 * pc$sdev[2]^2 / sum(pc$sdev^2)))
  }

  # PCA sign is arbitrary per side, so the two hemispheres could end up tiling
  # the visual field in opposite directions and cancelling. Anchor both to the
  # same anatomical axis (cx) so "increasing rf_az" means the same thing on
  # both sides, then mirror the left eye so azimuth is signed consistently in
  # world terms rather than in each eye's own frame.
  for (sd in c("L", "R")) {
    idx <- which(lc$side == sd)
    if (length(idx) < 3) next
    if (cor(lc$rf_az[idx], lc$cx[idx]) < 0) lc$rf_az[idx] <- -lc$rf_az[idx]
    if (cor(lc$rf_el[idx], lc$cz[idx]) < 0) lc$rf_el[idx] <- -lc$rf_el[idx]
  }

  circ$lc_meta <- lc
  write_json(circ, file.path(ROOT, OUT), auto_unbox = TRUE, digits = 6)
  cat(sprintf("  saved %s   rf_az %.2f..%.2f  rf_el %.2f..%.2f\n",
              OUT, min(lc$rf_az), max(lc$rf_az), min(lc$rf_el), max(lc$rf_el)))
}

build_rf("circuit_appetitive.json", "circuit_appetitive_rf.json")
build_rf("circuit_v2.json",         "circuit_v2_rf.json")
cat("\nDone.\n")

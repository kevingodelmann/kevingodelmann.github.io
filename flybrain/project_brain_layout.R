.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(jsonlite))

raw <- readRDS("C:/Users/kevin/Documents/Flybrain/brain_positions_raw.rds")
raw <- lapply(raw, function(d) d[stats::complete.cases(d[, c("x","y","z")]), ])

# Front view: x = left-right, z = dorsal-ventral (voxel space, z often inverted -> flip)
all_xz <- do.call(rbind, lapply(raw, function(d) d[, c("x","z")]))
xr <- range(all_xz$x); zr <- range(all_xz$z)

VW <- 460; VH <- 380  # target SVG viewport for the brain panel
pad <- 20
project <- function(d) {
  px <- pad + (d$x - xr[1]) / diff(xr) * (VW - 2*pad)
  pz <- pad + (d$z - zr[1]) / diff(zr) * (VH - 2*pad)
  data.frame(bodyid = d$bodyid, x = px, y = pz)
}

groups <- list()
for (n in setdiff(names(raw), "outline_sample")) groups[[n]] <- project(raw[[n]])

esc <- fromJSON("C:/Users/kevin/Documents/Flybrain/circuit_v2.json")
app <- fromJSON("C:/Users/kevin/Documents/Flybrain/circuit_appetitive.json")
attach_name <- function(df, meta) {
  m <- meta[match(df$bodyid, meta$bodyid), c("type","side")]
  cbind(df, name = paste0(m$type, "_", m$side))
}
groups$dn_e <- attach_name(groups$dn_e, esc$dn_meta)
groups$dn_a <- attach_name(groups$dn_a, app$dn_meta)

# Brain outline: convex hull of the broad sample, in the same projection
bp <- project(raw$outline_sample)
hull_idx <- chull(bp$x, bp$y)
outline <- bp[hull_idx, c("x","y")]
# smooth the hull slightly by inserting midpoints (cheap Chaikin-ish rounding)
chaikin <- function(pts, iters = 2) {
  for (k in seq_len(iters)) {
    n <- nrow(pts)
    newpts <- matrix(0, nrow = 2*n, ncol = 2)
    for (i in seq_len(n)) {
      p0 <- as.numeric(pts[i, ]); p1 <- as.numeric(pts[(i %% n) + 1, ])
      newpts[2*i-1, ] <- 0.75*p0 + 0.25*p1
      newpts[2*i,   ] <- 0.25*p0 + 0.75*p1
    }
    pts <- as.data.frame(newpts); colnames(pts) <- c("x","y")
  }
  pts
}
outline_smooth <- chaikin(outline, iters = 2)

out <- list(viewport = list(w = VW, h = VH), groups = groups, outline = outline_smooth)
write_json(out, "C:/Users/kevin/Documents/Flybrain/brain_layout.json", auto_unbox = TRUE, digits = 2)
cat("Saved brain_layout.json\n")
cat("Outline points:", nrow(outline_smooth), "\n")

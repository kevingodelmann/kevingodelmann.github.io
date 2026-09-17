.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(malecns))
suppressMessages(library(neuprintr))
suppressMessages(library(jsonlite))

cat("Layer 0: LC10 small-target/pursuit visual projection neurons...\n")
set.seed(7)
lc10_all <- mcns_neuprint_meta("/LC10.*")
lc10 <- lc10_all[sample(nrow(lc10_all), 200), c("bodyid","type","somaSide")]
lc10$side <- ifelse(grepl("R", lc10$somaSide), "R", ifelse(grepl("L", lc10$somaSide), "L", NA))
cat("LC10 sample:", nrow(lc10), "neurons (", sum(lc10$side=="L"), "L /", sum(lc10$side=="R"), "R )\n")

cat("\nLayer 1: DNs actually downstream of LC10 (pursuit/steering pathway)...\n")
dns <- mcns_neuprint_meta("/DNa10|DNp10")
dns <- dns[, c("bodyid","type","somaSide")]
dns$side <- ifelse(grepl("R", dns$somaSide), "R", ifelse(grepl("L", dns$somaSide), "L", NA))
cat("DN set:\n"); print(dns)

cat("\nLC10 -> DN real synaptic weights...\n")
e0 <- neuprint_connection_table(lc10$bodyid, prepost="POST", by.roi=FALSE)
e0 <- e0[e0$partner %in% dns$bodyid, ]
cat("LC10->DN edges:", nrow(e0), "\n")

cat("\nLayer 2: DN -> VNC interneurons (top by weight)...\n")
c1 <- neuprint_connection_table(dns$bodyid, prepost="POST", by.roi=FALSE)
c1 <- c1[order(-c1$weight),]
c1_top <- head(c1, 150)
l1_ids <- unique(c1_top$partner)

cat("Layer 3: interneurons -> leg/flight motor neurons (filter type contains 'MN')...\n")
c2 <- neuprint_connection_table(l1_ids, prepost="POST", by.roi=FALSE)
c2_meta <- neuprint_get_meta(unique(c2$partner))[, c("bodyid","type")]
mn_meta <- c2_meta[grepl("MN", c2_meta$type, ignore.case=FALSE), ]
cat("Found", nrow(mn_meta), "candidate motor neuron partners\n")

c2_mn <- c2[c2$partner %in% mn_meta$bodyid, ]
c2_mn <- c2_mn[order(-c2_mn$weight),]
c2_mn_top <- head(c2_mn, 120)

l1_used <- unique(c2_mn_top$bodyid)
c1_final <- c1_top[c1_top$partner %in% l1_used, ]

mn_ids <- unique(c2_mn_top$partner)
mn_meta_final <- neuprint_get_meta(mn_ids)[, c("bodyid","type")]
cat("\nFinal motor neuron set:\n")
print(table(mn_meta_final$type))

edges0 <- data.frame(from=e0$bodyid, to=e0$partner, weight=e0$weight, layer="lc_to_dn")
edges1 <- data.frame(from=c1_final$bodyid, to=c1_final$partner, weight=c1_final$weight, layer="dn_to_in")
edges2 <- data.frame(from=c2_mn_top$bodyid, to=c2_mn_top$partner, weight=c2_mn_top$weight, layer="in_to_mn")
edges <- rbind(edges0, edges1, edges2)

circuit <- list(
  lc_meta = lc10,
  dn_meta = dns,
  motor_meta = mn_meta_final,
  interneuron_ids = l1_used,
  edges = edges
)

out_path <- "C:/Users/kevin/Documents/Flybrain/circuit_appetitive.json"
write_json(circuit, out_path, auto_unbox=TRUE)
cat("\nSaved circuit to", out_path, "\n")
cat("Nodes: ", nrow(lc10), "LC10 +", nrow(dns), "DN +", length(l1_used), "interneurons +", length(mn_ids), "motor neurons\n")
cat("Edges:", nrow(edges), "(", nrow(edges0), "LC10->DN,", nrow(edges1), "DN->IN,", nrow(edges2), "IN->MN)\n")

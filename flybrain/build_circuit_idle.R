.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(malecns))
suppressMessages(library(neuprintr))
suppressMessages(library(jsonlite))

cat("DN: DNg13 (spontaneous/idle walking candidate, no known sensory-specific role)...\n")
dns <- mcns_neuprint_meta("/DNg13$")
dns <- dns[, c("bodyid","type","somaSide")]
dns$side <- ifelse(grepl("R", dns$somaSide), "R", ifelse(grepl("L", dns$somaSide), "L", NA))
print(dns)

cat("\nDN -> VNC interneurons...\n")
c1 <- neuprint_connection_table(dns$bodyid, prepost="POST", by.roi=FALSE)
c1 <- c1[order(-c1$weight),]
c1_top <- head(c1, 100)
l1_ids <- unique(c1_top$partner)

cat("interneurons -> leg motor neurons...\n")
c2 <- neuprint_connection_table(l1_ids, prepost="POST", by.roi=FALSE)
c2_meta <- neuprint_get_meta(unique(c2$partner))[, c("bodyid","type")]
mn_meta <- c2_meta[grepl("MN", c2_meta$type, ignore.case=FALSE), ]
cat("motor neuron candidates:", nrow(mn_meta), "\n")

c2_mn <- c2[c2$partner %in% mn_meta$bodyid, ]
c2_mn <- c2_mn[order(-c2_mn$weight),]
c2_mn_top <- head(c2_mn, 80)
l1_used <- unique(c2_mn_top$bodyid)
c1_final <- c1_top[c1_top$partner %in% l1_used, ]
mn_ids <- unique(c2_mn_top$partner)
mn_meta_final <- neuprint_get_meta(mn_ids)[, c("bodyid","type")]
print(table(mn_meta_final$type))

edges1 <- data.frame(from=c1_final$bodyid, to=c1_final$partner, weight=c1_final$weight, layer="dn_to_in")
edges2 <- data.frame(from=c2_mn_top$bodyid, to=c2_mn_top$partner, weight=c2_mn_top$weight, layer="in_to_mn")
edges <- rbind(edges1, edges2)

circuit <- list(dn_meta = dns, motor_meta = mn_meta_final, interneuron_ids = l1_used, edges = edges)
write_json(circuit, "C:/Users/kevin/Documents/Flybrain/circuit_idle.json", auto_unbox=TRUE)
cat("\nSaved. DN:", nrow(dns), " IN:", length(l1_used), " MN:", length(mn_ids), " edges:", nrow(edges), "\n")

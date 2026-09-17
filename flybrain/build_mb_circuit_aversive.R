.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(malecns))
suppressMessages(library(neuprintr))
suppressMessages(library(jsonlite))

set.seed(42)
cat("Fetching Kenyon cell pool (KCg-m, largest subtype)...\n")
kc_all <- mcns_neuprint_meta("/KCg-m")
kc <- kc_all[sample(nrow(kc_all), 300), c("bodyid","type")]

cat("Fetching MBON11 (real target of KC synapses, also matches published doomfly circuit)...\n")
mbon <- mcns_neuprint_meta("/MBON11")[, c("bodyid","type","somaSide")]

cat("Fetching PPL101 (aversive/punishment dopamine neuron pair)...\n")
dan <- mcns_neuprint_meta("/PPL101")[, c("bodyid","type","somaSide")]

cat("Real KC -> MBON11 synaptic weights...\n")
conn <- neuprint_connection_table(kc$bodyid, prepost="POST", by.roi=FALSE)
conn_mbon <- conn[conn$partner %in% mbon$bodyid, ]
cat("Edges found:", nrow(conn_mbon), "\n")

edges <- data.frame(from = conn_mbon$bodyid, to = conn_mbon$partner, weight = conn_mbon$weight)

circuit <- list(kc_meta = kc, mbon_meta = mbon, dan_meta = dan, edges = edges)
out_path <- "C:/Users/kevin/Documents/Flybrain/mb_circuit.json"
write_json(circuit, out_path, auto_unbox = TRUE)
cat("Saved to", out_path, "\n")
cat("KC:", nrow(kc), " MBON11:", nrow(mbon), " PPL101:", nrow(dan), " KC->MBON11 edges:", nrow(edges), "\n")

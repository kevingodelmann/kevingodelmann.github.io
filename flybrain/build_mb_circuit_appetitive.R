.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(malecns))
suppressMessages(library(neuprintr))
suppressMessages(library(jsonlite))

set.seed(43)
cat("Fetching Kenyon cell pool (KCg-m, largest subtype)...\n")
kc_all <- mcns_neuprint_meta("/KCg-m")
kc <- kc_all[sample(nrow(kc_all), 300), c("bodyid","type")]

cat("Fetching MBON01 (appetitive-side output neuron candidate)...\n")
mbon <- mcns_neuprint_meta("/MBON01")[, c("bodyid","type","somaSide")]

cat("Fetching PAM cluster (reward dopamine neurons)...\n")
dan <- mcns_neuprint_meta("/PAM01|PAM02")[, c("bodyid","type","somaSide")]

cat("Real KC -> MBON01 synaptic weights...\n")
conn <- neuprint_connection_table(kc$bodyid, prepost="POST", by.roi=FALSE)
conn_mbon <- conn[conn$partner %in% mbon$bodyid, ]
cat("Edges found:", nrow(conn_mbon), "\n")

edges <- data.frame(from = conn_mbon$bodyid, to = conn_mbon$partner, weight = conn_mbon$weight)

circuit <- list(kc_meta = kc, mbon_meta = mbon, dan_meta = dan, edges = edges)
out_path <- "C:/Users/kevin/Documents/Flybrain/mb_circuit_appetitive.json"
write_json(circuit, out_path, auto_unbox = TRUE)
cat("Saved to", out_path, "\n")
cat("KC:", nrow(kc), " MBON01:", nrow(mbon), " PAM:", nrow(dan), " KC->MBON01 edges:", nrow(edges), "\n")

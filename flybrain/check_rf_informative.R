.libPaths(c("C:/Users/kevin/Documents/R/win-library/4.6", .libPaths()))
suppressMessages(library(jsonlite))
c_ <- fromJSON("C:/Users/kevin/Documents/Flybrain/circuit_appetitive_rf.json")
lc <- c_$lc_meta; dn <- c_$dn_meta
e0 <- c_$edges[c_$edges$layer=="lc_to_dn",]
M <- matrix(0, nrow(lc), nrow(dn), dimnames=list(as.character(lc$bodyid), as.character(dn$bodyid)))
for (k in seq_len(nrow(e0))) { f<-as.character(e0$from[k]); t<-as.character(e0$to[k])
  if (f %in% rownames(M) && t %in% colnames(M)) M[f,t] <- e0$weight[k] }
colnames(M) <- paste0(dn$type,"_",dn$side)
cat("LC10 cells with any DN connection:", sum(rowSums(M)>0), "of", nrow(M), "\n\n")
for (j in seq_len(ncol(M))) {
  w <- M[,j]; keep <- w>0
  if (sum(keep)<4) { cat(sprintf("%-10s only %d inputs\n", colnames(M)[j], sum(keep))); next }
  ct <- cor.test(lc$rf_az[keep], w[keep], method="spearman")
  cat(sprintf("%-10s n=%3d  rho(rf_az, weight) = %+.2f  p=%.3f   mean rf_az of its inputs = %+.2f\n",
      colnames(M)[j], sum(keep), ct$estimate, ct$p.value, mean(lc$rf_az[keep])))
}
cat("\nDo the two DNa10 sides sample different azimuths?\n")
for (nm in c("DNa10_L","DNa10_R")) { w<-M[,nm]; cat(sprintf("  %s: mean rf_az %+.3f (n=%d)\n", nm, mean(lc$rf_az[w>0]), sum(w>0))) }
a<-lc$rf_az[M[,"DNa10_L"]>0]; b<-lc$rf_az[M[,"DNa10_R"]>0]
print(wilcox.test(a,b))

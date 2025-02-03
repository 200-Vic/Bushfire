library(data.table)

ALL <- as.data.table(read.csv("ALL_pwa_sa22003-2020.csv"))
WF <- as.data.table(read.csv("WF_pwa_sa22003-2020.csv"))
setnames(WF, "weighted_mean", "WF")
PB <- as.data.table(read.csv("PB_pwa_sa22003-2020.csv"))
setnames(PB, "weighted_mean", "PB")

noWF <- merge(ALL, WF[,.(SA2_MAIN16, year, WF)], by = c("SA2_MAIN16", "year"))
noWF$weighted_mean <- noWF$weighted_mean-noWF$WF
write.csv(noWF, "noWF_pwa_sa22003-2020.csv")

noPB <- merge(ALL, PB[,c("SA2_MAIN16", "year", "PB")], by = c("SA2_MAIN16", "year"))
noPB$weighted_mean <- noPB$weighted_mean-noPB$PB
write.csv(noPB, "noPB_pwa_sa22003-2020.csv")

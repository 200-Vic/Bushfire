tbl <- read.csv("C:/privateRlab/air-health-scenario-app/data/HIA_VIC.csv")
tbl <- as.data.table(tbl)
summary_years <- tbl[,.(mean(x)), by = c("date_year", "pollutant")]

hia <- readRDS("C:/privateRlab/air-health-scenario-app/results/do_hia.rds")
out_master <-  as.data.table(hia)
yll_sa2_year <- out_master[,.(var = sum(yll)),by = .(SA2_MAIN16, date_year, est_type, pollutant)]
yll_sa2_year <- dcast(yll_sa2_year, SA2_MAIN16 + date_year + pollutant ~ est_type, value.var = "var")

## note there are some with NA
nrow(yll_sa2_year[is.na(est)])
# 613, different from pm2.5 1766
paste(unique(yll_sa2_year[is.na(est)]$SA2_MAIN16), collapse ="','", sep = "")
# check in do_data_checking, these are sa2 with zero pop in some years


yll_nat_year <- yll_sa2_year[,.(est = sum(est, na.rm=TRUE), lci = sum(lci, na.rm=TRUE), uci = sum(uci, na.rm=TRUE)), by = .(date_year, pollutant)]

an_sa2_year <- out_master[,.(var = sum(an)),by = .(SA2_MAIN16, date_year, baseline.age, est_type, pollutant)]
an_sa2_year <- dcast(an_sa2_year, SA2_MAIN16 + date_year + baseline.age + pollutant ~ est_type, value.var = "var")
an_sa2_year$age <- substr(an_sa2_year$baseline.age, 1,2)
nrow(an_sa2_year[date_year == 2020])

an_nat_year <- an_sa2_year[,.(est = sum(est, na.rm=TRUE), lci = sum(lci, na.rm=TRUE), uci = sum(uci, na.rm=TRUE)), by = .(date_year, pollutant, age)]
an_nat <- an_nat_year[,.(est = mean(est, na.rm=TRUE), lci = mean(lci, na.rm=TRUE), uci = mean(uci, na.rm=TRUE)), by = .(pollutant, age)]
an_nat <- an_nat[order(as.numeric(age))]

table <- an_nat[,.(est = sum(est), lci = sum(lci), uci = sum(uci))]
an_nat <- t(an_nat)

write.csv(an_nat, "C:/privateRlab/air-health-scenario-app/results/AN_age_pm_ALL.csv")

###life expectancy

le_nat <- out_master[,.(le = mean(baseline.ex, na.rm=T), leloss = mean(difference.ex_diff, na.rm=T)), by = .(difference.age, date_year, est_type)]



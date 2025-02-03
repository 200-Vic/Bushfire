
library(data.table)
library(sf)
library(lubridate)

#READ daily PM.RDS
day_pm_SA2 <- readRDS("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Pollution_CSIRO/data_derived/Daily_PM2.5_conc_by_SA2.RDS")

#make into data table
day_pm_SA2 <- as.data.table(day_pm_SA2)

#get dates into a single column by melting
day_pm_SA2 <- melt(day_pm_SA2, id.vars = "SA2_MAIN16") 
setnames(day_pm_SA2, "variable", "date")

#Format dates into as.Dates 
day_pm_SA2$date <- paste0(substr(day_pm_SA2$date, 1,4), "-", substr(day_pm_SA2$date, 5,6), "-", substr(day_pm_SA2$date, 7,8))
day_pm_SA2$date <- as.Date(day_pm_SA2$date)
sa2 <- unique(day_pm_SA2$SA2_MAIN16)

#Get ABS spatial and population
areas <- st_read("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Spatial_ABS/MB_2016_VIC.shp")

pop <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Population_ABS/2016 census mesh block counts_no_footer.csv")
setnames(pop, "MB_CODE_2016", "MB_CODE16")
pop$MB_CODE16 <- as.character(pop$MB_CODE16)

##Merge pop and area at MB level
area_pop <- merge(areas, pop, by = c("MB_CODE16"))
area_pop <- as.data.table(area_pop)

##aggregate pop and area to SA2 and remove SA2's not in day_pm_sa2
area_pop_sa2 <- area_pop[,.(SA3_CODE16, SA3_NAME16, SA4_CODE16, SA4_NAME16, GCC_CODE16, GCC_NAME16, pop = sum(Person), AREASQKM16 = sum(AREASQKM16)), by = c("SA2_NAME16", "SA2_MAIN16")]
area_pop_sa2 <- unique(area_pop_sa2[])
area_pop_sa2 <- area_pop_sa2[SA2_MAIN16 %in% sa2]

###Merge SA2 pollution and pop and area
day_pm_pop <- merge(day_pm_SA2, area_pop_sa2, by = c("SA2_MAIN16"))
#plot(day_pm_pop$date, day_pm_pop$value)

#write.csv(day_pm_pop, "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/Data/Pollution_CSIRO/data_derived/day_pm_pop_sa2.csv")

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ time distributed lag from Jegasothy et al 2023 days 0-7 <- 1.002, 1.015, 1.013, 1.008, 1.004, 1.003, 1.004 and 1.006 
day_pm_SA2 

#take a simple 95th percentile threshold to identify PM2.5 events
thresh = FALSE
if(thresh){
thresh <- quantile(day_pm_SA2$value, 0.95)
day_pm_SA2$mort_risk_attrib <- (day_pm_SA2$value-thresh)#*10*1.032
day_pm_SA2$mort_risk_attrib <- ifelse(day_pm_SA2$mort_risk_attrib < 0, 0, day_pm_SA2$mort_risk_attrib)}


###use season trend decomposition to portion pm into anthropogenic, fire and burn
std = TRUE 
if(std){
std <- read.csv("ranger_fire_PM3.csv")
std$SA2_MAIN16 <- as.character(std$SA2_MAIN16)
std$date <- as.Date(std$date)

#day_pm_SA2 <- merge(day_pm_SA2, std, by=c("SA2_MAIN16", "date"))
# day_pm_SA2$mort_risk_fire <- day_pm_SA2$value*day_pm_SA2$fireprop
# day_pm_SA2$mort_risk_burn <- day_pm_SA2$value*day_pm_SA2$burnprop
day_pm_SA2 <- as.data.table(std)
# day_pm_SA2$mort_risk_fire <- day_pm_SA2$fire
# day_pm_SA2$mort_risk_burn <- day_pm_SA2$burn
# day_pm_SA2$mort_risk_attrib <- day_pm_SA2$mort_risk_fire+day_pm_SA2$mort_risk_burn
# day_pm_SA2$mort_risk_anth <- day_pm_SA2$all-day_pm_SA2$mort_risk_attrib
}
# cor.test(day_pm_SA2$all, day_pm_SA2$value)
# plot(day_pm_SA2$date, day_pm_SA2$mort_risk_anth)
str(day_pm_SA2)


rr_day <- c(0.002, 0.015, 0.013, 0.008, 0.004, 0.003, 0.004, 0.006)

#sa2_todo <- melb_innerSA2
thresh <- quantile(day_pm_SA2$all, 0.95, na.rm=T)
day_pm_SA2x <- day_pm_SA2[all > thresh & date > 2003-01-01]
sa2_todo <- unique(day_pm_SA2x$SA2_MAIN16)

for(sa2_i in 1:length(sa2_todo)){ 

#sa2_i = 8
sa21 <- day_pm_SA2x[SA2_MAIN16 == sa2_todo[sa2_i]]
#thresh <- quantile(sa21$mort_risk_anth, 0.95, na.rm=T)
haze <- sa21[all > thresh]

day_todo <- unique(haze$date, na.rm=T)

for(i in 1:length(day_todo)){
#i = 1

f <- sa21[date == day_todo[i]][0:8]
f$date <- seq(as.Date(day_todo[i]), by = "day", length.out = 8)
f1 <- cbind(f, rr_day)
ex <- f1$all[1]-thresh  
ex <- ifelse(ex<0, 0, ex)
f1$risk <- f1$rr_day*ex*10 
exf <- f1$fire[1]
f1$riskf <- f1$rr_day*exf*10 
exb <- f1$burn[1]
f1$riskb <- f1$rr_day*exb*10
exa <- f1$anth[1]
f1$riska <- f1$rr_day*exa*10

if(i == 1){
  out_day <- f1
}else{
  out_day <- rbind(out_day, f1)
}
out_day <- as.data.table(out_day)
out_day <- out_day[order(date)]

out_day_dt <- out_day[,.(SA2_MAIN16 = sa2_todo[sa2_i], risk = sum(risk), riskf = sum(riskf), riskb = sum(riskb), riska = sum(riska)), by = c("date")]
# day_dt <- out_day_dt[date > "2019-01-01"]
# plot(day_dt$date, day_dt$mort_risk_fire, type = "h")
# plot(day_dt$date, day_dt$riskf, type = "h")

}
#plot(out_day_dt$date, out_day_dt$risk, main = sa2_todo[i])
if(sa2_i == 1){
  out_day_all <- out_day_dt
}else{
  out_day_all <- rbind(out_day_all, out_day_dt)
}


}
#qc3 <- out_day_all[SA2_MAIN16 %in% melb_innerSA2]
day_pm_SA2_risk <- merge(day_pm_SA2, out_day_all, by = c("SA2_MAIN16", "date"), all.x=T)
day_pm_SA2_risk <- as.data.table(day_pm_SA2_risk)
#

day_pm_SA2_risk <- setnafill(day_pm_SA2_risk, "const", fill = 0, cols = c("risk", "riskf", "riskb", "riska"))
#qc1 <- day_pm_SA2_risk[risk==0 & ]

qc2 <- day_pm_SA2_risk[,.(thresh = quantile(all, 0.95, na.rm=T)), by = c("SA2_MAIN16")]

write.csv(day_pm_SA2_risk, "C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/EHTN/Power BI lab/Current project/daily_pm_mort_SA2_ranger.csv")
unique(day_pm_SA2_risk$SA2_MAIN16)


#@@@@Make graphs

day_pm_SA2_risk <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/EHTN/Power BI lab/Current project/daily_pm_mort_SA2_ranger.csv")
day_pm_SA2_risk$SA2_MAIN16 <- as.character(day_pm_SA2_risk$SA2_MAIN16)
day_pm_SA2_risk <- setnafill(day_pm_SA2_risk, "const", fill = 0, cols = c("fire","burn","anth", "all","risk","riskf","riskb","riska"))


sa2_vic <- st_read("data/SA2_2016_VIC.shp")
sa2_vic_dt <- as.data.table(sa2_vic)
summary <- merge(sa2_vic_dt[,.(SA2_MAIN16, SA2_NAME16, SA4_NAME16, SA4_CODE16, GCC_NAME16)], day_pm_SA2_risk, by = c("SA2_MAIN16"))
str(summary)
summary <- as.data.table(summary)
qc2 <- summary[SA4_NAME16 == "Melbourne - Inner"]
#melb_innerSA2 <- unique(qc2$SA2_MAIN16)

day_pm_sa4_risk <- summary[,.(car_PM2.5 = mean(all), anthpm = mean(anth), firepm = mean(fire), burnpm = mean(burn), Mort_risk = mean(risk), Mort_risk_fire = mean(riskf), Mort_risk_burn = mean(riskb), Mort_risk_anth = mean(riska)), by = c("SA4_NAME16", "GCC_NAME16", "date")]
day_pm_sa4_risk$date <- as.Date(day_pm_sa4_risk$date)
qc <- day_pm_sa4_risk[SA4_NAME16 == "Melbourne - Inner"]


SA4_todo <- unique(day_pm_sa4_risk$SA4_NAME16)
for(i in 1:length(SA4_todo)){
  #i = 1
plot <- day_pm_sa4_risk[SA4_NAME16 == SA4_todo[i]]
plot <- plot[(order(date))]
#setnafill(plot, "const", fill = 0, cols = c("fire","burn"))
plot(plot$date, plot$burnpm, main = SA4_todo[i], type = "p", xlab = "Date", ylab = "PM2.5 (ug/m3)")
plot(plot$date, plot$firepm, main = SA4_todo[i], type = "p", add=T, col= "red")
plot(plot$date, plot$anthpm, main = SA4_todo[i], type = "p", add=T, col= "blue")
plot(plot$date, plot$Mort_risk_fire, main = SA4_todo[i], type = "p", xlab = "Date", ylab = "% Excess death risk fire")
plot(plot$date, plot$Mort_risk_burn, main = SA4_todo[i], type = "p", xlab = "Date", ylab = "% Excess death risk burn")
plot(plot$date, plot$Mort_risk_anth, main = SA4_todo[i], type = "p", xlab = "Date", ylab = "% Excess death risk anth")
}
#plot(day_pm_SA2_risk$date,  day_pm_SA2_risk$fire)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#daypm_SA2_BI <- head(day_pm_SA2[date == "2019-11-25"], 462)
plot <- day_pm_SA2[SA2_MAIN16 == "202021027"]

x_sa <- plot[mort_risk_attrib > 0]
plot(x_sa$date, x_sa$mort_risk_attrib)

day_pm_SA2_risk <- as.data.table(day_pm_SA2_risk)#, by = "SA4_NAME16"
summaryf <- day_pm_sa4_risk[Mort_risk_fire > 0]
quantile(summaryf$firepm)
for(i in 1:length(SA4_todo)){
#i = 1
  s <- summaryf[SA4_NAME16 == SA4_todo[i]]
s1 <- unique(s[,.(SA4_NAME16, Fire_haze_days = nrow(s), mortriskf = mean(Mort_risk_fire), firepm = mean(firepm))])
if(i == 1){fire_dt <- s1}
else{fire_dt <- rbind(fire_dt, s1)}
fire_dt$mortriskf <- round(fire_dt$mortriskf, 4)
}


summaryb <- day_pm_sa4_risk[Mort_risk_burn > 0]
quantile(summaryb$burnpm)
for(i in 1:length(SA4_todo)){
  #i = 1
  t <- summaryb[SA4_NAME16 == SA4_todo[i]]
  t1 <- unique(t[,.(SA4_NAME16, Burn_haze_days = nrow(t), mortriskb = mean(Mort_risk_burn), burnpm = mean(burnpm))])
  if(i == 1){burn_dt <- t1}
  else{burn_dt <- rbind(burn_dt, t1)}
  burn_dt$mortriskb <- round(burn_dt$mortriskb, 4)
}
 
fire_burn <- merge(fire_dt, burn_dt, by = c("SA4_NAME16"))
write.csv(fire_burn, "fires_burns_days_%risk2.csv")
#write.csv(daypm_SA2_BI, "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/dayPM25_SA2_BIexp.csv")


write.csv(day_pm_SA2, "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/PM25_daily_SA2.csv")

#@@@@@@@@@@@@prep BI data table daily
day_pm_SA2_risk <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/EHTN/Power BI lab/Current project/daily_pm_mort_SA2_5.csv")
#Get ABS spatial and population 
areas <- st_read("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Spatial_ABS/MB_2016_VIC.shp")

pop <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Population_ABS/2016 census mesh block counts_no_footer.csv")
setnames(pop, "MB_CODE_2016", "MB_CODE16")
pop$MB_CODE16 <- as.character(pop$MB_CODE16)

##Merge pop and area at MB level
area_pop <- merge(areas, pop, by = c("MB_CODE16"))
area_pop <- as.data.table(area_pop)

##aggregate pop and area to SA2 and remove SA2's not in day_pm_sa2
area_pop_sa2 <- area_pop[,.(pop = sum(Person), AREASQKM16 = sum(AREASQKM16)), by = c("SA2_NAME16", "SA2_MAIN16", "SA3_CODE16", "SA3_NAME16", "SA4_CODE16", "SA4_NAME16", "GCC_CODE16", "GCC_NAME16")]

str(day_pm_SA2_risk)
day_pm_SA2_risk <- as.data.table(day_pm_SA2_risk)
day_pm_SA2_risk$date <- as.Date(day_pm_SA2_risk$date)
day_pm_SA2_risk$SA2_MAIN16 <- as.character(day_pm_SA2_risk$SA2_MAIN16)


day_pm_pop_SA2_risk <- merge(day_pm_SA2_risk[,c("SA2_MAIN16", "date", "all", "fire", "burn", "anth", "risk", "riskf", "riskb", "riska")], area_pop_sa2[,c("pop", "SA2_MAIN16", "SA2_NAME16", "SA3_CODE16", "SA4_CODE16")], by = c("SA2_MAIN16"))



#write.csv( , "Dashboard 1 - Daily_time_series_finalBIreport1")


#@@@@@@@@@@@@@@@@@month and year averages

day_pm_SA2 <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/EHTN/Power BI lab/Current project/daily_pm_mort_SA2.csv")

day_pm_SA2 <- as.data.table(day_pm_SA2)
day_pm_SA2$date <- as.Date(day_pm_SA2$date)
day_pm_SA2$months <-months(day_pm_SA2$date)
day_pm_SA2$year <-year(day_pm_SA2$date)
month_avs <- day_pm_SA2[,.(monthPM2.5 = mean(all)), by = c("SA2_MAIN16", "months", "year")]


min(month_avs$monthPM2.5)
write.csv(month_avs, "C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/EHTN/Power BI lab/Current project/BI_Data12052023/PM25_monthly_SA2_BIreport1.csv")

year_avs <- day_pm_SA2[,.(annualPM2.5 = mean(all)), by = c("SA2_MAIN16", "year")]
setnames(year_avs, "V1", "annualPM2.5")

min(year_avs$annualPM2.5)
write.csv(year_avs, "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/BI_Data12052023/PM25_annual_SA2_BIreport1.csv")  

long_avs <- day_pm_SA2[,. (pm = mean(all, na.rm=T), fpm = mean(fire, na.rm=T), apm = mean(anth, na.rm=T), pbpm = mean(burn, na.rm=T))]



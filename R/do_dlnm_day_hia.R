library(data.table)
library(sf)
library(lubridate)

std <- read.csv("ranger_fire_PM3.csv")
std$SA2_MAIN16 <- as.character(std$SA2_MAIN16)
std$date <- as.Date(std$date)

day_pm_SA2 <- as.data.table(std)

str(day_pm_SA2)


rr_day <- c(0.002, 0.015, 0.013, 0.008, 0.004, 0.003, 0.004, 0.006)

###note: here we adjust the all ages time distributed rr_day values by the exposure magnitude rr_day values for over 65s
rr_day_10 <- rr_day*(1.039/1.032)
rr_day_15 <- rr_day*(1.056/1.032)
rr_day_25 <- rr_day*(1.073/1.032)
rr_day_50 <- rr_day*(-0.03/1.032)

#sa2_todo <- melb_innerSA2
thresh <- quantile(day_pm_SA2$all, 0.95, na.rm=T)
day_pm_SA2$event <- day_pm_SA2$anth-thresh
day_pm_SA2$event <- ifelse(day_pm_SA2$event<0, 0, day_pm_SA2$event)
day_pm_SA2x <- day_pm_SA2[all > thresh & date > 2003-01-01]# & fire > 12.5]

# day_pm_SA2x$fire10 <- ifelse(day_pm_SA2x$fire < 12.5, day_pm_SA2x$fire, 0) 
# day_pm_SA2x$fire15 <- ifelse(day_pm_SA2x$fire > 12.5 & day_pm_SA2x$fire < 20 , day_pm_SA2x$fire, 0)
# day_pm_SA2x$fire25 <- ifelse(day_pm_SA2x$fire > 20 & day_pm_SA2x$fire < 37.5 , day_pm_SA2x$fire, 0)
# day_pm_SA2x$fire50 <- ifelse(day_pm_SA2x$fire > 37.5, day_pm_SA2x$fire, 0)
# 
# day_pm_SA2x$burn10 <- ifelse(day_pm_SA2x$burn < 12.5, day_pm_SA2x$burn, 0) 
# day_pm_SA2x$burn15 <- ifelse(day_pm_SA2x$burn > 12.5 & day_pm_SA2x$burn < 20 , day_pm_SA2x$burn, 0)
# day_pm_SA2x$burn25 <- ifelse(day_pm_SA2x$burn > 20 & day_pm_SA2x$burn < 37.5 , day_pm_SA2x$burn, 0)
# day_pm_SA2x$burn50 <- ifelse(day_pm_SA2x$burn > 37.5, day_pm_SA2x$burn, 0)

sa2_todo <- unique(day_pm_SA2x$SA2_MAIN16)

for(sa2_i in 1:length(sa2_todo)){ 
  
  #sa2_i = 1
  sa21 <- day_pm_SA2x[SA2_MAIN16 == sa2_todo[sa2_i]]
  #thresh <- quantile(sa21$mort_risk_anth, 0.95, na.rm=T)
  haze <- sa21[all > thresh]
  
  day_todo <- unique(haze$date, na.rm=T)
  
  for(i in 1:length(day_todo)){
    #i = 534
    
    f <- sa21[date == day_todo[i]][0:8]
    f$date <- seq(as.Date(day_todo[i]), by = "day", length.out = 8)
    f1 <- cbind(f, rr_day, rr_day_10, rr_day_15, rr_day_25, rr_day_50)
    
    exf <- f1$fire[1]
    if(exf < 12.5){f1$riskf <- f1$rr_day_10*exf*10}else
    {if(exf >= 12.5 & exf < 20){f1$riskf <- f1$rr_day_15*exf*10}else
    {if(exf >= 20 & exf < 37.5){f1$riskf <- f1$rr_day_25*exf*10}else
    {if(exf >= 37.5){f1$riskf <- f1$rr_day_50*exf*10}}}}
      
    exb <- f1$burn[1]
    if(exb < 12.5){f1$riskb <- f1$rr_day_10*exb*10}else
    {if(exb >= 12.5 & exb < 20){f1$riskb <- f1$rr_day_15*exb*10}else
    {if(exb >= 20 & exb < 37.5){f1$riskb <- f1$rr_day_25*exb*10}else
    {if(exb >= 37.5){f1$riskb <- f1$rr_day_50*exb*10}}}}
    
    exa <- f1$event[1]
    if(exa < 12.5){f1$riska <- f1$rr_day_10*exa*10}else
    {if(exa >= 12.5 & exa < 20){f1$riska <- f1$rr_day_15*exa*10}else
    {if(exa >= 20 & exa < 37.5){f1$riska <- f1$rr_day_25*exa*10}else
    {if(exa >= 37.5){f1$riska <- f1$rr_day_50*exa*10}}}}
    
    if(i == 1){
      out_day <- f1
    }else{
      out_day <- rbind(out_day, f1)
    }
    out_day <- as.data.table(out_day)
    out_day <- out_day[order(date)]
    
    out_day_dt <- out_day[,.(SA2_MAIN16 = sa2_todo[sa2_i], riskf = sum(riskf), riskb = sum(riskb), riska = sum(riska)), by = c("date")]
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

day_pm_SA2_risk <- setnafill(day_pm_SA2_risk, "const", fill = 0, cols = c("riskf", "riskb", "riska"))
#qc1 <- day_pm_SA2_risk[risk==0 & ]

#qc2 <- day_pm_SA2_risk[,.(thresh = quantile(all, 0.95, na.rm=T)), by = c("SA2_MAIN16")]
write.csv(day_pm_SA2_risk, "C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/EHTN/Power BI lab/Current project/daily_pm_mort_SA2_ranger_dlnm.csv")
#unique(day_pm_SA2_risk$SA2_MAIN16)


#@@@@Make graphs

day_pm_SA2_risk <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/EHTN/Power BI lab/Current project/daily_pm_mort_SA2_ranger_dlnm.csv")
day_pm_SA2_risk$SA2_MAIN16 <- as.character(day_pm_SA2_risk$SA2_MAIN16)
day_pm_SA2_risk <- setnafill(day_pm_SA2_risk, "const", fill = 0, cols = c("fire","burn","anth", "event", "all","riskf","riskb", "riska"))


sa2_vic <- st_read("data/SA2_2016_VIC.shp")
sa2_vic_dt <- as.data.table(sa2_vic)
summary <- merge(sa2_vic_dt[,.(SA2_MAIN16, SA2_NAME16, SA4_NAME16, SA4_CODE16, GCC_NAME16)], day_pm_SA2_risk, by = c("SA2_MAIN16"))
str(summary)
summary <- as.data.table(summary)
qc2 <- summary[SA4_NAME16 == "Melbourne - Inner"]
#melb_innerSA2 <- unique(qc2$SA2_MAIN16)

day_pm_sa4_risk <- summary[,.(car_PM2.5 = mean(all), anthpm = mean(anth), firepm = mean(fire), burnpm = mean(burn), eventpm = mean(event), Mort_risk_fire = mean(riskf), Mort_risk_burn = mean(riskb), Mort_risk_event = mean(riska)), by = c("SA4_NAME16", "GCC_NAME16", "date")]
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
  plot(plot$date, plot$eventpm, main = SA4_todo[i], type = "p", add=T, col= "blue")
  plot(plot$date, plot$Mort_risk_fire, main = SA4_todo[i], type = "p", xlab = "Date", ylab = "% Excess death risk fire")
  plot(plot$date, plot$Mort_risk_burn, main = SA4_todo[i], type = "p", xlab = "Date", ylab = "% Excess death risk burn")
  plot(plot$date, plot$Mort_risk_event, main = SA4_todo[i], type = "p", xlab = "Date", ylab = "% Excess death risk anth")
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
  fire_dt$mortriskf <- round(fire_dt$mortriskf, 2)
}


summaryb <- day_pm_sa4_risk[Mort_risk_burn > 0]
quantile(summaryb$burnpm)
for(i in 1:length(SA4_todo)){
  #i = 1
  t <- summaryb[SA4_NAME16 == SA4_todo[i]]
  t1 <- unique(t[,.(SA4_NAME16, Burn_haze_days = nrow(t), mortriskb = mean(Mort_risk_burn), burnpm = mean(burnpm))])
  if(i == 1){burn_dt <- t1}
  else{burn_dt <- rbind(burn_dt, t1)}
  burn_dt$mortriskb <- round(burn_dt$mortriskb, 2)
}

summarya <- day_pm_sa4_risk[Mort_risk_event > 0]
quantile(summarya$eventpm)
for(i in 1:length(SA4_todo)){
  #i = 1
  p <- summarya[SA4_NAME16 == SA4_todo[i]]
  p1 <- unique(p[,.(SA4_NAME16, anth_haze_days = nrow(p), mortriska = mean(Mort_risk_event), anthpm = mean(eventpm))])
  if(i == 1){anth_dt <- p1}
  else{anth_dt <- rbind(anth_dt, p1)}
  anth_dt$mortriska <- round(anth_dt$mortriska, 2)
}

fire_burn <- cbind(fire_dt, burn_dt[,c(2,3,4)], anth_dt[,c(2,3,4)])#,by = c("SA4_NAME16") )
write.csv(fire_burn, "fires_burns_days_%risk_dlnm.csv")
#write.csv(daypm_SA2_BI, "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/dayPM25_SA2_BIexp.csv")


write.csv(day_pm_SA2, "C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/EHTN/Power BI lab/Current project/PM25_daily_SA2_ranger.csv")

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

long_avs <- day_pm_SA2[,. (pm = mean(all, na.rm=T), fpm = mean(fire, na.rm=T), apm = mean(event, na.rm=T), pbpm = mean(burn, na.rm=T))]

library(data.table)
library(sf)
library(readxl)


setwd("C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - SA3_weekly_hospitalisation")

vahi <- read_xlsx("4824 - EPA VAED.xlsx", sheet = "Data Output", skip = 8)
vahi <- as.data.table(vahi)
setnames(vahi, "Week end date", "Week_end_date")
vahi$Week_end_date <- as.Date(vahi$Week_end_date)
qc <- vahi[Week_end_date == "2013-11-09"]
min(vahi$Week_end_date)


bi_vahi <- vahi[,.(`Calendar year`, Week_end_date, `Diagnosis chapter desc`, `SA3 code`, `SA3 name`, `Total separations`)]
setnames(bi_vahi, "Total separations", "Total_separations")
bi_vahi$Total_separations <- ifelse(bi_vahi$Total_separations == "<5", "2.5", bi_vahi$Total_separations)
bi_vahi$Total_separations <- as.numeric(bi_vahi$Total_separations)
bi_vahi <- bi_vahi[,.(Total_separations = sum(Total_separations)), by = c("Calendar year", "Week_end_date", "SA3 code", "SA3 name", "Diagnosis chapter desc")]


##@@@@Do baseline separations@SA3
#first explore data to see whether there is seasonality   &`Calendar year` == "2014"

bl <- bi_vahi[,.(sum(Total_separations)), by = c("Diagnosis chapter desc", "Calendar year", "Week_end_date")]

plot <- bl[`Diagnosis chapter desc` == "Diseases of the Respiratory System"]
plot(plot$Week_end_date, plot$V1, xlab = "Weekend date", ylab = "Respiratory hospitalisations")
#`SA3 code`== "20601"  &
## number weeks so that average departures from annual averages can be calculated for each week

weeks <- unique(bi_vahi$Week_end_date)
weeks <- as.data.table(weeks)
we <- weeks[order(weeks)]

no <- c(rep(1:52, 3), 1:53, rep(1:52, 6), 1:6)

weekn <- cbind(we, no)
setnames(weekn, "weeks", "Week_end_date") 

#get average weekly values relative to annual means in SA3
weekno <- weekn[!Week_end_date < "2013-01-03" & !Week_end_date > "2019-12-28"] ##choose 7 x 52 baseline weeks
spline <- merge(weekno, bi_vahi, by = c("Week_end_date"))
qc <- spline[no == 15 & `SA3 code`== 20201]

plot <- spline[`Diagnosis chapter desc`== "Diseases of the Respiratory System"]
plot <- plot[,.(separations = sum(Total_separations)), by = c("Week_end_date", "no", "Calendar year")]
plot(plot$Week_end_date, plot$separations, xlab = "Weekend date", ylab = "Respiratory hospitalisations")

ymean <- spline[,.(ymean = mean(Total_separations)), by = c("Calendar year", "Diagnosis chapter desc", "SA3 code")]

spline1 <- merge(spline, ymean, by = c("Calendar year", "Diagnosis chapter desc", "SA3 code"))



setnames(spline1, "SA3 code", "SA3_CODE16")

spline1$weekdiff <- spline1$Total_separations/spline1$ymean
spline2 <- spline1[,.(meanweekdiff = mean(weekdiff)), by = c("no", "Diagnosis chapter desc")]
plot <- spline2[`Diagnosis chapter desc`== "Diseases of the Respiratory System"]
plot(plot$no, plot$meanweekdiff)
##@@@@@@@@@@@@@@@@@@@spline done@@@@@@@@@@@@@@@@@@@@@@@@

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

##aggregate pop and area to SA3 and remove SA2's not in day_pm_sa2
area_pop_sa3 <- unique(area_pop[,.(SA4_CODE16, SA4_NAME16, GCC_CODE16, GCC_NAME16, pop = sum(Person), AREASQKM16 = sum(AREASQKM16)), by = c("SA3_NAME16", "SA3_CODE16")])

###number all 527 weeks in bi_vahi 2013-2023 
str(bi_vahi)
weekn <- merge(weekn, bi_vahi, by = c("Week_end_date"))

setnames(weekn, "SA3 code", "SA3_CODE16")

###merge with area and population data

ds_vahi <- merge(weekn, area_pop_sa2[,.(SA2_NAME16, SA3_CODE16, SA3_NAME16, pop)], by = c("SA3_CODE16"), allow.cartesian = T)

###read daily pollution data with mortality stats may2018-2021
day_pm_SA2 <- read.csv("C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/daily_pm_mort_SA2.csv")

str(day_pm_SA2)
day_pm_SA2 <- as.data.table(day_pm_SA2)
day_pm_SA2$date <- as.Date(day_pm_SA2$date)
day_pm_SA2$SA2_MAIN16 <- as.character(day_pm_SA2$SA2_MAIN16)

##expand ds_vahi from weeks to days 
day_pm_pop <- merge(day_pm_SA2[,c("SA2_MAIN16", "date")], area_pop_sa2, by = c("SA2_MAIN16"))
dates_todo <- unique(day_pm_pop$date)
# day1 <- min(dates_todo)
# max(dates_todo)
##cut ds_vahi down to the time period of AQFx data
ds_vahi_day <- ds_vahi[Week_end_date > min(dates_todo) & Week_end_date < max(dates_todo)]

###apply spline to ds_vahi_day annual averages to calculate baseline weekly separations
an_mean_ds_vahi <- ds_vahi_day[,.(an_sep = mean(Total_separations)), by = c("SA3_CODE16", "Diagnosis chapter desc", "Calendar year", "no")]

###merge annual mean separations at sa3 with the vic mean weekly residual separations spline
an_mean_ds_vahi <- merge(an_mean_ds_vahi, spline2, by = c("Diagnosis chapter desc", "no"), allow.cartesian = T)

##Multiply annual mean separations at sa3 by weekly spatial mean diff 
an_mean_ds_vahi$bl_separations <- an_mean_ds_vahi$an_sep*an_mean_ds_vahi$meanweekdiff


##make table of week_end_dates and dates to help with merging of week and day data
weeks1 <- as.data.table(rep(seq(as.Date("2018-05-05"), as.Date("2021-12-31"), by = "week"), 7))
setnames(weeks1, "V1", "Week_end_date")
weeks1 <- weeks1[order(Week_end_date)]
dates <- as.data.table(seq(as.Date("2018-05-05"), as.Date("2021-12-31"), by = "day"))
setnames(dates, "V1", "date")
weeks_dates <- cbind(weeks1, dates)


##merge by week_end_dates
day_pm_pop$date <- as.Date(day_pm_pop$date)
day_pm_pop <- merge(day_pm_pop, weeks_dates, by = c("date")) 
#setnames(ds_vahi_day, "date", "Week_end_date")

##merged table has all 1337 days, numbered week end dates @ SA2 and total separations
ds_vahi_day_sa2 <- merge(ds_vahi_day, day_pm_pop, by = c("Week_end_date", "SA2_NAME16", "SA3_CODE16", "SA3_NAME16", "pop"), allow.cartesian = T)

###merge in bl_separations by week no., calendar year, SA3_CODE16

diff_vahi_day_sa2 <- merge(ds_vahi_day_sa2, an_mean_ds_vahi, by = c("no", "Calendar year", "SA3_CODE16", "Diagnosis chapter desc"))
diff_vahi_day_sa2$perc_counts <- diff_vahi_day_sa2$Total_separations/diff_vahi_day_sa2$bl_separations*100
plot(diff_vahi_day_sa2$no, diff_vahi_day_sa2$perc_counts)

diff_vahi_day_sa4 <- diff_vahi_day_sa2[,.(Total_separations = sum(Total_separations), bl_separations = sum(bl_separations), perc_counts = mean(perc_counts)), by= c("no", "Week_end_date", "`Diagnosis chapter desc`", "SA4_NAME16")]


sub <- diff_vahi_day_sa4[`Diagnosis chapter desc` == "Diseases of the Respiratory System" & SA4_NAME16 == "Melbourne - North East" ,.(Week_end_date, Total_separations, bl_separations, perc_counts)]
sub <- unique(sub[order(Week_end_date)])
plot(sub$Week_end_date, sub$perc_counts, type = "l", xlab = "Week", ylab = " hospitalisation counts (% baseline)")

state <- diff_vahi_day_sa4[`Diagnosis chapter desc` == "Diseases of the Respiratory System"  ,.(perc = mean(perc_counts), bl = sum(bl_separations), counts = sum(Total_separations)), by = c("Week_end_date")]
state <- unique(state[order(Week_end_date)])
plot(state$Week_end_date, state$perc, type = "l", xlab = "Week", ylab = " hospitalisation counts (% baseline)")

###put master tabe together for BI dashboard 1 "daily time series of PM2.5 and health outcomes" SA2_NAME16, Date, Year, PM2.5, Wildfire PM, FRB PM, Non land PM, Excess mortality risk(%), CVD hospitalisations (% Baseline), Respiratory hospitalisations (% Baseline) 
names(day_pm_pop)
names(day_pm_SA2)
names(diff_vahi_day_sa2)
diff_vahi_day_sa2 <- diff_vahi_day_sa2[order(date)]
sa2 <- unique(diff_vahi_day_sa2$date)



BI <- merge(day_pm_pop[,c("SA2_MAIN16","date", "SA2_NAME16", "pop")],
            day_pm_SA2[,c("SA2_MAIN16", "date", "value", "risk")],by = c("date", "SA2_MAIN16"))
BI <- merge(BI,  diff_vahi_day_sa2[`Diagnosis chapter desc` == "Diseases of the Respiratory System", c("SA2_MAIN16", "date", "perc_counts")],by = c("date", "SA2_MAIN16"))
BI <- merge(BI, diff_vahi_day_sa2[`Diagnosis chapter desc` == "Diseases of the Circulatory System", c("SA2_MAIN16", "date", "perc_counts")], by = c("date", "SA2_MAIN16"))
setnames(BI, "value", "PM2.5(ug/m3)")
setnames(BI, "perc_counts.x", "Respiratory hosp%")
setnames(BI, "perc_counts.y", "Circulatory hosp%")
setnames(BI, "risk", "Excess mortality risk")

BI$WildfirePM <- BI$`PM2.5(ug/m3)`-13
BI$WildfirePM <- ifelse(BI$WildfirePM <0, 0, BI$WildfirePM)
BI$nonfirePM <- ifelse(BI$`PM2.5(ug/m3)`>10, 10, BI$`PM2.5(ug/m3)`)
BI$BurnPM <- ifelse(grepl("-05-", BI$date), BI$WildfirePM, 0)  
qc1 <- BI[BurnPM > 0]

write.csv(BI, "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/Daily_time_series_finalBI.csv")
quantile(BI$BurnPM)




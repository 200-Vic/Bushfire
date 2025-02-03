library(raster)
#install.packages("ncdf4")
library(ncdf4)
library(terra)
library(sf)
library(rgdal)
library(exactextractr)
library(dplyr)
library(data.table)

setwd("C:/privateRlab/Bushfire")

gridpwa <- TRUE
if(gridpwa){
SA2 <- st_read("data/SA2_2016_VIC.shp") %>% st_transform('+proj=longlat +datum=WGS84')
pop <- raster("data/apg16e1_1_1_0.tif") 

#cut population grid to victoria
vic_shp <- st_read("data/VIC.shp")
pop_vic <- exact_extract(pop, vic_shp, fun = NULL, include_cell = TRUE, include_xy = TRUE, force_df = TRUE, stack_apply = FALSE)
pop_vic <- rasterFromXYZ(as.data.frame(pop_vic)[, c("x", "y", "value")])
plot(pop_vic)
d <- res(pop_vic)

nc_rasters <- raster::brick("pm25_all_Vic_Jan2003_Jun2022_20230721.nc")
s <- res(nc_rasters)

#aggregate population to 5km to match air pollution rasters
pop5 <- aggregate(pop_vic, 5, fun=sum)
crs(pop5) <- crs(nc_rasters)

##do population weighting with gridded populations

d <- res(pop5)
days <- exact_extract(nc_rasters, SA2, "weighted_mean", weights = pop5, append_cols = "SA2_MAIN16", stack_apply = FALSE)
#qc <- exact_extract(nc_rasters, SA2, "mean", append_cols = "SA2_MAIN16", stack_apply = FALSE)
# check_diff <- qc$mean.X2003.01.01.12.00.00 - days$weighted_mean.X2003.01.01.12.00.00
day_dt <- as.data.table(days)
dt <- melt(day_dt, id.vars = c("SA2_MAIN16"))
setnames(dt, "variable", "date")
dt$date <- as.character(substr(dt$date, 16, 25))
dt$date <- gsub(".", "/", dt$date, fixed = TRUE)
dt$date <- as.Date(dt$date, "%Y/%m/%d")
}

##@@@@@@@@@@@@@@@@@@@@do population weighting using meshblock to SA2 instead
##load spatial and people data
MB <- readOGR("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Spatial_ABS/MB_2016_VIC.shp") 
#%>% st_transform('+proj=longlat +datum=WGS84')
mb_pop <- read.csv("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Population_ABS/2016 census mesh block counts_no_footer.csv")
setnames(mb_pop, "MB_CODE_2016", "MB_CODE16")
mb_pop <- mb_pop[, c("MB_CODE16", "Person")]

##load rasterbrick
nc_rasters <- raster::brick("pm25_all_Vic_Jan2003_Jun2020_20230916_ranger_car.nc")

mb_days <- exact_extract(nc_rasters, MB, "mean", append_cols = "MB_CODE16")



# write.csv(day_dt, "data/day_dt_mb.csv")
# ###saved mb extract to clear environment and relieve data constraints

# day_dt <- read.csv("data/day_dt_mb.csv")
###@@@@@@Checking for meshblocks that get zeros 
day_dt <- as.data.table(mb_days)
qc <- melt(day_dt, id.vars = c("MB_CODE16"))
qc1 <- qc[,.(mean_x = mean(value, na.rm=T)), by = c("MB_CODE16")]
qc2 <- qc1[mean_x > 0]
drop <- nrow(qc1)-nrow(qc2)
mb_todo <- unique(qc2$MB_CODE16)
missedmb <- qc1[!MB_CODE16 %in% mb_todo]
missedmb$MB_CODE16 <- as.numeric(missedmb$MB_CODE16)
qc_pop <- merge(missedmb, mb_pop, by = c("MB_CODE16"))
drop_pop <- sum(qc_pop$Person)

#MB_dt <- st_read("C:/Users/CastonT/OneDrive - Environment Protection Authority Victoria/Documents - Environmental Public Health Branch (internal) SharePoint/Data/Spatial_ABS/MB_2016_VIC.shp")
mb_pop1 <- merge(MB@data[, c("MB_CODE16", "SA2_MAIN16")], mb_pop, by = c("MB_CODE16"))
dt_all <- merge(mb_pop1, day_dt,  by = c("MB_CODE16"))
dt_all$MB_CODE16 <- as.character(dt_all$MB_CODE16)
dt_all <- as.data.table(dt_all)
#setnames(dt_all, "SA2_MAIN16.x", "SA2_MAIN16") 

##@@@@@@@@@@@@do population weighted average aggregate from MB to SA2

sa2_todo <- unique(dt_all$SA2_MAIN16)

for(sa2_i in 1:length(sa2_todo)){
#sa2_i = 1
  SA2 <- dt_all[SA2_MAIN16 == sa2_todo[sa2_i]]
  pwa <- melt(SA2, id.vars = c("MB_CODE16", "Person", "SA2_MAIN16"))
  setnames(pwa, "variable", "date")
  pwa$date <- as.character(substr(pwa$date, 7, 16))
  pwa$date <- gsub(".", "/", pwa$date, fixed = TRUE)
  pwa$date <- as.Date(pwa$date, "%Y/%m/%d")
  pwa$value <- as.numeric(pwa$value)
  pwa1 <- pwa[,.(Person = sum(Person), value = sum(value*Person)/sum(Person)), by = c("SA2_MAIN16", "date")]
  
  if(sa2_i == 1){
    dt <- pwa1
  }else{
    dt <- rbind(dt, pwa1)  
}}

quantile(dt$value, na.rm=T)
#all      0%        25%        50%        75%       100% 
#1.401988   4.547086   5.473774   7.137502 373.376596 
#bushfire    0%      25%      50%      75%     100% 
#0.0000   0.0000   0.0000   0.0000 367.9104
#non_fire      0%        25%        50%        75%       100% 
#1.401988   4.543509   5.454490   7.005231 109.773189
#prescribed burn     0%      25%      50%      75%     100% 
# 0.00000  0.00000  0.00000  0.00000 72.68059 

nrow(is.na(dt$date))
# all <-  dt
# setnames(all, "value", "all")
# bushfire <- dt
# setnames(bushfire, "value", "bushfire")
# non_fire <- dt
# setnames(non_fire, "value", "non_fire")
prescribed_burn <- dt
setnames(prescribed_burn, "value", "burn")

landpm <- cbind(all, bushfire$bushfire, non_fire$non_fire, prescribed_burn$burn)
setnames(landpm, "V2", "fire")
setnames(landpm, "V3", "anth")
setnames(landpm, "V4", "burn")
# landpm$fireprop <- landpm$fire/landpm$all
# landpm$anthprop <- landpm$anth/landpm$all
# landpm$burnprop <- landpm$burn/landpm$all
plot(landpm$date, landpm$all)
plot(landpm$date, landpm$fire)
plot(landpm$date, landpm$burn)
plot(landpm$date, landpm$anth)

write.csv(landpm, "ranger_fire_PM3.csv")
landpm <- read.csv("ranger_fire_PM3.csv")




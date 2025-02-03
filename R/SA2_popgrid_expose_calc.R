library(raster)
library(rgdal)
library(sf)
library(sp)
library(data.table)
library(dplyr)
library(exactextractr)
library(terra)
library(dplyr)

##Population grid
pop <- raster("data/apg16e1_1_1_0.tif")

##crop grid to Victoria only
vic_shp <- st_read("data/VIC.shp")
pop_vic <- exact_extract(pop, vic_shp, fun = NULL, include_cell = TRUE, include_xy = TRUE, force_df = TRUE, stack_apply = FALSE)
pop_vic <- rasterFromXYZ(as.data.frame(pop_vic)[, c("x", "y", "value")])
crs(pop_vic) <- crs(pop)
#plot(pop_vic)
#d <- res(pop_vic)


###@@@@@@@@SETUP LOOP to crop all SatLUR to VIC, project to pop grid and save as geotif@@@@@@
indir_hd <- "C:/privateRlab/Bushfire"
indir_dat <- "rangeryears"
rawfiles_lur <- dir(file.path(indir_hd, indir_dat), pattern = "ALL")

rast_todo <- file.path(rawfiles_lur)

for(i in length(rast_todo)){
  i = 1
  rast_i <- rast_todo[i]
  pol <- raster(file.path(indir_hd, indir_dat, rast_i))
  pol_vic <- exact_extract(pol, vic_shp, fun = NULL, include_cell = TRUE, include_xy = TRUE, force_df = TRUE, stack_apply = FALSE)
  pol_vic <- rasterFromXYZ(as.data.frame(pol_vic)[, c("x", "y", "value")])
  crs(pol_vic) <- crs(pol)
  run_label <- substr(rast_i, 11, 14)
  
  pol_vic <- projectRaster(pol_vic, pop_vic, method="bilinear",
                           alignOnly=FALSE, over=FALSE)
  writeRaster(pol_vic, filename = paste0(run_label, "vic"), "tif", format ="GTiff", overwrite=TRUE)
}
plot(pol_vic)




#####@@@@@@@@@CALCULATE POP WEIGHTED AVERAGES@@@@@@@@@


# plot(pol_vic)
##SA2_MAIN16 boundaries 694 in Victoria
shp <- st_read("data/SA2_2016_VIC.shp")
st_transform(shp, '+proj=longlat +datum=WGS84')

##get SA2_MAIN16 centroid coordinates usingsp
ogr <- readOGR("data/SA2_2016_VIC.shp")
cent <- spTransform(ogr, CRS("+proj=longlat +ellps=WGS84 +init=epsg:4326"))
st_crs(cent)
cents <- coordinates(cent)
cents <- SpatialPointsDataFrame(coords=cents, data=cent@data,
                                proj4string=CRS("+proj=longlat +ellps=WGS84 +init=epsg:4326"))
st_crs(cents)
coords <- as.data.table(cents)
coords <- coords[, .(SA2_MAIN16, lon = coords.x1, lat = coords.x2)]
shp <- merge(shp, coords, by = c("SA2_MAIN16"))
###write.csv(coords, "data/_centroids.csv")

##set up loop to extract all rasters and cbind into areaXyear
indir_onedrive <- "C:/privateRlab/"
indir_results <- "Bushfire/rangeryears"
infiles_lur <- dir(file.path(indir_onedrive, indir_results), pattern = "_nonfire_")

rast_todo <- file.path(infiles_lur)

for(i in 1:length(rast_todo)){
  #i = 2
  rast_i <- rast_todo[i]
  pol_vic <- raster(file.path(indir_onedrive, indir_results, rast_i))
  pol_vic <- projectRaster(pol_vic, pop_vic, method="bilinear",
                           alignOnly=FALSE, over=FALSE)
  PC_pop_av <- exact_extract(pol_vic, shp, fun = "weighted_mean", weights = area(pop_vic), append_cols = TRUE) %>% as.data.table
  
  run_label <- substr(rast_i, 16,19)
  pwa <- PC_pop_av[,.(SA2_MAIN16, SA2_NAME16,SA3_CODE16, SA3_NAME16, SA4_CODE16, SA4_NAME16, GCC_CODE16, GCC_NAME16, AREASQKM16, lon, lat, weighted_mean)]
  #setnames(pwa, "weighted_mean", paste0("PWA", run_label))
  pwa$year <- run_label
  if (i == 1){
    time_series <- pwa
  } else {
    time_series <- rbind(time_series, pwa)
    
    # time_series <- merge(time_series, pwa, by = c("SA2_MAIN16", "PFI", "PFI_CR", "UFI", "UFI_CR", "UFI_OLD", "lon", "lat" ))
  }
}

write.csv(time_series, "C:/privateRlab/air-health-scenario-app/data/NF_pwa_sa22003-2020.csv")
tmrel <- quantile(time_series$weighted_mean)

all <- read.csv("C:/privateRlab/air-health-scenario-app/data/ALL_pwa_sa22003-2020.csv")
quantile(all$weighted_mean)

###plot all rasters

for(i in 1:length(rast_todo)){
  #i = 1
  rast_i <- rast_todo[i]
  pol_vic <- raster(file.path(indir_onedrive, indir_results, rast_i))
  plot(pol_vic)}


##Quality control compare with straight means##@@@@@@@@@@@@@@@
comppl_avs <- exact_extract(pol_vic, shp, fun = "mean")
comp <- cbind(PC_pop_av[,c("SA2_MAIN16", "weighted_mean")], comppl_avs)
comp$diff <- comp$weighted_mean-comp$comppl_avs
quantile(comp$diff)


#time_series <- read.csv("C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/PM25_annualave_SA2.csv")

time_series <- as.data.table(time_series)
tmrel <- quantile(time_series$weighted_mean, 0.05, na.rm=T)
###All pollution fake CRFs
time_series$lower_mort <- (time_series$weighted_mean-tmrel)*10*0.04
time_series$est_mort <- (time_series$weighted_mean-tmrel)*10*0.062
time_series$upper_mort <- (time_series$weighted_mean-tmrel)*10*0.083
time_series$lower_CVD <- (time_series$weighted_mean-tmrel)*10*0.05
time_series$est_CVD <- (time_series$weighted_mean-tmrel)*10*0.1
time_series$upper_CVD <- (time_series$weighted_mean-tmrel)*10*0.15
time_series$lower_respiratory <- (time_series$weighted_mean-tmrel)*10*0.03
time_series$est_respiratory <- (time_series$weighted_mean-tmrel)*10*0.06
time_series$upper_respiratory <- (time_series$weighted_mean-tmrel)*10*0.09
###Domestic heating
time_series$dom_heating <- time_series$weighted_mean*0.45
time_series$lower_mort_heating <- (time_series$dom_heating)*10*0.04
time_series$est_mort_heating <- (time_series$dom_heating)*10*0.062
time_series$upper_mort_heating <- (time_series$dom_heating)*10*0.083
time_series$lower_CVD_heating <- (time_series$dom_heating)*10*0.05
time_series$est_CVD_heating <- (time_series$dom_heating)*10*0.1
time_series$upper_CVD_heating <- (time_series$dom_heating)*10*0.15
time_series$lower_respiratory_heating <- (time_series$dom_heating)*10*0.03
time_series$est_respiratory_heating <- (time_series$dom_heating)*10*0.06
time_series$upper_respiratory_heating <- (time_series$dom_heating)*10*0.09
##industry
time_series$industry <- time_series$weighted_mean*0.2
time_series$lower_mort_industry <- (time_series$industry)*10*0.04
time_series$est_mort_industry <- (time_series$industry)*10*0.062
time_series$upper_mort_industry <- (time_series$industry)*10*0.083
time_series$lower_CVD_industry <- (time_series$industry)*10*0.05
time_series$est_CVD_industry <- (time_series$industry)*10*0.1
time_series$upper_CVD_industry <- (time_series$industry)*10*0.15
time_series$lower_respiratory_industry <- (time_series$industry)*10*0.03
time_series$est_respiratory_industry <- (time_series$industry)*10*0.06
time_series$upper_respiratory_industry <- (time_series$industry)*10*0.09
##traffic
time_series$traffic <- time_series$weighted_mean*0.35
time_series$lower_mort_traffic <- (time_series$traffic)*10*0.04
time_series$est_mort_traffic <- (time_series$traffic)*10*0.062
time_series$upper_mort_traffic <- (time_series$traffic)*10*0.083
time_series$lower_CVD_traffic <- (time_series$traffic)*10*0.05
time_series$est_CVD_traffic <- (time_series$traffic)*10*0.1
time_series$upper_CVD_traffic <- (time_series$traffic)*10*0.15
time_series$lower_respiratory_traffic <- (time_series$traffic)*10*0.03
time_series$est_respiratory_traffic <- (time_series$traffic)*10*0.06
time_series$upper_respiratory_traffic <- (time_series$traffic)*10*0.09

SA2_BI <- time_series

write.csv(SA2_BI[,.(SA2_MAIN16, SA2_NAME16,SA3_CODE16, SA3_NAME16, SA4_CODE16, SA4_NAME16, GCC_CODE16, GCC_NAME16, AREASQKM16,lon, lat, year, weighted_mean, lower_mort,  est_mort, upper_mort,  lower_CVD, est_CVD, upper_CVD, lower_respiratory, est_respiratory, upper_respiratory, dom_heating,lower_mort_heating, est_mort_heating, upper_mort_heating, lower_CVD_heating, est_CVD_heating, upper_CVD_heating, lower_respiratory_heating, est_respiratory_heating, upper_respiratory_heating, industry, lower_mort_industry, est_mort_industry, upper_mort_industry, lower_CVD_industry, est_CVD_industry, upper_CVD_industry, lower_respiratory_industry, est_respiratory_industry, upper_respiratory_industry, traffic, lower_mort_traffic, est_mort_traffic, upper_mort_traffic, lower_CVD_traffic, est_CVD_traffic, upper_CVD_traffic, lower_respiratory_traffic, est_respiratory_traffic, upper_respiratory_traffic)], "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/BI_Data12052023/Dashboard 2 -PM25_annual_SA2_BIreport2.csv")

# write.csv(time_series[,.(SA2_MAIN16, lon, lat, weighted_mean, year, lower_burden, upper_burden)], "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/Power BI lab/Current project/PM25_annualave_SA2.csv")
# 
# write.csv(time_series[,.(SA2_MAIN16, lon, lat, weighted_mean, year, lower_burden, upper_burden)], "C:/privateRlab/air-health-scenario-app/data/NSW_SatLUR/PM25_annualave_SA2.csv")

pc <- time_series[SA2_MAIN16 == 3056]
quantile(pc$weighted_mean)

##@@@@@@@Events based on a statewide 95th percentile threshold
thresh <- quantile(time_series$weighted_mean, 0.95)
events <- time_series[weighted_mean > thresh]
unique(events$year)
unique(events$SA2_MAIN16)

##@@@@@@Events based on xxx SA2_MAIN16 95th percentile thresholds
event_po <- time_series[,.(weighted_mean, year, thresh = quantile(weighted_mean, 0.95, by = c("SA2_MAIN16", "lon", "lat")))]
quantile(time_series$thresh)

##@@@@@@@@@@@@@@@@@@@@@Calculate multiyear average air pollution, determine whether change is significant over years 2005-2022 and calculate delta air pollution if significant@@@@@@@


time_series <- read.csv("data/pm25_time_series_sa1.csv")
time_series <- as.data.table(time_series, na.rm=T)


###calculate 95th percentile SA2 annual 

q95 <- as.numeric(quantile(time_series$weighted_mean, 0.95, na.rm=T))
time_series1 <- time_series[weighted_mean <= q95]#discard PM major events 
delta_ind <- time_series1[!weighted_mean == "NaN"]#discard NaN in NO2 time_series
delta_ind$year <- as.numeric(delta_ind$year)#make year numeric for linear model calculation below
qc <- delta_ind[SA1_MAIN16 == "20605112919"]



sa1_todo <- unique(delta_ind$SA1_MAIN16)

for(i in 1:length(sa1_todo)){
  #i = 3066
  sa1_i <- sa1_todo[i]
  lm_sa1 <- delta_ind[SA1_MAIN16 %in% sa1_i]
  p <- cor.test(lm_sa1$weighted_mean, lm_sa1$year)
  lm <- lm(weighted_mean ~ year, lm_sa1)
  lm_f <- as.data.table(lm$fitted.values)
  setnames(lm_f, "V1", "fit")
  lm_r <- as.data.table(lm$residuals)
  setnames(lm_r, "V1", "res")
  lm_fr <- cbind(lm_f, lm_r, unique(lm_sa1$year))
  delta_lm <- ifelse(p$p.value < 0.05, max(lm_fr$fit)-min(lm_fr$fit), 0)
  delta_lm <- as.data.table(delta_lm[1])
  setnames(delta_lm, "V1", "delta")
  
  lm_sa1 <- unique(lm_sa1[,.(SA2_MAIN16, SA2_NAME16, SA3_CODE16, SA3_NAME16, SA4_CODE16, SA4_NAME16, GCC_CODE16, GCC_NAME16, AREASQKM16, lon, lat, baseline_value = mean(weighted_mean)), by = c("SA1_MAIN16", "SA1_7DIG16")])
  lm_sa1$delta <- delta_lm$delta
  print(lm_sa1$delta)
  if (i == 1){
    bl_yearlm_indicator <- lm_sa1
  } else {
    bl_yearlm_indicator <- rbind(bl_yearlm_indicator, lm_sa1)
  }
}
qc <- bl_yearlm_indicator[delta > 0]
quantile(bl_yearlm_indicator$delta)
plot(bl_yearlm_indicator$SA1_7DIG16, bl_yearlm_indicator$baseline_value)
write.csv(bl_yearlm_indicator, "data/long_pm25_SA1.csv")

qc <- bl_yearlm_indicator[delta > 0]
g <- delta_ind[SA1_MAIN16 == "21003143841"]
plot(g$year, g$weighted_mean)
cor.test(g$year, g$weighted_mean)

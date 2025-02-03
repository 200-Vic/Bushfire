library(raster)
library(rgdal)
library(sf)
library(sp)
library(data.table)
library(dplyr)
library(exactextractr)
library(terra)
library(dplyr)

#setwd("C:/privateRlab/extract")
##Population grid
pop <- raster("data/apg16e1_1_1_0.tif")

#shp <- st_read("data/VIC_SA1.shp")


indir_hd <- "D:/Knibbs updated"
indir_dat <- "PM25_2005_21_copy"
rawfiles_lur <- dir(file.path(indir_hd, indir_dat), pattern = "PM25_")

rast_todo <- file.path(rawfiles_lur)

for(i in seq(2, length(rast_todo[2:68]), 4)){
  i = 66
  rast_i <- rast_todo[i]
  pol <- raster(file.path(indir_hd, indir_dat, rast_i))
  # pol_vic <- exact_extract(pol, shp, fun = NULL, include_cell = TRUE, include_xy = TRUE, force_df = TRUE, stack_apply = FALSE)
  # pol_vic <- rasterFromXYZ(as.data.frame(pol_vic)[, c("x", "y", "value")])
  # crs(pol_vic) <- crs(pol)
  run_label <- substr(rast_i, 1, 9)
  
  pol <- projectRaster(pol, pop, method="bilinear",
                           alignOnly=FALSE, over=FALSE)
  writeRaster(pol, filename = paste0(run_label, "AUS"), "tif", format ="GTiff", overwrite=TRUE)
rm(pol)
  }
plot(pol)
##crop grid to Victoria only
#vic_shp <- st_read("data/VIC.shp")
# pop_vic <- exact_extract(pop, vic_shp, fun = NULL, include_cell = TRUE, include_xy = TRUE, force_df = TRUE, stack_apply = FALSE)
# pop_vic <- rasterFromXYZ(as.data.frame(pop_vic)[, c("x", "y", "value")])
# crs(pop_vic) <- crs(pop)


##get SA1_MAIN16 centroid coordinates using sp
ogr <- readOGR("C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/Data/Spatial_ABS/SA1_2016_AUST.shp")
cent <- spTransform(ogr, CRS("+proj=longlat +ellps=WGS84 +init=epsg:4326"))
st_crs(cent)
cents <- coordinates(cent)
cents <- SpatialPointsDataFrame(coords=cents, data=cent@data,
                                proj4string=CRS("+proj=longlat +ellps=WGS84 +init=epsg:4326"))
st_crs(cents)
coords <- as.data.table(cents)
coords <- coords[, .(SA1_MAIN16, lon = coords.x1, lat = coords.x2)]

shp <- st_read("C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/Data/Spatial_ABS/SA1_2016_AUST.shp")
st_transform(shp, '+proj=longlat +datum=WGS84')
shp <- merge(shp, coords, by = c("SA1_MAIN16"))
###write.csv(coords, "data/_centroids.csv")

##set up loop to extract all rasters and cbind into areaXyear
indir_onedrive <- "C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/Data"
indir_results <- "Pollution_Knibbs/data_derived/extract/Australia"
infiles_lur <- dir(file.path(indir_onedrive, indir_results), pattern = "NO2_")

rast_todo <- file.path(infiles_lur)

for(i in 1:length(rast_todo)){
  #i = 1
  rast_i <- rast_todo[i]
  pol <- raster(file.path(indir_onedrive, indir_results, rast_i))
  PC_pop_av <- exact_extract(pol, shp, fun = "weighted_mean", weights = area(pop), append_cols = TRUE) %>% as.data.table
  
  run_label <- substr(rast_i, 5, 8)
  pwa <- PC_pop_av[,.(SA1_MAIN16, SA1_7DIG16, SA2_MAIN16, SA2_NAME16, SA3_CODE16, SA3_NAME16, SA4_CODE16, SA4_NAME16, GCC_CODE16, GCC_NAME16, AREASQKM16, lon, lat, weighted_mean)]
  #setnames(pwa, "weighted_mean", paste0("PWA", run_label))
  pwa$year <- run_label
  if (i == 1){
    time_series <- pwa
  } else {
    time_series <- rbind(time_series, pwa)
  }
}
write.csv(time_series, "data/no2_time_series_sa1_nat.csv")


##@@@@@@@@@@@@@@@@@@@@@Calculate multiyear average air pollution, determine whether change is significant over years 2005-2022 and calculate delta air pollution if significant@@@@@@@


time_series <- read.csv("data/no2_time_series_sa1_nat.csv")
time_series <- as.data.table(time_series, na.rm=T)


###calculate 95th percentile SA2 annual 

q95 <- as.numeric(quantile(time_series$weighted_mean, 0.95, na.rm=T))
time_series1 <- time_series#[weighted_mean <= q95]#discard PM major events 
delta_ind <- time_series1[!weighted_mean == "NaN"]#discard NaN in NO2 time_series
delta_ind$year <- as.numeric(delta_ind$year)#make year numeric for linear model calculation below
qc <- delta_ind[SA1_7DIG16 == "1100701"]


  
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
#print(lm_sa1$delta)
if (i == 1){
  bl_yearlm_indicator <- lm_sa1
} else {
  bl_yearlm_indicator <- rbind(bl_yearlm_indicator, lm_sa1)
}
}
qc <- bl_yearlm_indicator[delta > 0]
quantile(bl_yearlm_indicator$delta)
plot(bl_yearlm_indicator$SA1_7DIG16, bl_yearlm_indicator$baseline_value)
write.csv(bl_yearlm_indicator, "data/long_NO2_SA1_Aus.csv")

qc <- bl_yearlm_indicator[delta > 0]
g <- delta_ind[SA1_MAIN16 == "21003143841"]
plot(g$year, g$weighted_mean)
cor.test(g$year, g$weighted_mean)

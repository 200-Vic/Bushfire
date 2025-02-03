library(raster)
#install.packages("ncdf4")
library(ncdf4)
library(terra)
library(sf)
library(rgdal)
library(exactextractr)
library(dplyr)
library(data.table)


bl_rasters <- raster::brick("pm25_all_Vic_Jan2003_Jun2020_20231008_ranger_car.nc")
bl_h <-  stack(bl_rasters)

bl_nms <- names(bl_h)
bl_yrs <- unique(substr(bl_nms, 2, 5))
bl_annual_means <- lapply(bl_yrs,function(x) mean(bl_h[[grep(x,bl_nms)]],na.rm=TRUE))

nc_rasters <- raster::brick("pm25_non_fire_Vic_Jan2003_Jun2020_20231008_ranger_car.nc")
h <-  stack(nc_rasters)

nms <- names(h)
yrs <- unique(substr(nms, 2, 5))
annual_means <- lapply(yrs,function(x) mean(h[[grep(x,nms)]],na.rm=TRUE))
plot(annual_means[[13]])


fnames <- lapply(yrs, function(y) paste0("C:/privateRlab/bushfire/rangeryears/ranger_nonfire", "_", y, ".tif"))



for(i in 1:length(annual_means)){
#i = 1
#diff_means <- bl_annual_means[[i]] - annual_means[[i]]
  #writeRaster(diff_means, filename = fnames[[i]], format = "GTiff", overwrite = TRUE)
  
  writeRaster(annual_means[[i]], filename = fnames[[i]], format = "GTiff", overwrite = TRUE)
  }

  # lapply(annual_means, function(z) {
  #   writeRaster(z, filename = fnames, format = "GTiff", overwrite = TRUE)
  # })


#C:/privateRlab/air-health-scenario-app/data/rangerpm/prescribed burn.tif

all <- unique(substr(nms, 1, 1))
grand_mean_all <- lapply(all, function(x) mean(bl_h[[grep(x,nms)]],na.rm=TRUE))

grand_mean <- lapply(all, function(x) mean(h[[grep(x,nms)]],na.rm=TRUE))

plot(grand_mean_all[[1]])
plot(grand_mean[[1]])

cf <- grand_mean_all[[1]]- grand_mean[[1]]

plot(cf[[1]])
writeRaster(grand_mean[[1]], "C:/privateRlab/Bushfire/rangeryears/NF_2003-2020.tif", "GTiff", overwrite=TRUE)
lost
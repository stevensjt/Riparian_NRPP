##Jens Stevens; stevensjt@gmail.com; created 3/1/20
##Goal of this script is to incrementally adjust the elevation rasters pre- and post-disturbance so that the offset is minimal

####0. Read libraries####
library(raster)
library(rgdal)


####1. Read and process spatial data: Process every subcatchment sequentially####
dem_10_bil <- #read 2010 lidar data, bilinear interpolation to get on same grid as 2016 data
  raster("./large_files/DEM_2010single_FrijolesWatershed_30mbuff_1mbilinear/DEM_2010single_FrijolesWatershed_30mbuff_1mbilinear.tif")
dem_16_tmp <- #read 2016 lidar data
  raster("./large_files/DEM_2016_FrijolesWatershed_30mbuff/DEM_2016_FrijolesWatershed_30mbuff_fixNoData.tif")
reach_boundaries <- #read subwatershed boundaries
  readOGR("./GIS/WatershedsBAND2_polygon_byReach/WatershedsBAND2_polygon_byReach.shp")
reach_boundaries <- #filter subwatershed boundaries to only include Frijoles, matching lidar data
  reach_boundaries[reach_boundaries$Canyon == "Frijoles",]

#Below, fix an issue with pixels outside the watershed being assigned a very large number in 2016
#This is very slow so only do it once, save new file and load that going forward
#dem_16[dem_16 > 4000] <- NA 
#writeRaster(dem_16, "./large_files/DEM_2016_FrijolesWatershed_30mbuff/DEM_2016_FrijolesWatershed_30mbuff_fixNoData.tif", overwrite = FALSE)

####2. Iteratively process every subwatershed####
for(sw in reach_boundaries$StreamReac){
  sw_poly <- reach_boundaries[reach_boundaries$StreamReac==sw,] #'sw' = subwatershed
  sw_2010 <- crop(mask(dem_10_bil,sw_poly),sw_poly) #slow
  sw_2016 <- crop(mask(dem_16,sw_poly),sw_poly) #slow
  
  s <- stack(sw_2010,sw_2016)
  d <- sw_2016 - sw_2010 #difference raster
  v <- getValues(d) #difference values
  
  d_mod <- d
  flags <- which(abs(v) > 20 | abs(v) < 0.5)
  d_mod[flags] <- NA
  v_mod <- getValues(d_mod)
  #plot(d)
  mean_dif <- round(mean(v,na.rm=T),2); median_dif <- round(median(v,na.rm=T),2)
  median_abs_dif <- round(median(abs(v),na.rm=T),2)
  plot(d_mod,main=paste0("shift x=0px, y=0px\n median dif = ",median_dif,"; \nmean dif = ", mean_dif,
                         "; \nmedian abs value dif = ", median_abs_dif))
  
  for(x_margin in (c(-1, 1))){
    for(y_margin in (c(-1, 1)){
      #START HERE pull in code from below, add lists etc.
      
    })
  }
}



####OLD:####
#START HERE, incorporate this into the loop above
#CHECKME the calculations of the mean, median difference, etc were done both in the baseline and below on the unfiltered v's, I'm not entirely sure if that is the appropriate way to evaluate. I think so?

####2. Shift without filtering####
x_inc <- 1; y_inc <- 0
dem_16_shift <- shift(dem_16, dx = x_inc, dy = y_inc)
d <- dem_16_shift - dem_10_bil #difference raster
v <- getValues(d) #difference values
#modify the raster to exclude large outliers
d_mod <- d
flags <- which(abs(v) > 20 | abs(v) < 0.5)
d_mod[flags] <- NA
#plot(d)
mean_dif <- round(mean(v,na.rm=T),2); median_dif <- round(median(v,na.rm=T),2)
median_abs_dif <- round(median(abs(v),na.rm=T),2)
plot(d_mod,main=paste0("shift x=1px, y=0px\n median dif = ",median_dif,"; \nmean dif = ", mean_dif,
                       "; \nmedian abs value dif = ", median_abs_dif))

#

x_inc <- -1; y_inc <- 0
dem_16_shift <- shift(dem_16, dx = x_inc, dy = y_inc)
d <- dem_16_shift - dem_10_bil #difference raster
v <- getValues(d) #difference values
#modify the raster to exclude large outliers
d_mod <- d
flags <- which(abs(v) > 20 | abs(v) < 0.5)
d_mod[flags] <- NA
#plot(d)
mean_dif <- round(mean(v,na.rm=T),2); median_dif <- round(median(v,na.rm=T),2)
median_abs_dif <- round(median(abs(v),na.rm=T),2)
plot(d_mod,main=paste0("shift x= ", x_inc, "px, y=", y_inc, "px\n median dif = ",median_dif,
                       "; \nmean dif = ", mean_dif,
                       "; \nmedian abs value dif = ", median_abs_dif))

x_inc <- 0; y_inc <- -1
dem_16_shift <- shift(dem_16, dx = x_inc, dy = y_inc)
d <- dem_16_shift - dem_10_bil #difference raster
v <- getValues(d) #difference values
#modify the raster to exclude large outliers
d_mod <- d
flags <- which(abs(v) > 20 | abs(v) < 0.5)
d_mod[flags] <- NA
#plot(d)
mean_dif <- round(mean(v,na.rm=T),2); median_dif <- round(median(v,na.rm=T),2)
median_abs_dif <- round(median(abs(v),na.rm=T),2)
plot(d_mod,main=paste0("shift x= ", x_inc, "px, y=", y_inc, "px\n median dif = ",median_dif,
                       "; \nmean dif = ", mean_dif,
                       "; \nmedian abs value dif = ", median_abs_dif))


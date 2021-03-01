##Jens Stevens; stevensjt@gmail.com; created 3/1/20
##Goal of this script is to incrementally adjust the elevation rasters pre- and post-disturbance so that the offset is minimal

####0. Read libraries####
library(raster)


####1. Read and evaluate raster data####
dem_10_bil <- raster("./GIS/DEM_2010_Frijoles01Watershed_30mbuff_1mbilinear/DEM_2010_Frijoles01Watershed_30mbuff_1mbilinear.tif")
dem_16 <- raster("./GIS/DEM_2016_Frijoles01Watershed_30mbuff/DEM_2016_Frijoles01Watershed_30mbuff.tif")

s <- stack(dem_10_bil,dem_16)

d <- dem_16 - dem_10_bil #difference raster
v <- getValues(d) #difference values

#modify the raster to exclude large outliers
d_mod <- d
flags <- which(abs(v) > 20 | abs(v) < 0.5)
d_mod[flags] <- NA
v_mod <- getValues(d_mod)
#plot(d)
mean_dif <- round(mean(v,na.rm=T),2); median_dif <- round(median(v,na.rm=T),2)
median_abs_dif <- round(median(abs(v),na.rm=T),2)
plot(d_mod,main=paste0("shift x=0px, y=0px\n median dif = ",median_dif,"; \nmean dif = ", mean_dif,
                       "; \nmedian abs value dif = ", median_abs_dif))




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

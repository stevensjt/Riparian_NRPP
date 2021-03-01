##Jens Stevens; stevensjt@gmail.com; created 3/1/20
##Goal of this script is to incrementally adjust the elevation rasters pre- and post-disturbance so that the offset is minimal

####0. Read libraries####
library(raster)


####1. Read and evaluate raster data####
dem_10_bil <- raster("./GIS/DEM_2010_Frijoles01Watershed_30mbuff_1mbilinear/DEM_2010_Frijoles01Watershed_30mbuff_1mbilinear.tif")
dem_16 <- raster("./GIS/DEM_2016_Frijoles01Watershed_30mbuff/DEM_2016_Frijoles01Watershed_30mbuff.tif")

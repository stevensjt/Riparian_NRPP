##Jens Stevens; stevensjt@gmail.com; created 3/1/20
##Goal of this script is to incrementally adjust the elevation rasters pre- and post-disturbance so that the offset is minimal

####0. Read libraries####
library(raster)
library(rgdal)


####1. Read and process spatial data: Process every subcatchment sequentially####
dem_10_bil <- #read 2010 lidar data, bilinear interpolation to get on same grid as 2016 data
  raster("./large_files/DEM_2010single_FrijolesWatershed_30mbuff_1mbilinear/DEM_2010single_FrijolesWatershed_30mbuff_1mbilinear.tif")
dem_16 <- #read 2016 lidar data
  raster("./large_files/DEM_2016_FrijolesWatershed_30mbuff/DEM_2016_FrijolesWatershed_30mbuff_fixNoData.tif")
reach_boundaries <- #read subwatershed boundaries
  readOGR("./GIS/WatershedsBAND2_polygon_byReach/WatershedsBAND2_polygon_byReach.shp")
reach_boundaries <- #filter subwatershed boundaries to only include Frijoles, matching lidar data
  reach_boundaries[reach_boundaries$Canyon == "Frijoles",]
df_analyze <- reach_boundaries@data
#Below, fix an issue with pixels outside the watershed being assigned a very large number in 2016
#This is very slow so only do it once, save new file and load that going forward
#dem_16[dem_16 > 4000] <- NA 
#writeRaster(dem_16, "./large_files/DEM_2016_FrijolesWatershed_30mbuff/DEM_2016_FrijolesWatershed_30mbuff_fixNoData.tif", overwrite = FALSE)

####2. Iteratively process every subwatershed####
offset_tracker <- data.frame(reach = sw, x_inc = NA, y_inc = NA, median_abs_dif = NA)
Sys.time()#track time
for(sw in reach_boundaries$StreamReac){ #sw = sub-watershed boundary polygon
  sw_poly <- reach_boundaries[reach_boundaries$StreamReac==sw,] #'sw' = subwatershed
  sw_2010 <- crop(mask(dem_10_bil,sw_poly),sw_poly) #slow
  sw_2016 <- crop(mask(dem_16,sw_poly),sw_poly) #slow
  
  #s <- stack(sw_2010,sw_2016) #deprecated
  #d <- sw_2016 - sw_2010 #difference raster
  #v <- getValues(d) #difference values
  #mean_dif <- round(mean(v,na.rm=T),2); median_dif <- round(median(v,na.rm=T),2)
  #baseline_median_abs_dif <- round(median(abs(v),na.rm=T),2)
  
  #modify the raster to exclude large outliers
  #d_mod <- d #set up modified differenc raster d
  #flags <- which(abs(v) > 20 | abs(v) < 0.5) #Look for pixels where the difference changes by more than 20 m or less than 0.5 and set to NA
  #d_mod[flags] <- NA
  #v_mod <- getValues(d_mod)
  #png(paste("./figures/raster_alignment_EDA/",sw,"baseline.png"))
  #plot(d_mod,main=paste0("shift x=0px, y=0px\n median dif = ",median_dif,"; \nmean dif = ", mean_dif,
  #                           "; \nbaseline median abs value dif = ", baseline_median_abs_dif)) 
  #dev.off()
  writeRaster(sw_2010,paste0("./large_files/DEM_2010_subwatershed/",sw,"_2010.tif"),overwrite = TRUE)
  
  #below is deprecated because we fold the baseline into the for loop
  #offset_tracker[nrow(offset_tracker)+1,"reach"] = sw
  #offset_tracker[nrow(offset_tracker),"x_inc"] = 0
  #offset_tracker[nrow(offset_tracker),"y_inc"] = 0
  #offset_tracker[nrow(offset_tracker),"median_abs_dif"] = baseline_median_abs_dif
  for(x_inc in (c(0, -1, 1))){ #try different x adjustment increments
    for(y_inc in (c(0, -1, 1))){ #try different y adjustment increments
      ##1. Do the shift
      sw_2016_shift <- shift(sw_2016, dx = x_inc, dy = y_inc) 
      d_shift <- #difference raster after shifting subwatershed for 2016 (slow). warning ok, only subtracting the part that overlaps.
        sw_2016_shift - sw_2010 
      v_shift <- getValues(d_shift) #difference values
      mean_dif <- round(mean(v_shift,na.rm=T),2); median_dif <- round(median(v_shift,na.rm=T),2)
      median_abs_dif <- round(median(abs(v_shift),na.rm=T),2)
      if(x_inc == 0 & y_inc == 0){
        baseline_median_abs_dif <- median_abs_dif
        writeRaster(sw_2016_shift, #if the shift improved the difference value, save as the "best"
                    paste0("./large_files/DEM_2016_subwatershed_shift/",sw,"_2016.tif"),overwrite = TRUE)
      }else if(median_abs_dif < baseline_median_abs_dif){
        baseline_median_abs_dif <- median_abs_dif #reset the baseline
        writeRaster(sw_2016_shift, #if the shift improved the difference value, save as the "best"
                    paste0("./large_files/DEM_2016_subwatershed_shift/",sw,"_2016.tif"),overwrite = TRUE)
        print(paste0("updating 2016 raster by x = ",x_inc,", y = ",y_inc))
      }
      
      ##2. Store the results from the shift
      offset_tracker[nrow(offset_tracker)+1,"reach"] = sw
      offset_tracker[nrow(offset_tracker),"x_inc"] = x_inc
      offset_tracker[nrow(offset_tracker),"y_inc"] = y_inc
      offset_tracker[nrow(offset_tracker),"median_abs_dif"] = median_abs_dif
      
      ##3. modify the raster to exclude large outliers, for plotting purposes ONLY, and save plot
      d_shift_mod <- d_shift
      flags <- which(abs(v_shift) > 20 | abs(v_shift) < 0.5)
      d_shift_mod[flags] <- NA
      png(paste("./figures/raster_alignment_EDA/",sw,"_x_",x_inc,"_y_",y_inc,".png"))
      plot(d_shift_mod,main=paste0("shift x=",x_inc,"px, y=",y_inc,"px\n median dif = ",median_dif,"; \nmean dif = ", mean_dif,
                             "; \nmedian abs value dif = ", median_abs_dif))
      dev.off()
      print(paste("finished",sw,x_inc,y_inc,Sys.time()))
      
    }
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


##This code takes our "final" severity raster layer (derivation described in Teams) and runs a series of intersections with zones of interest: (sub)watersheds, bottomlands, thalwegs (lines) and plots (points).
##Written by Jens Stevens, stevensjt@gmail.com, August 2021

library(sf)
library(stars)
library(raster)
library(dplyr)

#Set up data in an sf framework
watersheds <- read_sf("../GIS/Severity Attibution/Watersheds_Severity.shp")
bottoms <- read_sf("../GIS/Severity Attibution/BottomSeverity.shp")
thalwegs <- read_sf("../GIS/Severity Attibution/Thalwegs_Severity.shp")
#plots <- #note: JTS did this in QGIS because it was a small dataset and much faster - canned tools exist for it.
#transects <- #JTS: This file that was sent to me in July 2021 apparently only contains on transect. Skipping for now.
  #read_sf("../GIS/productsforJens072021/transects_derived/transects27June2019.shp")
s <- read_stars("../GIS/Severity Attibution/All Severity Products/Las_Conchas_Severity_jts.tif")
sc <- st_crop(s, watersheds); names(sc)[1] = "Severity"
#plot(watersheds$geometry)
#plot(sc,add=TRUE)
sf_sev <- st_make_valid(st_as_sf(sc,as_points = F, merge = T)) #make valid to clean up self-intersections

#plot(sf_sev)
sf_sev <- sf_sev %>%
  group_by(Severity) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()



##Extract areas of different severities (watersheds):

ls_intersect <- st_intersection(watersheds,sf_sev[sf_sev$Severity==1,])
ls_intersect$LS_HA <- as.numeric(st_area(ls_intersect)) * 0.0001
ls_df <- as.data.frame(as.data.frame(ls_intersect[,c("Reach","LS_HA")])[,c(1,2)])
watersheds <- merge(watersheds, ls_df, all = TRUE)

ms_intersect <- st_intersection(watersheds,sf_sev[sf_sev$Severity==2,])
ms_intersect$MS_HA <- as.numeric(st_area(ms_intersect)) * 0.0001
ms_df <- as.data.frame(as.data.frame(ms_intersect[,c("Reach","MS_HA")])[,c(1,2)])
watersheds <- merge(watersheds, ms_df, all = TRUE)

hs_intersect <- st_intersection(watersheds,sf_sev[sf_sev$Severity==3,])
hs_intersect$HS_HA <- as.numeric(st_area(hs_intersect)) * 0.0001
hs_df <- as.data.frame(as.data.frame(hs_intersect[,c("Reach","HS_HA")])[,c(1,2)])
watersheds <- merge(watersheds, hs_df, all = TRUE)

watersheds$LS_HA[which(is.na(watersheds$LS_HA))] <- 0
watersheds$MS_HA[which(is.na(watersheds$MS_HA))] <- 0
watersheds$HS_HA[which(is.na(watersheds$HS_HA))] <- 0

watersheds$UB_HA <- round(#Calculate unburned area
  watersheds$HA - watersheds$LS_HA - watersheds$MS_HA - watersheds$HS_HA, 1)

#This chunk deals with slight alignment areas for watersheds that were completely burned but showed up with small chunks of unburned due to some raster pixels being excluded by the cropping operation above.
watersheds[watersheds$UB_HA< 2,"MS_HA"] <- #add small bits of "false unburned" to moderate
  st_drop_geometry(watersheds[watersheds$UB_HA< 2,"MS_HA"]) + 
  st_drop_geometry(watersheds[watersheds$UB_HA< 2,"UB_HA"])
watersheds[watersheds$UB_HA< 2,"UB_HA"] <- 0 #delete small bits of "false unburned"

##Final processing of watersheds

watersheds$LS_Pct <- round(watersheds$LS_HA / watersheds$HA,2)
watersheds$MS_Pct <- round(watersheds$MS_HA / watersheds$HA,2)
watersheds$HS_Pct <- round(watersheds$HS_HA / watersheds$HA,2)
watersheds$UB_Pct <- round(watersheds$UB_HA / watersheds$HA,2)
watersheds <- watersheds[,-c("Reach_HSHA", "UpS_HSHA","Reach_HSP","UpS_HSP")]

#watersheds_view <- st_drop_geometry(watersheds) #Faster viewing to make sure everything looks ok).

write_sf(watersheds, 
         "../GIS/Severity Attibution/All Severity Raster/Watersheds_AllSeverity.shp")


##Extract areas of different severities (bottoms):

ls_intersect <- st_intersection(bottoms,sf_sev[sf_sev$Severity==1,])
ls_intersect$LS_HA <- as.numeric(st_area(ls_intersect)) * 0.0001
ls_df <- as.data.frame(as.data.frame(ls_intersect[,c("Reach","LS_HA")])[,c(1,2)])
bottoms <- merge(bottoms, ls_df, all = TRUE)

ms_intersect <- st_intersection(bottoms,sf_sev[sf_sev$Severity==2,])
ms_intersect$MS_HA <- as.numeric(st_area(ms_intersect)) * 0.0001
ms_df <- as.data.frame(as.data.frame(ms_intersect[,c("Reach","MS_HA")])[,c(1,2)])
bottoms <- merge(bottoms, ms_df, all = TRUE)

hs_intersect <- st_intersection(bottoms,sf_sev[sf_sev$Severity==3,])
hs_intersect$HS_HA <- as.numeric(st_area(hs_intersect)) * 0.0001
hs_df <- as.data.frame(as.data.frame(hs_intersect[,c("Reach","HS_HA")])[,c(1,2)])
bottoms <- merge(bottoms, hs_df, all = TRUE)

bottoms$LS_HA[which(is.na(bottoms$LS_HA))] <- 0
bottoms$MS_HA[which(is.na(bottoms$MS_HA))] <- 0
bottoms$HS_HA[which(is.na(bottoms$HS_HA))] <- 0

bottoms$UB_HA <- round(#Calculate unburned area
  bottoms$HA - bottoms$LS_HA - bottoms$MS_HA - bottoms$HS_HA, 1)

##Final processing of bottoms

bottoms$LS_Pct <- round(bottoms$LS_HA / bottoms$HA,2)
bottoms$MS_Pct <- round(bottoms$MS_HA / bottoms$HA,2)
bottoms$HS_Pct <- round(bottoms$HS_HA / bottoms$HA,2)
bottoms$UB_Pct <- round(bottoms$UB_HA / bottoms$HA,2)

#bottoms_view <- st_drop_geometry(bottoms) #Faster viewing to make sure everything looks ok).

write_sf(bottoms, 
         "../GIS/Severity Attibution/All Severity Raster/Bottoms_AllSeverity.shp")

##Extract areas of different severities (thalwegs):

ls_intersect <- st_intersection(thalwegs,sf_sev[sf_sev$Severity==1,])
ls_intersect$LS_M <- as.numeric(st_length(ls_intersect))
ls_df <- as.data.frame(as.data.frame(ls_intersect[,c("Reach","LS_M")])[,c(1,2)])
thalwegs <- merge(thalwegs, ls_df, all = TRUE)

ms_intersect <- st_intersection(thalwegs,sf_sev[sf_sev$Severity==2,])
ms_intersect$MS_M <- as.numeric(st_length(ms_intersect))
ms_df <- as.data.frame(as.data.frame(ms_intersect[,c("Reach","MS_M")])[,c(1,2)])
thalwegs <- merge(thalwegs, ms_df, all = TRUE)

hs_intersect <- st_intersection(thalwegs,sf_sev[sf_sev$Severity==3,])
hs_intersect$HS_M <- as.numeric(st_length(hs_intersect))
hs_df <- as.data.frame(as.data.frame(hs_intersect[,c("Reach","HS_M")])[,c(1,2)])
thalwegs <- merge(thalwegs, hs_df, all = TRUE)

thalwegs$LS_M[which(is.na(thalwegs$LS_M))] <- 0
thalwegs$MS_M[which(is.na(thalwegs$MS_M))] <- 0
thalwegs$HS_M[which(is.na(thalwegs$HS_M))] <- 0

thalwegs$UB_M <- round(#Calculate unburned length
  thalwegs$Length - thalwegs$LS_M - thalwegs$MS_M - thalwegs$HS_M, 1)

##Final processing of thalwegs

thalwegs$LS_Pct <- round(thalwegs$LS_M / thalwegs$Length,2)
thalwegs$MS_Pct <- round(thalwegs$MS_M / thalwegs$Length,2)
thalwegs$HS_Pct <- round(thalwegs$HS_M / thalwegs$Length,2)
thalwegs$UB_Pct <- round(thalwegs$UB_M / thalwegs$Length,2)

#thalwegs_view <- st_drop_geometry(thalwegs) #Faster viewing to make sure everything looks ok).
thalwegs <- thalwegs[,-c(6:11)]

write_sf(thalwegs, 
         "../GIS/Severity Attibution/All Severity Products/Thalwegs_AllSeverity.shp")
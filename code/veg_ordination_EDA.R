#Exploratory data analysis (EDA) for veg plots
#Bandelier riparian NRPP project; NMNH survey and 2020 resample.
#Authors: Jens Stevens

####0. Read libraries####
library(vegan)
library(readxl)
library(tidyverse)

####1. Read data and process####
d <- read_xlsx("./data/raw/BAND_NHNM_2020 RESAMPLING DATA.xlsx", col_types = c("text","date","numeric","text","numeric","text",rep("numeric",357)))
d$full_id <- paste(d$plot_id,d$year,d$canyon, sep="_")

d_ord <- d %>%
  select(full_id,ends_with("_cov"))
d_ord <- column_to_rownames(d_ord, var = "full_id")
d_ord [is.na(d_ord)] <- 0
names(d_ord) = gsub(pattern = "_cov", replacement = "", x = names(d_ord)) #condense species codes

d_env <- d[,c("canyon")]
rownames(d_env) <- d$full_id
d_env$fire_ <- ifelse(d$year ==2020, "burned","unburned")


####2. Ordination test####
example_NMDS=metaMDS(d_ord, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions. Increase if high stress is problem. 

ordiplot(example_NMDS,display ="sites") #Ordination plot function especially for congested plots
orditorp(example_NMDS,display="species",col="red",air=0.01) #The function adds text or points to ordination plots


ef <- envfit(example_NMDS, d_env, permu = 999)
plot(ef)

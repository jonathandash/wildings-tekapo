#Calculate density raster from field dataset

#### set libraries ####
library(here)
library(tidyverse)
library(sf)
library(raster)
library(sp)

#### read data ####
df<-read.csv(here('data', 'FieldMaster.csv'), stringsAsFactors = FALSE)

#### Spatial analysis ####

#Make dataframe a spatial points df
coordinates(df)<- ~Easting+Northing

# Create raster covering the study area with a 1 m resolution

ex<-extent(df)
r1<-raster(ext = ex, resolution =1)

# Use rasterise to make the count raster

x <- rasterize(df, r1, fun='count')

plot(x)
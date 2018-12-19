


library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(here)


library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)   # for web applications

rasterOptions(datatype = "FLT4S", 
              progress = "text", 
              #tmpdir = tmpdir_name, 
              tmptime = 4, 
              timer = TRUE,
              tolerance = 0.5,
              chunksize = 1e+08,
              maxmemory = 1e+09)

class       : Extent 
xmin        : 1386481 
xmax        : 1386945 
ymin        : 5126109 
ymax        : 5126647

nz_region = st_bbox(c(xmin = 1350000, xmax = 1480000,
                      ymin = 5100000, ymax = 5180000),
                    crs = st_crs(nz_height)) %>% 
  st_as_sfc()

#nz_region = st_bbox(c(xmin = 1386481 , xmax = 1386945,
#                      ymin = 5126109, ymax = 5126647)) %>% 
#                      st_as_sfc()


shp<-st_read(here('data', 'Full_Study_Area.shp'))

#st_transform(shp, crs = 32759)

#crs(nz_region)


nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
  tm_raster(style = "cont", palette = "viridis", legend.show = TRUE) +
  #tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
  tm_shape(shp) + tm_symbols(col = "yellow", size = 1) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_legend(bg.color = 'white')


nz_map = tm_shape(nz) + tm_polygons() +
 # tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
  tm_shape(nz_region) + tm_borders(lwd = 3) 

library(grid)

png(here('out','Figure1-map.png'), width = 17, height =10, units='cm', res=500)
nz_height_map
print(nz_map, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))
dev.off()















## Avoid scientific notation
options(scipen = 12)

## Load required packages
lib <- c("raster", "rgdal", "ggplot2")
sapply(lib, function(x) require(x, character.only = TRUE))

## Download and reproject data from gadm.org to UTM 60S
nz1 <- getData("GADM", country = "NZ", level = 1)
nz1 <- spTransform(nz1, CRS("+init=epsg:2135"))

## Extract polygon corners and merge with shapefile data
nz1@data$id <- rownames(nz1@data)
nz1.ff <- fortify(nz1)
nz1.df <- merge(nz1@data, nz1.ff, by = "id", all.y = TRUE)

## Plot map
ggplot() + 
  geom_polygon(data = nz1.df, aes(x = long, y = lat, group = group, 
                                  fill = NAME_1), 
               color = "black", show.legend= FALSE) +
  labs(x = "x", y = "y") + 
  theme_bw()

#Calculate density raster from field dataset

#### set libraries ####
library(here)
library(tidyverse)
library(sf)
library(raster)
library(sp)

#### read data ####
df<-read.csv(here('data', 'FieldMaster.csv'), stringsAsFactors = FALSE)
df<- df %>% filter(Northing <5160000)

#### Spatial analysis ####

#Make dataframe a spatial points df
coordinates(df)<- ~Easting+Northing

# Create raster covering the study area with a 1 m resolution

ex<-extent(df)
r1<-raster(ext = ex, resolution =10)

# Use rasterise to make the count raster

x <- rasterize(df, r1, field = 'H', fun=function(x,...)length(x))

plot(x)
plot(df["H"], add=T)

# output rasters for use in spectral modelling

res.list<- c(1,2,3,4,5,6,7,8,9,10)
#res.list<- c(5,10)

for(i in 1:length(res.list))
{
  
  r1<-raster(ext = ex, resolution =res.list[i])
  ct.rast <- rasterize(df, r1, field = 'H', fun=function(x,...)length(x))
  writeRaster(ct.rast, here::here('out', paste('count_rast_', res.list[i], '.tif', sep='')), overwrite = TRUE)
  
}

plot(ct.rast)

#### Plot density ####
# Make a plot of tree density for the paper

df2<-read.csv(here('data', 'FieldMaster.csv'), stringsAsFactors = FALSE)
df2<- df2 %>% filter(Northing <5160000)


png(here('out','hex_bin.png'), width = 10, height =11, units='cm', res=500)
df2 %>% ggplot(aes(x=Easting/10000, y=Northing/10000)) +
  #geom_point() +
  geom_hex(bins = 60) +
  scale_fill_viridis_c() +
  theme_bw() +
  labs(x='Easting', y='Northing')+
  theme(legend.position = 'bottom',
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
dev.off()
  
  
  




#  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
#  scale_fill_viridis_c(name = 'Density') 
  
#  stat_density2d(aes(color = ..level..))


#  stat_density2d(aes(fill = ..level..),n = 1000,contour = TRUE,geom = "polygon", alpha=0.5) +
#  scale_fill_viridis_c(name = 'Density')




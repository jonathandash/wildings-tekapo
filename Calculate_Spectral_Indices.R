# Calculate NDVI and other spectral indices for the study site for the density raster analysis

#### Setup libraries ####
library(raster)
library(here)
library(doParallel)

source(here('VegetationIndices.R')) # Read index functions


rasterOptions(tmpdir='D\\temp')


# Create UAV dataset for calculations
sent.rgb<-stack(here('data\\sentera', '075_Teakapo_sentera_2_transparent_mosaic_group2.tif'))
sent.ren<-stack(here('data\\sentera', '075_Teakapo_sentera_2_transparent_mosaic_group1.tif'))

analysis.rast<-stack(sent.rgb[[c(1,2,3)]], sent.ren[[c(1,3)]])
names(analysis.rast)<-c('red', 'green', 'blue', 'RE', 'NIR')

uav.chm<-raster(here('data', 'uav_chm_full_study_area.tif'))
#raster::plot(uav.chm)

uav.chm<-resample(uav.chm, analysis.rast, method = 'bilinear')
analysis.rast<-stack(analysis.rast, uav.chm)
names(analysis.rast)<-c('red', 'green', 'blue', 'RE', 'NIR', 'Z')
#plot(analysis.rast)

beginCluster(n=14)
ndvi.rast<-NDVI(NIR = analysis.rast$NIR, RED = analysis.rast$red)
endCluster()

writeRaster(ndvi.rast, here::here('out','uav-ndvi-rast.tif'), overwrite = TRUE)

beginCluster(n=14)
TCARI.rast<-TCARI(GREEN = analysis.rast$green, RED = analysis.rast$red, RED.EDGE = analysis.rast$NIR)
endCluster()

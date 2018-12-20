#### Setup libraries ####
library(raster)
library(here)
library(devtools)
#install_github("topepo/caret/pkg/caret")
library(caret)
library(doParallel)
library(ranger)

rasterOptions(tmpdir='D\\temp')

plane.ms<-stack(here('data', 'tekapo_landpro_clipped_full_study_area.tif'))

plane.ms<-stack(plane.ms[[c(1,2,3,4)]])
names(plane.ms)<-c('red', 'green', 'blue',  'NIR')

plane.chm<-raster(here('data', 'plane_chm_full_study_area.tif'))

plane.chm<-resample(plane.chm, plane.ms, method = 'bilinear')

analysis.rast<-stack(plane.ms, plane.chm)

names(analysis.rast)<-c('red', 'green', 'blue',  'NIR', 'Z')

trainData <- shapefile(here('data', 'training_data_plane.shp'))

responseCol <- "Classvalue"


dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(analysis.rast)) + 1))
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- raster::extract(analysis.rast, categorymap)
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
    dfAll <- rbind(dfAll, dataSet[complete.cases(dataSet),])
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
}

dfAll <- na.omit(dfAll)

control <- trainControl(method="repeatedcv", number=5, repeats=3, allowParallel = TRUE)

modFit_rf <- train(x = dfAll[,1:5],
                   y=as.factor(dfAll$class), 
                   method = "svmLinear2",
                   #method = "ranger",
                   #data = dfAll,
                   #model = FALSE,
                   trControl = control)

#endCluster()
print(modFit_rf)
importance <- varImp(modFit_rf, scale=FALSE)
plot(importance)
saveRDS(modFit_rf, here('out', 'svm_plane_RGBNZ.Rdata'))

#modFit_rf<-readRDS(here('out', 'rf_uav_RGBEN.Rdata'))
beginCluster()
preds_rf <- clusterR(analysis.rast, raster::predict, args = list(model = modFit_rf))
endCluster()

writeRaster(preds_rf, here('out', 'svm_RGBNZ_plane.tif'), overwrite=TRUE)


#Develop logistic regression models for comparison

# Check tuning parameter via `modelLookup` (shows a parameter called 'parameter')
modelLookup('glm')
#  model parameters - this confirms that caret doesn't tune any parameters for glm

# Try out the train function to see if 'parameter' gets tuned
set.seed(1)
model_glm <- train(Class ~., data=GermanCredit, method='glm')
model_glm



#writeRaster(preds_rf, here('out', 'rf_RGBEN_plane.tif'))

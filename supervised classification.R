
#### Setup libraries ####
library(raster)
library(here)
library(devtools)
#install_github("topepo/caret/pkg/caret")
library(caret)
library(doParallel)
library(ranger)

rasterOptions(tmpdir='D\\temp')



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

#shp<-shapefile(here('data', 'spread-shelter-belt-excluded.shp'))
#crp.ndvi<-mask(sen.ndvi, shp)



trainData <- shapefile(here('data', 'training_data.shp'))


responseCol <- "Classvalue"


dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(analysis.rast)) + 1))   
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(analysis.rast, categorymap)
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

#saveRDS(dfAll, here('out', 'dfAll_UAV_rgbrenirz.Rdata'))
dfAll<-readRDS(here('out', 'dfAll_UAV_rgbrenirz.Rdata'))


#v <- getValues(dfAll)
#rm(crp.ndvi)
#gc()
#i <- which(!is.na(dfAll))
#gc()
dfAll <- na.omit(dfAll)


# Take a subsample
nsamples <- 20000
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples), ]

# my.rang<-ranger(as.factor(class)~., 
#                 data = dfAll,
#                 write.forest = T)



# prepare training scheme
control <- trainControl(method="repeatedcv", number=5, repeats=3, allowParallel = TRUE)


#beginCluster()
#cl <- makeCluster(8) 
#registerDoParallel(cl) 

# Model fitting on the sample data
modFit_rf <- train(x = dfAll[,1:6],
                  y=as.factor(dfAll$class), 
                   method = "ranger", 
                   #data = dfAll,
                   #model = FALSE,
                   trControl = control)



#stopCluster(cl)
#registerDoSEQ()
modFit_rf<-readRDS(here('out', 'rf_uav_RGBENZ.Rdata'))

#endCluster()
print(modFit_rf)
importance <- varImp(modFit_rf, scale=FALSE)
plot(importance)
saveRDS(modFit_rf, here('out', 'rf_uav_RGBENZ.Rdata'))

beginCluster()
preds_rf <- clusterR(analysis.rast, raster::predict, args = list(model = modFit_rf))
endCluster()

plot(preds_rf)


writeRaster(preds_rf, here('out', 'rf_RGBENZ_uav.tif'), overwrite = T)

# Single model ranger fit to save time spectral and lidar
my.rang<-ranger(class~.,
                data = dfAll,
                mtry = 2,
                write.forest = T, 
                importance = 'impurity') 

print(my.rang)
importance(my.rang)

beginCluster()
preds_rf <- clusterR(analysis.rast, raster::predict, args = list(model = my.rang))
endCluster()

analysis.df<-as.data.frame(analysis.rast)
pred<-raster::predict(analysis.rast, my.rang, 
                      type='response', progress='window', fun = function(model, ...) predict(model, ...)$predictions)
plot(pred)

writeRaster(pred, here('out', 'rf_RGBENZ_uav.tif'), overwrite = T)

# Single model ranger fit to save time spectral and lidar
my.rang.2<-ranger(class~red + green + blue+ RE + NIR,
                data = dfAll,
                mtry = 2,
                write.forest = T, 
                importance = 'impurity') 

importance(my.rang.2)
pred2<-raster::predict(analysis.rast, my.rang.2, 
                      type='response', progress='window', fun = function(model, ...) predict(model, ...)$predictions)
plot(pred2)
writeRaster(pred2, here('out', 'rf_RGBEN_uav.tif'), overwrite = T)


#Develop logistic regression models for comparison

# Check tuning parameter via `modelLookup` (shows a parameter called 'parameter')
modelLookup('glm')
#  model parameters - this confirms that caret doesn't tune any parameters for glm

# Try out the train function to see if 'parameter' gets tuned
dfAll.2class<-subset(dfAll, class !=3)
set.seed(1)
model_glm_sp_Z <- train(factor(class)~ NIR + red + green, 
                   data=dfAll.2class, 
                   method='glmnet',
                   trControl = control,
                   family="binomial"
                   )
model_glm_sp_Z



# This script will produce a verification dataset for the outputs from the supervised 
# classification models in the Tekapo dataset.
# Jonathan Dash September 2019


#### set libraries ####
library(here)
library(raster)
library(sf)
library(tidyverse)
#library(mapview)
library(sp)
library(rgdal)
library(irr)
library(modeest)
library(snow)

#### Define functions ####

#SummarySE
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#### Global options####

#Set seed
set.seed(99)

# Samples per stratum
strt<-20

# Number of iterations
iter<-100

# data frame to store outputs
result.holder<-data.frame()


# Height breaks
hts<-c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5)

#### Read data ####

df<-read.csv(here( 'data', 'FieldMaster.csv')) # Field data

# Read classifier outputs
# All variables RF plane

tlist=list.files(here('data/classifier_out'), pattern="tif$", full.names=FALSE) # list all tifs in image.dir


#rf.plane.lid<-raster(here('data/classifier_out', 'rf_RGBENZ_plane.tif'))
#rf.uav.lid<-raster(here('data/classifier_out', 'rf_RGBENZ_uav.tif'))



# Calculate crown area
#df$CW<-((df$dia1 + df$dia2)/2)/100 # calculate crown width in M
df$CW<-((df$dia2)/2)/100 # calculate crown width in M - conservative version
df$crown_area<- pi * (df$dia2/2) ^2 # more conservative crown area
df$H_m<-df$H /100
df<-subset(df, spp %in% c("pp", "ps"))

df$Ht_Class<-cut(df$H_m, breaks = hts)

#### Sampling and extraction ####
# Start outer loop to go through rasters

for (k in 1:length(tlist))
  {
# Read the tif
#cls.result<-raster(here('data/classifier_out',tlist[k])) 
cls.result<-raster(here('data/classifier_out',tlist[k])) 

# Get the name of the dataset
identifier<-str_sub(tlist[k],
        start = 0,
        end = (str_locate(tlist[k], "[.]")-1)[,1])





#spl<-df.ps%>%split(.$Ht_Class) 
for (i in 1:iter)
  {
  
  
strat.samp<-df%>% group_by(spp, Ht_Class) %>%
  sample_n(strt, replace = TRUE, weight = NULL, .env = NULL) %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 2193)  # Convert to sf
   

# Buffer circles by crown radius
circles.st.samp.sf <- st_buffer(strat.samp, dist = strat.samp$CW)

# plot and check crs
#plot(circles.st.samp.sf)
#st_crs(circles.st.samp.sf)

circles.st.samp.sf$spp<-factor(circles.st.samp.sf$spp)
circles.st.samp.sf$ClassValue<-1
#circles.st.samp.sf$FID<-as.factor(circles.st.samp.sf$FID)
circles.st.samp.sf$merge.key<-row.names(circles.st.samp.sf)

val_shp<-as(circles.st.samp.sf, 'Spatial')
#class(val_shp)

# Extract classifier
beginCluster(n=6)
ex<-raster::extract(cls.result, val_shp,  
                    method = 'simple',
                    df = TRUE)




endCluster()

ex.df<-as.data.frame(ex)

names(ex)<-c('ID', 'Value')

#Calculate mode for each tree
dataSummary <- ex %>%
  group_by(ID) %>%
  summarise(mean.out = mean(Value, na.rm=TRUE),
            meadian=median(Value, na.rm=TRUE),
            minimum=min(Value, na.rm=TRUE),
            #mode = mlv(Value, method='mfv')[['M']], na.rm=TRUE)
            mode = mlv(Value, method='mfv', na.rm=TRUE)[['M']])




dataSummary$observed<-1
dataSummary$ID = as.factor(dataSummary$ID)

#output.class.val<-st_bind_cols(dataSummary, circles.st.samp.sf) #Replace this with Pebesmas suggestion from SO - didn't work

output.class.val<-merge(dataSummary, circles.st.samp.sf, by.x='ID', by.y='merge.key') # merge based on rownames of circles


#st_geometry(output.class.val) <- NULL # This removes the sf geometry

output.class.val$stratum<-paste(output.class.val$spp, output.class.val$Ht_Class, sep='')
output.class.val$iter<-i

output.class.val$Classifier<-identifier

#Store outputs
result.holder<-rbind(result.holder, output.class.val)



}

}

#### Purrrr extraction and modelling #####
result.holder$it.st.key<-paste(result.holder$stratum, result.holder$Classifier, result.holder$iter, sep=')')

# Use Purrrr for calculating agreement kappa and other functions.
tt<-result.holder %>%
  split(.$it.st.key) %>%
  map(`[`, c("minimum",  "observed")) %>%
  map(irr::agree)

# extract outputs of map into a tibble df
res<-map_dfr(tt, 'value') 
#res$iter<- i




res.hol.gath<-res %>%
  gather(variable, value) %>%
  separate(variable, c('spp', 'cohort'), sep = "[(]") %>%
  separate(cohort, c('cohort', 'model.name', 'iteration'), sep = "[)]") %>% 
  mutate(Height= str_sub(cohort, 
                          #start = (str_locate(cohort, "[,]")+)1[,1], 
                          (str_locate(cohort, "[,]")+1)[,1],
                          end = str_length(cohort)-1)) %>%
  separate(model.name, c('classifier', 'predictors', 'platform'), sep="[_]")
res.hol.gath$Height<-as.numeric(res.hol.gath$Height)


         

#### Graph the error rate ####      
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

res.hol.gath %>% filter(spp %in% c("pp", "ps")) %>%
  summarySE(measurevar = 'value', groupvars = c('spp', 'Height', "platform")) %>%
  ggplot(aes(x = Height, y= value, group = spp, colour = spp)) +
  #geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1, position=pd)+
  #geom_point(position=pd, size = 3) +
  #geom_line(position =pd) +
  geom_smooth(method = "loess", se=F) +
  theme_bw() +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  labs(y = 'Agreement (%)', colour = 'Species', x='Height (m)') +
  #geom_ribbon(aes(ymin=value-ci, ymax=value+ci), alpha=0.2) +
  facet_wrap(.~platform) 

# With no species
res.hol.gath %>% filter(spp %in% c("pp", "ps")) %>%
  summarySE(measurevar = 'value', groupvars = c('Height', "platform")) %>%
  ggplot(aes(x = Height, y= value, group = platform, colour = platform)) +
  #geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1, position=pd)+
  #geom_point(position=pd, size = 3) +
  #geom_line() +
  geom_smooth(method="loess", se=F) +
  #facet_wrap(.~platform) +
  theme_bw() +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  labs(y = 'Agreement (%)') #+
  #ÿgeom_ribbon(aes(ymin=value-ci, ymax=value+ci), alpha=0.2) 
  




res.hol.gath %>% filter(spp %in% c("pp", "ps")) %>%
  ggplot(aes(x = Height, y= value, group = spp, colour = spp)) +
  #geom_jitter(position=pd, size = 2) +
  #geom_line(position =pd) +
  geom_smooth(method="loess", se=F)+
  #geom_smooth(method="lm", se=TRUE, fill=NA,
  #            formula=y ~ poly(x, 2, raw=TRUE)) +
  scale_colour_brewer(palette = "Accent") + 
  labs(y = 'Agreement (%)', x='Height (m)', colour = 'Species')+
  facet_wrap(.~platform) +
  theme_bw()

png(here::here('out', 'Agreement-pred-platform.png'), w=10, h=16, units='cm', res = 500)
res.hol.gath %>% filter(spp %in% c("pp", "ps")) %>%
  mutate(predictors=replace(predictors, predictors=='RGBENZ', 'Spectral+ALS'))%>%
  mutate(predictors=replace(predictors, predictors=='RGBEN', 'Spectral'))%>%    
  mutate(predictors=replace(predictors, predictors=='RGBN', 'Spectral'))%>% 
  ggplot(aes(x = Height, y= value, group = predictors, colour = predictors)) +
  #geom_jitter(position=pd, size = 2) +
  #geom_line(position =pd) +
  geom_smooth(method="loess", se=F)+
  scale_colour_brewer(palette = "Accent") + 
  labs(y = 'Agreement (%)', x='Height (m)', colour = 'Predictors')+
  facet_grid(platform~.) +
  theme_bw() +
  theme(legend.position = 'bottom')
dev.off()

#install.packages('devtools')
devtools::install_github('thomasp85/gganimate')
library(gganimate)

# A working version of an annimation...  not the best fit but it works
gif1<-res.hol.gath %>% filter(spp %in% c("pp", "ps")) %>%
  ggplot(aes(x = Height, y= value, group = spp, colour = spp)) +
  #geom_jitter(position=pd, size = 2) +
  #geom_line(position =pd) +
  geom_smooth(method="loess", se=F, size = 2)+
  #geom_smooth(method="lm", se=TRUE, fill=NA,
  #            formula=y ~ poly(x, 2, raw=TRUE)) +
  scale_colour_brewer(palette = 'Paired') + 
  labs(title = 'Platform: {closest_state}', y = 'Agreement (%)', x='Height (m)', colour = 'Species')+
  #facet_wrap(.~platform) +
  theme_dark() +
  transition_states(
    platform,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save('test_gif.gif', gif1, here::here('out'))

  



library(here)
library(tidyverse)
library(knitr)
library(ggjoy)
library(ggExtra)
library(gridExtra)



df<-read.csv(here('data', 'FieldMaster.csv'), stringsAsFactors = FALSE)

paste('In the field data dataframe there are ', nrow(df), ' records', sep= '')

#summary(df)

df$CW<-(df$dia1 + df$dia2)/2
df$crown_area<- ((df$dia1/2 + df$dia2/2)/2 * pi) # Calcuation provided by Thomas in e-mail
df$crown_vol<- (0.3 * pi* df$dia1*df$dia2*df$H)  # Calcuation provided by Thomas in e-mail



kable( df %>% group_by(spp, alive) %>%
         summarise(n=length(spp),
                   H = mean(H),
                   D1 = mean(dia1),
                   D2 = mean(dia2),
                   CW = mean(dia1) + mean(dia2)/2,
                   CrownArea = mean(crown_area),
                   CrownVol = mean(crown_vol)),
       caption = 'Summary of the field measured trees. Canopy with (CW) is calculated as D1+D2/2')



#Check the spatial distribution of the trees. This shows thatsome of the points are in the wrong place. two points are in completely the wrong place. This was most likely a trial in Chch and will be safely removed with a simple filter `df<-subset(df,Northing>5160000) `.


df %>% filter(Northing <5160000) %>%
  ggplot(aes(y=Northing, x=Easting, colour=H)) +
  geom_point() +
  theme_bw() +
  scale_colour_viridis_c()

# First panel should show the distribution of the trees

t1<-df %>% filter(Northing <5160000,
              spp %in% c('pp','ps')) %>%
  mutate(spp=replace(spp, spp=='pp', 'P. pon'),
         spp=replace(spp, spp=='ps', 'P. syl')) %>%
  ggplot(aes(y=Northing, x=Easting, shape=as.factor(spp))) +
  geom_point(alpha=0.5, aes(col = H)) +
  theme_bw() +
  scale_colour_viridis_c() +
  #scale_colour_brewer(palette = 'Accent') +
  theme(legend.position="bottom",
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  labs(shape = 'Species', col='Height')
  



df %>% mutate(CW = (dia1 + dia2)/2) %>%
  filter(Northing <5160000,
         spp %in% c('pp','ps')) %>%
  ggplot(aes(y=Northing, x=Easting, colour=as.factor(spp), size = CW)) +
  geom_point(alpha=0.2) +
  theme_bw() +
  scale_colour_brewer(palette='Set2') +
  scale_fill_brewer(palette='Set2') +
  labs(colour='Species', Size = 'Crown Width')








df %>% filter(Northing <5160000,
              spp %in% c('ps', 'pp', 'pm?')) %>%
  ggplot(aes(x=H, y=spp, height=..density.., fill = spp)) +
  scale_fill_viridis_d() +
  geom_vline(xintercept = 0, col = "grey70") +
  geom_joy(col = "grey70", scale = 2.4, show.legend = F) +
  theme_bw()


df %>% filter(Northing <5160000,
              spp %in% c('ps', 'pp')) %>%
  ggplot(aes(y=H, x = (dia1 + dia2)/2)) +
  geom_point(alpha=0.5,aes( col=spp))  +
  stat_density2d(aes(fill = ..level..),n = 100,contour = TRUE,geom = "polygon", alpha=0.2) +
  scale_fill_viridis_c() +
  labs(x='Crown Width (cm)', y='Height (cm)', colour='Species') +
  theme_bw() +
  theme(legend.position="none")


#Maybe second panel should dhow the relationship between crown width and height

t2<-df %>% filter(Northing <5160000,
              spp %in% c('ps', 'pp')) %>%
  mutate(spp=replace(spp, spp=='pp', 'P. ponderosa'),
         spp=replace(spp, spp=='ps', 'P. sylvestris')) %>%
  ggplot(aes(y=H, x = (dia1 + dia2)/2)) +
  #stat_binhex() +
  geom_jitter(alpha=0.8, colour='grey', size=0.2)  +
  #geom_density_2d(col = 'red') +  
  stat_density2d(aes(fill = ..level..),n = 100,contour = TRUE,geom = "polygon", alpha=0.2) +
  scale_fill_viridis_c(name = 'Density') +
  #scale_colour_brewer(palette = 'Accent') +
  labs(x='Crown Width (cm)', y='Height (cm)', colour='Species') +
  #geom_smooth(aes(col=spp), method = 'lm', se=F) +
  theme_bw() +
  facet_wrap(spp~.) +
  theme(legend.position="bottom")



# Third panel should include coning size

t3<-df %>% filter(
              spp == 'ps') %>%
  mutate(coning=replace(coning, coning=='Y', 'y'),
         coning=replace(coning, coning=='N', 'n')) %>%
  ggplot(aes(x = coning, y=H)) +
  geom_violin (fill = 'lightgreen') + 
  labs(y = 'Height (cm)') +
  theme_bw()

df %>% filter(
  spp == 'ps') %>%
  mutate(coning=replace(coning, coning=='Y', 'y'),
         coning=replace(coning, coning=='N', 'n')) %>%
  group_by(coning) %>%
  summarise(ht = mean(H),
            mht = min(H),
            cwt = mean(CW),
            mcwt = min(CW))



png(here('out','Figure2.png'), width = 17, height =17, units='cm', res=500)
grid.arrange(t1,t2, nrow = 2)
dev.off()






df %>% mutate(category=cut(H,
                           breaks=c(0, 10, 15, 20, 25, 30, 40, 50, 75, 100, 125, 150, 175, 200, 225,
                                    250, 275, 300, 325, 350, 375, 400,500, 4000),
                           labels=c(0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25,
                                    2.5, 2.75, 3, 3.25, 3.5, 3.75, 4,5, 40))) %>%
  group_by(category) %>%
  summarise(length(H))

P = ecdf(df$H)
Q=ecdf(df$CW)

plot(P, main='', xlab= 'Height (cm)', ylab='Density')
plot(Q, main='', xlab= 'Crown Width (cm)', ylab='Density')

jpeg(here('out','ecdf.jpg'), width = 50, height =25, units='cm', res=500)
layout(matrix(1:2, nrow = 1))
plot(P, main='', xlab= 'Height (cm)', ylab='Density')
plot(Q, main='', xlab= 'Crown Width (cm)', ylab='Density')
dev.off()







# This script will be a stand alone for the publication figures
# First think I want to do is figure out when the trees in the field dataset start coning

library(tidyverse)
library(ggplot2)
library(here)

# First think I want to do is figure out when the trees in the field dataset start coning

#### Read  data ####
df<-read.csv(here( 'data', 'Full data_clean_Aug18.csv')) # Field data

# plots of coning trees
df %>% filter(coning == 'y') %>%
  group_by(spp) %>%
  summarise(Ht = mean(H),
            min.Ht = min(H))



df %>% filter(coning == 'y',
              spp != 'pd') %>%
  ggplot(aes(x = spp, y=H)) +
  geom_violin (fill = 'lightblue') + 
  theme_bw()


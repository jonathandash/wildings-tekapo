# This script will simply read and merge the field data

library(tidyverse)
#library(ggplot2)
library(here)

df1<-read.csv(here('data', 'Full data_clean_Aug18.csv'))
df2<-read.csv(here('data', 'Full_data_clean_Sep_Nov.csv'))
df3<-read.csv(here('data', 'Nov_data.csv'))


names(df1)
names(df2)
names(df3)

# Looks like df2 just has an extra "Species" column that is all NA and can be deleted
# df3 is missing Point_ID and Offset - I don't think that this matters but best to check with Thomas

df1<-subset(df1, select=-c(Point_ID, Offset))
df2<-subset(df2, select=-c(Point_ID, Offset, Species))

df<-rbind(df1, df2)
df<-rbind(df, df3)

write.csv(df, here('data', 'FieldMaster.csv'), row.names=F)



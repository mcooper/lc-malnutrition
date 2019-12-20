setwd('G://My Drive/lc-malnutrition/Age Structure/')

library(tidyverse)
library(countrycode)
library(rnaturalearth)
library(sp)
library(raster)

options(stringsAsFactors = F)

#In both cases, we have data for every coutnry in 2019, except in Eritrea, where most recent is 2011

fe <- read.csv('API_SP.POP.0004.FE.5Y_DS2_en_csv_v2_445609.csv', skip = 4) 
fe[fe$Country.Name == 'Eritrea', 'X2018'] <- fe[fe$Country.Name == 'Eritrea', 'X2011']
fe$Country.Name[fe$Country.Name == 'Eswatini'] <- 'Swaziland'
fe <- fe %>%
  dplyr::select(Country.Name, fe_u5=X2018) %>%
  na.omit

me <- read.csv('API_SP.POP.0004.MA.5Y_DS2_en_csv_v2_458632.csv', skip = 4) 
me[me$Country.Name == 'Eritrea', 'X2018'] <- me[me$Country.Name == 'Eritrea', 'X2011']
me$Country.Name[me$Country.Name == 'Eswatini'] <- 'Swaziland'
me <- me %>%
  dplyr::select(Country.Name, me_u5=X2018) %>%
  na.omit

comb <- merge(me, fe) %>%
  rowwise() %>%
  mutate(u5=mean(me_u5, fe_u5),
         iso_a3=countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  na.omit

write.csv(comb, 'G://My Drive/DHS Processed/Under5Population.csv', row.names=F)

cty <- ne_countries()
cty@data$iso_a3[cty@data$sovereignt=='Somaliland'] <- 'SOM'

cty <- merge(cty, comb)

pop <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2020/population.tif')
u5_rast <- rasterize(cty, pop, field='u5')

u5_pop <- pop*u5_rast/100

writeRaster(u5_pop, 'G://My Drive/lc-malnutrition/Age Structure/u5_pop.tif', format='GTiff')


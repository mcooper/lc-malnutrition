setwd('~/mortalityblob/dhs')

library(raster)
library(tidyverse)

points <- read.csv('lc-malnutrition-weights2.csv') %>%
  select(latitude, longitude) %>%
  unique

r <- raster('~/gd/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(extent(-18, 51.5, -35, 25))

#Mideast and North Africa to 0
r[r==12] <- 0
r[r==2] <- 1
r[r==3] <- 1

shader <- raster('~/gd/DHS Spatial Covars/Hillshade/MSR_50M.tif') %>%
  crop(r)
aez_s <- resample(r, shader, method='ngb')
shader[is.na(aez_s)] <- NA
white <- aez_s
white[white != 0] <- NA
aez_s[aez_s == 0] <- NA

shade <- shader %>%
  rasterToPoints %>%
  data.frame %>%
  mutate(MSR_50M = 256 - MSR_50M,
         MSR_50M = MSR_50M - min(MSR_50M))

aez <- aez_s %>%
  rasterToPoints %>%
  data.frame %>%
  mutate(AEZ_DHS = factor(AEZ_DHS, labels=c('Arid', 'Tropical Forest', 'Northern Savanna', 
                                            'Southern Savanna', 'Montane', 'Northern Woodland',
                                            'Southern Woodland')))

whiter <- white %>%
  rasterToPoints %>%
  data.frame

rgb2 <- function(r, g, b){
  rgb(r/255, g/255, b/255)
}

ggplot() + 
  geom_raster(data=shade, aes(x=x, y=y, alpha=MSR_50M), color='grey20', 
              show.legend=F) + 
  geom_raster(data=aez, aes(x=x, y=y, fill=AEZ_DHS), alpha=0.7) +
  geom_raster(data=whiter, aes(x=x, y=y), fill='grey', alpha=0.7) +
  geom_point(data=points, aes(x=longitude, y=latitude, color='DHS Sites'), size=0.05) + 
  scale_fill_manual(values = c('Arid'=rgb2(255, 225, 175), 
                               'Tropical Forest'=rgb2(50, 125, 0), 
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135), 
                               'Montane'=rgb2(200,143,200), 
                               'Northern Woodland'=rgb2(110, 175, 75),
                               'Southern Woodland'=rgb2(55, 145, 130))) + 
  scale_color_manual(values='black') + 
  scale_y_continuous(expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  theme_void() + 
  labs(color='', fill='Agro-Ecological\nZones') + 
  guides(fill = guide_legend(order=1),
         color = guide_legend(order=2))

ggsave('~/lc-malnutrition-tex/AEZ_Sites.png', width=7, height=5)


ggplot() + 
  geom_raster(data=aez, aes(x=x, y=y, fill=AEZ_DHS)) +
  geom_raster(data=whiter, aes(x=x, y=y), fill='grey') +
  scale_fill_manual(values = c('Arid'=rgb2(255, 225, 175), 
                               'Tropical Forest'=rgb2(50, 125, 0), 
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135), 
                               'Montane'=rgb2(200,143,200), 
                               'Northern Woodland'=rgb2(110, 175, 75),
                               'Southern Woodland'=rgb2(55, 145, 130))) + 
  scale_y_continuous(expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  theme_void() + 
  guides(fill = FALSE)

ggsave('~/lc-malnutrition-tex/AEZ_NoLegend.png', width=5, height=4)


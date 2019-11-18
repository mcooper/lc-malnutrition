setwd('G://My Drive/DHS Processed')

library(raster)
library(tidyverse)
library(stars)

points <- read.csv('lc-malnutrition-weights2.csv') %>%
  select(latitude, longitude) %>%
  unique %>%
  mutate(color='black')

r <- raster('G://My Drive/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(extent(-18, 51.5, -35, 25))

#Mideast and North Africa to 0
r[r==12] <- 0
r[r==2] <- 1
r[r==3] <- 1

r <- st_as_stars(r)

r_cut <- cut(r, breaks=c(-0.5, 0.5, 1.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5),
             labels=c("None", "Arid", "Forest", "Northern Savanna", "Southern Savanna",
                      "Highlands", "Northern Semi-Forest", "Southern Semi-Forest"))

rgb2 <- function(r, g, b){
  rgb(r/255, g/255, b/255)
}

ggplot() + 
  geom_stars(data=r_cut) + 
  geom_point(data=points, aes(x=longitude, y=latitude, color=color), size=0.05) + 
  scale_fill_manual(values = c('None'='gray', 
                               'Arid'=rgb2(255, 225, 175), 
                               'Forest'=rgb2(50, 125, 0), 
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135), 
                               'Highlands'=rgb2(200,143,200), 
                               'Northern Semi-Forest'=rgb2(110, 175, 75),
                               'Southern Semi-Forest'=rgb2(55, 145, 130),
                               'NA'='white'),
                    breaks = c('Arid', 'Forest', 'Northern Savanna', 'Southern Savanna',
                               'Highlands', 'Northern Semi-Forest', 'Southern Semi-Forest')) + 
  scale_color_manual(values = c('black'='black'), labels = c('black' = 'DHS Sites')) + 
  scale_y_continuous(expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  theme_void() + 
  labs(color='', fill='Agro-Ecological\nZones') + 
  guides(fill = guide_legend(order=1),
         color = guide_legend(order=2))

ggsave('C://Users/matt/lc-malnutrition-tex/AEZ_Sites.png', width=7, height=5)



ggplot() + 
  geom_stars(data=r_cut) + 
  #geom_point(data=points, aes(x=longitude, y=latitude, color=color), size=0.05) + 
  scale_fill_manual(values = c('None'='gray', 
                               'Arid'=rgb2(255, 225, 175), 
                               'Forest'=rgb2(50, 125, 0), 
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135), 
                               'Highlands'=rgb2(200,143,200), 
                               'Northern Semi-Forest'=rgb2(110, 175, 75),
                               'Southern Semi-Forest'=rgb2(55, 145, 130),
                               'NA'='white'),
                    breaks = c('Arid', 'Forest', 'Northern Savanna', 'Southern Savanna',
                               'Highlands', 'Northern Semi-Forest', 'Southern Semi-Forest')) + 
  scale_color_manual(values = c('black'='black'), labels = c('black' = 'DHS Sites')) + 
  scale_y_continuous(expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  theme_void() + 
  theme(legend.position = "none")

ggsave('C://Users/matt/lc-malnutrition-tex/AEZ_NoLegend.png', width=5, height=4)


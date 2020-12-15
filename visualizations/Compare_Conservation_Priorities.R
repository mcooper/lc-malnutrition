library(tidyverse)
library(raster)
library(Hmisc)
library(pals)

hunger <- raster('~/gd/lc-malnutrition/increased_burden.tif')
biodiv <- raster('~/gd/DHS Spatial Covars/Conservation Priorities/AT_Plants_Verts_SSI_rcp85_mean.tif') %>%
  resample(hunger)
aez <- raster('~/gd/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(hunger)

hun_pts <- rasterToPoints(hunger) %>%
  data.frame %>%
  mutate(x = round(x, 3), y = round(y, 3))
bio_pts <- rasterToPoints(biodiv) %>%
  data.frame %>%
  mutate(x = round(x, 3), y = round(y, 3))
aez_pts <- rasterToPoints(aez) %>%
  data.frame %>%
  mutate(x = round(x, 3), y = round(y, 3))

comb <- Reduce(function(x, y) merge(x, y, all=T), 
               list(hun_pts, bio_pts, aez_pts))

#Make quantiles
comb$bio_q <- cut2(comb$AT_Plants_Verts_SSI_rcp85_mean, g=3)
comb$hun_q[comb$AEZ_DHS %in% c(8, 9)] <- cut2(comb$increased_burden[comb$AEZ_DHS %in% c(8, 9)], g=3)
comb$hun_q <- factor(comb$hun_q, labels=c('low', 'med', 'hi'))

comb$bio_q[is.na(comb$bio_q)] <- levels(comb$bio_q)[1]
comb$hun_q[is.na(comb$hun_q)] <- levels(comb$hun_q)[1]

r <- function(x) t(apply(x, 2, rev))

pal_ix <- t(r(r(matrix(1:9, nrow=3))))

comb$color <- mapply(FUN=function(x, y){pal_ix[x, y]}, 
                     x=as.numeric(comb$bio_q), y=as.numeric(comb$hun_q)) %>%
  as.factor

#Make Visualization
palette <- stevens.greenblue()
names(palette) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

(map <- ggplot() + 
  geom_raster(data=comb, aes(x=x, y=y, fill=color), color='#000000',
               size=0.3) +
  scale_fill_manual(values = palette, na.value='#FFFFFF') + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) + 
  theme_void()+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)))











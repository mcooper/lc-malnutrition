library(tidyverse)
library(raster)
library(Hmisc)
library(pals)
library(rnaturalearth)
library(sf)
library(cowplot)

hunger <- raster('~/gd/lc-malnutrition/increased_burden.tif')
biodiv <- raster('~/gd/DHS Spatial Covars/Conservation Priorities/AT_Plants_Verts_SSI_rcp85_mean.tif') %>%
  resample(hunger)
aez <- raster('~/gd/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(hunger)
aez[is.na(aez)] <- 99

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
                     x=as.numeric(comb$bio_q), y=as.numeric(comb$hun_q))

#Make Bivariate Palette
palette <- brewer.seqseq2()
#palette[1] <- "#f7fcb9"

#       bio-hi bio-mid bio-low
#hun-hi   1       4       7
#hun-mi   2       5       8
#hun-lo   3       6       9

# #Try Univariate
# p <- c('#edf8fb','#b3cde3','#8c96c6','#8856a7','#810f7c')
# palette <- c(p[1], p[2], p[3], p[2], p[3], p[4], p[3], p[4], p[5])

names(palette) <- c(9, 6, 3, 8, 5, 2, 7, 4, 1)


#Make Colors for other parts of Africa and Ocean
comb$color[!comb$AEZ_DHS %in% c(8, 9)] <- 10
comb$color[comb$AEZ_DHS == 99] <- 11
palette <- c(palette, "#d9d9d9", "#FFFFFF")
names(palette)[10:11] <- c(10, 11)


comb$color <- factor(comb$color)

shader <- raster('~/gd/DHS Spatial Covars/Hillshade/MSR_50M.tif') %>%
  crop(hunger)
aez_s <- resample(aez, shader, method='ngb')
shader[aez_s==99] <- NA

shade <- shader %>%
  rasterToPoints %>%
  data.frame %>%
  mutate(MSR_50M = 256 - MSR_50M,
         MSR_50M = MSR_50M - min(MSR_50M))

cty <- ne_countries(scale=50) %>%
  crop(aez) %>%
  st_as_sf

(map <- ggplot() + 
  geom_raster(data=shade, aes(x=x, y=y, alpha=MSR_50M), color='grey20', 
              show.legend=F) + 
  geom_raster(data=comb, aes(x=x, y=y, fill=color), alpha=0.8) +
  scale_fill_manual(values = palette, na.value='#FFFFFF') + 
  geom_sf(data=cty, color='black', fill=NA) + 
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

leg_dat <- comb %>%
  dplyr::select(bio_q, hun_q, color) %>%
  filter(!color %in% c(10, 11)) %>%
  unique

(leg <- ggplot() +
  geom_tile(data = leg_dat, aes(x=hun_q, y=bio_q, fill=color), color='#000000',
            show.legend=FALSE) +
  scale_fill_manual(values = palette) +
  labs(x = sprintf("More Biodiversity Priority \u2192"),
       y = sprintf("More Food Security Priority \u2192")) +
  theme(legend.position="bottom",
        legend.key = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.background = element_rect(fill='transparent',
                                       color='transparent')) + 
  scale_x_discrete(expand=c(0,0), breaks=NULL) + 
  scale_y_discrete(expand=c(0,0), breaks=NULL) + 
  coord_fixed())

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(leg, 0.05, 0.15, 0.25, 0.35)

ggsave('~/lc-malnutrition-tex/Bivariate_Map.png', width=8, height=8)





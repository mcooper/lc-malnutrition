setwd('~/mortalityblob/lc_gams/')

library(raster)
library(mgcv)
library(tidyverse)
library(texreg)
library(cowplot)

#Make map of Africa with Colors
r <- raster('~/gd/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(extent(-18, 51.5, -35, 25))

#Mideast and North Africa to 0
r[r==12] <- 0
r[r==2] <- 1
r[r==3] <- 1

aezbackground <- r %>%
  rasterToPoints %>%
  data.frame %>%
  mutate(AEZ_DHS = ifelse(AEZ_DHS == 0, NA, AEZ_DHS),
         AEZ_DHS = factor(AEZ_DHS, labels=c('Arid', 'Tropical Forest', 'Northern Savanna', 
                                            'Southern Savanna', 'Montane', 'Northern Woodland',
                                            'Southern Woodland')))
aez <- aezbackground %>%
  filter(!is.na(AEZ_DHS))

leg <- ggplot() + 
  geom_raster(data=aezbackground, aes(x=x, y=y), fill='grey90') + 
  geom_raster(data=aez, aes(x=x, y=y, fill=factor(AEZ_DHS))) + 
  scale_fill_manual(values = c('Arid'=rgb2(255, 225, 175), 
                               'Tropical Forest'=rgb2(50, 125, 0), 
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135), 
                               'Montane'=rgb2(200,143,200), 
                               'Northern Woodland'=rgb2(110, 175, 75),
                               'Southern Woodland'=rgb2(55, 145, 130))) + 
  theme_void() + 
  theme(legend.position='none')

##################################
# Main Model
#####################################

load('AEZ_weights_GCV_natOnly.Rdata')

dat <- plot(mod)
#Blank space to <enter> through plots









pltdat <- data.frame()
#Skip lat-long smooth, so start from 2:
for (i in 2:length(dat)){
  pltdat <- bind_rows(pltdat,
                      data.frame(natural=dat[[i]]$x, 
                                 se=dat[[i]]$se, 
                                 fit=dat[[i]]$fit,
                                 AEZ=dat[[i]]$ylab))
}

#SEs are already at two
pltdat$max <- pltdat$fit + pltdat$se
pltdat$min <- pltdat$fit - pltdat$se

map <- data.frame(AEZ=c("s(natural,3.24):afr.arid.123", "s(natural,3.2):afr.forest.4", 
                        "s(natural,2.73):nafr.sav.5", "s(natural,3.2):seafr.sav.6", 
                        "s(natural,2.76):afr.high.7", 
                        "s(natural,2):nafr.subforest.8", "s(natural,2.97):safr.subforest.9"),
                  Map=c("Arid", "Tropical Forest", "Northern Savanna", "Southern Savanna", 
                        "Montane", "Northern Woodland", "Southern Woodland"))

pltdat <- merge(pltdat, map)

rgb2 <- function(r, g, b){
  rgb(r/255, g/255, b/255)
}

pltdat$natural <- pltdat$natural*100

pltdat$Map <- factor(pltdat$Map, levels=c('Tropical Forest', 'Northern Woodland',
                                          'Northern Savanna', 'Arid',
                                          'Montane', 'Southern Woodland',
                                          'Southern Savanna'))

map <- ggplot(pltdat) + 
  geom_ribbon(aes(x=natural, ymin=min, ymax=max, fill=Map)) + 
  geom_line(aes(x=natural, y=fit)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  facet_wrap(. ~ Map, ncol=4) + 
  scale_x_continuous(expand=c(0,0), breaks=c(25, 50, 75, 100), labels=function(x){paste0(x, '%')}) + 
  scale_y_continuous(sec.axis=dup_axis(name=sprintf('\u2190 More Drought Vulnerable'))) + 
  scale_fill_manual(values = c('Arid'=rgb2(255, 225, 175),
                               'Tropical Forest'=rgb2(50, 125, 0),
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135),
                               'Montane'=rgb2(200,143,200),
                               'Northern Woodland'=rgb2(110, 175, 75),
                               'Southern Woodland'=rgb2(55, 145, 130))) +
  theme_bw() + 
  labs(y='Coefficient for 24-Month SPEI',
       x='Percent of Nearby Land Uncultivated') + 
  theme(legend.position = 'none')

#Keep it simple, stupid
# ggdraw() +
  # draw_plot(map, 0, 0, 1, 1) +
  # draw_plot(leg, 0.75, 0.01, 0.2, 0.4)

ggsave('~/lc-malnutrition-tex/AEZ_effects.png', height = 3.5, width = 7)

texreg(mod, file = '~/lc-malnutrition-tex/Mod_Table.tex', longtable=TRUE, custom.model.names="", 
       center=FALSE, use.packages=FALSE, caption='Parameter estimates for Generalized Additive Model estimating the varying coefficient of SPEI.')


#####################################
# Model Censoring at 90%ile Weights
#####################################

load('AEZ_weights_GCV_natOnly_q90.Rdata')

dat <- plot(mod)
#Blank space to <enter> through plots









pltdat <- data.frame()
#Skip lat-long smooth, so start from 2:
for (i in 2:length(dat)){
  pltdat <- bind_rows(pltdat,
                      data.frame(natural=dat[[i]]$x, 
                                 se=dat[[i]]$se, 
                                 fit=dat[[i]]$fit,
                                 AEZ=dat[[i]]$ylab))
}

#SEs are already at two
pltdat$max <- pltdat$fit + pltdat$se
pltdat$min <- pltdat$fit - pltdat$se

map <- data.frame(AEZ=c("s(natural,3.26):afr.arid.123", "s(natural,3.32):afr.forest.4", 
                        "s(natural,3.33):nafr.sav.5", "s(natural,3.39):seafr.sav.6", 
                        "s(natural,2.46):afr.high.7", 
                        "s(natural,2):nafr.subforest.8", "s(natural,3.32):safr.subforest.9"),
                  Map=c("Arid", "Tropical Forest", "Northern Savanna", "Southern Savanna", 
                        "Montane", "Northern Woodland", "Southern Woodland"))

pltdat <- merge(pltdat, map)

rgb2 <- function(r, g, b){
  rgb(r/255, g/255, b/255)
}

pltdat$natural <- pltdat$natural*100

pltdat$Map <- factor(pltdat$Map, levels=c('Tropical Forest', 'Northern Woodland',
                                          'Northern Savanna', 'Arid',
                                          'Montane', 'Southern Woodland',
                                          'Southern Savanna'))

ggplot(pltdat) + 
  geom_ribbon(aes(x=natural, ymin=min, ymax=max, fill=Map)) + 
  geom_line(aes(x=natural, y=fit)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  facet_wrap(. ~ Map, ncol=4) + 
  scale_x_continuous(expand=c(0,0), breaks=c(25, 50, 75, 100), labels=function(x){paste0(x, '%')}) + 
  scale_y_continuous(sec.axis=dup_axis(name=sprintf('\u2190 More Drought Vulnerable'))) + 
  scale_fill_manual(values = c('Arid'=rgb2(255, 225, 175),
                               'Tropical Forest'=rgb2(50, 125, 0),
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135),
                               'Montane'=rgb2(200,143,200),
                               'Northern Woodland'=rgb2(110, 175, 75),
                               'Southern Woodland'=rgb2(55, 145, 130))) +
  theme_bw() + 
  labs(y='Coefficient for 24-Month SPEI',
       x='Percent of Nearby Land Uncultivated') + 
  theme(legend.position = 'none')

ggsave('~/lc-malnutrition-tex/AEZ_effects_q90.png', height = 3.5, width = 7)

texreg(mod, file = '~/lc-malnutrition-tex/Mod_Table_q90.tex', longtable=TRUE, custom.model.names="", 
       center=FALSE, use.packages=FALSE, caption='Parameter estimates for Generalized Additive Model estimating the varying coefficient of SPEI, with CBGPS weights censored at the 90th percentile.')



#####################################
# Model Censoring at 80%ile Weights
#####################################

load('AEZ_weights_GCV_natOnly_q80.Rdata')

dat <- plot(mod)
#Blank space to <enter> through plots









pltdat <- data.frame()
#Skip lat-long smooth, so start from 2:
for (i in 2:length(dat)){
  pltdat <- bind_rows(pltdat,
                      data.frame(natural=dat[[i]]$x, 
                                 se=dat[[i]]$se, 
                                 fit=dat[[i]]$fit,
                                 AEZ=dat[[i]]$ylab))
}

#SEs are already at two
pltdat$max <- pltdat$fit + pltdat$se
pltdat$min <- pltdat$fit - pltdat$se

map <- data.frame(AEZ=c("s(natural,3.24):afr.arid.123", "s(natural,3.24):afr.forest.4", 
                        "s(natural,3.16):nafr.sav.5", "s(natural,3.29):seafr.sav.6", 
                        "s(natural,2.36):afr.high.7", 
                        "s(natural,2):nafr.subforest.8", "s(natural,3.16):safr.subforest.9"),
                  Map=c("Arid", "Tropical Forest", "Northern Savanna", "Southern Savanna", 
                        "Montane", "Northern Woodland", "Southern Woodland"))

pltdat <- merge(pltdat, map)

rgb2 <- function(r, g, b){
  rgb(r/255, g/255, b/255)
}

pltdat$natural <- pltdat$natural*100

pltdat$Map <- factor(pltdat$Map, levels=c('Tropical Forest', 'Northern Woodland',
                                          'Northern Savanna', 'Arid',
                                          'Montane', 'Southern Woodland',
                                          'Southern Savanna'))

ggplot(pltdat) + 
  geom_ribbon(aes(x=natural, ymin=min, ymax=max, fill=Map)) + 
  geom_line(aes(x=natural, y=fit)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  facet_wrap(. ~ Map, ncol=4) + 
  scale_x_continuous(expand=c(0,0), breaks=c(25, 50, 75, 100), labels=function(x){paste0(x, '%')}) + 
  scale_y_continuous(sec.axis=dup_axis(name=sprintf('\u2190 More Drought Vulnerable'))) + 
  scale_fill_manual(values = c('Arid'=rgb2(255, 225, 175),
                               'Tropical Forest'=rgb2(50, 125, 0),
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135),
                               'Montane'=rgb2(200,143,200),
                               'Northern Woodland'=rgb2(110, 175, 75),
                               'Southern Woodland'=rgb2(55, 145, 130))) +
  theme_bw() + 
  labs(y='Coefficient for 24-Month SPEI',
       x='Percent of Nearby Land Uncultivated') + 
  theme(legend.position = 'none')

ggsave('~/lc-malnutrition-tex/AEZ_effects_q80.png', height = 3.5, width = 7)

texreg(mod, file = '~/lc-malnutrition-tex/Mod_Table_q80.tex', longtable=TRUE, custom.model.names="", 
       center=FALSE, use.packages=FALSE, caption='Parameter estimates for Generalized Additive Model estimating the varying coefficient of SPEI, with CBGPS weights censored at the 80th percentile.')

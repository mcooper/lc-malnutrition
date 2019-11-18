setwd('G://My Drive/lc-malnutrition/GAMs')

library(mgcv)
library(tidyverse)
library(texreg)

load('AEZ_weights_GCV_natOnly.Rdata')

dat <- plot(mod)








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
                        "s(natural,2.73):nafr.sav.5", "s(natural,3.2):seafr.sav.6", "s(natural,2.76):afr.high.7", 
                        "s(natural,2):nafr.subforest.8", "s(natural,2.97):safr.subforest.9"),
                  Map=c("Arid", "Forest", "Northern Savanna", "Southern Savanna", "Highlands", "Northern Semi-Forest", 
                        "Southern Semi-Forest"))

pltdat <- merge(pltdat, map)

rgb2 <- function(r, g, b){
  rgb(r/255, g/255, b/255)
}

pltdat$natural <- pltdat$natural*100



pltdat$Map <- factor(pltdat$Map, levels=c('Forest', 'Northern Semi-Forest',
                                          'Northern Savanna', 'Arid',
                                          'Highlands', 'Southern Semi-Forest',
                                          'Southern Savanna'))

ggplot(pltdat) + 
  geom_ribbon(aes(x=natural, ymin=min, ymax=max, fill=Map)) + 
  geom_line(aes(x=natural, y=fit)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  facet_wrap(. ~ Map, ncol=4) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_manual(values = c('Arid'=rgb2(255, 225, 175),
                               'Forest'=rgb2(50, 125, 0),
                               'Northern Savanna'=rgb2(179, 210, 52),
                               'Southern Savanna'=rgb2(222, 231, 135),
                               'Highlands'=rgb2(200,143,200),
                               'Northern Semi-Forest'=rgb2(110, 175, 75),
                               'Southern Semi-Forest'=rgb2(55, 145, 130))) +
  theme_bw() + 
  labs(y='Coefficient for 24-Month SPEI',
       x='Percent of Nearby Landcover Uncultivated') + 
  theme(legend.title = element_blank())

ggsave('C://Users/matt/lc-malnutrition-tex/AEZ_effects_2Row.png', height = 4, width = 8)

ggplot(pltdat %>% filter(Map %in% c("Northern Semi-Forest", "Southern Semi-Forest"))) + 
  geom_ribbon(aes(x=natural, ymin=min, ymax=max, fill=Map)) + 
  geom_line(aes(x=natural, y=fit)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  facet_wrap(. ~ Map, ncol=2) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_manual(values = c('Northern Semi-Forest'=rgb2(110, 175, 75),
                               'Southern Semi-Forest'=rgb2(55, 145, 130))) +
  theme_bw() + 
  labs(y='Coefficient for 24-Month SPEI',
       x='Percent of Nearby Landcover Uncultivated') + 
  theme(legend.position = "none")

ggsave('C://Users/matt/lc-malnutrition-tex/AEZ_effects_sel.png', height = 2.5, width = 7)

texreg(mod, file = 'C://Users/matt/lc-malnutrition-tex/Mod_Table.tex', longtable=TRUE, custom.model.names="", 
       center=FALSE, use.packages=FALSE, caption='Parameter estimates for Generalized Additive Model estimating the varying coefficient of SPEI')

library(tidyverse)
library(mgcv)
library(viridisLite)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
aez <- read.csv('AEZ.csv')

##################
#Just Africa
#################
all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, aez)) %>%
  filter(latitude < 20 & longitude > -25 & longitude < 75) %>%
  na.omit

all$market_dist <- log(all$market_dist + 1)

all$nafr.arid.1 <- (all$AEZ_new == 'nafr.arid.1')*all$spei24
all$eafr.arid.2 <- (all$AEZ_new == 'eafr.arid.2')*all$spei24
all$safr.arid.3 <- (all$AEZ_new == 'safr.arid.3')*all$spei24
all$afr.forest.4 <- (all$AEZ_new == 'afr.forest.4')*all$spei24
all$nafr.sav.5 <- (all$AEZ_new == 'nafr.sav.5')*all$spei24
all$seafr.sav.6 <- (all$AEZ_new == 'seafr.sav.6')*all$spei24
all$afr.high.7 <- (all$AEZ_new == 'afr.high.7')*all$spei24
all$afr.subforest.8 <- (all$AEZ_new == 'afr.subforest.8')*all$spei24


mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             s(market_dist, natural, by=nafr.arid.1) + 
             s(market_dsit, natural, by=eafr.arid.2) + 
             s(market_dist, natural, by=safr.arid.3) + 
             s(market_dist, natural, by=afr.forest.4) + 
             s(market_dist, natural, by=nafr.sav.5) + 
             s(market_dist, natural, by=seafr.sav.6) + 
             s(market_dist, natural, by=afr.high.7) + 
             s(market_dist, natural, by=afr.subforest.8) - 1, 
           data=all %>% filter(spei24 < 1))

save(mod, file = 'G://My Drive/lc-malnutrition/GAMs/AEZ_Gam.Rdata')

for (i in unique(all$AEZ_new)){
  df <- expand.grid(list(natural=seq(0, 1, length.out=100),
                         market_dist=seq(min(all$market_dist), max(all$market_dist), length.out = 100)))
  
  df$AEZ_new <- i
  df[ , gsub(' ', '.', as.character(unique(all$AEZ_new)))] <- 0
  df[ , gsub(' ', '.', i)] <- 1
  df$age <- 1
  df$birth_order <- 1
  df$hhsize <- 10
  df$sex <- "Male"
  df$mother_years_ed <- 10
  df$toilet <- "Other"
  df$interview_year <- 2000
  df$calc_birthmonth <- 3
  df$head_age <- 40
  df$head_sex <- "Male"
  df$wealth_norm <- 0
  df$latitude <- 0
  df$longitude <- 0
  
  p <- predict(mod, df, type='terms', se=T)
  
  df$smooth <- p$fit[ , paste0('s(market_dist,natural):', gsub(' ', '.', i))]
  df$smooth_se <- p$se.fit[ , paste0('s(market_dist,natural):', gsub(' ', '.', i))]
  
  sel <- df[ , c('smooth', 'market_dist', 'natural', 'smooth_se')]
  sel$AEZ <- i
  
  comb <- bind_rows(comb, sel)  
}

#Plot all
for (i in unique(comb$AEZ)){
  ggplot() + 
    geom_tile(data=comb %>% filter(AEZ==i), aes(x=market_dist, y=natural, fill=smooth)) + 
    geom_point(data=all %>% filter(AEZ_new == i), 
               aes(x=market_dist, y=natural), shape=16, size=0.1, alpha=0.5) +
    scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_viridis_c() + 
    labs(title=i)
  
  ggsave(paste0('G://My Drive/lc-malnutrition/GAMs/', i, '.png'))
}

comb$significant <- (abs(comb$smooth) - comb$smooth_se*2) > 0

#Plot only significant vars
for (i in unique(comb$AEZ)){
  ggplot() + 
    geom_tile(data=comb %>% filter(AEZ==i), aes(x=market_dist, y=natural, fill=smooth)) + 
    geom_point(data=comb %>% filter(AEZ==i), 
               aes(x=market_dist, y=natural, color=significant), shape=16, size=0.1, alpha=0.5) +
    scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_viridis_c() + 
    scale_color_manual(values=c('black', 'white')) + 
    labs(title=i)
  
  ggsave(paste0('G://My Drive/lc-malnutrition/GAMs/', i, '_signif.png'))
}





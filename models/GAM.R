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

all$Arid.NAfrica <- (all$AEZ_new == 'Arid NAfrica')*all$spei24
all$Arid.SAfrica <- (all$AEZ_new == 'Arid SAfrica')*all$spei24
all$Forest.Africa <- (all$AEZ_new == 'Forest Africa')*all$spei24
all$Highlands.Africa <- (all$AEZ_new == 'Highlands Africa')*all$spei24
all$LAC.Highlands <- (all$AEZ_new == 'LAC Highlands')*all$spei24
all$Savanna.NAfrica <- (all$AEZ_new == 'Savanna NAfrica')*all$spei24
all$Savanna.SEAfrica <- (all$AEZ_new == 'Savanna SEAfrica')*all$spei24
all$SemiForest.Africa <- (all$AEZ_new == 'SemiForest Africa')*all$spei24
  
mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             #s(latitude, longitude, bs='sos') + 
             s(market_dist, natural, by=Arid.NAfrica) + 
             s(market_dist, natural, by=Arid.SAfrica) + 
             #s(market_dist, natural, by=Forest.Africa) + 
             s(market_dist, natural, by=Highlands.Africa) + 
             s(market_dist, natural, by=LAC.Highlands) + 
             s(market_dist, natural, by=Savanna.NAfrica) + 
             s(market_dist, natural, by=Savanna.SEAfrica) + 
             s(market_dist, natural, by=SemiForest.Africa), 
           data=all,
           method='REML',
           select=TRUE)

comb <- data.frame()

for (i in unique(all$AEZ_new)){
  df <- expand.grid(list(natural=seq(0, 1, length.out=100),
                         market_dist=seq(min(modsel$market_dist), max(modsel$market_dist), length.out = 100),
                         spei24=1))
  
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
  df$AEZ_new <- i
  
  p <- predict(mod, df, type='terms', se=T)
  
  df$smooth <- p$fit[ , 's(market_dist,natural):spei24']
  df$smooth_se <- p$se.fit[ , 's(market_dist,natural):spei24']
  
  sel <- df %>% 
    select(smooth, market_dist, natural, spei24, smooth_se) %>%
    mutate(AEZ=i)
  

  comb <- bind_rows(comb, sel)  
}

#Plot all
for (i in unique(comb$AEZ)){
  ggplot() + 
    geom_tile(data=comb %>% filter(AEZ==i), aes(x=market_dist, y=natural, fill=smooth)) + 
    geom_point(data=all %>% filter(spei24 < 1 & market_dist > 2 & market_dist < 6 & AEZ_new == i), 
               aes(x=market_dist, y=natural), shape=16, size=0.1, alpha=0.5) +
    scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_viridis_c() + 
    labs(title=i)
  
  ggsave(paste0('C://Users/matt/Desktop/', i, '.png'))
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
  
  ggsave(paste0('C://Users/matt/Desktop/', i, '_signif.png'))
}








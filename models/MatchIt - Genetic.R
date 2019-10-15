setwd('~/../mattcoop/mortalityblob/dhs')

library(MatchIt)
library(tidyverse)
library(Hmisc)
library(lme4)
library(ggplot2)

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
aez <- read.csv('AEZ.csv')

##################
#Just Africa
#################
all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, aez)) %>%
  filter(latitude < 20 & longitude > -25 & longitude < 75 & builtup < 0.05) %>%
  na.omit

all$population <- log(all$population + 1)
all$grid_gdp <- log(all$grid_gdp)
all$urbanization <- log(all$urban*1000 + 1)
all$market_dist <- log(all$market_dist + 1)


hhmatch <- data.frame()
geomatch <- data.frame()
allmatch <- data.frame()

for (i in unique(all$AEZ_new)){
  
  cat("Working on", i, '\n', file = '~/genmatch', append = T)
  
  ix <- all$AEZ_new==i
  
  all$NatClass[ix] <- ifelse(all$natural[ix] > median(all$natural[ix]), "Nature", "Ag")
  
  all$NatBin <- all$NatClass == 'Nature'
  
  tryCatch({
    geo.out <- matchit(NatBin ~ population + grid_gdp + market_dist +
                         urbanization + interview_year,
                       method='genetic', replace=TRUE, data=all %>% filter(AEZ_new == i))
    save(geo.out, file = paste0('../lc_gams/matchgeo-', i, '-genetic.Rdata'))
    geomatch <- bind_rows(geomatch, match.data(geo.out))
  }, error = function(e){print(e);cat("Failure on geo for", i, "\n", file = '~/genmatch', append = T)})
  
  tryCatch({
    hh.out <- matchit(NatBin ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
                        head_age + head_sex + wealth_norm,
                      method='genetic', replace=TRUE, data=all %>% filter(AEZ_new == i))
    save(hh.out, file = paste0('../lc_gams/matchhh-', i, '-genetic.Rdata'))
    hhmatch <- bind_rows(hhmatch, match.data(hh.out))
  }, error = function(e){print(e);cat("Failure on hh for", i, "\n", file = '~/genmatch', append = T)})
  
  tryCatch({
    all.out <- matchit(NatBin ~ population + grid_gdp + market_dist +
                         urbanization + interview_year + age + birth_order + hhsize + sex + 
                         mother_years_ed + toilet +
                         head_age + head_sex + wealth_norm,
                       method='genetic', replace=TRUE, data=all %>% filter(AEZ_new == i))
    save(all.out, file = paste0('../lc_gams/matchall-', i, '-genetic.Rdata', file = '~/genmatch', append = T))
    allmatch <- bind_rows(allmatch, match.data(all.out))
  }, error = function(e){print(e);cat("Failure on all for", i, "\n")})
}

write.csv(allmatch, 'dhs-africa-matched-all-genetic.csv', row.names=F)
write.csv(hhmatch, 'dhs-africa-matched-hh-genetic.csv', row.names=F)
write.csv(geomatch, 'dhs-africa-matched-geo-genetic.csv', row.names=F)

system('~/telegram.sh "Matching Done!"')


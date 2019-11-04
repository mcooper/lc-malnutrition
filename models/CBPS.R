setwd('~/mortalityblob/dhs/')

#Based on 
#https://imai.fas.harvard.edu/research/files/CBGPS.pdf

options(stringsAsFactors=F)

library(tidyverse)
library(CBPS)
library(weights)

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv') %>%
  dplyr::select(-AEZ, -AEZ_new, -AEZ_class, -AEZ_all)
lc <- read.csv('landcover_processed.csv')
aez <- read.csv('AEZ.csv')

##################
#Just Africa
#################
all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, aez)) %>%
  filter(latitude < 20 & longitude > -25 & longitude < 75 & spei24 < 1) %>%
  #try subsetting to just rural areas without a lot of water
  filter(urban < 0.01 & nat_water < 0.05) %>%
  na.omit

#Agregate arid africa, which has the fewest observations
all$AEZ_new[all$AEZ_new == 'eafr.arid.2'] <- 'afr.arid.123'
all$AEZ_new[all$AEZ_new == 'safr.arid.3'] <- 'afr.arid.123'
all$AEZ_new[all$AEZ_new == 'nafr.arid.1'] <- 'afr.arid.123'

allweight <- data.frame()
for (aez in unique(all$AEZ_new)){
  print(aez)
  
  sel <- all %>%
    filter(AEZ_new == aez)
  
  nfe.pscore.form <- (natural ~ I(log(population+1)) + I(log(grid_gdp)) + I(log(imports_percap + 1)) + I(log(market_dist + 1)))
  
  pscorefit.nfe.np <- npCBPS(nfe.pscore.form, data = sel, corprior=0.1/nrow(sel))
  
  print(summary(lm(natural ~ I(log(population+1)) + I(log(imports_percap)) + I(log(imports_percap + 1)) + I(log(market_dist + 1)), 
                   data=sel)))
  
  cat("Population: ", cor(sel$natural, log(sel$population + 1)), '\n')
  cat("GDP: ", cor(sel$natural, log(sel$imports_percap + 1)), '\n')
  cat("Imports: ", cor(sel$natural, log(sel$imports_percap + 1)), '\n')
  
  sel$weights <- pscorefit.nfe.np$weights
  
  print(summary(lm(natural ~ I(log(population+1)) + I(log(grid_gdp)) + I(log(imports_percap + 1)) + I(log(market_dist + 1)), 
                   data=sel, weights = weights)))
  
  cat("Population: ", wtd.cor(sel$natural, log(sel$population + 1), weight=sel$weights)[1], '\n')
  cat("GDP: ", wtd.cor(sel$natural, log(sel$imports_percap + 1), weight=sel$weights)[1], '\n')
  cat("Imports: ", wtd.cor(sel$natural, log(sel$imports_percap + 1), weight=sel$weights)[1], '\n')
  
  allweight <- bind_rows(allweight, sel)
}

write.csv(allweight, '~/mortalityblob/dhs/lc-malnutrition-weights2.csv', row.names=F)

system('~/telegram.sh "landcover weights assigned!"')

system('shutdown')



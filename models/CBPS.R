setwd('~/mortalityblob/dhs/')

#Based on 
#https://imai.fas.harvard.edu/research/files/CBGPS.pdf

library(tidyverse)
library(CBPS)

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

allweight <- data.frame()
for (aez in unique(all$AEZ_new)){
  print(aez)
  
  sel <- all %>%
    filter(AEZ_new == aez)
  
  nfe.pscore.form <- (natural ~ I(log(population+1)) + I(log(grid_gdp)) + government_effectiveness + stability_violence + 
                        I(log(imports_percap + 1)) + I(log(assistance + 1)) + enrollment)
  
  pscorefit.nfe.np <- npCBPS(nfe.pscore.form, data = sel, corprior=0.1/nrow(sel))
  
  sel$weights <- pscorefit.nfe.np$weights
  
  allweight <- bind_rows(allweight, sel)
}

write.csv(allweight, 'lc-malnutrition-weights.csv', row.names=F)

system('~/telegram.sh "landcover weights assigned!"')




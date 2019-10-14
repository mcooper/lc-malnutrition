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

#Round for exact matching
all$population <- round(log(all$population + 1))
all$grid_gdp <- round(log(all$grid_gdp))
all$urbanization <- round(log(all$urban*1000 + 1))
all$market_dist <- round(log(all$market_dist + 1))

all$age <- round(all$age/12)
all$birth_order[all$birth_order > 5] <- 5
all$hhsize[all$hhsize > 10] <- 10
all$mother_years_ed <- round(all$mother_years_ed/5)
all$head_age <- round(all$head_age/10)
all$wealth_norm <- round(all$wealth_norm*5)

hhmatch <- data.frame()
geomatch <- data.frame()
allmatch <- data.frame()

for (i in unique(all$AEZ_new)){
  
  cat("Working on", i, '\n')
  
  ix <- all$AEZ_new==i
  
  all$NatClass[ix] <- ifelse(all$natural[ix] > median(all$natural[ix]), "Nature", "Ag")
  
  all$NatBin <- all$NatClass == 'Nature'
  
  tryCatch({
    geo.out <- matchit(NatBin ~ population + grid_gdp + market_dist +
                         urbanization + interview_year,
                       method='exact', replace=TRUE, data=all %>% filter(AEZ_new == i))
    save(geo.out, file = paste0('../lc_gams/matchgeo-', i, '-exact.Rdata'))
    geomatch <- bind_rows(geomatch, match.data(geo.out))
  }, error = function(e){print(e);cat("Failure on geo for", i, "\n")})
  
  tryCatch({
    hh.out <- matchit(NatBin ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
                        head_age + head_sex + wealth_norm,
                      method='exact', replace=TRUE, data=all %>% filter(AEZ_new == i))
    save(hh.out, file = paste0('../lc_gams/matchhh-', i, '-exact.Rdata'))
    hhmatch <- bind_rows(hhmatch, match.data(hh.out))
  }, error = function(e){print(e);cat("Failure on hh for", i, "\n")})
  
  tryCatch({
    all.out <- matchit(NatBin ~ population + grid_gdp + market_dist +
                         urbanization + interview_year + age + birth_order + hhsize + sex + 
                         mother_years_ed + toilet +
                         head_age + head_sex + wealth_norm,
                       method='exact', replace=TRUE, data=all %>% filter(AEZ_new == i))
    save(all.out, file = paste0('../lc_gams/matchall-', i, '-exact.Rdata'))
    allmatch <- bind_rows(allmatch, match.data(all.out))
  }, error = function(e){print(e);cat("Failure on all for", i, "\n")})
}

write.csv(allmatch, 'dhs-africa-matched-all-exact.csv', row.names=F)
write.csv(hhmatch, 'dhs-africa-matched-hh-exact.csv', row.names=F)
write.csv(geomatch, 'dhs-africa-matched-geo-exact.csv', row.names=F)

system('~/telegram.sh "Matching Done!"')


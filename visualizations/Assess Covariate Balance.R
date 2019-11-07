setwd('G://My Drive/DHS Processed/')

library(weights)
library(tidyverse)

options(stringsAsFactors=F)

dat <- read.csv('lc-malnutrition-weights2.csv')

dat$population <- log(dat$population + 1)
dat$grid_gdp <- log(dat$grid_gdp)
dat$imports_percap <- log(dat$imports_percap + 1)
dat$market_dist <- log(dat$market_dist + 1)
                                                                                   
                                    
sum <- dat %>%
  select(AEZ_new, natural, population, grid_gdp, imports_percap, market_dist, weights) %>%
  gather(covariate, value, -AEZ_new, -natural, -weights) %>%
  group_by(AEZ_new, covariate) %>%
  summarize(Unweighted = cor(natural, value),
            Weighted = wtd.cor(x=natural, y=value, weight=weights)[1])

aez_map <- data.frame(AEZ_new = c("safr.subforest.9", "afr.high.7", "seafr.sav.6", "afr.forest.4", 
                                  "nafr.sav.5", "nafr.subforest.8", "afr.arid.123"),
                      AEZ=c("Southern Sub-Forest", "Highlands", "Southern Savanna","Forest", "Northern Savanna", 
                      "Northern Sub-Forest", "Arid"))

cov_map <- data.frame(covariate=c("grid_gdp", "imports_percap", "market_dist", "population"),
                      Covariate=c("Subnational GDP Per Capita", "Import Value Per Capita", "Time to Travel to Major City", "Population Density"))

sum <- Reduce(x=list(sum, aez_map, cov_map), f=function(x, y){merge(x, y, all.x=T)})

sum$Unweighted <- round(sum$Unweighted, 2)
sum$Weighted <- round(sum$Weighted, 2)

sel <- sum %>%
  select(AEZ, Covariate, Unweighted, Weighted) %>%
  arrange(AEZ, Covariate) %>%
  gather(Cor, Value, -AEZ, -Covariate) %>%
  mutate(Col = paste0(Covariate, " - ", Cor)) %>%
  select(-Covariate, -Cor) %>%
  spread(Col, Value)

#It would be really nice to get this into a latex data flow, but no time now, so do it manually in excel

write.csv(sel, 'C://Users/matt/Desktop/tmp.csv', row.names=F)


setwd('G://My Drive/DHS Processed')

library(MatchIt)
library(tidyverse)
library(Hmisc)
library(lme4)
library(ggplot2)

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
fs <- read.csv('FarmingSystems.csv')

#################
#Entire DHS
#################

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, fs)) %>%
  filter(market_dist > 150) %>%
  na.omit

haz_mod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_norm + (1|surveycode) + (1|country), data=all)

all$haz_residuals <- residuals(haz_mod)

whz_mod <- lmer(whz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                  head_age + head_sex + wealth_norm + (1|surveycode) + (1|country), data=all)

all$whz_residuals <- residuals(whz_mod)

sel <- all %>%
  mutate(natural = ifelse(natural > median(all$natural), TRUE, FALSE))

# sel$population <- round(log(sel$population + 1))
# sel$grid_gdp <- round(log(sel$grid_gdp))
# sel$urbanization <- round(log(sel$urban*1000 + 1))
# sel$interview_decade <- trunc(sel$interview_year/10)*10
# sel$mean_annual_precip <- round(log(sel$mean_annual_precip), 1)
# sel$market_dist <- round(log(sel$market_dist + 1))

sel$population <- log(sel$population + 1)
sel$grid_gdp <- log(sel$grid_gdp)
sel$urbanization <- log(sel$urban*1000 + 1)
sel$mean_annual_precip <- log(sel$mean_annual_precip + 1)
sel$market_dist <- log(sel$market_dist + 1)

m.out <- matchit(natural ~ population + spei24 + grid_gdp + market_dist + 
                   urbanization + interview_year + mean_annual_precip,
                 method='nearest', replace=TRUE, data=sel)

dat <- match.data(m.out)

write.csv(dat, 'G://My Drive/DHS Processed/dhs-all-matched.csv', row.names=F)


##################
#Just Africa
#################
all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, fs)) %>%
  filter(market_dist > 150 & continent=="ssa_fs_final") %>%
  na.omit

haz_mod <- lmer(haz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                  head_age + head_sex + wealth_norm + (1|surveycode) + (1|country), data=all)

all$haz_residuals <- residuals(haz_mod)

whz_mod <- lmer(whz_dhs ~ interview_year + age + birth_order + hhsize + sex + mother_years_ed + toilet +
                  head_age + head_sex + wealth_norm + (1|surveycode) + (1|country), data=all)

all$whz_residuals <- residuals(whz_mod)

sel <- all %>%
  mutate(natural = ifelse(natural > median(all$natural), TRUE, FALSE))

# sel$population <- round(log(sel$population + 1))
# sel$grid_gdp <- round(log(sel$grid_gdp))
# sel$urbanization <- round(log(sel$urban*1000 + 1))
# sel$interview_decade <- trunc(sel$interview_year/10)*10
# sel$mean_annual_precip <- round(log(sel$mean_annual_precip), 1)
# sel$market_dist <- round(log(sel$market_dist + 1))

sel$population <- log(sel$population + 1)
sel$grid_gdp <- log(sel$grid_gdp)
sel$urbanization <- log(sel$urban*1000 + 1)
sel$mean_annual_precip <- log(sel$mean_annual_precip + 1)
sel$market_dist <- log(sel$market_dist + 1)

m.out <- matchit(natural ~ population + spei24 + grid_gdp + market_dist + 
                   urbanization + interview_year + mean_annual_precip,
                 method='nearest', replace=TRUE, data=sel)

dat <- match.data(m.out)

write.csv(dat, 'G://My Drive/DHS Processed/dhs-africa-matched.csv', row.names=F)

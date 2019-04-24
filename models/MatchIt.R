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

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, fs)) %>%
  na.omit

mod1 <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_norm +
              as.factor(calc_birthmonth) + (1|country) + (1|surveycode), data=all)

all$residuals_re <- residuals(mod1)

mod2 <- lm(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
              head_age + head_sex + wealth_norm +
              as.factor(calc_birthmonth), data=all)


all$residuals_fe <- residuals(mod2)

all$spei <- cut2(all$spei24, g=5) %>%
  factor(labels=c("vlow", "low", "Normal", "high", "vhigh")) %>%
  relevel(ref = "Normal")

sel <- all %>%
  filter(natural > median(all$natural) | natural < median(all$natural)) %>%
  filter(market_dist > 150) %>%
  #filter(continent == "ssa_fs_final") %>%
  mutate(natural = ifelse(natural > median(all$natural), FALSE, TRUE))

sel$population <- round(log(sel$population + 1))
sel$grid_gdp <- round(log(sel$grid_gdp))
#sel$urbanization <- round(log(sel$urban*1000 + 1))
sel$interview_decade <- trunc(sel$interview_year/10)*10
sel$mean_annual_precip <- round(log(sel$mean_annual_precip), 1)
sel$market_dist <- round(log(sel$market_dist + 1))

sel$age <- round(sel$age/12)
sel$birth_order[sel$birth_order > 5] <- 5
sel$hhsize <- cut2(sel$hhsize, g=5)
sel$mother_years_ed <- cut2(sel$mother_years_ed, g=5)
sel$head_age <- cut2(sel$head_age, g=5)
sel$wealth_norm <- cut2(sel$wealth_norm, g=10)


m.out <- matchit(natural ~ population + spei + grid_gdp + market_dist + 
                   #urbanization + 
                   interview_decade + mean_annual_precip + new_farm_system + 
                   age + birth_order + hhsize + sex + mother_years_ed + toilet +
                   head_age + head_sex + wealth_norm,
                 method='exact', data=sel)

dat <- match.data(m.out)

out <- lm(haz_dhs ~ spei*natural, data=dat)

test <- expand.grid(list(natural=c(TRUE, FALSE), spei=c("vlow", "low", "Normal", "high", "vhigh")))
test$spei <- test$spei %>%
  as.factor %>%
  relevel(ref = "Normal")

test$outcome <- predict(out, test)

test$treat <- as.factor(test$natural)

ggplot(test) + geom_bar(aes(x=spei, y=outcome, fill=natural), 
                        position='dodge', stat='identity')



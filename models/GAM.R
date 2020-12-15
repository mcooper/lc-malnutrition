library(tidyverse)
library(mgcv)

options(stringsAsFactors=F)

setwd('~/mortalityblob/')

all <- read.csv('dhs/lc-malnutrition-weights2.csv')

all$market_dist <- log(all$market_dist + 1)

all$afr.arid.123 <- (all$AEZ_new == 'afr.arid.123')*all$spei24
all$afr.forest.4 <- (all$AEZ_new == 'afr.forest.4')*all$spei24
all$nafr.sav.5 <- (all$AEZ_new == 'nafr.sav.5')*all$spei24
all$seafr.sav.6 <- (all$AEZ_new == 'seafr.sav.6')*all$spei24
all$afr.high.7 <- (all$AEZ_new == 'afr.high.7')*all$spei24
all$nafr.subforest.8 <- (all$AEZ_new == 'nafr.subforest.8')*all$spei24
all$safr.subforest.9 <- (all$AEZ_new == 'safr.subforest.9')*all$spei24

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             s(natural, by=afr.arid.123) + 
             s(natural, by=afr.forest.4) + 
             s(natural, by=nafr.sav.5) + 
             s(natural, by=seafr.sav.6) + 
             s(natural, by=afr.high.7) + 
             s(natural, by=nafr.subforest.8) + 
             s(natural, by=safr.subforest.9) - 1, 
           data=all, weights = weights)

save(mod, file = 'lc_gams/AEZ_weights_GCV_natOnly.Rdata')

#Try again, censoring extreme weights to 90%
q90 <- quantile(all$weights, probs=0.9)

all$weights90 <- all$weights
all$weights90[all$weights90 < q90] <- q90

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             s(natural, by=afr.arid.123) + 
             s(natural, by=afr.forest.4) + 
             s(natural, by=nafr.sav.5) + 
             s(natural, by=seafr.sav.6) + 
             s(natural, by=afr.high.7) + 
             s(natural, by=nafr.subforest.8) + 
             s(natural, by=safr.subforest.9) - 1, 
           data=all, weights = weights90)

save(mod, file = 'lc_gams/AEZ_weights_GCV_natOnly_q90.Rdata')

#And again, censoring extreme weights to 90
q80 <- quantile(all$weights, probs=0.8)

all$weights80 <- all$weights
all$weights80[all$weights80 < q80] <- q80

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             s(natural, by=afr.arid.123) + 
             s(natural, by=afr.forest.4) + 
             s(natural, by=nafr.sav.5) + 
             s(natural, by=seafr.sav.6) + 
             s(natural, by=afr.high.7) + 
             s(natural, by=nafr.subforest.8) + 
             s(natural, by=safr.subforest.9) - 1, 
           data=all, weights = weights80)

save(mod, file = 'lc_gams/AEZ_weights_GCV_natOnly_q80.Rdata')


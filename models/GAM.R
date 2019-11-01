library(tidyverse)
library(mgcv)

options(stringsAsFactors=F)

setwd('~/mortalityblob/')

all <- read.csv('dhs/lc-malnutrition-weights.csv')

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
             te(market_dist, natural, by=afr.arid.123) + 
             te(market_dist, natural, by=afr.forest.4) + 
             te(market_dist, natural, by=nafr.sav.5) + 
             te(market_dist, natural, by=seafr.sav.6) + 
             te(market_dist, natural, by=afr.high.7) + 
             te(market_dist, natural, by=nafr.subforest.8) + 
             te(market_dist, natural, by=safr.subforest.9) - 1, 
           data=all, weights = weights, method='REML')

save(mod, file = 'lc_gams/AEZ_weights_REML.Rdata')


mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=afr.arid.123) + 
             te(market_dist, natural, by=afr.forest.4) + 
             te(market_dist, natural, by=nafr.sav.5) + 
             te(market_dist, natural, by=seafr.sav.6) + 
             te(market_dist, natural, by=afr.high.7) + 
             te(market_dist, natural, by=nafr.subforest.8) + 
             te(market_dist, natural, by=safr.subforest.9) - 1, 
           data=all, method='REML')

save(mod, file = 'lc_gams/AEZ_noweights_REML.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=afr.arid.123) + 
             te(market_dist, natural, by=afr.forest.4) + 
             te(market_dist, natural, by=nafr.sav.5) + 
             te(market_dist, natural, by=seafr.sav.6) + 
             te(market_dist, natural, by=afr.high.7) + 
             te(market_dist, natural, by=nafr.subforest.8) + 
             te(market_dist, natural, by=safr.subforest.9) - 1, 
           data=all, weights = weights)

save(mod, file = 'lc_gams/AEZ_weights_GCV.Rdata')


mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=afr.arid.123) + 
             te(market_dist, natural, by=afr.forest.4) + 
             te(market_dist, natural, by=nafr.sav.5) + 
             te(market_dist, natural, by=seafr.sav.6) + 
             te(market_dist, natural, by=afr.high.7) + 
             te(market_dist, natural, by=nafr.subforest.8) + 
             te(market_dist, natural, by=safr.subforest.9) - 1, 
           data=all)

save(mod, file = 'lc_gams/AEZ_noweights_GCV.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=spei24) - 1, 
           data=all, weights=weights, method='REML')

save(mod, file = 'lc_gams/AEZmerge_weights_REML.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=spei24) - 1, 
           data=all, weights=weights)

save(mod, file = 'lc_gams/AEZmerge_weights_GCV.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=spei24) - 1, 
           data=all, method='REML')

save(mod, file = 'lc_gams/AEZmerge_noweights_REML.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=spei24) - 1, 
           data=all)

save(mod, file = 'lc_gams/AEZmerge_noweights_GCV.Rdata')
system('~/telegram.sh "Done with gams"')

system('shutdown now')



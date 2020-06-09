library(tidyverse)
library(mgcv)

options(stringsAsFactors=F)

setwd('~/mortalityblob/')

all <- read.csv('dhs/lc-malnutrition-weights2.csv')

all$market_dist <- log(all$market_dist + 1)

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + 
            toilet + interview_year + 
            as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
            s(latitude, longitude, bs='sos') +
            te(mean_annual_precip, market_dist, natural, by=spei24), 
           data=all, method='REML')

save(mod, file = 'lc_gams/s3_noweights_te.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + 
            toilet + interview_year + 
            as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
            s(latitude, longitude, bs='sos') +
            te(mean_annual_precip, market_dist, natural, by=spei24), 
           data=all, weights = weights, method='REML')

save(mod, file = 'lc_gams/s3_weights_te.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + 
            toilet + interview_year + 
            as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
            s(latitude, longitude, bs='sos') +
            te(elevation, tmax_10yr_mean, mean_annual_precip, market_dist, natural, by=spei24),
           data=all, method='REML')

save(mod, file = 'lc_gams/s5_noweights_te.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + 
            toilet + interview_year + 
            as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
            s(latitude, longitude, bs='sos') +
            te(elevation, tmax_10yr_mean, mean_annual_precip, market_dist, natural, by=spei24),
           data=all, weights = weights, method='REML')

save(mod, file = 'lc_gams/s5_weights_te.Rdata')

system('~/telegram.sh "Done with gams"')

system('shutdown now')



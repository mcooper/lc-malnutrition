setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(lme4)

hha <- read.csv('HH_data_A.csv')
hha_n <- read.csv('HH_data_A_norm.csv')

hhb <- merge(hha_n, hha, all.x=T, all.y=F)

hhb <- merge(hhb, data.frame(wealth_index=c("Poorest", "Poorer", "Middle", "Richer", "Richest"), wealth_quint=seq(1,5)))



re_mod_norm <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
                     head_age + head_sex + wealth_norm +
                     as.factor(calc_birthmonth) + (1|country) + (1|surveycode), data=hhb)

re_mod_quint <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
                      head_age + head_sex + wealth_quint +
                      as.factor(calc_birthmonth) + (1|country) + (1|surveycode), data=hhb)

re_mod_index <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
                       head_age + head_sex + wealth_index +
                       as.factor(calc_birthmonth) + (1|country) + (1|surveycode), data=hhb)


fe_mod_norm <- lm(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
                      head_age + head_sex + wealth_norm +
                      as.factor(calc_birthmonth), data=hhb)

fe_mod_quint <- lm(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
                       head_age + head_sex + wealth_quint +
                       as.factor(calc_birthmonth), data=hhb)

fe_mod_index <- lm(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
                     head_age + head_sex + wealth_index +
                     as.factor(calc_birthmonth), data=hhb)

AIC(re_mod_norm)
AIC(re_mod_quint)
AIC(re_mod_index)

AIC(fe_mod_norm)
AIC(fe_mod_quint)
AIC(fe_mod_index)

#Normalized wealth is a much better predictor, both with and without fixed effects
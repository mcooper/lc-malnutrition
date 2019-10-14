setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(broom)
library(lmer4)

precip <- read.csv('PrecipIndicesShortTerm.csv')

all <- read.csv('dhs-africa-matched-all.csv') %>% 
  merge(precip)
geo <- read.csv('dhs-africa-matched-geo.csv') %>% 
  merge(precip)
hh <- read.csv('dhs-africa-matched-hh.csv') %>% 
  merge(precip)

getSPEIstat <- function(mod){
  tidy(mod) %>% filter(term=='spei24') %>% select(statistic) %>% as.numeric
}

droughtdf <- data.frame()
for (aez in unique(all$AEZ_new)){
  
  allmod <- lm(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + 
                 head_age + head_sex + wealth_norm, data=all %>% filter(AEZ_new==aez & spei24 < 1))
  geomod <- lm(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + 
                 head_age + head_sex + wealth_norm, data=geo %>% filter(AEZ_new==aez & spei24 < 1))
  hhmod <- lm(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + 
                head_age + head_sex + wealth_norm, data=hh %>% filter(AEZ_new==aez & spei24 < 1))
  
  allmod_re <- lmer(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + 
                   head_age + head_sex + wealth_norm + (1|country) + (1|surveycode), data=all %>% filter(AEZ_new==aez & spei24 < 1))
  geomod_re <- lmer(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + 
                   head_age + head_sex + wealth_norm + (1|country) + (1|surveycode), data=geo %>% filter(AEZ_new==aez & spei24 < 1))
  hhmod_re <- lmer(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + 
                  head_age + head_sex + wealth_norm + (1|country) + (1|surveycode), data=hh %>% filter(AEZ_new==aez & spei24 < 1))
  
  droughtdf <- bind_rows(droughtdf, data.frame(AEZ=aez,
                                 matching=c('all', 'geo', 'hh'),
                                 ne.spei.tvalue=c(getSPEIstat(allmod),
                                               getSPEIstat(geomod),
                                               getSPEIstat(hhmod)),
                                 re.spei.tvalue=c(getSPEIstat(allmod_re),
                                               getSPEIstat(geomod_re),
                                               getSPEIstat(hhmod_re)),
                                 ne.AIC=c(AIC(allmod), AIC(geomod), AIC(hhmod)),
                                 re.AIC=c(AIC(allmod_re), AIC(geomod_re), AIC(hhmod_re))))
  
}

droughtdf$NoReBetter <- droughtdf$ne.AIC < droughtdf$re.AIC

#It looks like drought has an effect of SPEI scores in every AEZ of Africa!


#Try again list lmer?, so the coefs are the same for everything except SPEI
combDroughtMod <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + 
                         head_age + head_sex + wealth_norm + (1 + spei24|AEZ_new), data=geo %>% filter(spei24 < 1))

re <- ranef(combDroughtMod)

geo$AEZ.Nat <- paste0(geo$AEZ_new, geo$NatBin)

combDroughtNatureMod <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + 
                         head_age + head_sex + wealth_norm + (1 + spei24|AEZ.Nat), data=geo %>% filter(spei24 < 1))

re <- ranef(combDroughtNatureMod) %>% data.frame

re %>%
  filter(term=='spei24') %>%
  select(grp, condval) %>%
  mutate(natural=grepl('TRUE', grp),
         grp=gsub('TRUE|FALSE', '', grp)) %>%
  spread(natural, condval) %>%
  mutate(`TRUE` - `FALSE`)

#In every fucking case, the more natural area is more spei-vulnerable.  WTF!!

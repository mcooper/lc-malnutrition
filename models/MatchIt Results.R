setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(broom)
library(lme4)

all <- read.csv('dhs-africa-matched-all-bundoks.csv')
geo <- read.csv('dhs-africa-matched-geo-bundoks.csv')
hh <- read.csv('dhs-africa-matched-hh-bundoks.csv')

getSPEIstat <- function(mod){
  tidy(mod) %>% filter(term=='spei24') %>% select(statistic) %>% as.numeric
}

droughtdf <- data.frame()
for (aez in unique(all$AEZ_new)){
  
  geomod <- lm(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + population + grid_gdp + urban + market_dist + 
                 head_age + head_sex + wealth_norm, data=geo %>% filter(AEZ_new==aez & spei24 < 1))
  hhmod <- lm(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + population + grid_gdp + urban + market_dist + 
                head_age + head_sex + wealth_norm, data=hh %>% filter(AEZ_new==aez & spei24 < 1))
  
  geomod_re <- lmer(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + population + grid_gdp + urban + market_dist + 
                   head_age + head_sex + wealth_norm + (1|country) + (1|surveycode), data=geo %>% filter(AEZ_new==aez & spei24 < 1))
  hhmod_re <- lmer(haz_dhs ~ spei24 + age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + population + grid_gdp + urban + market_dist + 
                  head_age + head_sex + wealth_norm + (1|country) + (1|surveycode), data=hh %>% filter(AEZ_new==aez & spei24 < 1))
  
  droughtdf <- bind_rows(droughtdf, data.frame(AEZ=aez,
                                 matching=c('geo', 'hh'),
                                 ne.spei.tvalue=c(getSPEIstat(geomod),
                                               getSPEIstat(hhmod)),
                                 re.spei.tvalue=c(getSPEIstat(geomod_re),
                                               getSPEIstat(hhmod_re)),
                                 ne.AIC=c(AIC(geomod), AIC(hhmod)),
                                 re.AIC=c(AIC(geomod_re), AIC(hhmod_re))))
  
}

droughtdf$NoReBetter <- droughtdf$ne.AIC < droughtdf$re.AIC

#It looks like drought has an effect of SPEI scores in every AEZ of Africa!


#Try again list lmer?, so the coefs are the same for everything except SPEI
combDroughtMod <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + population + grid_gdp + urban + market_dist + 
                         head_age + head_sex + wealth_norm + (1 + spei24|AEZ_new), data=hh %>% filter(spei24 < 1))

re <- ranef(combDroughtMod)

all$AEZ.Nat <- paste0(all$AEZ_new, all$NatBin)

combDroughtNatureMod <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + as.factor(calc_birthmonth) + population + grid_gdp + urban + market_dist + 
                         head_age + head_sex + wealth_norm + (1 + spei24|AEZ.Nat), data=all %>% filter(spei24 < 1))

re <- ranef(combDroughtNatureMod) %>% data.frame

re %>%
  filter(term=='spei24') %>%
  select(grp, condval) %>%
  mutate(natural=grepl('TRUE', grp),
         grp=gsub('TRUE|FALSE', '', grp)) %>%
  spread(natural, condval) %>%
  mutate(`TRUE` - `FALSE`)

#In every fucking case, the more natural area is more spei-vulnerable.  WTF!!

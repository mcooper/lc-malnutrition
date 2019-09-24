library(dplyr)
library(broom)
library(lme4)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A_norm.csv') %>%
  mutate(country=substr(code, 1, 2))
spei <- read.csv('PrecipIndices.csv')
spei_short <- read.csv('PrecipIndicesShortTerm.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, spei_short))

#Filter to just Africa
all <- all %>%
  filter(latitude < 25 & longitude > -18 & longitude < 52 & !country %in% c('EG'))

for (i in c("spei12", "spei24", "spei36", "spei1", "spei2", "spei3", "spei6")){
  all <- all[!is.infinite(all[ , i]) & !is.na(all[ , i]) & !is.nan(all[ , i]), ]
}

summarydf <- data.frame()
for (i in c("spei12", "spei24", "spei36", "spei1", "spei2", "spei3", "spei6")){
  all$spei <- all[ , i]
  
  print(i)
  
  haz_mod <- lmer(haz_dhs ~ age + birth_order + sex + as.factor(calc_birthmonth) + 
                    mother_years_ed + toilet + hhsize + 
                    head_age + head_sex + wealth_norm + 
                    interview_year + spei + (1|country) + (1|surveycode), 
         data=all)
  
  whz_mod <- lmer(whz_dhs ~ age + birth_order + sex + as.factor(calc_birthmonth) + 
                    mother_years_ed + toilet + hhsize + 
                    head_age + head_sex + wealth_norm + 
                    interview_year + spei + (1|country) + (1|surveycode), 
                  data=all)
  
  
  haz_res <- tidy(haz_mod) %>%
    filter(term=='spei') %>%
    mutate(spei=i,
           AIC=AIC(haz_mod),
           y='haz',
           loglik=summary(haz_mod)$logLik,
           rsq=cor(residuals(haz_mod), all$haz_dhs))
  
  whz_res <- tidy(whz_mod) %>%
    filter(term=='spei') %>%
    mutate(spei=i,
           AIC=AIC(whz_mod),
           y='whz',
           loglik=summary(whz_mod)$logLik,
           rsq=cor(residuals(whz_mod), all$whz_dhs))

  summarydf <- bind_rows(summarydf, haz_res, whz_res)
  
}



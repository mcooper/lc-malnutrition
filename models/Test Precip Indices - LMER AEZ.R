library(dplyr)
library(broom)
library(lme4)
library(mgcv)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A_norm.csv') %>%
  mutate(country=substr(code, 1, 2))
spei <- read.csv('PrecipIndices.csv')
spei_short <- read.csv('PrecipIndicesShortTerm.csv')
aez <- read.csv('AEZ.csv')

comb <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, spei_short, aez))

#Filter to just Africa
all <- comb #%>%
  #filter(latitude < 25 & longitude > -18 & longitude < 52 & !country %in% c('EG'))

for (i in c("spei12", "spei24", "spei36", "spei1", "spei2", "spei3", "spei6")){
  all <- all[!is.infinite(all[ , i]) & !is.na(all[ , i]) & !is.nan(all[ , i]), ]
}

summarydf <- data.frame()
for (i in c("spei12", "spei24", "spei36", "spei1", "spei2", "spei3", "spei6")){
  all$spei <- all[ , i]
  
  for (z in levels(aez$AEZ_new)){
    
    sel <- all %>% filter(AEZ_new==z)
    
    print(i)
    
    haz_mod <- lm(haz_dhs ~ age + birth_order + sex + as.factor(calc_birthmonth) + 
                      mother_years_ed + toilet + hhsize + 
                      head_age + head_sex + wealth_norm + 
                      interview_year + spei + country, 
           data=sel)
    
    assign(paste0(i, '_haz_mod'), haz_mod)
    
    whz_mod <- lm(whz_dhs ~ age + birth_order + sex + as.factor(calc_birthmonth) + 
                      mother_years_ed + toilet + hhsize + 
                      head_age + head_sex + wealth_norm + 
                      interview_year + spei + country, 
                    data=sel)
    
    assign(paste0(i, '_whz_mod'), whz_mod)
    
    haz_res <- tidy(haz_mod) %>%
      filter(term=='spei') %>%
      mutate(spei=i,
             AIC=AIC(haz_mod),
             AEZ=z,
             y='haz',
             loglik=summary(haz_mod)$logLik,
             rsq=cor(residuals(haz_mod), sel$haz_dhs))
    
    whz_res <- tidy(whz_mod) %>%
      filter(term=='spei') %>%
      mutate(spei=i,
             AIC=AIC(whz_mod),
             AEZ=z,
             y='whz',
             loglik=summary(whz_mod)$logLik,
             rsq=cor(residuals(whz_mod), sel$whz_dhs))
  
    summarydf <- bind_rows(summarydf, haz_res, whz_res)

  }
}



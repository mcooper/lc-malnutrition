library(dplyr)
library(mgcv)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
spei_age <- read.csv('PrecipIndices_Individual.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, spei_age))

for (i in c("spei12", "spei24", "spei36", "spei12gs", "spei24gs", "spei36gs", 
            "spei_ageutero", "spei_gs_ageutero")){
  all <- all[!is.infinite(all[ , i]) & !is.na(all[ , i]) & !is.nan(all[ , i]), ]
}

for (i in c("spei12", "spei24", "spei36", "spei12gs", "spei24gs", "spei36gs", 
            "spei_ageutero", "spei_gs_ageutero")){
  all$spei <- all[ , i]
  
  print(i)
  
  assign(paste0(i, '_haz'), gamm(haz_dhs ~ age + birth_order + sex + as.factor(calc_birthmonth) + 
                   mother_years_ed + toilet + hhsize + 
         head_age + head_sex + wealth_norm + interview_year + 
         s(spei), random=list(country=~1, surveycode=~1), data=all))
  
  assign(paste0(i, '_whz'), gamm(whz_dhs ~ age + birth_order + sex + as.factor(calc_birthmonth) + 
                                   mother_years_ed + toilet + hhsize + 
                                   head_age + head_sex + wealth_norm + interview_year + 
                                   s(spei), random=list(country=~1, surveycode=~1), data=all))
}

##Evaluate by AIC
###############
AIC(spei12_haz)
AIC(spei24_haz)
AIC(spei36_haz)
AIC(spei12gs_haz)
AIC(spei24gs_haz)
AIC(spei36gs_haz)
AIC(spei_ageutero_haz)
AIC(spei_gs_ageutero_haz)
#lowest is spei24

AIC(spei12_whz)
AIC(spei24_whz)
AIC(spei36_whz)
AIC(spei12gs_whz)
AIC(spei24gs_whz)
AIC(spei36gs_whz)
AIC(spei_ageutero_whz)
AIC(spei_gs_ageutero_whz)
#lowest is spei36?
 
#Evaluate by R-squared
###################
cor(residuals(spei12_haz), all$haz_dhs)
cor(residuals(spei24_haz), all$haz_dhs)
cor(residuals(spei36_haz), all$haz_dhs)
cor(residuals(spei12gs_haz), all$haz_dhs)
cor(residuals(spei24gs_haz), all$haz_dhs)
cor(residuals(spei36gs_haz), all$haz_dhs)
cor(residuals(spei_ageutero_haz), all$haz_dhs)
cor(residuals(spei_gs_ageutero_haz), all$haz_dhs)
#highest is spei12gs

cor(residuals(spei12_whz), all$whz_dhs)
cor(residuals(spei24_whz), all$whz_dhs)
cor(residuals(spei36_whz), all$whz_dhs)
cor(residuals(spei12gs_whz), all$whz_dhs)
cor(residuals(spei24gs_whz), all$whz_dhs)
cor(residuals(spei36gs_whz), all$whz_dhs)
cor(residuals(spei_ageutero_whz), all$whz_dhs)
cor(residuals(spei_gs_ageutero_whz), all$whz_dhs)
#highest is spei_ageutero?
  
##Log Likelihood
##############################
summary(spei12_haz)$logLik
summary(spei24_haz)$logLik
summary(spei36_haz)$logLik
summary(spei12gs_haz)$logLik
summary(spei24gs_haz)$logLik
summary(spei36gs_haz)$logLik
summary(spei_ageutero_haz)$logLik
summary(spei_gs_ageutero_haz)$logLik
#lowest is spei24

summary(spei12_whz)$logLik
summary(spei24_whz)$logLik
summary(spei36_whz)$logLik
summary(spei12gs_whz)$logLik
summary(spei24gs_whz)$logLik
summary(spei36gs_whz)$logLik
summary(spei_ageutero_whz)$logLik
summary(spei_gs_ageutero_whz)$logLik
#lowest is spei36




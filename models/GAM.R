library(tidyverse)
library(mgcv)
library(Hmisc)

data <- read.csv('G://My Drive/DHS Processed/lc-malnutrition-weights.csv')
precip <- read.csv('G://My Drive/DHS Processed/PrecipIndicesShortTerm.csv')

comb <- merge(data, precip, all.x=T, all.y=F)

#data$urban_c <- cut2(data$urban, g=4)
#   
# whz_ti <- gam(whz_dhs ~ age + as.factor(calc_birthmonth) + 
#                 birth_order + hhsize + sex + mother_years_ed + toilet +
#                 head_age + head_sex + wealth_norm + interview_year + 
#                 s(spei24), data=data, weights=data$weights)

haz25gcv <- gamm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
                birth_order + hhsize + sex + mother_years_ed + toilet +
                head_age + head_sex + wealth_norm + interview_year +
                s(spei24), data=comb, random=list(country=~1, surveycode=~1))

haz25reml <- gamm(haz_dhs ~ age + as.factor(calc_birthmonth) + 
                  birth_order + hhsize + sex + mother_years_ed + toilet +
                  head_age + head_sex + wealth_norm + interview_year + 
                  s(spei24), data=comb, method='REML', random=list(country=~1, surveycode=~1))

data$urban_l <- cut2(data$urban, g=4)

whz_te <- gam(whz_dhs ~ age + as.factor(calc_birthmonth) + 
             birth_order + hhsize + sex + mother_years_ed + toilet +
             head_age + head_sex + wealth_norm + interview_year + 
             te(treat, spei24), data=data, weights=data$weights)

haz_te <- gam(haz_dhs ~ age + as.factor(calc_birthmonth) + 
             birth_order + hhsize + sex + mother_years_ed + toilet +
             head_age + head_sex + wealth_norm + interview_year + 
             te(treat, spei24), data=data, weights=data$weights)

plot(haz_ti, se=FALSE)
plot(haz_te, se=FALSE)


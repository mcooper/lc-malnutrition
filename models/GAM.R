library(tidyverse)
library(mgcv)
library(Hmisc)

data <- read.csv('G://My Drive/DHS Processed/lc-malnutrition-weights.csv')

data$urban_c <- cut2(data$urban, g=4)
  
whz_ti <- gam(whz_dhs ~ age + as.factor(calc_birthmonth) + 
                birth_order + hhsize + sex + mother_years_ed + toilet +
                head_age + head_sex + wealth_norm + interview_year + 
                ti(treat, spei24), data=data, weights=data$weights)

haz_ti <- gam(haz_dhs ~ age + as.factor(calc_birthmonth) + 
                birth_order + hhsize + sex + mother_years_ed + toilet +
                head_age + head_sex + wealth_norm + interview_year + 
                ti(treat, spei24), data=data, weights=data$weights)

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


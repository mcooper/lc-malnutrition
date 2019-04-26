setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(MatchIt)
library(Hmisc)
library(CBPS)

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
fs <- read.csv('FarmingSystems.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, fs)) %>%
  na.omit

mod1 <- lmer(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
               head_age + head_sex + wealth_norm +
               as.factor(calc_birthmonth) + (1|country) + (1|surveycode), data=all)

all$residuals_re <- residuals(mod1)

mod2 <- lm(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet +
             head_age + head_sex + wealth_norm +
             as.factor(calc_birthmonth), data=all)


all$residuals_fe <- residuals(mod2)


# Find the Box-Cox transformation of the treatment
# that makes its distribution closest to normal
# max.norm.cor<-cor(qqnorm(all$natural)$x,qqnorm(all$natural)$y)
# best.bc<-all$natural
# best.lambda<-1
# for (lambda in seq(-2,2,0.01)){
#   print(lambda)
#   
#   if (lambda != 0){
#     bc.natural<-((all$natural+1)^lambda - 1)/lambda
#   }
#   else{
#     bc.natural<-log(all$natural+1)
#   }
#   norm.cor<-cor(qqnorm(bc.natural, plot.it=FALSE)$x, qqnorm(bc.natural, plot.it=FALSE)$y)
#   
#   if (norm.cor > max.norm.cor){
#     max.norm.cor<-norm.cor
#     best.bc<-bc.natural
#     best.lambda<-lambda
#   }
# }
# 
# all$treat <- best.bc

all$treat <- all$natural

#ALSO CONTROL FOR MEAN ANNUAL PRECIPITATION!!!!
# Define the treatment model and the covariates associated with it
nfe.pscore.form <- (treat ~ I(log(population+1)) + I(log(market_dist + 1)) + I(log(grid_gdp)) + 
                      I(log(mean_annual_precip)) + spei24 + farm_system_new)
covars <- model.matrix(~ -1 + I(log(population+1)) + I(log(market_dist + 1)) + I(log(grid_gdp)) + 
                         I(log(mean_annual_precip)) + spei24 + farm_system_new,
                       data = all)

# Fit CBPS and npCBPS
#pscorefit.nfe.exact <- CBPS(nfe.pscore.form, data = sel, twostep = FALSE, method = "exact")
pscorefit.nfe.np <- npCBPS(nfe.pscore.form, data = sel, corprior=0.1/nrow(sel))

###################
#Look into cobalt and do some covariate balance weighting
###################
# 
# # Get point estimates that account for uncertainty in the weights (first column of Table 1)
# fit.outcome<-function(dat, wts, best.lambda){
#   outfit<-lm(haz_dhs ~ treat + I(treat^2), data = sel, weights=wts)
#   
#   dose <- ifelse(best.lambda != 0, ((1000+1)^best.lambda-1)/best.lambda, log(1000+1))
#   dose.eff <- coef(outfit)[2]*dose + coef(outfit)[3]*dose^2
#   
#   return(dose.eff)
# }



outfit <- lm(haz_dhs ~ treat + treat*spei, data = sel, weights=pscorefit.nfe.np$weights)

test <- expand.grid(list(treat=c(0, 1), spei=c('vlow', 'low', 'high', 'med', 'vhigh')))

test$outcome <- predict(outfit, test)

library(ggplot2)

test$treat <- as.factor(test$treat)
test$spei <- as.numeric(test$spei)

ggplot(test) + geom_line(aes(x=spei, y=outcome, color=treat))

# outfit <- lm(haz_dhs ~ treat + I(treat^2), data = sel, weights=pscorefit.nfe.exact$weights,
#              x = TRUE, y = TRUE)
# outfit.vcov <- vcov_outcome(pscorefit.nfe.exact, outfit$y, outfit$x, coef(outfit))
# 
# # Standard error for 1000 ad dose
# dose<-ifelse(best.lambda != 0, ((1000+1)^best.lambda-1)/best.lambda, log(1000+1))
# 
# dose.eff <- (dose*coef(outfit)[2] + dose^2*coef(outfit)[3])*1000
# dose.stderr <- sqrt(dose^2*outfit.vcov[2,2] + dose^4*outfit.vcov[3,3] + 2*dose^3*outfit.vcov[2,3])*1000
# dose.stderr
# c(dose.eff - 1.96*dose.stderr, dose.eff + 1.96*dose.stderr)

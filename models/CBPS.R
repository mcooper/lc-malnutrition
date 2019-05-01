setwd('G://My Drive/DHS Processed')

#Based on 
#https://imai.fas.harvard.edu/research/files/CBGPS.pdf

library(tidyverse)
library(MatchIt)
library(Hmisc)
library(CBPS)
library(mgcv)

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
fs <- read.csv('FarmingSystems.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, fs)) %>%
  filter(continent='ssa_fs_final')
  na.omit



# Find the Box-Cox transformation of the treatment
# that makes its distribution closest to normal
max.norm.cor<-cor(qqnorm(all$natural)$x,qqnorm(all$natural)$y)
best.bc<-all$natural
best.lambda<-1
for (lambda in seq(-2,2,0.01)){
  print(lambda)

  if (lambda != 0){
    bc.natural<-((all$natural+1)^lambda - 1)/lambda
  }
  else{
    bc.natural<-log(all$natural+1)
  }
  norm.cor<-cor(qqnorm(bc.natural, plot.it=FALSE)$x, qqnorm(bc.natural, plot.it=FALSE)$y)

  if (norm.cor > max.norm.cor){
    max.norm.cor<-norm.cor
    best.bc<-bc.natural
    best.lambda<-lambda
  }
}

all$treat <- best.bc

all$treat <- all$natural

sel <- all

#ALSO CONTROL FOR MEAN ANNUAL PRECIPITATION!!!!
# Define the treatment model and the covariates associated with it
nfe.pscore.form <- (treat ~ I(log(population+1)) + I(log(urban*100 + 1)) + I(log(grid_gdp)) + 
                      I(log(mean_annual_precip)) + spei24)
covars <- model.matrix(~ -1 + I(log(population+1)) + I(log(urban*100 + 1)) + I(log(grid_gdp)) + 
                         I(log(mean_annual_precip)) + spei24,
                       data = sel)

# Fit CBPS and npCBPS
#pscorefit.nfe.exact <- CBPS(nfe.pscore.form, data = sel, twostep = FALSE, method = "exact")
pscorefit.nfe.np <- npCBPS(nfe.pscore.form, data = sel, corprior=0.1/nrow(sel))

sel$weights <- pscorefit.nfe.np$weights

write.csv(sel, 'G://My Drive/DHS Processed/lc-malnutrition-weights.csv', row.names=F)

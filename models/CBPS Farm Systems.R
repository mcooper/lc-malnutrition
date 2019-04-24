setwd('G://My Drive/DHS Processed')

library(dplyr)
library(Hmisc)
library(CBPS)
library(broom)
library(foreach)
library(doParallel)

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
fs <- read.csv('FarmingSystems.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, fs)) %>%
  na.omit

all$spei <- cut2(all$spei24, g = 3) %>%
  as.numeric %>%
  factor(labels=c('low', 'med', 'high')) %>%
  relevel(ref='med')

sel <- all %>% 
  filter(market_dist > 2.5)

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

sel$treat <- sel$natural

fs <- table(sel[ , c('farm_system_id', 'spei')])
fs <- rownames(fs[rowSums(fs > 100)==3, ])

cl <- makeCluster(length(fs), outfile = '')
registerDoParallel(cl)

foreach(f=fs, .packages=c('CBPS', 'dplyr', 'broom', 'foreach', 'doParallel')) %dopar% {
  fs_sel <- sel %>% filter(farm_system_id==f)
  
  nfe.pscore.form <- (treat ~ I(log(population+1)) + I(log(market_dist + 1)) + wealth_norm + I(log(grid_gdp)) + 
                        hhsize + age + sex + birth_order + mother_years_ed + spei + I(log(mean_annual_precip)))
  covars <- model.matrix(~ -1 + I(log(population+1)) + I(log(market_dist + 1)) + wealth_norm + I(log(grid_gdp)) + 
                           hhsize + age + birth_order + mother_years_ed + spei + I(log(mean_annual_precip)),
                         data = fs_sel)
  
  # Fit CBPS and npCBPS
  #pscorefit.nfe.exact <- CBPS(nfe.pscore.form, data = sel, twostep = FALSE, method = "exact")
  pscorefit.nfe.np <- npCBPS(nfe.pscore.form, data = fs_sel, corprior=0.1/nrow(fs_sel))

  mod <- lm(haz_dhs ~ treat + treat*spei, data = fs_sel, weights=pscorefit.nfe.np$weights)
  
  coefs <- tidy(mod)
  
  coefs$farm_system_id <- f
  
  write.csv(coefs, paste0('coefs_', f, '.csv'), row.names=F)
  
  test <- expand.grid(list(treat=c(0, 1), spei=c('low', 'high', 'med')))
  
  test$outcome <- predict(mod, test)
  test$farm_system_id <- f
  
  write.csv(test, paste0('preds_', f, '.csv'), row.names=F)
}

system('./telegram.sh "Done!"')
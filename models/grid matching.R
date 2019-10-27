setwd('G://My Drive/DHS Processed')

#Based on 
#https://imai.fas.harvard.edu/research/files/CBGPS.pdf

library(tidyverse)
library(MatchIt)
library(Hmisc)
library(CBPS)
library(mgcv)
library(raster)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
aez <- read.csv('AEZ.csv')

##################
#Just Africa
#################
comb <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, aez)) %>%
  filter(latitude < 20 & longitude > -25 & longitude < 75) %>%
  na.omit

all <- comb %>%
  filter(spei < 1)

ymx=20.25
ymn=-30.75
xmx=50.5
xmn=-17.5

nrow = (ymx - ymn)/.5
ncol = (xmx - xmn)/.5

r <- raster(matrix(seq(1:(nrow*ncol)), nrow, ncol), ymx=ymx, ymn=ymn, xmx=xmx, xmn=xmn)

all$uiq <- extract(r, SpatialPoints(all[ , c('longitude', 'latitude')]))

sum <- all %>% group_by(uiq) %>%
  dplyr::summarize(nat_range = max(natural) - min(natural),
                   count=n(),
                   spei_range = max(spei24) - min(spei24)) %>%
  merge(data.frame(uiq=seq(1:(nrow*ncol))), all=T)

natrange <- raster(matrix(sum$nat_range, nrow, ncol), ymx=ymx, ymn=ymn, xmx=xmx, xmn=xmn)
count <- raster(matrix(sum$count, nrow, ncol), ymx=ymx, ymn=ymn, xmx=xmx, xmn=xmn)
speirange <- raster(matrix(sum$spei_range, nrow, ncol), ymx=ymx, ymn=ymn, xmx=xmx, xmn=xmn)

plot(speirange > 1 & natrange > 0.25 & count > 25)

all$age_t <- cut2(all$age, g = 4)
all$birth_order_t <- cut2(all$age, g = 4)
all$hhsize_t <- cut2(all$hhsize, g = 4)
all$mother_years_ed_t <- cut2(all$mother_years_ed, g = 4)
all$wealth_norm_t <- cut2(all$wealth_norm, g=5)

coefdat <- data.frame()
mdat <- data.frame()
for (u in unique(all$uiq)){
  sel <- all %>% 
    filter(uiq==u) %>%
    mutate(NatBin=natural > median(natural))
  
  print(which(unique(all$uiq) == u)/length(unique(all$uiq)))
  
  res <- tryCatch({
    mat <- matchit(NatBin ~ age_t + birth_order_t + sex + 
                     mother_years_ed + 
                     toilet + hhsize_t + wealth_norm_t,
                   method='exact', replace=TRUE, data=sel)
    match.data(mat)
  }, error=function(e){return(data.frame())})
  
  if (dim(res) > 10){
    mdat <- bind_rows(res, mdat)
    mod <- lm(haz_dhs ~ NatBin*spei24, data=res)
    
    coefdat <- bind_rows(coefdat, data.frame(uiq=u,
                                             NatBinTRUE=coef(mod)[2],
                                             spei24=coef(mod)[3],
                                             NatBinTRUEspei24=coef(mod)[4]))
  }
}



sum <- mdat %>% group_by(uiq) %>%
  dplyr::summarize(count=n()) %>%
  merge(data.frame(uiq=seq(1:(nrow*ncol))), all=T)


coefdat2 <- merge(coefdat, data.frame(uiq=seq(1:(nrow*ncol))), all=T)

NatBinTRUE <- raster(matrix(coefdat2$NatBinTRUE, nrow, ncol))
spei24 <- raster(matrix(coefdat2$spei24, nrow, ncol))
NatBinTRUEspei24 <- raster(matrix(coefdat2$NatBinTRUEspei24, nrow, ncol))




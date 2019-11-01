library(mgcv)
library(raster)
library(tidyverse)

load('G://My Drive/lc-malnutrition/GAMs/AEZ_noweights_GCV.Rdata')

nat <- raster('G://My Drive/DHS Spatial Covars/ESA Land Cover/Natural_Pct.tif')
aez <- raster('G://My Drive/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(extent(nat))
md <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2015/market_dist.tif') %>%
  crop(extent(nat))

md <- log(md + 1)
md <- round(md, 0)
nat <- round(nat, 1)

actual <- raster(ext=extent(aez), res=res(aez))
cfactual <- raster(ext=extent(aez), res=res(aez))

aez[aez==2] <- 1
aez[aez==3] <- 1

for (a_str in unique(mod$model$AEZ_new)){ #Get aez values
  
  #map aez factors to raster values
  if (a_str == "afr.arid.123"){
    a_num <- 1
  }
  if (a_str == "afr.forest.4"){
    a_num <- 4
  }
  if (a_str == "nafr.sav.5"){
    a_num <- 5
  }
  if (a_str == "seafr.sav.6"){
    a_num <- 6
  }
  if (a_str == "afr.high.7"){
    a_num <- 7
  }
  if (a_str == "nafr.subforest.8"){
    a_num <- 8
  }
  if (a_str == "safr.subforest.9"){
    a_num <- 9
  }
  
  df <- unique(data.frame(natural=nat[aez==a_num], market_dist=md[aez==a_num], 1))
  
  df$AEZ_new <- a_str
  df[ , as.character(unique(mod$model$AEZ_new))] <- 0
  df[ , a_str] <- 1
  df$age <- 1
  df$birth_order <- 1
  df$hhsize <- 10
  df$sex <- "Male"
  df$mother_years_ed <- 10
  df$toilet <- "Other"
  df$interview_year <- 2000
  df$calc_birthmonth <- 3
  df$head_age <- 40
  df$head_sex <- "Male"
  df$wealth_norm <- 0
  df$latitude <- 0
  df$longitude <- 0
  
  alt <- df
  alt$natural <- 0
  
  p_actual <- predict(mod, df, type='terms', se=T)
  p_cfactual <- predict(mod, alt, type='terms', se=T)
  
  print(a_str)
  
  pb <- txtProgressBar(min=1, max=nrow(df), style=3)
  for (i in 1:nrow(df)){
    actual[aez==a_num & nat==df$natural[i] & md==df$market_dist[i]] <- p_actual$fit[i , paste0('te(market_dist,natural):', a_str)]
    cfactual[aez==a_num & nat==df$natural[i] & md==df$market_dist[i]] <- p_cfactual$fit[i , paste0('te(market_dist,natural):', a_str)]
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

increase_without_nlc <- cfactual - actual

#increase_without_nlc[increase_without_nlc < 0] <- 0

plot(increase_without_nlc)

pop <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2020/population.tif') %>%
  crop(extent(nat))

pop_weighted <- increase_without_nlc*pop

pop_weighted[pop_weighted < 0] <- 0
pop_weighted[pop_weighted > 1000] <- 1000

plot(pop_weighted)

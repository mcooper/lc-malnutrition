library(mgcv)
library(raster)
library(tidyverse)

load('G://My Drive/lc-malnutrition/GAMs/AEZ_noweights_GCV.Rdata')

all <- read.csv('G://My Drive/DHS Processed/lc-malnutrition-weights.csv')

nat <- raster('G://My Drive/DHS Spatial Covars/ESA Land Cover/Natural_Pct.tif')
aez <- raster('G://My Drive/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(extent(nat))
md <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2015/market_dist.tif') %>%
  crop(extent(nat))

md <- log(md + 1)
all$market_dist2 <- log(all$market_dist + 1)

aez[aez==2] <- 1
aez[aez==3] <- 1

#The more precision here, the longer it takes!
md <- round(md, 0)
nat <- round(nat, 1)

actual <- raster(ext=extent(aez), res=res(aez))
cfactual <- raster(ext=extent(aez), res=res(aez))

actual_se <- raster(ext=extent(aez), res=res(aez))
cfactual_se <- raster(ext=extent(aez), res=res(aez))

feature_space <- raster(ext=extent(aez), res=res(aez))

in_feature_space <- function(x, y, all_x, all_y, dist){
  #' Determines whether or not a coordiante x, y is within a certain distance
  #' dist of a vector all_x, all_y
  
  #Get scaling parameters
  mn_x <- min(all_x)
  mx_x <- max(all_x - mn_x)
  
  mn_y <- min(all_y)
  mx_y <- max(all_y - mn_y)
  
  #Rescale vectors
  all_x <- all_x - mn_x
  all_x <- all_x/mx_x
  
  all_y <- all_y - mn_y
  all_y <- all_y/mx_y
  
  #Rescale points
  x <- x - mn_x
  x <- x/mx_x

  y <- y - mn_y
  y <- y/mx_y
  
  dists <- sqrt((x - all_x)^2 + (y - all_y)^2)
  
  return(any(dists < dist))    
  
}

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

    actual_se[aez==a_num & nat==df$natural[i] & md==df$market_dist[i]] <- p_actual$se.fit[i , paste0('te(market_dist,natural):', a_str)]
    cfactual_se[aez==a_num & nat==df$natural[i] & md==df$market_dist[i]] <- p_cfactual$se.fit[i , paste0('te(market_dist,natural):', a_str)]
    
    feature_space[aez==a_num & nat==df$natural[i] & md==df$market_dist[i]] <- in_feature_space(df$natural[i], 
                                                                                               df$market_dist[i], 
                                                                                               all$natural[all$AEZ_new==a_str],
                                                                                               all$market_dist[all$AEZ_new==a_str],
                                                                                               0.01)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

actual_signif <- actual
actual_signif[(abs(actual) - actual_se*2) < 0] <- NA
actual_signif[!feature_space] <- NA

cfactual_signif <- cfactual
cfactual_signif[(abs(cfactual) - cfactual_se*2) < 0] <- NA
cfactual_signif[!feature_space] <- NA

increase_without_nlc <- cfactual_signif - actual_signif




increase_without_nlc[increase_without_nlc >= 0] <- NA

plot(increase_without_nlc)

pop <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2020/population.tif') %>%
  crop(extent(nat))

pop_weighted <- increase_without_nlc*pop

pop_weighted[pop_weighted < 0] <- 0
pop_weighted[pop_weighted > 1000] <- 1000

plot(pop_weighted < -10000)

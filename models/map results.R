library(mgcv)
library(raster)
library(tidyverse)

load('G://My Drive/lc-malnutrition/GAMs/AEZ_Gam.Rdata')

nat <- raster('G://My Drive/DHS Spatial Covars/ESA Land Cover/Natural_Pct.tif')
aez <- raster('G://My Drive/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(extent(nat))
md <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2015/market_dist.tif') %>%
  crop(extent(nat))

md <- log(md + 1)
md <- round(md, 1)
nat <- round(nat, 2)
final <- raster(ext=extent(aez), res=res(aez))

for (a in unique(....)){ #Get aez values
  
  #map aez factors to raster values
  
  df <- unique(data.frame(natural=nat[aez==a], market_dist=md[aez==a], 1))
  
  df$AEZ_new <- a
  df[ , gsub(' ', '.', as.character(unique(mod$model$AEZ_new)))] <- 0
  df[ , gsub(' ', '.', a)] <- 1
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
  df$spei <- 1
  
  p <- predict(mod, df, type='terms', se=T)
  
  for (i in 1:nrow(df)){
    final[aez==a & nat==df$natural[i] & md==df$market_dist[i]] <- p[i]$spei24
  }
}

plot(final)
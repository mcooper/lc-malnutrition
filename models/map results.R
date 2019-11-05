library(mgcv)
library(raster)
library(tidyverse)
library(rnaturalearth)

load('G://My Drive/lc-malnutrition/GAMs/AEZ_weights_GCV_natOnly.Rdata')

all <- read.csv('G://My Drive/DHS Processed/lc-malnutrition-weights2.csv')

nat <- raster('G://My Drive/DHS Spatial Covars/ESA Land Cover/Natural_Pct.tif')
aez <- raster('G://My Drive/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(extent(raster(nrows=1100, ncol=1400, xmn=-18, xmx=52, ymn=-35, ymx=20)))

aez[aez==2] <- 1
aez[aez==3] <- 1

#The more precision here, the longer it takes!
nat <- round(nat, 1)

actual <- raster(ext=extent(aez), res=res(aez))
cfactual <- raster(ext=extent(aez), res=res(aez))

actual_se <- raster(ext=extent(aez), res=res(aez))
cfactual_se <- raster(ext=extent(aez), res=res(aez))


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
  
  df <- unique(data.frame(natural=nat[aez==a_num]))
  
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
    actual[aez==a_num & nat==df$natural[i]] <- p_actual$fit[i , paste0('s(natural):', a_str)]
    cfactual[aez==a_num & nat==df$natural[i]] <- p_cfactual$fit[i , paste0('s(natural):', a_str)]

    actual_se[aez==a_num & nat==df$natural[i]] <- p_actual$se.fit[i , paste0('s(natural):', a_str)]
    cfactual_se[aez==a_num & nat==df$natural[i]] <- p_cfactual$se.fit[i , paste0('s(natural):', a_str)]

    setTxtProgressBar(pb, i)
  }
  close(pb)
}

actual_signif <- actual

cfactual_signif <- cfactual

#Get estimated standard deviation of stunting
sdsum <- all %>%
  group_by(interview_year) %>%
  summarize(sd=sd(haz_dhs))

sd_pred <- predict(lm(sd ~ interview_year, data=sdsum), data.frame(interview_year=2020))

#Get inverse CDF
proj2020 <- raster('G://My Drive/lc-malnutrition/stunting/proj2020.tif')
proj2020_q <- calc(proj2020, function(x){qnorm(x)})

#To estimate mean HAZ scores across africa in 2020 
m <- -sd_pred*proj2020_q - 2

#Get HAZ scores under drought with current lc
m_drought <- m - actual_signif

#Get HAZ scores under drought without current lc (counterfactual)
m_drought_cf <- m - cfactual_signif
  
#Convert HAZ scores under drought back to P < q
p_drought <- calc(m_drought, function(x){pnorm(-2, x, sd_pred)})
p_drought_cf <- calc(m_drought_cf, function(x){pnorm(-2, x, sd_pred)})

#See increase in rate of stunting in counterfactual
increase <- p_drought_cf - p_drought

#Only look at increase in stunting in a all-ag scenario
increase[increase < 0] <- 0

#Get count of excess stunted children
u5pop <- raster('G://My Drive/lc-malnutrition/Age Structure/u5_pop.tif') %>%
  crop(extent(nat))

increased_burden <- u5pop*increase
increased_burden[is.na(increased_burden)] <- 0

#Aggregate by country
cty <- ne_countries()

cty@data$extract <- raster::extract(increased_burden, cty, fun=sum)

library(sf)

cty_sf <- st_as_sf(cty)

ggplot(cty_sf) + 
  geom_sf(aes(fill=extract))


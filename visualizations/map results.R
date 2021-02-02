library(mgcv)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(viridis)
library(RColorBrewer)
library(cowplot)
library(scales)


load('~/mortalityblob/lc_gams/AEZ_weights_GCV_natOnly.Rdata')

all <- read.csv('~/mortalityblob/dhs/lc-malnutrition-weights2.csv')

nat <- raster('~/gd/DHS Spatial Covars/ESA Land Cover/natural_raster.tif')
aez <- raster('~/gd/DHS Spatial Covars/AEZ/AEZ_DHS.tif') %>%
  crop(extent(raster(nrows=1100, ncol=1400, xmn=-18, xmx=52, ymn=-35, ymx=20)))

aez[aez==2] <- 1
aez[aez==3] <- 1

#The more precision here, the longer it takes!
nat <- round(nat, 2)

actual <- aez
actual[!is.na(actual)] <- 0

cfactual <- aez
cfactual[!is.na(cfactual)] <- 0

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

    setTxtProgressBar(pb, i)
  }
  close(pb)
}

#Only model counterfactual for subforest regions (where we saw a real effect)
cfactual[!aez %in% c(8, 9)] <- actual[!aez %in% c(8, 9)] 

#Get estimated standard deviation of stunting
# sdsum <- all %>%
#   group_by(interview_year) %>%
#   summarize(sd=sd(haz_dhs))

#sd_pred <- predict(lm(sd ~ interview_year, data=sdsum), data.frame(interview_year=2020))
sd_pred <- sd(all$haz_dhs)

#Get inverse CDF
proj2020 <- raster('~/gd/lc-malnutrition/stunting/proj2020.tif')
proj2020_q <- calc(proj2020, function(x){qnorm(x, 0, sd_pred)})

#To estimate mean HAZ scores across africa in 2020 
m <- -sd_pred*proj2020_q - 2

#Get HAZ scores under drought (SPEI=-2.5) with current lc
m_drought <- m - actual*2.5

#Get HAZ scores under drought (SPEI=-2.5) without current lc (counterfactual)
m_drought_cf <- m - cfactual*2.5
  
#Convert HAZ scores under drought back to P < q
p_drought <- calc(m_drought, function(x){pnorm(-2, x, sd_pred)})
p_drought_cf <- calc(m_drought_cf, function(x){pnorm(-2, x, sd_pred)})

#See increase in rate of stunting in counterfactual
increase <- p_drought_cf - p_drought

#Get count of excess stunted children
u5pop <- raster('~/gd/lc-malnutrition/Age Structure/u5_pop.tif') %>%
  crop(extent(nat))

increased_burden <- u5pop*increase
a <- raster::area(increased_burden)
increased_burden_perkm <- increased_burden/a

#Write resulting rasters
writeRaster(actual, '~/gd/lc-malnutrition/actual.tif', format='GTiff', overwrite=T)
writeRaster(increase, '~/gd/lc-malnutrition/increase.tif', format='GTiff', overwrite=T)
writeRaster(increased_burden, '~/gd/lc-malnutrition/increased_burden.tif', format='GTiff', overwrite=T)

#Get Per-Km
a <- raster::area(increased_burden)
sqkm <- increased_burden/a

sqkm[is.na(sqkm) & !is.na(aez) ] <- 0

sqkm_dat <- data.frame(rasterToPoints(sqkm)) %>%
  mutate(layer = log(layer + 1))
sqkm_dat$layer[sqkm_dat$layer == 0] <- NA


#Aggregate by country
cty <- ne_countries()
cty$iso_a3[cty$admin == 'Somaliland'] <- 'SOM'
increased_burden[!aez %in% c(8, 9)] <- NA
cty@data$increased_burden <- raster::extract(increased_burden, cty, fun=sum, na.rm=T)

u5tab <- read.csv('~/gd/DHS Processed/Under5Population.csv')

cty_sf <- st_as_sf(cty)
cty_sf <- merge(cty_sf, u5tab)

cty_sf <- st_crop(cty_sf, xmin=-17.525, xmax=51.975, ymin=-34.925, ymax=19.875)

(burden <- ggplot(sqkm_dat) +
  geom_raster(aes(x=x, y=y, fill=log(layer + 1))) + 
  geom_sf(data=cty_sf, fill=NA, color='grey20') + 
  scale_fill_viridis(direction = -1, option='A', na.value='grey80',
                     label=function(x){round(exp(x) - 1, 3)},
                     limits=c(0, 1.5),
                     breaks=c(0, log(2), log(3), log(4))) + 
  theme_void() + 
  theme(legend.position = c(0.2, 0.4)) + 
  labs(fill="Number of\nPotentally\nStunted\nChildren\nPer Sq. Km."))

options(scipen=1000)
cty_sf <- cty_sf %>%
  mutate(increased_burden=ifelse(increased_burden != 0, increased_burden, NA))

(cty_level <- ggplot(cty_sf) + 
  geom_sf(aes(fill=increased_burden)) + 
  geom_sf(data = cty_sf %>% filter(is.na(increased_burden)), show.legend='polygon') + 
  scale_fill_gradient(low="#b2e2e2", high="#003300", labels = function(x) format(x, big.mark=',')) + 
  theme_void() + 
  theme(legend.position = c(0.2, 0.4)) + 
  labs(fill="Number of\nPotentially\nStunted\nChildren\nPer Country"))

plot_grid(burden, cty_level, align='v', axis='tblr', labels='AUTO')

ggsave('~/lc-malnutrition-tex/AfricaEffect.png', height=4, width=10)


library(raster)
library(rgdal)
library(dplyr)

setwd('G://My Drive/DHS Spatial Covars/AEZ')

data <- read.csv('G://My Drive/DHS Processed/sp_export.csv') %>%
  dplyr::select(latitude, longitude, code) %>%
  unique

AEZ <- raster('G://My Drive/DHS Spatial Covars/AEZ/AEZ.tif')

sp <- SpatialPoints(data[ , c('longitude', 'latitude')])

data$AEZ <- extract(AEZ, sp)

for (i in which(is.na(data$AEZ))){
  sel <- data %>% 
    filter(substr(code, 1, 2)==substr(data$code[i], 1, 2)) %>%
    na.omit
  
  dists <- apply(sel[ , c('longitude', 'latitude')], 1, FUN = pointDistance, p2=data[i, c('longitude', 'latitude')], lonlat=TRUE)
  
  data$AEZ[i] <- sel$AEZ[which(dists==min(dists))]
  
  print(i)
}

class <- read.csv('AEZ_class.csv')

data <- merge(data, class, all.x=T, all.y=F)

se.asia <- data$latitude < 33 & data$longitude > 78 
eurasia <- substr(data$code, 1, 2) %in% c("MB", "AL", "AM", "TJ", "KY") 
me.na <- substr(data$code, 1, 2) %in% c("MA", "EG", "JO") 
lac.low <- data$longitude < -35 & !data$AEZ %in% c(322, 323, 324)  
lac.high <- data$longitude < -35 & data$AEZ %in% c(322, 323, 324) 
nafr.arid <- data$AEZ %in% c(211, 221, 311, 321) & substr(data$code, 1, 2) %in% c("SN", "ML", "NI", "NG", "BF", "CM", "TD") 
eafr.arid <- data$AEZ %in% c(211, 221, 311, 321) & data$longitude > 26 & data$latitude > -5 & data$longitude < 55 & data$latitude < 17
safr.arid <- data$AEZ %in% c(211, 221, 311, 321) & data$latitude < -5 & data$longitude > -25 & data$longitude < 55
afr.forest <- data$AEZ == 314 & data$latitude < 17 & data$longitude > -25 & data$longitude < 55
nafr.sav <- data$AEZ == 312 & data$latitude > 3.5 & data$longitude > -25 & data$longitude < 55
seafr.sav <- data$AEZ == 312 & data$latitude < 3.5 & data$longitude > -25 & data$longitude < 55
afr.high <- data$AEZ %in% c(322, 323, 324, 222, 223, 224) & data$latitude < 17 & data$longitude > -25 & data$longitude < 55
afr.subforest <- data$AEZ == 313 & data$latitude < 17 & data$longitude > -25 & data$longitude < 55

cl <- se.asia + eurasia + me.na + lac.low + lac.high + nafr.arid + eafr.arid + safr.arid + 
        afr.forest + nafr.sav + seafr.sav + afr.high + afr.subforest

data$AEZ_new[se.asia] <- "Southeast Asia"
data$AEZ_new[eurasia] <- "Eurasia"
data$AEZ_new[me.na] <- "Mideast NAfrica"
data$AEZ_new[lac.low] <- 'LAC Lowlands'
data$AEZ_new[lac.high] <- 'LAC Highlands'
data$AEZ_new[nafr.arid] <- 'Arid NAfrica'
data$AEZ_new[eafr.arid] <- 'Arid NAfrica'
data$AEZ_new[safr.arid] <- 'Arid SAfrica'
data$AEZ_new[afr.forest] <- 'Forest Africa'
data$AEZ_new[nafr.sav] <- 'Savanna NAfrica'
data$AEZ_new[seafr.sav] <- 'Savanna SEAfrica'
data$AEZ_new[afr.high] <- 'Highlands Africa'
data$AEZ_new[afr.subforest] <- 'SemiForest Africa'

write.csv(data, 'G://My Drive/DHS Processed/AEZ.csv', row.names=F)


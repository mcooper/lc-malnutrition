library(raster)
library(rgdal)
library(dplyr)
library(rnaturalearth)

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

new <- merge(data, class, all.x=T, all.y=F)

nafr.arid.1 <- new$AEZ %in% c(211, 221, 311, 321) & substr(new$code, 1, 2) %in% c("SN", "ML", "NI", "NG", "BF", "CM", "TD")
eafr.arid.2 <- new$AEZ %in% c(211, 221, 311, 321) & new$longitude > 26 & new$latitude > -5 & new$longitude < 55 & new$latitude < 17
safr.arid.3 <- new$AEZ %in% c(211, 221, 311, 321) & new$latitude < -5 & new$longitude > -25 & new$longitude < 55
afr.forest.4 <- new$AEZ == 314 & new$latitude < 17 & new$longitude > -25 & new$longitude < 55
nafr.sav.5 <- new$AEZ == 312 & new$latitude > 3.5 & new$longitude > -25 & new$longitude < 55
seafr.sav.6 <- new$AEZ == 312 & new$latitude < 3.5 & new$longitude > -25 & new$longitude < 55
afr.high.7 <- new$AEZ %in% c(322, 323, 324, 222, 223, 224) & new$latitude < 17 & new$longitude > -25 & new$longitude < 55
nafr.subforest.8 <- new$AEZ == 313 & new$latitude < 17 & new$latitude > 0 & new$longitude > -25 & new$longitude < 55
safr.subforest.9 <- new$AEZ == 313 & new$latitude < 0 & new$longitude > -25 & new$longitude < 55
se.asia.10 <- new$latitude < 33 & new$longitude > 78
eurasia.11 <- substr(new$code, 1, 2) %in% c("MB", "AL", "AM", "TJ", "KY")
me.na.12 <- substr(new$code, 1, 2) %in% c("MA", "EG", "JO")
lac.low.13 <- new$longitude < -35 & !new$AEZ %in% c(322, 323, 324)
lac.high.14 <- new$longitude < -35 & new$AEZ %in% c(322, 323, 324)


new$AEZ_new[nafr.arid.1] <- 'nafr.arid.1'
new$AEZ_new[eafr.arid.2] <- 'eafr.arid.2'
new$AEZ_new[safr.arid.3] <- 'safr.arid.3'
new$AEZ_new[afr.forest.4] <- 'afr.forest.4'
new$AEZ_new[nafr.sav.5] <- 'nafr.sav.5'
new$AEZ_new[seafr.sav.6] <- 'seafr.sav.6'
new$AEZ_new[afr.high.7] <- 'afr.high.7'
new$AEZ_new[nafr.subforest.8] <- 'nafr.subforest.8'
new$AEZ_new[safr.subforest.9] <- 'safr.subforest.9'
new$AEZ_new[se.asia.10] <- "se.asia.10"
new$AEZ_new[eurasia.11] <- "eurasia.11"
new$AEZ_new[me.na.12] <- "me.na.12"
new$AEZ_new[lac.low.13] <- 'lac.low.13'
new$AEZ_new[lac.high.14] <- 'lac.high.14'

#Get country ISO3N as raster
cty <- ne_countries()
cty@data$iso_n3[cty@data$sovereignt=='Somaliland'] <- cty@data$iso_n3[cty@data$sovereignt=='Somalia']
cty@data$iso_n3 <- as.numeric(cty@data$iso_n3)
ctyr <- rasterize(cty, AEZ, field='iso_n3')

#Get lakes to set null
lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical')
lakesr <- rasterize(lakes, AEZ, field='wdid_score')

#Get lat long rasters
lon_mat <- matrix(rep(seq(-180 + 0.05/2, 180 - 0.05/2, by=0.05), 2000), nrow = 2000, byrow = TRUE)
lat_mat <- matrix(rep(seq(50 - 0.05/2, -50 + 0.05/2, by=-0.05), 7200), nrow = 2000, byrow = FALSE)
lon <- raster(lon_mat, xmn=-180, xmx=180, ymn=-50, ymx=50)
lat <- raster(lat_mat, xmn=-180, xmx=180, ymn=-50, ymx=50)

#nafr.arid.1
AEZ[AEZ %in% c(211, 221, 311, 321) & ctyr %in% cty@data$iso_n3[cty@data$iso_a2 %in% c("SN", "ML", "NE", "NG", "BF", "CM", "TD", 'MR', 'SD')]] <- 1
#eafr.arid.2
AEZ[AEZ %in%  c(211, 221, 311, 321) & ctyr %in% cty@data$iso_n3[cty@data$iso_a2 %in% c("ER", "ET", "DJ", "SO", "KE")]] <- 2
#safr.arid.3
AEZ[AEZ %in% c(211, 221, 311, 321) & lat < -5 & lon > -25 & lon < 55] <- 3
#afr.forest.4
AEZ[AEZ == 314 & lat < 17 & lon > -25 & lon < 55] <- 4
#nafr.sav.5
AEZ[AEZ == 312 & lat > 3.5 & lon > -25 & lon < 55] <- 5
#seafr.sav.6
AEZ[AEZ == 312 & lat < 3.5 & lon > -25 & lon < 55] <- 6
#afr.high.7
AEZ[AEZ %in% c(322, 323, 324, 222, 223, 224) & lat < 17 & lon > -25 & lon < 55] <- 7
#nafr.subforest.8
AEZ[AEZ == 313 & lat < 17 & lat > 0 & lon > -25 & lon < 55] <- 8
#safr.subforest.9
AEZ[AEZ == 313 & lat < 0 & lon > -25 & lon < 55] <- 9
#se.asia.10
AEZ[lat < 33 & lon > 78] <- 10
#eurasia.11
AEZ[ctyr %in% cty@data$iso_n3[cty@data$iso_a2 %in% c("MB", "AL", "AM", "TJ", "KY")]] <- 11
#me.na.12
AEZ[ctyr %in% cty@data$iso_n3[cty@data$iso_a2 %in% c("MA", "EG", "JO")]] <- 12
#lac.low.13
AEZ[!AEZ %in% c(322, 323, 324) & lon < -35] <- 13
#lac.high.14
AEZ[AEZ %in% c(322, 323, 324) & lon < -35] <- 14

AEZ[AEZ > 14] <- 0

AEZ[lakesr > 3] <- 0

writeRaster(AEZ, 'G://My Drive/DHS Spatial Covars/AEZ/AEZ_DHS.tif', format='GTiff', overwrite=T)
write.csv(new, 'G://My Drive/DHS Processed/AEZ.csv', row.names=F)

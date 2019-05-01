library(tidyverse)
library(raster)
library(rgdal)

#Calculate AEZ Using methodology here: https://harvestchoice.org/maps/agro-ecological-zones-sub-saharan-africa

#Precip already calculate from CHIRPS
map <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2020/mean_annual_precip.tif')

#Elevation
elev <- raster('G://My Drive/DHS Spatial Covars/Final Rasters/2020/elevation.tif')

#Get mean and max temp by month
setwd('G://My Drive/temperature')

jan_min <- mean(stack(list.files(pattern='tmin....01.tif')))
jan_max <- mean(stack(list.files(pattern='tmax....01.tif')))

feb_min <- mean(stack(list.files(pattern='tmin....02.tif')))
feb_max <- mean(stack(list.files(pattern='tmax....02.tif')))

mar_min <- mean(stack(list.files(pattern='tmin....03.tif')))
mar_max <- mean(stack(list.files(pattern='tmax....03.tif')))

apr_min <- mean(stack(list.files(pattern='tmin....04.tif')))
apr_max <- mean(stack(list.files(pattern='tmax....04.tif')))

may_min <- mean(stack(list.files(pattern='tmin....05.tif')))
may_max <- mean(stack(list.files(pattern='tmax....05.tif')))

jun_min <- mean(stack(list.files(pattern='tmin....06.tif')))
jun_max <- mean(stack(list.files(pattern='tmax....06.tif')))

jul_min <- mean(stack(list.files(pattern='tmin....07.tif')))
jul_max <- mean(stack(list.files(pattern='tmax....07.tif')))

aug_min <- mean(stack(list.files(pattern='tmin....08.tif')))
aug_max <- mean(stack(list.files(pattern='tmax....08.tif')))

sep_min <- mean(stack(list.files(pattern='tmin....09.tif')))
sep_max <- mean(stack(list.files(pattern='tmax....09.tif')))

oct_min <- mean(stack(list.files(pattern='tmin....10.tif')))
oct_max <- mean(stack(list.files(pattern='tmax....10.tif')))

nov_min <- mean(stack(list.files(pattern='tmin....11.tif')))
nov_max <- mean(stack(list.files(pattern='tmax....11.tif')))

dec_min <- mean(stack(list.files(pattern='tmin....12.tif')))
dec_max <- mean(stack(list.files(pattern='tmax....12.tif')))

#Resample and adjust to Sea level
jan_sea <- resample((jan_min + jan_max)/2 - 273.15, map) + (0.55*elev/100)
feb_sea <- resample((feb_min + feb_max)/2 - 273.15, map) + (0.55*elev/100)
mar_sea <- resample((mar_min + mar_max)/2 - 273.15, map) + (0.55*elev/100)
apr_sea <- resample((apr_min + apr_max)/2 - 273.15, map) + (0.55*elev/100)
may_sea <- resample((may_min + may_max)/2 - 273.15, map) + (0.55*elev/100)
jun_sea <- resample((jun_min + jun_max)/2 - 273.15, map) + (0.55*elev/100)
jul_sea <- resample((jul_min + jul_max)/2 - 273.15, map) + (0.55*elev/100)
aug_sea <- resample((aug_min + aug_max)/2 - 273.15, map) + (0.55*elev/100)
sep_sea <- resample((sep_min + sep_max)/2 - 273.15, map) + (0.55*elev/100)
oct_sea <- resample((oct_min + oct_max)/2 - 273.15, map) + (0.55*elev/100)
nov_sea <- resample((nov_min + nov_max)/2 - 273.15, map) + (0.55*elev/100)
dec_sea <- resample((dec_min + dec_max)/2 - 273.15, map) + (0.55*elev/100)

#Length of growing period
#USE data for the year 2000 from FAO-IIASA GAEZ data portal
setwd('G://My Drive/DHS Spatial Covars/AEZ/')

lgp <- resample(raster('res01_hist_lgd_2000.tif'), map)

##Run calculation

#Climate
g18 <- (jan_sea > 18) + (feb_sea > 18) + (mar_sea > 18) + (apr_sea > 18) + (may_sea > 18) + (jun_sea > 18) + (jul_sea > 18) + (aug_sea > 18) + (sep_sea > 18) + (oct_sea > 18) + (nov_sea > 18) + (dec_sea > 18)
l5 <- (jan_sea < 5) + (feb_sea < 5) + (mar_sea < 5) + (apr_sea < 5) + (may_sea < 5) + (jun_sea < 5) + (jul_sea < 5) + (aug_sea < 5) + (sep_sea < 5) + (oct_sea < 5) + (nov_sea < 5) + (dec_sea < 5)

tropical <- g18 == 12
subtropical <- g18 < 12 & l5==0
temperate <- l5 > 0 & l5 < 12
boreal <- l5 == 12

climate <- temperate*100 + subtropical*200 + tropical*300 + boreal*400

#moisture zones
arid <- lgp < 70
semiarid <- lgp >= 70 & lgp < 180
subhumid <- lgp >= 180 & lgp < 270
humid <- lgp >= 270

humidity <- arid*1 + semiarid*2 + subhumid*3 + humid*4

#Temperature/Elevation
lowwarm <- (tropical*(elev < 1200)) | (subtropical*(elev < 800))
highcool <- !lowwarm

tempelev <- lowwarm*10 + highcool*20

#Combine all
AEZ <- climate + humidity + tempelev

writeRaster(AEZ, 'G://My Drive/DHS Spatial Covars/AEZ/AEZ.tif', format='GTiff', overwrite=T)


library(tidyverse)
library(raster)
library(rgdal)


#Calculate AEZ Using methodology here: https://harvestchoice.org/maps/agro-ecological-zones-sub-saharan-africa

#Precip already calculate from CHIRPS
map <- raster('Final Rasters/2020/mean_annual_precip.tif')

#Elevation
elev <- raster('Final Rasters/2020/elevation.tif')

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
#Use Monthly Data from CHIPS for Precip
#Use ET0 From https://figshare.com/articles/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448/3
#Which is in turn derived from WorldClim
setwd('G://My Drive/CHIRPS/Monthly')

jan_pcp <- mean(stack(list.files(pattern='chirps-v2.0......01.tif')))
feb_pcp <- mean(stack(list.files(pattern='chirps-v2.0......02.tif')))
mar_pcp <- mean(stack(list.files(pattern='chirps-v2.0......03.tif')))
apr_pcp <- mean(stack(list.files(pattern='chirps-v2.0......04.tif')))
may_pcp <- mean(stack(list.files(pattern='chirps-v2.0......05.tif')))
jun_pcp <- mean(stack(list.files(pattern='chirps-v2.0......06.tif')))
jul_pcp <- mean(stack(list.files(pattern='chirps-v2.0......07.tif')))
aug_pcp <- mean(stack(list.files(pattern='chirps-v2.0......08.tif')))
sep_pcp <- mean(stack(list.files(pattern='chirps-v2.0......09.tif')))
oct_pcp <- mean(stack(list.files(pattern='chirps-v2.0......10.tif')))
nov_pcp <- mean(stack(list.files(pattern='chirps-v2.0......11.tif')))
dec_pcp <- mean(stack(list.files(pattern='chirps-v2.0......12.tif')))

jan_pcp[jan_pcp < 0] <- NA
feb_pcp[feb_pcp < 0] <- NA
mar_pcp[mar_pcp < 0] <- NA
apr_pcp[apr_pcp < 0] <- NA
may_pcp[may_pcp < 0] <- NA
jun_pcp[jun_pcp < 0] <- NA
jul_pcp[jul_pcp < 0] <- NA
aug_pcp[aug_pcp < 0] <- NA
sep_pcp[sep_pcp < 0] <- NA
oct_pcp[oct_pcp < 0] <- NA
nov_pcp[nov_pcp < 0] <- NA
dec_pcp[dec_pcp < 0] <- NA

setwd('G://My Drive/DHS Spatial Covars/AEZ/et0_month')

jan_pet <- resample(raster('et0_01.tif'), dec_pcp)
feb_pet <- resample(raster('et0_02.tif'), dec_pcp)
mar_pet <- resample(raster('et0_03.tif'), dec_pcp)
apr_pet <- resample(raster('et0_04.tif'), dec_pcp)
may_pet <- resample(raster('et0_05.tif'), dec_pcp)
jun_pet <- resample(raster('et0_06.tif'), dec_pcp)
jul_pet <- resample(raster('et0_07.tif'), dec_pcp)
aug_pet <- resample(raster('et0_08.tif'), dec_pcp)
sep_pet <- resample(raster('et0_09.tif'), dec_pcp)
oct_pet <- resample(raster('et0_10.tif'), dec_pcp)
nov_pet <- resample(raster('et0_11.tif'), dec_pcp)
dec_pet <- resample(raster('et0_12.tif'), dec_pcp)

#Get if growing month
jan_grow <- (jan_pet*0.5 < jan_pcp) & jan_sea > 5
feb_grow <- (feb_pet*0.5 < feb_pcp) & feb_sea > 5
mar_grow <- (mar_pet*0.5 < mar_pcp) & mar_sea > 5
apr_grow <- (apr_pet*0.5 < apr_pcp) & apr_sea > 5
may_grow <- (may_pet*0.5 < may_pcp) & may_sea > 5
jun_grow <- (jun_pet*0.5 < jun_pcp) & jun_sea > 5
jul_grow <- (jul_pet*0.5 < jul_pcp) & jul_sea > 5
aug_grow <- (aug_pet*0.5 < aug_pcp) & aug_sea > 5
sep_grow <- (sep_pet*0.5 < sep_pcp) & sep_sea > 5
oct_grow <- (oct_pet*0.5 < oct_pcp) & oct_sea > 5
nov_grow <- (nov_pet*0.5 < nov_pcp) & nov_sea > 5
dec_grow <- (dec_pet*0.5 < dec_pcp) & dec_sea > 5

lgp <- jan_grow + feb_grow + mar_grow + apr_grow + may_grow + jun_grow + jul_grow + aug_grow + sep_grow + oct_grow + nov_grow + dec_grow

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
arid <- lgp <= 2
semiarid <- lgp > 2 & lgp <= 6
subhumid <- lgp > 6 & lgp <= 9
humid <- lgp > 9

humidity <- arid*1 + semiarid*2 + subhumid*3 + humid*4

#Temperature/Elevation
lowwarm <- (tropical*(elev < 1200)) | (subtropical*(elev < 800))
highcool <- !lowwarm

tempelev <- lowwarm*10 + highcool*20

#Combine all
AEZ <- climate + humidity + tempelev

writeRaster(AEZ, 'G://My Drive/DHS Spatial Covars/AEZ/AEZ.tif', format='GTiff', overwrite=T)


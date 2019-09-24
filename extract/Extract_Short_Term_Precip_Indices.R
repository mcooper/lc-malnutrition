library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('/mnt/climate')

dat <- read.csv('~/mortalityblob/dhs/sp_export.csv') %>%
  select(-calc_birthyear, -calc_birthmonth, -thousandday_month, -thousandday_year) %>%
  unique

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('~/mortalityblob/chirps-v2.0.1981.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp@data$tmpcode <- extract(codes, sp)

#Deal with points near a coast, coming up NA
spna <- sp[is.na(sp@data$tmpcode) , ]
spna$tmpcode <- NULL
badcoords <- unique(spna@coords)
tmpcode <- apply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])
badcoords <- cbind.data.frame(badcoords, tmpcode)
spna <- merge(spna@data, badcoords)
sp <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])

rll <- rasterToPoints(codes) %>% data.frame
rll <- rll[rll$layer %in% sp$tmpcode, ]

#Read in precip data
precip_in_folder <- '.'
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(precip_in_folder, pattern='^chirps.*tif$')
gdalbuildvrt(precip_files, precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmax data
tmax_in_folder <- '.'
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files(tmax_in_folder, pattern='^tmax.*tif$')
gdalbuildvrt(tmax_files, tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmin data
tmin_in_folder <- '.'
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files(tmin_in_folder, pattern='^tmin.*tif$')
gdalbuildvrt(tmin_files, tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

extract_neighbors <- function(vrt, x, y){
  
  m <- gdallocationinfo(vrt, x, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  u <- gdallocationinfo(vrt, x, y + 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  b <- gdallocationinfo(vrt, x, y - 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  l <- gdallocationinfo(vrt, x - 0.05, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  r <- gdallocationinfo(vrt, x + 0.05, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  ul <- gdallocationinfo(vrt, x - 0.05, y + 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  ur <- gdallocationinfo(vrt, x + 0.05, y + 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  bl <- gdallocationinfo(vrt, x - 0.05, y - 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  br <- gdallocationinfo(vrt, x + 0.05, y - 0.05, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  dat <- cbind(m, u, b, l, r, ul, ur, bl, br)
  
  dat[dat == -9999] <- NA
  
  return(rowMeans(dat, na.rm=T))
  
}

cl <- makeCluster(20, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo')) %dopar% {
  
  precip <- extract_neighbors(precip_vrt_file, rll$x[n], rll$y[n])[1:432]
  
  tmax <- extract_neighbors(tmax_vrt_file, rll$x[n], rll$y[n])
  
  tmin <- extract_neighbors(tmin_vrt_file, rll$x[n], rll$y[n])
  
  PET <- hargreaves(tmin-273.15, tmax-273.15, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  interview <- data.frame(tmpcode=rll$layer[n],
                          interview_month=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          interview_year=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          spei1=as.numeric(spei(s, 1, na.rm=TRUE)$fitted),
                          spei2=as.numeric(spei(s, 2, na.rm=TRUE)$fitted),
                          spei3=as.numeric(spei(s, 3, na.rm=TRUE)$fitted),
                          spei6=as.numeric(spei(s, 6, na.rm=TRUE)$fitted))
  
  sel <- sp[sp$tmpcode == rll$layer[n], ]
  sel <- Reduce(function(x, y){merge(x,y,all.x=T,all.y=F)}, list(sel, interview))
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  write.csv(sel, paste0('~/PrecipIndices/', n), row.names=F)
}

setwd('~/PrecipIndices/')

precip <- list.files()%>%
	lapply(read.csv) %>%
	bind_rows %>%
	select(-tmpcode)

write.csv(precip, '~/mortalityblob/dhs/PrecipIndicesShortTerm.csv', row.names=F)

system('~/telegram.sh "Done with Short Term SPEI"')





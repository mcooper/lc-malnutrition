setwd('/home/mattcoop/mortalityblob/dhs/')

library(tidyverse)
library(raster)

lc <- read.csv('landcover_grid.csv')

urban <- 'cci_190'
natural <- paste0('cci_', c('40', '50', '60', '61', '62', '70', '71', '80', '90', '100', '110', '120', '121', '122',
                            '130', '140', '150', '152', '153', '160', '170', '180', '210'))
water <- 'cci_210'

getPercentCover <- function(selcols, allcolmatch, df){
  if(length(selcols) > 1){
    selcolsum <- rowSums(df[ , selcols[selcols %in% names(df)]], na.rm=T)
  } else{
    selcolsum <- df[ , selcols]
  }
  allcolsum <- rowSums(df[ , grepl(allcolmatch, names(df))], na.rm=T)
  return(selcolsum/allcolsum)
}

lc$water <- getPercentCover(water, 'cci_', lc)
lc$urban <- getPercentCover(urban, 'cci_', lc)
lc$natural <- getPercentCover(natural, 'cci_', lc)

lc <- lc %>%
  filter(water < 0.05 & urban < 0.01) %>%
  dplyr::select(x, y, natural)

ref <- raster(nrows=1100, ncol=1400, xmn=-18, xmx=52, ymn=-35, ymx=20)

lc_sp <- SpatialPointsDataFrame(lc[ , c('x', 'y')], data=lc[ , 'natural', drop=FALSE])

natrast <- rasterize(lc_sp, ref, field='natural')

writeRaster(natrast, '/home/mattcoop/mortalityblob/dhs/natural_raster.tif', format='GTiff')

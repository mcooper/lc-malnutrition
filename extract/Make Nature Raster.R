setwd('~')

library(raster)

r <- crop(raster('ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif', band=24), 
          extent(c(-18, 52, -35, 20)))

rcl <- reclassify(r, matrix(c(0, 189, 1, 190, 209, 0, 210, 219, 1, 200, 255, 0), ncol=3, byrow = T))

res <- aggregate(rcl, fact=18, fun=mean)

writeRaster(res, 'Natural_Pct.tif', format='GTiff')

system('~/telegram.sh "Raster stuff done"')



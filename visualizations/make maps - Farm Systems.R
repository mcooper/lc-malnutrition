library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)
library(Hmisc)

#########################
#Read Data
########################

sp <- readOGR('G:/My Drive/DHS Spatial Covars/Farm Systems',
              'all_farming_systems', p4s = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

spt <- spTransform(sp, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

setwd('G://My Drive/DHS Processed')
hha <- read.csv('HH_data_A_norm.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')
lc <- read.csv('landcover_processed.csv')
fs <- read.csv('FarmingSystems.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov, lc, fs)) %>%
  na.omit

all$spei <- cut2(all$spei24, g = 3) %>%
  as.numeric %>%
  factor(labels=c('low', 'med', 'high')) %>%
  relevel(ref='med')

seldry <- all %>% 
  filter(market_dist > 2.5 & )

sel$treat <- sel$natural

fs <- table(sel[ , c('farm_system_id', 'spei')])
fs <- rownames(fs[rowSums(fs > 100)==2, ])

spt$include <- row.names(spt) %in% fs
# 
# spdat <- SpatialPointsDataFrame(coords=data[ , c('longitude', 'latitude')],
#                                 data=data, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
# 
# spdat_t <- spTransform(spdat, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
# 
# spdat_t@data[ , c('longitude', 'latitude')] <- spdat_t@coords


####################
#Make Images
####################

setwd('G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/figures')

##################
#DHS Points
###################

plt2 <- spplot(spt, "include", 
               col.regions = c("#780000", "#FFFF00"), 
               cex = 0.1)

# plt2$legend$bottom$args$key$text[[1]] <- c("", "", "")
# plt2$legend$bottom$args$key$points$cex <- c(0,0,0)

plt2

pdf("DHSPoints.pdf", width=8, height=6)
plot(plt2)
dev.off()

system("pdfcrop DHSPoints.pdf DHSPoints.pdf")

setwd('G://My Drive/DHS Processed')

library(dplyr)

lc <- read.csv('landcover.csv')

ag <- paste0('cci_', c('10', '11', '12', '20', '30', '200', '201', '202', '220'))
urban <- 'cci_190'
natural <- paste0('cci_', c('40', '50', '60', '61', '62', '70', '71', '80', '90', '100', '110', '120', '121', '122',
                            '130', '140', '150', '152', '153', '160', '170', '180', '210'))
nat_water <- 'cci_210'
nat_grass <- paste0('cci_', c('110', '120', '121', '122', '130', '140', '150', '152', '153', '180'))
nat_trees <- paste0('cci_', c('40', '50', '60', '61', '62', '70', '71', '80', '90', '100', '160', '170'))

getPercentCover <- function(selcols, allcolmatch, df){
  if(length(selcols) > 1){
    selcolsum <- rowSums(df[ , selcols[selcols %in% names(df)]], na.rm=T)
  } else{
    selcolsum <- df[ , selcols]
  }
  allcolsum <- rowSums(df[ , grepl(allcolmatch, names(df))], na.rm=T)
  return(selcolsum/allcolsum)
}

lc$ag <- getPercentCover(ag, 'cci_', lc)
lc$urban <- getPercentCover(urban, 'cci_', lc)
lc$natural <- getPercentCover(natural, 'cci_', lc)
lc$nat_water <- getPercentCover(nat_water, 'cci_', lc)
lc$nat_grass <- getPercentCover(nat_grass, 'cci_', lc)
lc$nat_trees <- getPercentCover(nat_trees, 'cci_', lc)

lc <- lc %>%
  select(ag, urban, natural, interview_year, code, nat_water, nat_grass, nat_trees)

write.csv(lc, 'landcover_processed.csv', row.names=F)

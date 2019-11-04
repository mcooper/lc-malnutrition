library(raster)

#Project rate of stunting in 2020
#Based on Methods -> Projections section
#In Ozgood-Zimmerman 2018
#https://www.nature.com/articles/nature25760

setwd('G://My Drive/lc-malnutrition/stunting/')

s <- stack(list.files(pattern='PREVALENCE'))

#Log transform prevalence estimates
sl <- log(s)

####################################
#Get Average AROC (Annual rate of change)
###################################

#Create an emty raster
aroc <- sl[[1]]
aroc[!is.na(aroc)] <- 0

#For each year after 2000
for (yr in 1:15){
  #Get the rate of change between that year and the previous 
  f1 <- paste0("IHME_AFRICA_CGF_2000_2015_STUNTING_MEAN_20", substr(100 + yr, 2, 3), "_PREVALENCE_Y2018M02D28")
  f0 <- paste0("IHME_AFRICA_CGF_2000_2015_STUNTING_MEAN_20", substr(100 + yr - 1, 2, 3), "_PREVALENCE_Y2018M02D28")
  aroc_yr <- sl[[f1]] - sl[[f0]]
  
  #Get the weight
  wt <- yr/sum(1:15)
  
  #Add that weighted ROC to the running total
  aroc <- aroc + wt*aroc_yr
}

#Get 2015 estimate and add 5 years to get 2020
proj2020 <- exp(sl[[16]] + aroc*5)

#Resample to fit our data
res <- raster(nrows=1100, ncol=1400, xmn=-18, xmx=52, ymn=-35, ymx=20)

proj2020res <- resample(proj2020, res, filename='G://My Drive/lc-malnutrition/stunting/proj2020.tif', format='GTiff')

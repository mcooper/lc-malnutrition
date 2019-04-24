setwd('G://My Drive/DHS Data/')

library(haven)
library(tidyverse)

##############################
#Scope Available Datasets
#################################

kr <- list.files(pattern='^..(KR|kr).....(DTA|dta)$')
pr <- list.files(pattern='^..(PR|pr).....(DTA|dta)$')
wi <- list.files(pattern='^..(WI|wi).....(DTA|dta)$')

makeFileNameDf <- function(f){
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                       ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                              ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                     ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4))))
  data.frame(num, cc, subversion, file=f)
}

kr_df <- lapply(X = kr, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(kr=file)
pr_df <- lapply(X = pr, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(pr=file)
wi_df <- lapply(X = wi, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(wi=file)

scope <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=T)}, list(kr_df, pr_df, wi_df))
#It seems that there are some with only KR but every file with HR also has KR

#Use PR.  If it is missing use KR. Also grab WI if it is there
vardat <- read.csv('C://Git/lc-malnutrition/normalize_wealth/WealthVars.csv',
                   stringsAsFactors=F)

tmp <- tempdir()
setwd(tmp)
for (i in 1:nrow(scope)){
  print(i/nrow(scope))
  
  if (!is.na(scope$pr[i])){
    #Get values from HR files
    dat <- read_dta(scope$pr[i])
    
    #If wealth data is missing and there is no wi file, next
    if (!('hv271' %in% names(dat)) & is.na(scope$wi[i])){
      next
    }
    
    #Drop unnecessary columns
    dat <- dat[ , vardat$pr[vardat$pr %in% names(dat)]]
    
    #Rename columns
    for (n in 1:nrow(vardat)){
      names(dat)[names(dat)==vardat$pr[n]] <- vardat$variable[n]
    }
    
  } else{
    #Get values from KR file in HR is missing
    dat <- read_dta(scope$kr[i])
    
    #If wealth data is missing and there is no wi file, next
    if (!('v191' %in% names(dat)) & is.na(scope$wi[i])){
      next
    }
    
    #Drop unnecessary columns
    dat <- dat[ , vardat$kr[vardat$kr %in% names(dat)]]
    
    #Rename columns
    for (n in 1:nrow(vardat)){
      names(dat)[names(dat)==vardat$kr[n]] <- vardat$variable[n]
    }
    
  }
  
  if (!is.na(scope$wi[i])){
    #Get wealth data if it is in separate file
    wi <- read_dta(scope$wi[i])
    names(wi) <- c('hhid', 'wealth_factor', 'wealth_index')
    
    wi$wealth_factor <- as.character(wi$wealth_factor)
    wi <- wi[ , c('hhid', 'wealth_factor')]
    
    if (sum(!dat$hhid %in% wi$hhid) > 0){
      #If hhid doesnt match, try it with just hhno and clusterid
      padws <- function(vect, n=0){
        len <- max(nchar(vect))
        padded <- paste0("     ", vect)
        substr(padded, nchar(padded) - (len - 1), nchar(padded))
      }
      
      dat$hhid <- paste0(padws(dat$clusterid), padws(dat$householdno))
      idlen <- nchar(dat$hhid)[1]
      dat$hhid <- paste0(paste0(rep(" ", 12-idlen), collapse=''), dat$hhid)
      
    }
    
    if (sum(!dat$hhid %in% wi$hhid) > 0){
      #Check again for matching, if not, cat error
      cat('Mismatches in wealth data for ', scope$cc[i], scope$num[i], scope$subversion[i], '\n') 
    }
    
    dat <- merge(dat, wi, all.x=T, all.y=F)
  }
  
  #If wealth factor still missing or all NA, next
  if (!('wealth_factor' %in% names(dat))){
    next
  }
  if (sum(is.na(dat$wealth_factor)) == nrow(dat)){
    next
  }
  
  #Save character version of labelled vars in new column, and convert original to integer
  for (n in names(dat)){
    if (is.labelled(dat[ , n])){
      dat[ , paste0(n, '_chr')] <- as.character(as_factor(dat[ , n]))
      dat[ , n] <- as.character(as.integer(dat[ , n]))
    }
  }
  
  survey_code <- paste0(scope$cc[i], '-', scope$num[i], '-', scope$subversion[i])
  dat$survey_code <- survey_code
  dat$code <- paste0(dat$survey_code, '-', dat$clusterid)
  
  #make numeric vars char, so there are no bind_rows isues
  dat <- dat %>%
    mutate_if(is.numeric, as.character)
  
  #Write and then read with a bind_rows to save on memory
  write.csv(dat, paste0(survey_code, '.csv'), row.names=F)
  
}

alldata <- Reduce(bind_rows, 
                  lapply(list.files(pattern='.csv$'), 
                         function(x){read.csv(x, colClasses='character')}))

#write.csv(alldata, 'G://My Drive/Dissertation/wealthvars_raw.csv', row.names=F)
alldata <- read.csv('G://My Drive/Dissertation/wealthvars_raw.csv')

###############################
#Calculate Cutpoints
###############################

#High end anchoring cutpoints
alldata$telephone <- alldata$telephone == 1
alldata$television <- alldata$television == 1
alldata$car_truck <- alldata$car_truck == 1
alldata$refrigerator <- alldata$refrigerator == 1

#Low end anchoring cutpoints
#housing
#table(alldata[ , c('floor_materials_chr', 'floor_materials')]) %>% as.data.frame.matrix %>% View
alldata$inadequate_floor = alldata$floor_materials < 15
alldata$inadequate_walls = alldata$wall_materials < 30 & alldata$wall_materials != 3

alldata$inadequate_housing <- alldata$inadequate_floor | alldata$inadequate_walls

#Sanitation
alldata$inadequate_toilet = alldata$toilet_type >= 30

alldata$inadequate_water_urban = alldata$drinking_water != 1 & alldata$drinking_water != 71 & alldata$drinking_water != 61
alldata$inadequate_water_rural = alldata$drinking_water >= 30 & alldata$drinking_water < 60

alldata$urban <- alldata$urban_rural == 1

alldata$inadequate_sanitation <- alldata$inadequate_toilet | (alldata$inadequate_water_urban & alldata$urban) | (alldata$inadequate_water_urban & !alldata$urban)

#Crowding
alldata$crowding <- (as.numeric(alldata$hhsize)/as.numeric(alldata$sleeping_rooms)) > 3

#School
alldata$unfinished_primary = alldata$primary_school < 2

getHHheadPrimary <- function(unfinished_primary, individual_number, household_head){
  unfinished <- unfinished_primary[individual_number==unique(household_head)]
  if (length(unfinished) > 1){
    return(NA)
  } else{
    return(unfinished)
  }
}

hh <- alldata %>% 
  select(hhid, householdno, clusterid, survey_code, code, television, refrigerator, car_truck, wealth_factor,
         telephone, inadequate_housing, inadequate_sanitation, crowding) %>%
  unique

head <- alldata %>% unique %>%
  group_by(hhid, householdno, code) %>%
  summarize(head_noprimary=getHHheadPrimary(unfinished_primary, individual_number, household_head)) %>%
  data.frame

hh <- merge(hh, head)

#Check that each household was unique for all the cutpoints
#nrow(hh) == nrow(unique(alldata[ , c('hhid', 'householdno', 'clusterid', 'survey_code', 'code')]))

#Scale wealth vars from 0-1
# hh <- hh %>%
#   group_by(survey_code) %>%
#   mutate(wealth_factor=wealth_factor - min(wealth_factor, na.rm=T),
#          wealth_factor=wealth_factor/max(wealth_factor, na.rm=T)) %>%
#   data.frame


#write.csv(hh, 'G://My Drive/Dissertation/wealthvars_hh.csv', row.names=F)
hh <- read.csv('G://My Drive/Dissertation/wealthvars_hh.csv', stringsAsFactors=F)

#Calculate Cutpoints
sdf <- hh %>% 
  select(survey_code) %>% 
  mutate(survey_code=as.character(survey_code)) %>%
  unique

baseline <- hh %>% filter(survey_code=='NG-5-1')

others <- sdf %>% filter(survey_code!='NG-5-1')

bad <- NULL

for (i in others$survey_code){
  sel <- hh %>% filter(survey_code==i)
  
  #Drop columns that are greater than half NA
  for (col in c("inadequate_housing", "inadequate_sanitation", "crowding", "head_noprimary",
                "television", "refrigerator", "car_truck", "wealth_factor", "telephone")){
    if (sum(is.na(sel[ , col])) > nrow(sel)/2){
      sel[ , col] <- NULL
    }
  }
  
  #Skip if there arent at least 2 points
  if (sum(c("inadequate_housing", "inadequate_sanitation", "crowding", "head_noprimary",
             "television", "refrigerator", "car_truck", "telephone") %in%
           names(sel)) < 2){
    bad <- c(bad, i)
    next
  }
  
  bsl <- baseline
  
  needs <- c("inadequate_housing", "inadequate_sanitation", "crowding", "head_noprimary")
  
  needs <- needs[needs %in% names(sel)]
  
  if (length(needs) > 1){
    bsl$deprivation <- rowSums(bsl[ , needs])
    sel$deprivation <- rowSums(sel[ , needs])
  } else{
    bsl$deprivation <- bsl[ , needs]
    sel$deprivation <- sel[ , needs]
  }
  
  bsl_anchors <- NULL
  sel_anchors <- NULL
  
  #First do deprivation, depending on the number of indicators of deprivation available
  if (length(needs) > 1){
    for (j in 1:length(needs)){
      bsl$depcut <- bsl$deprivation >= j
      sel$depcut <- sel$deprivation >= j
      
      moddep_sel <- glm(depcut ~ wealth_factor, data = sel, family = "binomial")
      moddep_bsl <- glm(depcut ~ wealth_factor, data = bsl, family = "binomial")
      
      bsl_anchors <- c(bsl_anchors, -moddep_bsl$coefficients[1]/moddep_bsl$coefficients[2])
      sel_anchors <- c(sel_anchors, -moddep_sel$coefficients[1]/moddep_sel$coefficients[2])
    }
  } else{
    moddep_sel <- glm(deprivation ~ wealth_factor, data = sel, family = "binomial")
    moddep_bsl <- glm(deprivation ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, -moddep_bsl$coefficients[1]/moddep_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, -moddep_sel$coefficients[1]/moddep_sel$coefficients[2])
  }
  
  #Then do Wealth Anchors
  #Television
  if ("television" %in% names(sel)){
    modtv_sel <- glm(television ~ wealth_factor, data = sel, family = "binomial")
    modtv_bsl <- glm(television ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, -modtv_bsl$coefficients[1]/modtv_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, -modtv_sel$coefficients[1]/modtv_sel$coefficients[2])
  }
  
  #Telephone
  if ("telephone" %in% names(sel)){
    modphone_sel <- glm(telephone ~ wealth_factor, data = sel, family = "binomial")
    modphone_bsl <- glm(telephone ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, -modphone_bsl$coefficients[1]/modphone_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, -modphone_sel$coefficients[1]/modphone_sel$coefficients[2])
  }
  
  #Car or Truck
  if ("car_truck" %in% names(sel)){
    modcar_sel <- glm(car_truck ~ wealth_factor, data = sel, family = "binomial")
    modcar_bsl <- glm(car_truck ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, -modcar_bsl$coefficients[1]/modcar_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, -modcar_sel$coefficients[1]/modcar_sel$coefficients[2])
  }
  
  #Refrigerator
  if ("refrigerator" %in% names(sel)){
    modref_sel <- glm(refrigerator ~ wealth_factor, data = sel, family = "binomial")
    modref_bsl <- glm(refrigerator ~ wealth_factor, data = bsl, family = "binomial")
    
    bsl_anchors <- c(bsl_anchors, -modref_bsl$coefficients[1]/modref_bsl$coefficients[2])
    sel_anchors <- c(sel_anchors, -modref_sel$coefficients[1]/modref_sel$coefficients[2])
  }
  
  #only regress anchors that are within a normal range
  ix <- which(sel_anchors < 2 & sel_anchors > -1)
  
  anchormod <- lm(bsl_anchors[ix] ~ sel_anchors[ix])
  
  others[others$survey_code==i, 'intercept'] <- anchormod$coefficients[1]
  others[others$survey_code==i, 'slope'] <- anchormod$coefficients[2]
  
  print(which(others$survey_code==i)/nrow(others))
}

##########################################
#Model Cutputs in relation to Baseline
##########################################

#Baseline is Nigeria 5 1 from 2008.  Large sample size, complete records, wide variety of income levels

hh_adj <- hh

for (i in others$survey_code){
  if (i %in% bad){
    hh_adj <- hh_adj %>%
      filter(survey_code != i)
    next
  }
  
  hh_ix <- hh_adj$survey_code == i
  
  hh_adj$wealth_factor[hh_ix] <- hh_adj$wealth_factor[hh_ix]*others$slope[others$survey_code==i] + others$intercept[others$survey_code==i]
}

hh_adj %>% group_by(survey_code) %>% summarize(mean(wealth_factor), sd(wealth_factor), n()) %>% View

hh_adj <- hh_adj %>%
  select(hhid, code, surveycode=survey_code, wealth_norm=wealth_factor)

write.csv(hh_adj, 'G://My Drive/DHS Processed/hh_wealth_harmonized.csv', row.names=F)

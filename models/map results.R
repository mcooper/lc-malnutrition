library(mgcv)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(patchwork)

load('~/mortalityblob/lc_gams/s2_noweights_te.Rdata')

all <- read.csv('~/mortalityblob/dhs/lc-malnutrition-weights2.csv')

nat <- raster('~/gd/DHS Spatial Covars/ESA Land Cover/natural_raster.tif')
prp <- raster('~/gd/DHS Spatial Covars/Final Rasters/2020/mean_annual_precip.tif')


##################################################
#Make a Map
################################################
#The more precision here, the longer it takes!
nat <- round(nat, 2)
prp <- crop(round(prp, -2), extent(nat))

df <- stack(nat, prp) %>%
  rasterToPoints %>%
  data.frame %>%
  select(natural=layer.1, mean_annual_precip=layer.2) %>%
  unique %>%
  na.omit

df$age <- 1
df$birth_order <- 1
df$hhsize <- 10
df$sex <- "Male"
df$mother_years_ed <- 10
df$toilet <- "Other"
df$interview_year <- 2000
df$calc_birthmonth <- 3
df$head_age <- 40
df$head_sex <- "Male"
df$wealth_norm <- 0
df$latitude <- 0
df$longitude <- 0
df$spei24 <- 1

alt <- df
alt$natural <- 0

p_actual <- predict(mod, df, type='terms', se=T)
p_cfactual <- predict(mod, alt, type='terms', se=T)

actual <- nat
cfactual <- nat
actual[actual > -99] <- 0
cfactual[cfactual > -99] <- 0

pb <- txtProgressBar(min=1, max=nrow(df), style=3)
for (i in 1:nrow(df)){
  ix <- (nat==df$natural[i] & prp==df$mean_annual_precip[i])
  actual[ix] <- p_actual$fit[i , paste0('te(mean_annual_precip,natural):spei24')]
  cfactual[nat==df$natural[i]] <- p_cfactual$fit[i , paste0('te(mean_annual_precip,natural):spei24')]

  setTxtProgressBar(pb, i)
}
close(pb)

#######################################
# Make a grid
#######################################

df <- expand.grid(list(natural=seq(0, 1, length.out=100),
                       mean_annual_precip=seq(0, maxValue(prp), length.out=100)))

df$age <- 1
df$birth_order <- 1
df$hhsize <- 10
df$sex <- "Male"
df$mother_years_ed <- 10
df$toilet <- "Other"
df$interview_year <- 2000
df$calc_birthmonth <- 3
df$head_age <- 40
df$head_sex <- "Male"
df$wealth_norm <- 0
df$latitude <- 0
df$longitude <- 0
df$spei24 <- 1

p_actual <- predict(mod, df[1, ], type='terms', se=T)

df$coef <- p_actual$fit[ , 'te(mean_annual_precip,natural):spei24']

ggplot(df) + 
  geom_raster(aes(x=natural, y=mean_annual_precip, fill=coef)) +
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", 
                                   "#e4ea9a", "#fbf8c0", "#fce08a", "#faae61", 
                                   "#f36c44", "#a01c44"))


##Explore distribution of the 3 key variables
library(plot3D)
library(car)

perc.rank <- function(x) trunc(rank(x))/length(x)

s <- sample(1:nrow(all), 500)

scatter3d(perc.rank(all$market_dist)[s],
         perc.rank(all$natural)[s],
         perc.rank(all$mean_annual_precip)[s],
         surface=FALSE)


md <- crop(raster('~/gd/DHS Spatial Covars/Final Rasters/2020/market_dist.tif'), extent(prp))

new <- stack(md, nat, prp) %>%
  rasterToPoints %>%
  data.frame %>%
  na.omit %>%
  rename(nat=layer.1, prp=layer.2)

new2 <- all %>%
  mutate(nat=Hmisc::cut2(natural, g=10),
         prp=Hmisc::cut2(mean_annual_precip, g=10),
         md=Hmisc::cut2(market_dist, g=10)) %>%
  group_by(nat, md) %>%
  summarize(n=n())

ggplot(new2) + geom_raster(aes(x=md, y=nat, fill=n)) +
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", 
                                   "#e4ea9a", "#fbf8c0", "#fce08a", "#faae61", 
                                   "#f36c44", "#a01c44"))





















#Only model counterfactual for subforest regions (where we saw a real effect)
cfactual[!aez %in% c(8, 9)] <- actual[!aez %in% c(8, 9)] 

#Get estimated standard deviation of stunting
# sdsum <- all %>%
#   group_by(interview_year) %>%
#   summarize(sd=sd(haz_dhs))

#sd_pred <- predict(lm(sd ~ interview_year, data=sdsum), data.frame(interview_year=2020))
sd_pred <- sd(all$haz_dhs)

#Get inverse CDF
proj2020 <- raster('G://My Drive/lc-malnutrition/stunting/proj2020.tif')
proj2020_q <- calc(proj2020, function(x){qnorm(x)})

#To estimate mean HAZ scores across africa in 2020 
m <- -sd_pred*proj2020_q - 2

#Get HAZ scores under drought (SPEI=-2.5) with current lc
m_drought <- m - actual*2.5

#Get HAZ scores under drought (SPEI=-2.5) without current lc (counterfactual)
m_drought_cf <- m - cfactual*2.5
  
#Convert HAZ scores under drought back to P < q
p_drought <- calc(m_drought, function(x){pnorm(-2, x, sd_pred)})
p_drought_cf <- calc(m_drought_cf, function(x){pnorm(-2, x, sd_pred)})

#See increase in rate of stunting in counterfactual
increase <- p_drought_cf - p_drought

#Get count of excess stunted children
u5pop <- raster('G://My Drive/lc-malnutrition/Age Structure/u5_pop.tif') %>%
  crop(extent(nat))

increased_burden <- u5pop*increase

nr_dependent <- u5pop
nr_dependent[increase == 0 | is.na(increase)] <- 0

#Aggregate by country
cty <- ne_countries()

cty@data$increased_burden <- raster::extract(increased_burden, cty, fun=sum, na.rm=T)
cty@data$nr_dependant <- raster::extract(nr_dependent, cty, fun=sum, na.rm=T)

u5tab <- read.csv('G://My Drive/DHS Processed/Under5Population.csv')

library(sf)
library(stars)

cty_sf <- st_as_sf(cty)

cty_sf <- merge(cty_sf, u5tab)

library(viridis)
library(RColorBrewer)
library(cowplot)
library(scales)

options(spipen=1000)

actual_stars <- st_as_stars(actual)
increase_stars <- st_as_stars(increase)
increased_burden_stars <- st_as_stars(increased_burden)

spei_coef <- ggplot() +
  geom_stars(data=actual_stars) + 
  scale_fill_gradient(low='#fff7bc', high='#d95f0e') + 
  theme_void() + 
  labs(fill="24 Monnth\nSPEI\nCoefficient")

increase <- ggplot() +
  geom_stars(data=increase_stars) + 
  scale_fill_gradient(low='#e7e1ef', high='#dd1c77') + 
  theme_void() + 
  labs(fill="Increase\nIn Stunting\nPrevalence\nWithout\nNature")

burden <- ggplot() +
  geom_stars(data=log(increased_burden_stars + 1)) + 
  #geom_sf(data=cty_sf, color='#EEEEEE', alpha=0) + 
  scale_fill_viridis(direction = 1, option='A', 
                     label=function(x){round(exp(x) + 1, 0)}) + 
  xlim(-18, 52) + 
  ylim(-35, 20) + 
  theme_void() + 
  labs(fill="Number\nPotental\nStunted\nChildren\nPer Pixel")

(cty_level <- ggplot(cty_sf) + 
  geom_sf(aes(fill=nr_dependant/(pop_est*u5*0.01))) + 
  scale_fill_gradient(low="#ece7f2", high="#2b8cbe") + 
  xlim(-18, 52) + 
  ylim(-35, 20) + 
  theme_void() + 
  labs(fill="Fraction\nof Children\nDependent\non Nature"))

wrap_plots(spei_coef, increase, burden, cty_level,
           widths=c(1, 1), heights=c(1, 1)) + 
  plot_annotation(tag_levels = 'A')

ggsave('C://Users/matt/lc-malnutrition-tex/AfricaEffect.png', height=6, width=8)




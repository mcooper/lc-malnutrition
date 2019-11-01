library(tidyverse)
library(mgcv)
library(ggplot2)

modname <- 'AEZmerge_weights_REML'

setwd('G://My Drive/lc-malnutrition/')

load(paste0('GAMs/', modname, '.Rdata'))

df <- expand.grid(list(natural=seq(0, 1, length.out=100),
                       market_dist=seq(0, 8.2, length.out = 100)))
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
df$AEZ_new <- mod$model$AEZ_new[1]

p <- predict(mod, df, type='terms', se=T)

df$smooth <- p$fit[ , 'te(market_dist,natural):spei24']
df$smooth_se <- p$se.fit[ , 'te(market_dist,natural):spei24']

comb <- df[ , c('smooth', 'market_dist', 'natural', 'smooth_se')]

setwd('GAM results')
dir.create(modname)
setwd(modname)

comb$significant <- (abs(comb$smooth) - comb$smooth_se*2) > 0
comb$smooth[comb$smooth > 0.25] <- 0.25
comb$smooth[comb$smooth < -0.25] <- -0.25

#Plot all
ggplot() +
  geom_tile(data=comb, aes(x=market_dist, y=natural, fill=smooth)) +
  geom_point(data=mod$model,
             aes(x=market_dist, y=natural), shape=16, size=0.05, alpha=0.5) +
  scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_viridis_c(limits=c(-0.25, 0.25))

ggsave('all_merge.png')

#Plot only significant vars
ggplot() +
  geom_tile(data=comb, aes(x=market_dist, y=natural, fill=smooth)) +
  geom_point(data=comb,
             aes(x=market_dist, y=natural, color=significant), shape=16, size=0.1, alpha=0.5) +
  scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_viridis_c(limits=c(-0.25, 0.25)) +
  scale_color_manual(values=c('black', 'white'))

ggsave('all_merge_signif.png')





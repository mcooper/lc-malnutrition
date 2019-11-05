library(tidyverse)
library(mgcv)
library(ggplot2)

setwd('G://My Drive/lc-malnutrition/')

modname <- 'AEZ_weights_REML_natOnly'

load(paste0('GAMs/', modname, '.Rdata'))

comb <- data.frame()
for (a in unique(mod$model$AEZ_new)){
  df <- expand.grid(list(natural=seq(0, 1, length.out=100)))

  df$AEZ_new <- a
  df[ , as.character(unique(mod$model$AEZ_new))] <- 0
  df[ , a] <- 1
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

  p <- predict(mod, df, type='terms', se=T)

  df$smooth <- p$fit[ , paste0('te(market_dist,natural):', a)]
  df$smooth_se <- p$se.fit[ , paste0('te(market_dist,natural):', a)]

  sel <- df[ , c('smooth', 'market_dist', 'natural', 'smooth_se')]
  sel$AEZ <- a

  comb <- bind_rows(comb, sel)
  
  print(a)
}

setwd('GAM results')
dir.create(modname)
setwd(modname)

comb$significant <- (abs(comb$smooth) - comb$smooth_se*2) > 0
comb$smooth[comb$smooth > 0.25] <- 0.25
comb$smooth[comb$smooth < -0.25] <- -0.25

#Plot all
for (i in unique(comb$AEZ)){
  ggplot() +
    geom_tile(data=comb %>% dplyr::filter(AEZ==i), aes(x=market_dist, y=natural, fill=smooth)) +
    geom_point(data=mod$model %>% dplyr::filter(AEZ_new == i),
               aes(x=market_dist, y=natural), shape=16, size=0.1, alpha=0.5) +
    scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_viridis_c(limits=c(-0.25, 0.25)) +
    labs(title=i)

  ggsave(paste0(i, '.png'))
}


#Plot only significant vars
for (i in unique(comb$AEZ)){
  ggplot() +
    geom_tile(data=comb %>% dplyr::filter(AEZ==i), aes(x=market_dist, y=natural, fill=smooth)) +
    geom_point(data=comb %>% dplyr::filter(AEZ==i),
               aes(x=market_dist, y=natural, color=significant), shape=16, size=0.1, alpha=0.5) +
    scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_viridis_c(limits=c(-0.25, 0.25)) +
    scale_color_manual(values=c('black', 'white')) +
    labs(title=i)

  ggsave(paste0(i, '_signif.png'))
}





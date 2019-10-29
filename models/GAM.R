library(tidyverse)
library(mgcv)

options(stringsAsFactors=F)

setwd('~/mortalityblob/')

all <- read.csv('dhs/lc-malnutrition-weights.csv')

all$market_dist <- log(all$market_dist + 1)

all$afr.arid.123 <- (all$AEZ_new == 'afr.arid.123')*all$spei24
all$afr.forest.4 <- (all$AEZ_new == 'afr.forest.4')*all$spei24
all$nafr.sav.5 <- (all$AEZ_new == 'nafr.sav.5')*all$spei24
all$seafr.sav.6 <- (all$AEZ_new == 'seafr.sav.6')*all$spei24
all$afr.high.7 <- (all$AEZ_new == 'afr.high.7')*all$spei24
all$nafr.subforest.8 <- (all$AEZ_new == 'nafr.subforest.8')*all$spei24
all$safr.subforest.9 <- (all$AEZ_new == 'safr.subforest.9')*all$spei24


mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=afr.arid.123) + 
             te(market_dist, natural, by=afr.forest.4) + 
             te(market_dist, natural, by=nafr.sav.5) + 
             te(market_dist, natural, by=seafr.sav.6) + 
             te(market_dist, natural, by=afr.high.7) + 
             te(market_dist, natural, by=nafr.subforest.8) + 
             te(market_dist, natural, by=safr.subforest.9) - 1, 
           data=all, weights = weights, method='REML')

save(mod, file = 'lc-gams/AEZ_weights_REML.Rdata')


mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=afr.arid.123) + 
             te(market_dist, natural, by=afr.forest.4) + 
             te(market_dist, natural, by=nafr.sav.5) + 
             te(market_dist, natural, by=seafr.sav.6) + 
             te(market_dist, natural, by=afr.high.7) + 
             te(market_dist, natural, by=nafr.subforest.8) + 
             te(market_dist, natural, by=safr.subforest.9) - 1, 
           data=all, method='REML')

save(mod, file = 'lc-gams/AEZ_noweights_REML.Rdata')

mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=afr.arid.123) + 
             te(market_dist, natural, by=afr.forest.4) + 
             te(market_dist, natural, by=nafr.sav.5) + 
             te(market_dist, natural, by=seafr.sav.6) + 
             te(market_dist, natural, by=afr.high.7) + 
             te(market_dist, natural, by=nafr.subforest.8) + 
             te(market_dist, natural, by=safr.subforest.9) - 1, 
           data=all, weights = weights)

save(mod, file = 'lc-gams/AEZ_weights_GCV.Rdata')


mod <- gam(haz_dhs ~ age + birth_order + hhsize + sex + mother_years_ed + toilet + interview_year + 
             as.factor(calc_birthmonth) + head_age + head_sex + wealth_norm + 
             s(latitude, longitude, bs='sos') + AEZ_new + 
             te(market_dist, natural, by=afr.arid.123) + 
             te(market_dist, natural, by=afr.forest.4) + 
             te(market_dist, natural, by=nafr.sav.5) + 
             te(market_dist, natural, by=seafr.sav.6) + 
             te(market_dist, natural, by=afr.high.7) + 
             te(market_dist, natural, by=nafr.subforest.8) + 
             te(market_dist, natural, by=safr.subforest.9) - 1, 
           data=all)

save(mod, file = 'lc-gams/AEZ_noweights_GCV.Rdata')

system('~/telegram.sh "Done with gams"')

# 
# 
# 
# for (i in unique(all$AEZ_new)){
#   df <- expand.grid(list(natural=seq(0, 1, length.out=100),
#                          market_dist=seq(min(all$market_dist), max(all$market_dist), length.out = 100)))
#   
#   df$AEZ_new <- i
#   df[ , gsub(' ', '.', as.character(unique(all$AEZ_new)))] <- 0
#   df[ , gsub(' ', '.', i)] <- 1
#   df$age <- 1
#   df$birth_order <- 1
#   df$hhsize <- 10
#   df$sex <- "Male"
#   df$mother_years_ed <- 10
#   df$toilet <- "Other"
#   df$interview_year <- 2000
#   df$calc_birthmonth <- 3
#   df$head_age <- 40
#   df$head_sex <- "Male"
#   df$wealth_norm <- 0
#   df$latitude <- 0
#   df$longitude <- 0
#   
#   p <- predict(mod, df, type='terms', se=T)
#   
#   df$smooth <- p$fit[ , paste0('s(market_dist,natural):', gsub(' ', '.', i))]
#   df$smooth_se <- p$se.fit[ , paste0('s(market_dist,natural):', gsub(' ', '.', i))]
#   
#   sel <- df[ , c('smooth', 'market_dist', 'natural', 'smooth_se')]
#   sel$AEZ <- i
#   
#   comb <- bind_rows(comb, sel)  
# }
# 
# #Plot all
# for (i in unique(comb$AEZ)){
#   ggplot() + 
#     geom_tile(data=comb %>% filter(AEZ==i), aes(x=market_dist, y=natural, fill=smooth)) + 
#     geom_point(data=all %>% filter(AEZ_new == i), 
#                aes(x=market_dist, y=natural), shape=16, size=0.1, alpha=0.5) +
#     scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) + 
#     scale_y_continuous(expand=c(0,0)) +
#     scale_fill_viridis_c() + 
#     labs(title=i)
#   
#   ggsave(paste0('G://My Drive/lc-malnutrition/GAMs/', i, '.png'))
# }
# 
# comb$significant <- (abs(comb$smooth) - comb$smooth_se*2) > 0
# 
# #Plot only significant vars
# for (i in unique(comb$AEZ)){
#   ggplot() + 
#     geom_tile(data=comb %>% filter(AEZ==i), aes(x=market_dist, y=natural, fill=smooth)) + 
#     geom_point(data=comb %>% filter(AEZ==i), 
#                aes(x=market_dist, y=natural, color=significant), shape=16, size=0.1, alpha=0.5) +
#     scale_x_continuous(labels=function(x){round(exp(x))}, expand = c(0,0)) + 
#     scale_y_continuous(expand=c(0,0)) +
#     scale_fill_viridis_c() + 
#     scale_color_manual(values=c('black', 'white')) + 
#     labs(title=i)
#   
#   ggsave(paste0('G://My Drive/lc-malnutrition/GAMs/', i, '_signif.png'))
# }
# 
# 
# 
# 

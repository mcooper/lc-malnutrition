library(ggplot2)
library(dplyr)
library(lme4)

setwd('G://My Drive/DHS Processed')

all <- read.csv('dhs-africa-matched.csv')

#Get Residuals
mod.ag <- loess(haz_residuals ~ spei36, data = all %>% filter(!natural), span = 0.5)

mod.nat <- loess(haz_residuals ~ spei36, data = all %>% filter(natural), span = 0.5)

mod <- c('> Median Natural Land Cover', '<= Median Natural Land Cover')
spei36 <- seq(-3, 3, len=100)

data <- expand.grid(mod, spei36)
names(data) <- c('mod', 'spei36')

pred <- function(spei36, mod){
  if (mod=='<= Median Natural Land Cover'){
    pred <- predict(mod.ag, newdata=data.frame(spei36=spei36))
  }
  if (mod=='> Median Natural Land Cover'){
    pred <- predict(mod.nat, newdata=data.frame(spei36=spei36))
  }
  pred
}

data$prediction <- mapply(pred, mod=data$mod, spei36=data$spei36)

ggplot(data, aes(x=spei36, y=prediction, color=mod)) + 
  geom_line(size=1.5) +
  labs(title="Rainfall and Predicted Child Heights - GlobeLand30",
       subtitle="For Households > 2.5 Hours From A Major City",
       x="24-Month Standardized Precipitation Evapotranspiration Index",
       y="Difference from Prediction (Residual)") +
  scale_color_manual(values=c('#4CA950', '#E6673E')) +
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position=c(0.05,0.05),
        legend.justification=c(0,0))

ggplot(sel) + 
  geom_point(aes(x=longitude, y=latitude, color=natural))



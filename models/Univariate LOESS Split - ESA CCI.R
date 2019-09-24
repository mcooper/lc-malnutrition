library(ggplot2)
library(dplyr)
library(lme4)

setwd('G://My Drive/DHS Processed')

all <- read.csv('dhs-africa-matched.csv')

#Get Residuals
mod.urban <- loess(haz_residuals ~ spei24, data = all %>% filter(!natural), span = 0.5)

mod.rural <- loess(haz_residuals ~ spei24, data = all %>% filter(natural), span = 0.5)

mod <- c('> Median Natural Land Cover', '<= Median Natural Land Cover')
spei24 <- seq(-3, 3, len=100)

data <- expand.grid(mod, spei24)
names(data) <- c('mod', 'spei24')

pred <- function(spei24, mod){
  if (mod=='<= Median Natural Land Cover'){
    pred <- predict(mod.urban, newdata=data.frame(spei24=spei24))
  }
  if (mod=='> Median Natural Land Cover'){
    pred <- predict(mod.rural, newdata=data.frame(spei24=spei24))
  }
  pred
}

data$prediction <- mapply(pred, mod=data$mod, spei24=data$spei24)

ggplot(data, aes(x=spei24, y=prediction, color=mod)) + 
  geom_line(size=1.5) +
  labs(title="Rainfall and Predicted Child Heights - ESA CCI Land Cover",
       subtitle="For Households > 2.5 Hours From A Major City",
       x="24-Month Standardized Precipitation Evapotranspiration Index",
       y="Difference from Prediction (Residual)") +
  scale_color_manual(values=c('#4CA950', '#E6673E')) +
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position=c(0.05,0.05),
        legend.justification=c(0,0))

ggsave('C://Users/matt/Desktop/ESA-CCI.png', width=6, height=4.5)

library(tidyverse)

data <- read_csv('/home/mattcoop/mortalityblob/dhs/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>% 
  filter(cc %in% c("AO", "BF", "BJ", "BU", "CD", "CF", "CI", "CM", "ET", "GA", 
                   "GH", "GN", "KE", "KM", "LB", "LS", "MD", "ML", "MW", "MZ", "NG", 
                   "NI", "NM", "RW", "SL", "SN", "SZ", "TD", "TG", "TZ", "UG", "ZM", 
                   "ZW") & year >= 1992 & year <= 2015)

data <- data %>%
  filter(!is.na(spei3) & !is.na(spei36) & !is.infinite(spei3) & !is.infinite(spei36))

write.csv(data, '/home/mattcoop/mortalityblob/dhs/Mortality-combined-ssa.csv', row.names=F)
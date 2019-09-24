library(tidyverse)
library(mgcv)
library(parallel)

#The purpose of this is to see whether climate shocks have a detectable effect on mortality if I subset to just Africa
#If so, i will continue this analysis using just Africa.

#Will use Cubic spline regression, as this is supposed to be faster.  Failing that, will try thin plate splines, as they have worked before with this data

data <- read_csv('/home/mattcoop/mortalityblob/dhs/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>% 
  filter(cc %in% c("AO", "BF", "BJ", "BU", "CD", "CF", "CI", "CM", "ET", "GA", 
                   "GH", "GN", "KE", "KM", "LB", "LS", "MD", "ML", "MW", "MZ", "NG", 
                   "NI", "NM", "RW", "SL", "SN", "SZ", "TD", "TG", "TZ", "UG", "ZM", 
                   "ZW") & year >= 1992 & year <= 2015)

data <- data %>%
  filter(is.na(spei3) | is.na(spei36))

cl <- makeCluster(20, outfile = '')

#Short term rainfall shocks
spei3_africa_gam <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, bs='cr'),
                        family='binomial', data=data, cluster=cl)

save(spei3_africa_gam, file='~/mortalityblob/lc_gams/spei3_africa_gam.Rdata')

rm(spei3_africa_gam)


#Long term rainfall shocks
spei36_africa_gam <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36, bs='cr'),
                         family='binomial', data=data, cluster=cl)

save(spei36_africa_gam, file='~/mortalityblob/lc_gams/spei36_africa_gam.Rdata')

rm(spei36_africa_gam)

system('/home/mattcoop/telegram.sh "Mortality GAMs Done!"')



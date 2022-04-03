#######################
## Survival Analysis
## Script Maxime
#######################

library(haven)
library(tidyverse)
data <- read_dta("divorce.dta")
# Data cleaning
data$years <- data$years %>% as.numeric()
data$heduc <- data$heduc %>% as.factor()
data$heblack <- data$heblack %>% as.factor()
data$mixed <- data$mixed %>% as_factor()
data$div <- data$div %>% as_factor()

str(data)


# Education 
# Labels:
#value       label
# 0  < 12 years
# 1 12-15 years
# 2   16+ years

hist(data$heblack)
hist(data$years)
str(data)
ggplot(data,aes(x=years,y=heduc,colour=div))+geom_jitter(alpha=.5)+theme_bw()

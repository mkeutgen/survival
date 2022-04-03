#######################
## Survival Analysis
## Script Maxime
#######################


# 0 Data Cleaning & Preprocessing
data <- read_dta("divorce.dta")

library(haven)
library(tidyverse)
library(survival)
library(KMsurv)
library(survminer)
# Covariates : 
# Heduc : categorical variable, 3 levels education of the husband. 0 is under 12 years of education, 1 between 12 and 15 years, 2 is 16 years & above.
# Heblack : boolean racial group of the husband : 0 is non-black. 1 is black.
# Mixed : boolean, are the couple from the same racial group 
# Years : duration of marriage. Random variable subject to random right censoring (divorce or widowing)
# Div : failure indicator. 0 for censoring, 1 for divorce


# Data cleaning
data$years <- data$years %>% as.numeric()
data$heduc <- data$heduc %>% as.factor()
data$heblack <- data$heblack %>% as.factor()
data$mixed <- data$mixed %>% as_factor()
data$div <- data$div %>% as_factor()

str(data)

# PART A :  Exploratory Analysis. 

# Duration of marriage with covariate "black"
ggplot(data,aes(x=years,y=heblack,colour=div))+geom_boxplot()+theme_bw()

# Duration of marriage with covariate "education"
ggplot(data,aes(x=years,y=heduc,colour=div))+geom_jitter(alpha=.5)+theme_bw()

# Duration of marriage with different level of education : 

ggplot(data,aes(x=years,fill=div))+geom_density(alpha=.4)+facet_grid(heduc~.)+theme_bw()

# PART B: Survival Distribution for each levels of heduc
fit <- survfit(Surv(time = years,event = div)~heduc,data = divorce)
plot(fit)
ggsurvplot(fit,data = data)
# To be continued
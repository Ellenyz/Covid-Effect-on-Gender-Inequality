suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
#install.packages('broom')
suppressMessages(library(broom))

library(sandwich)
robust_se <- function(md){
  cov <- vcovHC(md, type = "HC")
  robust.se <- sqrt(diag(cov))   
  return(robust.se )
}


dt3 <- read.csv('cps_00011.csv')

dt3$D <- fifelse((dt3$MONTH >=4 & dt3$YEAR==2020),1,0)
dt3$female <- fifelse(dt3$SEX==2,1,0,na=NA)

dt3$dad <- fifelse((dt3$SEX==1 & dt3$YNGCH<18 & dt3$NCHILD!=0),1,0,na=NA)
dt3$mom <- fifelse((dt3$SEX==2 & dt3$YNGCH<18 & dt3$NCHILD!=0),1,0,na=NA)
dt3$parent <- fifelse((dt3$YNGCH<18),1,0,na=NA)  ##remember to consider marital status

dt3$with_spouse <- fifelse((dt3$MARST==1),1,0,na=NA)
dt3$single <- fifelse((dt3$MARST==6),1,0,na=NA)

dt3$male_nochild <- fifelse((dt3$SEX==1 & (dt3$NCHILD==0|dt3$YNGCH>=18)),1,0,na=NA)
dt3$female_nochild <- fifelse((dt3$SEX==2 & (dt3$NCHILD==0|dt3$YNGCH>=18)),1,0,na=NA)

## Setup for est1
dt3$D_mom <- dt3$D*dt3$mom
dt3$D_prt <- dt3$D*dt3$parent
dt3$D_fm <- dt3$D*dt3$female

dt.full <- dt3 %>% select('MONTH', 'YEAR', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','WTFINL', 'AHRSWORKT','female_nochild') %>%
  mutate_at(.vars = c('YEAR','MONTH', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild'), .funs = as.factor)

# Pooled
md0.var <- c('D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild')
md0 <- as.formula(paste('AHRSWORKT ~',paste(md0.var,collapse = '+')))
model_full0 <- lm(md0,data=dt.full, weights=WTFINL)
#summary(model_full0)

# without statefip
md1.var <- c('MONTH', 'YEAR', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild')
md1 <- as.formula(paste('AHRSWORKT ~',paste(md1.var,collapse = '+')))
model_full1 <- lm(md1,data=dt.full, weights=WTFINL)
#summary(model_full1)

# with statefip
md2.var <- c('MONTH', 'YEAR','STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild')
md2 <- as.formula(paste('AHRSWORKT ~',paste(md2.var,collapse = '+')))
model_full2 <- lm(md2,data=dt.full, weights=WTFINL)
#summary(model_full2)


## Add controls for the full set
ctr <- c("UHRSWORKT",'AGE','EDUC','RACE')
dt.full_ctr <- dt3 %>% select('MONTH', 'YEAR', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','WTFINL','AHRSWORKT','female_nochild',ctr) %>%
  mutate_at(.vars = c('YEAR','MONTH', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild','EDUC','RACE'), .funs = as.factor)
md3.var <- c('MONTH', 'YEAR', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild',ctr)
md3 <- as.formula(paste('AHRSWORKT ~',paste(md3.var,collapse = '+')))
model_full3 <- lm(md3 , data=dt.full_ctr, weights=WTFINL)
#summary(model_full3)
stargazer::stargazer(model_full0,model_full1, model_full2, model_full3, 
                     se=list(robust_se(model_full0),robust_se(model_full1),robust_se(model_full2),robust_se(model_full3)), 
                     column.labels = c('Full Sample (Married/Unmarried)'), 
                     title="Regression on Full Set", out='reg4.tex')
                     
#---------------------------------------------------------
# Summary Stat

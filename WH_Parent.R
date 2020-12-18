rm(list=ls())
#install.packages('fastDummies')
# Load libraries
#install.packages("lmtest")
#install.packages("sandwich")
suppressMessages(library(lmtest))
suppressMessages(library(sandwich))
suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(expss))
suppressMessages(library(plm))
suppressMessages(library(fastDummies))

#data <- read_labelled_csv('panel_data_cleared_labeled.csv')
data <- read.csv('2020WH.csv')
data <- data[which(data$AGE>=15 & data$AGE<=64),]
########################
#Create Dummy Variables#
########################
data$D <- fifelse(data$MONTH>=4,1,0)

data$female <- fifelse(data$SEX==2,1,0,na=NA)

data$dad <- fifelse((data$SEX==1 & data$YNGCH<18 & data$NCHILD!=0),1,0,na=NA)
data$mom <- fifelse((data$SEX==2 & data$YNGCH<18 & data$NCHILD!=0),1,0,na=NA)
data$parent <- fifelse((data$YNGCH<18),1,0,na=NA)  ##remember to consider marital status

data$with_spouse <- fifelse((data$MARST==1),1,0,na=NA)
data$single <- fifelse((data$MARST==6),1,0,na=NA)

data$male_nochild <- fifelse((data$SEX==1 & (data$NCHILD==0|data$YNGCH>=18)),1,0,na=NA)
data$female_nochild <- fifelse((data$SEX==2 & (data$NCHILD==0|data$YNGCH>=18)),1,0,na=NA)
##MARST		Marital status
##1		Married, spouse present
##2		Married, spouse absent
##3		Separated
##4		Divorced
##5		Widowed
##6		Never married/single
##7		Widowed or Divorced
##9		NIU

data$singlemom <- fifelse((data$with_spouse==0 & data$mom==1),1,0,na=NA)
data$singledad <- fifelse((data$with_spouse==0 & data$dad==1),1,0,na=NA)

## Setup for est1
data$D_mom <- data$D*data$mom
data$D_prt <- data$D*data$parent
data$D_fm <- data$D*data$female


fac_list <- c('MONTH','STATEFIP','COUNTY','SEX','MARST','EMPSTAT','LABFORCE','OCC2010','IND',
              'ABSENT','WHYUNEMP','WHYABSNT','WHYPTLWK','EMPSAME',
              'dad','mom','parent','with_spouse','single','male_nochild','female_nochild',
              'singlemom','singledad','D','female','D_mom','D_prt','D_fm')

##Transform into factor type
#data <- data %>% mutate_at(.vars = fac_list, .funs = as.factor) %>% 
#  select(WTFINL, CPSIDP, AHRSWORKT, MONTH, D_mom, D_prt, D_fm, mom, dad, female_nochild) %>% drop_na()

##Transform into factor type
data <- data %>% mutate_at(.vars = c('STATEFIP','MONTH','CPSIDP','D_mom','D_prt','D_fm','mom','dad','female_nochild','IND','EMPSAME'), .funs = as.factor) %>% 
  select(WTFINL, D, MONTH, STATEFIP, CPSIDP, AHRSWORKT, D_mom, D_prt, D_fm, mom, dad, female_nochild, UHRSWORKT,IND,EMPSAME,AGE)

# Create unique ID
d <- transform(data, Cluster_ID = as.numeric(interaction(MONTH,STATEFIP,CPSIDP,drop=TRUE)))


#vec <- c('WTFINL','AHRSWORKT','MONTH','D_mom','D_prt','D_fm','mom','dad','female_nochild')
#data <- dummy_cols(data,select_columns = 'MONTH')

#------------------------------------------------------------

####################
#Estimation Section#
####################
colnames(data)

# WH = MONTH + D*MOM + D*Parent + D*Female + Mom + Dad + Female_no_kids
#str_mon <- paste(sprintf("MONTH_%d", 1:8),collapse = ' + ')
## MONTH_1 + MONTH_2 + MONTH_3 + MONTH_4 + MONTH_5 + MONTH_6 + MONTH_7 + MONTH_8
model <- lm(AHRSWORKT ~ (MONTH + D_mom + D_prt + D_fm + mom + dad + female_nochild + UHRSWORKT + AGE), data=data, 
            weights=WTFINL,na.action=na.omit)
# Robust t test
coeftest(model, vcov = vcovHC(model, type = "HC0"))


fe <- plm(AHRSWORKT ~ D_mom + D_prt + D_fm + mom + dad + female_nochild + UHRSWORKT, data=d, weights=WTFINL,na.action = na.omit,
          index = c('MONTH','Cluster_ID'),  model = 'within', effect = "twoways")



summary(model)
summary(fe)


####################
#Estimation Section#
####################
#For short-term#
data.st %>% filter()

  
  
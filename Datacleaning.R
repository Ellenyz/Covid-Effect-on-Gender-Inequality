#####################
### Load Data Set ###
### Through IPUMS ###
#####################
rm(list=ls())
#install.packages('ipumsr')
suppressMessages(library('ipumsr'))
suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
ddi <- read_ipums_ddi('cps_00003.xml')
setwd('/Users/ellenyz/Documents/GitHub/Ellen')
data <- read_ipums_micro(ddi)


#################
# Data Cleaning #
#################
as.character(dt$WTFINL)
factor(dt$SEX)
factor(dt$WHYUNEMP)
factor(dt$WHYABSNT)
factor(dt$WHYPTLWK)
factor(dt$EMPSAME)
factor(dt$COUNTY)
factor(dt$STATEFIP)
write.csv(dt,file = 'panel_data.csv')
dt <- dt[is.na(dt$WTFINL)==F,] ##not sure whether this step is proper
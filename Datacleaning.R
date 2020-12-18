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

# Check for the missing data
dt[which(dt$AHRSWORKT==999),'AHRSWORKT'] <- NA # Hours Worked Lase Week
dt[which(dt$UHRSWORKT==999),'UHRSWORKT'] <- NA # Usual Working Hours
dt[which(dt$ABSENT==0),'ABSENT'] <- NA # Dummy_Absent from work
dt[which(dt$DURUNEMP==999),'DURUNEMP'] <- NA # Duration of Unemp
dt[which(dt$EMPSAME==99),'EMPSAME'] <- NA # Whether change employers
dt[which(dt$SEX==9),'SEX'] <- NA  # 1:Male 2: Female
dt[which(dt$EMPSTAT==0),'EMPSTAT'] <- NA
dt[which(dt$ABSENT==0),'ABSENT'] <- NA
dt[which(dt$WHYUNEMP==0),'WHYUNEMP'] <- NA
dt[which(dt$WHYABSNT==0),'WHYABSNT'] <- NA
dt[which(dt$WHYPTLWK==0),'WHYPTLWK'] <- NA


write.csv(dt,file = 'panel_data_cleared.csv')


#####################
### Load Data Set ###
### Through IPUMS ###
#####################
rm(list=ls())
install.packages('ipumsr')
library('ipumsr')
ddi <- read_ipums_ddi("cps_00002.xml")
data <- read_ipums_micro(ddi)

#################
# Data Cleaning #
#################
dt <- data[which(data$MONTH<9),]
dt <- dt[,c(-2,-3,-4,-5,-6,-7,-9,-10)]

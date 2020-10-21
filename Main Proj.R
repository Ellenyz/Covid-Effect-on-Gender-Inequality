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

# Descriptive Analysis
dt <- data.table(dt)

## Married Couple (Married, spouse present, with child)
WH <- dt[dt$MARST==1 & dt$NCHILD>0 & dt$YNGCH<=18,]
  ### Clarify emp status in labor force: emp = 1 if unemployed
  WH$EMP <- NA
  WH$EMP[which(WH$LABFORCE==2 & (WH$EMPSTAT==20|WH$EMPSTAT==21|WH$EMPSTAT==22))] <- 1
  WH$EMP[which(WH$LABFORCE==2 & (WH$EMPSTAT==10|WH$EMPSTAT==12))] <- 0

# Label gender
WH$SEX <- factor(WH$SEX,levels = c(1,2), labels = c("Male", "Female"))

##Aggregate by gender
WH_agg <-  WH[,.(avg_wh = weighted.mean(AHRSWORKT,WTFINL,na.rm = T), 
            unemp_rt = weighted.mean(EMP,WTFINL,na.rm = T), 
            avg_dur_ump = weighted.mean(DURUNEMP,WTFINL,na.rm = T),
            obs = .N), 
            by = .(SEX,MONTH)]

# Subsample with continuously employed
WH_emp <- WH[WH$EMP==0,]
WH_agg_emp = WH_emp[,.(avg_wh = weighted.mean(AHRSWORKT,WTFINL,na.rm = T), 
               unemp_rt = weighted.mean(EMP,WTFINL,na.rm = T), 
               avg_dur_ump = weighted.mean(DURUNEMP,WTFINL,na.rm = T),
               obs = .N), 
            by = .(SEX,MONTH)]


#Plot
#### Average working hours variation through month 1~9 
#### SampleGroup: Married Couple (Married, spouse present, with child) in labor force
colors <- c("Male" = "Blue", "Female" = "Red")
plt1 <-  ggplot(data=WH_agg,aes(x=MONTH, y=avg_wh, group=factor(SEX), shape=factor(SEX), colour=factor(SEX))) + 
         ggtitle("Average Working Hours") + 
         xlab("Month") + ylab("Hours") + geom_line() + geom_point() + ylim(20,50)
plt1 + scale_colour_discrete(name  ="Gender", breaks=c(1, 2),labels=c("Man", "Woman")) + 
         scale_shape_discrete(name  ="Gender",breaks=c(1, 2),labels=c("Man", "Woman"))
   
plt2 <-  ggplot(data=WH_agg,aes(x=MONTH, y=unemp_rt, group=factor(SEX), shape=factor(SEX), colour=factor(SEX))) + 
  ggtitle("Umemployment Rate From January to August") + 
  xlab("Month") + ylab("Unemployment") + geom_line() + geom_point() 
plt2 + scale_colour_discrete(name  ="Gender",
                             breaks=c(1, 2),
                             labels=c("Man", "Woman")) + scale_shape_discrete(name  ="Gender",
                       breaks=c(1, 2),
                       labels=c("Man", "Woman"))+ scale_color_manual(values = colors)


### Add Relative WH
WH <- WH[WH$UHRSWORKT>0,]
WH$Relative_wh <- (WH$AHRSWORKT-WH$UHRSWORKT)/WH$UHRSWORKT*100

Relative_WH_agg <-  WH[,.(avg_Relative_wh = weighted.mean(Relative_wh,WTFINL,na.rm = T),
               obs = .N), 
            by = .(SEX,MONTH)]


plt3 <- ggplot(data=Relative_WH_agg, aes(x=MONTH, y=avg_Relative_wh, group=factor(SEX), shape=factor(SEX), colour=factor(SEX))) + 
  ggtitle("Reduction in Work Hours") + 
  xlab("Month") + ylab("Work Hours Reduction in Percentage") + geom_line()+ geom_point() + geom_smooth(method='loess', linetype=4, size=0.5, se=FALSE) 
plt3 + scale_colour_discrete(name  ="Gender",
                             breaks=c(1,2),
                             labels=c("Man", "Woman")) + 
  scale_shape_discrete(name  ="Gender",
                       breaks=c(1, 2),
                       labels=c("Man", "Woman")) + scale_color_manual(values = colors) 

# Check for the distribution of samples among the months
p <- ggplot(data = WH, mapping = aes(x = MONTH))
p+geom_bar()


# Unemployment Duration
plt4 <-  ggplot(data=WH_agg,aes(x=MONTH, y=avg_dur_ump, group=factor(SEX), shape=factor(SEX), colour=factor(SEX))) + 
  ggtitle("Average Umemployment Duration From January to August") + 
  xlab("Month") + ylab("Unemployment Duration") + geom_line() + geom_point() 
plt4 + scale_colour_discrete(name  ="Gender",
                             breaks=c(1, 2),
                             labels=c("Man", "Woman")) + 
  scale_shape_discrete(name  ="Gender",
                       breaks=c(1, 2),
                       labels=c("Man", "Woman"))

# Plotting with WH_emp
plt5 <-  ggplot(data=WH_agg_emp,aes(x=MONTH, y=avg_wh, group=factor(SEX), shape=factor(SEX), colour=factor(SEX))) + 
  ggtitle("Average Working Hours") + 
  xlab("Month") + ylab("Hours") + geom_line() + geom_point() + ylim(20,50)
plt5 + scale_colour_discrete(name  ="Gender", breaks=c(1, 2),labels=c("Man", "Woman")) + 
  scale_shape_discrete(name  ="Gender",breaks=c(1, 2),labels=c("Man", "Woman")) 



## Descriptive analysis
table(WH$MONTH,WH$SEX)
WH_clean <- WH %>%
  dplyr::select(SEX,MONTH,YNGCH,AHRSWORKT,EMP) %>%
  group_by(SEX,MONTH) %>%
  summarize(n = n(),mean_WH = mean(AHRSWORKT,na.rm = TRUE),sd_WH=sd(AHRSWORKT,na.rm = TRUE))


write.csv(WH_clean,file='WH_clean.csv')  
  
## This is for testing
view(WH_clean)

  
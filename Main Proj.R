#####################
### Load Data Set ###
### Through IPUMS ###
#####################
rm(list=ls())
install.packages('ipumsr')
suppressMessages(library('ipumsr'))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
ddi <- read_ipums_ddi("cps_00002.xml")
data <- read_ipums_micro(ddi)

#################
# Data Cleaning #
#################
dt <- data[which(data$MONTH<9),]
dt <- dt[,c(-1,-3,-4,-5,-6,-7,-10)]
as.character(dt$CPSIDP)
factor(dt$SEX)
factor(dt$WHYUNEMP)
factor(dt$WHYABSNT)
factor(dt$WHYPTLWK)
factor(dt$EMPSAME)
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

# Descriptive Analysis
dt <- data.table(dt)

## Married Couple (Married, spouse present, with child)
WH <- dt[dt$MARST==1 & dt$NCHILD>0 & dt$YNGCH<=18,]
  ### Clarify emp status in labor force: emp = 1 if unemployed
  WH$EMP <- NA
  WH$EMP[which(WH$LABFORCE==2 & (WH$EMPSTAT==20|WH$EMPSTAT==21|WH$EMPSTAT==22))] <- 1
  WH$EMP[which(WH$LABFORCE==2 & (WH$EMPSTAT==10|WH$EMPSTAT==12))] <- 0
  

##Aggregate by gender
 WH_agg = WH[,.(avg_wh = weighted.mean(AHRSWORKT,WTFINL,na.rm = T), 
            unemp_rt = weighted.mean(EMP,WTFINL,na.rm = T), 
            avg_dur_ump = weighted.mean(DURUNEMP,WTFINL,na.rm = T),
            obs = .N), 
            by = .(SEX,MONTH)]
 
#Plot
#### Average working hours variation through month 1~9 
#### SampleGroup: Married Couple (Married, spouse present, with child) in labor force
plt1 <-  ggplot(data=WH_agg,aes(x=MONTH, y=avg_wh, group=factor(SEX), shape=factor(SEX), colour=factor(SEX))) + 
         ggtitle("Average Working Hours") + 
         xlab("Month") + ylab("Hours") + geom_line() + geom_point() + ylim(20,50)
plt1 + scale_colour_discrete(name  ="Gender",
                            breaks=c(2, 1),
                            labels=c("Woman", "Man")) + 
         scale_shape_discrete(name  ="Gender",
                       breaks=c(2, 1),
                       labels=c("Woman", "Man"))
   
plt2 <-  ggplot(data=WH_agg,aes(x=MONTH, y=unemp_rt, group=factor(SEX), shape=factor(SEX), colour=factor(SEX))) + 
  ggtitle("Umemployment Rate From January to August") + 
  xlab("Month") + ylab("Unemployment") + geom_line() + geom_point()
plt2 + scale_colour_discrete(name  ="Gender",
                             breaks=c(2, 1),
                             labels=c("Woman", "Man")) + 
  scale_shape_discrete(name  ="Gender",
                       breaks=c(2, 1),
                       labels=c("Woman", "Man"))

   
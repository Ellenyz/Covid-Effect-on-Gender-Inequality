suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
#install.packages('broom')
suppressMessages(library(broom))
dt2 <- read.csv('cps_00007.csv')
#summary(dt2$MARST)

dt2$D <- fifelse((dt2$MONTH >=4 & dt2$YEAR==2020),1,0)
#dt2$D <- as.factor(dt2$D)
#summary(dt2$D)

married <- dt2 %>% mutate_at(.vars = c('SEX'), .funs = factor, exclude = NA, labels=c('Male','Female'),levels=c(1,2)) 
#summary(married$SEX)

married <- dt2 %>% mutate(parent=ifelse((YNGCH>=0 & YNGCH<=17 & YNGCH!=99),1,0)) %>%
  mutate(dad=fifelse((SEX==1 & parent==1),1,0,na=NA)) %>%
  mutate(mom=fifelse((SEX==2 & parent==1),1,0,na=NA)) %>%
  mutate(fm_0kids=fifelse((SEX==2 & parent==0),1,0,na=NA)) %>%
  mutate(female=fifelse((SEX==2),1,0,na=NA)) %>%
  mutate(mom_D=mom*D) %>%
  mutate(parent_D=parent*D) %>% 
  mutate(female_D=female*D) %>%
  mutate(WH=na_if(AHRSWORKT, 999)) %>%
  mutate(YNGCH=na_if(YNGCH, 99)) %>%
  mutate(Group_ChildAge=fifelse(YNGCH<=5,1,fifelse(YNGCH<=12,2,fifelse(YNGCH<=17,3,0,na=NA),0),0)) %>%
  ## age of the youngest child: 1-5, 6-12 and 13-17 and no kids
  mutate_at(.vars = c('Group_ChildAge'), .funs = factor, exclude = NA, labels=c('no child >18','0-5','6-12','13-17'),levels=c(0,1,2,3))

var <- names(married)

lm1.x <- c('MONTH', 'YEAR', 'mom_D', 'parent_D', 'female_D', 'mom', 'dad', 'fm_0kids')
married_lm <- married %>% select(WH, MONTH, YEAR, mom_D, parent_D, female_D, mom, dad, fm_0kids, WTFINL) %>%
  drop_na() %>%
  mutate_at(.vars = lm1.x, .funs = as.factor)
a = as.formula(paste('WH ~',paste(lm1.x,collapse = '+')))
model <- lm(data=married_lm, a, weights=WTFINL)
#summary(model)

library(sandwich)
cov <- vcovHC(model, type = "HC")
robust.se <- sqrt(diag(cov))

#stargazer::stargazer(model,model,se=list(NULL, robust.se),
#                     column.labels=c("default","robust"))


#-----------------------------------------
#Next I do with grouping: WH ~ MONTH + YEAR + mom_D + mom  
lm2.x <- c('MONTH', 'YEAR', 'female_D', 'mom')
b = as.formula(paste('WH ~',paste(lm2.x,collapse = '+')))
b

pop <- function(group){
  married_grp <- married %>% select(WH, MONTH, YEAR, mom_D, parent_D, female_D, mom, dad, Group_ChildAge, WTFINL) %>%
    mutate_at(.vars = c('YEAR','MONTH'), .funs = as.factor) %>%
    filter(Group_ChildAge==group)
  m2 <- lm(b,data=married_grp, weights=WTFINL)
  return(m2)
}

model_grp5 <- pop('0-5')
model_grp12 <- pop('6-12')
model_grp17 <- pop('13-17')

married_grp <- married %>% select(WH, MONTH, YEAR, mom_D, parent_D, female_D, mom, dad, Group_ChildAge, WTFINL) %>%
  mutate_at(.vars = c('YEAR','MONTH'), .funs = as.factor)
model_married <- lm(b,data=married_grp, weights=WTFINL)

#summary(model_married)
#summary(model_grp5)
#summary(model_grp12)
#summary(model_grp17)

robust_se <- function(md){
    cov <- vcovHC(md, type = "HC")
    robust.se <- sqrt(diag(cov))   
    return(robust.se )
  }

#stargazer::stargazer(model_married,model_grp5,model_grp12,model_grp17,type = 'latex', 
#                     se=list(robust_se(model_married),robust_se(model_grp5),robust_se(model_grp12),robust_se(model_grp17)),
#                     column.labels = c('Married Couples','Child Age 1-5','Child Age 6-12','Child Age 13-17'),
#                     title="Regression Result by Child Age",out='reg1.tex')

#---------------------------------------------------------
#Grouping while add STATEFIP: WH ~ MONTH + YEAR + STATEFIP + mom_D + mom  
lm3.x <- c('MONTH', 'YEAR', 'STATEFIP', 'female_D', 'mom')
c = as.formula(paste('WH ~',paste(lm3.x,collapse = '+')))

pop <- function(group){
  married_grp <- married %>% select(WH, MONTH, YEAR, STATEFIP, mom_D, parent_D, female_D, mom, dad, Group_ChildAge, WTFINL) %>%
    mutate_at(.vars = c('STATEFIP','YEAR','MONTH'), .funs = as.factor) %>%
    filter(Group_ChildAge==group)
  m2 <- lm(c,data=married_grp, weights=WTFINL)
  return(m2)
}

model_grp5 <- pop('0-5')
model_grp12 <- pop('6-12')
model_grp17 <- pop('13-17')

married_grp <- married %>% select(WH, STATEFIP, MONTH, YEAR, mom_D, parent_D, female_D, mom, dad, Group_ChildAge, WTFINL) %>%
  mutate_at(.vars = c('YEAR','MONTH','STATEFIP'), .funs = as.factor)
model_married <- lm(c,data=married_grp, weights=WTFINL)

#summary(model_married)
#summary(model_grp5)
#summary(model_grp12)
#summary(model_grp17)

#stargazer::stargazer(model_married,model_grp5,model_grp12,model_grp17,type = 'latex', 
#                     se=list(robust_se(model_married),robust_se(model_grp5),robust_se(model_grp12),robust_se(model_grp17)),
#                     column.labels = c('Married Couples','Child Age 1-5','Child Age 6-12','Child Age 13-17'),
#                     title="Regression Result by Child Age",out='reg2.tex')



#------------------------------------------
# With Controls: individual features - race age educ employment_status usual_wh 
lm3.x <- c('MONTH', 'YEAR', 'STATEFIP', 'female_D', 'mom')
lv_race <- sort(unique(married$RACE))
married_ctr <- c()
married_ctr$RACE <- factor(married$RACE,level=lv_race,label=c(100,200,300,rep(999,length(lv_race)-3)))

rm(lv_race)
#summary(married_ctr$RACE)

#lv_educ <- sort(unique(married$EDUC))
married_ctr$EDUC <- factor(fifelse(married$EDUC<=92,1,fifelse(married$EDUC==111,2,3)))

#AGE 16-64
married_ctr$AGE <- married$AGE

#IND
#married_ctr$IND <- factor(married$IND)


#usual_wh
#install.packages('naniar')
library(naniar)
married_ctr$UHRSWORKT <- fifelse(married$UHRSWORKT<997,married$UHRSWORKT,999)

married_ctr <- data.frame(married_ctr)

# Setup for data set
married_ctr <- married_ctr %>% replace_with_na(replace = list(UHRSWORKT = 999))

married_ctr <- married %>% select(WH, MONTH, YEAR, STATEFIP,female_D, mom, WTFINL,Group_ChildAge) %>%
  mutate_at(.vars = c('MONTH', 'YEAR', 'STATEFIP','female_D', 'mom'), .funs = as.factor) %>%
  add_column(.data=married_ctr) 

var_with_ctr <- c("UHRSWORKT",'AGE','EDUC','RACE','WH','WTFINL',lm3.x)


d <- as.formula(paste('WH ~',paste(var_with_ctr[which(var_with_ctr!='WH' & var_with_ctr!='WTFINL')], collapse = '+')))

married_ctr <- married_ctr%>%  mutate(UHRSWORKT=na_if(UHRSWORKT, 'NA')) %>% mutate(WH=na_if(WH, 'NA')) %>% drop_na()

model_ctr <- lm(data=married_ctr[var_with_ctr], d, weights=WTFINL,na.action=na.omit)

e <- as.formula(paste('WH ~',paste(lm3.x, collapse = '+')))

model_noctr <- lm(data = married_ctr[var_with_ctr], e, weights=WTFINL,na.action = na.omit)

#summary(model_ctr)
#summary(model_noctr)
#stargazer::stargazer(model_noctr,type='latex',model_ctr,
#                     se=list(robust_se(model_noctr),robust_se(model_ctr)),
#                     title="Regression Result of Married Couples",out='reg3.tex')

#--------------------------------------
#WH volatility
agg_wh <- married %>% select(YEAR,AHRSWORKT,MONTH,SEX,parent,LABFORCE) %>% 
  filter(LABFORCE==2) %>%
  group_by(YEAR,MONTH,SEX,parent)%>%
  mutate(WH=AHRSWORKT)%>%
  mutate(WH=na_if(AHRSWORKT, 999)) %>%
  filter(YEAR==2020) %>%
  drop_na() %>%
  summarise_at(.vars=c('WH'),.funs=sd)
 
agg_wh$cat <- ifelse(agg_wh$SEX==2&agg_wh$parent==0,1,
                     ifelse(agg_wh$SEX==1&agg_wh$parent==0,2,
                            ifelse(agg_wh$SEX==2&agg_wh$parent==1,3,4)))
agg_wh$cat <- factor(agg_wh$cat,levels=1:4,labels=c('Female w/o kids','Male w/o kids','Mom','Dad'))
agg_wh$MONTH <- factor(agg_wh$MONTH,levels=1:11,labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov'))

plt <- ggplot(data=agg_wh, aes(x=MONTH, y=WH, group=cat,colour=cat,linetype=cat)) + 
  ggtitle("Monthly Volatility of Work Hours") + 
  xlab("Month") + ylab("Volatility") + geom_line() + geom_point() +
  theme(legend.position="bottom", legend.box = "horizontal",legend.title = element_blank())
plt <- plt + theme(text = element_text(size = 20))
plt
#ggsave(file='WH_Vol.svg',device = "svg",width = 10, height = 8)


agg_wh2 <- married %>% select(YEAR,AHRSWORKT,MONTH,SEX,parent,LABFORCE) %>% 
  filter(LABFORCE==2) %>%
  group_by(YEAR,MONTH,SEX,parent)%>%
  mutate(WH=AHRSWORKT)%>%
  mutate(WH=na_if(AHRSWORKT, 999)) %>%
  filter(YEAR==2020) %>%
  drop_na() %>%
  summarise_at(.vars=c('WH'),.funs=mean)
agg_wh2$cat <- ifelse(agg_wh2$SEX==2&agg_wh2$parent==0,1,
                     ifelse(agg_wh2$SEX==1&agg_wh2$parent==0,2,
                            ifelse(agg_wh2$SEX==2&agg_wh2$parent==1,3,4)))
agg_wh2$cat <- factor(agg_wh2$cat,levels=1:4,labels=c('Female w/o kids','Male w/o kids','Mom','Dad'))
agg_wh2$MONTH <- factor(agg_wh2$MONTH,levels=1:11,labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov'))

plt3 <- ggplot(data=agg_wh2, aes(x=MONTH, y=WH, group=cat,colour=cat,linetype=cat)) + 
  ggtitle("Monthly Average of Work Hours") + 
  xlab("Month") + ylab("Hours") + geom_line() + geom_point() + geom_vline(xintercept='Mar') + 
  theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank())
plt3 <- plt3 + theme(text = element_text(size = 20)) 
plt3


agg_wh3 <- married %>% select(YEAR,AHRSWORKT,MONTH,SEX,parent,LABFORCE) %>% 
  filter(LABFORCE==2) %>%
  group_by(YEAR,MONTH,SEX,parent)%>%
  mutate(WH=AHRSWORKT)%>%
  mutate(WH=na_if(AHRSWORKT, 999)) %>%
  filter(YEAR==2020) %>%
  drop_na() %>%
  summarise(n = n())
agg_wh3$cat <- ifelse(agg_wh3$SEX==2&agg_wh3$parent==0,1,
                      ifelse(agg_wh3$SEX==1&agg_wh3$parent==0,2,
                             ifelse(agg_wh3$SEX==2&agg_wh3$parent==1,3,4)))
agg_wh3$cat <- factor(agg_wh3$cat,levels=1:4,labels=c('Female w/o kids','Male w/o kids','Mom','Dad'))
agg_wh3$MONTH <- factor(agg_wh3$MONTH,levels=1:11,labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov'))

plt4 <- ggplot(data=agg_wh3, aes(fill=agg_wh3$cat, y=agg_wh3$n, x=agg_wh3$MONTH)) + 
  geom_bar(position="dodge", stat="identity") +  ggtitle("Distribution of Observations in 2020") + 
  xlab("Month") + ylab("Number of Observations (unweighted)") + 
  theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank()) + 
  theme(text = element_text(size = 20)) 
plt4



agg_wh4 <- married %>% select(YEAR,AHRSWORKT,MONTH,SEX,parent,LABFORCE) %>% 
  filter(LABFORCE==2) %>%
  group_by(YEAR,SEX,parent)%>%
  mutate(WH=AHRSWORKT)%>%
  mutate(WH=na_if(AHRSWORKT, 999)) %>%
  filter(YEAR==2020) %>%
  drop_na() %>%
  summarise_at(.vars=c('WH'),.funs=mean)
agg_wh4$cat <- ifelse(agg_wh4$SEX==2&agg_wh4$parent==0,1,
                      ifelse(agg_wh4$SEX==1&agg_wh4$parent==0,2,
                             ifelse(agg_wh4$SEX==2&agg_wh4$parent==1,3,4)))
agg_wh4$cat <- factor(agg_wh4$cat,levels=1:4,labels=c('Female w/o kids','Male w/o kids','Mom','Dad'))
#agg_wh4$MONTH <- factor(agg_wh4$MONTH,levels=1:11,labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov'))





#--------------------------------------
#Grouping while add STATEFIP and controls: WH ~ MONTH + YEAR + STATEFIP + mom_D + mom  

pop2 <- function(group){
  married_ctr2 <- married_ctr %>%
    filter(Group_ChildAge==group)
  m2 <- lm(data=married_ctr2, d, weights=WTFINL,na.action=na.omit)
  return(m2)
}

model_grp5.c <- pop2('0-5')
model_grp12.c <- pop2('6-12')
model_grp17.c <- pop2('13-17')

summary(model_grp5.c)
summary(model_grp12.c)
summary(model_grp17.c)

stargazer::stargazer(model_ctr,model_grp5.c,model_grp12.c,model_grp17.c,type = 'latex', 
                     se=list(robust_se(model_ctr),robust_se(model_grp5.c),robust_se(model_grp12.c),robust_se(model_grp17.c)),
                     column.labels = c('Married Couples','Child Age 1-5','Child Age 6-12','Child Age 13-17'),
                     title="Regression Result by Child Age",out='reg_ctr.tex')

#--------------------------------------------
#Pooled estimation
lm4.x <- c('female_D', 'mom')
f = as.formula(paste('WH ~',paste(lm4.x,collapse = '+')))
f

pop <- function(group){
  married_grp <- married %>% select(WH, mom_D, parent_D, female_D, mom, dad, Group_ChildAge, WTFINL) %>%
    filter(Group_ChildAge==group)
  m2 <- lm(f,data=married_grp, weights=WTFINL)
  return(m2)
}

model_grp5 <- pop('0-5')
model_grp12 <- pop('6-12')
model_grp17 <- pop('13-17')

married_grp <- married %>% select(WH, mom_D, parent_D, female_D, mom, dad, Group_ChildAge, WTFINL) %>%
model_married <- lm(f,data=married_grp, weights=WTFINL)

#summary(model_married)
#summary(model_grp5)
#summary(model_grp12)
#summary(model_grp17)

robust_se <- function(md){
  cov <- vcovHC(md, type = "HC")
  robust.se <- sqrt(diag(cov))   
  return(robust.se )
}

stargazer::stargazer(model_married,model_grp5,model_grp12,model_grp17,type = 'latex', 
                     se=list(robust_se(model_married),robust_se(model_grp5),robust_se(model_grp12),robust_se(model_grp17)),
                     column.labels = c('Married Couples','Child Age 1-5','Child Age 6-12','Child Age 13-17'),
                     title="Regression Result by Child Age",out='reg_pool.tex')


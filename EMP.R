suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
#install.packages('broom')
suppressMessages(library(broom))
suppressMessages(library(sandwich))
robust_se <- function(md){
  cov <- vcovHC(md, type = "HC")
  robust.se <- sqrt(diag(cov))   
  return(robust.se )
}


dt2 <- read.csv('cps_00011.csv')
#summary(dt2$MARST)

dt2$D <- fifelse((dt2$MONTH >=4 & dt2$YEAR==2020),1,0)
#dt2$D <- as.factor(dt2$D)
#summary(dt2$D)

emp.data <- dt2 %>% mutate_at(.vars = c('SEX'), .funs = factor, exclude = NA, labels=c('Male','Female'),levels=c(1,2)) 
factor(emp.data$EMPSTAT)
emp.data <- emp.data %>% filter(LABFORCE==2) %>%
  mutate(unemp = fifelse(EMPSTAT==21|EMPSTAT==22,1,0))
#emp.data <- emp.data %>% mutate_at(.vars = c('unemp'), .funs = factor, exclude = NA, labels=c('Employed','Unemployed'),levels=c(0,1)) 
#table(emp.data$unemp,emp.data$SEX)
#-------------------------------------------------------------------------
## summarize unemployment rate by month and category
#emp.data$unemp <- factor(emp.data$unemp,levels=c('Employed','Unemployed'),labels=c(0,1))
#emp.data$unemp <- numeric(emp.data$unemp)
emp_agg <- emp.data %>% select(YNGCH,SEX,unemp,MONTH,YEAR) %>% 
  mutate(parent=ifelse((YNGCH>=0 & YNGCH<=17 & YNGCH!=99),1,0)) %>%
  mutate(cat = ifelse(SEX=='Female'&parent==0,1,
                       ifelse(SEX=='Male'&parent==0,2,
                              ifelse(SEX=='Female'&parent==1,3,4)))) %>%
  filter(YEAR==2020) %>%
  select(MONTH,cat,unemp) %>%
  group_by(MONTH,cat)%>%
  drop_na() %>%
  summarise_at(.vars=c('unemp'),.funs=mean)

emp_agg$cat <- factor(emp_agg$cat,levels=1:4,labels=c('Female w/o kids','Male w/o kids','Mom','Dad'))
emp_agg$MONTH <- factor(emp_agg$MONTH,levels=1:11,labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov'))
emp_agg 
plt2 <- ggplot(data=emp_agg, aes(x=MONTH, y=unemp*100, group=cat,colour=cat,linetype=cat)) + 
  ggtitle("US Monthly Trend of Unemployment Rate") + 
  xlab("Month") + ylab("Unemployment Rate (pct)") + geom_line() + geom_point() +
  theme(legend.position="bottom", legend.box = "horizontal",legend.title = element_blank())
plt2 <- plt2 + theme(text = element_text(size = 20))
ggsave(file='Unemp_trend.svg',device = "svg")
plt2
#---------------------------------------------------------------------------

## Below are regressions
emp.data$SEX <- factor(emp.data$SEX,labels=c(1,2),levels=c('Male','Female'))
emp.data$D <- fifelse((emp.data$MONTH >=4 & emp.data$YEAR==2020),1,0)
emp.data$female <- fifelse(emp.data$SEX==2,1,0,na=NA)

emp.data$dad <- fifelse((emp.data$SEX==1 & emp.data$YNGCH<18 & emp.data$NCHILD!=0),1,0,na=NA)
emp.data$mom <- fifelse((emp.data$SEX==2 & emp.data$YNGCH<18 & emp.data$NCHILD!=0),1,0,na=NA)
emp.data$parent <- fifelse((emp.data$YNGCH<18),1,0,na=NA)  ##remember to consider marital status

emp.data$with_spouse <- fifelse((emp.data$MARST==1),1,0,na=NA)
emp.data$single <- fifelse((emp.data$MARST==6),1,0,na=NA)

emp.data$male_nochild <- fifelse((emp.data$SEX==1 & (emp.data$NCHILD==0|emp.data$YNGCH>=18)),1,0,na=NA)
emp.data$female_nochild <- fifelse((emp.data$SEX==2 & (emp.data$NCHILD==0|emp.data$YNGCH>=18)),1,0,na=NA)

## Setup for est1
emp.data$D_mom <- emp.data$D*emp.data$mom
emp.data$D_prt <- emp.data$D*emp.data$parent
emp.data$D_fm <- emp.data$D*emp.data$female

dt.full.emp <- emp.data %>% select('MONTH', 'YEAR', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','WTFINL', 'unemp','female_nochild') %>%
  mutate_at(.vars = c('YEAR','MONTH', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild'), .funs = as.factor) %>% drop_na()

#Pooled
md0.var <- c('D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild')
md0 <- as.formula(paste('unemp ~',paste(md0.var,collapse = '+')))
model_full.emp0 <- lm(md0,data=dt.full.emp, weights=WTFINL,na.action = na.omit)
#summary(model_full.emp0)

# without statefip
md1.var <- c('MONTH', 'YEAR', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild')
md1 <- as.formula(paste('unemp ~',paste(md1.var,collapse = '+')))
model_full.emp <- lm(md1,data=dt.full.emp, weights=WTFINL,na.action = na.omit)
#summary(model_full.emp)

# with statefip
md2.var <- c('MONTH', 'YEAR','STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild')
md2 <- as.formula(paste('unemp ~',paste(md2.var,collapse = '+')))
model_full.emp2 <- lm(md2,data=dt.full.emp, weights=WTFINL)
#summary(model_full.emp2)


## Add controls for the full set
ctr <- c("UHRSWORKT",'AGE','EDUC','RACE')
dt.full.emp_ctr <- emp.data %>% select('MONTH', 'YEAR', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','WTFINL','unemp','female_nochild',ctr) %>%
  mutate_at(.vars = c('YEAR','MONTH', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild','EDUC','RACE'), .funs = as.factor)
md3.var <- c('MONTH', 'YEAR', 'STATEFIP', 'D_mom', 'D_prt', 'D_fm', 'mom', 'dad','female_nochild',ctr)
md3 <- as.formula(paste('unemp ~',paste(md3.var,collapse = '+')))
model_full.emp3 <- lm(md3 ,data=dt.full.emp_ctr, weights=WTFINL)
#summary(model_full.emp3)
stargazer::stargazer(model_full.emp0, model_full.emp, model_full.emp2, model_full.emp3, type = 'latex', 
                     se=list(robust_se(model_full.emp0),robust_se(model_full.emp),robust_se(model_full.emp2),robust_se(model_full.emp3)), 
                     column.labels = c('Full Sample (Married/Unmarried)'), 
                     title="Regression on Full Set", out='reg_emp.tex')



#----------------------------------------------
#Summarize
emp.data$SEX <- factor(emp.data$SEX, labels=c('Male','Female'),levels=c(1,2)) 
emp_agg2 <- emp.data %>% 
  mutate(cat = ifelse((SEX=='Female'&parent==0),1,
                      ifelse((SEX=='Male'&parent==0),2,
                             ifelse((SEX=='Female'&parent==1),3,4)))) 
emp_agg2$cat <- factor(emp_agg2$cat,levels=1:4,labels=c('Female w/o kids','Male w/o kids','Mom','Dad'))
emp_agg2 <- emp_agg2 %>%
  select(cat,unemp) %>%
  group_by(cat)%>%
  drop_na() %>%
  summarise_at(.vars=c('unemp'),.funs=mean)

emp_agg2 







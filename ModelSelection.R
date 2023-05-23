getwd()
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = getwd())

#Clear Environment
rm(list = ls())

#Turn Off Scientific Notation
options(scipen=999)

#Install and Load Packages
library(readxl)
library(janitor)
library(olsrr)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(devtools)


#read data
DataSample<-read_excel("DataSample_Final.xlsx", sheet = 1)

#Clean up column names with Janitor Package
DataSample<-clean_names(DataSample)


#Add new columns around years
DataSample$age<-lubridate::time_length(difftime(Sys.Date(),DataSample$birthday),"years")
DataSample$tenure<-lubridate::time_length(difftime(Sys.Date(),DataSample$hire_date),"years")
DataSample$time_in_role<-lubridate::time_length(difftime(Sys.Date(),DataSample$role_start_date),"years")
DataSample$bonus_amt<-round(DataSample$salary*DataSample$bonus_target,2)
DataSample<-DataSample %>%
mutate(age_bin = cut(age,breaks = c(0, 25, 35, 45, 55, Inf), right = FALSE)) %>%
mutate(total_pay = salary + bonus_amt) %>%
mutate(salary_log = log(salary, base = exp(1)),
       ttc_log = log(total_pay, base = exp(1)),
       bonus_log = log(bonus_amt + 1, base = exp(1))) %>%
mutate_if(is_character, fct_infreq) %>%
mutate(age_bin = fct_infreq(age_bin))



#Set Model
BaseModel<-lm(salary~grade+age+performance_score+job_function+education+tenure+time_in_role+state+gender, data=DataSample)
summary(BaseModel)

BaseModel_log<-lm(salary_log~grade+age+performance_score+job_function+education+tenure+time_in_role+state+gender, data=DataSample)
summary(BaseModel_log)

#Check Model Strength
allmodels<-ols_step_all_possible(BaseModel)
allmodels[which.max(allmodels$adjr),]
allmodels[which.min(allmodels$aic),]

bestmodels<-ols_step_best_subset(BaseModel)
plot(bestmodels)
#Forward Stepwise Regression
ols_step_forward_p(BaseModel, penter=.05)

#Forward Stepwise Regression
ols_step_backward_p(BaseModel, prem=.1)

#Stepwise Regression
ols_step_both_p(BaseModel, penter=.05, prem=.1)

#Best Model
FinalModel<-lm(salary~grade+age+job_function+education+tenure, data=DataSample)
summary(FinalModel)
FinalModel_log<-lm(salary_log~grade+age+job_function+education+tenure, data=DataSample)
summary(FinalModel_log)

Predict_std<-predict(FinalModel,data=DataSample, interval = "confidence", level=.9)
Predict_log<-predict(FinalModel_log,data=DataSample, interval = "confidence", level=.9)
Predict_log<-exp(Predict_log)


datawithprediction<-cbind(DataSample,abc)
datawithprediction<-cbind(datawithprediction,Predict_log)

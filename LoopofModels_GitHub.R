#Clear Environment
rm(list = ls())

#Turn Off Scientific Notation
options(scipen=999)

#Install and Load Packages
#install.packages("readxl")#load first time only
library(readxl)
#LoadFile
DataSample<-read_excel("C://Users//Compensation//DataSample_Final.xlsx", sheet = "EEData")
DataSample <- DataSample[order(DataSample$Structure),]

#Remove Spaces in column names
colnames(DataSample)<-gsub(" ","_",colnames(DataSample))

#Add new columns around years
DataSample$Age<-lubridate::time_length(difftime(Sys.Date(),DataSample$Birthday),"years")
DataSample$Tenure<-lubridate::time_length(difftime(Sys.Date(),DataSample$Hire_Date),"years")
DataSample$TimeInRole<-lubridate::time_length(difftime(Sys.Date(),DataSample$Role_Start_Date),"years")

my_List<-list()
#Loop of Models
for (j in unique(DataSample$Structure)){
  ModelName<-lm(Salary~Gender+Tenure+Education+Grade, data=DataSample[DataSample$Structure==j,])
  print(j)
  print(summary(ModelName))
  my_List[[j]]<-ModelName
}


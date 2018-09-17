rm(list=ls())

library(dplyr)
library(plyr)
library(lubridate)
library(MASS)
library(car) 
library(DataCombine)
library(zoo)
library(glmnet)
library(DAAG)
library(ggplot2)


ecom_data<-read.csv("ConsumerElectronics.csv", stringsAsFactors = F)
str(ecom_data)

colnames(ecom_data) <- c("FSN_ID","Order_date","Year","Month","Order_id","Order_item_id","gmv","Units","deliverybdays","deliverycdays", "payment_mode","SLA","cust_id","pincode","P_super_category","P_analytic_category","P_sub_category","P_analytic_vertical","MRP","Procurement_SLA")


#######Data Sanity Check##############

#Step 1 - Data sanity Check: Data should be from July-2015 to June -2016
nrow(ecom_data)
# total # of records : 1648824

unique(ecom_data$Year)
unique(ecom_data$Month)

ecom_data<-ecom_data %>% filter(ecom_data$Order_date>="2015-07-01 00:00:00" & ecom_data$Order_date<="2016-06-30 23:59:59")

nrow(ecom_data)
#1648215

ecom_data$Order_date<-as.POSIXct(ecom_data$Order_date, format="%Y-%m-%d %H:%M:%S")
ecom_data$week <- week(as.POSIXlt(ecom_data$Order_date))

ecom_data$week<- ifelse(ecom_data$week<=26 & ecom_data$Year==2016,ecom_data$week+53,ecom_data$week)
max(unique(ecom_data$week))


#Step 4 - We should not considering free products
ecom_data <- subset(ecom_data,MRP!=0)
#1642907

#Step 5 - removing rows with NA values
sum(is.na(ecom_data))
ecom_data <- na.omit(ecom_data)
#1638021

#Step 6 - Make "gmv" =1 if they are 0
sum(ecom_data$gmv == 0)
ecom_data$gmv[which(ecom_data$gmv == 0)] <- 1
sum(ecom_data$gmv == 0)

#Step 7 - gmv should not be more than MRP*units since we can offer discounts but should not charge higher. 
#So, we will take a subset such that (MRP*Units)>=gmv

ecom_data <- subset(ecom_data,(MRP*Units)>=gmv)
#1604389

unique(ecom_data$P_analytic_vertical)
split_data<- split( ecom_data , f = ecom_data$P_analytic_category )
Camera<-merge(split_data$Camera,split_data$CameraAccessory,all.x=TRUE, all.y=TRUE)
Gaming<-merge(split_data$GameCDDVD,split_data$GamingHardware,all.x=TRUE, all.y=TRUE)
Home<-as.data.frame(split_data$EntertainmentSmall)

#Preparing KPI's

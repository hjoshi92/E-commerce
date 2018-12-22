#-------------------------------------------------------------------------------------------------------------------------
# Ecommerce Capstone Project
#
# Submitted by: Krishnaraj Barvathaya
#       Prakash Kamatar
#       Hemanth Joshi
#       Bhavana Tumurugoti
#-------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(plyr)
library(lubridate)
library(MASS)
library(car) 
library(DataCombine)
library(zoo)
library(ggplot2)
library(cowplot)
library(DAAG)
library(DataCombine)

#load the dataset
ecom<-read.csv("ConsumerElectronics.csv", stringsAsFactors = F)
nrow(ecom)
# total # of records : 1648824

# structure of dataset 
str(ecom)

#modify the column Names
colnames(ecom) <- c("FSN_ID","Order_date","Year","Month","Order_id","Order_item_id","gmv","Units","deliverybdays","deliverycdays", "payment_mode","SLA","cust_id","pincode","P_super_category","P_analytic_category","P_sub_category","P_analytic_vertical","MRP","Procurement_SLA")

#######Data Sanity Check##############

#Step 1 - Data sanity Check: Data should be from July-2015 to June -2016
#ecom <- subset(ecom, !(Month ==5 & Year==2015|Month ==6 & Year==2015|Month ==7 & Year==2016))
ecom<-ecom %>% filter(ecom$Order_date>="2015-07-01 00:00:00" & ecom$Order_date<="2016-06-30 23:59:59")
nrow(ecom)
#1648215

#GET WEEK NUMBERS
### Convert the dataset into weekly levels. 
ecom$Order_date<-as.POSIXct(ecom$Order_date, format="%Y-%m-%d %H:%M:%S")
# create new column week of year 
ecom$week_year<-as.numeric(week(as.POSIXlt(ecom$Order_date)))
# Data quality issue - Jan 2016 should be week 54, not week 1 etc. 
ecom$week_year<- ifelse(ecom$week_year<=26 & ecom$Year==2016,ecom$week_year+53,ecom$week_year)

max(as.numeric(ecom$week_year)) #79
min(as.numeric(ecom$week_year)) #26

#Step 4 - We should not considering free products
summary(ecom$MRP)
#check for products with MRP=0(free products)
#sum(is.na(ecom$MRP)) #0
#sum(ecom$MRP==0)
#sum(ecom$MRP<0)
#5308 records with MRP=0
(sum(ecom$MRP==0)/nrow(ecom))*100 #.3%
#we can exclude the records with MRP=0
#mrp_zero<-ecom[which((ecom$MRP==0)),] 
ecom <- subset(ecom,MRP!=0)
#1642907

#Step 5 - removing rows with NA values
ecom <- na.omit(ecom)
#1638021

#Step 6 - Make "gmv" =1 if they are 0
#check if GMV is < 0
#sum(ecom$gmv<0) #0
#sum(ecom$gmv==0) #985
(sum(ecom$gmv==0)/nrow(ecom))*100 #.06%
ecom$gmv[which(ecom$gmv == 0)] <- 1

#Step 7 - gmv should not be more than MRP*units since we can offer discounts but should not charge higher. 
#So, we will take a subset such that (MRP*Units)>=gmv
ecom <- subset(ecom,(MRP*Units)>=gmv)
#1604389

#step 7 check for Invalid Units
sum(ecom$Units<0) #0
sum(ecom$Units==0) #0

#step 8 paymentmode
unique(ecom$payment_mode)
#COD and "Prepaid"

#SLA
unique(ecom$SLA)
# for now we have taken 15 as max for delivery
(sum(ecom$SLA>15)/nrow(ecom))*100  #.2%
(sum(ecom$SLA<=0)/nrow(ecom))*100  #.36%
ecom$SLA[which(ecom$SLA>15)] <- 15
ecom$SLA[which(ecom$SLA<=0)] <- 1
#ecom <- subset(ecom,SLA<=15 & SLA>0)

#Splitting the data.
GamingAccessory <-ecom[ecom$P_sub_category=="GamingAccessory" ,] 
CameraAccessory<-ecom[ecom$P_sub_category=="CameraAccessory",]
HomeAudio<-ecom[ecom$P_sub_category=="HomeAudio",]

##############################################################
## KPI FUNCTION FOR ALL THE ABOVE 3 CATEGORIES ##
##############################################################

prepareKPIs<-function(dataset)
{
  #1. KPI - List price for all the products
  dataset$list_price <- dataset$gmv/dataset$Units
  
  #2. KP - Promotional Offer for all the products
  dataset$promotional_offer <- (dataset$MRP-dataset$list_price)/dataset$MRP
  
  #3. Clustering
  ## Creating a new KPI (though not used in the final model)
  ## It divides the products into three categories based on MRP and num units sold - 
  ## mass market, medium market and premium product
  dataset$P_analytic_vertical <- factor(dataset$P_analytic_vertical)
  cluster<- aggregate(cbind(Units,list_price,MRP)~P_analytic_vertical,dataset,mean)
  
  if(nrow(cluster)<=3){
    cluster$p_tag <-NA
    cluster$P_tag[which(cluster$MRP>=mean(cluster$MRP))] <- "Middle_p"
    cluster$P_tag[-which(cluster$MRP>=mean(cluster$MRP))] <- "Mass_p"
    cluster <- cluster[,-c(2:4)]
    dataset <-merge(dataset,cluster,by="P_analytic_vertical")
  } else {
    cluster$list_price_1 <- scale(cluster$list_price)
    cluster$MRP_1<- scale(cluster$MRP)
    cluster$Units_1 <- scale(cluster$Units)
    k1 <- cluster[,-c(2:4)]
    clust <- kmeans(k1[,-1], centers = 3,iter.max = 50,nstart = 50)
    cluster$P_tag <- as.factor(clust$cluster)
    cluster <- cluster[,c(1,8)]
    dataset <-merge(dataset,cluster,by=c("P_analytic_vertical"),all.x=TRUE)
    k2 <- table(dataset$P_tag)
    levels(dataset$P_tag)[which(k2==max(table(dataset$P_tag)))] <- "Mass_p"
    levels(dataset$P_tag)[which(k2==min(table(dataset$P_tag)))] <- "Premium_p"
    levels(dataset$P_tag)[which(k2!=max(table(dataset$P_tag))& k2!=min(table(dataset$P_tag)))] <- "Middle_p"
  }
  dataset$order_pay_ind<-ifelse(tolower(dataset$payment_mode)=="prepaid",1,0)
  dataset$order_pay_ind <- as.numeric(dataset$order_pay_ind)
  summary(dataset$order_pay_ind)
  
  #4.Add (Online_order/Total Order) KPI

  total_order <-aggregate(order_pay_ind ~ week_year,data=dataset,FUN=NROW)
  Online_order<-aggregate(order_pay_ind ~ week_year,data=dataset,FUN=sum)
  merged <-merge(total_order,Online_order,by=c("week_year"),all.x=TRUE)
  merged$per_order <- merged$order_pay_ind.y/merged$order_pay_ind.x
  merged <- merged[,-c(2,3)]
  dataset<- merge(dataset,merged,by=c("week_year"),all.x=TRUE)
  dataset$P_sub_category <- NULL
  dataset$P_super_category <- NULL
  dataset$P_analytic_category <- NULL
  
  #5.NPS score
  nps<-read.csv("nps.csv", stringsAsFactors = F)
  nps<-read.csv("nps.csv",h=T)
  nps$Month<-as.character(nps$Month)
  dataset<-merge(dataset,nps,by=c("Month","Year"),all.x=TRUE)
  
  #6.Holidays List
  holiday_list<-c("2015-07-18","2015-07-19","2015-08-15",
                  "2015-08-16","2015-08-17","2015-08-28",
                  "2015-08-29","2015-08-30","2015-10-15",
                  "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10",
                  "2015-10-11","2015-10-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26",
                  "2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
                  "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02",
                  "2016-02-20","2016-02-21","2016-02-14","2016-02-15","2016-03-07","2016-03-08",
                  "2016-03-09","2016-05-25","2016-05-26","2016-05-27")
  holiday_list <- as.Date(holiday_list)
  week_year <- week(holiday_list)
  year <- year(holiday_list)
  holiday <- data.frame(week_year,year)
  holiday$week_year<- ifelse(holiday$week_year<=26 & holiday$year==2016,holiday$week_year+53,holiday$week_year)
  holiday$year <-NULL
  holiday$holiday_freq <- 1
  holiday <- aggregate( holiday_freq ~ week_year,holiday,sum)
  
  products <- as.data.frame.matrix(t(table(dataset$P_tag,dataset$week_year)))
  products$week_year <- row.names(products)
  
  dataset_1 <- aggregate(gmv~week_year,dataset,sum)
  dataset<- aggregate(cbind(list_price,MRP,Units,SLA,promotional_offer,Procurement_SLA,per_order,NPS)~ week_year,data=dataset,FUN = mean)
  dataset <- merge(dataset,products,by="week_year",all.x=TRUE)
  dataset <- merge(dataset,holiday,by="week_year",all.x=TRUE)
  dataset$holiday_freq[is.na(dataset$holiday_freq)] <-0
  dataset <- merge(dataset,dataset_1,by="week_year",all.x=TRUE)
  return(dataset) 
}

GamingAccessory<- prepareKPIs(GamingAccessory)
HomeAudio <-prepareKPIs(HomeAudio)
CameraAccessory <- prepareKPIs(CameraAccessory)

# Merging Adstock data for each of the variables.
Adstock <- read.csv("Final_adstock.csv")
GamingAccessory <- merge(GamingAccessory , Adstock,by.x = "week_year")
HomeAudio <- merge(HomeAudio , Adstock,by.x = "week_year")
CameraAccessory <- merge(CameraAccessory , Adstock,by.x = "week_year")


#Creating moving average variables

advanced_kpi <- function(dataset)
{
  function1 = function(x) rollmean(x, k = 2, fill = NA, align = "right")
  function2 = function(x) rollmean(x, k = 3, fill = NA, align = "right")
  myfun3 = function(x) rollmean(x, k = 4, fill = NA, align = "right")
 
  x=dataset[,c("week_year","list_price","promotional_offer")]
  x1<-x %>% mutate_each(funs(function1),list_price,promotional_offer) %>% data.frame()
  x2<-x %>% mutate_each(funs(function2),list_price,promotional_offer) %>% data.frame()
  x3<-x %>% mutate_each(funs(myfun3),list_price,promotional_offer) %>% data.frame()
  
  x1$LP_MA1<-(x1$list_price)
  x1$PO_MA1<-(x1$promotional_offer)
  
  x2$LP_MA2<-(x2$list_price)
  x2$PO_MA2<-(x2$promotional_offer)
  
  x3$LP_MA3<-(x3$list_price)
  x3$PO_MA3<-(x3$promotional_offer)
  
  x4=cbind(x1[,-c(2:3)],x2[,-c(1:3)],x3[,-c(1:3)])
  dataset<-merge(dataset,x4,by="week_year")
  
  dataset$inc_LP_MA1<-(dataset$list_price - dataset$LP_MA1)/dataset$LP_MA1
  dataset$inc_LP_MA2<-(dataset$list_price - dataset$LP_MA2)/dataset$LP_MA2
  dataset$inc_LP_MA3<-(dataset$list_price - dataset$LP_MA3)/dataset$LP_MA3
  
  dataset$inc_PO_MA1<-(dataset$promotional_offer - dataset$PO_MA1)/dataset$PO_MA1
  dataset$inc_PO_MA2<-(dataset$promotional_offer - dataset$PO_MA2)/dataset$PO_MA2
  dataset$inc_PO_MA3<-(dataset$promotional_offer - dataset$PO_MA3)/dataset$PO_MA3
  
  dataset$LP_MA1<-NULL
  dataset$LP_MA2<-NULL
  dataset$LP_MA3<-NULL
  dataset$PO_MA1<-NULL
  dataset$PO_MA2<-NULL
  dataset$PO_MA3<-NULL
  
  names(dataset)[22:27]<-c("inc_LP_MA1","inc_LP_MA2","inc_LP_MA3","inc_PO_MA1","inc_PO_MA2",
                           "inc_PO_MA3")

  #List of list price by 1,2,3 dates (Date values are ordered)
  #Previous List price
  data_dum <- slide(dataset, Var = "list_price",slideBy = -1)
  data_dum <- slide(data_dum, Var = "list_price",slideBy = -2)
  data_dum <- slide(data_dum, Var = "list_price", slideBy = -3)
  
  #9.lag the promotion variables
  
  data_dum <- slide(data_dum, Var = "promotional_offer", slideBy = -1)
  data_dum <- slide(data_dum, Var = "promotional_offer", slideBy = -2)
  data_dum <- slide(data_dum, Var = "promotional_offer", slideBy = -3)
  
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -1)
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -2)
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -3)
  
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -1)
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -2)
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -3)
  
  dataset <- na.omit(data_dum) 
  return(dataset)
}

Game_final <- advanced_kpi(GamingAccessory)
Home_final <- advanced_kpi(HomeAudio)
Camera_final <- advanced_kpi(CameraAccessory)

EDA_graph_plotting <- function(dataset,name){

  plot1 <- ggplot(dataset,aes(TV,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "TV AdStock", y = "GMV")
  plot1
  
  plot2 <- ggplot(dataset,aes(Digital,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Digital AdStock", y = "GMV")
  plot2
  
  plot3 <- ggplot(dataset,aes(Sponsorship,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Sponsorship AdStock", y = "GMV")
  plot3
  
  plot4 <- ggplot(dataset,aes(Content.Marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Content Marketing AdStock", y = "GMV")
  plot4
  
  plot5 <- ggplot(dataset,aes(Online_marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Online Marketing AdStock", y = "GMV")
  plot5
  
  plot6 <- ggplot(dataset,aes(Affiliates,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Affiliates AdStock", y = "GMV")
  plot6
  
  plot7 <- ggplot(dataset,aes(SEM,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "SEM AdStock", y = "GMV")
  plot7
  
  plot8 <- ggplot(dataset,aes(dataset$week_year,dataset$gmv, fill = as.factor(ifelse(dataset$holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Holiday_flag", x = "Week", y = "GMV") + ggtitle(name) 
  plot8
  
  plot_grid(plot1,plot2,plot3,plot4,plot5,plot6,plot7, labels = "AUTO", ncol = 4, align = 'v')

}
EDA_graph_plotting(Game_final,"Gaming Accessory")
EDA_graph_plotting(Camera_final,"Camera")
EDA_graph_plotting(Home_final,"Home Audio")

# Build the Basic Linear regression model
#Camera dataset Liner Regression model building.
Camera_rep<- rep(0,5)
Linear_model <- Camera_final
Linear_model <- Linear_model[,-c(1:4,10:12,28:39)]
Linear_model <- scale(Linear_model)
Linear_model <- data.frame(Linear_model)
Lmodel1 <- lm(gmv~.,Linear_model)
summary(Lmodel1)

Lmodel2 <- stepAIC(Lmodel1,direction = "both")
summary(Lmodel2) 
vif(Lmodel2)

#SEM variable vif value is 150.838975 which is too large hence removing the variable. 
Lmodel3 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                per_order + NPS + TV + Digital + Sponsorship + Online_marketing + 
                inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, 
              data = Linear_model)

summary(Lmodel3) 
vif(Lmodel3)

#inc_PO_MA2 has high vif value and also when we check the summary the pr value is also high 
#hence removing the variables and building further model.
Lmodel4 <-  lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                 per_order + NPS + TV + Digital + Sponsorship + Online_marketing + 
                 inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3, 
               data = Linear_model)

summary(Lmodel4) 
vif(Lmodel4)

#Here NPS value is 12.50 which is highest and pr value is also high hence removing this variable.
Lmodel5 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                per_order + TV + Digital + Sponsorship + Online_marketing + 
                inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3, 
              data = Linear_model)

summary(Lmodel5) 
vif(Lmodel5)

#Here promotional_offer variable vif value is 10.44 and also the pr value is high, so eliminating this variable.
Lmodel6 <- lm(formula = gmv ~ SLA + Procurement_SLA + 
                per_order + TV + Digital + Sponsorship + Online_marketing + 
                inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3, 
              data = Linear_model)

summary(Lmodel6) 
vif(Lmodel6)

#inc_PO_MA3 has a vif value 5.02 and high pr value hence removing this variable.
Lmodel7 <- lm(formula = gmv ~ SLA + Procurement_SLA + 
                per_order + TV + Digital + Sponsorship + Online_marketing + 
                inc_LP_MA2 + inc_PO_MA1,data = Linear_model)

summary(Lmodel7)
vif(Lmodel7)

#We can see that inc_LP_MA2 vif value is 3.26 which is high so removing it from model.
Lmodel8 <- lm(formula = gmv ~ SLA + Procurement_SLA + 
                per_order + TV + Digital + Sponsorship + Online_marketing + 
                inc_PO_MA1,data = Linear_model)
summary(Lmodel8)
vif(Lmodel8)

Lmodel9 <- lm(formula = gmv ~ SLA + Procurement_SLA + 
                TV + Digital + Sponsorship + Online_marketing + 
                inc_PO_MA1,data = Linear_model)
summary(Lmodel9)
vif(Lmodel9)

#TV variable has high pr value hence removing it in model.
Lmodel10 <- lm(formula = gmv ~ SLA + Procurement_SLA + 
                 Digital + Sponsorship + Online_marketing + 
                 inc_PO_MA1,data = Linear_model)
summary(Lmodel10)
vif(Lmodel10)

Lmodel11 <- lm(formula = gmv ~ SLA + Digital + Sponsorship + Online_marketing + inc_PO_MA1, 
               data = Linear_model)
summary(Lmodel11)

Lmodel12<-  lm(formula = gmv ~ SLA + Digital + Sponsorship + inc_PO_MA1, 
               data = Linear_model)
summary(Lmodel12)

LinearRegression_Finalmodel_Camera <- Lmodel12

# Adj R square  = 0.348   with 4 variables
crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ SLA + Digital + Sponsorship + inc_PO_MA1),m = 10)
Camera_rep[1] <- attr(crossval, "ms")

# Elasticity Analysis

train <- Linear_model
grlm <- LinearRegression_Finalmodel_Camera

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() + ggtitle("Camera Accessory - Linear Model") +xlab("Variables")

##Game Accessory dataset Liner Regression model building.
Game_rep<- rep(0,5)
Linear_model <- Game_final
Linear_model <- Linear_model[,-c(1:4,10:12,28:39)]
Linear_model <- scale(Linear_model)
Linear_model <-data.frame(Linear_model)
Lmodel1 <- lm(gmv~.,Linear_model)
summary(Lmodel1)

Lmodel2 <- stepAIC(Lmodel1,direction = "both")
summary(Lmodel2) 
vif(Lmodel2)

#Digital variable has vif value of 66 hence removing it.
Lmodel3 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                Sponsorship + Content.Marketing + SEM + inc_LP_MA1 + inc_LP_MA2 + 
                inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, data = Linear_model)
summary(Lmodel3) 
vif(Lmodel3)

#Content.Marketing has a high vif value of 26.25 hence removing it.
Lmodel4 <-  lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                 Sponsorship + SEM + inc_LP_MA1 + inc_LP_MA2 + 
                 inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, data = Linear_model)
summary(Lmodel4) 
vif(Lmodel4)

#inc_LP_MA2 has a high vif value of 15.37 so removing it.
Lmodel5 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                Sponsorship + SEM + inc_LP_MA1 + inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, data = Linear_model)
summary(Lmodel5) 
vif(Lmodel5)

#inc_PO_MA3 has a high vif value of 8.20 hence removing it.
Lmodel6 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                Sponsorship + SEM + inc_LP_MA1 + inc_LP_MA3 + inc_PO_MA2, data = Linear_model)
summary(Lmodel6) 
vif(Lmodel6)

#inc_LP_MA1 has a high pr value of 0.991 hence removing it.
Lmodel7 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                Sponsorship + SEM  + inc_LP_MA3 + inc_PO_MA2, data = Linear_model)
summary(Lmodel7)
vif(Lmodel7)

#Sponsorship has high vif value and pr value hence removing it.
Lmodel8 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + SEM  + inc_LP_MA3 + 
                inc_PO_MA2, data = Linear_model)
summary(Lmodel8)
vif(Lmodel8)

#TV variable has high pr value and hence removing it.
Lmodel9 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + SEM  + inc_LP_MA3 + 
                inc_PO_MA2, data = Linear_model)
summary(Lmodel9)
vif(Lmodel9)

Lmodel10 <- lm(formula = gmv ~ Procurement_SLA + NPS + SEM  + inc_LP_MA3 +inc_PO_MA2, data = Linear_model)
summary(Lmodel10)


Lmodel11 <- lm(formula = gmv ~ NPS + SEM  + inc_LP_MA3 +inc_PO_MA2, data = Linear_model)
summary(Lmodel11)
vif(Lmodel11)

LinearRegression_Finalmodel_Game <- Lmodel11

# Adj R square  = 0.404  with 4 variables
crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ NPS + SEM  + inc_LP_MA3 +inc_PO_MA2),m = 10)
Game_rep[1] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Linear_model
grlm <- LinearRegression_Finalmodel_Game

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() + ggtitle("Game Accessory - Linear Model") +xlab("Variables")

##Home Accessory dataset Liner Regression model building.
Home_rep<- rep(0,5)

Linear_model <- Home_final
Linear_model <- Linear_model[,-c(1:4,10:12,28:39)]
Linear_model <- scale(Linear_model)
Linear_model <-data.frame(Linear_model)
Lmodel1 <- lm(gmv~.,Linear_model)
summary(Lmodel1)

Lmodel2 <- stepAIC(Lmodel1,direction = "both")
summary(Lmodel1) 
vif(Lmodel2)

#Online_marketing has high vif value hence removing it.
Lmodel3 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + holiday_freq + 
                TV + Sponsorship + Content.Marketing + 
                Affiliates + SEM + inc_PO_MA1 + inc_PO_MA3, data = Linear_model)
summary(Lmodel3) 
vif(Lmodel3)

#SEM variable has high vif value of 24.89 and high pr value of 0.11 value.
Lmodel4 <-  lm(formula = gmv ~ Procurement_SLA + per_order + NPS + holiday_freq + 
                 TV + Sponsorship + Content.Marketing + 
                 Affiliates + inc_PO_MA1 + inc_PO_MA3, data = Linear_model)
summary(Lmodel4) 
vif(Lmodel4)

#NPS variable has high vif value of 5.82 and high pr value of 0.170 hence removing it.
Lmodel5 <-  lm(formula = gmv ~ Procurement_SLA + per_order + holiday_freq + 
                 TV + Sponsorship + Content.Marketing + 
                 Affiliates + inc_PO_MA1 + inc_PO_MA3, data = Linear_model)
summary(Lmodel5) 
vif(Lmodel5)

#Content.Marketing variable has high high vif value of 3.46 and high pr values of 0.54
Lmodel6 <-  lm(formula = gmv ~ Procurement_SLA + per_order + holiday_freq + 
                 TV + Sponsorship + Affiliates + inc_PO_MA1 + inc_PO_MA3, data = Linear_model)
summary(Lmodel6) 
vif(Lmodel6)

#TV has high pr value hence removing it.
Lmodel7 <-  lm(formula = gmv ~ Procurement_SLA + per_order + holiday_freq + 
                 Sponsorship + Affiliates + inc_PO_MA1 + inc_PO_MA3, data = Linear_model)
summary(Lmodel7)
vif(Lmodel7)

#removing inc_PO_MA1 has high pr value 
Lmodel8 <-  lm(formula = gmv ~ Procurement_SLA + per_order + holiday_freq + 
                 Sponsorship + Affiliates + inc_PO_MA3, data = Linear_model)
summary(Lmodel8)
vif(Lmodel8)

Lmodel9 <-  lm(formula = gmv ~  per_order + holiday_freq + 
                 Sponsorship + Affiliates + inc_PO_MA3, data = Linear_model)
summary(Lmodel9)
vif(Lmodel9)

Lmodel10 <-  lm(formula = gmv ~   holiday_freq + 
                  Sponsorship + Affiliates + inc_PO_MA3, data = Linear_model)
summary(Lmodel10)
vif(Lmodel10)

Lmodel11 <-  lm(formula = gmv ~   holiday_freq + Sponsorship + inc_PO_MA3, data = Linear_model)
summary(Lmodel11)
vif(Lmodel11)

Lmodel12 <- lm(formula = gmv ~ holiday_freq + Sponsorship, data = Linear_model)
summary(Lmodel12)
vif(Lmodel12)

LinearRegression_Finalmodel_Home <- Lmodel12

# Adj R square  = 0.306  with 2 variables
crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ holiday_freq + Sponsorship),m = 10)
Home_rep[1] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Linear_model
grlm <- LinearRegression_Finalmodel_Home

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Linear Model") +xlab("Variables")

#Camera Accessory dataset Multiplicative Linear Regression model building
Camera_multi <- Camera_final
Camera_multi <- Camera_multi[,-c(1:4,10:12,13,22:39)]
Camera_multi$Content.Marketing[which(Camera_multi$Content.Marketing==0)] <- 0.01
Camera_multi$per_order[which(Camera_multi$per_order==0)] <- 0.01
Camera_multi <- log(Camera_multi)

mmodel1 <- lm(gmv~.,Camera_multi)
summary(mmodel1)

mmodel2 <- stepAIC(mmodel1,direction = "both")
summary(mmodel2)
vif(mmodel2)

#Online_marketing variable has vif high value with 820.48.
mmodel3 <- lm(formula = gmv ~ per_order + NPS + TV + Digital + Affiliates + SEM, data = Camera_multi)
summary(mmodel3) 
vif(mmodel3)

#Affiliates variable has high vif value of 20.88.
mmodel4 <- lm(formula = gmv ~ per_order + NPS + TV + Digital + SEM, data = Camera_multi)
summary(mmodel4) 
vif(mmodel4)

#SEM variable has high vif value of 8.68
mmodel5 <- lm(formula = gmv ~ per_order + NPS + TV + Digital, data = Camera_multi)
summary(mmodel5) 
vif(mmodel5)

#TV variable has high vif and pr values of 3.35 and 0.65.
mmodel6 <- lm(formula = gmv ~ per_order + NPS + Digital, data = Camera_multi)
summary(mmodel6) 
vif(mmodel6)

#Digital has very high pr value.
mmodel7 <- lm(formula = gmv ~ per_order + NPS, data = Camera_multi)
summary(mmodel7) 
vif(mmodel7)

MultiLinearRegression_Finalmodel_Camera <- mmodel7
#Adj R-Squared = 0.834  with 2 significant variables 
crossval <- cv.lm(data = Camera_multi, form.lm = formula(gmv ~ per_order + NPS),m = 10)
Camera_rep[2] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Camera_multi
grlm <- MultiLinearRegression_Finalmodel_Camera

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables")

#Game Accessory dataset Multiplicative Linear Regression model building
Game_multi <- Game_final
Game_multi <- Game_multi[,-c(1:4,10:12,13,22:39)]
Game_multi$Content.Marketing[which(Game_multi$Content.Marketing==0)] <- 0.01
Game_multi$per_order[which(Game_multi$per_order==0)] <- 0.01
Game_multi <- log(Game_multi)
mmodel1 <- lm(gmv~.,Game_multi)
summary(mmodel1)

mmodel2 <- stepAIC(mmodel1,direction = "both")
summary(mmodel2)
vif(mmodel2)

#Online_marketing has high vif value.
mmodel3 <- lm(formula = gmv ~ SLA + per_order + NPS + TV + Sponsorship + Content.Marketing + 
                Affiliates, data = Game_multi)
summary(mmodel3) 
vif(mmodel3)

#Affiliates has high vif value.
mmodel4 <- lm(formula = gmv ~ SLA + per_order + NPS + TV + Sponsorship + Content.Marketing, data = Game_multi)
summary(mmodel4) 
vif(mmodel4)

#NPS has high vif value.
mmodel5 <- lm(formula = gmv ~ SLA + per_order + TV + Sponsorship + Content.Marketing, data = Game_multi)
summary(mmodel5) 
vif(mmodel5)

#Content.Marketing has a high pr value.
mmodel6 <- lm(formula = gmv ~ SLA + per_order + TV + Sponsorship, data = Game_multi)
summary(mmodel6) 
vif(mmodel6)

#SLA has a high pr value.
mmodel7 <- lm(formula = gmv ~ per_order + TV + Sponsorship, data = Game_multi)
summary(mmodel7) 
vif(mmodel7)

#Sponsorship has a high pr value.
mmodel8 <- lm(formula = gmv ~ per_order + TV, data = Game_multi)
summary(mmodel8) 
vif(mmodel8)

MultiLinearRegression_Finalmodel_Game <- mmodel8

#Adj R2 = 0.567  with 2 significant variables 
crossval <- cv.lm(data = Game_multi, form.lm = formula(gmv ~ per_order + TV),m = 10)
Game_rep[2] <- attr(crossval, "ms")

# Elasticity Analysis# 
train <- Game_multi
grlm <- MultiLinearRegression_Finalmodel_Game

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative Model") +xlab("Variables")

#Home Accessory dataset Multiplicative Linear Regression model building
Home_multi <- Home_final
Home_multi <- Home_multi[,-c(1:4,10:12,13,22:39)]
Home_multi$Content.Marketing[which(Home_multi$Content.Marketing==0)] <- 0.01
Home_multi$per_order[which(Home_multi$per_order==0)] <- 0.01
Home_multi <- log(Home_multi)
mmodel1 <- lm(gmv~.,Home_multi)
summary(mmodel1)

mmodel2 <- stepAIC(mmodel1,direction = "both")
summary(mmodel2)
vif(mmodel2)

#Affiliates has high vif values so removing.
mmodel3 <- lm(formula = gmv ~ promotional_offer + NPS + TV + Digital + Online_marketing
              , data = Home_multi)
summary(mmodel3) 
vif(mmodel3)

#Online_marketing has high vif values so removing.
mmodel4 <- lm(formula = gmv ~ promotional_offer + NPS + TV + Digital, data = Home_multi)
summary(mmodel4) 
vif(mmodel4)


#NPS has high pr values so removing.
mmodel5 <- lm(formula = gmv ~ promotional_offer + TV + Digital, data = Home_multi)
summary(mmodel5) 
vif(mmodel5)


#TV has high pr values so removing.
mmodel6 <- lm(formula = gmv ~ promotional_offer + Digital, data = Home_multi)
summary(mmodel6) 
vif(mmodel6)
MultiLinearRegression_Finalmodel_Home <- mmodel6

#Adj R2 = 0.259   with 2 significant variables 
crossval <- cv.lm(data = Home_multi, form.lm = formula(gmv ~ promotional_offer + Digital),m = 10)
Home_rep[2] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Home_multi
grlm <- MultiLinearRegression_Finalmodel_Home

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multiplicative Model") +xlab("Variables")

#Camera Accessory dataset Koyck models model building
Camera_koyck<- Camera_final[,-c(1:4,10:12,28:39)]
Camera_koyck <- slide(Camera_koyck, Var = "gmv",slideBy = -1)
Camera_koyck <- na.omit(Camera_koyck)
Camera_koyck <- scale(Camera_koyck)
Camera_koyck <- data.frame(Camera_koyck)
Koymodel1 <- lm(gmv~.,Camera_koyck)
summary(Koymodel1)

Koymodel2 <- stepAIC(Koymodel1,direction = "both")
summary(Koymodel2)  
vif(Koymodel2)

#SEM has too high vif value so removing it.
Koymodel3 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                  per_order + NPS + holiday_freq + TV + Digital + Sponsorship + 
                  Online_marketing + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA2 + 
                  inc_PO_MA3, data = Camera_koyck)
summary(Koymodel3)
vif(Koymodel3)

#inc_PO_MA2 has too high vif value so removing it
Koymodel4 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                  per_order + NPS + holiday_freq + TV + Digital + Sponsorship + 
                  Online_marketing + inc_LP_MA2 + inc_PO_MA1 + 
                  inc_PO_MA3, data = Camera_koyck)
summary(Koymodel4)
vif(Koymodel4)

#NPS has high vif value of 11.76 and high pr value of 0.687 hence removing it.
Koymodel5 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                  per_order + holiday_freq + TV + Digital + Sponsorship + 
                  Online_marketing + inc_LP_MA2 + inc_PO_MA1 + 
                  inc_PO_MA3, data = Camera_koyck)
summary(Koymodel5)
vif(Koymodel5)

#promotional_offer has high vif value hence removing it.
Koymodel6 <- lm(formula = gmv ~ SLA + Procurement_SLA + 
                  per_order + holiday_freq + TV + Digital + Sponsorship + 
                  Online_marketing + inc_LP_MA2 + inc_PO_MA1 + 
                  inc_PO_MA3, data = Camera_koyck)
summary(Koymodel6)
vif(Koymodel6)

#inc_PO_MA3 has high vif value of 5.15 hence removing it.
Koymodel7 <- lm(formula = gmv ~ SLA + Procurement_SLA + 
                  per_order + holiday_freq + TV + Digital + Sponsorship + 
                  Online_marketing + inc_LP_MA2 + inc_PO_MA1 
                , data = Camera_koyck)
summary(Koymodel7)
vif(Koymodel7)

#per_order has high pr value of 0.536 hence removing it.
Koymodel8 <- lm(formula = gmv ~ SLA + Procurement_SLA + holiday_freq + TV + Digital + Sponsorship + 
                  Online_marketing + inc_LP_MA2 + inc_PO_MA1 
                , data = Camera_koyck)
summary(Koymodel8)
vif(Koymodel8)

#holiday_freq has high pr value of 0.437 hence removing it.
Koymodel9 <- lm(formula = gmv ~ SLA + Procurement_SLA + TV + Digital + Sponsorship + 
                  Online_marketing + inc_LP_MA2 + inc_PO_MA1 
                , data = Camera_koyck)
summary(Koymodel9)
vif(Koymodel9)

#SLA has high pr value of 0.408 hence removing it.
Koymodel10 <- lm(formula = gmv ~ Procurement_SLA + TV + Digital + Sponsorship + 
                   Online_marketing + inc_LP_MA2 + inc_PO_MA1 
                 , data = Camera_koyck)
summary(Koymodel10)
vif(Koymodel10)

#TV has high pr value of 0.408 hence removing it.
Koymodel11 <- lm(formula = gmv ~ Procurement_SLA + Digital + Sponsorship + 
                   Online_marketing + inc_LP_MA2 + inc_PO_MA1 
                 , data = Camera_koyck)
summary(Koymodel11)
vif(Koymodel11)

#Procurement_SLA has high pr value of 0.2051 hence removing it.
Koymodel12 <- lm(formula = gmv ~ Digital + Sponsorship + Online_marketing + inc_LP_MA2 + inc_PO_MA1 
                 , data = Camera_koyck)
summary(Koymodel12)
vif(Koymodel12)

#Digital has high pr value of 0.0669 hence removing it.
Koymodel13 <- lm(formula = gmv ~  + Sponsorship + Online_marketing + inc_LP_MA2 + inc_PO_MA1 
                 , data = Camera_koyck)
summary(Koymodel13)
vif(Koymodel13)

#Online_marketing has high pr value of 0.0355 hence removing it.
Koymodel14 <- lm(formula = gmv ~  Sponsorship + inc_LP_MA2 + inc_PO_MA1, data = Camera_koyck)
summary(Koymodel14)
vif(Koymodel14)

koyckFinal_Model_camera <- Koymodel14 

# Adj R squared  = 0.515 with 3 significant variable 
crossval <- cv.lm(data = Camera_koyck, form.lm = formula(gmv ~ Sponsorship + inc_LP_MA2 + inc_PO_MA1),m = 10)
Camera_rep[3] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Camera_koyck
grlm <- koyckFinal_Model_camera

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Koyck Model") +xlab("Variables")

#Game Accessory dataset Koyck models model building
Game_koyck<- Game_final[,-c(1:4,10:12,28:39)]
Game_koyck <- slide(Game_koyck, Var = "gmv",slideBy = -1)
Game_koyck <- na.omit(Game_koyck)
Game_koyck <- scale(Game_koyck)
Game_koyck <- data.frame(Game_koyck)
Koymodel1 <- lm(gmv~.,Game_koyck)
summary(Koymodel1)

Koymodel2 <- stepAIC(Koymodel1,direction = "both")
summary(Koymodel2)  
vif(Koymodel2)

Koymodel3 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                  Digital + Sponsorship + Content.Marketing + inc_LP_MA2 + 
                  inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + gmv.1, 
                data = Game_koyck)
summary(Koymodel3)
vif(Koymodel3)

Koymodel4 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                  Digital + Sponsorship + Content.Marketing + inc_LP_MA2 + 
                  inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + gmv.1, 
                data = Game_koyck)
summary(Koymodel4)
vif(Koymodel4)

Koymodel5 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                  Digital + Sponsorship + inc_LP_MA2 + 
                  inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + gmv.1, 
                data = Game_koyck)
summary(Koymodel5)
vif(Koymodel5)

Koymodel6 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                  Digital + Sponsorship + 
                  inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + gmv.1, 
                data = Game_koyck)
summary(Koymodel6)
vif(Koymodel6)

Koymodel7 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                  Digital + Sponsorship + 
                  inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
                data = Game_koyck)
summary(Koymodel7)
vif(Koymodel7)

Koymodel8 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                  Digital + inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
                data = Game_koyck)
summary(Koymodel8)
vif(Koymodel8)

Koymodel9 <- lm(formula = gmv ~ SLA + per_order + NPS + Digital + inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
                data = Game_koyck)
summary(Koymodel9)
vif(Koymodel9)


Koymodel10 <- lm(formula = gmv ~ SLA + NPS + Digital + inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
                 data = Game_koyck)
summary(Koymodel10)


Koymodel11 <- lm(formula = gmv ~ SLA + NPS + inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
                 data = Game_koyck)
summary(Koymodel11)

Koymodel12 <- lm(formula = gmv ~ NPS + inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
                 data = Game_koyck)
summary(Koymodel12)

koyckFinal_Model_game <- Koymodel12 

# Adj R squared  = 0.406 with 4 significant variable 
crossval <- cv.lm(data = Game_koyck, form.lm = formula(gmv ~ NPS + inc_LP_MA3 + inc_PO_MA1 + gmv.1),m = 10)
Game_rep[3] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Game_koyck
grlm <- koyckFinal_Model_game

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Koyck Model") +xlab("Variables")

#Home Accessory dataset Koyck models model building
Home_koyck<- Home_final[,-c(1:4,10:12,28:39)]
Home_koyck <- slide(Home_koyck, Var = "gmv",slideBy = -1)
Home_koyck <- na.omit(Home_koyck)
Home_koyck <- scale(Home_koyck)
Home_koyck <- data.frame(Home_koyck)
Koymodel1 <- lm(gmv~.,Home_koyck)
summary(Koymodel1)

Koymodel2 <- stepAIC(Koymodel1,direction = "both")
summary(Koymodel2)  
vif(Koymodel2)

Koymodel3 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + holiday_freq + 
                  TV + Digital + Sponsorship + Content.Marketing + 
                  Affiliates + inc_PO_MA1 + inc_PO_MA3, data = Home_koyck)
summary(Koymodel3)
vif(Koymodel3)

Koymodel4 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + holiday_freq + 
                  TV + Digital + Sponsorship + 
                  Affiliates + inc_PO_MA1 + inc_PO_MA3, data = Home_koyck)
summary(Koymodel4)
vif(Koymodel4)

Koymodel5 <- lm(formula = gmv ~ Procurement_SLA + per_order + holiday_freq + 
                  TV + Digital + Sponsorship + 
                  Affiliates + inc_PO_MA1 + inc_PO_MA3, data = Home_koyck)
summary(Koymodel5)
vif(Koymodel5)

Koymodel6 <- lm(formula = gmv ~ Procurement_SLA + per_order + holiday_freq + 
                  TV + Digital + Sponsorship + Affiliates + inc_PO_MA3, data = Home_koyck)
summary(Koymodel6)
vif(Koymodel6)

Koymodel7 <- lm(formula = gmv ~ Procurement_SLA + per_order + holiday_freq + 
                  Digital + Sponsorship + Affiliates + inc_PO_MA3, data = Home_koyck)
summary(Koymodel7)
vif(Koymodel7)

Koymodel8 <- lm(formula = gmv ~ Procurement_SLA + per_order + holiday_freq + 
                  TV + inc_PO_MA3, data = Home_koyck)
summary(Koymodel8)
vif(Koymodel8)


Koymodel9 <- lm(formula = gmv ~ per_order + TV + Digital + 
                  inc_PO_MA3, data = Home_koyck)
summary(Koymodel9)
vif(Koymodel9)


Koymodel10 <- lm(formula = gmv ~ per_order +  Digital + inc_PO_MA3, data = Home_koyck)
summary(Koymodel10)
vif(Koymodel10)

Koymodel11 <- lm(formula = gmv ~ Digital + inc_PO_MA3, data = Home_koyck)
summary(Koymodel11)
vif(Koymodel11)

koyckFinal_Model_home <- Koymodel11 

# Adj R square  = 0.312 with 2 significant variable 
crossval <- cv.lm(data = Home_koyck, form.lm = formula(gmv ~ Digital + inc_PO_MA3),m = 10)
Home_rep[3] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Home_koyck
grlm <- koyckFinal_Model_home

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables")

#Camera Accessory dataset Distributed lag model building
Dis_Model <- Camera_final[,-c(1:4,10:12, 28:30)]
Dis_model_1 <- slide(Dis_Model, Var = "gmv",slideBy = -1)
Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -2)
Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -3)
Dis_model <- na.omit(Dis_model_1)
Dis_model <- scale(Dis_model)
Dis_model <- data.frame(Dis_model)

distmodel1 <- lm(gmv~.,Dis_model)
summary(distmodel1)

distmodel2 <- stepAIC(distmodel1,direction = "both")
summary(distmodel2)
vif(distmodel2)

distmodel3 <- lm(formula = gmv ~ SLA + promotional_offer + per_order + NPS + 
                   Digital + Sponsorship + Content.Marketing + Online_marketing + 
                   Affiliates + SEM + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                   promotional_offer.2 + promotional_offer.3 + 
                   NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel3)
vif(distmodel3)

distmodel4 <- lm(formula = gmv ~ SLA + promotional_offer + per_order + NPS + 
                   Digital + Sponsorship + Content.Marketing + 
                   Affiliates + SEM + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                   promotional_offer.2 + promotional_offer.3 + 
                   NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel4)
vif(distmodel4)

distmodel5 <- lm(formula = gmv ~ SLA + promotional_offer + per_order + NPS + 
                   Digital + Sponsorship + Content.Marketing + 
                   Affiliates + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                   promotional_offer.2 + promotional_offer.3 + 
                   NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel5)
vif(distmodel5)

distmodel6 <- lm(formula = gmv ~ SLA + promotional_offer + per_order + NPS + 
                   Digital + Sponsorship + Content.Marketing + 
                   Affiliates + inc_LP_MA3 + inc_PO_MA1 + 
                   promotional_offer.2 + promotional_offer.3 + 
                   NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel6)
vif(distmodel6)

distmodel7 <- lm(formula = gmv ~ SLA + promotional_offer + per_order + NPS + 
                   Digital + Sponsorship + 
                   Affiliates + inc_LP_MA3 + inc_PO_MA1 + 
                   promotional_offer.2 + promotional_offer.3 + 
                   NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel7)
vif(distmodel7)

distmodel8 <- lm(formula = gmv ~ SLA + promotional_offer + per_order + Digital + Sponsorship + 
                   Affiliates + inc_LP_MA3 + inc_PO_MA1 + 
                   promotional_offer.2 + promotional_offer.3 + 
                   NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel8)
vif(distmodel8)

distmodel9 <- lm(formula = gmv ~ SLA + per_order + Digital + Sponsorship + Affiliates + inc_LP_MA3 + 
                   inc_PO_MA1 + promotional_offer.2 + promotional_offer.3 + 
                   NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel9)
vif(distmodel9)

distmodel10 <- lm(formula = gmv ~ SLA + per_order + Sponsorship + Affiliates + inc_LP_MA3 + inc_PO_MA1 + 
                    promotional_offer.2 + promotional_offer.3 + 
                    NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel10)
vif(distmodel10)

distmodel11 <- lm(formula = gmv ~ SLA + per_order + Sponsorship + Affiliates + inc_LP_MA3 + inc_PO_MA1 
                  + promotional_offer.3 + NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel11)
vif(distmodel11)

distmodel12 <- lm(formula = gmv ~ SLA  + Sponsorship + Affiliates + inc_LP_MA3 + inc_PO_MA1 
                  + promotional_offer.3 + NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel12)
vif(distmodel12)

distmodel13 <- lm(formula = gmv ~ Sponsorship + Affiliates + inc_LP_MA3 + inc_PO_MA1 
                  + promotional_offer.3 + NPS.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel13)
vif(distmodel13)

distmodel14 <- lm(formula = gmv ~ Sponsorship + Affiliates + inc_LP_MA3 + inc_PO_MA1 
                  + promotional_offer.3 + holiday_freq.1 + holiday_freq.2 + gmv.2, data = Dis_model)
summary(distmodel14)
vif(distmodel14)

distmodel15 <- lm(formula = gmv ~ Sponsorship + Affiliates + inc_LP_MA3 + inc_PO_MA1 
                  + promotional_offer.3 + holiday_freq.1 + holiday_freq.2, data = Dis_model)
summary(distmodel15)
vif(distmodel15)

distmodel16 <- lm(formula = gmv ~ Sponsorship + Affiliates + inc_LP_MA3 + inc_PO_MA1 
                  + promotional_offer.3 + holiday_freq.1, data = Dis_model)
summary(distmodel16)
vif(distmodel16)

distmodel17 <- lm(formula = gmv ~ Sponsorship + Affiliates + inc_LP_MA3 + inc_PO_MA1 
                  + holiday_freq.1, data = Dis_model)
summary(distmodel17)
vif(distmodel17)

distmodel18 <- lm(formula = gmv ~ Sponsorship + inc_LP_MA3 + inc_PO_MA1 
                  + holiday_freq.1, data = Dis_model)
summary(distmodel18)
vif(distmodel18)

distmodel19 <- lm(formula = gmv ~ Sponsorship + inc_LP_MA3 + inc_PO_MA1, data = Dis_model)
summary(distmodel19)
vif(distmodel19)

Dist_Final_Model_camera <- distmodel19
# Adj R square  = 0.495 with 3 significant variables
crossval <- cv.lm(data = Dis_model, form.lm = formula(gmv ~ Sponsorship + inc_LP_MA3 + inc_PO_MA1),m = 10)
Camera_rep[4] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Dis_model
grlm <- Dist_Final_Model_camera

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Distributed Lag Model") +xlab("Variables")

#Game Accessory dataset Distributed lag model building
Dis_Model <- Game_final[,-c(1:4,10:12, 28:30)]
Dis_model_1 <- slide(Dis_Model, Var = "gmv",slideBy = -1)
Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -2)
Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -3)
Dis_model <- na.omit(Dis_model_1)
Dis_model <- scale(Dis_model)
Dis_model <- data.frame(Dis_model)

distmodel1 <- lm(gmv~.,Dis_model)
summary(distmodel1)

distmodel2 <- stepAIC(distmodel1,direction = "both")
summary(distmodel2)
vif(distmodel2)

distmodel3 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                   Sponsorship + Content.Marketing + Online_marketing + Affiliates + 
                   SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                   inc_PO_MA3 + promotional_offer.1 + promotional_offer.2 + 
                   promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                   gmv.2 + gmv.3, data = Dis_model)
summary(distmodel3)
vif(distmodel3)

distmodel4 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                   Sponsorship + Content.Marketing + Online_marketing + Affiliates + 
                   SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                   promotional_offer.1 + promotional_offer.2 + 
                   promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                   gmv.2 + gmv.3, data = Dis_model)
summary(distmodel4)
vif(distmodel4)

distmodel5 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                   Sponsorship + Content.Marketing + Online_marketing + Affiliates + 
                   SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + promotional_offer.1 + promotional_offer.2 + 
                   promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                   gmv.2 + gmv.3, data = Dis_model)
summary(distmodel5)
vif(distmodel5)

distmodel6 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                   Sponsorship + Content.Marketing + Online_marketing + 
                   SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + promotional_offer.1 + promotional_offer.2 + 
                   promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                   gmv.2 + gmv.3, data = Dis_model)
summary(distmodel6)
vif(distmodel6)

distmodel7 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                   Sponsorship + Online_marketing + SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
                   promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                   gmv.2 + gmv.3, data = Dis_model)
summary(distmodel7)
vif(distmodel7)

distmodel8 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                   Sponsorship + Online_marketing + SEM + inc_LP_MA1 + inc_LP_MA3 + 
                   promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                   gmv.2 + gmv.3, data = Dis_model)
summary(distmodel8)
vif(distmodel8)

distmodel9 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                   Sponsorship + Online_marketing + SEM + inc_LP_MA1 + inc_LP_MA3 + 
                   promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.3 + holiday_freq.2 + gmv.1 + 
                   gmv.2 + gmv.3, data = Dis_model)
summary(distmodel9)
vif(distmodel9)

distmodel10 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                    Sponsorship + Online_marketing + SEM + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.3 + holiday_freq.2 + gmv.1 + 
                    gmv.2 + gmv.3, data = Dis_model)
summary(distmodel10)
vif(distmodel10)

distmodel11 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                    Sponsorship + Online_marketing + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.3 + holiday_freq.2 + gmv.1 + 
                    gmv.2 + gmv.3, data = Dis_model)
summary(distmodel11)
vif(distmodel11)

distmodel12 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                    Sponsorship + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.3 + holiday_freq.2 + gmv.1 + 
                    gmv.2 + gmv.3, data = Dis_model)
summary(distmodel12)
vif(distmodel12)

distmodel13 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                    Sponsorship + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                    gmv.2 + gmv.3, data = Dis_model)
summary(distmodel13)
vif(distmodel13)

distmodel14 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                    Sponsorship + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + holiday_freq.2 + gmv.1 + 
                    gmv.2 + gmv.3, data = Dis_model)
summary(distmodel14)
vif(distmodel14)

distmodel15 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                    Sponsorship + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + holiday_freq.2 + gmv.1 + 
                    gmv.2, data = Dis_model)
summary(distmodel15)
vif(distmodel15)

distmodel16 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                    Sponsorship + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + gmv.1 + gmv.2, data = Dis_model)
summary(distmodel16)
vif(distmodel16)

distmodel17 <- lm(formula = gmv ~ promotional_offer + holiday_freq + Sponsorship + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + gmv.1 + gmv.2, data = Dis_model)
summary(distmodel17)
vif(distmodel17)

distmodel18 <- lm(formula = gmv ~ promotional_offer + holiday_freq + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + gmv.1 + gmv.2, data = Dis_model)
summary(distmodel18)
vif(distmodel18)

distmodel19 <- lm(formula = gmv ~ promotional_offer + inc_LP_MA1 + 
                    promotional_offer.1 + promotional_offer.2 + gmv.1 + gmv.2, data = Dis_model)
summary(distmodel19)
vif(distmodel19)

distmodel20 <- lm(formula = gmv ~ promotional_offer + inc_LP_MA1 + promotional_offer.1 + gmv.1 + gmv.2, data = Dis_model)
summary(distmodel20)
vif(distmodel20)

distmodel21 <- lm(formula = gmv ~ promotional_offer + inc_LP_MA1 + promotional_offer.1 + gmv.1, data = Dis_model)
summary(distmodel21)
vif(distmodel21)

Dist_Final_Model_game <- distmodel21

# Adj R squared  = 0.473 with 4 significant variables
crossval <- cv.lm(data = Dis_model, form.lm = formula(gmv ~ promotional_offer + inc_LP_MA1 + promotional_offer.1 + gmv.1),m = 10)
Game_rep[4] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Dis_model
grlm <- Dist_Final_Model_game

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Distributed Lag Model") +xlab("Variables")

#Home Accessory dataset Distributed lag model building
Dis_Model <- Home_final[,-c(1:4,10:12, 28:30)]
Dis_model_1 <- slide(Dis_Model, Var = "gmv",slideBy = -1)
Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -2)
Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -3)
Dis_model <- na.omit(Dis_model_1)
Dis_model <- scale(Dis_model)
Dis_model <- data.frame(Dis_model)

distmodel1 <- lm(gmv~.,Dis_model)
summary(distmodel1)

distmodel2 <- stepAIC(distmodel1,direction = "both")
summary(distmodel2)
vif(distmodel2)

distmodel3 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                   per_order + NPS + holiday_freq + TV + Digital + Sponsorship + 
                   Content.Marketing + Online_marketing + Affiliates + inc_LP_MA2 + 
                   inc_LP_MA3 + inc_PO_MA2 + promotional_offer.1 + 
                   promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                   holiday_freq.2 + holiday_freq.3 + gmv.3, data = Dis_model)
summary(distmodel3)
vif(distmodel3)

distmodel4 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                   per_order + NPS + holiday_freq + TV + Digital + Sponsorship + 
                   Content.Marketing + Online_marketing + Affiliates + inc_LP_MA2 + 
                   inc_LP_MA3 + promotional_offer.1 + 
                   promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                   holiday_freq.2 + holiday_freq.3 + gmv.3, data = Dis_model)
summary(distmodel4)
vif(distmodel4)

distmodel5 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                   per_order + NPS + holiday_freq + TV + Digital + Sponsorship + 
                   Content.Marketing + Online_marketing + inc_LP_MA2 + 
                   inc_LP_MA3 + promotional_offer.1 + 
                   promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                   holiday_freq.2 + holiday_freq.3 + gmv.3, data = Dis_model)
summary(distmodel5)
vif(distmodel5)

distmodel6 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                   per_order + NPS + holiday_freq + TV + Digital + Sponsorship + 
                   Content.Marketing + Online_marketing + inc_LP_MA2 + 
                   promotional_offer.1 + 
                   promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                   holiday_freq.2 + holiday_freq.3 + gmv.3, data = Dis_model)
summary(distmodel6)
vif(distmodel6)

distmodel7 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                   per_order + NPS + holiday_freq + TV + Digital + Sponsorship + 
                   Online_marketing + inc_LP_MA2 + 
                   promotional_offer.1 + 
                   promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                   holiday_freq.2 + holiday_freq.3 + gmv.3, data = Dis_model)
summary(distmodel7)
vif(distmodel7)

distmodel8 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                   per_order + NPS + holiday_freq + TV + Digital + Sponsorship + inc_LP_MA2 + 
                   promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                   holiday_freq.2 + holiday_freq.3 + gmv.3, data = Dis_model)
summary(distmodel8)
vif(distmodel8)

distmodel9 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                   per_order + NPS + holiday_freq + TV + Sponsorship + inc_LP_MA2 + 
                   promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                   holiday_freq.2 + holiday_freq.3 + gmv.3, data = Dis_model)
summary(distmodel9)
vif(distmodel9)

distmodel10 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                    per_order + NPS + holiday_freq + TV + Sponsorship + inc_LP_MA2 + 
                    promotional_offer.1 + promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.2 + holiday_freq.3, data = Dis_model)
summary(distmodel10)
vif(distmodel10)

distmodel11 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                    per_order + NPS + holiday_freq + TV + Sponsorship + inc_LP_MA2 + promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.2 + holiday_freq.3, data = Dis_model)
summary(distmodel11)
vif(distmodel11)

distmodel12 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                    per_order + NPS + holiday_freq + TV + Sponsorship + inc_LP_MA2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.2 + holiday_freq.3, data = Dis_model)
summary(distmodel12)
vif(distmodel12)

distmodel13 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                    per_order + NPS + holiday_freq + TV + Sponsorship + inc_LP_MA2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.3, data = Dis_model)
summary(distmodel13)
vif(distmodel13)

distmodel14 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                    per_order + NPS + holiday_freq + Sponsorship + inc_LP_MA2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.3, data = Dis_model)
summary(distmodel14)
vif(distmodel14)

distmodel15 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                    per_order + holiday_freq + Sponsorship + inc_LP_MA2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.3, data = Dis_model)
summary(distmodel15)
vif(distmodel15)

distmodel16 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + holiday_freq + Sponsorship + inc_LP_MA2 + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.3, data = Dis_model)
summary(distmodel16)
vif(distmodel16)

distmodel17 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + holiday_freq + Sponsorship + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.3, data = Dis_model)
summary(distmodel17)
vif(distmodel17)

distmodel18 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + Sponsorship + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.3, data = Dis_model)
summary(distmodel18)
vif(distmodel18)

distmodel19 <- lm(formula = gmv ~ promotional_offer + holiday_freq + Sponsorship + promotional_offer.3 + NPS.1 + NPS.3 + 
                    holiday_freq.3, data = Dis_model)
summary(distmodel19)
vif(distmodel19)

distmodel20 <- lm(formula = gmv ~ promotional_offer + holiday_freq + Sponsorship + promotional_offer.3 + NPS.1 + 
                    holiday_freq.3, data = Dis_model)
summary(distmodel20)
vif(distmodel20)

distmodel21 <- lm(formula = gmv ~ promotional_offer + holiday_freq + Sponsorship + promotional_offer.3 + NPS.1 
                  , data = Dis_model)
summary(distmodel21)
vif(distmodel21)

distmodel22 <- lm(formula = gmv ~ promotional_offer + holiday_freq + Sponsorship + NPS.1, data = Dis_model)
summary(distmodel22)
vif(distmodel22)

distmodel23 <- lm(formula = gmv ~ promotional_offer + Sponsorship + NPS.1, data = Dis_model)
summary(distmodel23)
vif(distmodel23)

distmodel23 <- lm(formula = gmv ~ promotional_offer + Sponsorship, data = Dis_model)
summary(distmodel23)
vif(distmodel23)

Dist_Final_Model_home <- distmodel23

# Adj R square  = 0.429 with 2 significant variables
crossval <- cv.lm(data = Dis_model, form.lm = formula(gmv ~ promotional_offer + Sponsorship),m = 10)
Home_rep[4] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Dis_model
grlm <- Dist_Final_Model_home

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Distributed Lag Model") +xlab("Variables")

#Camera Accessory dataset Multiplicative + distributed lag model building
Multi_dist <- Camera_final[-c(1:4,10:13,22:30,37:39)] 
Multi_Dis_model_1 <- slide(Multi_dist, Var = "gmv",slideBy = -1)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -2)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -3)
Multi_dist <- na.omit(Multi_Dis_model_1)
colnames(Multi_dist)[14:22]<- c("promotional_offer_1","promotional_offer_2","promotional_offer_3","NPS_1",
                                "NPS_2","NPS_3", "gmv_1", "gmv_2", "gmv_3")
Multi_dist$Content.Marketing[which(Multi_dist$Content.Marketing==0)] <-1
Multi_dist$per_order[which(Multi_dist$per_order==0)] <-0.01

Multi_dist <- log(Multi_dist)
mdmodel1 <- lm(gmv~., Multi_dist)
summary(mdmodel1)

mdmodel2 <- stepAIC(mdmodel1,direction = "both")
summary(mdmodel2) 
vif(mdmodel2)

mdmodel3 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + per_order + 
                 Digital + Affiliates + SEM + gmv_1, data = Multi_dist)
summary(mdmodel3)
vif(mdmodel3)

mdmodel4 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + per_order + Affiliates + SEM + gmv_1, data = Multi_dist)
summary(mdmodel4)
vif(mdmodel4)

mdmodel5 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + per_order + SEM + gmv_1, data = Multi_dist)
summary(mdmodel5)
vif(mdmodel5)

mdmodel6 <- lm(formula = gmv ~ promotional_offer + per_order + SEM + gmv_1, data = Multi_dist)
summary(mdmodel6)
vif(mdmodel6)

mdmodel7 <- lm(formula = gmv ~ promotional_offer + per_order + gmv_1, data = Multi_dist)
summary(mdmodel7)
vif(mdmodel7)

# Adjusted R-squared:  0.758  with 3 significant variables
Multi_Dist_Final_Model_camera <- mdmodel7

crossval <- cv.lm(data = Multi_dist, form.lm = formula(gmv ~ promotional_offer + per_order + gmv_1),m = 10)
Camera_rep[5] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Multi_dist
grlm <- Multi_Dist_Final_Model_camera

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative & Distributed Lag Model") +xlab("Variables")

#Game Accessory dataset Multiplicative + distributed lag model building
Multi_dist <- Game_final[-c(1:4,10:13,22:30,37:39)]
Multi_Dis_model_1 <- slide(Multi_dist, Var = "gmv",slideBy = -1)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -2)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -3)
Multi_dist <- na.omit(Multi_Dis_model_1)
colnames(Multi_dist)[14:22]<- c("promotional_offer_1","promotional_offer_2","promotional_offer_3","NPS_1",
                                "NPS_2","NPS_3", "gmv_1", "gmv_2", "gmv_3")
Multi_dist$Content.Marketing[which(Multi_dist$Content.Marketing==0)] <-1
Multi_dist$per_order[which(Multi_dist$per_order==0)] <-0.01
Multi_dist <- log(Multi_dist)

mdmodel1 <- lm(gmv~., Multi_dist)
summary(mdmodel1)

mdmodel2 <- stepAIC(mdmodel1,direction = "both")
summary(mdmodel2) 
vif(mdmodel2)

mdmodel3 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                 TV + Digital + Sponsorship + Content.Marketing + Online_marketing + 
                 SEM + promotional_offer_1 + promotional_offer_2 + 
                 NPS_2 + gmv_1 + gmv_2 + gmv_3, data = Multi_dist)
summary(mdmodel3)
vif(mdmodel3)

mdmodel4 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                 TV + Digital + Sponsorship + Content.Marketing + 
                 SEM + promotional_offer_1 + promotional_offer_2 + 
                 NPS_2 + gmv_1 + gmv_2 + gmv_3, data = Multi_dist)
summary(mdmodel4)
vif(mdmodel4)

mdmodel5 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                 TV + Digital + Sponsorship + Content.Marketing + 
                 promotional_offer_1 + promotional_offer_2 + 
                 NPS_2 + gmv_1 + gmv_2 + gmv_3, data = Multi_dist)
summary(mdmodel5)
vif(mdmodel5)

mdmodel6 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + 
                 TV + Digital + Sponsorship + Content.Marketing + 
                 promotional_offer_1 + promotional_offer_2 + 
                 NPS_2 + gmv_1 + gmv_2 + gmv_3, data = Multi_dist)
summary(mdmodel6)
vif(mdmodel6)

mdmodel7 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + 
                 TV + Digital + Sponsorship + Content.Marketing + 
                 promotional_offer_1 + promotional_offer_2 + NPS_2 + gmv_1 + gmv_2, data = Multi_dist)
summary(mdmodel7)
vif(mdmodel7)

mdmodel8 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + 
                 TV + Digital + Sponsorship + Content.Marketing + 
                 promotional_offer_1 + promotional_offer_2 + gmv_1 + gmv_2, data = Multi_dist)
summary(mdmodel8)
vif(mdmodel8)

mdmodel9 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + 
                 TV + Digital + Sponsorship + Content.Marketing + 
                 promotional_offer_1 + gmv_1 + gmv_2, data = Multi_dist)
summary(mdmodel9)
vif(mdmodel9)

mdmodel10 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + 
                  TV + Digital + Sponsorship + promotional_offer_1 + gmv_1 + gmv_2, data = Multi_dist)
summary(mdmodel10)
vif(mdmodel10)

mdmodel11 <- lm(formula = gmv ~ Procurement_SLA + 
                  TV + Digital + Sponsorship + promotional_offer_1 + gmv_1 + gmv_2, data = Multi_dist)
summary(mdmodel11)
vif(mdmodel11)

mdmodel12 <- lm(formula = gmv ~ Procurement_SLA + TV + Digital + Sponsorship + gmv_1 + gmv_2, data = Multi_dist)
summary(mdmodel12)
vif(mdmodel12)

mdmodel13 <- lm(formula = gmv ~ Procurement_SLA + TV + Digital + Sponsorship + gmv_2, data = Multi_dist)
summary(mdmodel13)
vif(mdmodel13)

mdmodel14 <- lm(formula = gmv ~ Procurement_SLA + TV + Digital + Sponsorship, data = Multi_dist)
summary(mdmodel14)
vif(mdmodel14)

mdmodel15 <- lm(formula = gmv ~ TV + Digital + Sponsorship, data = Multi_dist)
summary(mdmodel15)
vif(mdmodel15)

# Adjusted R-squared:  0.778  with 3 significant variables
Multi_Dist_Final_Model_game <- mdmodel15
crossval <- cv.lm(data = Multi_dist, form.lm = formula(gmv ~ TV + Digital + Sponsorship),m = 10)
Game_rep[5] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Multi_dist
grlm <- Multi_Dist_Final_Model_game

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multi. & DL model") +xlab("Variables")

#Home Accessory dataset Multiplicative + distributed lag model building
Multi_dist <- Home_final[-c(1:4,10:13,22:30,37:39)] 
Multi_Dis_model_1 <- slide(Multi_dist, Var = "gmv",slideBy = -1)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -2)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -3)
Multi_dist <- na.omit(Multi_Dis_model_1)
colnames(Multi_dist)[14:22]<- c("promotional_offer_1","promotional_offer_2","promotional_offer_3","NPS_1",
                                "NPS_2","NPS_3", "gmv_1", "gmv_2", "gmv_3")
Multi_dist$Content.Marketing[which(Multi_dist$Content.Marketing==0)] <-1
Multi_dist$per_order[which(Multi_dist$per_order==0)] <-0.01
Multi_dist <- log(Multi_dist)

mdmodel1 <- lm(gmv~., Multi_dist)
summary(mdmodel1)

mdmodel2 <- stepAIC(mdmodel1,direction = "both")
summary(mdmodel2) 
vif(mdmodel2)

mdmodel3 <- lm(formula = gmv ~ SLA + promotional_offer + NPS + TV + 
                 Affiliates + NPS_1 + Content.Marketing, data = Multi_dist)
summary(mdmodel3)
vif(mdmodel3)

mdmodel4 <- lm(formula = gmv ~ SLA + promotional_offer + TV + 
                 Affiliates + NPS_1 + Content.Marketing, data = Multi_dist)
summary(mdmodel4)
vif(mdmodel4)

mdmodel5 <- lm(formula = gmv ~ SLA + promotional_offer + TV + 
                 NPS_1 + Content.Marketing, data = Multi_dist)
summary(mdmodel5)
vif(mdmodel5)

mdmodel6 <- lm(formula = gmv ~ SLA + promotional_offer + NPS_1 + Content.Marketing, data = Multi_dist)
summary(mdmodel6)
vif(mdmodel6)

mdmodel7 <- lm(formula = gmv ~ SLA + promotional_offer + NPS_1, data = Multi_dist)
summary(mdmodel7)
vif(mdmodel7)

mdmodel8 <- lm(formula = gmv ~ promotional_offer + NPS_1, data = Multi_dist)
summary(mdmodel8)
vif(mdmodel8)

# Adjusted R-squared:  0.249  with 2 significant variables
Multi_Dist_Final_Model_home <- mdmodel8
crossval <- cv.lm(data = Multi_dist, form.lm = formula(gmv ~ promotional_offer + NPS_1),m = 10)
Home_rep[5] <- attr(crossval, "ms")

# Elasticity Analysis
train <- Multi_dist
grlm <- Multi_Dist_Final_Model_home

elasticity <- function(var){
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")
elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multi. & DL Model") +xlab("Variables")
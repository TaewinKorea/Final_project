########## 	Creating random numbers: 3 Fulfillment centers and Hourly order data of Oct 2021 #################
year <- 2021
month <- 10
day <- 1:31
Date <- as.Date(ISOdate(year, month, day))
Date

FC1 <- data.frame(Date, rep("FC1", 31), rnorm(31,0.01*30000,1000), rnorm(31,0.03*30000,1000),rnorm(31,0.01*30000),rnorm(31,0.05*30000),rnorm(31,0.04*30000),rnorm(31,0.05*30000),rnorm(31,0.04*30000),rnorm(31,0.05*30000),rnorm(31,0.05*30000),rnorm(31,0.05*30000),rnorm(31,0.05*30000),rnorm(31,0.05*30000),rnorm(31,0.05*30000),rnorm(31,0.06*30000),rnorm(31,0.08*30000),rnorm(31,0.07*30000),rnorm(31,0.07*30000),rnorm(31,0.07*30000),rnorm(31,0.05*30000),rnorm(31,0.02*30000),rnorm(31,0.01*30000),rnorm(31,0.01*30000,500),rnorm(31,0.01*30000,500),rnorm(31,0*30000,0) )
names(FC1) <- c("Date","FC_Name", "06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","01","02","03","04","05")
FC1

FC2 <- data.frame(Date, rep("FC2", 31), rnorm(31,0.01*20000,1000), rnorm(31,0.03*20000,1000),rnorm(31,0.01*20000),rnorm(31,0.05*20000),rnorm(31,0.04*20000),rnorm(31,0.05*20000),rnorm(31,0.04*20000),rnorm(31,0.05*20000),rnorm(31,0.05*20000),rnorm(31,0.05*20000),rnorm(31,0.05*20000),rnorm(31,0.05*20000),rnorm(31,0.05*20000),rnorm(31,0.06*20000),rnorm(31,0.08*20000),rnorm(31,0.07*20000),rnorm(31,0.07*20000),rnorm(31,0.07*20000),rnorm(31,0.05*20000),rnorm(31,0.02*20000),rnorm(31,0.01*20000),rnorm(31,0.01*20000,500),rnorm(31,0.01*20000,500),rnorm(31,0*20000,0) )
names(FC2) <- c("Date","FC_Name", "06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","01","02","03","04","05")
FC2

FC3 <- data.frame(Date, rep("FC3", 31), rnorm(31,0.01*10000,1000), rnorm(31,0.03*10000,1000),rnorm(31,0.01*10000),rnorm(31,0.05*10000),rnorm(31,0.04*10000),rnorm(31,0.05*10000),rnorm(31,0.04*10000),rnorm(31,0.05*10000),rnorm(31,0.05*10000),rnorm(31,0.05*10000),rnorm(31,0.05*10000),rnorm(31,0.05*10000),rnorm(31,0.05*10000),rnorm(31,0.06*10000),rnorm(31,0.08*10000),rnorm(31,0.07*10000),rnorm(31,0.07*10000),rnorm(31,0.07*10000),rnorm(31,0.05*10000),rnorm(31,0.02*10000),rnorm(31,0.01*10000),rnorm(31,0.01*10000,500),rnorm(31,0.01*10000,500),rnorm(31,0*10000,0) )
names(FC3) <- c("Date","FC_Name", "06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","01","02","03","04","05")
FC3

FC1$Capacity <- 30000
FC2$Capacity <- 20000
FC3$Capacity <- 10000

All_FC_Data <- rbind(FC1, FC2, FC3)

Daily_order <- rowSums(All_FC_Data[,c(3:26)],na.rm=TRUE)

All_FC_Data$Daily_Total_Order <- Daily_order

All_FC_Data

############### Descriptive statistics ######################
tapply(All_FC_Data$Daily_Total_Order, All_FC_Data$FC_Name, mean)
tapply(All_FC_Data$Daily_Total_Order, All_FC_Data$FC_Name, sd)
All_FC_Data$Diff_Capacity <- All_FC_Data$Capacity - All_FC_Data$Daily_Total_Order
All_FC_Data$Beyond_Capacity <- ifelse(All_FC_Data$Diff_Capacity > 0, 0, 1)
All_FC_Data
tapply(All_FC_Data$Beyond_Capacity, All_FC_Data$FC_Name, sum)


############# Estimating next month’s order volume #############
library(forecast)
FC1$Daily_Order <- rowSums(FC1[,c(3:26)],na.rm=TRUE)
forecast(FC1$Daily_Order, h=30)
FC2$Daily_Order <- rowSums(FC2[,c(3:26)],na.rm=TRUE)
forecast(FC2$Daily_Order, h=30)
FC3$Daily_Order <- rowSums(FC3[,c(3:26)],na.rm=TRUE)
forecast(FC3$Daily_Order, h=30)


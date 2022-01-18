

## data exploration of wetland P retention from literature studies


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")

x <- read.csv("Wetland_P_Toy_Data2.csv", header = T)
head(x)

## omit rows with missing data
x<- x[-c(118,119,120,121,122,123,124,125,135),]


lX<-log(x[,c(11,13, 15,16)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)


x$ratio <- x$SRP_Retention_percent/x$TP_Retention_percent

## mass at outflow
x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)

### Hydraulic loading rate
x$HRL <- x$Inflow_m3_yr/x$Area_m2


### mass removed
x$TP_removal <- x$TP_load_in_g_m2_yr - x$TP_load_out
x$SRP_removal <- x$SRP_load_in_g_m2_yr - x$SRP_load_out





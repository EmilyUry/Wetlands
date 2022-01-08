



setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")


library(viridis)
library(dplyr)
library(tidyr)


x <- read.csv("Wetland_P_Toy_Data.csv", header = T)


plot(x$TP_Retention_., x$PO4_Retention_.)
abline(1,1)


hist(x$Area_m2, breaks=50)
hist(x$Inflow_m3_yr, breaks=50)
hist(x$TP_inflow_mg_L, breaks=50)



lX<-log(x[,c(8:11)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)


hist(x$logArea_m2, breaks=50)
hist(x$logInflow_m3_yr, breaks=50)
hist(x$logTP_inflow_mg_L, breaks=50)

### TP retention
plot(x$logArea_m2, x$TP_Retention_., pch = 16, col = x$Catchment_Type)
plot(x$logInflow_m3_yr, x$TP_Retention_., pch = 16, col = x$Catchment_Type)
plot(x$logTP_inflow_mg_L, x$TP_Retention_., pch = 16, col = x$Catchment_Type)
legend("bottomright", c("Ag", "urban", "WWTP"), pch = 16, col = c(1,2,3))


plot(x$logArea_m2, x$TP_Retention_., pch = 16, col = x$Wetland_Type)
plot(x$logInflow_m3_yr, x$TP_Retention_., pch = 16, col = x$Wetland_Type)
plot(x$logTP_inflow_mg_L, x$TP_Retention_., pch = 16, col = x$Wetland_Type)
legend("bottomright", c("Constructed", "restored", "mesocosm", "natural"), pch = 16, col = c(1,2,3,4))


### SRP retention
plot(x$logArea_m2, x$PO4_Retention_., pch = 16, col = x$Catchment_Type)
plot(x$logInflow_m3_yr, x$PO4_Retention_., pch = 16, col = x$Catchment_Type)
plot(x$logTP_inflow_mg_L, x$PO4_Retention_., pch = 16, col = x$Catchment_Type)
legend("bottomright", c("Ag", "urban", "WWTP"), pch = 16, col = c(1,2,3))


plot(x$logArea_m2, x$PO4_Retention_., pch = 16, col = x$Wetland_Type)
plot(x$logInflow_m3_yr, x$PO4_Retention_., pch = 16, col = x$Wetland_Type)
plot(x$logTP_inflow_mg_L, x$PO4_Retention_., pch = 16, col = x$Wetland_Type)
legend("bottomright", c("Constructed", "restored", "mesocosm", "natural"), pch = 16, col = c(1,2,3,4))



#x$ratio <- x$TP_Retention_./x$PO4_Retention_.
x$ratio <- x$PO4_Retention_./x$TP_Retention_.

plot(x$logTP_inflow_mg_L, x$ratio, ylim = c(-30,10))
plot(x$logSRP_inflow_mg_L, x$ratio, ylim = c(-30,10))

plot(x$Age_yr, x$ratio, xlim=c(0,6))
plot(x$logArea_m2, x$ratio)


plot(x$TP_Retention_., x$PO4_Retention_.)
abline(h=0)
abline(v=0)


hist(x$logArea_m2)
## bin data by wetland size into 5 roughly evenly sized bins
breaks  <- c(0,7.2, 8, 10, 20)
#breaks <- c(-1, 100, 1000, 10000, 100000, 100000000000)
tags <- c("small","medium", "med-large","large")
#tags <- c("0-100","100-1k", "1k-10k", "10k-100k", "100k+")
group_tags <- cut(x$logArea_m2,
                  breaks = breaks,
                  include.lowest = TRUE, right = FALSE, 
                  labels = tags)
summary(group_tags)
area_bins <- factor(group_tags, levels = tags, ordered = TRUE)
x$area_bins <- area_bins




plot(x$TP_Retention_., x$PO4_Retention_., pch=16, col = x$area_bins)
abline(h=0)
abline(v=0)
legend("bottomleft", c("small","medium", "med-large","large"), pch = 16, col = c(1:4))






### Relative flow (relflow) is the amount of water the wetland recieves relative to its size
### this should be a good predictor of P retention and P retention ratio
x$relflow <- x$Inflow_m3_yr/x$Area_m2
x$logrelflow <- x$logInflow_m3_yr/x$logArea_m2
plot(x$relflow, x$TP_Retention_.)
plot(x$relflow, x$PO4_Retention_.)


plot(x$logrelflow, x$TP_Retention_.)
plot(x$logrelflow, x$PO4_Retention_., pch = 16, col = x$Reference.Paper)



## normalize relflow for inflow conc


x$n.relflow <- x$Inflow_m3_yr/x$Area_m2/x$TP_inflow_mg_L
plot(x$n.relflow, x$TP_Retention_.)
plot(log(x$n.relflow), x$PO4_Retention_.)



rbPal <- colorRampPalette(c('red','blue'))
x$col <- rbPal(10)[as.numeric(cut(x$logTP_inflow_mg_L, breaks = 10))]
plot(x$relflow, x$TP_Retention_., pch = 16, col = x$col)
plot(x$relflow, x$PO4_Retention_., pch = 16, col = x$col)





plot(log(x$n.relflow), x$PO4_Retention_., pch = 16, col = x$col)









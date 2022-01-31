

## data exploration of wetland P retention from literature studies


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")

x <- read.csv("Wetland_P_Toy_Data2.csv", header = T)
head(x)

## omit rows with missing concentration data
#x<- x[-c(118,119,120,121,122,123,124,125,135),]


lX<-log(x[,c(11,13, 16,17)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)


x$ratio <- x$SRP_Retention_percent/x$TP_Retention_percent

## mass at outflow
x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)

### Hydraulic loading rate
x$HLR <- x$Inflow_m3_yr/x$Area_m2


### mass removed
x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out


## Figure 1. P retention by wetland type

par(mfrow = c(2,2), mar = c(4,5,1,1))
boxplot(TP_retention ~ Wetland_Type, data = x, ylim = c(-25,125), 
        ylab = "TP retention g/m2/yr")
boxplot(SRP_retention ~ Wetland_Type, data = x, ylim = c(-25,125),
        ylab = "SRP retention g/m2/yr")

boxplot(TP_Retention_percent ~ Wetland_Type, data = x, ylim = c(-200,100), 
        ylab = "TP retention %")
boxplot(SRP_Retention_percent ~ Wetland_Type, data = x, ylim = c(-200,100),
        ylab = "SRP retention g/m2/yr")

## note, one data point for TP and 5 for SRP not shown because they are below -200% retention
## note the distribution of wetland type:
table(x$Wetland_Type)


## Figure 2. P retention by load in

par(mfrow = c(2,2), mar = c(4,5,1,1))

plot(x$TP_load_in_g_m2_yr, x$TP_Retention_percent, 
     ylim = c(-200,100), xlim = c(0,150), 
     ylab = "TP Retention %", xlab = "TP load (g/m2/yr)")

plot(x$SRP_load_in_g_m2_yr, x$SRP_Retention_percent, 
     ylim = c(-200,100), xlim = c(0,70), 
     ylab = "SRP Retention %", xlab = "SRP load (g/m2/yr)")


plot(x$TP_Inflow_mg_L, x$TP_Retention_percent, 
     ylim = c(-200,100), xlim = c(0,25), 
     ylab = "TP Retention %", xlab = "[TP] (mg/L)")

plot(x$SRP_Inflow_mg_L, x$SRP_Retention_percent, 
     ylim = c(-200,100), xlim = c(0,25), 
     ylab = "SRP Retention %", xlab = "[SRP] (mg/L)")



## Figure 2. P retention by load in **LOG SCALE**

par(mfrow = c(2,2), mar = c(5,5,1,1))
options(scipen = 100)

plot(x$TP_load_in_g_m2_yr, x$TP_Retention_percent, log = "x",
     ylim = c(-200,100),  
     ylab = "TP Retention %", xlab = "TP load (g/m2/yr)")


plot(log(x$SRP_load_in_g_m2_yr), x$SRP_Retention_percent, log = "x",
     ylim = c(-200,100), 
     ylab = "SRP Retention %", xlab = "SRP load (g/m2/yr)")


plot(x$TP_Inflow_mg_L, x$TP_Retention_percent, log = "x",
     ylim = c(-200,100),  
     ylab = "TP Retention %", xlab = "[TP] (mg/L)")

plot(x$SRP_Inflow_mg_L, x$SRP_Retention_percent, log = "x",
     ylim = c(-200,100),
     ylab = "SRP Retention %", xlab = "[SRP] (mg/L)")




## fred's figures: K ~ tau
par(mfrow = c(1,1))

## calculate Tau
#x <- x[which(x$Area_m2 < 500000),]

#plot(x$Area_m2, x$HRT_d, xlim = c(0,20000))

y <- x[which(x$HRT_d >0),]
plot(log(y$Area_m2), log(y$HRT_d))
fit <- lm(log(HRT_d) ~ log(Area_m2), data = y)
summary(fit)                                      ## not great.
abline(fit, col = 'red')


x1 <- c(1,10,1000,10000,100000, 10000000, 100000000)
y1 <- exp(1.64)*x1^(0.04898)

plot(y$Area_m2, y$HRT_d, log = 'xy', 
     xlab = "Surface Area (m2)",
     ylab = "Hydraulic Residence Time, Tau (d)")
points(x1,y1, col = "red", type = 'l')
text(100, 50, "Tau = 5.2*SA^0.049")
text(100, 40, "R2 = 0.045, p = 0.011")
 

## The SA~Tau relationship from this data set is very weak,
## we will use the relationship from Cheng and Basu 2017 Eq. 13



x$Tau <- 1.51*x$Area_m2^0.23
points(x$Area_m2, x$Tau, type ='l')
hist(x$Tau)




## calculate k (rate constant) TP
x$k2 <- -(log(1-(x$TP_Retention_percent/100))/x$Tau)

plot(log(x$Tau), log(x$k2))
fit <- lm(log(x$k2) ~ log(x$Tau))
summary(fit)
abline(fit, col = 'red')

x1 <- seq(0,80, by = 0.1)
y1 <- exp(-1.1235)*x1^(-0.812)
plot(x$Tau, x$k2)
points(x1,y1, col = "red", type = 'l')


tiff(filename = "figures/TP_K_Tau_plot.tiff", height=3600, width=3600, units= "px", res=800, compression= "lzw")

plot((x$Tau), (x$k2), log = 'xy', pch = 16, 
     xlab = expression(paste("Water Residence Time, ", tau, " (d)")), 
     ylab = expression(paste("TP Rate Constant, k ( ", d^-1, ")")))
points(x1,y1, col = "red", type = 'l')
text(4,0.001, expression(bold(paste("k = 0.33", tau)^-0.81)), cex = 0.8)
text(4,0.00055, expression(bold(paste(" ",R^2, " = 0.32, p < 0.001"))), cex = 0.8)

dev.off()





## calculate k (rate constant) SRP
x$k2 <- -(log(1-(x$SRP_Retention_percent/100))/x$Tau)

plot(log(x$Tau), log(x$k2))
fit <- lm(log(x$k2) ~ log(x$Tau))
summary(fit)
abline(fit, col = 'red')

x1 <- seq(0,80, by = 0.1)
y1 <- exp(-1.54)*x1^(-0.532)
plot(x$Tau, x$k2)
points(x1,y1, col = "red", type = 'l')


tiff(filename = "figures/SRP_K_Tau_plot.tiff", height=3600, width=3600, units= "px", res=800, compression= "lzw")

plot((x$Tau), (x$k2), log = 'xy', pch = 16, 
     xlab = expression(paste("Water Residence Time, ", tau, " (d)")), 
     ylab = expression(paste("SRP Rate Constant, k ( ", d^-1, ")")))
points(x1,y1, col = "red", type = 'l')
text(4,0.002, expression(bold(paste("k = 0.21", tau)^-0.53)), cex = 0.8)
text(4,0.001, expression(bold(paste(" ",R^2, " = 0.15, p < 0.001"))), cex = 0.8)

dev.off()



### look at p sink/source behavior based on flow and flow anomaly

## Figure 2. P retention by flow

par(mfrow = c(2,2), mar = c(4,5,1,1))

plot(x$Inflow_m3_yr, x$TP_Retention_percent, log = 'x', pch = 16,
     ylim = c(-200,100),  
     ylab = "TP Retention %", xlab = "Inflow (m3/yr)")
abline(h=0, col = 'red')

plot(x$Inflow_m3_yr, x$SRP_Retention_percent, log = 'x', pch = 16,
     ylim = c(-200,100),  
     ylab = "SRP Retention %", xlab = "Inflow (m3/yr)")
abline(h=0, col = 'red')




## calculate flow anomaly
library(tidyverse)

Flow <- aggregate(Inflow_m3_yr ~ Source + WetlandID, data = x, FUN = mean) %>%
        mutate(Unique_ID = paste(Source, WetlandID))
names(Flow)[3] <- "mean_flow"

wetlands <- x %>%
        mutate(Unique_ID = paste(Source, WetlandID)) %>%
        group_by(Unique_ID) %>%
        summarize(num = n()) %>%
        filter(num > 1) %>%
        left_join(Flow, by = "Unique_ID") %>%
        select(c('Source', 'WetlandID', 'mean_flow'))

data <- x %>%
        left_join(wetlands, by = c("Source", "WetlandID")) %>%
        mutate(flow_anom = Inflow_m3_yr - mean_flow)
data <- data[-c(98:107),]                              ###### a bunch of rows report the average flow for all yrs. need to remove these
data <- data[-c(54:82),]

par(mfrow = c(1,1))
plot(data$flow_anom, data$TP_Retention_percent,
     xlim = c(-50000, 50000), ylim = c(-150,150),
     xlab = "flow anomaly", ylab = "TP Retention %")
abline(h=0, col = 'red')
abline(v=0, col = 'blue')





plot(data$flow_anom, data$TP_Retention_percent, pch = 16, col = as.factor(x$Wetland_Type),
     xlim = c(-50000, 50000), ylim = c(-150,150),
     xlab = "flow anomaly", ylab = "TP Retention %")
abline(h=0, col = 'red')
abline(v=0, col = 'blue')
legend("bottomright", c("Constructed", "Mesocosm", "Natural", "Restored"), pch = 16, col = c(1,2,3,4))



plot(data$flow_anom, data$SRP_Retention_percent, 
     xlim = c(-50000, 50000), ylim = c(-150,150),
     xlab = "flow anomaly", ylab = "SRP Retention %")
abline(h=0, col = 'red')
abline(v=0, col = 'blue')







################################################
### junk code

plot(x$Inflow_m3_yr, x$TP_Retention_percent, log = 'x', pch = 16,
ylim = c(-200,100),  col = as.factor(x$WetlandID),
ylab = "TP Retention %", xlab = "Inflow (m3/yr)")
abline(h=0, col = 'red')




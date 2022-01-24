

## data exploration of wetland P retention from literature studies


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")

x <- read.csv("Wetland_P_Toy_Data2.csv", header = T)
head(x)

## omit rows with missing data
x<- x[-c(118,119,120,121,122,123,124,125,135),]


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

plot(x$Area_m2, x$HRT_d, log = 'xy')

fit <- lm(HRT_d ~ Area_m2, data = x)
abline(fit)
summary(fit)


x$Tau. <- x

x$Tau <- x$Area_m2*0.00019086 

hist(x$Tau)




## calculate k (rate constant) TP
x$k <- -(log(1-(x$TP_Retention_percent/100))/x$Tau)
plot(x$Tau, x$k, log = 'xy')
fit <- lm(log(x$k) ~ log(x$Tau))
summary(fit)
abline(fit)
x <- seq(0,10, by = 0.1)
y <- 0.33*exp(-0.88*x)
points(x,y, col = "red")

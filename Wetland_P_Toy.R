



setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")


library(viridis)
library(dplyr)
library(tidyr)


x <- read.csv("Wetland_P_Toy_Data2.csv", header = T)
head(x)

plot(x$TP_Retention_percent, x$SRP_Retention_percent)
abline(1,1)


hist(x$Area_m2, breaks=50)
hist(x$Inflow_m3_yr, breaks=50)
hist(x$TP_Inflow_mg_L, breaks=50)



lX<-log(x[,c(11,13, 15,16)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)


hist(x$logArea_m2, breaks=50)
hist(x$logInflow_m3_yr, breaks=50)
hist(x$logTP_Inflow_mg_L, breaks=50)

### TP retention
plot(x$logArea_m2, x$TP_Retention_percent, pch = 16, col = x$Catchment_Type, ylim = c(-60, 100))
plot(x$logInflow_m3_yr, x$TP_Retention_percent, pch = 16, col = x$Catchment_Type, ylim = c(-60, 100))
plot(x$logTP_Inflow_mg_L, x$TP_Retention_percent, pch = 16, col = x$Catchment_Type, ylim = c(-60, 100))
legend("bottomright", c("Ag", "urban", "WWTP"), pch = 16, col = c(1,2,3))
levels(x$Catchment_Type)

plot(x$logArea_m2, x$TP_Retention_percent, pch = 16, col = x$Wetland_Type, ylim = c(-60, 100))
plot(x$logInflow_m3_yr, x$TP_Retention_percent, pch = 16, col = x$Wetland_Type, ylim = c(-60, 100))
plot(x$logTP_Inflow_mg_L, x$TP_Retention_percent, pch = 16, col = x$Wetland_Type, ylim = c(-60, 100))
legend("bottomright", c("Constructed", "Mesocosm", "Natural", "Restored"), pch = 16, col = c(1,2,3,4))
levels(x$Wetland_Type)

### SRP retention
plot(x$logArea_m2, x$SRP_Retention_percent, pch = 16, col = x$Catchment_Type)
plot(x$logInflow_m3_yr, x$SRP_Retention_percent, pch = 16, col = x$Catchment_Type)
plot(x$logTP_Inflow_mg_L, x$SRP_Retention_percent, pch = 16, col = x$Catchment_Type)
legend("bottomright", c("Ag", "urban", "WWTP"), pch = 16, col = c(1,2,3))


plot(x$logArea_m2, x$SRP_Retention_percent, pch = 16, col = x$Wetland_Type)
plot(x$logInflow_m3_yr, x$SRP_Retention_percent, pch = 16, col = x$Wetland_Type)
plot(x$logTP_Inflow_mg_L, x$SRP_Retention_percent, pch = 16, col = x$Wetland_Type)
legend("bottomright", c("Constructed", "Mesocosm", "Natural", "Restored"), pch = 16, col = c(1,2,3,4))



#x$ratio <- x$TP_Retention_./x$PO4_Retention_.
x$ratio <- x$SRP_Retention_percent/x$TP_Retention_percent

plot(x$logTP_Inflow_mg_L, x$ratio, ylim = c(-30,10))
plot(x$logSRP_Inflow_mg_L, x$ratio, ylim = c(-30,10))

plot(x$Age_yr, x$ratio, xlim=c(0,6))
plot(x$logArea_m2, x$ratio)


plot(x$TP_Retention_percent, x$SRP_Retention_percent)
abline(h=0)
abline(v=0)


hist(x$logArea_m2)
## bin data by wetland size into 5 roughly evenly sized bins
breaks  <- c(0,6.8, 8, 10, 20)
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




plot(x$TP_Retention_percent, x$SRP_Retention_percent, pch=16, col = x$area_bins)
abline(h=0)
abline(v=0)
legend("bottomleft", c("small","medium", "med-large","large"), pch = 16, col = c(1:4))






### Relative flow (relflow) is the amount of water the wetland recieves relative to its size
### this should be a good predictor of P retention and P retention ratio
x$relflow <- x$Inflow_m3_yr/x$Area_m2
x$logrelflow <- x$logInflow_m3_yr/x$logArea_m2
plot(x$relflow, x$TP_Retention_percent)
plot(x$relflow, x$SRP_Retention_percent)


plot(x$logrelflow, x$TP_Retention_.)
plot(x$logrelflow, x$PO4_Retention_., pch = 16, col = x$Reference.Paper)



## normalize relflow for inflow conc


x$n.relflow <- x$Inflow_m3_yr/x$Area_m2/x$TP_Inflow_mg_L
plot(x$n.relflow, x$TP_Retention_percent)
plot(log(x$n.relflow), x$SRP_Retention_percent)



rbPal <- colorRampPalette(c('red','blue'))
x$col <- rbPal(10)[as.numeric(cut(x$logTP_Inflow_mg_L, breaks = 10))]
plot(x$relflow, x$TP_Retention_percent, pch = 16, col = x$col)
plot(x$relflow, x$SRP_Retention_percent, pch = 16, col = x$col)





plot(log(x$n.relflow), x$SRP_Retention_percent, pch = 16, col = x$col)





## compare loding and inflow concentration

x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")
plot(log(x$TP_Inflow_mg_L), log(x$TP_load_in_g_m2_yr), pch = 16, col = as.factor(x$ret))

rbPal <- colorRampPalette(c('red','blue'))
x$col <- rbPal(9)[as.numeric(cut(x$TP_Retention_percent, breaks = c(-1000, -100, -10, -1, 0 , 1, 10, 50, 100)))]
plot(log(x$TP_Inflow_mg_L), log(x$TP_load_in_g_m2_yr), pch = 16, col = x$col)



table(x$ret)
32/(32+144)
TP.source <- table(x$ret)[1]
TP.sink <- table(x$ret)[2]
TP.source.percent <- round(TP.source/(TP.sink+TP.source)*100,1)

x$PO4ret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")
table(x$PO4ret)
49/(49+126)
SRP.source <- table(x$PO4ret)[1]
SRP.sink <- table(x$PO4ret)[2]
SRP.source.percent <- round(SRP.source/(SRP.sink+SRP.source)*100,1)

data<- matrix(c(TP.sink, TP.source, SRP.sink, SRP.source), nrow = 2)
colnames(data) <- c("TP", "SRP")
rownames(data) <- c("sink", "source")

par(mfrow = c(1,1))
barplot(data, col = c("royalblue3", "lightblue"),
        border = "black", 
        space = 0.1, 
        xlab = " ", 
        ylab = "# wetland (site-years)")
text(0.4,150, paste(TP.source.percent, "% source", sep = ""), font = 2, cex = 1.5)
text(0.21,10, "Sink", font = 2, cex = 1.5)
text(1.31,10, "Sink", font = 2, cex = 1.5)
text(1.5,132, paste(SRP.source.percent, "% source", sep =""),  font = 2, cex = 1.5)
data.n <- nrow(x)
mtext(data.n, 3, 0)


hist(x$TP_Inflow_mg_L)
hist(x$logTP_Inflow_mg_L)



#### plot retention by size bin
{
par(mfrow = c(1,2))

## bin data by inflow TP concentration
breaks  <- c(0, 0.1, 1, 10, 100)
tags <- c("<0.1","0.1 - 1", "1-10", ">10")
group_tags <- cut(x$TP_Inflow_mg_L,
                  breaks = breaks,
                  include.lowest = TRUE, right = FALSE, 
                  labels = tags)
summary(group_tags)
TPin_bins <- factor(group_tags, levels = tags, ordered = TRUE)
x$TPin_bins <- TPin_bins


summary <- table(x$TPin_bins, x$ret)
summary
data<- matrix(c(summary[1,2], summary[1,1], summary[2,2],summary[2,1],summary[3,2],
                summary[3,1],summary[4,2],summary[4,1]), nrow = 2)
names(data) <- c("<0.1","0.1 - 1", "1-10", ">10")
rownames(data) <- c("sink", "source")

barplot(data, col = c("royalblue3", "lightblue"),
        border = "black", ylim = c(0,70),
        space = 0.1, main = "TP",
        xlab = "Inflow [TP] (mg/L)", 
        ylab = "frequency")
legend("topright", c("source", "sink"), pch = 22, cex = 1, pt.bg = c("lightblue","royalblue3") )
mtext("<0.1        0.1 - 1        1-10        >10", 1, 0.2)

## bin data by inflow TP concentration
breaks  <- c(0, 0.1, 1, 10, 100)
tags <- c("<0.1","0.1 - 1", "1-10", ">10")
group_tags <- cut(x$SRP_Inflow_mg_L,
                  breaks = breaks,
                  include.lowest = TRUE, right = FALSE, 
                  labels = tags)
summary(group_tags)
SRPin_bins <- factor(group_tags, levels = tags, ordered = TRUE)
x$SRPin_bins <- SRPin_bins
summary <- table(x$SRPin_bins, x$ret)
summary
data2<- matrix(c(summary[1,2], summary[1,1], summary[2,2],summary[2,1],summary[3,2],
                summary[3,1],summary[4,2],summary[4,1]), nrow = 2)

names(data2) <- c("<0.1","0.1 - 1", "1-10", ">10")
rownames(data2) <- c("sink", "source")

barplot(data2, col = c("royalblue3", "lightblue"),
        border = "black",  ylim = c(0,70),
        space = 0.1, main = "SRP",
        xlab = "Inflow [SRP] (mg/L)", 
        ylab = "frequency")
legend("topright", c("source", "sink"), pch = 22, cex = 1, pt.bg = c("lightblue","royalblue3") )
mtext("<0.1        0.1 - 1        1-10        >10", 1, 0.2)
}

par(mfrow = c(1,2))
plot(x$logArea_m2, x$TP_Retention_percent, pch = 16, col = x$Wetland_Type, ylim = c(-200,100) )
abline(h=0, lty =2)
legend("bottomright", c("Constructed", "Mesocosm", "Natural", "Restored"), pch = 16, col = c(1,2,3,4))
fit <- lm(TP_Retention_percent ~ logArea_m2, data = x)
abline(fit, col = "lightpink")
summary(fit)

plot(x$logArea_m2, x$SRP_Retention_percent, pch = 16, col = x$Wetland_Type, ylim = c(-200,100) )
abline(h=0, lty =2)
fit <- lm(SRP_Retention_percent ~ logArea_m2, data = x)
abline(fit, col = "lightpink")
summary(fit)


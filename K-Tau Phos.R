
### Wetland P Removal
## K-Tau 


#' Here we are exploring the relationship between the emperical 
#' data for P retention in wetlands and lakes and k and Tau's coefficients
#' for the first-order removal rate equations. 
#'
#' Here we are using data from a meta-analysis put together by Fred
#' Eventually we will update the data to use info from a lit compilation
#' I put together myself. 
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")


library(viridis)
library(dplyr)
library(tidyr)


## Fred's meta-analysis of literature values
x <- read.csv("P_speciation2.csv", header = T)


## filters
x <- x[which(x$area > 0),]
x <- x[which(x$area < 1000000000),]

table(x$Type)

## Tau = a(SA)^b

plot(log(x$area), log(x$tau), pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topleft", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(log(tau)~log(area), data = x)
abline(fit)
summary(fit)
abline(.6, .22)

plot(x$area, x$tau, logs= "xy", pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topleft", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(tau~area, data = x)
abline(fit)
summary(fit)
abline(.6, exp(.22))





plot(x$area, x$tau, pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topleft", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(tau~area, data = x)
abline(fit)
summary(fit)

plot(log(x$area), x$tau, pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topleft", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(tau~log(area), data = x)
abline(fit)
summary(fit)


plot(x$area, log(x$tau), pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topleft", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(log(tau)~area, data = x)
abline(fit)
summary(fit)


theta.0 <- 0
alpha.0 <- exp(0.6)
beta.0 <- 0.22
start <- list(alpha = alpha.0, beta = beta.0, theta = 0)
start
model <-nls(tau ~ alpha*exp(beta*area) + theta, data = x, start = start)
coef(model)

#           a           r 
# 11.30876845  0.09867135




## calculate k (rate constant) TP
x$k2 <- -1*(log(1-x$TP_Retention)/x$tau)
plot(log(x$tau), log(x$k2), pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topright", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(log(k2)~log(tau), data = x)
abline(fit)
summary(fit)




summary(x$TP_Retention)
x$Z <- (x$TP_Retention - min(x$TP_Retention))/(max(x$TP_Retention) - min(x$TP_Retention))
summary(x$Z)

x$k3 <- -1*(log(1-x$Z)/x$tau)
summary(x$k3)
x <- x[which(x$k3 < 5 & x$k3 > 0),]
plot(log(x$tau), log(x$k3), pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topright", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(log(k3)~log(tau), data = x)
abline(fit)
summary(fit)




## calculate k (rate constant) PO4
x$k2 <- -1*(log(1-x$PO4_Retention)/x$tau)
plot((x$tau), (x$k2), log = "xy", pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
plot(log(x$tau), log(x$k2), pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topright", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(log(k2)~log(tau), data = x)
abline(fit)
summary(fit)



## symbol for NADB

x$source <- ifelse(x$study == "NADB", 1, 2)
plot(log(x$tau), log(x$k2), pch = c(1,16)[x$source], col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])




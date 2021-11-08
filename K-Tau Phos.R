
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

## Tau = a(SA)^b

plot(x$area, x$tau, log ="xy", pch = 16, col = c("darkgreen", "darkgreen", "darkgreen", "blue", "red")[x$Type])
legend("topleft", c("Wetland", "Lake", "Reservoir"), pch = 16, col = c("darkgreen", "blue", "red"))
fit <- lm(log(tau)~log(area), data = x)
abline(fit)
summary(fit)



## calculate k (rate constant)
x$k2 <- -1*(log(1-x$TP_Retention)/x$tau)
plot(x$tau, x$k2, log = "xy")


table(x$Type)
 

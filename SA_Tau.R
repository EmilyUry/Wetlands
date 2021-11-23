

#' Investingating the relationship between SA and tau
#' 

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files")


data <- read.csv("SA_Tau.csv")



plot((data$SA_m2), (data$Tau_d), pch = 16 )
plot(log(data$SA_m2), log(data$Tau_d), pch = 16 )


fit <- lm(log(Tau_d) ~ log(SA_m2), data = data)
abline(fit)
summary(fit)




filter <- data[which(data$SA_m2 < 400000),]
filter <- filter[which(filter$Tau_d > 0.1),]

plot((filter$SA_m2), (filter$Tau_d), pch = 16 )

plot(log(filter$SA_m2), (filter$Tau_d), pch = 16 )

plot(log(filter$SA_m2), log(filter$Tau_d), pch = 16 )

fit <- lm(log(Tau_d) ~ log(SA_m2), data = filter)
abline(fit)
summary(fit)



subset <- data[1:28,]
plot((subset$SA_m2), (subset$Tau_d), pch = 16 )

plot(log(subset$SA_m2), (subset$Tau_d), pch = 16 )

plot(log(subset$SA_m2), log(subset$Tau_d), pch = 16 )

fit <- lm(log(Tau_d) ~ log(SA_m2), data = subset)
abline(fit)
summary(fit)


subset$pred <- 2.33 * subset$SA_m2 ^ 0.133
points(log(subset$SA_m2), log(subset$pred), col = "red")






##WES

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Literature")

data <- read.csv("Land_PCA.csv", head = TRUE)

d <- na.omit(data)

info <- subset(d, select = c(source, Water_regime, WetlandType, InflowType, Inflow, History, Veg))
res <- subset(d, select = c(Lat, Temp_C, Prec_mm_yr, HLR_m_yr, TN_mg_L, TP_mg_L, Area_m2, Age_yr, 
                            TN_loading_gm2yr, TP_loading_gm2yr))

pca <-prcomp(res, center = TRUE, scale = TRUE)
print(pca)
plot(pca)
plot(pca,type="line",cex.lab=1.5, cex.main=1.5)
abline(h=1,lty=3, col="red") ## keep  first 5 principle components (var > 1)


summary(pca)

pca.scores<-pca$x
pca.loading <- pca$rotation
df <- cbind(info, pca.scores[,1:3])
df$WetlandType <- as.factor(df$WetlandType)
df$Water_regime <- as.factor(df$Water_regime)
df$Inflow <- as.factor(df$Inflow)


plot(df$PC1, df$PC2, pch = 16, cex = 0.9, main = "PCA", 
     xlab = "PC1", ylab = "PC2")
arrows(0,0, pca.loading[,1]*8, pca.loading[,2]*8, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*8.5, pca.loading[,2]*8.3, row.names(pca.loading), cex = 1, col = "red")


hist(d$TP_percent_removal)
hist(d$TN_percent_removal)


 
rbPal <- colorRampPalette(c('red','blue'))
df$col <- ifelse(d$TP_percent_removal > 0, "#0000FFbb", "#FF0000bb")

plot(df$PC1, df$PC2, 
     xlab = "PC1", ylab = "PC2", 
     main = "TP removal",
     pch = 16, cex = abs(d$TP_percent_removal)/25+1, 
     col = df$col)
arrows(0,0, pca.loading[,1]*8, pca.loading[,2]*8, length = 0.1, lwd = 1.5, col = "black")
text(pca.loading[,1]*8, pca.loading[,2]*8, row.names(pca.loading), cex = 0.8, col = "black", pos = 4)





#### limited vars

info <- subset(d, select = c(source, Water_regime, WetlandType, InflowType, Inflow, History, Veg))
res <- subset(d, select = c(HLR_m_yr, Area_m2, Age_yr))

pca <-prcomp(res, center = TRUE, scale = TRUE)
print(pca)
plot(pca)
plot(pca,type="line",cex.lab=1.5, cex.main=1.5)
abline(h=1,lty=3, col="red") ## keep  first 5 principle components (var > 1)


summary(pca)

pca.scores<-pca$x
pca.loading <- pca$rotation
df <- cbind(info, pca.scores[,1:3])
df$WetlandType <- as.factor(df$WetlandType)
df$Water_regime <- as.factor(df$Water_regime)
df$Inflow <- as.factor(df$Inflow)


plot(df$PC1, df$PC2, pch = 16, cex = 0.9, main = "PCA", 
     xlab = "PC1", ylab = "PC2")
arrows(0,0, pca.loading[,1]*3, pca.loading[,2]*3, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*3.5, pca.loading[,2]*3.3, row.names(pca.loading), cex = 1, col = "red")


hist(d$TP_percent_removal)
hist(d$TN_percent_removal)



rbPal <- colorRampPalette(c('red','blue'))
df$col <- ifelse(d$TP_percent_removal > 0, "#0000FF99", "#FF000099")

plot(df$PC1, df$PC2, xlim = c(-5,3), ylim = c(-2,5),
     xlab = "PC1", ylab = "PC2", 
     main = "TP removal",
     pch = 16, cex = abs(d$TP_percent_removal)/25+1, 
     col = df$col)
arrows(0,0, pca.loading[,1]*5, pca.loading[,2]*5, length = 0.1, lwd = 1.5, col = "black")
text(pca.loading[,1]*5.5, pca.loading[,2]*5.5, row.names(pca.loading), cex = 0.8, col = "black", pos = 4)

plot(df$PC1, df$PC2, xlim = c(-1,1), ylim = c(-1,0),
     xlab = "PC1", ylab = "PC2", 
     main = "TP removal",
     pch = 16, cex = abs(d$TP_percent_removal)/25+1, 
     col = df$col)
arrows(0,0, pca.loading[,1]*5, pca.loading[,2]*5, length = 0.1, lwd = 1.5, col = "black")
text(pca.loading[,1]*5.5, pca.loading[,2]*5.5, row.names(pca.loading), cex = 0.8, col = "black", pos = 4)




rbPal <- colorRampPalette(c('red','blue'))
df$col <- rbPal(3)[as.numeric(cut(d$TN_percent_removal, breaks = 3))]

df$col <- ifelse(d$TP_percent_removal > 0, "#0000FF99", "#FF000099")

plot(df$PC1, df$PC2, xlim = c(-5,3), ylim = c(-2,5),
     xlab = "PC1", ylab = "PC2", 
     main = "TP removal",
     pch = 16, cex = abs(d$TP_percent_removal)/25+1, 
     col = df$col)
arrows(0,0, pca.loading[,1]*5, pca.loading[,2]*5, length = 0.1, lwd = 1.5, col = "black")
text(pca.loading[,1]*5.5, pca.loading[,2]*5.5, row.names(pca.loading), cex = 0.8, col = "black", pos = 4)

plot(df$PC1, df$PC2, xlim = c(-1,1), ylim = c(-1,0),
     xlab = "PC1", ylab = "PC2", 
     main = "TP removal",
     pch = 16, cex = abs(d$TP_percent_removal)/25+1, 
     col = df$col)
arrows(0,0, pca.loading[,1]*5, pca.loading[,2]*5, length = 0.1, lwd = 1.5, col = "black")
text(pca.loading[,1]*5.5, pca.loading[,2]*5.5, row.names(pca.loading), cex = 0.8, col = "black", pos = 4)



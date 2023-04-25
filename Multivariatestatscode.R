#Homework3
#Q1 Compute the descriptive statistics of the 3 response variables (Y1, Y2, and Y3), including 
#n, mean, median, std, min and max, for each level of advertising
summary(Newfood$Y1)
sd(Newfood$Y1)
length(Newfood$Y1)

summary(Newfood$Y2)
sd(Newfood$Y2)
length(Newfood$Y2)

summary(Newfood$Y3)
sd(Newfood$Y3)
length(Newfood$Y3)

#Low Advertising Summary stats
Low <- subset(Newfood, ADV=="L")
summary(Low$Y1)
sd(Low$Y1)
length(Low$Y1)

summary(Low$Y2)
sd(Low$Y2)
length(Low$Y2)

summary(Low$Y3)
sd(Low$Y3)
length(Low$Y3)

High <-subset(Newfood, ADV=="H")
summary(High$Y1)
sd(High$Y1)
length(High$Y1)

summary(High$Y2)
sd(High$Y2)
length(High$Y2)

summary(High$Y3)
sd(High$Y3)
length(High$Y3)

#Q2 Compute correlations among the three response variables, using all data
cor(Newfood$Y1,Newfood$Y2, method="pearson")
cor(Newfood$Y1, Newfood$Y3, method="pearson")
cor(Newfood$Y2, Newfood$Y3, method="pearson")
cor.test(Newfood$Y1,Newfood$Y2)
cor.test(Newfood$Y1, Newfood$Y3)
cor.test(Newfood$Y2, Newfood$Y3)

#histogram and corr graphs
hist(Newfood$Y1)
hist(Newfood$Y2)
hist(Newfood$Y3)
plot(Newfood$Y1~Newfood$Y2)
plot(Newfood$Y1~Newfood$Y3)
plot(Newfood$Y2~Newfood$Y3)
plot(Newfood$Y2~Newfood$Y1)
plot(Newfood$Y3~Newfood$Y1)
plot(Newfood$Y3~Newfood$Y2)

#Q3 Test univariate and multivariate normality of the three response variables, using all data
library(MVN)
mvn(Newfood[,2:4], mvnTest = "mardia", univariateTest = "SW", multivariatePlot = "qq", covariance = T)

#Q4 Conduct a Hotelling’s T2 test for H0: μL = μH vs Ha: μL ≠ μH at α = 0.05.
ICSNP::HotellingsT2(High[,2:4],Low[,2:4])

#Q5 Conduct a univariate t-test for each response variable for H0: μL = μH vs Ha: μL ≠ μH
#at α = 0.05.
t.test(High$Y1,Low$Y1, var.equal = T)
t.test(High$Y2, Low$Y2, var.equal = T)
t.test(High$Y3, Low$Y3, var.equal = T)

#Homework4
#Compute basic statistics for the 4 response variables and correlations between them
#(using the entire data).
summary(Fish2023$Aroma)
sd(Fish2023$Aroma)
length(Fish2023$Aroma)

summary(Fish2023$Flavor)
sd(Fish2023$Flavor)
length(Fish2023$Flavor)

summary(Fish2023$Texture)
sd(Fish2023$Texture)
length(Fish2023$Texture)

summary(Fish2023$Moisture)
sd(Fish2023$Moisture)
length(Fish2023$Moisture)

cor(Fish2023$Aroma,Fish2023$Flavor, method="pearson")
cor(Fish2023$Aroma,Fish2023$Texture, method="pearson")
cor(Fish2023$Aroma,Fish2023$Moisture, method="pearson")
cor(Fish2023$Flavor,Fish2023$Texture, method="pearson")
cor(Fish2023$Flavor,Fish2023$Moisture, method="pearson")
cor(Fish2023$Texture,Fish2023$Moisture, method="pearson")

#Compute basic statistics for the 4 response variables by each cooking method.
BBQ <- subset(Fish2023, METHOD=="BBQ")
Fry <- subset(Fish2023, METHOD=="Fry")
Steam <- subset(Fish2023, METHOD=="Steam")

summary(BBQ)
sd(BBQ$Aroma)
sd(BBQ$Flavor)
sd(BBQ$Texture)
sd(BBQ$Moisture)

summary(Fry)
sd(Fry$Aroma)
sd(Fry$Flavor)
sd(Fry$Texture)
sd(Fry$Moisture)

summary(Steam)
sd(Steam$Aroma)
sd(Steam$Flavor)
sd(Steam$Texture)
sd(Steam$Moisture)

#Test multivariate normality on the four variables (using the entire data).
library(MVN)
mvn(Fish2023[,2:5], mvnTest = "mardia", univariateTest = "SW", 
    multivariatePlot = "qq", covariance = T)

#Test the null hypothesis that there is no difference between the 3 cooking methods on the 
#four response variables using MANOVA.
Fish2023manova <- manova(cbind(Aroma, Flavor, Texture, Moisture)~METHOD, data=Fish2023)
summary(Fish2023manova)
summary.aov(Fish2023manova)
#Found there is a sig diff b/w the 3 cooking methods
library(rrcov)
WilksFish <- Wilks.test(METHOD~., data=Fish2023, method="c")
WilksFish
#Wilks-lambda version

#If MANOVA is significant, conduct an ANOVA on each response variable
summary.aov(Fish2023manova)

#Conduct multivariate and univariate contrasts to compare BBQ vs. Fry, BBQ vs. Steam, 
#and Fry vs. Steam on the four variables.
library(emmeans)
Fish2023.lm <- lm(cbind(Aroma, Flavor, Texture, Moisture) ~ METHOD, data = Fish2023)
Fish2023.emm <- emmeans(Fish2023.lm, ~ METHOD | rep.meas)
mvcontrast(Fish2023.emm, show.ests = TRUE) 
contrast(Fish2023.emm, show.ests = TRUE)

library(writexl)
estimates <- as.data.frame(contrasts$estimates)
tests <- as.data.frame(contrasts$tests)
write_xlsx(estimates, "C:\\Users\\ericr\\OneDrive\\Desktop\\SUNY ESF\\Spring 2023\\Multivariate Stats and SAS\\SAS_HW4\\estimates.xlsx")

#Compute the two canonical discriminant functions using e1 and e2. Interpret the 
#contribution of each response variable on the two linear combinations
#vignette("diabetes", package="candisc") was v. helpful
library(candisc)
Fishcan <- candisc(Fish2023.lm)
Fishcan
summary(Fishcan)
#eigenvectors in std coefficients section 

library(lda)
library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
linear <- can_lm(METHOD~., data = Fish2023)
linear
#Draw a scatterplot of Z2 vs Z1 (x-axis), and interpret the differences between the 3 
#cooking methods based on the meaning of Z1 and Z2.
plot(Fishcan, ellipse=T, var.lwd=2)
plot(linear)

#Homework5
#Data is normal
mvn(Mice[,4:6], mvnTest = "mardia", univariateTest = "SW", 
    multivariatePlot = "qq", covariance = T)

cor(Mice$X1, Mice$X2, method="pearson")
cor(Mice$X1, Mice$X3, method="pearson")
cor(Mice$X2, Mice$X3, method="pearson")

#Conduct MANOVA on the main effects of Temp and Sex, and the interaction 
#between the two factors using the three response variables.
#X = mu + A + B + (A*B) + e
library(mvnormtest)
library(car)
Mice$Sex <- as.factor(Mice$Sex)
Mice$Temp <- as.factor(Mice$Temp)

fit1 <- lm(cbind(X1,X2,X3) ~ Sex*Temp, data=Mice,
           contrasts=list(Sex=contr.sum, Temp=contr.sum))
Manova(fit1, type="III")

manovatable <- summary(manovaRes, multivariate=TRUE)
manovatable
#Conduct ANOVA on the main effects of Temp and Sex, and the interaction between the 
#two factors for each of the three response variables.
summary.aov(fit1)

#Conduct univariate contrasts for the levels of Sex and Temp for each variable.
library(emmeans)
mice.emm <- emmeans(fit1, ~Sex*Temp | rep.meas)
mvcontrast(mice.emm, show.ests = TRUE) 
contrast(mice.emm, show.ests = TRUE)

#obtain the multivariate mean plot of Z2 vs. Z1 
#using the eigenvectors in the MANOVA for 
#testing the interaction. 
library(candisc)
library(MASS)
Mice$Temp <- as.factor(Mice$Temp)
Mice$Sex <- as.factor(Mice$Sex)

library(MASS)

lda.mod <- lda(Sex:Temp ~ X1 + X2 + X3, data = Mice, scaled=T, method="moment")
lda.mod
ldacoeff <- (t(lda.mod$scaling))
ldacoeff
z1 <- c(ldacoeff[1,1]*Mice[,4]+ldacoeff[1,2]*Mice[,5]+ldacoeff[1,3]*Mice[,6])
z2 <- c(ldacoeff[2,1]*Mice[,4]+ldacoeff[2,2]*Mice[,5]+ldacoeff[2,3]*Mice[,6])
?gather
miceL1 <- gather(Mice, weight, value, X1, factor_key=T)
miceL1

discrim.plot<-cbind(z1,z2)

discrim.plot<- cbind(discrim.plot,miceL1)

means <- lda.mod[["means"]]
means
centroids <- aggregate(cbind(z1,z2) ~ Temp:Sex, Mice, mean)

centroids <- merge(centroids,mice, by="Temp")

centroids$Sex <- centroids$Sex.x



ggplot(discrim.plot, aes(z1, z2))+
  
  geom_point(aes(shape=Temp))+
  
  ggtitle("Dimentional Reduction of Temp, Sex, Responses")+
  
  geom_line(data=centroids, aes(z1,z2,color=Sex),size=2)+
  
  geom_point(data=centroids, aes(z1,z2,shape=),size=1)+
  
  geom_text(data=centroids, aes(z1,z2,color=Sex,label=Temp),color="Black",size=7)+
  
  theme_bw()
#these plots dont help, but look cool
pairs(fit1, fill=TRUE, fill.alpha=.1, var.cex=1.5)
scatterplotMatrix(mean(Mice[4:6]))

#Homework6
library(MVN)
Crimemvn <- mvn(Crime2022[,2:8], mvnTest = "mardia", univariateTest = "SW", 
                multivariatePlot = "qq", covariance = T)
library(writexl)
write_xlsx(Crimemvn, "C:\\Users\\ericr\\OneDrive\\Desktop\\SUNY ESF\\Spring 2023\\Multivariate Stats and SAS\\Crimemvn.xlsx")
library(corrr)
library(ggcorrplot)
library(FactoMineR)
CrimeNormal <- Crime2022[,2:8]
CrimeNormal <- scale(CrimeNormal)
corr_matrix <- cor(CrimeNormal)
corr_matrix
ggcorrplot(corr_matrix, hc.order = TRUE, 
           #type = "lower",
           lab = TRUE,
           ggtheme = ggplot2::theme_classic(),)

data.pca <- princomp(corr_matrix)

pca <- prcomp(Crime2022[,2:8], scale = T)
pca$rotation <- pca$rotation
rotation <- pca$rotation
rotation <- as.data.frame(rotation)

library("factoextra")
fviz_eig(pca, addlabels = TRUE)
fviz_pca_ind(pca, habillage = Crime2022$State)

pca <- prcomp(Crime2022[,2:8], scale = T)
pca$rotation <- pca$rotation
rotation <- pca$rotation
rotation <- as.data.frame(rotation)

library("factoextra")
fviz_eig(pca, addlabels = TRUE)
fviz_pca_ind(pca, habillage = Crime2022$State)

#Homework7
library(MVN)
Crimemvn <- mvn(Crime2022[,2:8], mvnTest = "mardia", univariateTest = "SW", 
                multivariatePlot = "qq", covariance = T)
Crimemvn
library(writexl)
write_xlsx(Crimemvn, "C:\\Users\\ericr\\OneDrive\\Desktop\\SUNY ESF\\Spring 2023\\Multivariate Stats and SAS\\Crimemvn.xlsx")

library(corrr)
library(ggcorrplot)
library(FactoMineR)

CrimeNormal <- Crime2022[,2:8]
CrimeNormal <- scale(CrimeNormal)
corr_matrix <- cor(CrimeNormal)
corr_matrix
ggcorrplot(corr_matrix, hc.order = TRUE, 
           lab = TRUE,
           ggtheme = ggplot2::theme_classic(),)
data.pca <- princomp(corr_matrix)
data.pca

env.pca <- rda(Crime2022[,2:8], scale = TRUE)
env.pca$CA$eig

pca <- prcomp(Crime2022[,2:8], scale = T)
pcarot <- as.data.frame(pca$rotation)
summary(pca)
write_xlsx(pcarot, "C:\\Users\\ericr\\OneDrive\\Desktop\\SUNY ESF\\Spring 2023\\Multivariate Stats and SAS\\pca.xlsx")

library("factoextra")
eig <- fviz_eig(pca, addlabels = TRUE)
fviz_pca_ind(pca, habillage = Crime2022$State)

library(psych)
library(nFactors)
library(ggplot2)
library(GPArotation)
df <- Crime2022[,2:8]                                 
df <- df[complete.cases(df),]                     
df_cor <- cor(df)
?fa
fitrot <- fa(r = df_cor, nfactors = 4, rotate = "quartimax",scores = TRUE)
fitrot
?fa
printme1 <- print(fitrot, digits=2, cutoff=.001, sort=F)  
fs <- factor.scores(df, fit)                       
fs <- fs$scores                                    
df <- cbind(Crime2022,fs)   


ggplot(df, aes(MR1, MR2,label=State))+
  geom_text()+
  theme_classic()

ggplot(df, aes(MR3, MR4,label=State))+
  geom_text()+
  theme_classic()

#Homework8
library(MVN)
mvn(Crime[,3:9], mvnTest = "mardia", univariateTest = "SW", 
    multivariatePlot = "qq", covariance = T)

West <- subset(Crime, Region == "W")
North <- subset(Crime, Region == "N")
South <- subset(Crime, Region == "S")
Midwest <- subset(Crime, Region == "M")
write.csv(sumMidwest,"C:\\Users\\ericr\\OneDrive\\Desktop\\SUNY ESF\\Spring 2023\\Multivariate Stats and SAS\\sumMidwest.csv")
?write.csv
summary(West)
sumNorth <- summary(North)
sumSouth <- summary(South)
sumMidwest <- summary(Midwest)

library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(ggpubr)
CrimeNormal <- Crime[,c(3:9)]
CrimeNormal <- scale(CrimeNormal)
corr_matrixCrime <- cor(CrimeNormal)
ggcorrplot(corr_matrixCrime, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           ggtheme = ggpubr::theme_classic2(),
           title = "Crime Correlation Matrix",
           lab_size = 4) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
  theme(legend.key.size = unit(0.4, 'cm')) +
  theme(plot.title=element_text(face="bold", 
                                size=15, hjust=0.6))

library(MASS)
lda.mod <- lda(Region ~ Murder + Rape + Robbery + Assault + Burglary + Larceny +
                 Autotheft, data = Crime, scaled=T, method="mve")
lda.mod
#I think boxM is better for homogeneity of covariances but double check
library(heplots)
boxM <- boxM(cbind(Murder, Rape, Robbery, Assault, Burglary, Larceny,
                   Autotheft) ~ Region, data = Crime)
boxM$cov
boxmsum <- summary(boxmsum)
write.csv(boxM$cov,"C:\\Users\\ericr\\OneDrive\\Desktop\\SUNY ESF\\Spring 2023\\Multivariate Stats and SAS\\boxmsum.csv")

Crime$Region <- factor(Crime$Region)
library(candisc)
library(emmeans)
Crime.lm <- lm(cbind(Murder, Rape, Robbery, Assault, Burglary, Larceny,
                     Autotheft) ~ Region, data = Crime)
CrimeCDA <- candisc(Crime.lm)
CrimeCDA
cdasum <- summary(CrimeCDA)
write.csv(cdasum,"C:\\Users\\ericr\\OneDrive\\Desktop\\SUNY ESF\\Spring 2023\\Multivariate Stats and SAS\\CDA.csv")

#individual states scores
CrimeCDA$scores
plot(CrimeCDA, ellipse=T, var.lwd=2)

CanWest <- subset(CrimeCDA$scores, Region == "W")
CanMidwest <- subset(CrimeCDA$scores, Region == "M")
CanNorth <- subset(CrimeCDA$scores, Region == "N")
CanSouth <- subset(CrimeCDA$scores, Region == "S")
?dist
dist(rbind(CanWest$Can1,CanMidwest$Can1), method = "euclidean")
dist(rbind(CanWest$Can1,CanNorth$Can1), method = "euclidean")
dist(rbind(CanWest$Can1,CanSouth$Can1), method = "euclidean")
dist(rbind(CanMidwest$Can1,CanNorth$Can1), method = "euclidean")
dist(rbind(CanMidwest$Can1,CanSouth$Can1), method = "euclidean")
dist(rbind(CanNorth$Can1,CanSouth$Can1), method = "euclidean")

dist(rbind(CanMidwest$Can1,CanMidwest$Can1), method = "euclidean")

maha <- mahalanobis(Crime[3:9], colMeans(Crime[3:9]), cov(Crime[3:9]))
maha
mahap <- pchisq(maha, df=3, lower.tail=FALSE)
mahap

#Homework9
summary(Crime)

library(MVN)
Crimemvn <- mvn(Crime2022[,2:8], mvnTest = "mardia", univariateTest = "SW", 
                multivariatePlot = "qq", covariance = T)
Crimemvn
library(writexl)
write_xlsx(Crimemvn, "C:\\Users\\ericr\\OneDrive\\Desktop\\SUNY ESF\\Spring 2023\\Multivariate Stats and SAS\\Crimemvn.xlsx")

library(corrr)
library(ggcorrplot)
library(FactoMineR)

CrimeNormal <- Crime[,3:9]
CrimeNormal <- scale(CrimeNormal)
corr_matrix <- cor(CrimeNormal)
corr_matrix
ggcorrplot(corr_matrix, hc.order = TRUE, 
           lab = TRUE,
           ggtheme = ggplot2::theme_classic(),)

library(cluster)
?pltree
hc2 <- agnes(CrimeNormal, method = "complete")
hc2
summary(hc2)
pltree(hc2, cex = 0.6, hang = -1, main = "Crime Dendogram with Complete")
hc3 <- agnes(CrimeNormal, method = "ward")
hc3
pltree(hc3, cex = 0.6, hang = -1, main = "Crime Dendogram with Wards")
#best to use wards with 2 clusters

#clusters data into the 2 groupings made by the cluster analysis
clust <- cutree(hc3, k = 2)
library(factoextra)
fviz_cluster(list(data = Crime[3:9], cluster = clust))



library(heplots)
boxM <- boxM(cbind(Murder, Rape, Robbery, Assault, Burglary, Larceny,
                   Autotheft) ~ Region, data = Crime)
boxM$cov
summary(boxM)
Crime$Region <- factor(Crime$Region)
library(candisc)
library(emmeans)
Crime.lm <- lm(cbind(Murder, Rape, Robbery, Assault, Burglary, Larceny,
                     Autotheft) ~ Region, data = Crime)
CrimeCDA <- candisc(Crime.lm)
CrimeCDA
plot(CrimeCDA, ellipse=T, var.lwd=2)

#From here determine the differences in the 2 groupings: are the groupings of the states similar in
#each plot or different, and how are they different?

#Homework10
summary(Set1)
summary(Set2)
library(tidyverse)
library(ggpubr)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
SetNormal <- Set[,c(2:10,12:20)]
SetNormal <- scale(SetNormal)
corr_matrix <- cor(SetNormal)
ggcorrplot(corr_matrix, hc.order = F, 
           lab = TRUE,
           lab_size = 3,
           ggtheme = ggplot2::theme_classic(),)

Set <- merge(Set1, Set2, by = "ID")
Set <- Set[,-21]
library(psych)
library(GGally)
library(CCA)

cc1 <- cc(Set[2:10], Set[12:20])
#raw scores
cc1[3:4]
cc2 <- comput(Set[2:10], Set[12:20], cc1)
#loadings
cc2[3:6]

library(CCP)
# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, 
#and number of variables in the second set.
n <- dim(Set1[2:10])[1]
p <- length(Set1[2:10])
q <- length(Set[12:20])

## Calculate p-values using the F-approximations of 
#different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")

#RDA

library(candisc)
cc <- cancor(Set[2:10], Set[12:20], set.names=c("Set1", "Set2"))
ccrda <- redundancy(cc)
ccrda

plot(ccrda$Xcan.redun, ccrda$Ycan.redun)
#usually make biplot, but this data was all +

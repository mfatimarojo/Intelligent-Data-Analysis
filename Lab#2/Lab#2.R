# Importing data sets
medifis=read.table("medifis.txt")
colnames(medifis)=c("gr","hg","wg","foot","arm","back","skull","knee")
hbat=read.csv("hbat.csv",header=TRUE,sep=",")
# if we have imported the id column as first column, we delete it
head(hbat)
hbat=hbat[-1]

# Covariance and Correlation matrix
cov(medifis[,-1])
r=cor(medifis[,-1])
r# which variables correlate the most?
diag(r)=0
which(r==max(abs(r)), arr.ind=TRUE)
# Correlation coefficients and tests
library(Hmisc)
rcorr(as.matrix(medifis[,-1]))

# Pairwise correlations with p-values (package RcmdrMisc)
library(RcmdrMisc)
rcorr.adjust(medifis[,-1])

# Visualizing correlations: Package corrplot
library(corrplot)
corrplot(cor(medifis[,-1]))
corrplot.mixed(cor(medifis[,-1]))
# reordering a correlation matrix
corrplot.mixed(cor(medifis[,-1]),order="AOE" )
corrplot.mixed(cor(medifis[,-1]),order="FPC" )
corrplot(cor(medifis[,-1]), order="hclust",hclust.method="ward.D", addrect=3)

#Visualizing: Package corrgram
library(corrgram)
corrgram(medifis[,-1])

corrgram(medifis[,-1],
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrgram(medifis[,-1], order=TRUE,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrgram(medifis[,-1], order=TRUE,
         lower.panel=panel.shade, upper.panel=panel.pts,
         diag.panel=panel.minmax, text.panel=panel.txt)

#Partial correlations, package ppcor. Detailed output
library(ppcor)
pcor(medifis[,-1])
matrix.partial=pcor(medifis[,-1])$estimate

#Visualizing partial correlations
corrgram(matrix.partial,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrplot.mixed(matrix.partial,order="AOE" )


#Define a function r2multv for squared multiple correlation coeficients (r-squared)

r2multv<-function(x){
r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
r2s
}

#use it on data set "medifis"
r2multv(medifis)
# What is the variable "more linearly explained" by the others?
#use this function on hbat dataset
r2multv(hbat[,6:22])

#In this data set, important linear relationships are present. Let's calculate the determinant of S and R:
det(cor(hbat[,6:22]))
det(cov(hbat[,6:22]))

#Can you find the variables involved in the overall linear dependence?
eigen(cov(hbat[,6:22]))
# However, linear pairwise correlations between those variables are not very strong
cor(hbat[,c(11,17,18)])
#But R^2's are
r2multv(hbat[,c(11,17,18)])
    
# Effective dependence coefficient
1-det(cor(medifis[,-1]))^{1/6}
#Variables from 6 to 19 from hbat
1-det(cor(hbat[,6:19]))^{1/13}
#Variables from 6 to 22 from hbat
1-det(cor(hbat[,6:22]))^{1/16}

# Testing normality assumptions
# We can use Shapiro-Wilk's test for small samples $n<50$. Lilliefors' test (package nortest) for bigger samples.
library(nortest)
lillie.test(hbat[,8])
shapiro.test(medifis[,3])
# Any conclusion?
#Other tests include Anderson-Darling test, Cramer Von-Misses and Jarque-Bera (tseries package, based on a joint statistics of skewness and kurtosis)
cvm.test(hbat[,8])
library(tseries)
jarque.bera.test(hbat[,8])
# The Jarque-Bera and Shapiro-Wilk seem to work significantly better when testing normality

#### Test for skewness and kurtosis (if normality is failing, for instance)
library(moments)
anscombe.test(hbat[,6])  
#kurtosis
agostino.test(hbat[,6])
# Why is failing normality in variable hbat[,6]?

#We can try a transformation on hbat[,6]
jarque.bera.test(hbat[,6])
jarque.bera.test(hbat[,6]^2)

# Variable transformation, new data rdoorcl and rdoorcl	
rdoorcl=read.table("rdoorcl.txt")
rdoorop=read.table("rdoorop.txt")
library(car)
qqPlot(rdoorcl$V1, dist="norm")
shapiro.test(rdoorcl$V1)
# qqPlot is designed to give us the information regarding tests on normality. IT's still possible to use
# package ggplot2 for plotting, but you get less information
# The line depicted in qqPlot has to do with the mean and standard deviation of the data
ggplot(rdoorcl, aes(sample=rdoorcl$V1))+stat_qq()+geom_abline(intercept=0.128,slope=.1)
mean(rdoorcl$V1)
sd(rdoorcl$V1)


#boxcox function, package MASS
library(MASS)
boxcox(rdoorcl$V1~1)
#values of previous function
boxcox(rdoorcl$V1~1,plotit=FALSE)
#finding the maximum of the likelihood function
max1=boxcox(rdoorcl$V1~1,plotit=FALSE)
max1=data.frame(max1)
max1[which.max(max1$y),]

#Another more direct way to find that value along with some statistical tests using function powerTransform (package "car")
powerTransform(rdoorcl$V1)
# and more information
summary(powerTransform(rdoorcl$V1))
#We make a variable transformation using lambda=0.27
rdoorclt=bcPower(rdoorcl, lambda=0.27)
#and check if it improves normality
#Comparing both qqplots
par(mfrow=c(1,2))
qqPlot(rdoorcl$V1, dist="norm")
qqPlot(rdoorclt$V1, dist="norm")
par(mfrow=c(1,1))
#Cheking improvement of normality
jarque.bera.test(rdoorcl$V1)
jarque.bera.test(rdoorclt$V1)
#Since we cannot reject the logarithmic transformation, let's check if it works (easier to interpret)
jarque.bera.test(log(rdoorcl$V1))

#To test a particular value for \lambda, we can use the function testTransform acting on a powerTransform object

testTransform(powerTransform(rdoorcl$V1~1), lambda=1)
testTransform(powerTransform(rdoorcl$V1~1), lambda=0.3)

# Repeat the whole analysis with variable rdoorop$V1 to check if the transformation improves normality

# Bivariant Normality for the joint variable (rdoorcl$V1,rdoorop$V1)
# Estimating bivariate parameter (\lambda_1,\lambda_2)
powerTransform(cbind(rdoorcl$V1,rdoorop$V1))
# Variables plotted in the transform scale
plot(powerTransform(cbind(rdoorcl$V1,rdoorop$V1)~1))
summary(powerTransform(cbind(rdoorcl$V1,rdoorop$V1)~1))

# Transformations to Multinormality 
# Although We can accept the logarithmic transformation for both variables, 
# we are going to transform them with (\lambda_1,\lambda_2) values =c(0.16, 0.15).
# Defining the transformed variable with those lambdas
rdoorT=bcPower(cbind(rdoorcl$V1,rdoorop$V1), c(0.16,0.15))

# Redefining some graphical parameters to combine multiple plots into one overall graph
par(mfrow=c(1,2))

#Comparing fitting to a normal distribution before and after the transformation with Mardia test (package MVN)
library(MVN)
#Before
mardiaTest(cbind(rdoorcl$V1,rdoorop$V1), qqplot=T)
# We reject normality given p values equal to 0 for skewness and kurtosis
#After
mardiaTest(rdoorT, qqplot=T)
#We have improved normality

## Exploring multivariate Normality for (hbat[,6],hbat[7])
mardiaTest(cbind(hbat[,6],hbat[,7]))
summary(powerTransform(cbind(hbat[,6],hbat[,7])~1))
mardiaTest(cbind(hbat[,6],log(hbat[,7])))

## Outliers

# Univariate identification with boxplots. Package car:
par(mfrow=c(1,1))
#It gives you the observation index classified as outlier
Boxplot(hbat[,7], id.method="y")

# with default built-in functions
boxplot(hbat[,7])
#It gives you the observation classified as outlier
boxplot(hbat[,7])$out

# Multivariate outliers, "by hand" with Mahalanobis distance, non-robust
# variables hbat[,6:18]
hbat618=hbat[,6:18]
# mean vector, cov matrix and squared mahalanobis distance
meanhbat618=sapply(hbat618, mean)
covhbat618=cov(hbat618)
mahalanobis618=mahalanobis(hbat618,meanhbat618,covhbat618)
mahalanobis618
# 95th percentile of a chi-squared distribution with 13 degrees of freedom (we are using 13 variables)
#Position of outliers
which(mahalanobis618 > qchisq(0.95,df=13))
#We got 6 outliers, their rows in the data set
pos=which(mahalanobis618 > 22.36)
pos
mahalanobis618[pos]

## To plot outliers in a different color
x=rep(1, 100)
x[pos]=0
# We plot them on a scatterplot of variables 6 and 7 (they are outliers for the whole set of variables 6:18).
plot(hbat[,6],hbat[,7],col=x+2,pch=16)
# Visual identification, function qqPlot package car
qqPlot(mahalanobis618,dist="chisq",df=13, line="robust", id.method="identify")

#Package mvoutlier
library(mvoutlier)
##### pcout function, robust method based on principal components
hb618.out=pcout(hbat[,6:18], makeplot=TRUE)
# which potential outliers does it find?
which(hb618.out$wfinal01==0)

# plotting each point with each final combined weighed. Small values indicate potential multivariate outliers.
plot(seq(1,100),hb618.out$wfinal)

# Bivariate graphic highlighting outliers in red
plot(hbat[,6],hbat[,7],pch=16,col=hb618.out$wfinal01+2) 

# Interactive chi-squared plot
chisq.plot(hbat[,6:18])

#Four graphs
aq.plot(hbat[,6:18])

# changing the default quantile
which(aq.plot(hbat[,6:18],delta=qchisq(0.95,df=13))$outliers=="TRUE")
which(aq.plot(hbat[,6:18])$outliers=="TRUE")

par(mfrow=c(1,1))
# Symbol plot, bivariate outliers (only for two variables)
symbol.plot(cbind(hbat[,16], hbat[,17]))
#distance plot, comparing Mahalanobis distance with robust Mahalanobis distance. With robust estimates, outliers 
#stick out much more.
dd.plot(cbind(hbat[,16], hbat[,17]))

#correlation plot, for two variables
corr.plot(hbat[,16], hbat[,17])
# Other plot for no more than 10 variables
uni.plot(hbat[,6:12])


## Function spm, package car. Very complete, see parameter "transform=TRUE".
spm(hbat[,6:11],reg.line=lm, diagonal="histogram", smoother=FALSE, spread=FALSE, ellipse=TRUE, transform=TRUE)

library(GGally)
ggpairs(medifis)
medifis$gr=as.factor(medifis$gr)
ggpairs(medifis, lower = list(continuous="points",combo="facetdensity",mapping=aes(color=gr)))

## Explore the linear structure of the numeric variables in the set cereal. 
#calories, protein, fat, sodium, fiber, carbo, sugars, potass, vitamins
#Explore differences between correlations and partial correlations.


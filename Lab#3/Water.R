water=read.table("water.txt")
colnames(water)=c("code","long", "lat", "TMerc","MetMerc", "Turb", "TPhos","MercFish")
head(water)

# Correlations and partial correlations
cor(water[-c(1,2,3)])
library(ppcor)

library(Hmisc)
rcorr(as.matrix(water[,-c(1,2,3)]))

cor(water[c(4,5,6)])
pcor(water[c(4,5,6)])


cor(water[-c(1,2,3)])
pcor(water[-c(1,2,3)])

#####
eigen(cor(water[-c(1,2,3)]))
## Effective dependence coefficient
1-det(cor(water[-c(1,2,3)]))^(1/4)

r2multv<-function(x){
r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
sort(r2s, decreasing=TRUE)
}
r2multv((water[-c(1,2,3)]))

###
library("GGally")
ggpairs(water[-c(1,2,3)])
summary(water$V7)
which(water$V7==max(water$V7))
which(water$V6==max(water$V6))
cor(water$V6[-1], water$V7[-1])
cor(water$V6, water$V7)
library("tseries")
jarque.bera.test(water$V4)
jarque.bera.test(log(water$V4))

# Transformations
library("MASS")
library("car")
# Log transf improves normality
summary(powerTransform(water[,4]))
qqPlot(water[,4], dist="norm")
qqPlot(log(water[,4]), dist="norm")
summary(powerTransform(cbind(water[,4],water[,5])~1))
plot(powerTransform(cbind(water[,4],water[,5])~1))
transf45=bcPower(cbind(water[,4],water[,5]), c(0,0))

library("MVN")
mardiaTest(cbind(water[,4],water[,5]), qqplot=T)
mardiaTest(transf45, qqplot=T)
# Inspecting the plots, we see that we have improved bivariate normality. Tests don't say so, but it is the best
# we can achieve with transformations
hzTest(transf45)

# outliers
library("mvoutlier")
water.out=pcout(water[,4:8], makeplot=TRUE)
# Bivariate graphic highlighting mutivariate outliers in red
plot(water[,4],water[,5],pch=16,col=water.out$wfinal01+2) 

# Symbol plot, bivariate outliers (only for two variables)
symbol.plot(cbind(water[,4], water[,5]))
symbol.plot(cbind(log(water[,4]), log(water[,5])))
#distance plot, comparing Mahalanobis distance with robust Mahalanobis distance. With robust estimates, outliers 
#stick out much more.
dd.plot(cbind(water[,4], water[,5]))
dd.plot(cbind(log(water[,4]), log(water[,5])))

# A dataset with a low eigenvalue for matrix R
teen=read.table("T4-6.txt")
colnames(teen)=c("Indep","Supp","Benev","Conform","Leader","Gender","Socio")
head(teen)

eigen(cov(teen[,1:5]))
eigen(cor(teen[,1:5]))

vector=eigen(cor(teen[,1:5]))$vectors[,5]
l=t(as.matrix(vector))%*%t(as.matrix(scale(teen[,1:5])))

l=as.vector(l)

# The variance of the linear combination is the eigenvalue
var(l)


# PCA on the Happy Planet Index
library("dplyr")
library("Lock5Data")
h_pca_data=dplyr::select(HappyPlanetIndex,Happiness,LifeExpectancy,Footprint,HLY,HPI,GDPperCapita,HDI,Population,Region)
library("FactoMineR")
h_pca_data=na.omit(h_pca_data)
h_pca_data$Region=as.factor(h_pca_data$Region)
h_pca=PCA(h_pca_data, quali.sup=9, scale.unit=TRUE)
summary(h_pca)

dimdesc(h_pca)

# Variable Loadings (eigenvectors)
happy_var=sweep(h_pca$var$coord,2,sqrt(h_pca$eig[1:ncol(h_pca$var$coord),1]),FUN="/")
happy_var

library(factoextra)
fviz_pca_ind(h_pca,  habillage="Region")

fviz_pca_var(h_pca, col.var="contrib")

var.contrib=h_pca$var$contrib
var.contrib=h_pca$var$contrib
my.grid=expand.grid(x=rownames(var.contrib), y=colnames(var.contrib))
my.grid$values= c(var.contrib[,1], var.contrib[,2], var.contrib[,3], var.contrib[,4], var.contrib[,5])
ggplot(my.grid, aes(x=x, y=values))+geom_bar(stat="identity", aes(fill=y), position=position_dodge())
ggplot(my.grid, aes(x=y, y=values))+geom_bar(stat="identity", aes(fill=x), position=position_dodge())

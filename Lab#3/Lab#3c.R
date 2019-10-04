#check you have the correct data set
cereals <- read.table("cerealdata.txt", header=TRUE, as.is=TRUE, na.strings="-1")
cereals1=na.omit(cereals)
rownames(cereals1)=abbreviate(cereals1$Name)
library("dplyr")
cereal.pc=select(cereals1,calories,protein,fat,sodium,fiber,carbo,sugars,potass,shelf)
cereal.pc=na.omit(cereal.pc)
head(cereal.pc)
cereal.pc$shelf=as.factor(cereal.pc$shelf)

#summary of the variables, shelf is a factor
#Contribution: fiber >> potass > protein > carbo are the variables with most contribution
summary(cereal.pc)
str(cereal.pc)

#covariance matrix, very different variances
cov(cereal.pc[,-9]) #They weay you tell R, you are going to use every variable unless 9
# SODIUM = 6850.83765272 is enough reason to use R maxtrix instead of S matrix

# Before starting with PCA, a nice scatterplot matrix
library(ggplot2)
library(GGally) #Very useful to use this function to show nice (1.3)
ggpairs(cereal.pc, lower = list(continuous="points",combo="facetdensity",mapping=aes(color=shelf)))
#Boxplots are very useful to really see the differences between shelfs

#load FactoMineR and perform a PCA analysis on matrix R, not S. Store the results in object cereal_pca_r. We are using shelf as
# a supplementary qualitative variable. By default 5 components are calculated, use ncp= to change it.
library(FactoMineR)
cereal_pca_r=PCA(cereal.pc,quali.sup=9,scale.unit=TRUE, graph=FALSE) #Creating an object to store PCA
#Use 9 column for qualitative.sup

# by default dimensions 1 and 2 are plotted. We are using that option. To change them use axes=c(1,3).
# type cereal_pca_r to see the extensive list of results provided in the output of PCA()
cereal_pca_r

#summary of the numerical output
summary(cereal_pca_r)

#Working on the map of points, representation of individuals
plot(cereal_pca_r, cex=0.7)

# coloring points by variable shelf
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9)
# change individual color by group
library(factoextra)  #Useful to find clusters
fviz_pca_ind(cereal_pca_r,  habillage="shelf")
fviz_pca_ind(cereal_pca_r,  label="none", habillage="shelf")

# if we don't want shelfs on the plot
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali")
#labels for those points with cosin squared greater than 0.7 (in any of the 2 first PC), for example
#cos2, the more value (the closer to 1) the best explains the observation that dimension
#ctr, contribution of the observation in that dimension
#Dim = coordinates in axis
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7") #Only shows those ones with cos^2>0.7
# plotting only previously selected observations
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7", unselect=1) #No mostrar clumn 1 Name
#selecting a color for unselected points
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7", unselect="grey70")
# To select the five observations that contribute the most to the two first components, the more extreme individuals in both components
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 5") #The five with the greatest cos^2
#selecting particular individuals by their names
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9, invisible="quali", select=c("1", "2", "3"))
#selecting particular individuals by their row in the dataset
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9, invisible="quali", select=1:10)
# Control automatically the color of individuals using the cos2 values (the quality of the individuals on the factor map)
 fviz_pca_ind(cereal_pca_r, col.ind="cos2", repel=TRUE) +
 scale_color_gradient2(low="white", mid="blue", 
                      high="red", midpoint=0.50)
# What if I want the previous plot but showing also the shelf each cereal is on? Solution at the end of the script 

## Working on variables, circle of correlations
 #The closer the circle the better they are reperesented by the principal components
 # We can see if variables are correlated as calories and sugar or potass and fiber, so
 # their angles are close between them and their arrows have similar length
plot(cereal_pca_r, choix="var")
## or with package factoextra
fviz_pca_var(cereal_pca_r)

# Ploting the 3 variables that contribute the most to the representation 
plot(cereal_pca_r, shadow=TRUE,choix="var", select="contrib 3" )
# selecting variables by their contributions, quality of representation greater than 0.7
plot(cereal_pca_r, shadow=TRUE,choix="var", select="cos2 0.7" )
# control variable colors using their contribution
#Maybe the second component is telling you something about health and unhealth contents
fviz_pca_var(cereal_pca_r, col.var="contrib")

# Change the gradient color
fviz_pca_var(cereal_pca_r, col.var="contrib")+
 scale_color_gradient2(low="white", mid="blue", 
                       high="red", midpoint=55)+theme_bw()

# Quality of each variable representation on these two dimensions
# The closer to 1, the better they are represented by these two components
 cereal_pca_r$var$cos2[,1:2][,1]+ cereal_pca_r$var$cos2[,1:2][,2]

# First a table, then a barplot of variable contribution
rbind(cereal_pca_r$var$contrib, TOTAL=colSums(cereal_pca_r$var$contrib))
barplot(t(cereal_pca_r$var$contrib), beside=TRUE, legend.text=TRUE,args.legend=list(x="topleft", bty='n', ncol=5))
#with colors
barplot(t(cereal_pca_r$var$contrib), beside=TRUE, legend.text=TRUE,args.legend=list(x="topleft", bty='n', ncol=5), col=c(1:8))
barplot(cereal_pca_r$var$contrib, beside=TRUE, legend.text=TRUE,args.legend=list(x=55,y=50, bty='n', ncol=5), col=c(1:8))

### Objects contribution
objcontrib=data.frame(C1=cereal_pca_r$ind$contrib[,1],C2=cereal_pca_r$ind$contrib[,2],n=rownames(cereal.pc))

# Barplots of object contributions to PC 1
par(mfrow=c(2,1))
barplot(objcontrib$C1[1:36],cex.names=0.7, names.arg=objcontrib$n[1:36])
abline(h=100/74, col="gray50")
barplot(objcontrib$C1[37:74],cex.names=0.7, names.arg=objcontrib$n[37:74])
abline(h=100/74, col="gray50")
# or with ggplot and package gridExtra
library(gridExtra)
G1=ggplot(objcontrib[1:36,],aes(x=n, y=C1))+geom_bar(position=position_dodge(), stat="identity", fill="steelblue")+geom_text(aes(label=n), vjust=1.6, color="white", size=2.5, position=position_dodge(0.9))+geom_hline(yintercept=100/74)
G2=ggplot(objcontrib[31:74,],aes(x=n, y=C1))+geom_bar(position=position_dodge(), stat="identity", fill="steelblue")+geom_text(aes(label=n), vjust=1.6, color="white", size=2.5, position=position_dodge(0.9))+geom_hline(yintercept=100/74)
grid.arrange(G1,G2,nrow=2)
# (compare them with the position of each point in the observations plot)

#If you want to order observations by the first component value (score), make a data frame with the two first scores, for instance 
obsor=data.frame(C1=cereal_pca_r$ind$coord[,1],C2=cereal_pca_r$ind$coord[,2])
head(obsor[order(obsor[,1], decreasing=TRUE),])

#scree plot
par(mfrow=c(1,1))
plot(cereal_pca_r$eig$eigenvalue, type="l")
points(cereal_pca_r$eig$eigenvalue)
# scree plot (barplot type)
barplot(cereal_pca_r$eig$eigenvalue, names.arg=rownames(cereal_pca_r$eig))

#biplot: observation and variables together in the same plot
fviz_pca_biplot(cereal_pca_r)

#loadings (eigenvectors), use function princomp() because it seems FactoMineR doesn't provide them directly:
#Look for de loadings: the 1st linear combination (1st pc)
# 
princomp(cereal.pc[-9], cor=TRUE)$loadings

# but it does doing some calculations
cerealvar=sweep(cereal_pca_r$var$coord,2,sqrt(cereal_pca_r$eig[1:ncol(cereal_pca_r$var$coord),1]),FUN="/")

#correlation coefficients between variables and dimensions and significance tests
# For the first (second) component, are there any differences in the categorical variable?
dimdesc(cereal_pca_r,axes=c(1,2)) # Dimension description of the pc, for the first 2 dimensions
concat1 = cbind.data.frame(cereal.pc[,9],cereal_pca_r$ind$coord[,1:2])
boxplot(concat1[,2]~concat1[,1])

##Ellipses
#This function draws confidence ellipses around the categories of a supplementary categorical variable. The objective is to see 
#whether the categories of a #categorical variable are significantly different from each other.
#It uses a data set with the categorical variable and the coordinates of the individuals on the principal components.
plotellipses(cereal_pca_r,9) 

## reconstruction of the original data matrix usig 2 PC's
reconstruction1 = reconst(cereal_pca_r,ncp=2)
coeffRV(reconstruction1, cereal.pc[,1:8])

## Clustering partition, using hierarchical clustering
cereal_cluster=hclust(dist(cereal_pca_r$ind$coord), method="ward.D")
plot(cereal_cluster)
rect.hclust(cereal_cluster, k = 3, border ="blue")
cereal_clusters=cutree(cereal_cluster, k=3)
cereal_clusters

#PC plot with clustering partition
cereal_pca_obs=data.frame(C1=cereal_pca_r$ind$coord[,1],C2=cereal_pca_r$ind$coord[,2], cluster=factor(cereal_clusters))
ggplot(cereal_pca_obs, aes(x=C1,y=C2, label=rownames(cereal.pc)))+
 geom_hline(yintercept=0, color="gray70")+
 geom_vline(xintercept=0,color="gray70")+
 geom_point(aes(color=cluster), alpha=0.55, size=2)+
geom_text(aes(color=cluster),alpha=0.55)

## Univariate clustering, with PC1, using mclust. First install package "mclust"
library(mclust)
mcl4=Mclust(cereal_pca_r$ind$coord[,1])
summary(mcl4)
par(mfrow=c(2,2))
plot(mcl4)


#############################################################################
### BIPLOTGUI, transform shelf again to numerical values
cereal.pc.b=cereal.pc
cereal.pc.b$shelf=as.numeric(cereal.pc.b$shelf)
Biplots(Data=cereal.pc.b[,-9])
# follow instructions in class

# This is a function that provides the p-value of the test for the null
# hypothesis that the first q PC's are adequate in representing a certain
# proportion of the total variation in the data. It can be obtained using the material in
# Anderson (1984)'s book titled "An Introduction to Multivariate Statistical
# Analysis".  Ranjan Maitra (03/30/2012)

#This is an inference result and therefore multivariate normality is required. 
#The data we are using can't be assumed to have this distribution.

PCs.proportion.variation <- function(lambda, q = 1, propn, nobs)
  {
    den <- sum(lambda) # sum of all the eigenvalues
    num <- sum(lambda[1:q]) # sum of the first q eigenvalues
    if (num/den >= propn) return(1)
    else {
      se <- sqrt(2 * sum(lambda[-(1:q)])^2 * sum(lambda[1:q]^2) +
                 2 * sum(lambda[1:q])^2 * sum(lambda[-(1:q)]^2)) /
                   (nobs * den^2)
                                        #asymptotic sd of the test statistic
      test.stat <- (num/den - propn)/se
      return(pnorm(test.stat))
    }
  }

lambda3=cereal_pca_r$eig[,1]
PCs.proportion.variation(lambda=lambda3, q=3,propn=0.78, nobs=65)

PCs.proportion.variation(lambda=lambda3, q=3,propn=0.77, nobs=65)

## Barplot of variable contribution with ggplot
var.contrib=cereal_pca_r$var$contrib
my.grid=expand.grid(x=rownames(var.contrib), y=colnames(var.contrib))
my.grid$values= c(var.contrib[,1], var.contrib[,2], var.contrib[,3], var.contrib[,4], var.contrib[,5])
ggplot(my.grid, aes(x=x, y=values))+geom_bar(stat="identity", aes(fill=y), position=position_dodge())
ggplot(my.grid, aes(x=y, y=values))+geom_bar(stat="identity", aes(fill=x), position=position_dodge())

#### Solution to ***
newdf=data.frame(col=cereal_pca_r$ind$cos2[,1]+cereal_pca_r$ind$cos2[,2],shape=cereal.pc$shelf, 
                 coord1=cereal_pca_r$ind$coord[,1],coord2=cereal_pca_r$ind$coord[,2], col2=cereal_clusters)

ggplot(data=newdf, aes(x=coord1, y=coord2, colour=col, shape=shape))+
    geom_text(aes(label=rownames(newdf)), hjust=1.5)+geom_jitter()

ggplot(data=newdf, aes(x=coord1, y=coord2, colour=as.factor(col2), shape=shape))+
  geom_text(aes(label=rownames(newdf)), hjust=1.5)+geom_jitter()




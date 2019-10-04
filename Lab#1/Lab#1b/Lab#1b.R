# data cereals, import, change column names, remove NA's, define shelf as a factor
cereals <- read.table("cereal.txt", header=FALSE, as.is=TRUE, na.strings="-1")
names(cereals) <- c('name','mfr','type','calories','protein','fat','sodium','fiber','carbo',
        'sugars','shelf','potass','vitamins','weight','cups')
cereals=na.omit(cereals)
cereals$shelf <- factor(cereals$shelf, levels=1:3, ordered=TRUE)

# We also create a factor from variable vitamins
table(cereals$vitamins)
cereals$vitC <- factor(cereals$vitamins, levels=c(0,25,100), ordered=TRUE)

# create a ggplot object which will serve as the basis
# for a scatter plot of fiber vs potass
bs=ggplot(cereals, aes(x=fiber,y=potass))+ggtitle("Fiber vs Potassium")
bs
# adding layers
# scatterplot
bs+geom_point()
bs+geom_point(aes(shape=shelf))
bs+geom_point(size=2,aes(shape=shelf))

bs+geom_point(aes(color=shelf))
bs+geom_point(aes(color=shelf, size=vitamins))
# This last plot is not accurate because vitamins has only three values: 0, 25 100. Try these ones.
bs+geom_point(size=2,aes(shape=shelf,color=vitC))
bs+geom_jitter(size=2,aes(shape=shelf,color=vitC))


# you have 5 variables represented in the following plot, which may not be a good idea.
bs+geom_point(aes(color=fat, size=vitamins, shape=shelf))

#ggplot2 is mainly intended to produce graphics
# of more than one variable. If you want to get univariate
# charts, probably ggplot2 may not be the best option, but still
#histogram, default histogram
ggplot(data = cereals, aes(x = calories)) +geom_histogram()

#histogram, changing bindwith
ggplot(data = cereals, aes(x = calories)) +geom_histogram(binwidth=13)
ggplot(data = cereals, aes(x = calories)) +geom_histogram(binwidth=13, color="white")
ggplot(data = cereals, aes(x = calories)) +geom_histogram(binwidth=13, color="white", aes(fill=..count..))
#more histograms
ggplot(data = cereals, aes(x = calories, y=..density.., fill=shelf)) +geom_histogram(binwidth=15)
ggplot(data = cereals, aes(x = calories, y=..density.., colour=shelf)) +geom_freqpoly(binwidth=15)

#boxplots
ggplot(cereals, aes(x=shelf,y=sugars))+geom_boxplot()
#let's add some color
ggplot(cereals, aes(x=shelf,y=sugars, fill=shelf))+geom_boxplot()
#let's add points
ggplot(cereals, aes(x=shelf,y=sugars, fill=shelf))+geom_boxplot()+geom_point()
#let's jitter the points
ggplot(cereals, aes(x=shelf,y=sugars, fill=shelf))+geom_boxplot()+geom_jitter()
# You can use a different parameter inside the boxplot function, for instance, in each shelf split every boxplot by another
# categorical variable. Since we cannot use "type", and "mfr" has many different values, let's see how to create a categorical variable # from a continuous one. Intervals [0,220),[220,321):
cereals$sodiumC=cut(cereals$sodium, c(0,220, 321), right=FALSE, labels=c("LowNa", "HighNa"))
# two different displays of the same variables, reordering the boxplots, compare them
ggplot(cereals, aes(x=sodiumC, y=carbo))+geom_boxplot(aes(fill=shelf))
ggplot(cereals, aes(x=shelf,y=carbo, fill=shelf))+geom_boxplot(aes(fill=sodiumC))

# densities, smooth version of the histogram
ggplot(cereals, aes(x=sugars,group=shelf))+geom_density()
ggplot(cereals, aes(x=sugars,group=shelf))+geom_density(aes(fill=shelf), alpha=0.7, kernel="gaussian")

#barplots
# vitamins by shelf
ggplot(cereals,aes(x=shelf,fill=factor(vitamins)))+geom_bar()
# changing the position of bars
ggplot(cereals,aes(x=shelf,fill=factor(vitamins)))+geom_bar(position=position_dodge())
# distribution of fiber by shelf
ggplot(cereals,aes(x=shelf,fill=factor(fiber)))+geom_bar()
# manufacturer distribution by shelf
ggplot(cereals,aes(x=shelf,fill=factor(mfr)))+geom_bar(position=position_dodge())

#adding different regression lines to each subset in shelf
ggplot(cereals, aes(x=fiber,y=potass, color=factor(shelf)))+geom_point()+geom_smooth(method="lm")

# If I were to predict linearly calories(Y) from sugars(X), would the models be the same from different shelfs? And manufacturers?
ggplot(cereals, aes(x=sugars,y=calories, color=factor(shelf)))+geom_point()+geom_smooth(method="lm")
ggplot(cereals, aes(x=sugars,y=calories, color=factor(mfr)))+geom_point()+geom_smooth(method="lm")

#Facet grid Faceting allows to split one plot into multiple plots based on 
# some factor.
bs+geom_point(aes(color=shelf))+facet_grid(~vitamins)
bs+geom_point(aes(color=shelf))+facet_grid(vitamins~.)
bs+geom_point(aes(color=shelf))+facet_grid(~mfr)
#relationship between fiber and potassium for each combination of vitamins and manufacturer
bs+geom_point(aes(color=shelf),size=3)+facet_grid(vitamins~mfr)
bs+geom_point()+geom_smooth(method="lm")+facet_grid(shelf~.)
ggplot(cereals, aes(x=fiber,y=potass,color=shelf))+geom_point()+geom_smooth(method="lm")+facet_grid(shelf~.)
ggplot(cereals, aes(x=fiber,y=potass,color=shelf))+geom_point()+geom_smooth(method="lm", aes(fill=shelf))+facet_grid(shelf~.)

ggplot(cereals, aes(x=fiber,y=potass, color=shelf))+geom_point()+geom_smooth(method="lm", aes(fill=shelf))

## package GGally, to plot a scatterplot matrix
ggpairs(cereals[,c("sugars","protein","shelf")])
ggpairs(cereals[,c("fiber","potass","shelf")])
ggpairs(cereals[,c("fiber","potass","mfr")])
custom_pl <- ggpairs(cereals[,c("sugars","protein","fiber")], upper = c(cor), title = "Custom Example")
ggpairs(cereals[,c("sugars","protein","fiber")],  upper = list(continuous = "density", combo = "box"))

## with data set hbat.csv, use of the function round_any() (need package Hmisc) and stat_summary()
hbat=read.csv("hbat.csv",header=TRUE,sep=",")
# if we have imported the id column as first column, we delete it
head(hbat)
hbat=hbat[-1]

# We need Hmisc package
# stat summary, using the function round_any() to round to multiple of any number, 1 in this particular case.
ggplot(hbat, aes(x=round_any(X6,1,floor),y=X19))+geom_point(color="red")+stat_summary(fun.data="mean_cl_normal", geom="ribbon", alpha=0.1)

#Using the function round_any() to plot the distribution of a continuous variable conditional on a continuous one.
ggplot(hbat, aes(x=round_any(X6,1,floor),y=X19, group=1))+stat_summary(fun.data="mean_cl_normal", geom="ribbon", alpha=0.1)+geom_boxplot(aes(group=round_any(X6,1,floor)), alpha=0.7)+geom_point(color="red")+scale_x_continuous(breaks=c(5:10))+
xlab("Product Quality")+ylab("Customer Satisfaction")


## Exercise: make a table with all the columns you would need to plot mean values of say, sugars, across shelf,
# together with their 95% confidence intervals. Hint: for plotting intervals, use the function geom_errorbar()

df1=cereals %>% group_by(shelf) %>% summarise(N=n(),avg=mean(sugars), s=sd(sugars),se=s/sqrt(N),cil=avg-qt(0.975,N-1)*se,ciu=avg+qt(0.975,N-1)*se) 
ggplot(df1, aes(x=shelf, y=avg, group=1))+geom_errorbar(aes(ymin=cil,ymax=ciu), width=.1, size=1)+geom_point(size=1)+geom_line(colour="#3366FF")

## Exercise: create a categorical version of variable fiber based on its median, i.e., taking the values of "high" and "low" with ## splitting the values at the median. 
## We would want to examine if the mean calories per serving varies by fiber class. Make a chart of the ## estimated mean from each fiber class along with a 95% conf. interval. 
## Now use the t.test() function to do the formal hypothesis test ## of equality of population mean calories between the two fiber class.

cereals$fiberclass=cut(cereals$fiber, c(0,1.5,14.5), right=FALSE, labels=c("Low", "High"))
df2=cereals %>% group_by(fiberclass) %>% summarise(N=n(),avgCalories=mean(calories), s=sd(sugars),se=s/sqrt(N),cil=avgCalories-qt(0.975,N-1)*se,ciu=avgCalories+qt(0.975,N-1)*se) 
ggplot(df2, aes(x=fiberclass, y=avgCalories, group=1))+geom_errorbar(aes(ymin=cil,ymax=ciu), width=.1, size=1)+geom_point(size=1)+geom_line(colour="#3366FF")

var.test(cereals$calories[cereals$fiberclass=="High"],cereals$calories[cereals$fiberclass=="Low"], var.equal=FALSE)
t.test(cereals$calories[cereals$fiberclass=="High"],cereals$calories[cereals$fiberclass=="Low"], var.equal=FALSE)

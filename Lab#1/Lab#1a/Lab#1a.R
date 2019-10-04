url="http://lib.stat.cmu.edu/datasets/1993.expo/cereal"
cereals <- read.table(url, header=FALSE, as.is=TRUE, na.strings="-1")

# or from local file
cereals <- read.table("cerealdata.txt", header=TRUE, as.is=TRUE, na.strings="-1")
names(cereals) <- c('name','mfr','type','calories','protein','fat','sodium','fiber','carbo',
                    'sugars','shelf','potass','vitamins','weight','cups', 'rating')

#checking for missing data and its number
sum(is.na(cereals))

#deleting missing values, creating new data frame cereal. There exist imputation methods, but we're not studying them.
cereals=na.omit(cereals)
str(cereals)

# Types of variables: quantitative, qualitative, nominal, ordinal, interval, ratio.

# Creating a factor
cereals$shelf <- factor(cereals$shelf, levels=1:3, ordered=TRUE)
cereals$shelf
table(cereals$shelf)

#if you want to name the factor levels instead of numbering them, package car, function recode
cereals$shelfC <- recode(cereals$shelf, " 1='low'; 2='medium'; 3='high' ")

# Basic statistical summary of the data set
summary(cereals)

## Skewness and kurtosis, need package "moments" to be loaded.
apply(cereals[,7:10],2, kurtosis)
# defining a function to calculate coefficient of variation:
cv=function(x){
sd=sqrt(var(x))
mu=abs(mean(x))
sd/mu}
# using it 
apply(cereals[,7:10],2, cv)

#other function
cereal_stats=data.frame(
cv=apply(cereals[,c(4:10,12:16)],2,cv),
kurtosis=apply(cereals[,c(4:10,12:16)],2,kurtosis),
skew=apply(cereals[,c(4:10,12:16)],2,skewness))
#using it
print(cereal_stats, gap=3)


# Variable mfr=manufacturer, we may want to know what is the mean content of, say, sugars in
# the different manufacturers. Install package plyr and dplyr, in that order. 
## Package dplyr, provides a consistent and concise grammar for manipulating tabular data. 
# The five main data manipulation 'verbs' in 
# dplyr are: select(), ## filter(), arrange(), mutate(), and summarise().
glimpse(cereals)
filter1=filter(cereals, shelf==3)
head(filter1)
# filtering using more than one variable and multiple criteria
filter2=filter(cereals,shelf==3,sodium > 100, sodium <=130)
head(filter2)

# Arrange reorders rows
head(arrange(cereals,sugars))

# If more than one variable is included, they are use to break the ties, if any.
head(arrange(cereals,sugars, protein))

# If you are working with large datasets with many columns, you may only be interested in a subset of them. 
# Function select() allows you to zoom in on a useful subset. For more information type ?select

head(select(cereals, shelf, sugars))
head(select(cereals, starts_with("p")))

# mutate() adds new columns that are functions of existing ones:
mutate(cereals, logC=log(calories) )

# summarise and group_by
summarise(group_by(cereals,mfr), mean(sugars), sd(sugars))

# sample_n and sample_frac: select random rows from the dataset
sample_n(cereals, 10)
sample_frac(cereals, 0.1)

#or using the piping option, chaining operations
cereals %>% group_by(mfr) %>% summarise(N=n(),avg=mean(sugars), s=sd(sugars)) %>%arrange(avg)
#exploring relationships versus shelf and sugars, fiber and calories
cereals %>% group_by(shelf) %>% summarise(N=n(),avg=mean(sugars), s=sd(sugars)) %>%arrange(avg)
cereals %>% group_by(shelf) %>% summarise(N=n(),avg=mean(fiber), s=sd(fiber)) %>%arrange(avg)
cereals %>% group_by(shelf) %>% summarise(N=n(),avg=mean(calories), s=sd(calories)) %>%arrange(avg)

#using two variables to split up the results for variable sugars
cereals %>% group_by(mfr, shelf) %>% summarise(N=n(),avg=mean(sugars), s=sd(sugars))

cereals %>% group_by(mfr, shelf) %>% summarise(N=n(),avg=mean(sugars), s=sd(sugars))%>%filter(N>1)

# Exercise: make a table with all the columns you would need to plot mean values of say, sugars, across shelf,
# together with their 95% confidence intervals.

df1=cereals %>% group_by(shelf) %>% summarise(N=n(),avg=mean(sugars), s=sd(sugars),se=s/sqrt(N),cil=avg-qt(0.975,N-1)*se,ciu=avg+qt(0.975,N-1)*se) 

# Basic plot: relationship between fiber and calories
plot(cereals$fiber, cereals$calories)
# Basic plot: relationship between fat and calories
plot(cereals$fat, cereals$calories)
plot(jitter(cereals$fat), jitter(cereals$calories))









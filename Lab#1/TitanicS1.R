# Load up ggplot2 package to use for visualizations and dplyr for data manipulation
library(ggplot2)
library(dplyr)

# Load data set
titanic <- read.csv("titanic.csv", header = TRUE)
head(titanic)
str(titanic)

# declare some factor variables 
titanic$pclass <- as.factor(titanic$pclass)
titanic$survived <- as.factor(titanic$survived)

table(titanic$survived)
table(titanic$pclass)

# Hypothesis - Rich passengers survived at a higer rate
ggplot(titanic, aes(x = pclass, fill = survived)) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 

# Perhaps a better view
ggplot(titanic, aes(x = pclass, y=..prop.., fill = survived, group=survived)) +
  geom_bar(position=position_dodge()) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

table0=xtabs(~survived+pclass, data=titanic)
prop.table(table0,1)
mosaicplot(table0,shade=TRUE, type="pearson",main="Survival on the Titanic")

# Examine the first few names in the data set
str(titanic)
head(as.character(titanic$name))

# What is going on with the 'Miss.' and 'Mr.' thing? Can we derive anything from them?
library(stringr)

# Any relationship with other variables (e.g., sibsp)?
misses=filter(titanic,str_detect(titanic$name, "Miss.") )
misses[1:5,]

# Hypothesis - Name titles are related to age
mrses <- filter(titanic,str_detect(titanic$name, "Mrs.") )
mrses[1:5,]

# Check out males to see if pattern continues
males<- filter(titanic,titanic$sex == "male")
males[1:5,]

# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-way relationship.

# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (as.numeric(str_detect(name, "Miss.")) > 0) {
    return ("Miss.")
  } else if (as.numeric(str_detect(name, "Master.")) > 0) {
    return ("Master.")
  } else if (as.numeric(str_detect(name,"Mrs.")) > 0) {
    return ("Mrs.")
  } else if (as.numeric(str_detect(name,"Mr.")) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

# The code below uses a for loop which is not a very R way of
#        doing things
titles <- NULL
for (i in 1:nrow(titanic)) {
  titles <- c(titles, extractTitle(titanic[i,"name"]))
}
titanic$title <- as.factor(titles)

ggplot(titanic, aes(x = title, fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(titanic, aes(x = sex, fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set
summary(titanic$age)

# Take a look at survival rates broken out by sex, pclass, and age
ggplot(titanic, aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master." is a good proxy for male children
boys <- filter(titanic, title == "Master.")
summary(boys$age)

# We know that "Miss." is more complicated, let's examine further
misses <- filter(titanic, title == "Miss.")
summary(misses$age)

# Histogram
ggplot(misses, aes(x = age, fill = survived)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.") +
  xlab("Age") +
  ylab("Total Count")

library("moments")
skewness(misses$age, na.rm=TRUE)

#Histogram by pclass

ggplot(misses, aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

# Age distribution over pclass is a little skewed to the right, more for 1st class, but there are
# a lot of values greater then, say, 12, which is the oldest boy in the Master group
misses %>% group_by(pclass) %>% summarise(skewness(age, na.rm=TRUE))

# It appears that female children may have different survival rate, 
# could be a candidate for feature engineering later

# Group of Misses travelling alone, there are 100
misses.alone=filter(misses, sibsp==0, parch==0)
summary(misses.alone$age)

ggplot(misses.alone, aes(x = age, fill = survived)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.Alone") +
  xlab("Age") +
  ylab("Total Count")

# How many of them were younger than the oldest boy (in the Master group)
nrow(filter(misses.alone,age<=12))

# Does survivability depend on where you got onboard the Titanic?
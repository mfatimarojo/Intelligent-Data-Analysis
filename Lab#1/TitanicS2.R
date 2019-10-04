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

#######  Session 2    ##########
# Move on to the sibsp variable, summarize the variable. Can we treat it like a factor?
table(titanic$sibsp)

titanic$sibsp <- as.factor(titanic$sibsp)

# Title could be predictive. Visualize survival reates by sibsp, pclass, and title
ggplot(titanic, aes(x = sibsp, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Treat the parch variable as a factor and visualize
titanic$parch <- as.factor(titanic$parch)
ggplot(titanic, aes(x = parch, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# What about creating a family size feature?

titanic$family.size <- as.numeric(as.character(titanic$sibsp)) + as.numeric(as.character(titanic$parch)) + 1
titanic$family.size <- as.factor(titanic$family.size)
table(titanic$family.size)
# Visualize it to see if it is predictive

ggplot(titanic, aes(x = family.size, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Family Size') 

# It seems there's a survival penalty for people travelling alone and those with family
# sizes above 4. Let's discretize this variable and create a contingency table:

titanic$Family=car::recode(titanic$family.size, "1='Al'; c(2,3,4)='Sm'; c(5,6,7,8,9,10,11)='Big' ")
table1=xtabs(~survived+family.size, data=titanic)
table1
mosaicplot(table1, shade=TRUE)
chisq.test(table1)
chisq.test(table1, simulate.p.value=TRUE)

ggplot(titanic, aes(x = family.size, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Take a look at the ticket variable
str(titanic$ticket)

# Based on the huge number of levels, ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
titanic$ticket <- as.character(titanic$ticket)
titanic$ticket[1:20]

# Let's explore this variable taking a look at the first character of each ticket
ticket.first.char <- substr(titanic$ticket, 1, 1)
unique(ticket.first.char)

# Let's make a factor for analysis purposes and visualize
titanic$ticket.first.char <- as.factor(ticket.first.char)

# A high-level plot of the data
ggplot(titanic, aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# More fine-grained
ggplot(titanic, aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# The fares the Titanic passengers paid. It's a quantitative variable. 

summary(titanic$fare)

# Visualize  its distribution with histogram
ggplot(titanic, aes(x = fare)) +
  geom_histogram(binwidth=4) +
  ggtitle("Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

## A remark on the logarithmic transf. It has nothing to do with
# the task at hand
ggplot(titanic, aes(x = fare+1, y=..count..)) +
  geom_histogram(binwidth=.2) +
  ggtitle("Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)+scale_x_log10(breaks=c(1,10,100,500))
## you should dig in ggplot2's book for finer details
ggplot(titanic, aes(x = fare+1, y=..count..)) +
  geom_histogram(binwidth=1) +
  ggtitle("Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)+coord_trans(x="log")
## Remark finishes here


# Let's check to see if fare has predictive power
ggplot(titanic, aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")

##########
library("rpart")
library("rpart.plot")

tr.train=select(titanic,pclass,title, survived, family.size)
tr.train$survived=as.factor(tr.train$survived)
tree=rpart(survived~pclass+title+family.size, data=tr.train)
prp(tree, type=0, extra=1, under=TRUE)

# Accuracy, confusion matrix
table(titanic$survived,predict(tree, type="class"))

#####
# rpart confirms that title is important, let's investigate further
table(titanic$title)

# Parse out last name and title
titanic[1:25, "name"]

name.splits <- str_split(titanic$name, ",")
name.splits[1]
# To extract the surname (last name) of every row, it would also work with
# function lapply instead of sapply. We take the list and apply an indexing
# operator to it, "[", and grab the first element in the list.
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names to dataframe in case we find it useful later
titanic$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# What's up with a title of 'the'?
titanic[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
titanic$new.title <- as.factor(titles)

# Visualize new version of title
ggplot(titanic, aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) +
  ggtitle("Surival Rates for new.title by pclass")

# Collapse titles based on visual analysis
indexes <- which(titanic$new.title == "Lady.")
titanic$new.title[indexes] <- "Mrs."

indexes <- which(titanic$new.title == "Dr." |
                   titanic$new.title == "Rev." |
                   titanic$new.title == "Sir." |
                   titanic$new.title == "Officer")
titanic$new.title[indexes] <- "Mr."

# Visualize 
ggplot(titanic, aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) +
  ggtitle("Surival Rates for Collapsed new.title by pclass")


# Grab features
tr.train2 <- select(titanic,pclass, new.title, survived,family.size)

# Run CV and check out results
tr.train2$survived=as.factor(tr.train2$survived)
tree2=rpart(survived~pclass+new.title+family.size, data=tr.train2)
prp(tree2, type=0, extra=1, under=TRUE)

# Confusion matrix
table(titanic$survived,predict(tree2, type="class"))


accidents <- read.csv('road-accidents-2010.csv', header=TRUE,as.is=TRUE, na.strings="-1")
# use the function as.Dates to convert string to dates. The as.Date methods return a character vector of class Date.
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
# Creating a logical variable: Accidents Fatality = 1 if there were casualties in the accident, 0 otherwise.
accidents$Fatality <- accidents$Accident_Severity == 1
# Looking at Weather_Conditions, we are going to use the codings 1 to 6 for this variable, which represent 94.5% of the data.
 sum(! (accidents$Weather_Conditions %in% 1:6))
sum(! (accidents$Weather_Conditions %in% 1:6))/nrow(accidents)
# we delete missing values. Yes, there are a lot of them, we loose many data points, but we still have 90733 observations
# to work with
accidents=na.omit(accidents)
dim(accidents)
# function subset is like filter((accidents,Weather_Conditions %in% 1:6))
accidents <- subset(accidents, accidents$Weather_Conditions %in% 1:6)
help(subset)
dim(accidents)

newframe=accidents %>% group_by(mydate)%>% summarise(N=n(), meanW=mean(Weather_Conditions), p.fatal=mean(Fatality))
names(newframe)[1]="Date"
#extracting the day of the week from the date, 0=Sunday
newframe$Day=format(newframe$Date, "%w")

# PLOT 1: relationship between the day of the week, number of accidents in a day (Y variable), proportion of accidents that had a 
# fatality (p.fatal) and the mean weather severity.
plot1=ggplot(data=newframe, aes(x=Day, y=N))
plot1+geom_boxplot()
plot1+geom_jitter(aes(size=p.fatal, color=meanW), position=position_jitter(w=.3, h=.0))
plot1+geom_boxplot()+geom_jitter(aes(size=p.fatal, color=meanW), position=position_jitter(w=.3, h=.0))+ggtitle("Number of accident by day of the week")+
  xlab("Day of the week. 0 is Sunday") +
  ylab("Number of accidents in a day")

#  PLOT 2
# Fatality rates by weather severity
plot2 <- ggplot(data=newframe, aes(x=meanW, y=p.fatal))
plot2 <-plot2 +
  geom_point(aes(size=Day, color=N))+
  geom_smooth(color="black")+
  ggtitle("Proporton of fatalities by mean weather condition")+
  xlab("Mean Weather Severity") +
  ylab("Proportion of accidents with at least one fatality")
plot2

## Categorize the Day variable into Weekdays=W and Weekend days =WE in one variable called DayF
newframe$DayF=newframe$Day
newframe$DayF=gsub(5, "W", newframe$DayF)
newframe$DayF=gsub(4, "W", newframe$DayF)
newframe$DayF=gsub(3, "W", newframe$DayF)
newframe$DayF=gsub(2, "W", newframe$DayF)
newframe$DayF=gsub(6, "W", newframe$DayF)
newframe$DayF=gsub(0, "WE", newframe$DayF)
newframe$DayF=gsub(1, "WE", newframe$DayF)

## 
plot2.1 <- ggplot(data=newframe, aes(x=meanW, y=p.fatal))
plot2.1 <-plot2.1 +
  geom_point(aes(shape=DayF, color=N), size=2.5)+
  geom_smooth(color="black")+
  ggtitle("Proporton of fatalities by mean weather condition")+
  xlab("Mean Weather Severity") +
  ylab("Proportion of accidents with at least one fatality")
plot2.1

## Splitting the relationship for W and WE
plot2.2 <- ggplot(data=newframe, aes(x=meanW, y=p.fatal, group=DayF) )
plot2.2 <-plot2.2 +
  geom_point(aes(colour=DayF))+
  geom_smooth(aes(colour=DayF))+
  ggtitle("Proporton of fatalities by mean weather condition")+
  xlab("Mean Weather Severity") +
  ylab("Proportion of accidents with at least one fatality")
plot2.2

# PLOT 3
## number of accidents by Date
plot3<- ggplot(data=newframe, aes(x=Date, y=N))+
    ggtitle("Number of accidents with injury in 2010")+
    xlab("Date")+ylab("Number of accidents")+
    geom_point()+
    geom_smooth()
plot3

## by month
# Extract the month from Date
newframe$month <- as.numeric(format(newframe$Date, "%m"))
#calculations to compute confidence intervals, N is number of accidents each day

newframe2=newframe %>% group_by(month)%>% summarise(Mu=mean(N), s=sd(N), se=s/sqrt(n()),lcp= Mu-
qt(0.975,n()-1)*se,ucp= Mu+
qt(0.975,n()-1)*se)

# PLOT 4
ggplot(newframe2,aes(x=month, y=Mu))+geom_line()+geom_ribbon(aes(ymin=lcp,ymax=ucp), fill="grey70", alpha=0.7)+geom_line(aes(y=Mu))+scale_x_continuous(breaks=c(1:12))
# or
plot4 <- ggplot(data=newframe2, aes(x=month, y=Mu))+
    ggtitle("Number of accidents by month")+
    xlab("Month")+ylab("Mean number of accidents by month with 95% ci")+
    geom_point()+
    geom_errorbar(aes(ymin=lcp, ymax=ucp), width=0.2)+
    geom_line(aes(group=1))+
    scale_x_continuous(breaks=c(1:12))
plot4

## 
# let's try to extract the hour of the day an accidents happens
accidents$DateTime <- as.POSIXlt(paste(accidents$Date," ",accidents$Time), format="%d/%m/%Y %H:%M", tz="GMT")
accidents$DateTime[1:10]
accidents$Hour <- as.numeric(format(accidents$DateTime, "%H"))

# PLOT 5
ggplot(data=accidents, aes(x=Hour))+
  ggtitle("Histogram of accidents with injury by hour of the day")+
  ylab("Number of accidents")+xlab("Hour of the day")+
  geom_histogram( binwidth=1, alpha=0.2)

ggplot(data=accidents, aes(x=Hour,y=..density..))+
  ggtitle("Histogram of accidents with injury by hour of the day")+
  ylab("Relative Freq. of accidents")+xlab("Hour of the day")+
  geom_histogram( binwidth=1, alpha=0.2)
# the warning is because there are some missing values, probably due to the conversion process from dates to Hour
sum(is.na(accidents$Hour))

# contingency table
newframe4=table(accidents$Hour, accidents$Fatality)
newframe4
# it is better to use command xtabs(), altough the output is the same. It's used to obtain a contingency table in Frequency Form from 
# a table in Case Form.

#contingency table
newframe5=xtabs(~Hour+Fatality,data=accidents)
addmargins(newframe5)
# showing proportions instead of frequencies
prop.table(newframe5,1)
chisq.test(newframe5)
# warning: if we cannot guarantee that our data meet the conditions for the chisq.test to work well 
# with the asintotic approximation, we can compute the p-value from Monte Carlo simulation
chi1=chisq.test(newframe5, simulate.p.value=TRUE)
# residuals
chi1$residuals
#expected values
chi1$expected
# the original table
chi1$obs

#function mosaic(), package vcd
mosaic(newframe5, shade=TRUE)

# preliminary plot
plot(prop.table(newframe5,1)[,2], type="l", xlab="", ylab="")

#Variables Fatality and Hour
newframe6=accidents[,c(34,36)]

newframe7=newframe6 %>% group_by(Hour) %>% summarise(n=sum(Fatality), N=n(), p=n/N, cil=p-1.96*sqrt((p*(1-p)/N)), ciu=p+1.96*sqrt((p*(1-p)/N)) )

################### PLOT 6
plot6 <- ggplot(data=newframe7, aes(x=Hour, y=p))+
  ggtitle("Probability of a fatality by hour of the day")+
  xlab("Hour of the day")+ylab("Probability of a fatality and 95% ci")+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=0.2)

plot6

# Exercise: you can also try the plot "Accident Fatality rate by month, with confidence intervals"

# Exercise: Construct the contingency table Month X Fatality, 
# Day of the week x Fatality and test the hypothesis of independence. Does 
# the day of # the week or the month of the # year make any difference in the distribution of fatality accidents? 
# Interpret the output of the test, # # the residuals and plot a mosaic for each test. 

accidents$Month <-as.numeric(format(accidents$DateTime, "%m"))

newframe9=xtabs(~Day_of_Week+Fatality, data=accidents)
chisq.test(newframe9)

# you can also use the function recode in package car
accidents$DayF=car::recode(accidents$Day_of_Week, "c(2,3,4,5,6)='W'; c(7,1)='WE'")
newframe10=xtabs(~DayF+Fatality, data=accidents)

### Bonus plots
ggplot(subset(accidents, Number_of_Vehicles %in% c(1:9)), aes(x=factor(Day_of_Week), fill=factor(Number_of_Vehicles)))+
  ggtitle("Number of accidents by Day of Week and Vehicles involved")+
  ylab("Number of accidents")+xlab("Day of the week")+
  geom_bar()+scale_fill_brewer(name="Number\nof Vehicles")+
  scale_x_discrete(labels=c("Mo","Tu","We","Th","Wd","S","Su"))

ggplot(subset(accidents, Number_of_Vehicles %in% c(1:9)), aes(x=factor(Number_of_Vehicles)))+
  ggtitle("Number of accidents by Day of Week and Vehicles involved")+
  ylab("Number of accidents")+xlab("Number of vehicles involved")+
  geom_bar(position=position_dodge())+scale_fill_brewer(palette="Spectral")+facet_grid(~Day_of_Week)

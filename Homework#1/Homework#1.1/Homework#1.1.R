
# Load local file:
wines <- read.table("wines.txt", header=TRUE, as.is=TRUE, na.strings="-1")
names(wines) <- c('fixed_acidity','volatile_acidity','citic_acid','residual_sugar','chlorides',
                  'free_sulfur_dioxide','total_sulfur_dioxide','density','pH',
                    'sulphates','alcohol','quality','type')

# Show data:
str(wines)

# Show summary:
summary(wines)

# Create a filter for white and red wines to explore any of them separately
whiteWines=filter(wines, type==1)
redWines=filter(wines, type==2)

# Create a new column with the type of wine recoded
wines$type <- factor(wines$type, levels=1:2, ordered = TRUE)
wines$typew <- recode(wines$type, " 1='White'; 2='Red' ")

# Create a new column with the quality of wine recoded
wines$quality <- factor(wines$quality, levels=4:8, ordered = TRUE)
wines$qualityC <- recode(wines$quality, " 4='bad'; 5='decent'; 6='decent'; 7='good'; 8='very good' ")

# PLOTS
# Quality vs Alcohol
bs=ggplot(wines, aes(x=alcohol,y=quality))+ggtitle("Quality vs Alcohol %")
bs+geom_point(aes(color=type))

# Quality vs Alcohol by type of wine
bs+geom_point(aes(color=type))+facet_grid(~type)
ggplot(wines, aes(x = alcohol, y = qualityC))+ggtitle("Quality vs Alcohol %") +
  geom_point(aes(color=type))+
  xlab("Alcohol %") +
  ylab("Quality") +
  labs(fill = "Type")

# Create a new categorical variable "sweetness" from a continuous one "residual_sugar"
wines$sweetness=cut(wines$residual_sugar, c(0,5,15,30,50,100), right=FALSE, labels=c("Dry", "Off-dry", "Medium-dry/Semi-sweet", "Medium-sweet", "Sweet"))
whiteWines$sweetness=cut(whiteWines$residual_sugar, c(0,5,15,30,50,100), right=FALSE, labels=c("Dry", "Off-dry", "Medium-dry/Semi-sweet", "Medium-sweet", "Sweet"))
redWines$sweetness=cut(redWines$residual_sugar, c(0,5,15,30,50,100), right=FALSE, labels=c("Dry", "Off-dry", "Medium-dry/Semi-sweet", "Medium-sweet", "Sweet"))

# Sweetness vs Acidity by quality and type of wine
plot1=ggplot(wines, aes(x=sweetness,y=volatile_acidity))+ggtitle("Sweetness vs Acidity by type of wine")+xlab("Sweetness")+ylab("Acidity")
plot1+geom_point(aes(color=typew),size=3)+facet_grid(~typew)

# Boxplot that shows red wines tend to have higher pH levels than white ones
ggplot(wines, aes(x=typew,y=ph, fill=typew))+geom_boxplot()+geom_point()+xlab("Type")+ylab("pH")

# Alcohol vs Density by type of wine
winesWhite = subset(wines, wines$type==1 & wines$density <= 1 & wines$alcohol > 9)
winesRed = subset(wines, wines$type==2 & wines$density <= 1 & wines$alcohol > 9)
ggplot(wines,aes(colour = Type_of_wine))+
  geom_smooth(method = "loess",data = winesRed,se=FALSE,aes(x = alcohol, y= density, color='Red Wine'))+
  geom_smooth(method = "loess",data = winesWhite,se=FALSE, aes(x = alcohol, y= density, color='White Wine'))


#MEGHNATH REDDY CHALLA-IDA HW-2

require(tidyverse) #To install and load multiple tidyverse packages
require(plyr) ##package to implementsplit-apply combine pattern
require(mice) #Multivariate Imputation by Chained equation to deal with missing data
require(VIM) #library for Visualization and imputation of Missing values
require(Amelia) #to load freetrade data
#Problem 1)Learning ggplot2
#a) i) 3.2.4 Excersise 4 and 5
ggplot(data=mpg)
?mpg
# 4-Basic scatter plot hwy vs cyl
ggplot(mpg, aes(x=cyl, y=hwy)) + 
  geom_point(size=2, shape=20)+labs(title="Basic scatter plot hwy vs cyl")
#With the help of this scatterplot we can easily distinguish range of
#number of miles the car can travel based on the type of cylinder
x<-mpg #remove
# 5-Basic scatter plot class vs drv
ggplot(mpg, aes(x=class, y=drv)) + geom_point(size=2, shape=20)+
  labs(title="Basic scatter plot class vs drv")
#Since both the variables class and drv are categorical values, the number of
#unique combinations of (x,y) are limited which is 3 for drv * 7 for class. Scatterplot works better when
#the variables compared are unique and continuous.

#a) ii) 3.3.1 Exercises #3, #4, #6
# 3-Map continuous variable
ggplot(mpg, aes(x = cyl, y = hwy, color = displ)) + geom_point()+
  labs(title="Color as Continuous variable") #color = displ which is continuous
ggplot(mpg, aes(x = cyl, y = hwy, size = displ)) + geom_point() +
  labs(title="Size as Continuous variable") #size=displ which is continuous
ggplot(mpg, aes(x = cyl, y = hwy, shape = displ)) + geom_point() #displ which is continuous

# 3-Map categorical 
ggplot(mpg, aes(x = cyl, y = hwy, color = class)) + geom_point() +
  labs(title="Color as categorical variable")#color = class which is categorical
ggplot(mpg, aes(x = cyl, y = hwy, size = class)) + geom_point() + 
  labs(title="Size as categorical variable") #size=class which is categorical
ggplot(mpg, aes(x = cyl, y = hwy, shape = class)) + geom_point() +
  labs(title="Shape as categorical variable")#shape=class which is categorical

#4-same variable to multiple aesthetics
ggplot(mpg, aes(x = cyl, y = hwy, color = displ,size=class))  + geom_point() +
  labs(title="same variable to multiple aesthetics")#color = displ which is continuous

#6- aesthetic to something other than a variable name
ggplot(mpg, aes(x = cyl, y = hwy,color = year<2005)) + geom_point() +
  labs(title="Aesthetic to something other than a variable name")

#a) iii) ADv and Dis of facet
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)+labs(title="Facet grid")

#b)  reproduce the plot in Figure 1
ggplot(data=mpg) + geom_point(mapping = aes(x=displ,y=hwy),
  position = "jitter",alpha=1/5) +facet_grid(. ~ drv)+
  geom_smooth(mapping = aes(x=displ,y=hwy),method=lm,color="black")+
  geom_smooth(mapping = aes(x=displ,y=hwy),se=FALSE)+
  xlab("Displacement") +
  ylab("Highway MPG")  

#Problem 2:Generating data and advanced density plots
#a) i)Generating data
a<-rnorm(500)
b<-rnorm(500)
c<-rnorm(500)
d<-rnorm(500)
df <- data.frame(a,b,c,d)
df <- setNames(df, c("a","b","c","d"))
head(df) #to display top elements with 4 columns
?gather
#ii)gather function
df2<-df %>% gather(groupVar,value,a:d)
nrow(df2)
head(df2)

 #b)density plots
density_plot <- ggplot(df2, aes(x=value,fill=groupVar)) + 
  geom_density(alpha=0.3)+labs(title="Advanced density plot")
density_plot

#Problem 3: House prices data
housing_data<-read.csv('housingData.csv')
summary(housing_data)

#Different plots

#price vs lot area -scatterplot
plot(SalePrice  ~ LotArea , data=housing_data,
            xlab=" LotArea", ylab="SalePrice",
            main="LotArea vs SalePrice Scatter Plot")

#price vs garage area -scatterplot
plot(SalePrice  ~ GarageArea , data=housing_data,
     xlab=" Garage area", ylab="SalePrice",
     main="Garage area vs SalePrice Scatter Plot")

# Simple Bar Plot with count of different dwelling styles-bar plot
counts <- table(housing_data$HouseStyle)
barplot(counts, main="Housing style of dwelling Distribution",
        legend=rownames(counts),xlab = "Style of dwelling",ylab="count")

#Bar Plot with count of Central A/C
counts1 <- table(housing_data$CentralAir)
barplot(counts1, main="Central Air Conditioning",
        legend=rownames(counts1),xlab = "Yes/No",ylab="count")

#  Bar Plot for Exterior condition
counts2 <- table(housing_data$ExterCond)
barplot(counts2, main="Housing Exterior Condition",
        legend=rownames(counts2),xlab = "Quality",ylab="count")

#boxplot
boxplot (housing_data$OverallQual,housing_data$OverallCond, ylab="Rating",
         main="Quality vs Condition box Plot")

table(housing_data$YearBuilt,housing_data$OverallQual)

#density plot
density_plot1 <- ggplot(housing_data, aes(x=SalePrice,fill=LotShape)) + 
  geom_density(alpha=0.3)+labs(title="Advanced density plot")
density_plot1

#stat_bin
ggplot(housing_data,aes(x=OverallQual))+stat_bin()+
scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) + labs(title="To give count based on Quality")

#stat_summary
ggplot(housing_data,aes(x=OverallQual,y=LotArea))+stat_summary()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))+
  theme_dark(base_family = "serif")+labs(title="Summary and theme")

#scale_shape_manual
ggplot(housing_data,aes(x=OverallQual,y=Condition1,shape=LotShape))+
  geom_point()+scale_shape_manual(values=c(5,3,8,2))+
  scale_color_brewer(type="qual")+scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))+
  labs(title="Qual vs Cond based on Lot shape using brewer")
#facet_wrap()
ggplot(housing_data,aes(x=MSZoning,color=Foundation))+geom_density()+facet_wrap(~OverallQual)+
  labs(title="Density plots generated using facet wrap")

#heatmap
x<-data.frame(housing_data$SalePrice,housing_data$LotArea)
cor(x)
heatmap(cor(x))

#linear model
linModel<-lm(data=housing_data,OverallQual~OverallCond)
summary(linModel)

#Problem 4:Missing Data
data("freetrade")
?freetrade
summary(freetrade)
#from summary it is clear that tariff has most NA with 58 instances
#if the missing values in column is more than 5% of observations it is ideal
#to drop the feature
#Using mice
pMiss <- function(x){sum(is.na(x))/length(x)*100} #calculate missing variable math function
apply(freetrade,1,pMiss)
apply(freetrade,2,pMiss)
#tariff,intresmi, fiveop features have more than 5% of missing values
md.pattern(freetrade)
#96 rows have no missing features,similarly 52 rows have only tariff as missing

#using VIM
aggr_plot <- aggr(freetrade, col=c('navyblue','red'), 
        numbers=TRUE, sortVars=TRUE, labels=names(data), 
        cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#Almost 56% of data is not missing any value

#margin plot
marginplot(freetrade[c("signed","intresmi")],col=c("blue","red"))

#scatmatrix
scattmatrixMiss(freetrade)

#Problem 5:  Extra credit
# using ANOVA
real_data <- aov(is.na(freetrade$tariff) ~ freetrade$country, freetrade)
real_data
#is.na()  returns boolean value
# Summary of the analysis
summary(real_data)

#remove Nepal
nepalr<-freetrade[!freetrade$country=="Nepal",]
without_Nepal<-aov(is.na(nepalr$tariff) ~ nepalr$country, nepalr)
# Summary of the analysis
summary(without_Nepal)

#remove Philippines
Philr<-freetrade[!freetrade$country=="Philippines",]
without_Philippines<-aov(is.na(Philr$tariff) ~ Philr$country, Philr)
# Summary of the analysis
summary(without_Philippines)


getwd()
setwd("C:/Users/jaaaa/Desktop/MD")

dd <- read.csv("muestra.csv",header=T,sep=",")

custom_breaks <- function(x) {
  min_val <- min(x)  # Get minimum value
  max_val <- max(x)  # Get maximum value
  seq(min_val, max_val, by = 3)  # Create breaks every 3 units
}

install.packages("colorspace")
library(colorspace)
library(stats)
library(DescTools)
library(ggplot2)

colors <- qualitative_hcl(12, palette = "dark 3")

#-------------------------------------------------------------------------------

CustomerAge <- dd[,"Customer_Age"]

#histogram for CustomerAge
hist(CustomerAge, custom_breaks, freq=T, col="purple4",border="lightblue1")
hist(log(CustomerAge), custom_breaks, freq=T, col="purple4",border="lightblue1")

summary(CustomerAge)

#testing for normality -> conclusion: not normal
shapiro.test(CustomerAge)
shapiro.test(log(CustomerAge))
PearsonTest(CustomerAge)

#qqnorm of CustomerAge -> middle values follow normal distribution closely
#however tails differ somewhat significantly
qqnorm(CustomerAge)
qqline(CustomerAge, col="orange2")

qqnorm(log(CustomerAge))
qqline(log(CustomerAge), col="orange2")


#boxplot #beware there are no outliers because the distribution isn't normal!
boxplot(CustomerAge, col="purple4")

Freq(CustomerAge)

#-------------------------------------------------------------------------------

CustomerGender <- dd[,"Customer_Gender"]

barplot(table(CustomerGender), col = c("purple4","green4"))
Freq(CustomerGender)
chisq.test(table(CustomerGender),p = c(0.5,0.5))

#-------------------------------------------------------------------------------

Month <- dd[,"Month"]

summary(factor(Month, levels = month.name))
barplot(table(factor(Month, levels = month.name)), col = colors)
Freq(factor(Month, levels = month.name))

#-------------------------------------------------------------------------------

Year <- dd[,"Year"]

summary(as.factor(Year))
barplot(table(as.factor(Year)), col = "gray50")
Freq(as.factor(Year))

#-------------------------------------------------------------------------------

Country <- dd[,"Country"]

summary(as.factor(Country))
pie(table(as.factor(Country)), col = colors)
barplot(table(as.factor(Country)), col = colors)
Freq(as.factor(Country))

#-------------------------------------------------------------------------------

ProductCategory <- dd[,"Product_Category"]

summary(as.factor(ProductCategory))
pie(table(as.factor(ProductCategory)), col = colors)
barplot(table(as.factor(ProductCategory)), col = colors)
Freq(as.factor(ProductCategory))

#-------------------------------------------------------------------------------

OrderQty <- dd[,"Order_Quantity"]
hist(OrderQty, freq=T, col="purple4",border="lightblue1")
summary(OrderQty)
boxplot(OrderQty, col="purple4")

#-------------------------------------------------------------------------------

UCost <- dd[,"Unit_Cost"]
hist(UCost, freq=T, col="purple4",border="lightblue1")
hist(log(UCost), freq=T, col="purple4",border="lightblue1")
summary(UCost)
boxplot(log(UCost), col="purple4")

#-------------------------------------------------------------------------------

UPrice <- dd[,"Unit_Price"]
hist(UPrice, freq=T, col="purple4",border="lightblue1")
hist(log(UPrice), freq=T, col="purple4",border="lightblue1")
summary(UPrice)
boxplot(log(UPrice), col="purple4")

#-------------------------------------------------------------------------------

Size <- dd[,"Size"]

summary(as.factor(Size))
pie(table(as.factor(Size)), col = colors)
barplot(table(factor(Size, levels = c("Small", "Medium", "Large", "Extra Large"))), col = colors)
Freq(as.factor(Size))

#-------------------------------------------------------------------------------


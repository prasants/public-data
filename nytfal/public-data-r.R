#Prasant Sudhakaran####
setwd("~/Google Drive/05. Other Projects/02. Articles/01. Beef/csv")

#Loading Libraries####

library(ggplot2)
library(ggthemes)
library(reshape)
library(plyr)

#Loading FAO Index####
faoind <- read.csv("~/Google Drive/05. Other Projects/02. Articles/01. Beef/csv/fao-index.csv")
str(faoind)

#Basic Plot####
plot(faoind$Year, faoind$Food, xlab="Year", ylab="Food Price Index")
head(faoind)

#Reshaping the FAO Index####
faoindresh <- reshape(faoind, 
             varying = c("Food", "Meat", "Dairy", "Cereals", "Sugar"), 
             v.names = "Levels",
             timevar = "Indices", 
             times = c("Food", "Meat", "Dairy", "Cereals", "Sugar"), 
             direction = "long")

#Using gplot to Plot the FAO Index####
ggplot(faoindresh, aes(x=Year, y=Levels, colour=Indices, group=Indices))+geom_line() + theme_wsj()


#Selecting Food, Meat####
ggplot(faoindresh, aes(x=Year, y=Levels, ,
                color=Indices, group=Indices))+ 
  geom_line(subset= .(Indices %in% c('Food','Meat'))) + theme_wsj()


#Historical Beef vs Pork Prices####
# Here we explore further, using historical data from http://www.ers.usda.gov/datafiles/Meat_Price_Spreads/history.xls

beefvspork <- read.csv("~/Google Drive/05. Other Projects/02. Articles/01. Beef/csv/beefvspork.csv")
View(beefvspork)
#Changing the format of the "Months' column
beefvspork$Month <- as.Date(beefvspork$Month , "%m/%d/%y")
beefvspork[order(beefvspork$Month),]

View(beefvspork)

# Just a quick plot to view the data 
plot(beefvspork$Month, beefvspork$Beef, xlab=" ", ylab="Beef Prices")
plot(beefvspork$Month, beefvspork$Pork, xlab=" ", ylab="Pork Prices")

# Reshaping the data for the ggplot graph
bvp <- reshape(beefvspork, 
                      varying = c("Beef", "Pork"), 
                      v.names = "Prices",
                      timevar = "Meats", 
                      times = c("Beef", "Pork"), 
                      direction = "long")

#Using ggplot to plot Historical Beef vs Pork Prices####
ggplot(bvp, aes(x=Month, y=Prices, colour=Meats, group=Meats))+geom_line() + theme_wsj()

#Studying Joint Malnutrition Reports - UNICEF, IMF, WB####
jtmal <- read.csv("~/Google Drive/05. Other Projects/02. Articles/01. Beef/csv/jt-mal-selected.csv")
View(jtmal)

jtmal <- reshape(jtmal, 
               varying = c("Underweight"), 
               v.names = "Scores",
               timevar = "Indicators", 
               times = c("Underweight"), 
               direction = "long")
ggplot(jtmal, aes(x=Year, y=Scores, colour=Country, group=Country))+geom_line() + theme_wsj()


jtmal2 <- read.csv("~/Google Drive/05. Other Projects/02. Articles/01. Beef/csv/jt-mal-selected2.csv")
jtmal2 <- reshape(jtmal2, 
                 varying = c("Underweight"), 
                 v.names = "Scores",
                 timevar = "Indicators", 
                 times = c("Underweight"), 
                 direction = "long")
ggplot(jtmal2, aes(x=Year, y=Scores, colour=Country, group=Country))+geom_line()+ theme_wsj()

#USDA Foods Database####
USDA <- read.csv("~/Google Drive/05. Other Projects/02. Articles/01. Beef/csv/USDA.csv")
str(USDA)
summary(USDA$Protein)
which.max(USDA$Protein)
USDA$Description[4661]
HighProtein = subset(USDA, Protein>=11.71)
HighProtein

HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))

#Commodity Prices as Reported by IMF####
imfcomm <- read.csv("~/Google Drive/05. Other Projects/02. Articles/01. Beef/csv/imf-commodity-prices.csv")

imfcomm <- reshape(imfcomm, 
               varying = c("Beef", "Groundnut", "Lamb", "Pork"), 
               v.names = "Prices",
               timevar = "Commodities", 
               times = c("Beef", "Groundnut", "Lamb", "Pork"), 
               direction = "long")

imfcomm$Dates <- as.Date( imfcomm$Dates, '%m/%d/%y')

ggplot(imfcomm, aes(x=Dates, y=Prices, colour=Commodities, group=Commodities))+geom_line() + theme_wsj()

#Subsetting Beef, Lamb, Pork####
ggplot(imfcomm, aes(x=Dates, y=Prices, ,
                       color=Commodities, group=Commodities))+ 
  geom_line(subset= .(Commodities %in% c("Beef", "Lamb","Pork"))) + theme_wsj()

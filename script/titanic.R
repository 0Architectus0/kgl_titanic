#install missing libraries
# install.packages('randomForest')
# install.packages('mice')
# install.packages('ggthemes')

#load libraries
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

#get data
train.df <- read.csv("data/train.csv", stringsAsFactors = F)
test.df <- read.csv("data/test.csv", stringsAsFactors = F)

#check out data 
str(test.df)
str(train.df)

#create survived col to combine data
test.df$Survived <-"TBT"

#full data set
full.df <- rbind(train.df, test.df)

str(full.df)

#derive title from name
##pattern string replace removes 
full.df$Title <- gsub('(.*, ) | (\\..*)', '', full.df$Name)

ggplot(full.df[which(full.df$Survived != "TBT"),], aes(x = Sex, fill = Survived)) + 
        geom_bar() +
        facet_wrap(~Pclass)


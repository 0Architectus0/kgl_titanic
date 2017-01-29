#load raw data
train <- read.csv("data/train.csv", header = T)
test <- read.csv("data/test.csv", header = T)

# add a "survived" Variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#Combine data sets
data.combined<- rbind(train, test)

# A bit about R data types (e.g. factors)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Survived)
table(data.combined$Pclass)

library(ggplot2)

train$Pclass <- as.factor(train$Pclass)

ggplot(train, aes(x = Pclass, fill = factor(Survived)))+ 
  geom_histogram(width = 0.5) +
  xlab("PClass")+
  ylab("Total Count") +
  labs(fill = "Survived")

head(as.character(train$Name))

length(unique(as.character(data.combined$Name)))

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

dup.rows<- data.combined[which(data.combined$Name %in% dup.names),]


library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]

misses[1:5,]

mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]

mrs[1:5,]


# Add calc column using function for title conversion ---------------------

extractTitle <- function(name) {
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) > 0 ) {
    return("Miss.")
  } else if(length(grep("Master.", name)) > 0 ) {
    return("Master.")
  } else if(length(grep("Mrs.", name)) > 0 ) {
    return("Mrs.")
  } else if(length(grep("Mr.", name)) > 0 ) {
    return("Mr.")
  } else {
    return("Other")
  }
}


# Call Function on data combined--------------------------------------------------------------

titles <- NULL

for(i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

data.combined$title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x = title, fill = Survived))+
  geom_bar(binwidth = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("PClass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")
  
table(data.combined$Sex)

# Day 2 -------------------------------------------------------------------

# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(binwidth = 0.5) +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")


# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)


# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))


# Move on to the sibsp variable, summarize the variable
summary(data.combined$sibsp)


# Can we treat as a factor?
length(unique(data.combined$sibsp))


data.combined$sibsp <- as.factor(data.combined$sibsp)


# We believe title is predictive. Visualize survival reates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the parch vaiable as a factor and visualize
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

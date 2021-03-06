

#load raw csv data 
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#add a "Survived" variable to current test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])


#combine data sets
data.combined <- rbind(train, test.survived)

#R data types
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Take a look at gross survival rates
table(data.combined$Survived)


#Distribution across the classes
table(data.combined$Pclass)

#Load up ggplot2 package for visualization
library(ggplot2)

#Hypothesis - Upper class survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived)) ) +
  stat_count(width = 0.5) + 
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


#Examine the first few names in the training data set
head(as.character(train$Name))

#How many unique names are there across both train and test set
length(unique(as.character(data.combined$Name)))

#Two duplicate names, take a closer look 
#First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Next, take a look in the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]


library(stringr)

#Any correlation with other variables ?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses[1:5,]


#Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]
mrses[1:5,]

#Check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]


#Expand upon the relationship between "Survived" and "Pclass" by adding the new "Title" variable to the main
#data set and then explore a potiental 3-dimensional relationship

#Create a utility function to help with title extraction
extractTitle <- function(name){
  name <- as.character(name)
  
  if(length(grep("Miss.", name))> 0) {
    return("Miss.")
  } else if(length(grep("Mrs.", name))> 0) {
    return("Mrs.")
  } else if(length(grep("Mr.", name))> 0) {
    return("Mr.")
  } else if(length(grep("Master.", name))> 0){
    return("Master.")
  } else{
    return("Other")
  }
}


titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}


data.combined$Title <- as.factor(titles)


#Since we only have survived labels for the train set, only use the first 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
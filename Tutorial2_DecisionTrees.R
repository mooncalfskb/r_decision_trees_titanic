#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)



#https://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/

base_dir = "~/Dropbox/skb/DataProjectsBankruptcy/2019/Tutorials/DecisionsTreeTutorial/"

#training data
train <- read.csv(paste(base_dir, "data/train.csv", sep = ""))

#test data
test <- read.csv(paste(base_dir, "data/test.csv", sep = ""))

#make a tree with class method
#If you wanted to predict a continuous variable, such as age, you may use method="anova". 
#This would run generate decimal quantities for you. 
#But here, we just want a one or a zero, so method="class" is appropriate:
#Here we are dividing Survived by Passenger class, sex, age, SibSp = siblings/spouse, Parch = parentchild
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

#basic visuals. very crude
plot(fit)
text(fit)

#amazing graphic
fancyRpartPlot(fit)

#Here we have called rpart’s predict function. 
#Here we point the function to the model’s fit object, 
#which contains all of the decisions we see above, 
#and tell it to work its magic on the test dataframe. 
#No need to tell it which variables we originally used in the model-building phase, 
#it automatically looks for them and will certainly let you know if something is wrong. 
#Finally we tell it to again use the class method (for ones and zeros output) 
#and as before write the output to a dataframe and submission file.

#Prediction = the variable created by the predict method. type class means 0/1
Prediction <- predict(fit, test, type = "class")
#create dataframe. not quite clear on the "Survived = Predicion  but I guess that's the magic part.
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
perish_file = paste(base_dir, "04_decision_trees.csv", sep = "")
write.csv(submit, file = perish_file, row.names = FALSE)

#don't do this in real life, but do it here to show overfitting
#these variables: control=rpart.control(minsplit=2, cp=0) control how much splitting is done.
#set it to go hog wild. 
#less accurate than the more constrained.
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)


#examples of how to manipulate your decision tree better.
#An interactive version of the decision tree will appear 
#in the plot tab where you simply click on the nodes that 
#you want to kill. Once you’re satisfied with the tree, 
#hit “quit” and it will be stored to the new.fit object. 
#Try to look for overly complex decisions being made, 
#and kill the nodes that appear to go to far.
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class", control=rpart.control( your controls ))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


#https://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/

base_dir = "~/Dropbox/skb/DataProjectsBankruptcy/2019/Tutorials/DecisionsTreeTutorial/"

#training data
train <- read.csv(paste(base_dir, "data/train.csv", sep = ""))

#test data
test <- read.csv(paste(base_dir, "data/test.csv", sep = ""))

#view dataframe structure
str(train)
#view one column
table(train$Survived)
#shows this
#0   1 
#549 342 

#convert unique values to percentages
prop.table(table(train$Survived))
#creates this
#0         1 
#0.6161616 0.3838384 

#assign a new column to the test data
#assume everyone died
test$Survived <- rep(0, 418)

#create first submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
perish_file = paste(base_dir, "01_theyallperish.csv", sep = "")
write.csv(submit, file = perish_file, row.names = FALSE)

#view unique counts in a column
summary(train$Sex)
#female   male 
#314    577 

#one way to compare sex to survival rates
#doesn't use axis so gives bad result
prop.table(table(train$Sex, train$Survived))
#                0          1
#female 0.09090909 0.26150393
#male   0.52525253 0.12233446

#better result with axis 1, which is rows
prop.table(table(train$Sex, train$Survived),1)
#               0         1
#female 0.2579618 0.7420382
#male   0.8110919 0.1889081

#update the column test$Survived, which we created above
#set the column to 1 where sex = female
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

#create second submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
perish_file = paste(base_dir, "02_women_survive.csv", sep = "")
write.csv(submit, file = perish_file, row.names = FALSE)

#look at age factor. Median age is 28
#177 NAs. Tutorials say to make those the average age, 28.
summary(train$Age)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.42   20.12   28.00   29.70   38.00   80.00     177 

#Our last few tables were on categorical variables, 
#ie. they only had a few values. Now we have a continuous variable 
#which makes drawing proportion tables almost useless, 
#as there may only be one or two passengers for each age! 
#So, let’s create a new variable, “Child”, to indicate whether the passenger is below the age of 18:

#new column Child. Set to one if passenger's age is below 18
train$Child <- 0
train$Child[train$Age < 18] <- 1

#fancy pants aggregate function
#organizes sum of survived (each adds 1) by Child and Sex
#NOTE: Important. This only shows the people who survived,
#Not the people who died, so you don't know the proportion.
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
#  Child    Sex Survived
#1     0 female      195
#2     1 female       38
#3     0   male       86
#4     1   male       23

#note this is intermediate. just to show you that you can get the totals
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
#  Child    Sex Survived
#1     0 female      259
#2     1 female       55
#3     0   male      519
#4     1   male       58

#The money shot! Gives proportion of adult/child, male/femail with lambda
#this still shows that mostly women survive and men die, so moving on to a different factor
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#  Child    Sex  Survived
#1     0 female 0.7528958
#2     1 female 0.6909091
#3     0   male 0.1657033
#4     1   male 0.3965517

#add new column Fare2 (Fare was in original data set)
#start with default value of 30+
train$Fare2 <- '30+'
#if fare between 20 and 30, set value to 20-30
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
#if fare between 10-20, set value to 10-20
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
#ditto for less than 10
train$Fare2[train$Fare < 10] <- '<10'

#new aggregate
#compare three variables: Fare2 (cost of ticket), Pclass (passenger class) and Sex.
#lambda to compare sum to length. All who survived will add 1 (sum) divided by length (total)
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#command-shift-c = comment how whole section
# Fare2 Pclass    Sex  Survived
# 1  20-30      1 female 0.8333333
# 2    30+      1 female 0.9772727
# 3  10-20      2 female 0.9142857
# 4  20-30      2 female 0.9000000
# 5    30+      2 female 1.0000000
# 6    <10      3 female 0.5937500
# 7  10-20      3 female 0.5813953
# 8  20-30      3 female 0.3333333 * these women died in unusual numbers
# 9    30+      3 female 0.1250000 * these women died in unusual numbers
# 10   <10      1   male 0.0000000
# 11 20-30      1   male 0.4000000
# 12   30+      1   male 0.3837209
# 13   <10      2   male 0.0000000
# 14 10-20      2   male 0.1587302
# 15 20-30      2   male 0.1600000
# 16   30+      2   male 0.2142857
# 17   <10      3   male 0.1115385
# 18 10-20      3   male 0.2368421
# 19 20-30      3   male 0.1250000
# 20   30+      3   male 0.2400000


#change the test survived column 
#first set default to 0
test$Survived <- 0
#if it's a woman assume she survived
test$Survived[test$Sex == 'female'] <- 1
#except if she like was 3rd class passenger with expensive ticket
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

#3rd submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
perish_file = paste(base_dir, "03_third_class_women_die.csv", sep = "")
write.csv(submit, file = perish_file, row.names = FALSE)

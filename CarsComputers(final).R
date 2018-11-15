#You should work harder Guillermo. Sorry. Not sorry


setwd("/Users/guillermoserrahv/Desktop/Ubiqum/CarsComputers")
getwd()
library(caret)
library(readxl)
library(rpart)

# Upload the Complete file
Survey_Responses_Complete <- read_excel("Survey_Key_and_Complete_Responses_excel.xlsx", 
sheet = "Survey Results Complete", col_types = c("numeric", "numeric", "numeric", "numeric", 
"numeric", "numeric", "text"))

Survey_Responses_Complete <- as.data.frame(Survey_Responses_Complete)

# Change the numerical values into labels for "brand", "zipcode" and "elevel" for the Complete set
Survey_Responses_Complete$brand<-replace(Survey_Responses_Complete$brand, Survey_Responses_Complete$brand == '0', "Acer")
Survey_Responses_Complete$brand<-replace(Survey_Responses_Complete$brand, Survey_Responses_Complete$brand == '1', "Sony")
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "0", "New England")
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "1", "Mid-Atlantic")
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "2", "East North Central")
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "3", "West North Central") 
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "4", "South Atlantic")
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "5", "East South Central")
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "6", "West South Central")
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "7", "Mountain")
Survey_Responses_Complete$zipcode<-replace(Survey_Responses_Complete$zipcode,Survey_Responses_Complete$zipcode == "8", "Pacific")
Survey_Responses_Complete$elevel<-replace(Survey_Responses_Complete$elevel,Survey_Responses_Complete$elevel == "0", "Less than High School")
Survey_Responses_Complete$elevel<-replace(Survey_Responses_Complete$elevel,Survey_Responses_Complete$elevel == "1", "High School Degree")
Survey_Responses_Complete$elevel<-replace(Survey_Responses_Complete$elevel,Survey_Responses_Complete$elevel == "2", "Some College")
Survey_Responses_Complete$elevel<-replace(Survey_Responses_Complete$elevel,Survey_Responses_Complete$elevel == "3", "4-year College Degree")
Survey_Responses_Complete$elevel<-replace(Survey_Responses_Complete$elevel,Survey_Responses_Complete$elevel == "4", "Masters or Doctoral")

# Visual plotting tests for the Complete set
#ggplot(Survey_Responses_Complete, aes(x=age, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
#ggplot(Survey_Responses_Complete, aes(x=age, y=credit, color=brand)) + geom_point(size=4, alpha=0.6)
#ggplot(Survey_Responses_Complete, aes(x=car, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
#ggplot(Survey_Responses_Complete, aes(x=zipcode, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
#ggplot(Survey_Responses_Complete, aes(x=elevel, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)

# Change the variables from numerical to factor on the Complete dataset
Survey_Responses_Complete$elevel<- as.factor(Survey_Responses_Complete$elevel)
Survey_Responses_Complete$car<- as.factor(Survey_Responses_Complete$car)
Survey_Responses_Complete$zipcode<- as.factor(Survey_Responses_Complete$zipcode)
Survey_Responses_Complete$brand<- as.factor(Survey_Responses_Complete$brand)

#qqplots and histograms and for each numeric variables
for(i in 1:(ncol(Survey_Responses_Complete))) {    #for every column
  if (is.numeric(Survey_Responses_Complete[,i])){  #if the variable is numeric
    qqnorm(Survey_Responses_Complete[,i],main=paste("Test",colnames(Survey_Responses_Complete)[i])) #plot qqnorm
    qqline(Survey_Responses_Complete[,i],col="red") #add qqnormal line in red
    hist(Survey_Responses_Complete[,i],main=paste("Histogram of",colnames(Survey_Responses_Complete)[i]), 
         xlab=colnames(Survey_Responses_Complete)[i])
  }
}
i

# Data partition - define an 75%train/25%test split of the dataset using "Y=brand"
is.na(Survey_Responses_Complete)
set.seed(123)
inTrainComplete <- createDataPartition(y=Survey_Responses_Complete$brand, p=.75, list=FALSE)
summary(Survey_Responses_Complete)
trainingComplete <- Survey_Responses_Complete[inTrainComplete,]
testingComplete <- Survey_Responses_Complete[-inTrainComplete,]

###########################################################TESTS kNN############################################################

# 10 fold cross validation
set.seed(123)
knntests <- trainControl(method="repeatedcv", number=10, repeats=3)

# kNN - All variables
knnall <- train(brand~.,data = trainingComplete,method = "knn", 
                 na.action = na.omit, trControl = knntests, tuneLength=20, 
                 preProcess = c("center", "scale"))
knnall
summary(knnall)
# kNN - Salary
knnsalary <- train(brand~salary,data = trainingComplete,method = "knn", 
                 na.action = na.omit, trControl = knntests, tuneLength=20, 
                 preProcess = c("center", "scale"))
knnsalary
summary(knnsalary)
# kNN - Age
knnage <- train(brand~age,data = trainingComplete,method = "knn", 
                      na.action = na.omit, trControl = knntests, tuneLength=20, 
                      preProcess = c("center", "scale"))
knnage
summary(knnage)
# kNN - Salary and Age
knnsalaryage <- train(brand~salary+age,data = trainingComplete,method = "knn", 
                  na.action = na.omit, trControl = knntests, tuneLength=20, 
                  preProcess = c("center", "scale"))
knnsalaryage
summary(knnsalaryage)
# kNN (7) - Salary and Age
knn7salaryage <- train(brand~salary+age,data = trainingComplete,method = "knn", 
                    na.action = na.omit, trControl = knntests, tuneGrid=expand.grid(k=c(7)), 
                    preProcess = c("center", "scale"))
knn7salaryage
summary(knn7salaryage)

###########################################################TESTS Random Trees#################################################################
# Random Trees - Tests
# 10 fold cross validation
set.seed(123)
treestests <- trainControl(method = "repeatedcv", number=10, repeats=3)

# Random Trees - All variables
randomall <- train(brand~., data=trainingComplete, method="rf", 
                       trControl=treestests, ntree=100)
randomall
summary(randomall)
# Random Trees - Salary
randomsalary <- train(brand~salary, data=trainingComplete, method="rf", 
                   trControl=treestests, ntree=100)
randomsalary
summary(randomsalary)
# Random Trees - Age
randomage <- train(brand~age, data=trainingComplete, method="rf", 
                      trControl=treestests, ntree=100)
randomage
summary(randomage)
# Random Trees - Salary and Age
randomsalaryage <- train(brand~salary+age, data=trainingComplete, method="rf", 
                         trControl=treestests, ntree=100)
randomsalaryage
summary(randomsalaryage)

#########################################CHOOSING THE MODEL AND MAKING PREDICTIONS ON COMPLETE DATASET#############################################

# CHOSEN MODEL - kNN (7) - Salary and Age
#set.seed(123)
knntests <- trainControl(method="repeatedcv", number=10, repeats=3)
knn7salaryage <- train(brand~salary+age,data=trainingComplete,method="knn", 
                       na.action=na.omit, trControl=knntests, tuneGrid=expand.grid(k=c(7)), 
                       preProcess=c("center", "scale"))
knn7salaryage
summary(knn7salaryage)

# Prediction Model for the Complete dataset using kNN (7)
predictionCompleted<- predict(knn7salaryage,testingComplete)
str(predictionCompleted)
predictionCompleted

# Prediction Model for the Random Forest
predictionCompletedRF<- predict(randomsalaryage,testingComplete)
str(predictionCompletedRF)
predictionCompletedRF

# Confusion Matrix - kNN (7) Salary and Age
confusionMatrix(predictionCompleted, testingComplete$brand)

# Confusion Matrix - Random Forest
confusionMatrix(predictionCompletedRF, testingComplete$brand)

#############################################PLACE THE MODEL INTO THE INCOMPLETE DATASET#########################################################
# Upload the Incomplete dataset file
Survey_Responses_Incomplete<-read_csv("SurveyIncomplete.csv") 

# Change the numerical values into labels for "brand", "zipcode" and "elevel"
Survey_Responses_Incomplete$brand<-replace(Survey_Responses_Incomplete$brand, Survey_Responses_Incomplete$brand == '0', "NA")
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "0", "New England")
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "1", "Mid-Atlantic")
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "2", "East North Central")
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "3", "West North Central") 
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "4", "South Atlantic")
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "5", "East South Central")
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "6", "West South Central")
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "7", "Mountain")
Survey_Responses_Incomplete$zipcode<-replace(Survey_Responses_Incomplete$zipcode,Survey_Responses_Incomplete$zipcode == "8", "Pacific")
Survey_Responses_Incomplete$elevel<-replace(Survey_Responses_Incomplete$elevel,Survey_Responses_Incomplete$elevel == "0", "Less than High School")
Survey_Responses_Incomplete$elevel<-replace(Survey_Responses_Incomplete$elevel,Survey_Responses_Incomplete$elevel == "1", "High School Degree")
Survey_Responses_Incomplete$elevel<-replace(Survey_Responses_Incomplete$elevel,Survey_Responses_Incomplete$elevel == "2", "Some College")
Survey_Responses_Incomplete$elevel<-replace(Survey_Responses_Incomplete$elevel,Survey_Responses_Incomplete$elevel == "3", "4-year College Degree")
Survey_Responses_Incomplete$elevel<-replace(Survey_Responses_Incomplete$elevel,Survey_Responses_Incomplete$elevel == "4", "Masters or Doctoral")

# Visual plotting tests for the Incomplete dataset
#ggplot(Survey_Responses_Incomplete, aes(x=age, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
#ggplot(Survey_Responses_Incomplete, aes(x=age, y=credit, color=brand)) + geom_point(size=4, alpha=0.6)
#ggplot(Survey_Responses_Incomplete, aes(x=car, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
#ggplot(Survey_Responses_Incomplete, aes(x=zipcode, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
#ggplot(Survey_Responses_Incomplete, aes(x=elevel, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)

# Change the variables from numerical to factor on the Incomplete dataset
Survey_Responses_Incomplete$elevel<- as.factor(Survey_Responses_Incomplete$elevel)
Survey_Responses_Incomplete$car<- as.factor(Survey_Responses_Incomplete$car)
Survey_Responses_Incomplete$zipcode<- as.factor(Survey_Responses_Incomplete$zipcode)
Survey_Responses_Incomplete$brand<- as.factor(Survey_Responses_Incomplete$brand)

# Applied prediction model from Complete dataset to the Incomplete dataset
predictionIncomplete<-predict(knn7salaryage, newdata=Survey_Responses_Incomplete)
predictionIncomplete
summary(predictionIncomplete)

# I am adding the column to the incomplete dataset using the information from the prediction
Survey_Responses_Incomplete$brandprediction<-predictionIncomplete


ggplot(Survey_Responses_Incomplete, aes(x=salary, y=age, color=brandprediction)) + geom_point(size=4, alpha=0.6)



plot(Survey_Responses_Incomplete$brandprediction) #if time use other graphs







# Load rpart and rpart.plot
library(rpart)
library(rpart.plot)
# Create a decision tree model
dtree<-rpart(brand~., data=TitanicData, cp=.02)
# Visualize the decision tree with rpart.plot
rpart.plot(dtree)


#Commands to learn
#anyNA() - To search for missing values across the dataset



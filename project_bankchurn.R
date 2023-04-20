#Importing necessary libraries
library(caret)
library(forecast)
library(rpart) 
library(rpart.plot)
library(randomForest)
library(reprtree)

#Importing bank churn data
bank_data <- read.csv('/Users/kalyanrohithtg/Documents/spring/machinelearning/project/bank_churn/BankChurn.csv')

bank_data$churn <- ifelse(bank_churn$churn=="Yes",1,0)

#Train-test splitting
train_index<-sample(rownames(bank_data), dim(bank_data)[1]*0.6)
valid_index<-setdiff(rownames(bank_data),train_index)
train_data<-bank_data[train_index, ]
valid_data<-bank_data[valid_index, ]

#Logit - Classifier model
mymodel<-glm(churn ~ estimated_salary+credit_score+country+gender+age+tenure+balance+products_number+credit_card+active_member, data=train_data, family='binomial')
summary(mymodel)  ## regression results

predicted_values<- predict(mymodel, type="response", newdata=valid_data)

confusionMatrix(relevel(as.factor(ifelse(predicted_values>0.5,1,0)),"1"),
                relevel(as.factor(valid_data$churn),"1"))


#CART - Classification tree
mytree <- rpart(churn ~ estimated_salary+credit_score+country+gender+age+tenure+balance+products_number+credit_card+active_member, data=train_data, method="class")

#Classification tree
prp(mytree)

#Predictions/Classifications
predicted_values <- predict(mytree, newdata=valid_data, type = "class")

#Evaluation metrics
confusionMatrix(relevel(as.factor(predicted_values), "1"), 
                relevel(as.factor(valid_data$churn), "1"))


train_data$churn<-as.factor(train_data$churn) 
valid_data$churn<-as.factor(valid_data$churn) 

#Randomforest
myforest <- randomForest(churn ~ estimated_salary+credit_score+country+gender+age+tenure+balance+products_number+credit_card+active_member, data = train_data)

plot(myforest)

#Predictions
predicted_values_forest <- predict(myforest, newdata=valid_data)

#Evaluation metrics
confusionMatrix(relevel(as.factor(predicted_values_forest), "1"), 
                relevel(as.factor(valid_data$churn), "1"))

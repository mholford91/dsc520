library (caret)
library (plyr)
library(tidyverse)
library(ggplot2)


diabetes<- read.csv("data/diabetes_data_upload.csv")
str(diabetes)
## convert aall predictors except age into factors 
diabetes<- diabetes %>% mutate_if(is.character, as.factor)
str(diabetes)
diabetes$class %>% table() %>% prop.table()*100
##  graphical representation 

# get distribution of ages from data set
ggplot(diabetes, aes(x=Age)) + geom_bar()
table(diabetes$Age)

names(diabetes)[3] <- "frequent.urination"
names(diabetes)[4] <- "extreme.thirst"
names(diabetes)[7] <- "extreme.hunger"
names(diabetes)[13] <- "muscle.impairment"


colnames(diabetes)


str(diabetes)

summary(diabetes)

##  create train and test data set 
set.seed(100)
diabetes_data<- createDataPartition(diabetes$class, p=.75, list = F)
train_diabetes<- diabetes[diabetes_data,]
test_diabetes<- diabetes[-diabetes_data,]

## compare frequencies of diagnosis in test and train data set against the original dataset
train_set<-round(prop.table(table(train_diabetes$class))*100,1)
test_set<- round(prop.table(table(test_diabetes$class))*100,1)
original_set<- round(prop.table(table(diabetes$class))*100,1) 
set_freq<- data.frame(cbind(original_set,train_set,test_set))
colnames(set_freq)<- c("Original","Training", "Testing")
set_freq ## the frequencies are  similar

## parameter tuning 
diabetes_control<- trainControl(method = "repeatedcv", number = 10, repeats = 3)

##  KNN classification 
library(e1071)
set.seed(100)
diabetes_knnFit<- train(class~., data = train_diabetes, method="knn", preProcess=c("center","scale"),
                        metric="Accuracy", tuneLength=17, trControl=diabetes_control)
diabetes_knnFit
ggplot(diabetes_knnFit, aes(x=k_values, y=Accuracy)) + geom_point() + geom_line(colour="red")
scale_x_continuous(breaks = c(1:43)) # k=5 has highest accuracy on train data

## make predictions on test dataset 
diabetes_knnPred<- predict(diabetes_knnFit, test_diabetes)

##  build confusion matrix 
cmKnn<- confusionMatrix(diabetes_knnPred, test_diabetes$class, positive = "Positive")
cmKnn # knn algorithm has 90% accuracy 

##  building classification tree using rpart 
set.seed(100)
diabetes_rpartFit<- train(class~., data = train_diabetes, method="rpart", metric="Accuracy", tuneLength=17, trControl=diabetes_control)
##  view the classification tree
rpart.plot::rpart.plot(diabetes_rpartFit$finalModel) 

##  make predicition on test data 
diabetes_rpartPred<- predict(diabetes_rpartFit, test_diabetes)
cmRpart<- confusionMatrix(diabetes_rpartPred, test_diabetes$class, positive = "Positive")
cmRpart ## 87% Accuracy rate 

##  logistic regression 
set.seed(100)
diabetes_lrFit<-train(class~., data = train_diabetes, method="glm", family="binomial", metric="Accuracy", 
              tuneLength=17, trControl=diabetes_control)

# make predictions 
diabetes_lrPred<- predict(diabetes_lrFit, test_diabetes)
cmLr<- confusionMatrix(diabetes_lrPred, test_diabetes$class, positive = "Positive")
cmLr ## logistic regression has an accuracy rate of 93%

##  Random forest 
set.seed(100)
diabetes_rfFit<- train(class~., data = train_diabetes, method="rf", metric="Accuracy", tuneLength=17, trControl=diabetes_control)
diabetes_rfFit %>% plot() # mtry of 5 has highest accuracy 
diabetes_rfFit

## make predictiions on the test data set
diabetes_rfPred<- predict(diabetes_rfFit, test_diabetes)
cmRf<- confusionMatrix(diabetes_rfPred, test_diabetes$class, positive = "Positive")
cmRf# randomforest has accuracy rate of 98%

library(gbm)
diabetes_gb<- expand.grid(.interaction.depth = (1:5) * 2,.n.trees = (1:10)*25, .shrinkage = c(0.01,0.05,0.1,0.5),
                      .n.minobsinnode=10)
set.seed(100)
diabetes_gbFit<- train(class~., data = train_diabetes, method="gbm", metric="Accuracy", 
               trControl=diabetes_control, tuneGrid=diabetes_gb, verbose=FALSE, distribution="bernoulli",tuneLength=17)
diabetes_gbFit$finalModel

##  make a predition using test data set
diabetes_gbPred<- predict(diabetes_gbFit, test_diabetes)
cmGb<- confusionMatrix(diabetes_gbPred, test_diabetes$class, positive = "Positive")
cmGb ## accuracy of 96%


## compare performances of the models 
model_comp<- resamples(list(Knn=diabetes_knnFit, LogisticReg=diabetes_lrFit, RpartTree=diabetes_rpartFit, 
                            RandomForest=diabetes_rfFit, GBM=diabetes_gbFit))
summary(model_comp)
dotplot(model_comp) # display the accuracy and kappa values for the different models (Radomforest, GBM and SVM have highest accuracy rates)
##  create a function that compares the performance of different models 
## function that to round values in a list if it is numeric 
round_num<- function(list){
  lapply(list, function(x){
    if(is.numeric(x)){
      x=round(x, 2) # round to 2 D.P
    }
    
  })
}

##  create a function that compares results of the models 

comp_summ<- function(cm, fit){
  summ<- list(TN= cm$table[1,1], #  True Negative
              TP= cm$table[2,2], #  True Positive
              FN= cm$table[1,2], #  False Negative
              FP= cm$table[2,1], #  False Positive
              Acc=cm$overall["Accuracy"], # Accuracy
              Sens=cm$byClass["Sensitivity"], # Sensitivity
              Spec=cm$byClass["Specificity"], # Specificity
              Prec=cm$byClass["Precision"], # Precision
              Recall= cm$byClass["Recall"], # Recall
              F1_Score=cm$byClass["F1"], #  F1 score
              PPV= cm$byClass["Pos Pred Value"], #  Positive predictive value
              NPV= cm$byClass["Neg Pred Value"] # Negative predictive value
  )
  round_num(summ) # rounds to 2 D.P
}

##  create a dataframe that stores the performance of the models

model_performance<- data.frame(rbind(comp_summ(cmLr,diabetes_lrFit),
                                     comp_summ(cmKnn,diabetes_knnFit),
                                     comp_summ(cmRpart,diabetes_rpartFit),
                                     comp_summ(cmRf,diabetes_rfFit),
                                     comp_summ(cmGb,diabetes_gbFit)))
##  create names for rows in model performanc
rownames(model_performance)<- c("LogisticReg","KNN","RpartTree","RandomForest", "GradientBoosting")
model_performance


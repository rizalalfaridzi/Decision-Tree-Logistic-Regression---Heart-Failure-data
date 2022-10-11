#clear env vars
rm(list=ls())

#set directory
setwd("~/Downloads/BUSN5002/Assignment") 

#load libraries
library(tidyverse)
library(psych)
library(car)
library(tidymodels)
library(MASS)
library(GGally)
library(superml)
library(bonsai)
library(mltools)
library(data.table)
library(corrplot)
library(tidyverse)
library(caret)
library(Boruta)
library(tidymodels)
library(baguette)
library(discrim)
if (!require("pacman")) install.packages("pacman")
## load packages (including pacman) with pacman
pacman::p_load(pacman, tidyverse, gmodels,caret,ROCR)

#load heart failure data
df <- read_csv('heart.csv')

####Attribute Information
#Age: age of the patient [years]
#Sex: sex of the patient [M: Male, F: Female]
#ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
#RestingBP: resting blood pressure [mm Hg]
#Cholesterol: serum cholesterol [mm/dl]
#FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
#RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria]
#MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]
#ExerciseAngina: exercise-induced angina [Y: Yes, N: No]
#Oldpeak: oldpeak = ST [Numeric value measured in depression]
#ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping]
#HeartDisease: output class [1: heart disease, 0: Normal]
#######

#description every variable properties
summary(df)

df_for_eda <- df

view(df_for_eda)
#assigning variables as factor (to df and df_for_eda)
df_factors<-c("Sex", "ExerciseAngina", 
              "ChestPainType", "ST_Slope", "RestingECG")
df[df_factors]<- lapply(df[df_factors], as.factor)

#label encoding

df$Sex <- ifelse(df$Sex == "M",1,0) #male = 1 , Female = 0
df$ExerciseAngina <- ifelse(df$ExerciseAngina == "Y",1,0) #Y = 1 , N = 0
dummy <- dummyVars(" ~ .", data=df)
newdata <- data.frame(predict(dummy, df))
glimpse(newdata)

df_for_eda$HeartDisease <- ifelse(df_for_eda$HeartDisease == 1 ,"Diseased","Not Diseased") 

#checking data type again
summary(df)

#plot for EDA
#boxplot for numerical variables
require(reshape2)
df_numeric<-c("Age", "RestingBP", "Cholesterol", "MaxHR")
boxplot(df_for_eda[df_numeric],horizontal = TRUE, cex=1, 
        cex.axis=0.685, main = "Boxplot for Age, RestingBP, Cholestrol, MaxHR")  

#boxplot for oldpeak
boxplot(df_for_eda[c("Oldpeak")], horizontal  = TRUE, main = "Boxplot for oldpeak",
        cex.axis=0.685, names=c("Oldpeak"))  

boxplot(df_for_eda[c("Age")], horizontal  = TRUE, main = "Boxplot for Age",
        cex.axis=0.685, names=c("Age"))  

#pie chart of heartdisease
# Pie Chart from data frame with Appended Sample Sizes
library(plotrix)
mytable <- table(df_for_eda$HeartDisease)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie3D(mytable, labels = lbls,
    main="Pie Chart of Heart Disease")

#heart disease based on sex
ggplot(df_for_eda, aes(Sex, fill = HeartDisease))+
  geom_bar(position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", 
            position = position_stack(vjust = 0.5)) +
  ylab("count") + 
  ggtitle("HeartDisease based on Sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#heart disease based on ST_Slope
ggplot(df_for_eda, aes(ST_Slope, fill = HeartDisease))+
  geom_bar(position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", 
            position = position_stack(vjust = 0.5)) +
  ylab("count") + 
  ggtitle("HeartDisease based on ST_Slope") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#heart disease based on Chestpaintype
ggplot(df_for_eda, aes(ChestPainType, fill = HeartDisease))+
  geom_bar(position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", 
            position = position_stack(vjust = 0.5)) +
  ylab("count") + 
  ggtitle("HeartDisease based on ChestPainType") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

#heart disease based on RestingECG
ggplot(df_for_eda, aes(RestingECG, fill = HeartDisease))+
  geom_bar(position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", 
            position = position_stack(vjust = 0.5)) +
  ylab("count") + 
  ggtitle("HeartDisease based on RestingECG") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

#heart disease based on exerciseangina
ggplot(df_for_eda, aes(ExerciseAngina, fill = HeartDisease))+
  geom_bar(position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", 
            position = position_stack(vjust = 0.5)) +
  ylab("count") + 
  ggtitle("HeartDisease based on ExerciseAngina") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

#heart disease based on FastingBS
ggplot(df_for_eda, aes(FastingBS, fill = HeartDisease))+
  geom_bar(position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", 
            position = position_stack(vjust = 0.5)) +
  ylab("count") + 
  ggtitle("HeartDisease based on FastingBS") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

#heart disease based on #age distribution
ggplot(df_for_eda, aes(Age, fill = HeartDisease))+
  geom_bar(position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", 
            position = position_stack(vjust = 0.5), size = 2.5) +
  ylab("count") + 
  ggtitle("HeartDisease based on Age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#correlation plot numerical var only
#df_numeric<-c("Age", "Sex", "RestingBP", "Cholesterol", "FastingBS", "MaxHR",
        #      'ExerciseAngina','Oldpeak','HeartDisease')

#corPlot(df[df_numeric],min.length = 3,cex = 0.75,scale = FALSE)

#correlation plot after label encoding
corPlot(newdata,min.length = 3,cex = 0.75,scale = FALSE)


#crosstabs (chi-sq test) for ST_slope
#expected values specify what the values of each cell of the table would
#be if there was no association between the two variables.
CrossTable(df$HeartDisease,df$ST_Slope, digits=3, 
           max.width = 5, expected=TRUE, prop.r=FALSE, prop.c=TRUE,
           prop.t=FALSE, prop.chisq=FALSE, chisq = TRUE, fisher=FALSE, mcnemar=FALSE,
           resid=FALSE, sresid=FALSE, asresid=FALSE,
           missing.include=FALSE,
           format=c("SPSS"), dnn = NULL)

#crosstabs (chi-sq test) for chestpaintype
#expected values specify what the values of each cell of the table would
#be if there was no association between the two variables.
CrossTable(df$HeartDisease,df$ChestPainType, digits=3, 
           max.width = 5, expected=TRUE, prop.r=FALSE, prop.c=TRUE,
           prop.t=FALSE, prop.chisq=FALSE, chisq = TRUE, fisher=FALSE, mcnemar=FALSE,
           resid=FALSE, sresid=FALSE, asresid=FALSE,
           missing.include=FALSE,
           format=c("SPSS"), dnn = NULL)

#t-test for exercise angina
t.test(df$ExerciseAngina~df$HeartDisease,mu=0,
       alternative = "two.sided",conf.level = 0.95,  var.equal = TRUE)

#check null values
sum(is.na(newdata))

view(newdata)
#train test splitting
set.seed(777)
newdata$HeartDisease <- as.factor(newdata$HeartDisease)
heart_split <- initial_split(newdata, prop = 0.80)
heart_train <- training(heart_split)
heart_test  <- testing(heart_split)

#fitting data to log regression without outlier treatment
glimpse(newdata)
set.seed(777)

model_1_logreg <- glm(HeartDisease~ .,data = heart_train,family = 
                        binomial(link = "logit"))
summary(model_1_logreg) #still has multicolinearity
formula(model_1_logreg)
vif(model_1_logreg)


#stepwise model selection by AIC. (feature selection) 
model_2 <- stepAIC(model_1_logreg, direction = "both")

#check multicolinearity (2 or more independent vars affecting each other)
(vif_vars <- as.data.frame(vif(model_2))) #no multicolinearity (no value > 5)
vif(model_2)
summary(model_2)


#predicting on training & test set log regression
pred_train <- predict(model_2,newdata = heart_train,type = "response")
pred_test <- predict(model_2,newdata = heart_test ,type = "response")

#evaluating model on training data log regression
pred_trainHeartDisease <- factor(ifelse(pred_train >= 0.50, 1, 0))
actual_trainHeartDisease <- factor(ifelse(heart_train$HeartDisease==1,1,0))
table(pred_trainHeartDisease,actual_trainHeartDisease)

confusion_matrix_training <- as.data.frame(table(pred_trainHeartDisease,
                                                 actual_trainHeartDisease))


#evaluating model on testing data log regression
pred_testHeartDisease <- factor(ifelse(pred_test >= 0.50, 1, 0))
actual_testHeartDisease <- factor(ifelse(heart_test$HeartDisease==1,1,0))
#confusion_matrix_test<- as.data.frame(table(pred_testHeartDisease,actual_testHeartDisease))


#evaluation using caret on training & testing data log regression
caret::confusionMatrix(pred_trainHeartDisease,actual_trainHeartDisease,positive = "1")
caret::confusionMatrix(pred_testHeartDisease,actual_testHeartDisease,positive = "1")

#shows f1, precision, recall on test data log regression
cm_log<-confusionMatrix(pred_testHeartDisease,actual_testHeartDisease)
recall_log <- cm_log$byClass["Recall"]
precision_log <- cm_log$byClass["Precision"]
f1_log <- cm_log$byClass["F1"]
accuracy_log <- cm_log$overall["Accuracy"]
sensitivity_log <- cm_log$byClass["Sensitivity"]
specificity_log <- cm_log$byClass["Specificity"]
cat(" Accuracy of Logistic Regression on test dataset is:", accuracy_log,"\n",
    "Specificity of Logistic Regression on test dataset is",specificity_log, "\n",
    "Sensitivity of Logistic Regression on test dataset is",sensitivity_log,
    "\n","Precision of Logistic Regression on test dataset is:", precision_log,
    "\n","Recall of Logistic Regression on test dataset is:", recall_log, "\n", 
    "F1 Score of Logistic Regression on test dataset is:",  f1_log )

# Use the predictions to build a ROC curve to assess the performance of our model
library(ROCR)
library(psych)
#install.packages("cowplot") #new package for the unit library(cowplot)
predROCR <- prediction(pred_test, heart_test$HeartDisease)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE) + abline(0,1)
performance(predROCR, "auc")@y.values

#R^2 score
model_2_McfR2=1 -deviance(model_2)/model_2[["null.deviance"]]
model_2_McfR2


####DECISION TREE CLASSIFIER
#for tree based classification
df2 <- read_csv('heart.csv')
df2_factors<-c("Sex", "ExerciseAngina", "ChestPainType", "ST_Slope", "RestingECG")
df2[df2_factors]<- lapply(df2[df2_factors], as.factor)
df2$HeartDisease <- as.factor(df2$HeartDisease)

#training & test
set.seed(12)
split_model <- createDataPartition(y=df2$HeartDisease, # Split on target var
                                   list = FALSE,      
                                   p=0.80,            # 75% of data in the training set
                                   times=1)           # Make 1 split
training_set <- df2[split_model,]     # Get the new training set
test_set <- df2[-split_model,]  # Get the test set

library(rpart) # Prediction: Decision Tree
library(rpart.plot) # Prediction: Decision Tree

# Fitting Decision Tree Classification Model to the Training set
classifier_DT = rpart(HeartDisease ~ ., data = training_set, 
                      method = 'class')

# Tree Visualization
rpart.plot(classifier_DT, extra=4, 
           type = 4, nn = TRUE) #1 have heartdisease, 0 don't have heartdisease

# Predicting the Validation set results
y_pred_training = predict(classifier_DT, 
                            newdata = training_set[,-which(names(training_set)=="HeartDisease")],
                            type='class')


#comparison actual target value on validation and prediciton on validation
#caret::confusionMatrix(y_pred_validation,test$HeartDisease,positive = "1")

#show precision , recall, f1 score on validation
library(caret)
cm<-confusionMatrix(training_set$HeartDisease,y_pred_training)
recall_DT <- cm$byClass["Recall"]
precision_DT <- cm$byClass["Precision"]
f1_DT <- cm$byClass["F1"]
accuracy_DT <- cm$overall["Accuracy"]
sensitivity_DT <- cm$byClass["Sensitivity"]
specificity_DT <- cm$byClass["Specificity"]
cat(" Accuracy of Decision Tree on training dataset is:", accuracy_DT,"\n",
    "Specificity of Decision Tree on training dataset is",specificity_DT, "\n",
    "Sensitivity of Decision Tree on training dataset is",sensitivity_DT,
    "\n","Precision of Decision Tree on training dataset is:", precision_DT,
    "\n","Recall of Decision Tree on training dataset is:", recall_DT, "\n", 
    "F1 Score of Decision Tree on training dataset is:",  f1_DT )

#try predict on test_set with default params
# Predicting the test set results
set.seed(777)
y_pred_test = predict(classifier_DT, 
                            newdata = test_set[,-which(names(test_set)=="HeartDisease")],
                            type='class')

#comparison actual target value on test and prediciton on test
caret::confusionMatrix(y_pred_test,test_set$HeartDisease,positive = "1")
#show precision , recall, f1 score on test set

library(caret)
cm<-confusionMatrix(test_set$HeartDisease,y_pred_test)
recall_DT_testset <- cm$byClass["Recall"]
precision_DT_testset <- cm$byClass["Precision"]
f1_DT_testset <- cm$byClass["F1"]
accuracy_DT_testset <- cm$overall["Accuracy"]
sensitivity_DT_testset <- cm$byClass["Sensitivity"]
specificity_DT_testset <- cm$byClass["Specificity"]
cat(" Accuracy of Decision Tree on test dataset is:", accuracy_DT_testset,"\n",
    "Specificity of Decision Tree on test dataset is",specificity_DT_testset, "\n",
    "Sensitivity of Decision Tree on test dataset is",sensitivity_DT_testset,
    "\n","Precision of Decision Tree on test dataset is:", precision_DT_testset,
    "\n","Recall of Decision Tree on test dataset is:", recall_DT_testset, "\n", 
    "F1 Score of Decision Tree on test dataset is:",  f1_DT_testset )

caret::confusionMatrix(y_pred_test,test_set$HeartDisease,positive = "1")

#ROC AUC test set decision tree default param
library("ROCR")
Pred.cart = predict(classifier_DT, 
                    newdata = test_set[,-which(names(test_set)=="HeartDisease")],
                    type="prob")[,2]

predROCR <- prediction(Pred.cart, test_set$HeartDisease)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE) + abline(0,1)
performance(predROCR, "auc")@y.values

#show variable importance 
df_varimp <- data.frame(imp = classifier_DT$variable.importance)
df2 <- df_varimp %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()



#================================================================================

#trying out cross validation & splitting type (not covered in report cause lower result to default params) 


# Applying k-Fold Cross Validation on Gini split
set.seed(222)
folds = createMultiFolds(training_set$HeartDisease, k = 10, times = 2)
control <- trainControl(method = "repeatedcv", index = folds)

classifier_cv_DT_gini <- train(HeartDisease ~ . , 
                          data = training_set, method = "rpart", trControl = control,
                          parms = list(split = "gini"))

y_pred_train = predict(classifier_cv_DT_gini, 
                               newdata = training_set[,-which(names(training_set)=="HeartDisease")])

#comparison actual target value on validation and prediciton on validation
#caret::confusionMatrix(y_pred_validation_cv,test$HeartDisease,positive = "1")

#show precision , recall, f1 score on training_set
library(caret)
cm<-confusionMatrix(training_set$HeartDisease,y_pred_train)
recall_DT <- cm$byClass["Recall"]
precision_DT <- cm$byClass["Precision"]
f1_DT <- cm$byClass["F1"]
accuracy_DT <- cm$overall["Accuracy"]
sensitivity_DT <- cm$byClass["Sensitivity"]
specificity_DT <- cm$byClass["Specificity"]
cat(" Accuracy of Decision Tree training_set Gini is:", accuracy_DT,"\n",
    "Specificity of Decision Tree training_set Gini is",specificity_DT, "\n",
    "Sensitivity of Decision Tree training_set Giniis",sensitivity_DT,
    "\n","Precision of Decision Tree training_set Gini is:", precision_DT,
    "\n","Recall of Decision Tree training_set Gini is:", recall_DT, "\n", 
    "F1 Score of Decision Tree training_set Gini is:",  f1_DT )


# Predicting the test set results
set.seed(222)
y_pred_test = predict(classifier_cv_DT_gini, 
                      newdata = test_set[,-which(names(test_set)=="HeartDisease")])

#comparison actual target value on validation and prediciton on validation
#caret::confusionMatrix(y_pred_test,test_set$HeartDisease,positive = "1")

#show precision , recall, f1 score on test set
library(caret)
cm<-confusionMatrix(test_set$HeartDisease,y_pred_test)
recall_DT_testset <- cm$byClass["Recall"]
precision_DT_testset <- cm$byClass["Precision"]
f1_DT_testset <- cm$byClass["F1"]
accuracy_DT_testset <- cm$overall["Accuracy"]
sensitivity_DT_testset <- cm$byClass["Sensitivity"]
specificity_DT_testset <- cm$byClass["Specificity"]
cat(" Accuracy of Decision Tree test_set Gini is:", accuracy_DT_testset,"\n",
    "Specificity of Decision Tree test_set Gini is",specificity_DT_testset, "\n",
    "Sensitivity of Decision Tree test_set Gini is",sensitivity_DT_testset,
    "\n","Precision of Decision Tree test_set Gini is:", precision_DT_testset,
    "\n","Recall of Decision Tree test_set Gini is:", recall_DT_testset, "\n", 
    "F1 Score of Decision Tree test_set Gini is:",  f1_DT_testset )

#ROC AUC DECISON TREE GINI
Pred.cart = predict(classifier_DT, 
                    newdata = test_set[,-which(names(test_set)=="HeartDisease")],
                    type="prob")[,2]

predROCR <- prediction(Pred.cart, test_set$HeartDisease)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE) + abline(0,1)
performance(predROCR, "auc")@y.values



#######
# Applying k-Fold Cross Validation on information split
set.seed(222)
folds = createMultiFolds(training_set$HeartDisease, k = 10, times = 2)
control <- trainControl(method = "repeatedcv", index = folds)

classifier_cv_DT_info <- train(HeartDisease ~ ., 
                       data = training_set, method = "rpart", trControl = control,
                       parms = list(split = "information"),
                       control = rpart.control(maxdepth = 2,minsplit = 20, minbucket=7))

y_pred_training_info = predict(classifier_cv_DT_info, 
                            newdata = training_set[,-which(names(training_set)=="HeartDisease")])

#comparison actual target value on validation and prediciton on validation
#caret::confusionMatrix(y_pred_validation_cv,test$HeartDisease,positive = "1")

#show precision , recall, f1 score on training_set
library(caret)
cm<-confusionMatrix(training_set$HeartDisease,y_pred_training_info)
recall_DT <- cm$byClass["Recall"]
precision_DT <- cm$byClass["Precision"]
f1_DT <- cm$byClass["F1"]
accuracy_DT <- cm$overall["Accuracy"]
sensitivity_DT <- cm$byClass["Sensitivity"]
specificity_DT <- cm$byClass["Specificity"]
cat(" Accuracy of Decision Tree training_set information split is:", accuracy_DT,"\n",
    "Specificity of Decision Tree training_set information split is",specificity_DT, "\n",
    "Sensitivity of Decision Tree training_set information split is",sensitivity_DT,
    "\n","Precision of Decision Tree training_set information split is:", precision_DT,
    "\n","Recall of Decision Tree training_set information split is:", recall_DT, "\n", 
    "F1 Score of Decision Tree training_set information split is:",  f1_DT )

# Predicting the test set results
set.seed(222)
y_pred_test = predict(classifier_cv_DT_info, 
                      newdata = test_set[,-which(names(test_set)=="HeartDisease")])

#comparison actual target value on validation and prediciton on validation
#caret::confusionMatrix(y_pred_test,test_set$HeartDisease,positive = "1")

#show precision , recall, f1 score on test set
library(caret)
cm<-confusionMatrix(test_set$HeartDisease,y_pred_test)
recall_DT_testset <- cm$byClass["Recall"]
precision_DT_testset <- cm$byClass["Precision"]
f1_DT_testset <- cm$byClass["F1"]
accuracy_DT_testset <- cm$overall["Accuracy"]
sensitivity_DT_testset <- cm$byClass["Sensitivity"]
specificity_DT_testset <- cm$byClass["Specificity"]
cat(" Accuracy of Decision Tree test_set information split on is:", accuracy_DT_testset,"\n",
    "Specificity of Decision Tree test_set information split is",specificity_DT_testset, "\n",
    "Sensitivity of Decision Tree test_set information split is",sensitivity_DT_testset,
    "\n","Precision of Decision Tree test_set information split is:", precision_DT_testset,
    "\n","Recall of Decision Tree test_set information split is:", recall_DT_testset, "\n", 
    "F1 Score of Decision Tree test_set information split is:",  f1_DT_testset )
  # 6324 Final Project
  # Project Code Version 2
  # R Version 4.1.2 (for library compatibility)
  
  #Data dictionary for the heart disease dataset
  #https://www.kaggle.com/datasets/kamilpytlak/personal-key-indicators-of-heart-disease
  
  
  #Reference of similar work
  
  # https://www.kaggle.com/code/georgyzubkov/heart-disease-exploratory-data-analysis
  # has good exploratory data methods including graphs.
  
  
  
  #Clear Environment
  remove(list = ls())
  
  #Add libraries
  
  
  library(caret)  # confusion matrix
  library(e1071) # svm 
  #library(DMwR) #for SMOTE
  library(pROC) #ROC/AUC
  library(ggplot2)
  library(dplyr)

  
  #Set working directory (you can add yours and comment the rest out)
  setwd("/Users/nicoleng/Dropbox/SMU/MASDA/STAT 6324/Project") #Nicole
  
  #Read dataset
  heart <- read.csv("heart_2020_cleaned.csv")
  
  #Set seed number
  set.seed(1234)
  
  #Preliminary inspection of data
  str(heart)
  summary(heart)
  
  #EDA
  #Describe variables and distribution
  #Ex: Comparing explanatory variables to variable:heartdisease (like race/age/diabetes etc...)
  #Through EDA, we should be able to show that the graphs presented, it can be concluded that alcohol consumption and smoking are not the main factors in heart disease, as scientists testify to this.
  
  
  #We could also check for multicollinearity for between variables 
  
  
  
  #Check for missing data
  
  sum(is.na(heart)) #note: results show no missing values
  
  
  #Check for outliers
  
  #Get a basic description of numeric data
  # 4 variables
  # Hypothesis : There are outliers in the data for both maximum and minimum values.
  # show distribution
  # it should show that only the BMI variable is close to the normal distribution, the rest are close to bimodal.
  
  #sleep
  #not possible to have 24 hr sleep in a 24 hr period unless they are hospitalized and sedated or something
  sleepout1 <- subset(heart,SleepTime>23) #there are 30 obs that have 24 hr sleep in 24 hr period
  sleepout2 <- subset(heart,SleepTime>19) #there are 108 obs that have 20 hrs or more sleep in a 24 hr period
  
  
  #factorize categorical variables
  #one-hot encoding if needed
  #heart$HeartDisease <- as.factor(heart$HeartDisease)
  
  heart <- as.data.frame(unclass(heart),                     # Convert all columns to factor
                         stringsAsFactors = TRUE)
  
  #data transformations and creation of new variables
  
  
  
  #random sampling to reduce run time and debugging
  heart_old2 <- heart
  #heart <- heart[sample(1:nrow(heart), 5000), ] #Comment out for actual 
  
  #split to training and test set
  
  #split using stratified sampling to maintain same class imbalance in training and test sets
  
  rows <- sample(nrow(heart))
  dat <- heart[rows, ]
  train_index <- createDataPartition(heart$HeartDisease, p = .8, list = FALSE)
  training <- dat[train_index, ]  
  test <- dat[-train_index, ]
  
  #Check for class imbalance for y-variable in the training set
  
  table(training$HeartDisease) #huge class imbalance
  
  #note: because of huge class imbalance it will show an accuracy of 90% or higher because the majority are healthy people
  # consider showing a model without balancing classes to show high accuracy as misleading
  # this misleading information can be confirmed with 
  
  #Use SMOTE to balance the training set, make sure to check for any assumptions in using SMOTE
  # might not be able to use SMOTE because the imbalance is really huge
  # might be better to downsample the healthy people
  
  # also consider balancing the test set
  # there is a lot of debate around whether to balance test sets
  # the rationale for balancing this test set is due to how large the class imbalance
  
  #Downsample training and test sets (might be able to downsample before splitting as well)
  
  # # Down Sample
  training_old1 <- training #make a copy for comparison
  training <- downSample(training,
                         y = training$HeartDisease)
  
  test_old1 <- test #make a copy for comparison
  test <- downSample(test,
                           y = test$HeartDisease)
  
  #in the modeling stage, the training set is reduce so need original for each iteration of model
  training_original <- training 
  test_original <- test
  
  #Check assumptions for logistic regression in the training set (important to test after SMOTE)
  #ex: multicollinearity
  
  
  #Modeling notes:
  #Base Logistic Model should be able to achieve AUC of 0.84 
  #Keep the same method for prediction, confusion matrix and AUC calculations so that the models can be compared
  
  ## Model 11 - START - Base Logistic Model ##
  # This is a template to use for the different models
  
  #model 11: PCA
  
  #model 11: parameter tuning
  
  #model 11: logistic model 1
  
  #model 11: prediction
  
  #model 11: confusion matrix and auc
  
  ## Model 11 - END - Base Logistic Model ##
  
  
  
  
  
  
  ## Model 12 - START - Logistic Model 2##
  #  Base Model
  # AUC of 0.85 

  #model 12: Select Variables to Model
  colnames(heart) # to list variables to select from
  selectvars <- c("HeartDisease",
                  "BMI","PhysicalHealth","MentalHealth","SleepTime",
                  "Smoking","AlcoholDrinking","Stroke","DiffWalking","Sex",
                  "AgeCategory","Race","Diabetic","PhysicalActivity","GenHealth",
                  "Asthma","KidneyDisease","SkinCancer")
  training <- subset(training_original,select=selectvars)
  test <- subset(test_original,select=selectvars)
  
  
  #model 12: PCA
  
  #model 12: parameter tuning
  
  #model 12: logistic model 2
  
  model <- glm(HeartDisease ~ ., family = "binomial", data=training)
  
  #model 12: disable scientific notation for model summary
  options(scipen=999)
  
  #model 12: view model summary
  summary(model)
  
  #model 12: prediction
  #predict using test set
  probabs <- predict(model, test, type="response")

  predicted <- ifelse(probabs > 0.5, 'Yes', 'No')
  predicted <- as.factor(predicted)
  
  #model 12: confusion matrix
  # #confusion matrix
  cm <- confusionMatrix(test$HeartDisease, probabs)
  cm
  
  #model 12: AUC and ROC
  roc_model=roc(response=test$HeartDisease, predictor= factor(probabs,
                                                            ordered = TRUE), plot=TRUE,quiet = TRUE)
  auc_model<-auc(roc_model)
  
  ## Model 12 - END -  Logistic Model 2 ##
  
  

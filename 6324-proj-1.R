# 6324 Final Project
#Version 1

#Data dictionary for the heart disease dataset
#https://www.kaggle.com/datasets/kamilpytlak/personal-key-indicators-of-heart-disease

#Clear Environment
remove(list = ls())

#Add libraries

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
#We could also check for multicollinearity for between variables 

#Check for missing data

#Check for outliers

#Check for class imbalance for y-variable

#Use SMOTE to balance data set, make sure to check for any assumptions in using SMOTE

#Check assumptions for logistic regression
#ex: multicollinearity


#split to training and test set



## Model 11 - START - Base Logistic Model ##

#model 11: PCA

#model 11: parameter tuning

#model 11: logistic model 1

#model 11: prediction

#model 11: confusion matrix and auc

## Model 11 - END - Base Logistic Model ##



## Model 21 - START - Base SVM Model ##

#model 21: PCA

#model 21: parameter tuning

#model 21: svm model 1

#model 21: prediction

#model 21: confusion matrix and auc

## Model 21 - END - Base Logistic Model ##



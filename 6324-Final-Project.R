# 6324 Final Project
# R Version 4.1.2 (for library compatibility)
# CANNOT be used for R that is higher than 4.1.2

#Data dictionary for the heart disease dataset
#https://www.kaggle.com/datasets/kamilpytlak/personal-key-indicators-of-heart-disease

#Clear Environment
remove(list = ls())

#Add libraries
library(caret)  # confusion matrix
library(pROC) #ROC/AUC
library(ggplot2)
library(dplyr)
library(car)
library(DMwR) #for SMOTE
library(ggpubr)
library(e1071) 
library(caret)
library(glmnet)
library(plotmo)
library(mice)
library(corrplot)
library(gmodels)


#Set working directory
setwd("/Users/nicoleng/Dropbox/SMU/MASDA/STAT 6324/Project") #Nicole

#Read dataset
heart <- read.csv("heart_2020_cleaned.csv")

#Set seed number
set.seed(1234)

#Preliminary inspection of data
str(heart)
summary(heart)

#EDA (Allison)
#Visualize missing data - no missing values!!
md.pattern(heart)
simple_aggr = aggr(heart, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(heart), cex.axis=.7, 
                   gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#Boxplot of bmi by heart disease - outliers check
boxplot(BMI ~ HeartDisease, data=heart, horizontal=T, col=c("yellow"), 
        main='Boxplots of BMI Score by Heart Disease Diagnosis')

heart$CDC <- ifelse(heart$BMI<=18.50, "Underweight",
                    ifelse(18.51<=heart$BMI & heart$BMI<=24.99, "Healthy",
                           ifelse(25.00<=heart$BMI & heart$BMI<=29.99, "Overweight",
                                  ifelse(heart$BMI>=30.00, "Obese", NA))))

#All heart data BMI
ggplot(heart, aes(x = CDC, fill = HeartDisease)) + geom_bar() + ggtitle("CDC BMI Classification by Heart Disease") 

#A huge majority of the data is people who do not have heart disease. However, we're interested in those who do.
#Splitting the data and only doing further EDA on heart disease positive people
#Of those that have heart disease:
dfHD <- heart[which(heart$HeartDisease == "Yes"),]
dfNOHD <- heart[which(heart$HeartDisease == "No"),]


#Heart disease positive only BMI
ggplot(dfHD, aes(x = CDC)) + geom_bar(fill="lightblue") + ggtitle("CDC BMI Classification Among People with Heart Disease")

#Heart disease negative only BMI
ggplot(dfNOHD, aes(x = CDC)) + geom_bar(fill="purple") + ggtitle("CDC BMI Classification Among People without Heart Disease")


#Boxplot of sleep by heart disease - outliers check
boxplot(SleepTime ~ HeartDisease, data=heart, horizontal=T, col=c("orange"), 
        main='Boxplots of Sleep by Heart Disease Diagnosis')

#Boxplot of physical health by heart disease - outliers check
boxplot(PhysicalHealth ~ HeartDisease, data=heart, horizontal=T, col=c("deeppink"), 
        main='Boxplots of Physical Health Score by Heart Disease Diagnosis')

#Boxplot of mental health by heart disease - outliers check
boxplot(MentalHealth ~ HeartDisease, data=heart, horizontal=T, col=c("lightblue"), 
        main='Boxplots of Mental Health Score by Heart Disease Diagnosis')

#Correlation matrix on numeric data only
num_df <- data.frame(heart$BMI, heart$PhysicalHealth, heart$MentalHealth, heart$SleepTime)
M <- cor(num_df)
corrplot(M, method="circle")

#Finding how many correlations are bigger than 0.70
k = 0
for(i in 1:4) {
  for(r in 1:4){
    if(M[i,r]> 0.70 & i != r){
      k= k + 1
    }
  }  }
print(k/2)

#Bar graph showing class distribution of heart disease column
barplot(prop.table(table(heart$HeartDisease)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#Save a table of heart disease and smoking
HeartSmoke <- table(heart$HeartDisease,
                    heart$Smoking)

#Barplot for smoking for people who have heart disease
barplot(HeartSmoke,
        beside = TRUE,
        main = "Smoking Distribution by Heart Disease Group",
        xlab = "Smoker",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 200000),
        args.legend=list(title="Heart Disease"),
        col = c("deeppink", "orange"))

# Save a table of heart disease and alcohol
HeartAlc <- table(heart$HeartDisease,
                  heart$AlcoholDrinking)

#Barplot of alcohol consumption for people who have heart disease
barplot(HeartAlc,
        beside = TRUE,
        main = "Alcohol Consumption by Heart Disease Group",
        xlab = "Alcohol Drinker",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("green", "yellow"))

# Save a table of age for people who have heart disease
HeartAge <- table(heart$HeartDisease,
                  heart$AgeCategory)

#Barplot for age vs heart disease
barplot(HeartAge,
        beside = TRUE,
        main = "Age Distribution by Heart Disease Group",
        xlab = "Age",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 31000),
        args.legend=list(title="Heart Disease", x = "topright",
                         inset = c(0.005,0), cex = 0.7),
        col = c("lightblue", "purple"))

# Save a table of walking and heart disease
HeartWalk <- table(heart$HeartDisease,
                   heart$DiffWalking)

#Barplot for walking for people who have heart disease
barplot(HeartWalk,
        beside = TRUE,
        main = "Difficulty Walking by Heart Disease Group",
        xlab = "Difficulty Walking",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("yellow", "blue"))

# Save a table of race and heart disease
HeartRace <- table(heart$HeartDisease,
                   heart$Race)

#Barplot for race vs heart disease
barplot(HeartRace,
        beside = TRUE,
        main = "Race Distribution by Heart Disease Group",
        xlab = "Race",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 250000),
        args.legend=list(title="Heart Disease", x = "topleft",
                         inset = c(0.15, 0)),
        col = c("pink", "red"))

# Save a table of heart disease and diabetes
HeartDia <- table(heart$HeartDisease, heart$Diabetic)

#Barplot for heart disease and diabetes
barplot(HeartDia,
        beside = TRUE,
        main = "Diabetes Distribution by Heart Disease Group",
        xlab = "Diabetes",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 260000),
        args.legend=list(title="Heart Disease"),
        col = c("orange", "lightgreen"))

# Save a table of heart disease and sex
Heartsex <- table(heart$HeartDisease, heart$Sex)

#Barplot for heart disease and diabetes
barplot(Heartsex,
        beside = TRUE,
        main = "Gender Distribution by Heart Disease Group",
        xlab = "Sex",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 170000),
        args.legend=list(title="Heart Disease", x = "topright",
                         inset = c(0.06,0), cex = 0.7),
        col = c("deeppink", "blue"))

# Save a table of heart disease and general health
HeartGE <- table(heart$HeartDisease, heart$GenHealth)
HeartGEdf <- as.data.frame(HeartGE)
HeartGEyes <- subset(HeartGEdf, Var1=="Yes")
names(HeartGEyes)[names(HeartGEyes) == "Freq"] <- "Yes"
HeartGEno <- subset(HeartGEdf, Var1=="No")
names(HeartGEno)[names(HeartGEno) == "Freq"] <- "No"
HeartGEcombined <- merge(HeartGEyes, HeartGEno, by.x="Var2", by.y="Var2")
HeartGEcombined <- subset(HeartGEcombined, select = c("Var2","Yes","No"))
HeartGEcombined$Yes <- as.numeric(HeartGEcombined$Yes)
HeartGEcombined$No <- as.numeric(HeartGEcombined$No)
HeartGEcombined$Total <- HeartGEcombined$Yes + HeartGEcombined$No
HeartGEcombined$YesPerc <- HeartGEcombined$Yes / HeartGEcombined$Total
HeartGEcombined$Var2 <- factor(HeartGEcombined$Var2,levels = c("Poor","Fair","Good","Very good","Excellent"))
barplot(HeartGEcombined$YesPerc~HeartGEcombined$Var2,
        main="Heart Disease Percentage by General Health",
        xlab="General Health",
        ylab="Percentage",
        ylim=c(0,0.35),
        col = "green")

#Barplot for heart disease and diabetes
barplot(HeartGE,
        beside = TRUE,
        main = "General Health Distribution by Heart Disease Group",
        xlab = "General Health",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 120000),
        args.legend=list(title="Heart Disease", x = "topright", inset = c(0.20,0), cex = 0.7),
        col = c("lightblue", "green"))

# Save a table of heart disease and stroke
Heartst <- table(heart$HeartDisease, heart$Stroke)

#Barplot for heart disease and stroke
barplot(Heartst,
        beside = TRUE,
        main = "Stroke Distribution by Heart Disease Group",
        xlab = "Stroke",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("orange", "yellow"))

# Save a table of heart disease and skin cancer
Heartskin <- table(heart$HeartDisease, heart$SkinCancer)

#Barplot for heart disease and skin cancer
barplot(Heartskin,
        beside = TRUE,
        main = "Skin Cancer Distribution by Heart Disease Group",
        xlab = "Skin Cancer",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("blue", "red"))

# Save a table of heart disease and kidney disease
Heartkid <- table(heart$HeartDisease, heart$KidneyDisease)

#Barplot for heart disease and kidney disease
barplot(Heartkid,
        beside = TRUE,
        main = "Kidney Disease Distribution by Heart Disease Group",
        xlab = "Kidney Disease",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("lightpink", "yellow"))

# Save a table of heart disease and asthma
Heartast <- table(heart$HeartDisease, heart$Asthma)

#Barplot for heart disease and asthma
barplot(Heartast,
        beside = TRUE,
        main = "Asthma Distribution by Heart Disease Group",
        xlab = "Asthma",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 270000),
        args.legend=list(title="Heart Disease"),
        col = c("red", "yellow"))

# Save a table of heart disease and physical activity
Heartpa <- table(heart$HeartDisease, heart$PhysicalActivity)

#Barplot for heart disease and physical activity
barplot(Heartpa,
        beside = TRUE,
        main = "Physical Activity Distribution by Heart Disease Group",
        xlab = "Physical Activity",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 250000),
        args.legend=list(title="Heart Disease", x = "topright",
                         inset = c(0.01,0), cex = 0.7),
        col = c("purple", "green"))


#2-Way Cross Tabulation for balloon plot
df <- data.frame(CrossTable(heart$AgeCategory, heart$Race)) 
new_df <- df[,c("t.x","t.y","t.Freq")]
heartdat <- pivot_wider(new_df, names_from = t.x, values_from = t.Freq)

#Balloon plot
ggballoonplot(
  df, x = "t.x", y = "t.y",
  size = "t.Freq", fill = "t.Freq",
  ggtheme = theme_bw()
)


#Check for missing data
sum(is.na(heart)) 

#BMI
gghistogram(heart, x = "BMI", bins=100,
            add = "median", 
            color = "HeartDisease", fill = "HeartDisease",
            palette = c("#0073C2FF", "#FC4E07"))

#sleep inspection
hist(heart$SleepTime, 
     xlab="Sleep Time (Hours)",
     xlim=c(0,24),
     col="blue",
     freq=TRUE
)

#Convert all string columns to factors
heart <- as.data.frame(unclass(heart),            
                       stringsAsFactors = TRUE)

#log transformation to deal with outliers
heart$PhysicalHealth <- heart$PhysicalHealth+1
heart$MentalHealth <- heart$MentalHealth+1
heart$BMI <- log(heart$BMI)
heart$PhysicalHealth <- log(heart$PhysicalHealth)
heart$MentalHealth <- log(heart$MentalHealth)
heart$SleepTime <- log(heart$SleepTime)

#Convert GenHealth and AgeCategory to ordinal data since ratings and age are ordinal
heart$GenHealth <- as.character(heart$GenHealth)
heart$GenHealth <- factor(heart$GenHealth, ordered = TRUE, 
                          levels = c("Poor", "Fair", "Good","Very good", "Excellent"))

heart$GenHealth  <- unclass(heart$GenHealth)#changes ordinal into numerical so that in can be used in elastic net

heart$AgeCategory <- as.character(heart$AgeCategory)
heart$AgeCategory <- factor(heart$AgeCategory, ordered = TRUE, 
                            levels = c("18-24", "25-29", "30-34","35-39", "40-44",
                                       "45-49", "50-54", "55-59", "60-64", "65-69",
                                       "70-74", "75-79", "80 or older"))

heart$AgeCategory <- unclass(heart$AgeCategory) #changes ordinal into numerical so that in can be used in elastic net

# one-hot encoding
#define variables to keep in two separate dataframes since we're only hot encoding variables in df_hotencode
df <- c("HeartDisease","BMI","PhysicalHealth","MentalHealth","SleepTime","AgeCategory",
        "PhysicalActivity","KidneyDisease","Asthma","GenHealth","Sex","DiffWalking","Stroke",
        "AlcoholDrinking","Smoking", "SkinCancer")

df_hotencode <- c("Race","Diabetic")

data<- subset(heart,select=df)
data_hotencode <-subset(heart,select=df_hotencode)

#define one-hot encoding
dummy <- dummyVars(" ~ .", data=data_hotencode)

#perform one-hot encoding
data_hotencode2 <- data.frame(predict(dummy, newdata=data_hotencode))

#merge dataset of one-hot encoded data with the rest of the dataset
newdf <- cbind(data,data_hotencode2)
heart <- newdf

#split train and test set using stratified sampling to maintain same class imbalance 
rows <- sample(nrow(heart))
dat <- heart[rows, ]
train_index <- createDataPartition(heart$HeartDisease, p = .8, list = FALSE)
training <- dat[train_index, ]  
test <- dat[-train_index, ]

#Check for class imbalance for y-variable in the training set
table(training$HeartDisease) 

#SMOTE
training_old8 <- training # a copy for comparison

training_smote <- SMOTE(HeartDisease ~ .  , data  = training, dup_size = 0, perc.over=150, K = 5)
table(training_smote$HeartDisease) 
training <- training_smote

#in the modeling stage, the training set is reduce so need original for each iteration of model
training_original <- training 
test_original <- test

barplot(prop.table(table(training$HeartDisease)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

## Comparing models
# using lasso and 1o-fold cross validation

#convert to x and y
y <- training$HeartDisease 
x <- model.matrix(HeartDisease~., training)[,-1] #convert to matrix and make dummy
test2 <- as.data.frame(model.matrix(HeartDisease~., test)[,-1]) #convert to matrix and make dummy
test2 <- cbind(test$HeartDisease,test2)
names(test2)[names(test2) == "test$HeartDisease"] <- "HeartDisease"

# Fit full logistic model
full.model <- glm(HeartDisease ~., data=training, family=binomial)
# Make predictions
probabilities <- full.model %>% predict(test,type="response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
# Model Accuracy
observed.classes <- test$HeartDisease
mean(predicted.classes == observed.classes)
predicted <- as.factor(predicted.classes)
#Calculate AIC, BIC values
BICAICglm=function(fit){
  #tLL <- fit$null.deviance - deviance(fit)  
  tLL <- -deviance(fit) # 2*log-likelihood
  k <- dim(model.matrix(fit))[2]
  n <- nobs(fit)
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  AIC_ <- -tLL+2*k
  
  BIC<-log(n)*k - tLL
  res=c(AIC_, BIC, AICc)
  names(res)=c("AIC", "BIC", "AICc")
  return(res)
}
BICAICglm(full.model)

# Find the best lambda using cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se)


# Fit the model on the training data with lambda.min
lasso.model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Make predictions on the test data
x.test <- model.matrix(HeartDisease ~., test2)[,-1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
# Model accuracy rate
observed.classes <- test2$HeartDisease
mean(predicted.classes == observed.classes)
predicted <- as.factor(predicted.classes)
#Calculate AIC, BIC values
tLL <- lasso.model$nulldev - deviance(lasso.model)
k <- lasso.model$df
n <- lasso.model$nobs
AIC <- -tLL+2*k
AIC

BIC<-log(n)*k - tLL
BIC


# Fit the model on the training data with lambda.1se
lasso.model.2 <- glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.1se)
# Make predictions on the test data
x.test <- model.matrix(HeartDisease ~., test2)[,-1]
probabilities <- lasso.model.2 %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
# Model accuracy rate
observed.classes <- test2$HeartDisease
mean(predicted.classes == observed.classes)
predicted <- as.factor(predicted.classes)
#Calculate AIC, BIC values
tLL <- lasso.model.2$nulldev - deviance(lasso.model.2)
k <- lasso.model.2$df
n <- lasso.model.2$nobs
AIC <- -tLL+2*k
AIC

BIC<-log(n)*k - tLL
BIC

#Diagnostic plots
plot.model <- glm(HeartDisease ~ AgeCategory + AlcoholDrinking + Asthma + BMI
                  + Diabetic.No + Diabetic.Yes + DiffWalking + GenHealth + KidneyDisease + PhysicalActivity
                  + PhysicalHealth + Race.Asian + Race.Black + Sex + SkinCancer + SleepTime + Smoking + Stroke,
                  data=training, family=binomial)
par(mfrow=c(2,2))
plot(plot.model)
vif(plot.model)

#Confusion matrix
cm <- confusionMatrix(test$HeartDisease, predicted)
cm

#AUC and ROC
roc_model=roc(response=test$HeartDisease, predictor= factor(predicted,
                                                            ordered = TRUE), plot=TRUE,quiet = TRUE)
auc_model<-auc(roc_model)
auc_model

# create a function to transform coefficient of glmnet and cvglmnet to data.frame
coeff2dt <- function(fitobject, s) {
  coeffs <- coef(fitobject, s) 
  coeffs.dt <- data.frame(name = coeffs@Dimnames[[1]][coeffs@i + 1], coefficient = coeffs@x) 
  
  # reorder the variables in term of coefficients
  return(coeffs.dt[order(coeffs.dt$coefficient, decreasing = T),])
}

#Visualize Coefficient for lambda.min
coeff2dt(fitobject = lasso.model, s = "lambda.min") %>% head(20)
coeffs.table <- coeff2dt(fitobject = lasso.model, s = "lambda.min")
ggplot(data = coeffs.table) +
  geom_col(aes(x = name, y = coefficient, fill = {coefficient > 0})) +
  xlab(label = "") +
  ggtitle(expression(paste("Lasso Coefficients with optimal ", lambda))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") 


#Visualize Coefficient for lambda.1se
coeff2dt(fitobject = lasso.model.2, s = "lambda.1se") %>% head(20)
coeffs.table <- coeff2dt(fitobject = lasso.model.2, s = "lambda.1se")
ggplot(data = coeffs.table) +
  geom_col(aes(x = name, y = coefficient, fill = {coefficient > 0})) +
  xlab(label = "") +
  ggtitle(expression(paste("Lasso Coefficients with ", lambda, " that lies within 1 SE of the optimal value of ", lambda))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") 

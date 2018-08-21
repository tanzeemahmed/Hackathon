###########################Clear Environment Variable###############################

rm(list = ls(all = T))

###########################Load the required libraries#############################

library(DMwR)
library(infotheo)
library(vegan)
library(randomForest)
library(caret)
library(ROCR)
library(e1071)
library(corrplot)
library(car)
library(standardize)
library(tidyverse)
library(gridExtra)
library(ggridges)
library(GGally)
library(ggplot2)
library(glmnet)
library(rpart)
library(rpart.plot)
library(RANN)
library(zoo)
library(xgboost)
library(dplyr)


#####################Set and get the Working directory##############################

setwd("C:/Users/Yunus Saleem/Desktop/Insofe/MiTH")

getwd()

###############################Read the train data#################################


master_data <- read.csv("dataset.csv",header = TRUE)


#########Store the ID and target in separate variables and remove ID from the dataset

ExtraTime <- master_data$target

RowID <- master_data$ID


#########################Check the structure of the data###########################

str(master_data)

# No of Numeric Variables initially: 23 Numeric Variables
 
# No of Categorical Variables initially: 10 Categorical Variables

#########################Check the Summary of the data#############################

summary(master_data)

#Any outliers - Some values are higher than Average
#NAs - No NAs observed
#Special Symbols - No special characters observed

#########################Check the Head and Tail of the data#######################

head(master_data)

tail(master_data)

###################Check for the various levels in case of Categorical variables(Separating categorical variables)#################################################

cat_attr <- c("Joblevel","FrequencyofTravel","Gender","OfficeAmbienceRating","SelfMotivationRating","Division","JobRole","RelationshipSatisfaction","WorkLifeBalance","Happynesslevelinjob","Specialization","PerformanceRating","MaritalStatus")


lapply(master_data[cat_attr], table)

############################Data Pre-Processing####################################

#------Set the variables which have no variation to Null
master_data$RowID <- NULL

master_data$istrain <- NULL

master_data$datacollected <- NULL

master_data$StandardHours <- NULL

master_data$Over18 <- NULL

master_data$EmployeeCount <- NULL

master_data$Division <- NULL

#------Convert variables to appropriate types

#Convert First Job Date and Date of Joining Current Company to Date

master_data$FirstJobDate <- as.Date(master_dataset$FirstJobDate,"%m/%d/%Y")

master_data$DateOfjoiningintheCurrentCompany <- as.Date(master_dataset$DateOfjoiningintheCurrentCompany,"%m/%d/%Y")

#Convert these variables to Factors

master_data$Joblevel <- as.factor(master_data$Joblevel)
master_data$OfficeAmbienceRating <- as.factor(master_data$OfficeAmbienceRating)
master_data$SelfMotivationRating <- as.factor(master_data$SelfMotivationRating)
master_data$RelationshipSatisfaction <- as.factor(master_data$RelationshipSatisfaction)
master_data$WorkLifeBalance <- as.factor(master_data$WorkLifeBalance)
master_data$Happynesslevelinjob <- as.factor(master_data$Happynesslevelinjob)
master_data$PerformanceRating <- as.factor(master_data$PerformanceRating)


#Convert Employee ID to factor later and check

str(master_data$EmployeeID)


#----------Combine levels into another level and change wrong level names
data_level_changed <- master_data

data_level_changed <- gsub("%", "", as.matrix(data_level_changed)) #if any worng level

levels(data_level_changed$placeholder)[8] <- 'ph'
data_level_changed$abcd <- gsub('national', 'Government', data_level_changed$abcd)

train_data$working_sector <- gsub('local_body', 'Government', train_data$working_sector)
train_data$working_sector <- gsub('state', 'Government', train_data$working_sector)


#----------Check for correlation of Numeric Attributes

num_attr <- c("DistancetoHome","MonthlyRate","YearsSinceLastPromotion","Hourlyrate","Education","No.ofWorkshopsAttended","Age","YearsInCurrentRole","NumberofCompaniesChanged","Emolumnet_in_Pecentage","DialyRate","EmployeeID","ESOps","MonthlyIncome")

res <- cor(master_data[num_attr], use = "complete.obs")

corrplot(res,  
         tl.col = "black",number.cex = 0.6, cl.cex = 0.7,tl.cex = 0.7,method = "number")

#Years in Current Role and Years Since Last Promotion are slightly colinear. 
#Rest are pretty independent

#----------Remove Colinear Attributes

#Check VIF

log_trial <- glm(ExtraTime~.,data = master_data,family = binomial)

summary(log_trial)

sort(vif(log_trial),decreasing = TRUE)

ExtraTime <- train_data$ExtraTime

train_data$ExtraTime <- NULL

#Separate Numeric attributes with categorical

num_attr <- c("DistancetoHome","MonthlyRate","YearsSinceLastPromotion","Hourlyrate","Education","No.ofWorkshopsAttended","Age","YearsInCurrentRole","NumberofCompaniesChanged","Emolumnet_in_Pecentage","DialyRate","EmployeeID","ESOps","MonthlyIncome")

#Check Corelation of each Numeric attribute with others

num_data <- train_data[num_attr]

cat_data <- train_data[cat_attr]

cor(x = num_data,y = num_data$MonthlyIncome,method = "pearson")

train_data <- data.frame(train_data,ExtraTime)

#Check Variable Importance on Random Forest

model_trial = randomForest(target ~ ., data=train_data, 
                           keep.forest=TRUE, ntree=100)
rf_Imp_Attr = data.frame(model_trial$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

#Remove Variables that have least Importance and high VIF

#Check the corelation between Categorical Variables cant be missing values

chi2 <- chisq.test(table(train_data$JobRole,train_data$Joblevel))

chi2

chi3 <- chisq.test(table(train_data$JobRole,train_data$Division))

chi3

#Remove based on p-value and X2 statistic

###########################Data Visualization using ggplot#########################
#Check Random Forest Importance and Intuition to select variables

#-------Box Plot - Continuos vs Categorical( Target(Cat) vs Continuos)

master_data %>% 
  ggplot( aes(x = factor(ExtraTime), y = MonthlyIncome,color= ExtraTime)) +
  geom_boxplot()

#People who are working extratime are generally making more money than average

master_data %>% 
  ggplot( aes(x = factor(ExtraTime), y = YearsInCurrentRole,color= ExtraTime)) +
  geom_boxplot()
#People who are new to the role are generally working oveertime less maybe because of lack of experience


master_data %>% 
  ggplot( aes(x = factor(ExtraTime), y = No.ofWorkshopsAttended,color= ExtraTime)) +
  geom_boxplot()

#There is no significant variance- Random Forest Importance is also low

master_data %>% 
  ggplot( aes(x = factor(ExtraTime), y = FirstJobDate,color= ExtraTime)) +
  geom_boxplot()
#People who have joined the industry recently are working overtime a little more


master_data %>% 
  ggplot( aes(x = factor(ExtraTime), y = DateOfjoiningintheCurrentCompany,color= ExtraTime)) +
  geom_boxplot()
##People who have joined the company  recently are working overtime a little more



#-----------Scatter Plot - Continuous vs Continuous

master_data %>% ggplot(aes(x = Age, y = YearsInCurrentRole,color= ExtraTime,fill= ExtraTime)) +
  geom_point(alpha = .3)

master_data %>% ggplot(aes(x = YearsInCurrentRole, y = MonthlyIncome,color= ExtraTime,fill= ExtraTime)) +
  geom_point(alpha = .3)
#Monthly Income is less and Years of Experiece is medium work overtime more- Can derive a new column maybe

master_data %>% ggplot(aes(x = FirstJobDate, y = MonthlyIncome,color= ExtraTime,fill= ExtraTime)) +
  geom_point(alpha = .3)
#People who have joined the industry late and have low income work overtime more

master_data %>% ggplot(aes(x = months, y = ESOps,color= ExtraTime,fill= ExtraTime)) +
  geom_point(alpha = .3)

#People with an experience between 40 months and 90 months and with E-Sops are generally working overtime

#-------Distribution of Categorical variables based on target and their levels

qplot (ExtraTime, data = master_data, fill = Joblevel) + facet_grid (. ~ Joblevel)
qplot (ExtraTime, data = master_data, fill = FrequencyofTravel) + facet_grid (. ~ FrequencyofTravel)
qplot (ExtraTime, data = master_data, fill = Gender) + facet_grid (. ~ Gender)
qplot (ExtraTime, data = master_data, fill = OfficeAmbienceRating
) + facet_grid (. ~ OfficeAmbienceRating)
qplot (ExtraTime, data = master_data, fill = SelfMotivationRating
) + facet_grid (. ~ SelfMotivationRating)
qplot (ExtraTime, data = master_data, fill = Division
) + facet_grid (. ~ Division)
qplot (ExtraTime, data = master_data, fill = JobRole
) + facet_grid (. ~ JobRole)
qplot (ExtraTime, data = master_data, fill = RelationshipSatisfaction) + facet_grid (. ~ RelationshipSatisfaction)
qplot (ExtraTime, data = master_data, fill = WorkLifeBalance
) + facet_grid (. ~ WorkLifeBalance)
qplot (ExtraTime, data = master_data, fill = Happynesslevelinjob
) + facet_grid (. ~ Happynesslevelinjob)
qplot (ExtraTime, data = master_data, fill = Specialization
) + facet_grid (. ~ Specialization)
qplot (ExtraTime, data = master_data, fill = PerformanceRating
) + facet_grid (. ~ PerformanceRating)
qplot (ExtraTime, data = master_data, fill = MaritalStatus
) + facet_grid (. ~ MaritalStatus)



#############################Feature Engineering###################################

#Derive a new column indicating the total number of Months worked
date <- as.Date(as.factor(c("3/20/2014")),"%m/%d/%Y")

months <- round((date- master_data$FirstJobDate 
)/(365.25/12))

master_data$months <- months

#Derive a column indicating -Monthly Income less than 10000 and NO. of months between 98 and 132 or 0 and 48

condition <- ifelse(((master_data$months>98 & master_data$months<132)| (master_data$months >0 & master_data$months<48)),"1","0")

attr <- ifelse((master_data$MonthlyIncome<10000 & condition == "1"),"1","0" )

master_data$new_attr <- attr

#If Female and Married

master_data$new_attr2 <- ifelse((master_data$Gender == "Female" & master_data$MaritalStatus == "Married"),"1","0")

#If Job role is Healthcare Representative, Manufacturing Director, Research Director and if income is greater than 10000

master_data$new_attr3 <- ifelse((master_data$JobRole %in% c("Healthcare Representative","Research Director","Manufacturing Director") & master_data$MonthlyIncome >10000),"1","0")

#If E-Sops greater than 0 , and no of months is less than 90 and greater than 40

master_data$new_attr4 <- ifelse((master_data$ESOps > 0 & (master_data$months < 90 & master_data$months >40)),"1","0")


##########################Split into Train and Validation##########################

set.seed(786)

train_RowIDs =  createDataPartition(master_data$ExtraTime, p = 0.7, list = F)
train_data = master_data[train_RowIDs,]
val_data = master_data[-train_RowIDs,]


# Check how records are split with respect to target attribute 

table(master_data$ExtraTime)
table(train_data$ExtraTime)
table(val_data$ExtraTime)


##########Standardize the Train and Validation Data-Continuous Variables###########

std_obj <- preProcess(x = train_data[, !colnames(train_data) %in% c("target")],
                      method = c("center", "scale"))

train_std_data <- predict(std_obj, train_data)

val_std_data <- predict(std_obj, val_data)

#####################Dummify the Categorical Variables############################

dummy_obj <- dummyVars( ~ . , train_std_data)

train_dummy_data <- as.data.frame(predict(dummy_obj, train_std_data))

val_dummy_data <- as.data.frame(predict(dummy_obj, val_std_data))


#########################Read and Pre-Process the Test Data########################

master_test <- read.csv("test.csv",header = TRUE)

#--------Read the ID variable into another variable and remove it from the test data. Change the name according to the submission file

index <- master_test$RowID

master_test$RowID <- NULL

master_test$istrain <- NULL

master_test$datacollected <- NULL

master_test$StandardHours <- NULL

master_test$Over18 <- NULL

master_test$EmployeeCount <- NULL


#--------Structure,Summary,Head and Tail of test data

str(master_test)

summary(master_test)

head(master_test)

tail(master_test)

#--------Convert variables to appropriate types
#Convert to Date

master_test$FirstJobDate <- as.Date(master_test_ref$FirstJobDate,"%m/%d/%Y")

master_test$DateOfjoiningintheCurrentCompany <- as.Date(master_test_ref$DateOfjoiningintheCurrentCompany,"%m/%d/%Y")


#Convert these variables to Factors

master_test$Joblevel <- as.factor(master_test$Joblevel)
master_test$OfficeAmbienceRating <- as.factor(master_test$OfficeAmbienceRating)
master_test$SelfMotivationRating <- as.factor(master_test$SelfMotivationRating)
master_test$RelationshipSatisfaction <- as.factor(master_test$RelationshipSatisfaction)
master_test$WorkLifeBalance <- as.factor(master_test$WorkLifeBalance)
master_test$Happynesslevelinjob <- as.factor(master_test$Happynesslevelinjob)
master_test$PerformanceRating <- as.factor(master_test$PerformanceRating)

str(master_test)

#--------Check for the various levels in case of Categorical variables

table(master_test)


#Add Feature Engineered Variables

#Add months variable
date <- as.Date(as.factor(c("3/20/2014")),"%m/%d/%Y")

months_test <- round((date- master_test$FirstJobDate 
)/(365.25/12))

master_test$months <- months_test

#Monthly Income less than 10000 and NO. of months between 98 and 132  or 0 and 48

condition_test <- ifelse(((master_test$months>98 & master_test$months<132)| (master_test$months >0 & master_test$months<48)),"1","0")

attr_test <- ifelse((master_test$MonthlyIncome<10000 & condition_test == "1"),"1","0" )

master_test$new_attr <- attr_test


#If Female and Married

master_test$new_attr2 <- ifelse((master_test$Gender == "Female" & master_test$MaritalStatus == "Married"),"1","0")

#If Job role is Healthcare Representative, Manufacturing Director, Research Director and if income is greater than 10000

master_test$new_attr3 <- ifelse((master_test$JobRole %in% c("Healthcare Representative","Research Director","Manufacturing Director") & master_test$MonthlyIncome >10000),"1","0")

#If E-Sops greater than 0 , and no of months is less than 100

master_test$new_attr4 <- ifelse((master_test$ESOps > 0 & (master_test$months < 90 & master_test$months >40)),"1","0")


#Standardize test data-contnuous variables

test_std_data <- predict(std_obj, master_test)

#Dummify the categorical Variables

test_dummy_data <- as.data.frame(predict(dummy_obj, test_std_data))

#Assign the final dataset to test_data

test_data <- master_test

index <- test_data$ID  
  
##########################Build a Logistic Regression Model########################


#Build the model

log_reg = glm(formula = ExtraTime ~., data = train_data, family="binomial")

#Summary of the model

summary(log_reg)

#Make the predictions on train data

prob_train <- predict(log_reg,train_data[, !(names(train_data) %in% c("ExtraTime"))],type="response")

#Create a Prediction object 

pred <- prediction(prob_train,train_data$ExtraTime)

#Create a performance object to draw the ROC Curve

perf <- performance(pred,measure = "tpr",x.measure = "fpr")

#Plot the ROC Curve

plot(perf,col=rainbow(10),colorize=T,print.cutoffs.at=seq(0,1,0.05))

#Check the AUC score

# Access the auc score from the performance object

perf_auc <- performance(pred,measure = "auc")

perf_auc@y.values[[1]]

#Choose a Cutoff Value and predict on Validation data

prob_val <- predict(log_reg,val_data[, !(names(val_data) %in% c("ExtraTime"))],type = "response")

pred_val <-ifelse(prob_val>0.5,"Yes","No")

#Build a confusion Matrix

confusionMatrix(pred_val, val_data$ExtraTime,positive = "Yes")


#----Choose a cutoff value and predict on test data

prob_test <- predict(log_reg,test_data,type = "response")

pred_test <-ifelse(prob_test>0.5,"Yes","No")

#Write the predictions to the submission file

final <-cbind(index,test_data,pred_test)

names(final)[c(1,31)] = c("RowID","ExtraTime")

submission <- final[,names(final) %in% c("RowID","ExtraTime") ]

write.csv(submission, "C:/Users/Yunus Saleem/Desktop/Insofe/MiTH/Submission-2ndattLog.csv")


###########################Build a Decision Tree for Classification################


#Build the Model
rpart_tree <- rpart(ExtraTime ~ . , data = train_data, method="class")

#Check Variable Importance
rpart_tree$variable.importance

printcp(rpart_tree)

#Plot the Decision Tree
plotcp(rpart_tree)

#Decision Tree

library(rpart.plot)

rpart.plot(rpart_tree)

#Variable Importance

rpart_tree$variable.importance


#Make the Predictions on Validation Data

pred_val <- predict(rpart_tree, val_data[, !(names(val_data) %in% c("ExtraTime"))], type="class")


#Check Accuracy, Recall and Precision

confusionMatrix(pred_val, val_data$ExtraTime,positive = "Yes")

#Predict on Test data

pred_test <- predict(rpart_tree, test_data, type="class")

#Write the predictions to the submission file

final <-cbind(index,test_data,pred_test)

names(final)[c(1,31)] = c("RowID","ExtraTime")

submission <- final[,names(final) %in% c("RowID","ExtraTime") ]

write.csv(submission, "C:/Users/Yunus Saleem/Desktop/Insofe/MiTH/Submission-3rdattemptDec.csv")

#################Build a Decision Tree using C5.0#################################

#Build the Model

library(C50)

c5_tree <- C5.0(ExtraTime ~ . , train_data,rules = T)

#Variable Importance in Trees

C5imp(c5_tree, metric = "usage")

#Summary of the model

summary(c5_tree)
#Error in Model Building stage

##########################Build Random Forest Model################################

model = randomForest(ExtraTime ~ ., data=train_data, 
                     keep.forest=TRUE, ntree=100) 

# Print the model
print(model)

#Find the Importance of Variables and order them

rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

#Plot the Important Variables

varImpPlot(model)

#Predict on Validation Data

pred_val = predict(model, val_data[, !(names(val_data) %in% c("ExtraTime"))],
                     type="response", 
                     norm.votes=TRUE)

#Check Accuracy, Recall and Precision

confusionMatrix(pred_val, val_data$ExtraTime,positive = "Yes")

#Predict on Test Data

test_data <- master_test

pred_test = predict(model, test_data,
                    type="response", 
                    norm.votes=TRUE)

#Write the predictions to the submission file

final <-cbind(index,test_data,pred_test)

names(final)[c(1,36)] = c("RowID","ExtraTime")

submission <- final[,names(final) %in% c("RowID","ExtraTime") ]

write.csv(submission, "C:/Users/Yunus Saleem/Desktop/Insofe/MiTH/Submission-RF-75-5newvar.csv")

#Find the best mtry and apply Random Forest

mtry <- tuneRF(train_data[!names(train_data) %in% c("ExtraTime")],train_data$ExtraTime, ntreeTry=100,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)



###########Build XGBoost with parameter tuning for CLassification##################

# Convert data into an object of the class "xgb.Dmatrix"


dummy_obj <- dummyVars( ~ . , train_data)

train_dummy_data <- as.data.frame(predict(dummy_obj, train_data))

val_dummy_data <- as.data.frame(predict(dummy_obj, val_data))


train_matrix <- xgb.DMatrix(data = as.matrix(train_dummy_data[, !(names(train_dummy_data) %in% c("ExtraTime"))]), 
                            label = as.matrix(train_dummy_data[, names(train_dummy_data) %in% "ExtraTime"]))

val_matrix <- xgb.DMatrix(data = as.matrix(val_data[, !(names(val_data) %in% c("ExtraTime"))]), 
                           label = as.matrix(val_data[, names(val_data) %in% "ExtraTime"]))

##Define parameter list
params_list <- list("objective" = "binary:logistic",
                    "eta" = 0.1,
                    "early_stopping_rounds" = 10,
                    "max_depth" = 6,
                    "gamma" = 0.5,
                    "colsample_bytree" = 0.6,
                    "subsample" = 0.65,
                    "eval_metric" = "auc",
                    "silent" = 1)

#Build the model

xgb_model_with_params <- xgboost(data = train_matrix, params = params_list, nrounds = 500, early_stopping_rounds = 20)

#Predict on validation data

prob_val <- predict(xgb_model_with_params, val_matrix,type="response", norm.votes=TRUE) 

#Plot ROC and AUC 

pred <- prediction(prob_val,val_data$target)
perf <- performance(pred, measure = "tpr", x.measure ="fpr")

plot(perf,col = rainbow(10),colorize = T,print.cutoffs.at= seq(0,1,0.2))
auc <- performance(pred,measure = "auc")
auc@y.values[[1]]
pred_val <- ifelse(prob_val > 0.2 , 1,0)

#Confusion Matrix and Error Metrics

confusionMatrix(pred_val, val_data$target)

#Choose the cutoff point from above and Predict on Test data

prob_test <- predict(xgb_model_with_params, as.matrix(test_data),type="response", norm.votes=TRUE) 

#Write the predictions to the submission file

final <-cbind(index,test_data,pred_test)

names(final)[c(1,33)] = c("RowID","ExtraTime")

submission <- final[,names(final) %in% c("RowID","ExtraTime") ]

write.csv(submission, "C:/Users/Yunus Saleem/Desktop/Insofe/MiTH/Submission-XGoost-Tuned.csv")

###########Build XGBoost with Caret for CLassification#############################

#Make a sampling strategy and build the model

sampling_strategy <- trainControl(method = "repeatedcv", number = 5, repeats = 2, verboseIter = F, allowParallel = T)

param_grid <- expand.grid(.nrounds = 20, .max_depth = c(2, 4, 6), .eta = c(0.1, 0.3),
                          .gamma = c(0.6, 0.5, 0.3), .colsample_bytree = c(0.6, 0.4),
                          .min_child_weight = 1, .subsample = c(0.5, 0.6, 0.9))

xgb_tuned_model <- train(x = train_data[ , !(names(train_data) %in% c("ExtraTime", "class"))], 
                         y = train_data[ , names(train_data) %in% c("ExtraTime")], 
                         method = "xgbTree",
                         trControl = sampling_strategy,
                         tuneGrid = param_grid)

#Check the best tuning parameters

xgb_tuned_model$bestTune

#Plot the XGB tuned model

plot(xgb_tuned_model)

# Predict on Validation data

pred_val <- predict(xgb_tuned_model, val_data[ , !(names(val_data) %in% c("ExtraTime"))])

#Check Error Metrics

confusionMatrix(pred_val, val_data$target,positive = "Yes")

#Predict on Test Data

pred_test <- predict(xgb_tuned_model, test_data)

#Write the predictions to the submission file

final <-cbind(index,test_data,pred_test)

names(final)[c(1,31)] = c("RowID","ExtraTime")

submission <- final[,names(final) %in% c("RowID","ExtraTime") ]

write.csv(submission, "C:/Users/Yunus Saleem/Desktop/Insofe/MiTH/Submission-5thAttXGb.csv")

####################Build an XGBoost with XGB.train for classification#############


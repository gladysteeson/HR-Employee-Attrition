# Model to Predict Employee Attrition
#####################################

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(arules)) install.packages("arules", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org")

#IBM Dataset
#https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset

## Import the dataset 
data = read.csv("https://raw.githubusercontent.com/gladysteeson/HR-Employee-Attrition/master/HR-Employee-Attrition.csv") 

# structure of the dataset
str(data)

# number of rows and columns in the dataset
dim(data)

## Pre-process the data

# check for NA/missing values
sum(is.na(data))

# check for Duplicate entries
nrow(data[!(duplicated(data)),])
nrow(data)

# remove columns which have same value for all
data$EmployeeCount <- NULL
data$StandardHours <- NULL
data$Over18 <- NULL

# remove irrelevant columns 
data$EmployeeNumber <- NULL
data$DailyRate <- NULL

# current number of rows and columns in the dataset
dim(data)

## Analyse the data

# Attrition by Age 
ggplot(data, aes(x=Age, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_manual(values=c("chartreuse4", "brown2")) +
  scale_color_manual(values=c("chartreuse4", "brown2")) +
  ggtitle("Density plot of Age") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "Age") 


# Gender distribution by Attrition
data %>% group_by(Attrition, Gender) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, Gender, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=Attrition, y=N, fill=Gender)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "#FFFFFF", position = position_dodge(0.9)) + 
  ggtitle("Gender by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")


# Business Travel frequency distribution by Attrition 
ggplot(data,aes(Attrition,BusinessTravel,color=Attrition))+geom_jitter() + scale_color_manual(values=c("chartreuse4", "brown2")) 


# Monthly Income distribution by Attrition 
ggplot(data, aes(x=MonthlyIncome, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_manual(values=c("chartreuse4", "brown2")) +
  scale_color_manual(values=c("chartreuse4", "brown2")) +
  ggtitle("Density plot of Monthly Income") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "MonthlyIncome") 


# Attrition by Monthly Income and Department
ggplot(data,aes(MonthlyIncome,Department, color=Attrition))+geom_point() + scale_color_manual(values=c("chartreuse4", "brown2"))


# Attrition by Monthly Income and Job Role
ggplot(data,aes(MonthlyIncome,JobRole, color=Attrition))+geom_point() + scale_color_manual(values=c("chartreuse4", "brown2"))


# Job Levels by Attrition 
ggplot(data, aes(x=Attrition, y=JobLevel, color=Attrition, fill=Attrition)) +
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_manual(values=c("chartreuse4", "brown2")) +
  scale_color_manual(values=c("#040242", "#040242")) +
  ggtitle("Job Level by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Job Level")


# Attrition by Distance From Home
ggplot(data, aes(x=DistanceFromHome, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_manual(values=c("chartreuse4", "brown2")) +
  scale_color_manual(values=c("chartreuse4", "brown2")) +
  ggtitle("Density plot of Distance From Home") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "Distance From Home") 


# Job Satisfaction by Attrition
ggplot(data, aes(x=Attrition, y=JobSatisfaction, color=Attrition, fill=Attrition)) +
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_manual(values=c("chartreuse4", "brown2")) +
  scale_color_manual(values=c("#040242", "#040242")) +
  ggtitle("Job Satisfaction by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Job Satisfaction")


# Marital Status by Attrition
data %>% group_by(Attrition, MaritalStatus) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, MaritalStatus, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=Attrition, y=N, fill=MaritalStatus)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "#FFFFFF", position = position_dodge(0.9)) + 
  ggtitle("MaritalStatus by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")


# Over Time worked by Attrition
data %>% group_by(Attrition, OverTime) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, OverTime, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=Attrition, y=N, fill=OverTime)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "#FFFFFF", position = position_dodge(0.9)) + 
  ggtitle("Over Time by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")


# Attrition by Years At Company
ggplot(data, aes(x=YearsAtCompany, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_manual(values=c("chartreuse4", "brown2")) +
  scale_color_manual(values=c("chartreuse4", "brown2")) +
  ggtitle("Density plot of Years At Company") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "Years At Company") 


# correlation between the attributes
corrplot(cor(sapply(data,as.integer)),method = "color")


## Transform the data

# convert every variables into numeric
data <- data %>% mutate_if(is.factor, as.integer)

# convert the target variable Attrition to factor 
data$Attrition=ifelse(data$Attrition==1,"No","Yes")
data$Attrition <- factor(data$Attrition)

# list types for each attribute
sapply(data, class)


## Create data parition with 80% as training and 20% as testing
set.seed(1, sample.kind="Rounding")
train_indices <- createDataPartition(data$Attrition 
                                     ,p = 0.8
                                     ,list = F)
data_train <- data[train_indices,]
data_test <- data[-train_indices,]

# MODEL 1: Logistic Regression

# define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

# fit Logistic regression Model
logR_model = train(Attrition~., data=data_train
                     , method="glm"
                     , family = "binomial"
                     , trControl=tr_control)

# predict using the Logistic Regression Model
predict_logR <- predict(logR_model
                         ,newdata = data_test
                         ,type = 'raw')

# evaluate the model
confusionMatrix(as.factor(predict_logR), as.factor(data_test$Attrition))
LogR_Model_auc <- auc(as.numeric(data_test$Attrition), as.numeric(predict_logR))
LogR_Model_auc

# add auc results in a table 
auc_results <- data.frame(method = "LogR_Model", auc = as.numeric(LogR_Model_auc)) 
auc_results %>% knitr::kable() 

# MODEL 2: Random Forest

# define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

# fit the Basic Random Forest Model
RF_model_1 = train(Attrition~., data=data_train
                   , method="rf"
                   , trControl=tr_control)


# predict using the Basic Random Forest Model
predict_RF1 <- predict(RF_model_1
                       ,newdata = data_test
                       ,type = 'raw')

# evaluate the model
confusionMatrix(as.factor(predict_RF1), as.factor(data_test$Attrition))
RF_Model_1_auc <- auc(as.numeric(data_test$Attrition), as.numeric(predict_RF1))
RF_Model_1_auc

# add auc results in a table 
auc_results <- bind_rows(auc_results, 
                         data_frame(method="RF_Model",   
                                    auc = as.numeric(RF_Model_1_auc)))
auc_results %>% knitr::kable()


## use SMOTE to create balanced dataset

#1. Get the attrition class count
attrition_count <- table(data$Attrition)
attrition_count

#2. Compute oversampling
over_count <- ((0.6 * max(attrition_count)) - min(attrition_count)) / min(attrition_count)

#3. Compute under sampling
under_count <- (0.4 * max(attrition_count)) / (min(attrition_count) * over_count)

over <- round(over_count,1) * 100
under <- round(under_count, 1) * 100

smote_data <-   SMOTE(Attrition ~.
                              ,data
                              ,perc.over = over
                              , k = 5
                              , perc.under = under)

# check the balanced dataset
table(smote_data$Attrition)


# create data parition with 80% as training and 20% as testing
set.seed(1, sample.kind="Rounding")
smote_train_indices <- createDataPartition(smote_data$Attrition 
                                     ,p = 0.8
                                     ,list = F)
smote_data_train <- smote_data[smote_train_indices,]
smote_data_test <- smote_data[-smote_train_indices,]

# MODEL 3: Random Forest using balanced dataset

# define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

# fit the Random Forest Model
RF_model_2 = train(Attrition~., data=smote_data_train
                                    , method="rf"
                                    , trControl=tr_control)


# predict using the Random Forest Model
predict_RF2 <- predict(RF_model_2
          ,newdata = smote_data_test
          ,type = 'raw')

# evaluate the model
confusionMatrix(as.factor(predict_RF2), as.factor(smote_data_test$Attrition))
RF_Model_2_auc <- auc(as.numeric(smote_data_test$Attrition), as.numeric(predict_RF2))
RF_Model_2_auc

# add auc results in a table 
auc_results <- bind_rows(auc_results, 
                         data_frame(method="RF_Model_balanced",   
                                    auc = as.numeric(RF_Model_2_auc)))
auc_results %>% knitr::kable()

# MODEL 4: Extreme Gradient Boosting using balanced dataset

# define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)
xgbm_grid <- expand.grid(
  max_depth = 12
  , subsample = 0.9
  , nrounds  = 200
  , gamma  = 0.01
  , colsample_bytree = 0.5
  , min_child_weight = 1
  , eta = 0.02
)

# fit the Extreme GBM Model
XGBM_model = train(Attrition~., data=smote_data_train
                                    , method="xgbTree"
                                    , verbose = F
                                    , trControl=tr_control
                                    , tuneGrid = xgbm_grid)

# predict using the XGBM Model 
predict_XGBM <- predict(XGBM_model
          ,newdata = smote_data_test
  )

# evaluate the model
confusionMatrix(as.factor(predict_XGBM), as.factor(smote_data_test$Attrition))
xgbm_auc <- auc(as.numeric(smote_data_test$Attrition), as.numeric(predict_XGBM))
xgbm_auc

# add auc results to the table
auc_results <- bind_rows(auc_results, 
                         data_frame(method="XGBM_Model",   
                                    auc = as.numeric(xgbm_auc)))
auc_results %>% knitr::kable()

# plot ROC-AUC curves
par(pty = "s")
plot.roc(as.numeric(data_test$Attrition), as.numeric(predict_logR)
         ,  main = 'ROC-AUC Curve', col='darkseagreen4', print.auc = F)
plot.roc(as.numeric(data_test$Attrition), as.numeric(predict_RF1)
         , col='blue', print.auc = F, add = TRUE)
plot.roc(as.numeric(smote_data_test$Attrition), as.numeric(predict_RF2)
         , col='red', print.auc = F, add = TRUE)
plot.roc(as.numeric(smote_data_test$Attrition), as.numeric(predict_XGBM)
         , col='orange', print.auc = F, add = TRUE)
legend('bottomright', c("LogR", "RF", "RF_balanced", "XGBM"), fill = c('darkseagreen4','blue','red', 'orange'), bty='n')

## Features of importance from the best model
feat_importance <- varImp(XGBM_model)

imp_DF <- data.frame(features = row.names(feat_importance[[1]]),
                     importance_val =  round(feat_importance[[1]]$Overall, 2)
                     
) 

imp_DF <- arrange(imp_DF, desc(importance_val))

# plot the top 10 features of importance
ggplot(head(imp_DF, 20), aes(x = reorder(features, importance_val), y = importance_val)) + 
  ggtitle("Feature Importance for Extreme Gradient Boosting") + 
  labs(x = "Feature Importance") + labs(y = "Value") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="none")+
  geom_bar(stat = "identity", fill = 'tan4') + coord_flip()













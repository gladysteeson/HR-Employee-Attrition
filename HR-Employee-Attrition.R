# Model to Predict Employee Attrition
#library(utils)    #for importing the file
#library(mlbench)  #for correlation matrix
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#library(caret)    #for models
#library(ggplot2)  #for plotting graphs
if(!require(arules)) install.packages("arules", repos = "http://cran.us.r-project.org")
#library(arules)
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
#library(corrplot)
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#library(dplyr)

if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#library(data.table)
#library(VIM)
#library(DT)
#library(gridExtra)
#library(ggplot2)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#library(tidyverse)
#library(Metrics)
#library(randomForest)
#install.packages("randomForest")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
#library(pROC)
#library(e1071)
#library(dtree)
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
#library(xgboost)
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org")
#library(DMwR)

#import the dataset 

data = read.csv("https://raw.githubusercontent.com/gladysteeson/HR-Employee-Attrition/master/HR-Employee-Attrition.csv") 

#structure of the dataset
str(data)

#number of rows and columns in the dataset
dim(data)

#checking for NA/missing values
sum(is.na(data))

#??Finding out the columns with NAs/missing values
colSums(is.na(data))
#Shows that none of the columns have NA values

#check for Duplicate entries
nrow(data[!(duplicated(data)),])
nrow(data)

#analyse the data

#examine the range of Age of the employees
data %>% 
  summarise(Median = median(`Age`), 
            Mean = mean(`Age`), 
            Max = max(`Age`), 
            Min = min(`Age`))

ggplot(data, aes(Age, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "Age vs. Attrition")+
  scale_fill_brewer(palette="Set1")#The attriton of younger employees are more than the old employees.
ggplot(data, aes(x=Attrition, Age, color=Attrition)) +
  geom_boxplot() +
  scale_color_manual(values=c("#CB181D", "#2171B5")) #There is a two outlier for Attrition is yes. #The people age betwwon 40 and 33 quiting their jobs


#Age distribution by Attrition Density Plot
ggplot(data, aes(x=Age, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of Age") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "Age") 

#manika
ggplot(data,aes(Attrition,YearsSinceLastPromotion,fill=Attrition))+geom_violin()
ggplot(data,aes(MonthlyIncome,WorkLifeBalance, color=Attrition))+geom_point()
ggplot(data,aes(MonthlyIncome,JobLevel, color=Attrition))+geom_point()
ggplot(data,aes(Attrition,BusinessTravel,color=Attrition))+geom_jitter()
ggplot(data,aes(Attrition,MonthlyIncome,fill=Attrition))+geom_boxplot()


#Gender distribution by Attrition bar plot
plot(data$Gender)
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


#Business Travel frequency distribution by Attrition bar plot
ggplot(data,aes(Attrition,BusinessTravel,color=Attrition))+geom_jitter()

data %>%
  ggplot(aes(x = BusinessTravel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "BusinessTravel") +
  facet_grid(~Attrition) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

#MonthlyIncome distribution by Attrition Density
ggplot(data, aes(x=MonthlyIncome, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of MonthlyIncome") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "MonthlyIncome") 

ggplot(data, aes(MonthlyIncome, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "Monthly Income vs. Attrition")+
  scale_fill_brewer(palette="Set1")
ggplot(data, aes(x=Attrition, MonthlyIncome, color=Attrition)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") 

#Job Levels and Attrition boxplot
ggplot(data, aes(x=Attrition, y=JobLevel, color=Attrition, fill=Attrition)) +
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  scale_color_manual(values=c("#661304", "#040242")) +
  ggtitle("Job Level by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Job Level")

#Department and Attrition
ggplot(data,aes(Attrition,Department,color=Attrition))+geom_jitter()

data %>%
  ggplot(aes(x = Department, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "Department") +
  facet_grid(~Attrition) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")


ggplot(data,aes(x=Attrition,group=Department))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Department)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Attrition vs. Department")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_brewer(palette="Set1")

#Distance From Home and Attrition 
ggplot(data, aes(x=DistanceFromHome, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of DistanceFromHome") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "DistanceFromHome") 

#Job Satisfaction and Attrition
ggplot(data, aes(x=Attrition, y=JobLevel, color=Attrition, fill=Attrition)) +
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  scale_color_manual(values=c("#661304", "#040242")) +
  ggtitle("JobtSatisfaction by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "JobSatisfaction")

#Marital Status and Attrition
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

########Number of Companies Worked in and Attrtion
ggplot(data, aes(x=NumCompaniesWorked, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of NumCompaniesWorked") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "NumCompaniesWorked") 

#Over Time worked and Attrtion
#ggplot(data,aes(Attrition,OverTime,color=Attrition))+geom_jitter()


data %>% group_by(Attrition, OverTime) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, OverTime, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=Attrition, y=N, fill=OverTime)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "#FFFFFF", position = position_dodge(0.9)) + 
  ggtitle("OverTime by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")

#Performance Rating and Attrition
plottable1=table(data$Attrition,data$JobLevel)
plottable6=table(data$Attrition,data$PerformanceRating)
barplot(plottable6, main="Employees left vs Performance Rating", xlab="PerformanceRating",col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)

#Work Life Balance and Attrition
plottable6=table(data$Attrition,data$WorkLifeBalance)
barplot(plottable6, main="Employees left vs WorkLifeBalance", xlab="WorkLifeBalance",col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)

#Total Working Years and Attrition
ggplot(data, aes(x=TotalWorkingYears, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of TotalWorkingYears") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "TotalWorkingYears") 

#Years At Company and Attrition
ggplot(data, aes(x=YearsAtCompany, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of YearsAtCompany") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "YearsAtCompany") 

#YearsInCurrentRole
ggplot(data, aes(x=YearsInCurrentRole, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of YearsInCurrentRole") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "YearsInCurrentRole") 

#YearsSinceLastPromotion
ggplot(data, aes(x=YearsSinceLastPromotion, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of YearsSinceLastPromotion") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "YearsSinceLastPromotion") 

#YearsWithCurrManager
ggplot(data, aes(x=YearsWithCurrManager, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of YearsWithCurrManager") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "YearsWithCurrManager") 


#remove columns which have same value for all
data$EmployeeCount <- NULL
data$StandardHours <- NULL
data$Over18 <- NULL

#remove irrelevant columns 
data$EmployeeNumber <- NULL
data$DailyRate <- NULL

#current number of rows and columns in the dataset
dim(data)
#We have reduced the number of features from 35 to useful 30 features.

#examine structure of data
str(data)

#discretize variables

data$Age <- discretize(data$Age, method = "interval",breaks = 4, labels = c("Young","Thirties","Forties","Old"))
data$HourlyRate <- discretize(data$HourlyRate, method = "interval",breaks = 7, labels = c("30-40","40-50","50-60","60-70","70-80","80-90","80-100"))
data$DistanceFromHome <- discretize(data$DistanceFromHome, method = "interval",breaks = 6, labels = c("1-5","6-10","11-15","16-20","21-25","26-30"))
data$PercentSalaryHike <- discretize(data$PercentSalaryHike, method = "interval",breaks = 3, labels = c("11%-15%","16%-20%","21%-25%"))
data$YearsWithCurrManager <- discretize(data$YearsWithCurrManager, method = "interval", breaks = 6, labels  = c('0-3','4-6','7-9','10-12','13-15','16-18'))
data$MonthlyIncome <- discretize(data$MonthlyIncome, method = 'interval')
data$MonthlyRate <- discretize(data$MonthlyRate, method = 'interval', categories = 4)

#convert every variables into categorical type
data <- data %>% mutate_if(is.integer, as.factor)

#examine the structure of data
str(data)

#correlation between attributes
corrplot(cor(sapply(data,as.integer)),method = "color")

#use SMOTE to create balanced dataset

#1. Get the attrition class count
attrition_count <- table(data$Attrition)
attrition_count

#2. Compute oversampling
over_count <- ((0.6 * max(attrition_count)) - min(attrition_count)) / min(attrition_count)

#3. Compute under sampling
under_count <- (0.4 * max(attrition_count)) / (min(attrition_count) * over_count)

over = round(over_count,1) * 100
under = round(under_count, 1) * 100

smote_data <-   SMOTE(Attrition ~.
                              ,data
                              ,perc.over = over
                              , k = 5
                              , perc.under = under)

#check the balanced dataset
table(smote_data$Attrition)



#create data parition with 80% as training and 20% as testing
train_indices <- createDataPartition(smote_data$Attrition 
                                     ,p = 0.8
                                     ,list = F)
data_train <- smote_data[train_indices,]
data_test <- smote_data[-train_indices,]

# MODEL 1: Logistic Regression

#define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

#basic Logistic regression Model
logR_model_1 = train(Attrition~., data=data_train
                     , method="glm"
                     , metric="ROC"
                     , trControl=tr_control)

#predict using the Logistic Regression Model
predict_logR1 <- predict(logR_model_1
                         ,newdata = data_test
                         ,type = 'raw')

#evaluate the model

confusionMatrix(as.factor(predict_logR1), as.factor(data_test$Attrition))
LogR_Model_auc <- auc(as.numeric(data_test$Attrition), as.numeric(predict_logR1))
LogR_Model_auc

#add auc results in a table 
auc_results <- data.frame(method = "LogR_Model", auc = as.numeric(LogR_Model_auc)) 
auc_results %>% knitr::kable() 


# MODEL 2: Random Forest

#define the cross validation
tr_control <- trainControl(method = 'cv', number = 5, classProbs = T)

#basic Random Forest Model
RF_model_1 = train(Attrition~., data=data_train
                                    , method="rf"
                                    , metric="ROC"
                                    , trControl=tr_control)


#predict using the Basic Random Forest Model 1
predict_RF1 <- predict(RF_model_1
          ,newdata = data_test
          ,type = 'raw')

#evaluate the model

confusionMatrix(as.factor(predict_RF1), as.factor(data_test$Attrition))
RF_Model_1_auc <- auc(as.numeric(data_test$Attrition), as.numeric(predict_RF1))

as.numeric(RF_Model_1_auc)
# add auc results in a table 
auc_results <- bind_rows(auc_results, 
                         data_frame(method="RF_Model",   
                                    auc = as.numeric(RF_Model_1_auc)))
auc_results %>% knitr::kable()

# MODEL 3: Extreme Gradient Boosting

#Introduction to Extreme Gradient Boosting classifier
# The Extreme Gradient boosting classifier is similar to that of a gradient boosting classifier. However, it provides
#better performance because - 
#1. Formulization of more-regularized models to control overfitting
#2.Completes tasks at faster speed using parallel computation on a single machine
###############################


#define the cross validation
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

#Extreme GBM Model

XGBM_model = train(Attrition~., data=data_train
                                    , method="xgbTree"
                                    , verbose = F
                                    , metric="ROC"
                                    , trControl=tr_control
                                    , tuneGrid = xgbm_grid)

#predict using the XGBM Model 
predict_XGBM <- predict(XGBM_model
          ,newdata = data_test
  )

#evaluate the model

confusionMatrix(as.factor(predict_XGBM), as.factor(data_test$Attrition))
xgbm_auc_1 <- auc(as.numeric(data_test$Attrition), as.numeric(predict_XGBM))

#add auc results to the table
auc_results <- bind_rows(auc_results, 
                         data_frame(method="XGBM_Model_1",   
                                    auc = as.numeric(xgbm_auc_1)))
auc_results %>% knitr::kable()

#3. Check the features of importance from the best model
feat_importance <- varImp(XGBM_model)

imp_DF <- data.frame(features = row.names(feat_importance[[1]]),
                     importance_val =  round(feat_importance[[1]]$Overall, 2)
                     
) 

imp_DF <- arrange(imp_DF, desc(importance_val))

#Plot the top 10 features with their importances
ggplot(head(imp_DF, 20), aes(x = reorder(features, importance_val), y = importance_val)) +
  geom_bar(stat = "identity", fill = 'tan4') + coord_flip()


#It can be observed from the plot that the most influencing features are - 
#1.Overtime
# - When employees work overtime, the probablity of employee attrition is high. This result is evident
#from the previous descriptive visualization plots 

#2. Job levels
# The occurence of employee retention is least among the employees at job level = 2.
#This mostly depicts the common cases - once the the employees at level=1, get promoted and reach the 
#level 2, the tendency is to shift to another company as the chances of getting a salary hike are high. This shows
#that measures that HR must take to influence the newly promoted employees to remain in the company (Especially, those from level 1 to level 2)

#3. Work-life balance
#The value of 3 refers to a better work-life balance.

#4. The Marital Status = Single
#Considering the cases where employees who are Single prefer exploring greater opportunities thereby,
#tend to switch jobs. In case of employees who are married prefer stability and hence, the attrition
#rate amongst such employees is lower. 

#5. Stock options
#This implies that considering the policy of providing stock options for employees will help in increasing employee retition. This is
#because of the fact that it can be oserved the employees with least stock options tend to leave the company.

#5. Number of Years served at company
#The employees who have spent the least time - 1 year with the company are the ones who tend to leave. This 
#depicts that within the period of 1 or 2 years, the employees are not influenced enough to remain in the organization.
#Hence, the HR can devise strategies that attract and influence the employees in the initial
#years itself. 

#These suggest the areas that need the HR attention. Thus, provides the direction in which 
#employee attrition can be prevented. 












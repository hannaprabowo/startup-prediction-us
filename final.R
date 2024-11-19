
# set up
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory

# install packages
if (!require("pacman")) install.packages("pacman")
p_load(caret, tidyverse, kableExtra, glmnet, performanceEstimation, class, 
       rpart, rattle, plyr, randomForest, rpart.plot, ggpubr) # load packages

# load dataset
data <- read_csv("startup data.csv")

data %>% is.na() %>% colSums() # check NAs per column

colnames(data)

# clean data by removing columns that has NAs
final_data <- data[,-c(1:14,23,40,47)] # remove all NAs column
final_data <- final_data[, which(colMeans(!is.na(final_data)) > 0.8)] # remove columns that has more than 80% NA

# We can see there are still empty values in the First and Last Milestone data, particularly for startups that are already closed
# Because NAs can hinder the performance of the analyses, this value will be filled with 0 

final_data %>% is.na() %>% colSums() # check NAs per column

final_data2 <- final_data %>% 
                  mutate(age_first_milestone_year = ifelse(is.na(age_first_milestone_year),
                                                                  0, age_first_milestone_year),
                  age_last_milestone_year = ifelse(is.na(age_last_milestone_year),
                                                          0, age_last_milestone_year))

final_data2 %>% is.na() %>% colSums() # check NAs per column

final_data2$status %>% str()

# change to factors for whether a startup is Acquired / Closed
final_data2$status <- mapvalues(final_data2$status, 
                                from = c('acquired', 'closed'),
                                to = c(1,0)) # 1 = acquired, 0 = close
final_data2$status <- as.factor(final_data2$status) # change to factor for target variable

## convert to one hot encoding

# state
state <- as.factor(ifelse(final_data2$is_CA == 1, "CA",
                          ifelse(final_data2$is_NY == 1, "NY",
                                 ifelse(final_data2$is_MA == 1, "MA",
                                        ifelse(final_data2$is_TX == 1, "TX",
                                               ifelse(final_data2$is_otherstate == 1, "Other State", "Other State"))))))
# industry
industry <- as.factor(ifelse(final_data2$is_software == 1, "Software",
                             ifelse(final_data$is_web == 1, "Web",
                                    ifelse(final_data2$is_mobile == 1, "Mobile",
                                           ifelse(final_data2$is_enterprise == 1, "Enterprise",
                                                  ifelse(final_data2$is_advertising == 1, "Advertising",
                                                         ifelse(final_data$is_gamesvideo == 1, "Video Games",
                                                                ifelse(final_data$is_ecommerce == 1, "E-Commerce",
                                                                       ifelse(final_data2$is_biotech == 1, "Biotech",
                                                                              ifelse(final_data2$is_consulting == 1, "Consulting",
                                                                                     ifelse(final_data2$is_othercategory == 1, "Other Category",0)))))))))))

# add categorical factors
final_data2$industry <- industry
final_data2$state <- state

# remove one hot encoding
final_data2 <- final_data2[,-c(9:24)]

# divide train and test set, 70 -30 split #
set.seed(1000)
index <- createDataPartition(final_data2$status, p = .7, list = FALSE)
train <- final_data2[ index,]
test  <- final_data2[-index,]

##########################################################
################### BUILD MODELS #########################
##########################################################  

#### LOGISTIC REGRESSION ####

set.seed(1000)
logistic_model <- glm(status ~ .,
                      data = train,
                      family = binomial) # y = status of company, 0 = closed, 1 = acquired

predictions.log <- predict(logistic_model, newdata = test, type = "response")
predictions.log <- as.factor(ifelse(predictions.log > 0.5, "1", "0")) # 1 = acquired, 0 = closed
confmatrix.log <- confusionMatrix(predictions.log, test$status, positive = "1", mode = 'everything')
print(confmatrix.log) # confusion matrix

summary(logistic_model) #company's status as top 500 highly determined the success of startup

#### DECISION TREES ####

set.seed(1000)
dt_model <- rpart(status ~., 
                  data = train,
                  method = 'class')
predictions.dt <- predict(dt_model, type="class", newdata = test)
confmatrix.dt <- confusionMatrix(predictions.dt, test$status, positive = "1")
print(confmatrix.dt) # dt's confusion matrix

rpart.plot(dt_model) # decision tree plot

#### BAGGING ####

n_pred <- ncol(final_data2) - 1
set.seed(1000)
bag <- randomForest(status ~ ., 
                    data=train, 
                    mtry=n_pred, 
                    importance=TRUE)
predictions.bagging <- predict(bag, newdata=test)
confmatrix.bagging <- confusionMatrix(predictions.bagging, test$status, positive = "1", mode = 'everything')
print(confmatrix.bagging) # bagging confusion matrix

#### RANDOM FOREST #### 

# Cross validation to find the optimal m (number of trees), 5-cv
set.seed(1000)
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(status ~.,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE, 
                 ntree = 500)

print(rf_mtry) # cv results, optimal number of tree is 5

# Random forest with optimal m
set.seed(1000)
rf_tune <- randomForest(status ~ ., 
                        data=train, 
                        mtry = rf_mtry$bestTune$mtry, 
                        importance=TRUE,
                        ntree = 500)
test.pred.rf <- predict(rf_tune, newdata=test)
confmatrix.rf <- confusionMatrix(test.pred.rf, test$status, positive = "1", mode = 'everything')
print(confmatrix.rf) # random forest confusion matrix

# Random Forest with default mtry (sqrt of # of variables) and ntrees for comparison #
set.seed(1000)
rf_default <- randomForest(status ~ ., 
                           data=train, 
                           importance=TRUE)
test.pred.default <- predict(rf_default, newdata=test)
confmatrix.rf.default <- confusionMatrix(test.pred.default, test$status, positive = "1", mode = 'everything')
print(confmatrix.rf.default) # confusion matrix

#### RF OOB PLOT #### 

legends.oob <- rf_tune$err.rate
colnames(legends.oob) <- c("OOB Error", "Closed (Class: 0)", "Acquired (Class: 1)")
plot(rf_tune)
legend("topright", colnames(legends.oob),col=1:4,cex=0.8,fill=1:4) # for legend

#### VARIABLE IMPORTANCE PLOT #### 

# Results indicate the importance of variables based on Mean Decrease Accuracy and Gini Index
# MDA = higher value indicates the removal of variable leads to an impact on the accuracy results
# MD Gini = higher value indicates the variable contributes to the homogeneity of the nodes

varImpPlot(rf_tune, main = "")



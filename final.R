
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




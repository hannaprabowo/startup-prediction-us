
# set up
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory

# install packages
if (!require("pacman")) install.packages("pacman")
p_load(caret, tidyverse, kableExtra, glmnet, performanceEstimation, class, 
       rpart, rattle, plyr, randomForest, rpart.plot, ggpubr) # load packages

# load dataset
data <- read_csv("startup data.csv")

data %>% is.na() %>% colSums() # check NAs per column

# clean data
final_data <- data[,-c(1:14,23,40,47)] # remove all NAs column
final_data <- final_data[, which(colMeans(!is.na(final_data)) > 0.8)] # remove columns that has more than 80% NA

final_data

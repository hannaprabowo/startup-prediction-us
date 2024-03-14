
# set up
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory

# install packages
if (!require("pacman")) install.packages("pacman")
p_load(caret, tidyverse, kableExtra, glmnet, performanceEstimation, class, 
       rpart, rattle, plyr, randomForest, rpart.plot, ggpubr) # load packages

# load dataset
data <- read_csv("startup data.csv")

data %>% is.na() %>% colSums() # check NAs per column

# 

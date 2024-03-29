# Main Function to run scripts automatically

# Introduction and presetting -----------

# Choose between full ("research"), operational and revised model regarding adverse impact analysis 
# exclude protected class attributes  that violate discrimination criteria (age, gender)
# should protected groups be included? Should correlated features with protected group attributes be included?
# Options: "full", "revised"
adverse_impact_model_type <- "revised"

# choose grid_type full to perform a complete training process including a wider range of hyper parameters
# This option could lead to way higher time consuming 
# grid_type <- "full"



#Setting up seed for pseudo-random numbers to get reproducible results
set.seed(1)


# General R libraries for data analysis and plotting ---------------
suppressPackageStartupMessages({
  library(scales)
  library(stringr)
  library(dplyr)
  library(reshape2) 
  library(tidyverse)
  library(ggplot2)
  library(psych)
  library(vtable)
  library(ggplot2)
  # Data science frameworks for generalized parallel training, evalutation and prediction or visualization
  library(ModelMetrics)
  library(caret) #Central Framework for Machine Learning
  library(doParallel)
  library(ROCR)
  library(pROC)
  library(ROSE)  #Resampling
  library(car)   #VIF  
  
  # Specific packages for algorithms
  library(nnet)
  library(gbm)
  library(kernlab)
  library(randomForest)
  library(MASS)
  library(rpart)
  library(rpart.plot)
  library(e1071)
  library(xgboost)
  library(ranger)
  
  # Explainable Artificial Intelligence (XAI) Packackes
  library(iml) # Central Package for Interpretable Machine Learning
  library(lime)
  library(anchors)
  library(vip)
  library(treeshap)
  library(ALEPlot)
  library(fastshap)
  # Supporting packages for visualization
  library(patchwork)
  library(partykit)
  library(libcoin)
  library(mvtnorm)
  library(DescTools)
})

# Print all relevant packages and their dependencies
devtools::session_info()


source(stringr::str_c(dirname(getwd()),"/DRV_Preprocessing.R"), echo=T)

#source(stringr::str_c(dirname(getwd()),"DRV_statistical_analysis.R"), echo=T) # commented out, since not relevant for experimental purposes

#source(stringr::str_c(dirname(getwd()),"DRV_Resampling_methods.R"), echo=T) # commented out, since resampling and balancing not increasing performance 

source(stringr::str_c(dirname(getwd()),"/DRV_Model_Training.R"), echo=T)

source(stringr::str_c(dirname(getwd()),"/DRV_Analysis.R"), echo=T)

source(stringr::str_c(dirname(getwd()),"/DRV_XAI.R"), echo=T)

source(stringr::str_c(dirname(getwd()),"/DRV_Adverse_Impact.R"), echo=T)

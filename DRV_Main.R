# Main Function to run scripts automatically

# Introduction and presetting -----------

#Setting up seed for pseudo-random numbers to get reproducible results
set.seed(1)

# General R libraries for data manipulation and plotting
suppressPackageStartupMessages({
  library(scales)
  library(stringr)
  library(dplyr)
  library(reshape2) 
  library(tidyverse)
  library(ggplot2)
  library(psych)
  library(vtable)
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
})


# Print all relevant packages and their dependencies
devtools::session_info()

source("DRV_Preprocessing.R", echo=T)

#source("DRV_statistical_analysis.R", echo=T) # commented out, since not relevant for experimental purposes

#source("DRV_Resampling_methods.R", echo=T) # commented out, since resampling and balancing not increasing performance 

source("DRV_Model_Training.R", echo=T)

source("DRV_Analysis.R", echo=T)

source("DRV_XAI.R", echo=T)

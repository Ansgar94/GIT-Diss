# Setting up caret train control for resampling with cross validation, using parallel processing on computing cluster -----

# Setup trainControl and grid for Hyperparameter Tuning with repeated cross validation
ctrl <- trainControl(method = 'repeatedcv', repeats=1, number=5, classProbs=T, summaryFunction = twoClassSummary, verboseIter = T) #5,10

# Setup grid of hyper parameter for XGBoost 
grid_xgbDART <- expand.grid(nrounds = 50, max_depth = c(4), eta = c(0.5), gamma = 0, subsample = 0.5, colsample_bytree = c(0.8), rate_drop = c(0.01),
                            skip_drop =  c(0.95), min_child_weight = 1)


# Create different versions of the training set prior to model tuning

down_train <-  ovun.sample(Turnover ~ ., data  = training, seed=1,
                    method="under")$data                         
table(down_train$Turnover) 

up_train <- ovun.sample(Turnover ~ ., data  = training, seed=1,
                 method="over")$data                         
table(up_train$Turnover) 


rose_train <- ROSE(Turnover ~ ., data  = training)$data                         
table(rose_train$Turnover) 

rose_train_0_1 <- ROSE(Turnover ~ ., data  = training, seed=1,
                       N = 100000,
                       p = 0.1)$data                         
table(rose_train_0_1$Turnover) 


rose_train_0_15 <- ROSE(Turnover ~ ., data  = training, seed=1,
                        N = 200000,
                        p = 0.15)$data                         
table(rose_train_0_15$Turnover) 


rose_train_0_2 <- ROSE(Turnover ~ ., data  = training, seed=1,
                       N = 150000,
                       p = 0.2)$data                         
table(rose_train_0_2$Turnover) 

rose_train_0_25 <- ROSE(Turnover ~ ., data  = training, seed=1,
                       N = 80000,
                       p = 0.25)$data                         
table(rose_train_0_25$Turnover) 


rose_train_0_3 <- ROSE(Turnover ~ ., data  = training, seed=1,
                        N = 100000,
                        p = 0.3)$data                         
table(rose_train_0_3$Turnover) 


# Train model for each resampled data set -----------

#enable parallel clustering (stop command located at the buttom of model training)

cl <- makePSOCKcluster(detectCores()-1)
print(str_c('Number of cores used for parallel Processing is: ', detectCores()-1))  
registerDoParallel(cl)


orig_fit <- train(Turnover ~ ., data = training, 
                  method = "xgbDART",
                  tuneGrid=grid_xgbDART,
                  metric = "ROC",
                  trControl = ctrl)
#                  tuneLength=1)

down_outside <- train(Turnover ~ ., data = down_train, 
                      method = "xgbDART",
                      tuneGrid=grid_xgbDART,
                      metric = "ROC",
                      trControl = ctrl)

up_outside <- train(Turnover ~ ., data = up_train, 
                    method = "xgbDART",
                    tuneGrid=grid_xgbDART,
                    metric = "ROC",
                    trControl = ctrl)

rose_outside <- train(Turnover ~ ., data = rose_train, 
                      method = "xgbDART",
                      tuneGrid=grid_xgbDART,
                      metric = "ROC",
                      trControl = ctrl)


rose_outside_0_1 <- train(Turnover ~ ., data = rose_train_0_1, 
                          method = "xgbDART",
                          tuneGrid=grid_xgbDART,
                          metric = "ROC",
                          trControl = ctrl)

rose_outside_0_15 <- train(Turnover ~ ., data = rose_train_0_15, 
                           method = "xgbDART",
                           tuneGrid=grid_xgbDART,
                           metric = "ROC",
                           trControl = ctrl)


rose_outside_0_2 <- train(Turnover ~ ., data = rose_train_0_2, 
                          method = "xgbDART",
                          tuneGrid=grid_xgbDART,
                          metric = "ROC",
                          trControl = ctrl)

rose_outside_0_25 <- train(Turnover ~ ., data = rose_train_0_25, 
                           method = "xgbDART",
                           tuneGrid=grid_xgbDART,
                           metric = "ROC",
                           trControl = ctrl)

rose_outside_0_3 <- train(Turnover ~ ., data = rose_train_0_3, 
                          method = "xgbDART",
                          tuneGrid=grid_xgbDART,
                          metric = "ROC",
                          trControl = ctrl)

#Stop using parallel Clustering
stopCluster(cl)


# Evaluate results with resampling on training data -------------

# Set up list of models
rose_models <- list(original = orig_fit,
                       down = down_outside,
                       up = up_outside,
                       ROSE = rose_outside,
                       ROSE01 = rose_outside_0_1,
                       ROSE015 = rose_outside_0_15,
                       ROSE02 = rose_outside_0_2,
                       ROSE025 = rose_outside_0_25,
                       ROSE03 = rose_outside_0_3)


# Evaluate results with resampling on training data
rose_train_resampling <- resamples(rose_models)

print("rose_train_resampling: ")
summary(rose_train_resampling, metric = "ROC")



# Evaluate results with Hold.out in Rose package on train data -----------

# Set up wrapper function
rose_train_fun <- function(rose_data, measure){
  
  print(i)
  i<-i+1
  print(measure)
  rose_test <- ROSE.eval(Turnover ~ ., data = rose_data, 
                         learner = "rpart", 
                         method.assess = "holdout", 
                         extr.pred = function(obj)obj[,2], 
                         trace=T,
                         acc.measure = measure)$acc
  
}

# Initialize list with resampled train data
rose_train_list <- list(original = training,
                        down = down_train,
                        up = up_train,
                        ROSE = rose_train,
                        ROSE01 = rose_train_0_1,
                        ROSE015 = rose_train_0_15,
                        ROSE02 = rose_train_0_2,
                        ROSE025 = rose_train_0_25,
                        ROSE03 = rose_train_0_3)



# Define measures to be evaluated
measures <- c("auc", "precision", "recall", "F")

# Initialize data frame to safe results
rose_train_eval <-as.data.frame(matrix(0,nrow=length(rose_train_list), ncol=length(measures)))
rownames(rose_train_eval) <- names(rose_train_list)
colnames(rose_train_eval) <- measures


# Start Training process, takes about 10 minutes

for (measure in measures) {
  i<-0
  rose_train_result <- lapply(rose_train_list, rose_train_fun, measure) 
  rose_train_eval[names(rose_train_result), measure] <- unlist(rose_train_result)
}

print("rose_train_eval:")
rose_train_eval




# Evaluate results on test data for multiple classification measures on test data -----------

# Set up wrapper function
conMA <- function(model, data) {
  conMa <- confusionMatrix(test$Turnover, 
                            predict(object=model, test),
                            positive="Turnover") 
  conMa_measure <- c(conMa$byClass[c("Sensitivity","Specificity","Precision","Recall","F1")], conMa$overall[c("Accuracy", "Kappa")])
  conMa_measure <- c("ROC"=roc(test$Turnover, 
                       predict(object=model, test, type = "prob")[, "No_Turnover"],
                       levels = rev(levels(test$Turnover)))$auc
                     , conMa_measure)
  conMa_measure                      
}

# Initialize data frame to safe results
rose_test_measures=c("ROC","Sensitivity","Specificity","Precision","Recall","F1","Accuracy", "Kappa")
rose_test_results <-as.data.frame(matrix(0,nrow=length(rose_models), ncol=length(rose_test_measures)))
rownames(rose_test_results) <- names(rose_models)
colnames(rose_test_results) <- rose_test_measures

# Start Training process, takes about 10 minutes
rose_test_list <- lapply(rose_models, conMA, test) 

# show results
rose_test_results <- data.frame(rose_test_list) %>% t()
print("rose_test_results: ")
rose_test_results


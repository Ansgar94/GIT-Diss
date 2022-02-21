# Setting up caret train control for resampling with cross validation, using parallel processing on computing cluster -----

# Setup trainControl and grid for Hyperparameter Tuning with repeated cross validation
ctrl <- trainControl(method = 'repeatedcv', repeats=1, number=5, classProbs=T, summaryFunction = twoClassSummary, verboseIter = T) #5,10

# Setup grid of hyper parameter for XGBoost 
grid_xgbDART <- expand.grid(nrounds = 100, max_depth = c(4,6), eta = c(0.5), gamma = 0, subsample = 0.5, colsample_bytree = c(0.8), rate_drop = c(0.05),
                            skip_drop =  c(0.95), min_child_weight = 1)

# Initialize resampling with chosen number of data points ---------

# Create different versions of the training set prior to model tuning without choosen number of data points 
down_train <-  ovun.sample(Turnover ~ ., data  = training, seed=1, method="under")$data                         
up_train <- ovun.sample(Turnover ~ ., data  = training, seed=1, method="over")$data                         
rose_train <- ROSE(Turnover ~ ., data  = training)$data

# Safe resampled train data in list 
rose_train_list <- list(original = training,
                        down = down_train,
                        up = up_train,
                        ROSE = rose_train)

# Create different versions of the training set based on chosen number of data points N and percentage share p with hybrid resampling of ROSE --------

# Number of data points N in resampled set
options(scipen=99) # raise threshold of scientific notation to always use non-scientific notation of high numbers
N_range <- seq(from=100000,to=500000, by=100000)

# Percentage share of Turnover
p_range <- seq(from=0.1,to=0.50, by=0.1)

# Setup resampling function using number of data points n and percentage share p
rose_train_fun <- function(N, p){
  rose_train <- ROSE(Turnover ~ ., 
                        data  = training, 
                        seed=1,
                        N = N,
                        p = p)$data   
}

# Initialize ROSE resampling for all combinations of data points N and percentage share p, safe them in rose_train_list
for (p in p_range){
  for (N in N_range) {
  name_rose_train <- paste("ROSE", p, N, sep="_")
  rose_train_list[[name_rose_train]] <- rose_train_fun(N,p)
  print(paste(name_rose_train,"   generated"))
  }
}  

# show names of all training sets generated
names(rose_train_list)

# show one example data set and table
table(rose_train_list[[5]]$Turnover)
summary(rose_train_list[[5]])


# Train model for each resampled data set and test them on real-world (non-resampled) test data set -----------

# Define methods to evaluate resampled data set
methods_eval <- c("rpart", "glm","xgbDART")

#enable parallel clustering (stop command located at the button of model training)

cl <- makePSOCKcluster(detectCores()-2)
print(str_c('Number of cores used for parallel Processing is: ', detectCores()-1))  
registerDoParallel(cl)



#Define function to test several methods on resampled dataset
rose_models_fun <- function(rose_data_name, method){
  #Train model resampled data science
  model <- train(Turnover ~ ., data = rose_train_list[[rose_data_name]], 
                            method = method,
                            metric = "ROC",
                           tuneGrid = if (method=="xgbDART") 
                             {grid_xgbDART} else 
                               {NULL},
                            trControl = ctrl)
  conMa_F1 <- confusionMatrix(test$Turnover, 
                              predict(object=model, test),
                              positive="Turnover")$byClass['F1']
  # Print result of each iteration to monitor the process
  print(conMa_F1)
}




# Initialize data frame to safe results with col and row names
rose_methods_eval <-as.data.frame(matrix(0,nrow=length(rose_train_list), ncol=length(methods_eval)))
rownames(rose_methods_eval) <- names(rose_train_list)
colnames(rose_methods_eval) <- methods_eval


# Initialize performance test on test data
for (method in methods_eval) {
  rose_method_ROC  <- lapply(names(rose_train_list), rose_models_fun, method) 
  print(unlist(rose_method_ROC))
  rose_methods_eval[, method] <- unlist(rose_method_ROC)
}


# Ürint out results on test data
rose_methods_eval

# Close connection to cluster for parallel processing
stopCluster(cl)
print("Resampling finished")


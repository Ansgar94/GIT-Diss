# Setting up caret train control for resampling with cross validation, using parallel processing on computing cluster ------
# Default Set-up: Measure=Kappe, training set = Originial (not resampled), cross validation with 5 splites, amount of test data = 6Month


# Optional: Initialize subsambling method for imbalanced classes  (e.g, N=200000, p=0.15 identified as best fit for XGB in Subsampling tests)
#train_sampled  <- ROSE(Turnover ~ ., data  = training, seed=1,
#                        N = 200000,
#                       p = 0.15)$data                         
#table(train_sampled$Turnover) 

train_sampled <- training  #alternatively use original data set

# Choose metric: F1 Score, g-measure, precision, recall, cohens kappa (or own calculation based on economic impacts)
# Caret: n problems where there are a low percentage of samples in one class, using metric = "Kappa" can improve quality of the final model
# defaultSummary:    Accuracy, Kappa
# twoClassSummary:   AUC (area under the ROC curve - see last line of answer), Sens and Spec
# prSummary:         Precision, Recall

# combine all measures in one function
fullSummary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

chosen_measure <- "Kappa"   #standard: 'Kappa', tested: ROC; Kappa, Spec


ctrl <- trainControl(method = 'repeatedcv', repeats=1, number=5, classProbs=T, summaryFunction = fullSummary, trim=F, returnData=F) #5,10
# Other tested alternatives:  gridsearch: 'random' with tuneLength=10, Timeslice
# Trim and return data minimize the required data https://stackoverflow.com/questions/6543999/why-is-caret-train-taking-up-so-much-memory


# Choose grid type if a "full" more complete seach for possible hype parameters, in option "short" a more narrow pre-selection of hyperparameters is used 
grid_type <- 'short'
#grid_type <- 'full'

#enable parallel clustering (stop command located at the bottom of model train_sampled) --------------
cl <- makePSOCKcluster(detectCores()-1)
#cl <- makePSOCKcluster(8) SVM
print(str_c('Number of CPU cores used for parallel processing is: ', detectCores()-1))  
registerDoParallel(cl)

modelLookup(model='rf')         # Random Forest -------
#Hyperparameter: mtry = Number of variables randomly sampled as candidates at each split
#these parameters are theoretically not important for accuracy (just for performance)https://stats.stackexchange.com/questions/50210/caret-and-randomforest-number-of-trees
if (grid_type=='full')
{grid_rf <- expand.grid(splitrule="gini",mtry=seq(from=2, to=8, by=2),min.node.size=c(3,5))}

grid_rf <- expand.grid(splitrule="gini",mtry=c(6),min.node.size=c(10)) #min.node.size=c(10) for full model, 5 revised model

model_rf<-caret::train(Turnover~.,data=train_sampled, method='ranger', trControl=ctrl, tuneGrid=grid_rf, metric=chosen_measure)
print(model_rf)
  

modelLookup(model='glm')        # Generalized Linear Model -----
#Hyperparameter: not needed (intercept and psy)
model_glm <- caret::train(Turnover ~., data = train_sampled, method = 'glm', trControl=ctrl, metric=chosen_measure)
print(model_glm)
summary(model_glm$finalModel)



modelLookup(model='rpart2')     # Decision Tree) --------
#Hyperparameter: Maximum depth of decision tree

grid_rpart <- expand.grid(cp=seq(from=0.005,to=0.01,by=0.005)) 

if (grid_type=='full')
{grid_rpart <- expand.grid(cp=seq(from=0.001,to=0.03,by=0.002))}

model_rpart <- caret::train(Turnover ~., data=train_sampled %>% replace(.,.<0,0), method='rpart', trControl=ctrl, tuneGrid=grid_rpart, metric="ROC")
model_rpart$finalModel
#Plotting decision tree to visualize rules used for prediction in this white-box
rpart.plot(model_rpart$finalModel,roundint=T)



modelLookup(model='gbm')        # Generalized Boosted Machine (based on Decision Tree) --------
#Hyperparameter: Number of Trees, Shrinkage used in gradient descent Boosting, Minimum of observations per node, Number maximum Splits in depth
grid_gbm <- expand.grid(n.trees=c(50,100),shrinkage=c(0.1),n.minobsinnode = c(5,7),interaction.depth=c(5,8))
if (grid_type=='full')
{grid_gbm <- expand.grid(n.trees=c(50,100,150,200),shrinkage=c(0.1,0.15,0.2),n.minobsinnode = c(3,5,7,10),interaction.depth=c(5,7,9,11))}

model_gbm<-caret::train(Turnover~., data=train_sampled, method='gbm', trControl=ctrl, tuneGrid=grid_gbm, metric=chosen_measure)
#Results of Hyperparameter Tuning for best model, used as example for Hyperparamters
plot_gbm<-plot(model_gbm)
plot_gbm

modelLookup(model='pcaNNet')    # Neural Net with principal component analysis (0PCA)-------
# method 'nnet' does not train properly and RMSE does not decline for various Hyperparameter (possibly to much weights to learn)
# tried solution: use a principal component analysis (pca) to reduce number of weights, different layouts/depth/number of neurons/aplha, usind subset of Inputs, plotting (NeuralNetTools)
#Hyperparameter: size = number of hidden nodes, decay = alpha used in Backprogation to adjust weights
grid_nnet <- expand.grid(size = seq(from=10,to=30, by=2), decay=seq(from=0.01,to=0.31,by=0.05))
if (grid_type=='full')
{grid_nnet <- expand.grid(size =  seq(from=10,to=50, by=5), decay=seq(from=0.01,to=0.15,by=0.02))}

model_pcannet <- caret::train(Turnover~., data=train_sampled, method='pcaNNet',trControl=ctrl,tuneGrid=grid_nnet, linout=F, metric="ROC")
model_pcannet

#modelLookup(model='nnet')      # Neural Net withouht PCA -------
##Hyperparameter: size = number of hidden nodes, decay = alpha used in Backprogation to adjust weights
#grid_nnet <- expand.grid(size = seq(from=10,to=30, by=2), decay=seq(from=0.01,to=0.2,by=0.05))
#if (grid_type=='full')
#{grid_nnet <- expand.grid(size =  seq(from=10,to=50, by=5), decay=seq(from=0.01,to=0.15,by=0.02))}
#model_nnet <- caret::train(Turnover~., data=train_sampled, method='nnet',trControl=ctrl,tuneGrid=grid_nnet, linout=F, metric="ROC")




#modelLookup(model='glmStepAIC') # Generalized Linear Model with step-wise parameter selection using AIC  #--------

#model_glmStepAIC <- caret::train(Turnover~., data=train_sampled, method='glmStepAIC', trControl=ctrl, metric=chosen_measure)
#model_glmStepAIC
#summary(model_glmStepAIC)

modelLookup(model='xgbDART')    # Extreme Gradient Boosting --------
grid_xgbDART <- expand.grid(nrounds = c(50,100), max_depth = c(2,4), eta = c(0.4,0.5), gamma = 0, subsample = 0.5, colsample_bytree = c(0.8), rate_drop = c(0.01,0.1),
                            skip_drop =  c(0.8), min_child_weight = 1)
if (grid_type=='full'){
  grid_xgbDART <- expand.grid(nrounds = c(50,100,200), max_depth = c(2,4,6), eta = c(0.4,0.5), gamma = 0, subsample = 0.5, colsample_bytree = c(0.6,0.8), rate_drop = c(0.01,0.1,0.5),
                              skip_drop =  c(0.8,0.95), min_child_weight = 1)
}
model_xgbDART <- caret::train(Turnover~., data=train_sampled, method='xgbDART',tuneGrid=grid_xgbDART,trControl=ctrl, metric=chosen_measure) # tuneLength=2
model_xgbDART
plot_xgbDART <- plot(model_xgbDART)
plot_xgbDART



modelLookup(model='glmnet') # glmnet (Elastic nets) include or improve LARS, LASSO, RIDGE, STEPWISE, Full Ordinary Least Square (OLS) Models ------
grid_glmnet <- expand.grid(alpha=c(0.001, 0.005),lambda=seq(0.01,0.2,length=4))
if (grid_type=='full'){
  grid_glmnet <- expand.grid(alpha=c(0.0001,0.001, 0.005,0.01,0.1,0.95),lambda=seq(0.001,0.1,length=10))
}
model_glmnet <- caret::train(Turnover~., data=train_sampled, method='glmnet',tuneGrid=grid_glmnet,trControl=ctrl, metric="ROC")
model_glmnet


modelLookup(model='naive_bayes') # Naive Bayes --------------
grid_nb <- expand.grid(laplace=c(0,0.5,1), usekernel=c(F,T), adjust=c(0.75, 1, 1.25, 1.5))
model_nb <- caret::train(Turnover~., data=train_sampled, method='naive_bayes',tuneGrid=grid_nb,trControl=ctrl, metric=chosen_measure)
model_nb



modelLookup(model='svmLinearWeights') #--------------------
#grid_svmLinearWeights <- expand.grid(cost=c(0.01),weight = c(1))
#model_svmLinearWeights <- caret::train(Turnover~., data=train_sampled, method='svmLinearWeights',tuneGrid=grid_svmLinearWeights,trControl=ctrl, metric="ROC", preProcess= c("center", "scale"))


#modelLookup(model='svmLinear2')  #(Support Vector Machine with linear kernel) --> long training required  -------------
grid_svmLinear <- expand.grid(cost=0.1)#seq(0.0005,0.1,length=5))
model_svmLinear <- caret::train(Turnover~., data=train_sampled, method='svmLinear2',tuneGrid=grid_svmLinear,trControl=ctrl, metric="ROC", preProcess= c("center", "scale"))
model_svmLinear




#Test downsampling svmLinear --> not good accuarcy
#ctrl_down <- trainControl(method = 'repeatedcv', repeats=1, number=5, classProbs=T, summaryFunction = fullSummary, trim=F, returnData=F,sampling="down")
#grid_svmLinear <- expand.grid(cost=seq(0.005,0.1,length=10))
#model_svmLinear <- caret::train(Turnover~., data=train_sampled, method='svmLinear2',tuneGrid=grid_svmLinear,trControl=ctrl_down, metric="ROC", preProcess= c("center", "scale"))
#model_svmLinear

# svmlLinear (Support Vector Machine with linear kernel) --> NA values
#modelLookup(model='svmPoly')
#grid_svmPoly <- expand.grid(C=seq(0,2,length=3),degree=1,scale=0.01)
#model_svmPoly <- caret::train(Turnover~., data=train_sampled, method='svmPoly',tuneGrid=grid_svmPoly,trControl=ctrl, metric=chosen_measure)
#model_svmPoly

##modelLookup(model='knn')        # K-Nearest Neighbors  --------
#Hyperparameter: Number of nearest-neigbbours --> NA values

##grid_knn <- expand.grid(k=seq(from=5, to=5, by=5))
##if (grid_type=='full'){
##  grid_knn <- expand.grid(k=seq(from=5, to=25, by=5))
##}
##model_knn <- caret::train(Turnover~., data=train_sampled, method='knn', trControl=ctrl, tuneGrid=grid_knn, metric=chosen_measure)
##model_knn


# Close connection to cluster for parallel processing -------------


stopCluster(cl)
print("Model training process finished")

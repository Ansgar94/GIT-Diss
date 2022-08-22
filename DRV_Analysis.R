# Setting up model_list for further analysis of spend computing ressources and predictive power ------
model_list <- list(GeneralizedLinearModel = model_glm,
                   ClassificationTree =  model_rpart,
                   GeneralizedBoostingMachine = model_gbm, 
#                   FeedFowardNeuralNetworkNoPCA = model_nnet,
                   FeedFowardNeuralNetwork = model_pcannet,
                   RandomForest = model_rf,
#                   GeneralizedLinearModelStepwise = model_glmStepAIC,
#                   KNearestNeighbors = model_knn,
                   ElasticNetRegression = model_glmnet,
                   SupportVectorMachine = model_svmLinear,
                   NaiveBayes = model_nb,
                   ExtremeGradientBoosting = model_xgbDART
)


# Model evaluation on training data with resampling on training/validation data  --------------
trellis.par.set(caretTheme())
resampling_training <-resamples(model_list, metric="Kappa", decreasing=T)
plot_resamps_KAPPA <- dotplot(resampling_training, metric="Kappa")
plot_resamps_KAPPA
plot_resamps_ROC <- dotplot(resampling_training, metric="ROC")
plot_resamps_ROC
plot_resamps_SPEC <- dotplot(resampling_training, metric="Spec")
plot_resamps_SPEC
summary(resampling_training)$statistics


# Evaluate predictive power on test data WITHOUT confidence interval  ------------
# Calculate performance measures for each model on test data
conMA_fun <- function(model, data) {
  conMa <- confusionMatrix(test$Turnover, 
                           predict(object=model, test)) 
  conMa_measure <- c(conMa$byClass[c("Sensitivity","Specificity","Precision","Recall","F1")], conMa$overall[c("Accuracy", "Kappa")])
  conMa_measure <- c("ROC"=roc(test$Turnover, 
                               predict(object=model, test, type = "prob" )[, "No_Turnover"],
                               levels = rev(levels(test$Turnover)))$ROC
                     , conMa_measure)
  conMa_measure                      
}


# Initialize data frame to safe results
test_measures=c("ROC","Sensitivity","Specificity","Precision","Recall","F1","Accuracy", "Kappa")
test_results <-as.data.frame(matrix(0,nrow=length(model_list), ncol=length(test_measures)))
rownames(test_results) <- names(model_list)
colnames(test_results) <- test_measures



# Create performance measures on test results
test_list <- lapply(model_list, conMA_fun, test) 

# Show results
test_results <- data.frame(test_list) %>% t() %>% data.frame()
test_results 

# Evaluate predictive power on test data WITH confidence interval  ------------

# Choosing a threshold for specific goals is determined by the operational use in HR Analytics processes
# In this setting the overall goal is to retain as many employees as possible --> main goal is to avoid false-negative 
# After discussion with HR professional the threshold is therefore relatively low: 5%
# In this specific case, this low threshold might not lead to the greatest economical benefit due to high  false-negative rate
p_chosen <- 0.05

#Nevertheless experiments with the treshold are interesting to investigate model robustness
p_experiments <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.07)

for (p_experiment in p_experiments) {

# Calculate performance measures for each model on test data
Test_measures_fun <- function(model, data) {
  
  # calculate cohens Kappa on test data with confidence interval
   Kappa_measure <- confusionMatrix(
                      ifelse(
                        predict(object=model, test, type="prob")[,"Turnover"]> p_experiment, # p_chosen,
                          "Turnover", 
                          "No_Turnover") %>% 
                                      factor(),
                      test$Turnover, 
                      positive="Turnover")$table  %>% 
                        CohenKappa(.,conf.level=0.95) %>% 
                        data.frame() %>% 
                        t(.)
                
  
   # calculate Area under Curve on test data with confidence interval 
  ROC_measure <- roc(test$Turnover, 
                     predict(object=model, test, type = "prob" )[, "No_Turnover"],
                     levels = rev(levels(test$Turnover)),ci=T)$ci  %>% 
#                         data.frame(row.names = c("lwr.ci", "ROC", "upr.ci"))  %>%  
                         t(.)
  
  # bind results from Kappa and Area under curve 
  Test_measures <- c(Kappa_measure, ROC_measure)
  Test_measures
}


# Initialize list to safe results
result_test_ci_list <- list()

# Create performance measures on test results and store in list
result_test_ci_list <- lapply(model_list, Test_measures_fun, test) 

# Combine list elements to dataframe (plotting preparation)
result_test_ci_df <- do.call(rbind.data.frame,result_test_ci_list)  
result_test_ci_df$method <- names(model_list) %>% factor()
colnames(result_test_ci_df) <- c("kappa", "kappa.lwr.ci", "kappa.upr.ci", "ROC.lwr.ci", "ROC", "ROC.upr.ci","method")
result_test_ci_df

# Plot performance on test data measured by cohens kappa with ci
plot_test_kappa_ci <- ggplot(aes(y=reorder(method,kappa),x=kappa),data=result_test_ci_df[order(-result_test_ci_df$kappa),])+
  geom_point(size=2)+
  geom_errorbar(aes(xmin=kappa.lwr.ci,xmax=kappa.upr.ci),width=.1)+
  theme_classic()
plot_test_kappa_ci

assign(paste("plot_test_kappa_ci_",p_experiment,  sep="",collapse = NULL),plot_test_kappa_ci)
}

# Plot performance on test data measured by ROC 
plot_test_ROC_ci <- ggplot(aes(y=reorder(method,ROC),x=ROC),data=result_test_ci_df[order(-result_test_ci_df$ROC),])+
  geom_point(size=2)+
  geom_errorbar(aes(xmin=ROC.lwr.ci,xmax=ROC.upr.ci),width=.1)+
  theme_classic()
plot_test_ROC_ci


# Plot results of winner (ROC) in ROC curve and give out confusion matrix and final (global) variable Importance ------------

# Choose winner based on Kappa-Score on training data
model_win <- summary(resampling_training)$statistics$Kappa[,'Mean'] %>% 
               which.max() %>% 
               names() %>% 
               model_list[[.]]
# alternatively choose best model on test data:  model_win <- model_list[[rownames(test_results[which.max(test_results$ROC),])]]
# model_win <- model_list[['ExtremeGradientBoosting']]
# model_win <- model_list[['RandomForest']]
print("The best model on test data is: ")
model_win

# Final confusion matrix with default threshold p 
conMaWin_default_p <- confusionMatrix(predict(object=model_win, test),
                            test$Turnover) 
conMaWin_default_p


# Further analysis and ROC Curve of winner model  ----------------
prob <- predict(object=model_win, test, type="prob")

# Print full confusion matrix of winner model
conMa_p_chosen <- confusionMatrix(ifelse(prob[,"Turnover"]>p_chosen, "Turnover", "No_Turnover") %>% factor(),
                test$Turnover, 
                positive="Turnover")
conMa_p_chosen


# Relative Confusion matrix of winner model
confusionMatrix(ifelse(prob[,"Turnover"]>p_chosen, "Turnover", "No_Turnover") %>% factor(),
                test$Turnover, 
                positive="Turnover")$table/nrow(test)*100 %>% round(.,4)

# Monthly confusion matrix (devided by 6 due to 6 month in test data) of winner model
confusionMatrix(ifelse(prob[,"Turnover"]>p_chosen, "Turnover", "No_Turnover") %>% factor(),
                test$Turnover, 
                positive="Turnover")$table/6 %>% round(.,0)


# Plot ROC Curve of winning model of winner model
rocCurve <- roc(response = test[,'Turnover'],
                  predictor = predict.train(object=model_win, test, type="prob")$Turnover,
                  levels = rev(levels(test[,'Turnover'])))
plot(rocCurve)


# Further confusion matrix tests: Time Horizon and other thresholds (NOT NEEDED FOR PUPLICATION) ---------------


# Set own list of thresholds p
thresholds_p = c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.1,0.5)
prob <- predict(object=model_win, test, type="prob")

# Open list of to safe several confusion matrices 
conMalist <- list()
conMalist_rel <- list()
# Create several confusion matrices according to threshold
for(p in thresholds_p){
  conMalist[[paste0("ConMa_p_",p)]] <- confusionMatrix(ifelse(prob[,"Turnover"]>p, "Turnover", "No_Turnover") %>% factor(),
                                                       test$Turnover, 
                                                       positive="Turnover")$table
  conMalist_rel[[paste0("ConMa_p_",p)]] <- confusionMatrix(ifelse(prob[,"Turnover"]>p, "Turnover", "No_Turnover") %>% factor(),
                                                           test$Turnover, 
                                                           positive="Turnover")$table/nrow(test)*100 %>% round(.,4)
}
conMalist_rel


# Compare Performance during different month on test data
conMalist_month <- list()
for(i in seq(1,6)){
  conMalist_month[[paste0("ConMa_",i)]] <- confusionMatrix(
    ifelse(prob[seq(1+(i-1)*floor(nrow(test)/6),i*floor(nrow(test)/6)),"Turnover"]>p_chosen,
           "Turnover", 
           "No_Turnover") 
    %>% factor(),
    test[seq(1+(i-1)*floor(nrow(test)/6),i*floor(nrow(test)/6)),"Turnover"], 
    positive="Turnover")$table/nrow(test)*100 %>% round(.,4)
}

# Calculating real values taking into in account that 6 month are analyzed
for (conMa in conMalist) {  
  print(round(conMa/6,0))
}


# Collect times spend for hyper parameterization in  parallel computing ------

# Safe computation time for all models in a data frame
df_times <- data.frame()
for (element in model_list) {
  df_times <- rbind(df_times,element$times$everything)
}

# Delete unnecessary data
df_times[,4:5] <- NULL
rownames(df_times) <- names(model_list)
colnames(df_times) <- c('User','System','Overall')

# Preparing df_tune_plot to allow plotting (melting)
df_times[,'Method'] <- factor(names(model_list))
df_times 

# Plotting results by using different graphs for each reason time is comsumted (User, System, verstrichen)
#plot_times <- reshape2::melt(df_times, id.vars='Method', variable.name='Computing',value.name='Time') %>% 
#  ggplot(aes(x=Computing,y=Time,fill=Method))+
#geom_bar(stat='identity',position='dodge')+
#  scale_fill_manual(values = c('darkblue', 'darkred','darkgreen','darkgrey','darkorange','black','cyan4'))+
#  theme_light()+
#  facet_wrap(~ Computing, scales = 'free', nrow = 1) 
#plot_times



#Preparing results of prediction performance on Test data for plotting as a melted Version with factors
#test_results[,'model'] <- factor(names(model_list))
#
#plot_results <- melt(test_results, id.vars='model', variable.name='measure',value.name='value') %>% 
#  ggplot(aes(x=measure,y=value,fill=model))+
#  geom_bar(stat='identity',position='dodge')+
#  theme_light()+
#  facet_wrap(~ measure, scales = 'free', nrow = 1) 
#plot_results#

# Plot results on test data vs. time needed to train
#df_acc_vs_time <- data.frame("Method" = df_times$Method, 
#                             "Minutes_spend_for_Training" = df_times$Overall/60, 
#                             "Kappa" = test_results$Kappa)
#df_acc_vs_time






# save results on test data ------------
#write.csv(test, stringr::str_c(dirname(getwd()),'/Results/',model_win$method,'_',chosen_measure,'_',nrow(train_sampled),'.csv'))
#write.csv(test_results, stringr::str_c(dirname(getwd()),'/Results/',model_win$method,'_',chosen_measure,'_',nrow(train_sampled),'.csv'))
#write.csv(conMalist_rel, stringr::str_c(dirname(getwd()),'/Results/',model_win$method,'_',chosen_measure,'_',nrow(train_sampled),'conMA.csv'))

#print("Inference on test data finished")


# Extreme p=0.01: try to find every employee churning, accepting high FP rate
plot_test_kappa_ci_0.01

# Extreme p=0.01: try to find every employee churning, accepting high FP rate
plot_test_kappa_ci_0.02

# Extreme p=0.01: try to find every employee churning, accepting high FP rate
plot_test_kappa_ci_0.03

# Extreme p=0.05: try to find many employee churning, restricting FP rate
plot_test_kappa_ci_0.04

# Extreme p=0.05: try to find many employee churning, restricting FP rate
plot_test_kappa_ci_0.05

# Extreme p=0.06: try to find many employee churning, restricting FP rate
plot_test_kappa_ci_0.06

# Extreme p=0.07: try to find many employee churning, restricting FP rate
plot_test_kappa_ci_0.07

# Extreme p=0.07: try to find many employee churning, restricting FP rate
plot_test_ROC_ci

# Setting up model_list for further analysis of spend computing ressources and predictive power ------
model_list <- list(GeneralizedLinearModel = model_glm,
                   DecisionTree =  model_rpart,
                   GeneralizedBoostingMachine = model_gbm, 
                   NeuralNetwork = model_nnet,
                   RandomForest = model_rf, 
                   GeneralizedLinearModelStepwise = model_glmStepAIC,
#                   KNearestNeighbors = model_knn,
                   ElasticNetRegression = model_glmnet,
                   ExtremeGradientBoosting = model_xgbDART)
model_list

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
plot_times <- reshape2::melt(df_times, id.vars='Method', variable.name='Computing',value.name='Time') %>% 
  ggplot(aes(x=Computing,y=Time,fill=Method))+
  geom_bar(stat='identity',position='dodge')+
  #  scale_fill_manual(values = c('darkblue', 'darkred','darkgreen','darkgrey','darkorange','black','cyan4'))+
  theme_light()+
  facet_wrap(~ Computing, scales = 'free', nrow = 1) 
plot_times

# Evaluate predictive power  using multiple measures ------------

# Model evaluation on training data with resampling on training data
trellis.par.set(caretTheme())
resampling_training <-resamples(model_list, metric="Kappa", decreasing=T)
plot_resamps <- dotplot(resampling_training, metric="Kappa")
plot_resamps
summary(resampling_training)$statistics$Kappa


# Calculate performance measures for each model on test data
conMA_fun <- function(model, data) {
  conMa <- confusionMatrix(test$Turnover, 
                           predict(object=model, test)) 
  conMa_measure <- c(conMa$byClass[c("Sensitivity","Specificity","Precision","Recall","F1")], conMa$overall[c("Accuracy", "Kappa")])
  conMa_measure <- c("ROC"=roc(test$Turnover, 
                               predict(object=model, test, type = "prob" )[, "No_Turnover"],
                               levels = rev(levels(test$Turnover)))$auc
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
print("rose_test_results: ")
test_results 




# Plot results after inference on test data  -------
#Preparing results of prediction performance on Test data for plotting as a melted Version with factors
test_results[,'model'] <- factor(names(model_list))

plot_results <- melt(test_results, id.vars='model', variable.name='measure',value.name='value') %>% 
  ggplot(aes(x=measure,y=value,fill=model))+
  geom_bar(stat='identity',position='dodge')+
  theme_light()+
  facet_wrap(~ measure, scales = 'free', nrow = 1) 
plot_results

# Plot results on test data vs. time needed to train
df_acc_vs_time <- data.frame("Method" = df_times$Method, 
                             "Minutes_spend_for_Training" = df_times$Overall/60, 
                             "Kappa" = test_results$Kappa)
df_acc_vs_time


# Plot results of winner (ROC-AUC) in ROC curve and give out confusion matrix and final (global) variable Importance ------------


test_results
# Choose winner based on Kappa-Score on training data(harmonized)
model_win <- summary(resampling_training)$statistics$F[,'Mean'] %>% 
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




# Set own list of thresholds p
thresholds_p = c(0.01,0.015,0.02,0.03,0.04,0.05,0.075,0.1,0.15,0.2,0.25,0.3,0.4,0.5)
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


# Choosing threshold for further analysis ----------------
# Choosing a threshold for specific goal, e.g.: finding 50% of positives aspects with p_choosen 0.0585
p_choosen <- 0.0585


confusionMatrix(ifelse(prob[,"Turnover"]>p_choosen, "Turnover", "No_Turnover") %>% factor(),
                test$Turnover, 
                positive="Turnover")$table/nrow(test)*100 %>% round(.,4)

confusionMatrix(ifelse(prob[,"Turnover"]>p_choosen, "Turnover", "No_Turnover") %>% factor(),
                test$Turnover, 
                positive="Turnover")$table/6 %>% round(.,0)
# Print full confusion matrix

conMa_p_choosen <- confusionMatrix(ifelse(prob[,"Turnover"]>p_choosen, "Turnover", "No_Turnover") %>% factor(),
                test$Turnover, 
                positive="Turnover")
conMa_p_choosen

# Checking conMa over time (per month i)
conMalist_month <- list()
for(i in seq(1,6)){
  conMalist_month[[paste0("ConMa_",i)]] <- confusionMatrix(
            ifelse(prob[seq(1+(i-1)*floor(nrow(test)/6),i*floor(nrow(test)/6)),"Turnover"]>p_choosen,
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
#42*0.25*2*50.000 - 263 * 1.000 #- 264 * 0.1 * 1000  # value creation using assumptions of value propositions

# create ROC Curve and plot results
rocCurve <- roc(response = test[,'Turnover'],
                  predictor = predict.train(object=model_win, test, type="prob")$No_Turnover,
                  levels = rev(levels(test[,'Turnover'])))
plot(rocCurve)

# save results on test data ------------
write.csv(test, stringr::str_c(getwd(),'/Results/',model_win$method,'_',chosen_measure,'_',nrow(train_sampled),'.csv'))
write.csv(test_results, stringr::str_c(getwd(),'/Results/',model_win$method,'_',chosen_measure,'_',nrow(train_sampled),'.csv'))
write.csv(conMalist_rel, stringr::str_c(getwd(),'/Results/',model_win$method,'_',chosen_measure,'_',nrow(train_sampled),'conMA.csv'))

print("Inference on test data finished")
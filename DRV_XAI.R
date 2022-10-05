# Set up Interpretable Machine Learning (IML) Framework  ------------------
# Focus on relevant entities: most 1.000 relevant elements according to highest predicted probability
df_xai <- test[,which(!names(test) %in% c("Monat"))] %>% 
  mutate(Turnover_prob = predict(object=model_win, test, type = "prob" )[, "Turnover"])  %>%  
  top_n(1000, Turnover_prob) %>% 
  .[order(.[,"Turnover_prob"], decreasing = T),]

df_xai_X <- df_xai[,which(!names(df_xai) %in% c("Turnover","Monat","Turnover_prob"))]

# ALTERNATIVE CODE
# Turnover_high <- predict(model_win, test, type = "prob" )[,"Turnover"] %>% as.vector() %>% sort(.,index.return=T)
# XAI_examples <- Turnover_high[["ix"]] %>%  tail(.,Nr_XAI_examples) %>% test[.,]  %>%  .[which(names(test) != "Turnover")]


# Opening IML predictor object
IML_Predictor <- Predictor$new(model=model_win, 
                               data=df_xai_X, 
                               y=df_xai[,'Turnover'],
                               class='Turnover') # needed for SHAP Values!, not working for FeatureImp

IML_Predictor_2_class <- Predictor$new(model=model_win, 
                                       data=df_xai_X, 
                                       y=df_xai[,'Turnover']) #y=df_xai[,'Turnover_prob'],class = 'Turnover',type = "prob"

# Use multiple methods of IML Framework to analyze feature impact/effect/interaction on global level  ------------------


# Feature Importance
# We can measure how important each feature was for the predictions with FeatureImp. 
# The feature importance measure works by shuffling each feature and measuring how much the performance drops. 
# For this classification task we choose to measure the loss in performance with classification error (ce) as information criteria
# ce is defined as the proportion of elements in actual that are not equal to the corresponding element in predicted.

imp <- FeatureImp$new(IML_Predictor_2_class, loss = "ce")
plot(imp)
#imp$results

# Alternative vib variable importance plot (attention: not available for all machine learning models)
# vip(model_win, method="model",measure="ROC")

# Measure interactions
# We can also measure how strongly features interact with each other. The interaction measure regards how much of the variance of f(x)
# is explained by the interaction. The measure is between 0 (no interaction) and 1 (= 100% of variance of f(x)
# due to interactions). For each feature, we measure how much they interact with any other feature:
interact <- Interaction$new(IML_Predictor)
plot(interact)


# Accumulated Local Effects (=ALE) ------------
# ALE describe how features influence the prediction of a machine learning model on average. 
# ALE plots are a faster and unbiased alternative to partial dependence plots (PDPs).
# Besides knowing which features were important, we are interested in how the features influence the predicted outcome. 
# The FeatureEffect class implements accumulated local effect plots, partial dependence plots and individual conditional expectation curves. 
# (little or no points mean that we should not over-interpret this region).
#ale <- FeatureEffect$new(IML_Predictor, feature="Length_of_service_in_month") 
#ale$plot()

# Matrix of all Feature Effects using Partial Dependence Plot
#PDP_ICE_plot <- FeatureEffects$new(IML_Predictor,method = 'pdp+ice' )
#plot(PDP_ICE_plot)


# Matrix of all Feature Effects using ALE
#ALE_plot <- FeatureEffects$new(IML_Predictor)
#plot(ALE_plot)


# Individual ALE plot for interesting features varying with number of intervals

# ALE Gender
if (!which(names(df_xai_X)=="A1_Gender") %>% is_empty() ) {
  ALEPlot(df_xai_X,
          model_win, J=which(names(df_xai_X)=="A1_Gender"), K=10, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
}


# Prepare seperate datasets for men and woman
df_xai_X_men <- df_xai_X %>% filter(A1_Gender=="men")
df_xai_X_woman <- df_xai_X %>% filter(A1_Gender=="woman")


plot_ALE_fun <- function() {

# Create Layout for continuous variables
par(mfrow=c(3,4),yaxt="s",ann=F,mar = c(3, 3, 2 ,1),fg='black',bty='n')


# ALE Commute_distance_in_km
par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
ALEPlot(df_xai_X %>% filter(between(B1_Commute_distance_in_km,0,100)),
        model_win, J=which(names(df_xai_X)=="B1_Commute_distance_in_km"), K=3, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",lwd=1)
par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
ALEPlot(df_xai_X_men%>% filter(between(B1_Commute_distance_in_km,0,100)),
        model_win, J=which(names(df_xai_X)=="B1_Commute_distance_in_km"), K=3, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
ALEPlot(df_xai_X_woman %>% filter(between(B1_Commute_distance_in_km,0,100)),
        model_win, J=which(names(df_xai_X)=="B1_Commute_distance_in_km"), K=3, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
title("B1_Commute_distance_in_km",ylab=NULL)

# ALE Public service status
if (!which(names(df_xai_X)=="B2_Public_service_status_GER") %>% is_empty() ) {
  par(new=F,xaxt='s',yaxt='s',col='red',fg='red',usr=c(0,1,-0.06,0.6))
  ALEPlot(df_xai_X_woman,
          model_win, J=which(names(df_xai_X)=="B2_Public_service_status_GER"), K=2, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  title("B2_Public_service_status_GER",ylab=NULL)
  abline(h=0,col="lightgray",lwd=1)
  par(new=F,xaxt='s',yaxt='s',col='black',fg='black')
#  title("B2_Public_service_status_GER",ylab=NULL)
#  par(new=F,xaxt='s',yaxt='s',col='blue',fg='blue')
#  ALEPlot(df_xai_X_men,
#          model_win, J=which(names(df_xai_X)=="B2_Public_service_status_GER"), K=2, 
#          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
#  abline(h=0,col="lightgray",)
#  title("B2_Public_service_status_GER",ylab=NULL)
#  par(new=F,xaxt='s',yaxt='s',col='black',fg='black')
#  ALEPlot(df_xai_X,
#          model_win, J=which(names(df_xai_X)=="B2_Public_service_status_GER"), K=2, 
#          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
#  abline(h=0,col="lightgray",)
#  title("B2_Public_service_status_GER",ylab=NULL)
}



# ALE Sickness days
par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="B4_Sickness_days"), K=15, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",lwd=1)
par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
ALEPlot(df_xai_X_men,
        model_win, J=which(names(df_xai_X)=="B4_Sickness_days"), K=15, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
ALEPlot(df_xai_X_woman,
        model_win, J=which(names(df_xai_X)=="B4_Sickness_days"), K=15, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
title("B4_Sickness_days",ylab=NULL)


# ALE Net_working_days
par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="B8_Net_working_days"), K=20, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",lwd=1)
par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
ALEPlot(df_xai_X_men,
        model_win, J=which(names(df_xai_X)=="B8_Net_working_days"), K=20, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
ALEPlot(df_xai_X_woman,
        model_win, J=which(names(df_xai_X)=="B8_Net_working_days"), K=20, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
title("B8_Net_working_days",ylab=NULL)


# ALE Age
if (!which(names(df_xai_X)=="A2_Age") %>% is_empty() ) {
  par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
  ALEPlot(df_xai_X %>% filter(between(A2_Age,16,61)),
          model_win, J=which(names(df_xai_X)=="A2_Age"), K=15, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  abline(h=0,col="lightgray",lwd=1)
  par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
  ALEPlot(df_xai_X_men %>% filter(between(A2_Age,16,61)),
          model_win, J=which(names(df_xai_X)=="A2_Age"), K=15, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
  ALEPlot(df_xai_X_woman %>% filter(between(A2_Age,16,61)),
          model_win, J=which(names(df_xai_X)=="A2_Age"), K=15, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  title("A2_Age",ylab=NULL)
}


# ALE Salary_increase_last_5_years --> not robust!
par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
ALEPlot(df_xai_X %>% filter(between(B12_Salary_increase_last_5_years,-0.1,0.25)),
        model_win, J=which(names(df_xai_X)=="B12_Salary_increase_last_5_years"), K=5, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",lwd=1)
par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
ALEPlot(df_xai_X_men %>% filter(between(B12_Salary_increase_last_5_years,-0.1,0.25)),
        model_win, J=which(names(df_xai_X)=="B12_Salary_increase_last_5_years"), K=5, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
ALEPlot(df_xai_X_woman %>% filter(between(B12_Salary_increase_last_5_years,-0.1,0.25)),
        model_win, J=which(names(df_xai_X)=="B12_Salary_increase_last_5_years"), K=5, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
title("B12_Salary_increase_last_5_years",ylab=NULL)


# ALE Salary_today_in_EUR
par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
ALEPlot(df_xai_X %>% filter(between(B11_Salary_today_EUR,2000,6000)),
        model_win, J=which(names(df_xai_X)=="B11_Salary_today_EUR"), K=100, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",lwd=1)
par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
ALEPlot(df_xai_X_men %>% filter(between(B11_Salary_today_EUR,2000,6000)),
        model_win, J=which(names(df_xai_X)=="B11_Salary_today_EUR"), K=100, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
ALEPlot(df_xai_X_woman %>% filter(between(B11_Salary_today_EUR,2000,6000)),
        model_win, J=which(names(df_xai_X)=="B11_Salary_today_EUR"), K=100, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
title("B11_Salary_today_EUR",ylab=NULL)


# ALE Education level
if (!which(names(df_xai_X)=="A6_Education_level") %>% is_empty() ) {

  
  par(new=F,xaxt='n',yaxt='s',col='blue',fg="blue")
 # title("A6_Education_level",ylab=NULL)
  ALEPlot(df_xai_X_men,
          model_win, J=which(names(df_xai_X)=="A6_Education_level"), K=2, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  abline(h=0,col="lightgray",lwd=1)
  title("A6_Education_level",ylab=NULL)
  par(new=F,xaxt='n',yaxt='s',col='black',fg="black")
#  title("A6_Education_level",ylab=NULL)
#  par(new=F,xaxt='n',yaxt='s',col='red',fg="red")
#  ALEPlot(df_xai_X_woman,
#          model_win, J=which(names(df_xai_X)=="A6_Education_level"), K=2, 
#          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
#  abline(h=0,col="lightgray",lwd=1)
#  title("A6_Education_level",ylab=NULL)
#  par(new=F,xaxt='n',yaxt='s',col='black',fg="black")
#  ALEPlot(df_xai_X,
#          model_win, J=which(names(df_xai_X)=="A6_Education_level"), K=2, 
#          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
#  abline(h=0,col="lightgray",lwd=1)
}

# ALE Tenure_in_month
if (!which(names(df_xai_X)=="B10_Tenure_in_month") %>% is_empty()  ) {
  par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
  ALEPlot(df_xai_X,
          model_win, J=which(names(df_xai_X)=="B10_Tenure_in_month"), K=50, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  abline(h=0,col="lightgray",lwd=1)
  par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
  ALEPlot(df_xai_X_men,
          model_win, J=which(names(df_xai_X)=="B10_Tenure_in_month"), K=20, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
  ALEPlot(df_xai_X_woman,
          model_win, J=which(names(df_xai_X)=="B10_Tenure_in_month"), K=50, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  title("B10_Tenure_in_month")
}


# ALE Salary_increase_last_year --> not robust!
par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
ALEPlot(df_xai_X %>% filter(between(B9_Salary_increase_last_year,0,0.15)),
        model_win, J=which(names(df_xai_X)=="B9_Salary_increase_last_year"), K=30, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",lwd=1)
par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
ALEPlot(df_xai_X_men %>% filter(between(B9_Salary_increase_last_year,0,0.15)),
        model_win, J=which(names(df_xai_X)=="B9_Salary_increase_last_year"), K=30, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
ALEPlot(df_xai_X_woman %>% filter(between(B9_Salary_increase_last_year,0,0.15)),
        model_win, J=which(names(df_xai_X)=="B9_Salary_increase_last_year"), K=30, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
title("B9_Salary_increase_last_year",ylab=NULL)


# ALE Degree_of_employment
par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
ALEPlot(df_xai_X %>% filter(between(B5_Degree_of_employment,10,100)),
        model_win, J=which(names(df_xai_X)=="B5_Degree_of_employment"), K=15, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",lwd=1)
par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
ALEPlot(df_xai_X_men %>% filter(between(B5_Degree_of_employment,10,100)),
        model_win, J=which(names(df_xai_X)=="B5_Degree_of_employment"), K=15, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
ALEPlot(df_xai_X_woman %>% filter(between(B5_Degree_of_employment,10,100)),
        model_win, J=which(names(df_xai_X)=="B5_Degree_of_employment"), K=15, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
title("B5_Degree_of_employment",ylab=NULL)

# ALE Number_of_children
par(new=F,xaxt='s',yaxt='s',col=rgb(169/255,169/255,169/255,alpha = 0.9),lwd=5)
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="A3_Number_of_children"), K=100, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",lwd=1)
par(new=T,xaxt='n',yaxt='n',col='blue',bty='n',lwd=1)
ALEPlot(df_xai_X_men,
        model_win, J=which(names(df_xai_X)=="A3_Number_of_children"), K=100, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
par(new=T,xaxt='n',yaxt='n',col='red',lwd=1)
ALEPlot(df_xai_X_woman,
        model_win, J=which(names(df_xai_X)=="A3_Number_of_children"), K=100, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
title("A3_Number_of_children",ylab=NULL)
}

plot_ALE_fun()

# Training of ALE below TOP 12 (inactive) -------------
ALE_train_other_ALE<- function() {

# ALE Salary_increase_last_year --> not robust!
ALEPlot(df_xai_X_woman %>% filter(between(B9_Salary_increase_last_year,-0.1,0.2)),
        model_win, J=which(names(df_xai_X)=="B9_Salary_increase_last_year"), K=20, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B9_Salary_increase_last_year",ylab=NULL)


# ALE Early_retirement_rate
ALEPlot(df_xai_X_woman ,
        model_win, J=which(names(df_xai_X)=="B3_Early_retirement_rate"), K=20, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B3_Early_retirement_rate",ylab=NULL)

# ALE Vacation_days
ALEPlot(df_xai_X ,
        model_win, J=which(names(df_xai_X)=="B7_Vacation_days"), K=5, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B7_Vacation_days",ylab=NULL)


# ALE Unemployment_rate_ger
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="D1_Unemployment_rate_GER"), K=2, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("D1_Unemployment_rate_GER",ylab=NULL)


# ALE Age_youngest_children
if (!which(names(df_xai_X)=="A5_Age_youngest_children") %>% is_empty() ) {
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="A5_Age_youngest_children"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  abline(h=0,col="lightgray",)
  title("A5_Age_youngest_children",ylab=NULL)



# ALE Gross_working_days
ALEPlot(df_xai_X ,
        model_win, J=which(names(df_xai_X)=="B6_Gross_working_days"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B6_Gross_working_days",ylab=NULL)

# ALE Covid_Google_Trends
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="E3_Covid_Google_trends_GER"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("E3_Covid_Google_trends_GER",ylab=NULL)

# ALE Covid_Cases_ger
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="E2_Covid_cases_GER"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("E2_Covid_cases_GER",ylab=NULL)

# ALE Covid_Stringency_Index
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="E1_Covid_strigency_index_GER"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("E1_Covid_strigency_index_GER",ylab=NULL)

}

}


# Second order ALE/PDP Interaction

#ALEPlot(df_xai_X,
#        model_win, J=c(12,11), K=20, 
#        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))


# Surrogate model
#We take the predictions of the black box model (in our case the random forest) and train a decision tree on the original features and 
#the predicted outcome. The plot shows the terminal nodes of the fitted tree. The maxdepth parameter controls how deep the tree can grow and 
#therefore how interpretable it is.
#TreeSurrogate <- TreeSurrogate$new(IML_Predictor, maxdepth = 3) --> Error: attempt to apply non-function
#plot(TreeSurrogate)
#TreeSurrogate$r.squared
#?TreeSurrogate



# SHAPley (SHapley Additive exPlanations) ------------
#An alternative for explaining individual predictions is a method from coalitional game theory named Shapley value. 
#Assume that for one data point, the feature values play a game together, in which they get the prediction as a payout. 
#The Shapley value tells us how to fairly distribute the payout among the feature values.
SHAPley.explain <- list()
SHAPley.plot <- list()


for (i in 1:200){
  #calculate SHAP values
  SHAPley.explain[[i]] <- Shapley$new(IML_Predictor, 
                                      x.interest = df_xai_X[i,])
  
  #create plot SHAP values
  SHAPley.plot[[i]] <- SHAPley.explain[[i]]$results %>% 
    .[order(abs(.$phi), decreasing = TRUE), ] %>% 
    .[abs(.$phi)>0.02, ] %>% 
    ggplot(aes(phi, reorder(feature.value, phi), color = phi < 0)) +
    geom_bar(stat="identity",fill="transparent",size=1.2) + 
    theme_classic() + 
    theme(plot.title=element_text(size=10))+
    labs(x="Explanation Contribution", y="Feature and Value") +
    scale_color_discrete(name="Direction", labels=c("increase","decrease"))+
    ggtitle(paste("Prob(RR):", 
                  toString(round(df_xai[i, "Turnover_prob"],2)), 
                  "|Prob(SHAP):", 
                  SHAPley.explain[[i]]$results %>%
                    top_n(10, wt = abs(phi)) %>%
                    .$"phi" %>% 
                    sum() %>% 
                    round(.,digits=3)))
}



# plot examples in a 2X2 grid
xai_plot <- do.call("grid.arrange",c(SHAPley.plot[1], #1 --> example 1
                                     SHAPley.plot[88], 
                                     SHAPley.plot[63],
                                     SHAPley.plot[65],
                                     SHAPley.plot[72], #72 --> example 2
                                     SHAPley.plot[92])) 
#1 88 low degree of employement, early retirement, net=0 
#6  Net = 0 / Age youngest children = 33
#9 19 25 26 49  66 70 full sickness and/or net = 0, manager
#12 new manager
#63 65 72 Mother of young child with salary and salary increase, networking days = 0 (Prob check 0.58 vs. 0.83)
#64 67 68 69 shortly before pension (63+)
#77 manager and parent of young child
#92 113 150 Balanced contributions, low turnover probability, sick mother of young child but gut salary (increase)


# Print results in console
SHAPley.explain[[1]]$results

# find relevant examples
xai_examples <-list()
for (i in 1:100){ 
  #calculate SHAP values
  ifelse (
  'A3_Number_of_children=0' %in% SHAPley.explain[[i]]$results$feature.value
  | !('A4_Children_under_18_years=0' %in% SHAPley.explain[[i]]$results$feature.value),
#  | ('B1_Commute_distance_in_km=14' %in% SHAPley.explain[[i]]$results$feature.value),
  xai_examples [[i]] <- "bad",
  xai_examples [[i]] <- "good")
}

xai_examples[1:100]

# SHAP using fastshap package

#fastshap <- fastshap::explain(model_win, 
#                              X=df_xai_X,
#                              pred_wrapper=function(X.model, newdata) as.numeric(predict(X.model, newdata)))

#autoplot(fastshap)
#autoplot(fastshap,type="dependence",feature="Home_workplace_distance",X=df_xai_X) 
#autoplot(fastshap,type="contribution",row_num=50,X=df_xai_X) 


#possible with external factors: 91

# Classification Based on Association Rules Algorithm 
#library(arulesCBA)
#library(arules)
#library(arulesViz)

#CBA_model <- CBA(Turnover~.,df_xai[,which(!names(df_xai) %in% c("Monat","Turnover_prob"))])

#inspectDT(CBA_model$rules)

# Use post-hoc explanations from LIME to explain the Black Box on local level   -----------


# LIME (Local Interpretable Model-agnostic Explanations) 

# Global surrogate model can improve the understanding of the global model behaviour. 
# We can also fit a model locally to understand an individual prediction better. 
# The local model fitted by LocalModel is a linear regression model and the data points are weighted by how close they are to the data point 
# for wich we want to explain the prediction

lime.explain <- list()

for (i in 1:10){
  lime.explain[[i]] <- LocalModel$new(IML_Predictor, k=ncol(df_xai)-2, x.interest = df_xai[i,])
}

lime.explain[[1]]$plot()
lime.explain[1]$results

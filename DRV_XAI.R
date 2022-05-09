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
ALE_plot <- FeatureEffects$new(IML_Predictor)
plot(ALE_plot)


# Individual ALE plot for interesting features varying with number of intervals

# ALE Gender
if (!which(names(df_xai_X)=="A1_Gender") %>% is_empty() ) {
  ALEPlot(df_xai_X,
          model_win, J=which(names(df_xai_X)=="A1_Gender"), K=10, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
}


# Create Layout for continuous variables
par(mfrow=c(3,3),yaxt="s",ann=F,mar = c(3, 3, 2 ,1) )

# ALE Commute_distance_in_km
ALEPlot(df_xai_X %>% filter(between(B1_Commute_distance_in_km,0,100)),
        model_win, J=which(names(df_xai_X)=="B1_Commute_distance_in_km"), K=4, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B1_Commute_distance_in_km",ylab=NULL)

# ALE Net_working_days
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="B8_Net_working_days"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B8_Net_working_days",ylab=NULL)

# ALE Salary_today_in_EUR
ALEPlot(df_xai_X %>% filter(between(B11_Salary_today_EUR,1000,6000)),
        model_win, J=which(names(df_xai_X)=="B11_Salary_today_EUR"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B11_Salary_today_EUR",ylab=NULL)


# ALE Tenure_in_month
if (!which(names(df_xai_X)=="B10_Tenure_in_month") %>% is_empty() ) {
  ALEPlot(df_xai_X,
          model_win, J=which(names(df_xai_X)=="B10_Tenure_in_month"), K=10, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  abline(h=0,col="lightgray",)
  title("B10_Tenure_in_month",ylab=NULL)
}

# ALE Salary_increase_last_year --> not robust!
ALEPlot(df_xai_X %>% filter(between(B9_Salary_increase_last_year,-0.1,0.2)),
        model_win, J=which(names(df_xai_X)=="B9_Salary_increase_last_year"), K=20, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B9_Salary_increase_last_year",ylab=NULL)

# ALE Sick_days
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="B4_Sickness_days"), K=20, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B4_Sickness_days",ylab=NULL)


# ALE Age
if (!which(names(df_xai_X)=="A2_Age") %>% is_empty() ) {
  ALEPlot(df_xai_X %>% filter(between(B9_Salary_increase_last_year,20,65),
          model_win, J=which(names(df_xai_X)=="A2_Age"), K=10, 
          pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
  abline(h=0,col="lightgray",)
  title("A2_Age",ylab=NULL)
}


# ALE Number_of_children
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="A3_Number_of_children"), K=50, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("A3_Number_of_children",ylab=NULL)


# ALE Degree_of_employment
ALEPlot(df_xai_X ,
        model_win, J=which(names(df_xai_X)=="B5_Degree_of_employment"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B5_Degree_of_employment",ylab=NULL)

# ALE Early_retirement_rate
ALEPlot(df_xai_X ,
        model_win, J=which(names(df_xai_X)=="B3_Early_retirement_rate"), K=20, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B3_Early_retirement_rate",ylab=NULL)

# ALE Vacation_days
ALEPlot(df_xai_X ,
        model_win, J=which(names(df_xai_X)=="B7_Vacation_days"), K=10, 
        pred.fun=function(X.model, newdata) as.numeric(predict(X.model, newdata)))
abline(h=0,col="lightgray",)
title("B7_Vacation_days",ylab=NULL)


# ALE Unemployment_rate_ger
ALEPlot(df_xai_X,
        model_win, J=which(names(df_xai_X)=="D1_Unemployment_rate_GER"), K=10, 
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
}


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


# Use post-hoc explanations from LIME and SHAP to explain the Black Box on local level   -----------


# LIME (Local Interpretable Model-agnostic Explanations) 

# Global surrogate model can improve the understanding of the global model behaviour. 
# We can also fit a model locally to understand an individual prediction better. 
# The local model fitted by LocalModel is a linear regression model and the data points are weighted by how close they are to the data point 
# for wich we want to explain the prediction

lime.explain <- list()

for (i in 1:10){
  lime.explain[[i]] <- LocalModel$new(IML_Predictor, k=ncol(df_xai)-2, x.interest = df_xai[i,])
}

lime.explain[[10]]$plot()
lime.explain[[10]]$results



# SHAPley (SHapley Additive exPlanations) ------------
#An alternative for explaining individual predictions is a method from coalitional game theory named Shapley value. 
#Assume that for one data point, the feature values play a game together, in which they get the prediction as a payout. 
#The Shapley value tells us how to fairly distribute the payout among the feature values.
SHAPley.explain <- list()
SHAPley.plot <- list()

for (i in 1:200){
  #calculate SHAP values
  SHAPley.explain[[i]] <- Shapley$new(IML_Predictor, 
                                      x.interest = df_xai %>% 
                                        .[,which(!names(df_xai) %in% c("Turnover","Monat","Turnover_prob"))] %>% 
                                        .[i,], 
                                      sample.size=25) #50,200
  
  #create plot SHAP values
  SHAPley.plot[[i]]<- SHAPley.explain[[i]]$results %>%
    top_n(10, wt = abs(phi)) %>%
    ggplot(aes(phi, reorder(feature.value, phi, color = phi > 0))) +
    geom_bar(stat="identity") + 
    theme_classic() + 
    labs(x="Explanation Contribution", y="Feature and Value") +
    scale_color_discrete(name="Direction", labels=c("decrease","increase"))+
    ggtitle(paste("Prob(RR):", 
                  toString(round(df_xai[i, "Turnover_prob"],2)), 
                  "|Prob(SHAP):", 
                  SHAPley.explain[[i]]$results %>%
                    top_n(10, wt = abs(phi)) %>%
                    .$"phi" %>% 
                    sum() %>% 
                    round(.,digits=3)
                  
    ))
}


# portray examples in a grid
xai_plot <- do.call("grid.arrange",c( 
                        SHAPley.plot[62], #sick      # 63 Long commute, part time worker with young child
                        SHAPley.plot[107], #old      #  76 Full sick_days as early indicator for older employees, but not as strong in management area
                        SHAPley.plot[115], #mother   #  107 Established Manager with high Sick_Days
                        SHAPley.plot[117])) #new manager   # 116 Newly hired Manager with young children


xai_plot_test <- do.call("grid.arrange",c( 
  SHAPley.plot[175], # Long commute, part time worker with young child
  SHAPley.plot[177], # Full sick_days as early indicator for older employees
  SHAPley.plot[187], # Established Manager with high Sick_Days
  SHAPley.plot[190]))

# Print results in console
SHAPley.explain[[1]]$results

# find relevant examples
xai_examples <-list()
for (i in 1:200){ 
  #calculate SHAP values
  ifelse (
  'A3_Number_of_children=0' %in% SHAPley.explain[[i]]$results$feature.value
  & !('A4_Children_under_18_years=0' %in% SHAPley.explain[[i]]$results$feature.value)
  | ('B1_Commute_distance_in_km=14' %in% SHAPley.explain[[i]]$results$feature.value),
  xai_examples [[i]] <- "bad",
  xai_examples [[i]] <- "good")
}

xai_examples[1:200]


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

# TO-DO: Using the other packages without IML Framework (Treeshap, Anchors...) ----------
# Treeshap as optimized version for treea algorithms (Random Forest, XGBoost, etc.): https://github.com/ModelOriented/treeshap

#model_unified <- ranger.unify(model_rf)


# Using Anchors and other XAI methods, https://github.com/viadee/anchorsOnR 

#model_type("classification")

#Create Anchor explainer for test data
#anchor_explainer <- anchors(df_xai, model_win)
#?anchors()

#Use Anchor explainer on XAI_examples

#anchor_explanations <- explain(df_xai[5,],anchor_explainer, labels="Turnover")
#anchor_explanations
#printExplanations(anchor_explainer, anchor_explanations)

# TO-DO
# library(Counterfactual)
#counterfactual(Turnover~Age, df_xai, counterfactual_var=contrasts(df_xai$Turnover))
# Not Understood yet!
# Use statistical models/tests to determine statistical complexity and calculate statistical significance of attributes -------- 

# Create Linear Discriminant Analysis to determine statistical significance (compare LDA for loads)
lda_fit <- lda(Turnover ~., data = training)
lda_fit
table(predict(lda_fit,test)$class,test$Turnover)

 
# Create Quadratic Discriminant Analysis to determine statistical significance
qda_fit <- qda(Turnover ~., data = training)
qda_fit
table(predict(qda_fit,test)$class,test$Turnover)

caret::confusionMatrix()

# Create Naive Bayes to determine statistical significance 
nb_fit <- naiveBayes(Turnover ~., data = training)
nb_fit
table(predict(nb_fit,test),test$Turnover)


# Create full model with all data to determine statistical significance
# Feedback: Cautious evalution needed, due to multicollinearity in predictors and big number of observation (p-values always small)
glm_fit <- glm(Turnover ~., data = df, family=binomial)
glm_fit_scaled <- df %>% 
  preProcess(df, method=c("center","scale")) %>% 
  predict(.,df) %>% 
  glm(Turnover ~., data=., family = binomial)
summary(glm_fit_scaled)


#table(predict(glm_fit,newdata=test),test$Turnover)
#zvalues_combined = summary(glm_fit$coefficients[,4])  %>% sort()
#zvalues_combined

# Pseudo-determinism R²
# In the case of linear regression, the coefficient of determination describes the explained portion of the variability (variance) of a dependent variable  by a statistical model. 
# However, for a nominal or ordinal scale level of Y, there is no equivalent, since one would need the variance and thus a cannot be computed. Maximum log-likelihood estimation, 
# meanwhile, can be used to estimate more general regression models. For these, various pseudo-determinism measures  have been proposed.
glm_fit_ll_null <- glm_fit$null.deviance/-2
glm_fit_ll_proposed <- glm_fit$deviance/-2
glm_fit_r2 <- (glm_fit_ll_null-glm_fit_ll_proposed)/glm_fit_ll_null
glm_fit_r2

# P-value test for r² with Chi² distribution -> 0, which confirms the R² as robust
1-pchisq(2*(glm_fit_ll_proposed-glm_fit_ll_null), df=length(glm_fit$coefficients)-1)



# Feature Selection using uni variate filters with caret    
# https://topepo.github.io/caret/feature-selection-using-univariate-filters.html 
# filter <- filterVarImp(y=df[,'Turnover'],x=df[,-which(names(df)=='Turnover')],nonpara=T)
# ROC_importance <- filter[order(filter$Turnover,decreasing = T),]
# plot(ROC_importance$Turnover)
# ROC_importance ##--> Exclude Salary_today, Rel_salary_increase_1_year, Covid_Cases_ger,...?


# Multicollinearity with variance inflation factor (VIF) --> maybe exclude sick_days and Covid_Strigency_Index
vif(glm_fit)


# Comparing other statistical tests, e.g. analysis for variance with Chi² distribution vs. null model
anova(glm_fit, test="Chisq") 
# More infos for Chisq: https://towardsdatascience.com/chi-square-test-for-feature-selection-in-machine-learning-206b1f0b8223



# Plotting data, distribution and correlation -------------

# Numerical features
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=20)


# Categorial features
df %>%
  keep(is.factor) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

# Calculate imbalance of Turnover as target attribute
df %>%
  filter(Turnover=="Turnover") %>% 
  nrow()/nrow(filter(df,Turnover=="No_Turnover"))

training %>%
  filter(Turnover=="Turnover") %>% 
  nrow()/nrow(filter(training,Turnover=="No_Turnover"))

test %>%
  filter(Turnover=="Turnover") %>% 
  nrow()/nrow(filter(test,Turnover=="No_Turnover"))

# Partial correlation plot
df_protected %>%
  keep(is.numeric) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot()

# Partial correlation --> Pearson or Spearman?
#df %>%
#  keep(is.numeric) %>% 
#  partial.r(., method="pearson") 



#plot correlation for some features (pearson -> linear)
#pairs.panels(df[,c("Turnover","Age", "Net_working_hours","Length_of_service_in_month","Home_workplace_distance","Number_of_children")], 
#                         method = "pearson", # correlation method
#                         hist.col = "#00AFBB",
#                         density = TRUE,  # show density plots
#                         ellipses = TRUE) # show correlation ellipses


#plot correlation for some features (spearman -> rank monotonic)
#pairs.panels(df[,c("Turnover","Age", "Net_working_hours","Length_of_service_in_month","Home_workplace_distance")], 
#            method = "pearson", # correlation method
#            hist.col = "#00AFBB",
#            density = TRUE,  # show density plots
#            ellipses = TRUE) # show correlation ellipses)


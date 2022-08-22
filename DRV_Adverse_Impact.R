# train revised model

train_revised <- training
train_revised[,c('A2_Age',
                 'A1_Gender',
                 'B10_Tenure_in_month',
                 'A5_Age_youngest_children',
                 'A4_Children_under_18_years1')] <- NULL #Satisfied with A2, A1, B10, A5, A4

#enable parallel clustering (stop command located at the bottom of model train_sampled) --------------
cl <- makePSOCKcluster(detectCores()-1)
print(str_c('Number of CPU cores used for parallel processing is: ', detectCores()-1))  
registerDoParallel(cl)

modelLookup(model='rf')         # Random Forest -------
#Hyperparameter: mtry = Number of variables randomly sampled as candidates at each split
#these parameters are theoretically not important for accuracy (just for performance)https://stats.stackexchange.com/questions/50210/caret-and-randomforest-number-of-trees

grid_rf <- expand.grid(splitrule="gini",mtry=c(6),min.node.size=c(10)) #min.node.size=c(10) for full model, 5 revised model

if (grid_type=='full')
{grid_rf <- expand.grid(splitrule="gini",mtry=seq(from=2, to=8, by=2),min.node.size=c(3,5))}

model_rf_revised<-caret::train(Turnover~.,data=train_revised, method='ranger', trControl=ctrl, tuneGrid=grid_rf, metric=chosen_measure)
print(model_rf_revised)


stopCluster(cl)
print("Model training process finished")


# Preparing data for Adverse Impact analysis -------



# Predict turnover probabilities
df_adverse_impact <- test[,which(!names(test) %in% c("Monat"))] %>% 
  mutate(Turnover_prob = predict(object=model_rf_revised, test, type = "prob" )[, "Turnover"])#[,which(!names(test_protected) %in% c("Age","Gender"))]



# Choosing threshold for Adverse Impact analysis, which should normally be the same threshold as the p 
p_adverse_impact <- 0.05
df_adverse_impact$Turnover_pred <- ifelse(df_adverse_impact[,"Turnover_prob"]>p_adverse_impact, "Turnover", "No_Turnover") %>% factor()
df_adverse_impact$grouped_age <- ifelse(df_adverse_impact[,"A2_Age"]>median(df_adverse_impact[,"A2_Age"]), "old", "young") %>% factor()
summary(df_adverse_impact)

# Show overall confusion matrix
confusionMatrix(df_adverse_impact$Turnover_pred,
                   df_adverse_impact$Turnover, 
                   positive="Turnover")$table/nrow(df_adverse_impact)*100 %>% round(.,4)


# Aggregating according to gender for adverse impact analysis (4/5 rule)
merge(dplyr::count(df_adverse_impact, A1_Gender, Turnover), 
      dplyr::count(df_adverse_impact, A1_Gender, Turnover_pred), 
      by.x=0, by.y = 0, all=F) %>%  
  .[,c("A1_Gender.x","Turnover", "n.x", "n.y")] %>% 
  dplyr::rename(Gender=A1_Gender.x, Turnover_n = n.x, Turnover_pred = n.y) 


# Aggregating according to grouped_age for adverse impact analysis (4/5 rule)
merge(dplyr::count(df_adverse_impact, grouped_age, Turnover), 
      dplyr::count(df_adverse_impact, grouped_age, Turnover_pred), 
      by.x=0, by.y = 0, all=F) %>%  
  .[,c("grouped_age.x","Turnover", "n.x", "n.y")] %>% 
  dplyr::rename(grouped_age=grouped_age.x, Turnover_n = n.x, Turnover_pred = n.y) %>% 
  dplyr::mutate(Ration = Turnover_n/Turnover_pred)


# Adverse Impact Analysis Gender ---------
df_gender <- dplyr::count(df_adverse_impact, A1_Gender, Turnover_pred)[3:6,]
rownames(df_gender) <- 1:4
df_gender 

NT_men <-  df_gender[1,3]
T_men <-  df_gender[2,3]
NT_woman <-  df_gender[3,3]
T_woman <-  df_gender[4,3]

Ratio_men <- T_men/(NT_men+T_men)
Ratio_woman <- T_woman/(T_woman+NT_woman)
Ratio_total_gender <- (T_men + T_woman) / (NT_men+T_men+NT_woman+T_woman)

Ratio_Gender <- min(Ratio_men,Ratio_woman)/max(Ratio_men,Ratio_woman)
Ratio_Gender

# Pearson's Chi-squared test for gender
df_gender %>% 
  pivot_wider(names_from=Turnover_pred,values_from=n) %>% 
  .[,c("Turnover","No_Turnover")] %>% 
  chisq.test(.,correct=T) #%>%   .$observed

# Effect size phi for gender in chi-square test
df_gender %>% 
  pivot_wider(names_from=Turnover_pred,values_from=n) %>% 
  .[,c("Turnover","No_Turnover")] %>% 
  phi()

# Fisher Exact test for gender
df_gender %>% 
  pivot_wider(names_from=Turnover_pred,values_from=n) %>% 
  .[,c("Turnover","No_Turnover")] %>% 
  fisher.test()

# Z_ir test (Z Impact Ratio Test)
Z_ir_gender <- log(Ratio_Gender)/
  sqrt(((1-Ratio_total_gender)/Ratio_total_gender)*(1/(NT_woman+T_woman)+1/(NT_men+T_men)))
abs(Z_ir_gender)
# calculate p value for Z_ir, using more conversative one-sided p-value
pnorm(abs(Z_ir_gender), lower.tail=F)
# calculate conficende intervall
Ratio_Gender_SE <- sqrt(((1 - Ratio_woman)/(T_woman)+(1-Ratio_men)/(T_men)))
exp(log(Ratio_Gender)+1.96*Ratio_Gender_SE)
exp(log(Ratio_Gender)-1.96*Ratio_Gender_SE)

# p-values are under the conventional alpha of .05, and thus indicate that we confirm the null hypothesis 
# that there is zero relative difference between the population selection ratios for men and women.

# Adverse Impact Analysis Age ---------
df_age <- dplyr::count(df_adverse_impact, grouped_age, Turnover_pred)
df_age

NT_old <-  df_age[1,3]
T_old <-  df_age[2,3]
NT_young <-  df_age[3,3]
T_young <-  df_age[4,3]
Ratio_total_age <- (T_old + T_young) / (NT_old+T_old+NT_young+T_young)
Ratio_total_age

Ratio_old <- T_old/(NT_old+T_old)
Ratio_young <- T_young/(T_young+NT_young)

Ratio_age   <- min(Ratio_old,Ratio_young)/max(Ratio_old,Ratio_young)
Ratio_age

# Pearson's Chi-squared test for age
df_age %>% 
  pivot_wider(names_from=Turnover_pred,values_from=n) %>% 
  .[,c("Turnover","No_Turnover")] %>% 
  chisq.test(.,correct=F)

# Effect size phi for age in  chi-square test
df_age %>% 
  pivot_wider(names_from=Turnover_pred,values_from=n) %>% 
  .[,c("Turnover","No_Turnover")] %>% 
  phi()

# Fisher Exact test for age
df_age %>% 
  pivot_wider(names_from=Turnover_pred,values_from=n) %>% 
  .[,c("Turnover","No_Turnover")] %>% 
  fisher.test()

# Z_ir test (Z Impact Ratio Test)
Z_ir_age <- log(Ratio_age)/  
  sqrt(
    ((1-Ratio_total_age)/Ratio_total_age)*(1/(NT_old+T_old)+1/(NT_young+T_young))
    )
abs(Z_ir_age)
# calculate p value for Z_ir, using more conversative one-sided p-value
# null hypothesis that there is zero relative difference between the population selection ratios
pnorm(abs(Z_ir_age), lower.tail=F)

# p-values are under the conventional alpha of .05, and thus indicate that we confirm the null hypothesis 
# that there is zero relative difference between the population selection ratios for men and women.

# calculate confidence interval
Ratio_age_SE <- sqrt(((1 - Ratio_young)/(T_young)+(1-Ratio_old)/(T_old)))
exp(log(Ratio_age)+1.96*Ratio_age_SE)
exp(log(Ratio_age)-1.96*Ratio_age_SE)



# Model revision --> gender analysis


# Check for correlation with other variables for gender
glm_revision_gender <- glm(A1_Gender ~., data = df[,2:27], family=binomial)
summary(glm_revision_gender)

# Check for correlation with other variables for age
glm_revision_age <- glm(A2_Age ~., data = df[,2:27])
summary(glm_revision_age)

# Further analysis, not required for paper ------------
#Civil_servant_status analysis analysis: Turnover is higher if the employee has a civil servants status
df_civil_servant <- dplyr::count(df_adverse_impact, B2_Public_service_status_GER, Turnover)
df_civil_servant
df_civil_servant[2,3]/df_civil_servant[1,3] 
df_civil_servant[4,3]/df_civil_servant[3,3] 

df_age
Ratio_age
df_gender
Ratio_Gender
model_rf_revised$results

# Safe Image with name of model_type -----------
#paste("~/",adverse_impact_model_type,"_Model_afterRenaming.RData",sep="") %>%  save.image()
ENDOFPROGRAMMERROR

#Predict values for Adverse_Impact analysis
df_adverse_impact <- test_protected[,which(!names(test_protected) %in% c("Monat"))] %>% 
  mutate(Turnover_prob = predict(object=model_win, test_protected[,which(!names(test_protected) %in% c("Age","Gender"))], type = "prob" )[, "Turnover"])



# choosing threshold for Adverse Impact analysis
p_adverse_impact <- 0.05
df_adverse_impact$Turnover_pred <- ifelse(df_adverse_impact[,"Turnover_prob"]>p_adverse_impact, "Turnover", "No_Turnover") %>% factor()
df_adverse_impact$grouped_age <- ifelse(df_adverse_impact[,"Age"]>50, "old", "young") %>% factor()
summary(df_adverse_impact)

# show overall consfusion matrix
confusionMatrix(df_adverse_impact$Turnover_pred,
                   df_adverse_impact$Turnover, 
                   positive="Turnover")$table/nrow(df_adverse_impact)*100 %>% round(.,4)


# Aggregating according to gender for adverse impact analysis (4/5 rule)
merge(dplyr::count(df_adverse_impact, Gender, Turnover), 
      dplyr::count(df_adverse_impact, Gender, Turnover_pred), 
      by.x=0, by.y = 0, all=F) %>%  
  .[,c("Gender.x","Turnover", "n.x", "n.y")] %>% 
  dplyr::rename(Gender=Gender.x, Turnover_n = n.x, Turnover_pred = n.y) 


# Aggregating according to grouped_age for adverse impact analysis (4/5 rule)
merge(dplyr::count(df_adverse_impact, grouped_age, Turnover), 
      dplyr::count(df_adverse_impact, grouped_age, Turnover_pred), 
      by.x=0, by.y = 0, all=F) %>%  
  .[,c("grouped_age.x","Turnover", "n.x", "n.y")] %>% 
  dplyr::rename(grouped_age=grouped_age.x, Turnover_n = n.x, Turnover_pred = n.y) %>% 
  dplyr::mutate(Ration = Turnover_n/Turnover_pred)


# Adverse Impact Analysis Gender
df_gender <- dplyr::count(df_adverse_impact, Gender, Turnover_pred)[3:6,]
rownames(df_gender) <- 1:4

NT_men <-  df_gender[1,3]
T_men <-  df_gender[2,3]
NT_woman <-  df_gender[3,3]
T_woman <-  df_gender[4,3]

Ratio_men <- T_men/(NT_men+T_men)
Ratio_woman <- T_woman/(T_woman+NT_woman)

Ratio_Gender <- min(Ratio_men,Ratio_woman)/max(Ratio_men,Ratio_woman)
Ratio_Gender


# Adverse Impact Analysis Age
df_age <- dplyr::count(df_adverse_impact, grouped_age, Turnover_pred)
df_age

NT_old <-  df_age[1,3]
T_old <-  df_age[2,3]
NT_young <-  df_age[3,3]
T_young <-  df_age[4,3]

Ratio_old <- T_old/(NT_old+T_old)
Ratio_young <- T_young/(T_young+NT_young)

Ratio_age   <- min(Ratio_old,Ratio_young)/max(Ratio_old,Ratio_young)
Ratio_age


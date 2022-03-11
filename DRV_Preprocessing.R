# Data preparation: Read data frame, adjust variable types, rename and reorder attributes -------

# Read data from local file
df <- read.table(stringr::str_c(getwd(),'/Data/DRV_POC_Employees_pre.csv'), sep=';', header=T, dec=',', stringsAsFactors=T, comment.char = "", fill=T, encoding='UTF-8')

# Adjust variable types
df[,c('Kind.U18','X1.Jahr.im.Betrieb')] <- lapply(df[,c('Kind.U18','X1.Jahr.im.Betrieb')],as.factor) 
df$Abt <- as.factor(as.character(df$Abt))
df$Verbeamtung <- as.factor(as.character(df$Verbeamtung))
df$NaK_6 <-as.factor(df$NaK_6)
levels(df$NaK_6) <- c("No_Turnover", "Turnover")
df$Monat <- as.POSIXct(strptime(df$Monat, format='%Y-%m-%d'))


# Fill NA, trivial --> not needed, bagImpute used instead (see below)
# df <- df %>% replace_na(list(var4=0, #most values
#                       Diff.Gehalt.1.Jahre.rel=0,  #most values
#                       Diff.Gehalt.5.Jahre.rel=0,  #most values
#                       Soll.AT=23, # most values
#                       Anzahl.Kinder=1,# Median
#                       Alter.des.juengsten.Kinds=20,# Median
#                       Var4=0, # most values
#                       Var5=0, # most values
#                       Var7=20, # Median
#                       Var8=22, # Median
#                       Diff.Gehalt.1.Jahre.abs=360,
#                       Diff.Gehalt.5.Jahre.abs=640, # Median
#                       Distanz=14.68, # Median
#                       Betriebszugehoerigkeit.in.Monaten =0)) # Median


# Delete columns Monat to avoid it beeing used as factor in analysis
df[,c(      'Maßnahmenart',
            'Maßnahmengrund',
            'Mitarbeiterkreis',
            'FAKE_ID','X.U.FEFF.',
            'Beginn.der.Maßnahme',
            'Monate.bis.Maßnahme',
            'NaK',
            'NaK_1',
            'NaK_3',
            'NaK_12')] <- NULL

# Exclude non-critical attributes, excluded from SAP Analytics Cloud project
df[,c(      'Abt',
            'Stelle',
            'Tarifgruppe',
            'Tarifstufe',
            'Tarifgruppe.vor.einem.Jahr',
            'Tarifstufe.vor.einem.Jahr',
            'Tarifgruppe.vor.5.Jahren',
            'Tarifstufe.vor.5.Jahren',
            'Vertragsart',
            'Mitarbeiterkreis',
            'Gehalt.vor.einem.Jahr',
            'Gehalt.vor.5.Jahren',
            'Diff.Gehalt.1.Jahre.abs',
            'Diff.Gehalt.5.Jahre.abs',
            'Sabbaticaltage',
            'Bildungsniveau',
            'Mutterschutztage',
            'indiv.Arbeitszeit')] <- NULL

# Exclude non-significant attributes
df[,c('Bildungsniveau',
            'Var5',
            'Var6',
            'Kurzarbeiter.DE',
            'Kununu.Score',
            'Avg.Kununu.Score',
            'X1.Jahr.im.Betrieb',
            'Diff.Gehalt.5.Jahre.rel',
            'Var2',
            'Var3',
            'Var7',
            'Gemeldete.Stellen.DE')] <- NULL


# Create full model with all data to determine statistical significance (before Imputation)
glm_fit = glm(NaK_6 ~.-Monat, data = df, family=binomial)
summary(glm_fit)
summary(df)


# Rename columns to English
df <- df %>% 
  dplyr::rename(
    Gender=Geschlecht,
    Age=Alter,
    Early_retirement_rate=Var4,
    Degree_of_employment=Beschaeftigungsgrad,
    Salary_today=Gehalt.heute,
    Rel_salary_increase_1_years=Diff.Gehalt.1.Jahre.rel,
    Gross_working_days=Soll.AT,
    Sick_days=Var1,
    Length_of_service_in_month=Betriebszugehoerigkeit.in.Monaten,
    Number_of_children=Anzahl.Kinder,
    Age_youngest_children=Alter.des.juengsten.Kinds,
    Net_working_days=Var8,
    Children_under_18=Kind.U18,
    Holiday_days=Urlaubstage,
    Turnover=NaK_6,
    Civil_servant_status_ger=Verbeamtung,
    Covid_Strigency_Index=Covid.Strigency.Index,
    Covid_Cases_ger=Covid.Cases,
    Covid_Google_Trends=Trends.Covid.Google,
    Unemployment_rate_ger=Arbeitslosenquote.DE,
    Home_workplace_distance=Distanz
)

# Reorder columns 
df <- df[,c(    'Monat',
                'Gender',
                'Age',
                'Degree_of_employment',
                'Gross_working_days',
                'Early_retirement_rate',
                'Holiday_days',
                'Net_working_days',
                'Sick_days',
                'Salary_today',
                'Rel_salary_increase_1_years',
                'Length_of_service_in_month',
                'Number_of_children',
                'Age_youngest_children',
                'Children_under_18',
                'Civil_servant_status_ger',
                'Covid_Strigency_Index',
                'Covid_Cases_ger',
                'Covid_Google_Trends',
                'Unemployment_rate_ger',
                'Home_workplace_distance',
                'Turnover')]


# Imputation and data split in training and test data sets, save data to csv ----------

# Chosse nominal values for Imputation
nominal_values=c('Age',
                 'Degree_of_employment',
                 'Gross_working_days',
                 'Early_retirement_rate',
                 'Holiday_days',
                 'Net_working_days',
                 'Sick_days',
                 'Salary_today',
                 'Rel_salary_increase_1_years',
                 'Length_of_service_in_month',
                 'Number_of_children',
                 'Age_youngest_children',
                 'Covid_Strigency_Index',
                 'Covid_Cases_ger',
                 'Covid_Google_Trends',
                 'Unemployment_rate_ger',
                 'Home_workplace_distance')

# Impute missing values with bagImpute
df[,nominal_values] <- preProcess(df[,nominal_values], method = "bagImpute") %>% 
  predict(., df[,nominal_values]) 

# Round relevant integer values
df[,c('Home_workplace_distance',
      'Salary_today',
      'Length_of_service_in_month',
      'Age_youngest_children',
      'Degree_of_employment')] <- df[,c('Home_workplace_distance',
                                        'Salary_today',
                                        'Length_of_service_in_month',
                                        'Age_youngest_children',
                                        'Degree_of_employment' )] %>%  round()


# exclude protected class attributes that cause adverse impact, based on chosen model type  ------------
# safe all data in df_procteded to analyze adverse impact after model training and evaluation
df_protected <- df

# for operational model: exclude procted class variables violate discrimination rules (age, gender)
if(adverse_impact_model_type=="operational"){
  df[,c('Age','Gender')] <- NULL
  
}

# for revised model: exclude variables that correlate with adverse impact predictor "age" (young vs. old)
if(adverse_impact_model_type=="revised"){
  df[,c('Age','Gender')] <- NULL
  df[,c('Age_youngest_children')] <- NULL #,'Length_of_service_in_month'
}


# Splitting data in training and test data, using the last 6 month as test data -----

# Generating training data set
training <- subset(df, Monat >= as.POSIXct('2018-01-01') & Monat < as.POSIXct('2020-07-01'))[,!(names(df)=='Monat')]

# Generating test data set
test <- subset(df, Monat >= as.POSIXct('2020-07-01'))
#df[,c('Monat')]<-NULL
test_protected <- subset(df_protected, Monat >= as.POSIXct('2020-07-01'))

#print out summary statistics
summary(df)
psych::describe(df)
sumtable(df) #, out="csv",file=stringr::str_c(getwd(),'/Data/DRV_POC_Employees_R_sumtable.csv')

# save data to external csv files ------
write.csv(df, stringr::str_c(getwd(),'/Data/DRV_POC_Employees_R.csv'))
write.csv(df, stringr::str_c(getwd(),'/Data/DRV_POC_Employees_R_training.csv'))
write.csv(test, stringr::str_c(getwd(),'/Data/DRV_POC_Employees_R_test.csv'))
print("Prepocessing finished")




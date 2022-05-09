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
    A1_Gender=Geschlecht,
    A2_Age=Alter,
    B3_Early_retirement_rate = Var4,
    B5_Degree_of_employment = Beschaeftigungsgrad,
    B11_Salary_today_EUR=Gehalt.heute,
    B9_Salary_increase_last_year=Diff.Gehalt.1.Jahre.rel,
    B6_Gross_working_days=Soll.AT,
    B4_Sickness_days=Var1,
    B10_Tenure_in_month=Betriebszugehoerigkeit.in.Monaten,
    A3_Number_of_children=Anzahl.Kinder,
    A5_Age_youngest_children=Alter.des.juengsten.Kinds,
    B8_Net_working_days=Var8,
    A4_Children_under_18_years=Kind.U18,
    B7_Vacation_days=Urlaubstage,
    Turnover=NaK_6,
    B2_Public_service_status_GER=Verbeamtung,
    E1_Covid_strigency_index_GER=Covid.Strigency.Index,
    E2_Covid_cases_GER=Covid.Cases,
    E3_Covid_Google_trends_GER=Trends.Covid.Google,
    D1_Unemployment_rate_GER=Arbeitslosenquote.DE,
    B1_Commute_distance_in_km=Distanz
)

# Reorder columns 
df <- df[,c(    'Monat',
                'A1_Gender',
                'A2_Age',
                'A3_Number_of_children',
                'A4_Children_under_18_years',
                'A5_Age_youngest_children',
                'B1_Commute_distance_in_km',
                'B2_Public_service_status_GER',
                'B3_Early_retirement_rate',
                'B4_Sickness_days',
                'B5_Degree_of_employment',
                'B6_Gross_working_days',
                'B7_Vacation_days',
                'B8_Net_working_days',
                'B9_Salary_increase_last_year',
                'B10_Tenure_in_month',
                'B11_Salary_today_EUR',
                'E1_Covid_strigency_index_GER',
                'E2_Covid_cases_GER',
                'E3_Covid_Google_trends_GER',
                'D1_Unemployment_rate_GER',
                'Turnover')]


# Imputation and data split in training and test data sets, save data to csv ----------

# Chosse nominal values for Imputation
nominal_values=c('A2_Age',
                 'A3_Number_of_children',
                 'A5_Age_youngest_children',
                 'B1_Commute_distance_in_km',
                 'B3_Early_retirement_rate',
                 'B4_Sickness_days',
                 'B5_Degree_of_employment',
                 'B6_Gross_working_days',
                 'B7_Vacation_days',
                 'B8_Net_working_days',
                 'B9_Salary_increase_last_year',
                 'B10_Tenure_in_month',
                 'B11_Salary_today_EUR',
                 'E1_Covid_strigency_index_GER',
                 'E2_Covid_cases_GER',
                 'E3_Covid_Google_trends_GER',
                 'D1_Unemployment_rate_GER')

# Impute missing values with bagImpute
df[,nominal_values] <- preProcess(df[,nominal_values], method = "bagImpute") %>% 
  predict(., df[,nominal_values]) 

# Round relevant integer values
df[,c('B1_Commute_distance_in_km',
      'B11_Salary_today_EUR',
      'B10_Tenure_in_month',
      'A5_Age_youngest_children',
      'B5_Degree_of_employment')] <- df[,c('B1_Commute_distance_in_km',
                                        'B11_Salary_today_EUR',
                                        'B10_Tenure_in_month',
                                        'A5_Age_youngest_children',
                                        'B5_Degree_of_employment' )] %>%  round()


# exclude protected class attributes that cause adverse impact, based on chosen model type  ------------
# safe all data in df_procteded to analyze adverse impact after model training and evaluation
df_protected <- df

# for operational model: exclude procted class variables violate discrimination rules (age, gender)
if(adverse_impact_model_type=="operational"){
  df[,c('A2_Age','A1_Gender')] <- NULL
  
}

# for revised model: exclude variables that correlate with adverse impact predictor "age" (young vs. old)
if(adverse_impact_model_type=="revised"){
  df[,c('A2_Age','A1_Gender')] <- NULL
  df[,c('A5_Age_youngest_children')] <- NULL #,'Length_of_service_in_month'
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
sumtable(df) #, out="csv",file=stringr::str_c(getwd(),'/Data/DRV_POC_Employees_R_sumtable.csv'))

# save data to external csv files ------
write.csv(df, stringr::str_c(getwd(),'/Data/DRV_POC_Employees_R.csv'))
write.csv(df, stringr::str_c(getwd(),'/Data/DRV_POC_Employees_R_training.csv'))
write.csv(test, stringr::str_c(getwd(),'/Data/DRV_POC_Employees_R_test.csv'))
print("Prepocessing finished")





# 0. Description ----------------------------------------------------------
# 1. Load data and functions
# 2. Blood Donor (CBS)
# 3. Outpatient Laboratory (APL)
# 4. Probabilistic Survey 1 (Ab-c)
# 5. Probabilistic Survey 2 (CLSA)
# 6. Probabilistic Survey 3 (CanPath)
# 7. 2016 Canadian census
# 8. Create summary table for supplement


# 1. Load data and functions ---------------------------------------------------------------
rm(list = ls())
library(haven)
library(lubridate)
library(tidyverse)# loads readr
library(DBI)
library(RPostgres)
library(readxl)
library(tableone)

#CBS CITF Serology dataset import (only run when data is updated)

#Test connection arguments
#con <- dbConnect(
#  RPostgres::Postgres(),
#  dbname = 'cbs0', 
#  host = '132.216.183.71', 
#  port = 5432 
#)

#Import dataset
#cbs_data <- dbReadTable(con, SQL('students.copy_cbs_combined'))

#Disconnect from database once data is loaded into R
#dbDisconnect(con)

#write_csv(cbs_data,'cbs_unmodified_df_backup_final.csv')"
cbs_data<-read.csv("./1_data/private/CBS/cbs_unmodified_df_backup_final.csv")

#APL dataset import
load("./1_data/private/APL/RFD4682_ev2.RData")
apl_data<-RFD4682_ev2

#Ab-C dataset import (contains Ab-C questionnaire cohort & serology cohort)
abc_data<-read.csv("./1_data/private/Ab-c/df_047_hs_jha_phases1234.csv")

#CLSA COVID-19 serology study dataset import
#Combined cohort
df_clsa_cb<-read.csv("./1_data/private/CLSA/2209005_McGill_ARussell_Covid_Combined_v1-1.csv")

#Antibody cohort
df_clsa_anti<-read.csv("./1_data/private/CLSA/2209005_McGill_ARussell_Covid_Antibody_Combined_NoIndigenousIdentifiers_v1.csv")

#Canpath antibody study dataset import
canpath_data<-read.csv("./1_data/private/CanPath/DAO-543759_ResearcherDataset_Qx_96014par_1125var.csv")
canpath_seradmin<-read.csv("./1_data/private/CanPath/DAO-543759_ResearcherDataset_Serology_Admin_25727par.csv")
canpath_serres<-read.csv("./1_data/private/CanPath/DAO-543759_ResearcherDataset_Serology_Results_74503par.csv")

#Census dataset import - provinces & territories
casup<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censasup.xlsx")
casrp<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censasrp.xlsx")
csaqm<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censsqmp.xlsx")
csaqs<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censsqsp.xlsx")
cast<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censast.xlsx")

#Alternative census province datasets
casup_alt<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censasup_alt.xlsx")
casrp_alt<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censasrp_alt.xlsx")
csaqm_alt<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censsqmp_alt.xlsx")
csaqs_alt<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_Census/censsqsp_alt.xlsx")

#Package census datasets into lists
census<-list(casup,casrp,csaqm,csaqs,cast)
census_alt<-list(casup_alt,casrp_alt,csaqm_alt,csaqs_alt)

#Load functions
source("2_scripts/00_Helper_Functions.R")

# 2. Blood Donor (CBS) -------------------------------------------------------
#Generate province variable
cbs_data<-cbs_data %>% mutate(province = province_fun(fsa))

#Convert dob of participant to age at donation
cbs_data$year_donation<-as.numeric(format.Date(cbs_data$sampledate,"%Y")) #extract year of sample donation
cbs_data$donation_age <- cbs_data$year_donation - cbs_data$dob

#Fix incorrect dob entries -- 6 individuals with date of birth in 1800s
cbs_data$donation_age[cbs_data$donation_age > 120] #n = 6
cbs_data$donation_age[cbs_data$donation_age < 16] #n = 0
cbs_data$donation_age <- ifelse(cbs_data$donation_age > 120, cbs_data$donation_age - 100, cbs_data$donation_age)

#Create age groups variable
cbs_data$age_groups<-age_groups_fun(cbs_data$donation_age)

#Bin date of sample collection into two month buckets
cbs_data$month <- floor_date(as.Date(cbs_data$sampledate,tz = "UTC"), unit = "2 months")

#Categorize participant race/ethnicity as either white, racialized minority, or missing
cbs_data$race<-case_when(
  cbs_data$ethnic1 == "0 missing" ~ as.character(NA),
  cbs_data$ethnic1 == "0 Missing" ~ as.character(NA),
  cbs_data$ethnic1 == "1 White" ~ "White",
  cbs_data$ethnic1 == "2 Aborigin" ~ "Racialized minority",
  cbs_data$ethnic1 == "2 Aboriginal" ~ "Racialized minority",
  cbs_data$ethnic1 == "3 Asian" ~ "Racialized minority",
  cbs_data$ethnic1 == "4 Others" ~ "Racialized minority",
  cbs_data$ethnic1 == "4 Other" ~ "Racialized minority",
  TRUE ~ NA
)

#Create urban variable identifying urban or rural residence
cbs_data$urban<-case_when(
  substr(cbs_data$fsa,start = 2,stop = 2) == 0 ~ "Rural",
  substr(cbs_data$fsa,start = 2,stop = 2) %in% 1:9 ~ "Urban",
  TRUE ~ NA
)

#Re-label sex variable
cbs_data$sex<-case_when(
  cbs_data$sex == "F" ~ "Female",
  cbs_data$sex == "M" ~ "Male"
)

#Generate final df
nrow(cbs_data[is.na(cbs_data$cur_result_n) & is.na(cbs_data$cur_result_s),]) #0 - all participants have a serology result
cbs_df<-cbs_data %>% 
  select(pid,sampledate,sex,race,urban,quintmat,
         quintsoc,province,month,age_groups,fsa) %>% 
  filter(province != "YT" & province != "NU/NT" & province != "QC" &
           age_groups != "0-17 years") #n = 1035580

cbs_pre_impute <- colSums(is.na(cbs_df))

#For each participant, carry forward/backward missing sociodemographic observations if 
# available for another encounter
cbs_df<-cbs_df %>% 
  group_by(pid) %>% 
  fill(race,.direction = "downup") %>% 
  fill(urban,.direction = "downup") %>% 
  fill(quintmat,.direction = "downup") %>% 
  fill(quintsoc,.direction = "downup") %>% 
  ungroup() %>% 
  as.data.frame()

cbs_post_impute <- colSums(is.na(cbs_df))

#Generate counts by age-sex-urban strata
cbs_asu<-cbs_df %>%
  filter(!is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>%
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata
cbs_allu<-cbs_df %>% 
  filter(!is.na(urban)) %>%
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by age-sex, sex strata and combine
cbs_asall<-cbs_df %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  mutate(urban = "All regions") %>% 
  ungroup()

cbs_allsu<-cbs_df %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()
cbs_asu<-do.call("rbind",list(cbs_asu,cbs_allu,cbs_asall,cbs_allsu))

#Generate counts by age-sex-race strata
cbs_asr<-cbs_df %>% 
  filter(!is.na(race)) %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
cbs_allr<-cbs_df %>% 
  filter(!is.na(race)) %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
cbs_asr<-rbind(cbs_asr,cbs_allr)

#Generate counts by sex-quintmat strata
cbs_sqm<-cbs_df %>% 
  filter(!is.na(quintmat)) %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-quintsoc strata
cbs_sqs<-cbs_df %>% 
  filter(!is.na(quintsoc)) %>% 
  group_by(sex,quintsoc) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Save to .csv
#write_csv(cbs_asu,"./1_data/private/cbs_asu_final.csv")
#write_csv(cbs_asr,"./1_data/private/cbs_asr_final.csv")
#write_csv(cbs_sqm,"./1_data/private/cbs_sqm_final.csv")
#write_csv(cbs_sqs,"./1_data/private/cbs_sqs_final.csv")
#write_csv(cbs_df,"./1_data/private/cbs_df_final.csv")

# 3. Outpatient Laboratory (APL) ---------------------------------------------

#Remove individuals without a unique participant ID
apl_data<-apl_data[!is.na(apl_data$clean_IDe),]

#Remove participants residing outside of Alberta and generate urban variable
apl_data<-apl_data %>% 
  mutate(PAT_FSA = ifelse(substr(PAT_FSA,2,2) == "O", 
                          paste(substr(PAT_FSA,1,1),"0",substr(PAT_FSA,3,3),sep = ""),
                          PAT_FSA)
  ) %>% 
  filter(substr(PAT_FSA,1,1) == "T")
apl_data$urban<-case_when(
  substr(apl_data$PAT_FSA,start = 2,stop = 2) == "0" ~ "Rural",
  substr(apl_data$PAT_FSA,start = 2,stop = 2) %in% 1:9 ~ "Urban",
  TRUE ~ NA
)
           
#Bin date of sample collection into two month buckets
attr(apl_data$COLLECTION_DATE[1],"tz") #UTC timezone
apl_data$COLLECTION_DATE<-as.Date(apl_data$COLLECTION_DATE,tz = "UTC")
apl_data$month<-floor_date(apl_data$COLLECTION_DATE,unit = "2 months")

#Generate age group and province variables
apl_data<-apl_data %>%
 mutate(age_groups = age_groups_fun(apl_data$AGE_AT_COLLECTION),
        province = province_fun(apl_data$PAT_FSA))

#Remove participants missing a serology result (n = 6)
apl_data<-apl_data[!(is.na(apl_data$`N-IgG_INTERP`) & is.na(apl_data$`RBD-IgGII_INTERP`)),]

#Generate final df
apl_df<-apl_data %>% 
  select(clean_IDe,age_groups,GENDER,urban,province,month,
         COLLECTION_DATE,QUINTMAT,QUINTSOC,PAT_FSA) %>% 
  mutate(GENDER = if_else(GENDER == "Unknown", NA, GENDER))
colnames(apl_df)<-c("pid","age_groups","sex","urban","province","month",
                    "sampledate","quintmat","quintsoc","fsa") #n = 210906

#For each participant, carry forward/backward missing sociodemographic observations if 
# available for another encounter
apl_pre_impute <- colSums(is.na(apl_df))
  
apl_df <- apl_df %>% 
  group_by(pid) %>% 
  fill(age_groups,.direction = "downup") %>% 
  fill(sex,.direction = "downup") %>% 
  fill(urban,.direction = "downup") %>% 
  fill(quintmat,.direction = "downup") %>% 
  fill(quintsoc,.direction = "downup") %>% 
  ungroup() %>% 
  as.data.frame()

apl_post_impute <- colSums(is.na(apl_df))

#Generate counts by age-sex-urban strata
apl_asu<-apl_df %>% 
  filter(!is.na(age_groups) & !is.na(sex)) %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup() 

#Generate counts by sex-urban strata
apl_allu<-apl_df %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by age-sex, sex strata and combine
apl_asall<-apl_df %>% 
  filter(!is.na(age_groups) & !is.na(sex)) %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  mutate(urban = "All regions") %>% 
  ungroup()

apl_allsu<-apl_df %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

apl_asu<-do.call("rbind",list(apl_asu,apl_allu,apl_asall,apl_allsu))

#Generate counts by sex-quintmat strata
apl_sqm<-apl_df %>% 
  filter(!is.na(sex) & !is.na(quintmat)) %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-quintsoc strata
apl_sqs<-apl_df %>% 
  filter(!is.na(sex) & !is.na(quintsoc)) %>% 
  group_by(sex,quintsoc) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Save to .csv
#write_csv(apl_asu,"./1_data/private/apl_asu_final.csv")
#write_csv(apl_sqm,"./1_data/private/apl_sqm_final.csv")
#write_csv(apl_sqs,"./1_data/private/apl_sqs_final.csv")
#write_csv(apl_df,"./1_data/private/apl_df_final.csv")


# 4. Probabilistic Survey 1 (Ab-c) ---------------------------------------
#Categorize participant race/ethnicity as either white, racialized minority, or missing
abc_data$race<-NULL
abc_data$race1<-NULL
names<-c("p1_ethnicity_1","p1_ethnicity_2",
         "p1_ethnicity_3","p1_ethnicity_4","p1_ethnicity_5","p1_ethnicity_6",
         "p1_ethnicity_7","p1_ethnicity_8","p1_ethnicity_9","p1_ethnicity_10",
         "p1_ethnicity_11","p1_ethnicity_12","p1_ethnicity_13","p1_ethnicity_14",
         "p1_ethnicity_15")

for(i in 1:nrow(abc_data)){ 
  #Participant refused to state ethnicity
  if(sum(abc_data[i,names] %in% 15) > 0 & 
     sum(abc_data[i,names] %in% 1:14) == 0){
    abc_data$race[i]<-"pnts"
  
  #Participant provided a response
  } else{
    #Participant selected a response associated with european ethnicity (white)
    if(sum(abc_data[i,names] %in% c(2,3,4)) > 0){ 
      #Participant also selected a response associated with racialized minority
      if(sum(abc_data[i,names] %in% c(1,5:14)) > 0){
        abc_data$race[i]<-"Racialized minority"
      
      #Participant only selected european ethnicities
      } else{
        abc_data$race[i]<-"White"
      }
    
    #Participants who did not select a european (white) ethnicity  
    } else if(sum(abc_data[i,names] %in% c(1,5:14)) > 0) {
      #Only racialized minority ethnicities
      abc_data$race[i]<-"Racialized minority"
    } else{
      #Else, classify as missing
      abc_data$race[i]<-"Missing"
    }
    
  } 
  
}

#Sensitivity analysis 1: classify participants identifying as mixed race as white
for(i in 1:nrow(abc_data)){
  #Participant refused to state ethnicity
  if(sum(abc_data[i,names] %in% 15) > 0 & 
     sum(abc_data[i,names] %in% 1:14) == 0){
    abc_data$race1[i]<-"pnts"
  
  #Participant provided a response  
  } else{
    #Participant selected a response associated with racialized minority
    if(sum(abc_data[i,names] %in% c(1,5:14)) > 0){
      #Participant selected a response associated with european ethnicity (white)
      if(sum(abc_data[i,names] %in% c(2,3,4)) > 0){
        abc_data$race1[i]<-"White"
      
      #Participant only selected responses associated with racialized minority  
      }else{
        abc_data$race1[i]<-"Racialized minority"
      }
    
    #Participants who did not select a racialized minority ethnicity  
    } else if (sum(abc_data[i,names] %in% c(2,3,4)) > 0){
      #Only white ethnicities
      abc_data$race1[i]<-"White"
    #Else, classify as missing
    } else{
      abc_data$race1[i]<-"Missing" 
    }
    
  } 
  
}

#Period 1
abc_data1<-abc_data %>% 
  select(rseed,p1_result_sinai,p1_int_month,p1_province,p1_fsa,
         p1_age,p1_qe2,race,race1) %>% 
  mutate(sampledate = as.Date(case_when(
    p1_int_month == 5 ~ "2020-05-01",
    p1_int_month == 6 ~ "2020-06-01",
    p1_int_month == 7 ~ "2020-07-01",
    p1_int_month == 8 ~ "2020-08-01",
    p1_int_month == 9 ~ "2020-09-01",
    TRUE ~ NA),tz = "UTC"),
    province = province_fun2(p1_province),
    urban = case_when(
      substr(p1_fsa,start = 2,stop = 2) == 0 ~ "Rural",
      substr(p1_fsa,start = 2,stop = 2) %in% 1:9 ~ "Urban",
      TRUE ~ NA),
    age_groups = age_groups_fun(as.numeric(p1_age)),
    sex = case_when(p1_qe2 == 1 ~ "Male",
                    p1_qe2 == 2 ~ "Female",
                    p1_qe2 == 3 ~ "Self described",
                    TRUE ~ NA)
     )

#Categorize date sample received into 2 month time buckets
abc_data1$month<-floor_date(abc_data1$sampledate,unit = "2 months")
colnames(abc_data1)[5]<-"fsa"

#Sensitivity analysis 3: questionnaire respondents 
abc_data1s3 <- abc_data1[!is.na(abc_data1$p1_age),] #n = 20343

#Main analysis: participants who provided serology
abc_data1 <- filter(abc_data1,p1_result_sinai != "" ) #n = 8955 

#Period 2
#Impute missing p2 age and sex with p1 age and sex
abc_data2<-abc_data %>% 
  mutate(p2_age = ifelse(is.na(p2_age),p1_age,p2_age),
         p2_qe2 = ifelse(is.na(p2_qe2),p1_qe2,p2_qe2))

abc_data2<-abc_data2 %>% 
  select(rseed,p2_np_igg_pred,p2_rbd_igg_pred,p2_smt1_igg_pred,
         p2_received_date,p2_province,p2_fsa,p2_age,p2_qe2,p2_suggested_status,
         race,race1) %>% 
  mutate(sampledate = as.Date(p2_received_date,tz = "UTC"),
         province = province_fun2(p2_province),
         urban = case_when(
           substr(p2_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p2_fsa,start = 2,stop = 2) %in% 1:9 ~ "Urban",
           TRUE ~ NA),
         age_groups = age_groups_fun(as.numeric(p2_age)),
         sex = case_when(p2_qe2 == 1 ~ "Male",
                         p2_qe2 == 2 ~ "Female",
                         p2_qe2 == 3 ~ "Self described",
                         TRUE ~ NA)
  )

#Categorize date sample received into 2 month time buckets
abc_data2$month<-floor_date(abc_data2$sampledate,unit = "2 months")
colnames(abc_data2)[7]<-"fsa"

#Sensitivity analysis 3: questionnaire respondents
abc_data2s3 <- abc_data[!is.na(abc_data$p2_age),] 
abc_data2s3 <- merge(abc_data2s3, abc_data2) #n = 14730

#Main analysis: participants who provided serology
abc_data2 <- filter(abc_data2, p2_np_igg_pred != "" | p2_rbd_igg_pred != "" | 
                      p2_smt1_igg_pred != "") #n = 7160 

#Period 3
#Impute missing p3 age and sex with p1 age and sex
abc_data3<-abc_data %>% 
  mutate(p3_age = ifelse(is.na(p3_age),p1_age,p3_age),
         p3_qe2 = ifelse(is.na(p3_qe2),p1_qe2,p3_qe2))

abc_data3<-abc_data3 %>% 
  select(rseed,p3_np_igg_pred,p3_rbd_igg_pred,p3_smt1_igg_pred,
         p3_dbs_received_date,p3_province,p3_fsa,p3_age,p3_qe2,p3_suggested_status,
         race,race1) %>% 
  mutate(sampledate = as.Date(p3_dbs_received_date,tz = "UTC"),
         province = province_fun2(p3_province),
         urban = case_when(
           substr(p3_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p3_fsa,start = 2,stop = 2) %in% 1:9 ~ "Urban",
           TRUE ~ NA),
         age_groups = age_groups_fun(as.numeric(p3_age)),
         sex = case_when(p3_qe2 == 1 ~ "Male",
                         p3_qe2 == 2 ~ "Female",
                         p3_qe2 == 3 ~ "Self described",
                         TRUE ~ NA)
  )

#Place date sample received into 2 month time buckets
abc_data3$month<-floor_date(abc_data3$sampledate,unit = "2 months")
colnames(abc_data3)[7]<-"fsa"

#Sensitivity analysis 3: questionnaire respondents 
abc_data3s3 <- abc_data[!is.na(abc_data$p3_age),]
abc_data3s3 <- merge(abc_data3s3, abc_data3) # n = 10542

#Main analysis: participants who provided serology
abc_data3 <- filter(abc_data3,p3_np_igg_pred != "" |
                      p3_rbd_igg_pred != "" |
                      p3_smt1_igg_pred != "") #n = 5674 

#Period 4
#Impute missing p4 age and sex with p1 age and sex. For individuals with missing p1 race/ethnicity,
# assign race based on p4 race/ethnicity.
names4<-c("p4a_ethnicity_1","p4a_ethnicity_2",
          "p4a_ethnicity_3","p4a_ethnicity_4","p4a_ethnicity_5","p4a_ethnicity_6",
          "p4a_ethnicity_7","p4a_ethnicity_8","p4a_ethnicity_9","p4a_ethnicity_10",
          "p4a_ethnicity_11","p4a_ethnicity_12","p4a_ethnicity_13","p4a_ethnicity_14",
          "p4a_ethnicity_15")

for(i in 1:nrow(abc_data)) {
  #If p1 race/ethnicity unavailable, classify based on p4 race/ethnicity
  if ((abc_data$race[i] %in% "Missing") == T) {
    #Participant refused to state ethnicity
    if (sum(abc_data[i, names4] %in% 15) > 0 &
        sum(abc_data[i, names4] %in% 1:14) == 0) {
      abc_data$race[i] <- "pnts"
    }
    
    #Participant provided a response
    else{
      #Participant selected a response associated with european ethnicity (white)
      if (sum(abc_data[i, names4] %in% c(2, 3, 4)) > 0) {
        #Participant also selected a response associated with racialized minority
        if (sum(abc_data[i, names4] %in% c(1, 5:14)) > 0) {
          abc_data$race[i] <- "Racialized minority"
        
        #Participant only selected european ethnicities  
        } else{
          abc_data$race[i] <- "White"
        }
      
      #Particiapnts who did not select a european (white) ethnicity  
      } else if (sum(abc_data[i, names4] %in% c(1, 5:14)) > 0) {
        #Only racialized minority ethnicities
        abc_data$race[i] <- "Racialized minority"
      } else{
        #Else, classify as missing
        abc_data$race[i] <- NA
      }
      
    }
  #Else, leave race as is 
  } else{}
} 

#Sensitivity analysis 1
for(i in 1:nrow(abc_data)) {
  #If p1 race/ethnicity unavailable, classify based on p4 race/ethnicity
  if ((abc_data$race1[i] %in% "Missing") == T) {
    #Participant refused to state ethnicity
    if (sum(abc_data[i, names4] %in% 15) > 0 &
        sum(abc_data[i, names4] %in% 1:14) == 0) {
      abc_data$race1[i] <- "pnts"
    
    #Participant provided a response  
    } else{
      #Participant selected a response associated with racialized minority
      if (sum(abc_data[i, names4] %in% c(1, 5:14)) > 0) {
        #Participant selected a response associated with european ethnicity (white)
        if (sum(abc_data[i, names4] %in% c(2, 3, 4)) > 0) {
          abc_data$race1[i] <- "White"
        
        #Participant only selected responses associated with racialized minority  
        } else{
          abc_data$race1[i] <- "Racialized minority"
        }
      
      #Participants who did not select a racialized minority ethnicity  
      } else if (sum(abc_data[i, names4] %in% c(2, 3, 4)) > 0) {
        #Only white ethnicities
        abc_data$race1[i] <- "White"
      
      #Else, classify as missing  
      } else{
        abc_data$race1[i] <- NA
      }
      
    }
    
  # Else, leave race as is
  } else{}
}

abc_data4a<-abc_data %>% 
  mutate(p4a_age = ifelse(is.na(p4a_age),p1_age,p4a_age),
         p4a_qe2 = ifelse(is.na(p4a_qe2),p1_qe2,p4a_qe2))

abc_data4a<-abc_data4a %>% 
  select(rseed,p4_np_igg_pred,p4_rbd_igg_pred,p4_smt1_igg_pred,
         p4_dbs_received_date,p4a_province,p4a_fsa,p4a_age,p4a_qe2,p4_suggested_status,
         race,race1) %>% 
  mutate(sampledate = as.Date(p4_dbs_received_date,tz = "UTC"),
         province = province_fun2(p4a_province),
         urban = case_when(
           substr(p4a_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p4a_fsa,start = 2,stop = 2) %in% 1:9 ~ "Urban",
           TRUE ~ NA),
         age_groups = age_groups_fun(as.numeric(p4a_age)),
         sex = case_when(p4a_qe2 == 1 ~ "Male",
                         p4a_qe2 == 2 ~ "Female",
                         p4a_qe2 == 3 ~ "Self described")
  )

#Place date sample received into 2 month time buckets
abc_data4a$month<-floor_date(abc_data4a$sampledate,unit = "2 months")
colnames(abc_data4a)[7]<-"fsa"

# Phase 4 had two surveys. Since we're going to look at both for the sensitivity
# analysis, need to process the 4b dataset as well
# Note any participant with NA p4a ethnicity also has NA p4b ethnicity.
abc_data4b<-abc_data %>% 
  mutate(p4b_age = ifelse(is.na(p4b_age),p1_age,p4b_age),
         p4b_qe2 = ifelse(is.na(p4b_qe2),p1_qe2,p4b_qe2))

abc_data4b<-abc_data4b %>% 
  select(rseed,p4_np_igg_pred,p4_rbd_igg_pred,p4_smt1_igg_pred,
         p4_dbs_received_date,p4b_province,p4b_fsa,p4b_age,p4b_qe2,p4_suggested_status,
         race,race1) %>% 
  mutate(sampledate = as.Date(p4_dbs_received_date,tz = "UTC"),
         province = province_fun2(p4b_province),
         urban = case_when(
           substr(p4b_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p4b_fsa,start = 2,stop = 2) %in% 1:9 ~ "Urban",
           TRUE ~ NA),
         age_groups = age_groups_fun(as.numeric(p4b_age)),
         sex = case_when(p4b_qe2 == 1 ~ "Male",
                         p4b_qe2 == 2 ~ "Female",
                         p4b_qe2 == 3 ~ "Self described")
  )

#Place date sample received into 2 month time buckets
abc_data4b$month<-floor_date(abc_data4b$sampledate,unit = "2 months")
colnames(abc_data4b)[7]<-"fsa"

#Sensitivity analysis 3: questionnaire respondents
abc_data4as3 <- abc_data[!is.na(abc_data$p4a_age),]
abc_data4as3 <- merge(abc_data4as3, abc_data4a) #n = 12107

abc_data4bs3 <- abc_data[!is.na(abc_data$p4b_age),]
abc_data4bs3 <- merge(abc_data4bs3, abc_data4b) #n = 11596

#Main analysis: participants who provided serology
abc_data4 <- filter(abc_data4a, p4_np_igg_pred != "" |
                             p4_rbd_igg_pred != "" |
                             p4_smt1_igg_pred != "") #n = 5373

#Generate final working df
abc_df<-do.call("rbind",list(abc_data1[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa","race1")],
                             abc_data2[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa","race1")],
                             abc_data3[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa","race1")],
                             abc_data4[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa","race1")]))

abc_df_qab <- do.call("rbind",list(abc_data1s3[,c("rseed","age_groups","sex","urban","sampledate",
                                              "month","race","province","fsa","race1")],
                                 abc_data2s3[,c("rseed","age_groups","sex","urban","sampledate",
                                              "month","race","province","fsa","race1")],
                                 abc_data3s3[,c("rseed","age_groups","sex","urban","sampledate",
                                              "month","race","province","fsa","race1")],
                                 abc_data4as3[,c("rseed","age_groups","sex","urban","sampledate",
                                              "month","race","province","fsa","race1")],
                                 abc_data4bs3[,c("rseed","age_groups","sex","urban","sampledate",
                                                 "month","race","province","fsa","race1")])) #n = 67202

min(abc_data1$p1_age)
min(abc_data4a$p4a_age)

# From documentation, number of observations with serology specimens should be 27,204.
# Here, we have 27,162. Thus, we removed 42 observations with missing serology.

# For each participant, impute missing province observations if
# province available at another encounter
abc_df <- abc_df %>% 
  group_by(rseed) %>% 
  fill(province, .direction = "downup") %>% 
  ungroup() %>% 
  as.data.frame()

#Remove individuals still missing province
abc_df<-abc_df %>% 
  mutate(race = if_else(race == "pnts", NA, race),
         race1 = if_else(race1 == "pnts", NA, race1)) %>% 
  filter(!is.na(province) ) #n = 27140

#For each participant, carry forward/backward missing sociodemographic observations if 
# available for another encounter
abc_pre_impute <- colSums(is.na(abc_df))

abc_df <- abc_df %>% 
  group_by(rseed) %>% 
  fill(urban,.direction = "downup") %>% 
  fill(race,.direction = "downup") %>% 
  fill(race1,.direction = "downup") %>% 
  ungroup() %>% 
  as.data.frame()

abc_post_impute <- colSums(is.na(abc_df))

#Generate counts by age-sex-urban strata
abc_asu<-abc_df %>% 
  filter(province != "YT" & sex != "Self described" & !is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

abc_asu_qab<-abc_df_qab %>% 
  filter(province != "YT" & sex != "Self described" & !is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata
abc_allu<-abc_df %>% 
  filter(province != "YT" & sex != "Self described" & !is.na(urban)) %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

abc_allu_qab<-abc_df_qab %>% 
  filter(province != "YT" & sex != "Self described" & !is.na(urban)) %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by age-sex, sex strata and combine
abc_asall<-abc_df %>% 
  filter(province != "YT" & sex != "Self described") %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  mutate(urban = "All regions") %>% 
  ungroup()

abc_asall_qab<-abc_df_qab %>% 
  filter(province != "YT" & sex != "Self described") %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  mutate(urban = "All regions") %>% 
  ungroup()

abc_allsu<-abc_df %>% 
  filter(province != "YT" & sex != "Self described") %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

abc_allsu_qab<-abc_df_qab %>% 
  filter(province != "YT" & sex != "Self described") %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

abc_asu<-do.call("rbind",list(abc_asu,abc_allu,abc_asall,abc_allsu))
abc_asu_qab<-do.call("rbind",list(abc_asu_qab,abc_allu_qab,abc_asall_qab,abc_allsu_qab))

#Generate counts by age-sex-race strata
abc_asr<-abc_df %>% 
  filter(province != "YT" & sex != "Self described"  &
           !is.na(race)) %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

abc_asr_qab<-abc_df_qab %>% 
  filter(province != "YT" & sex != "Self described" & 
           !is.na(race)) %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
abc_allr<-abc_df %>% 
  filter(province != "YT" & sex != "Self described" & 
           !is.na(race)) %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

abc_allr_qab<-abc_df_qab %>% 
  filter(province != "YT" & sex != "Self described" &
           !is.na(race)) %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

abc_asr<-rbind(abc_asr,abc_allr)
abc_asr_qab<-rbind(abc_asr_qab,abc_allr_qab)

#Generate counts by age-sex strata (territories only)
abc_ast<-abc_df %>% 
  filter(province == "YT" & sex != "Self described") %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  ungroup()

abc_ast_qab<-abc_df_qab %>% 
  filter(province == "YT" & sex != "Self described") %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Sensitivity analysis 1: generate alternative age-sex-race counts when mixed race classified as "White"
#Generate counts by age-sex-race1 strata
abc_asr1<-abc_df %>%  
  filter(province != "YT" & sex != "Self described" &
           !is.na(race1)) %>% 
  group_by(age_groups,sex,race1) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race1 strata and combine
abc_allr1<-abc_df %>% 
  filter(province != "YT" & sex != "Self described" &
           !is.na(race1)) %>% 
  group_by(sex,race1) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
abc_asr1<-rbind(abc_asr1,abc_allr1)

#Save to csv
#write_csv(abc_asu,"./1_data/private/abc_asu_final.csv")
#write_csv(abc_asr,"./1_data/private/abc_asr_final.csv")
#write_csv(abc_df,"./1_data/private/abc_df_final.csv")
#write_csv(abc_ast,"./1_data/private/abc_ast_final.csv")
#write_csv(abc_asr1,"./1_data/private/abc_asr1_final.csv")
#write_csv(abc_asu_qab,"./1_data/private/abc_asu_final_qab.csv")
#write_csv(abc_asr_qab,"./1_data/private/abc_asr_final_qab.csv")
#write_csv(abc_df_qab,"./1_data/private/abc_df_final_qab.csv")
#write_csv(abc_ast_qab,"./1_data/private/abc_ast_final_qab.csv")

# 5. Probabilistic Survey 2 (CLSA) -----------------------------
#Join demographic & serological vars from antibody and combined cohorts
df_all_clsa <- merge(df_clsa_anti[,c("entity_id","SER_AGE_COV", "SER_SEX_COV", "start_datetime_COV",  
                              "SER_ETHN_WH_COV", "SER_ETHN_SA_COV", "SER_ETHN_ZH_COV", "SER_ETHN_BL_COV", "SER_ETHN_FP_COV", "SER_ETHN_LA_COV", "SER_ETHN_AR_COV", "SER_ETHN_SE_COV",
                              "SER_ETHN_WA_COV", "SER_ETHN_KO_COV", "SER_ETHN_JA_COV", "SER_ETHN_OTSP_COV", "SER_ETHN_DK_NA_COV", 
                              "SER_ETHN_REFUSED_COV","SER_NUCLEOCAPSID_COV", "SER_SPIKE_COV", "SER_ABRSLT_COV")], 
                     df_clsa_cb[,c("entity_id", "PROV_COVID", "FSA_COVID")],
                     by='entity_id', all.x = TRUE)

#Create province variable
df_all_clsa$province_anti<-province_fun(df_all_clsa$FSA_COVID)

#Create age variable
df_all_clsa$age_groups <- age_groups_fun(df_all_clsa$SER_AGE_COV)

#Re-label sex variable
df_all_clsa$sex<-case_when(
  df_all_clsa$SER_SEX_COV == "F" ~ "Female",
  df_all_clsa$SER_SEX_COV == "M" ~ "Male",
  TRUE ~ NA)

#Classify residence as urban or rural
df_all_clsa$urban<-case_when(substr(df_all_clsa$FSA_COVID,2,2) == 0 ~ "Rural",
                             substr(df_all_clsa$FSA_COVID,2,2) %in% 1:9 ~ "Urban",
                             TRUE ~ NA)

#Classify ethnicity as white or racialized minority
df_all_clsa$race<-NULL
df_all_clsa$race1<-NULL
for(i in 1:nrow(df_all_clsa)){#open for loop
  #Participant did not know or preferred not to state ethnicity
  if(sum(df_all_clsa[i,17:18] %in% 1) > 0){
    df_all_clsa$race[i]<-"pnts"
  
    #Participant provided a response  
  } else{
    #Participant selected a response associated with european ethnicity (white)
    if(sum(df_all_clsa[i,5] %in% 1) > 0){ #choose white
      if(sum(df_all_clsa[i,6:15] %in% 1) > 0){ #also choose RM
        df_all_clsa$race[i]<-"Racialized minority"
      #Participant only selected european ethnicities
      } else{
        df_all_clsa$race[i]<-"White"
      }
    #Participants who did not select a european (white) ethnicity
    } else if(sum(df_all_clsa[i,6:15] %in% 1) > 0){
      #Only racialized minority ethnicities
        df_all_clsa$race[i]<-"Racialized minority"
      } else{
      #Else, classify as missing
      df_all_clsa$race[i]<-"Missing"
    }
    
  } #close else for participants who responded
  
  #Check text box responses of individuals who also used mark in
  if(sum((df_all_clsa[i,16] %in% c(-88888,-99999)) == 0)){ #if otsp used
    if((df_all_clsa[i,"race"] %in% "Missing")==F){ #race != "Missing"
      if((df_all_clsa[i,"race"] %in% "White" &
          df_all_clsa[i,16] %in% rm_indn)==T){ #if race is "White" and text box %in% rm_indn string
        df_all_clsa$race[i]<-"Racialized minority"
      } else{} #else leave the same
    } else if((df_all_clsa[i,"race"] %in% "Missing")==T){ #tb used and race is still missing
      if(df_all_clsa[i,16] %in% w){ #if white string, white
        df_all_clsa$race[i]<-"White"
      }else if(df_all_clsa[i,16] %in% rm_indn){ #if rm string, rm
        df_all_clsa$race[i]<-"Racialized minority"
      }else{ #no response, still missing
        df_all_clsa$race[i]<-"Missing"
      }
    } else{}
  } else{} #text box not used, remain the same
  
}

#Sensitivity analysis 1:
for(i in 1:nrow(df_all_clsa)){
  #Participant did not know or preferred not to state ethnicity
  if(sum(df_all_clsa[i,17:18] %in% 1) > 0){
    df_all_clsa$race1[i]<-"pnts"
  
    #Participant provided a response  
  } else{
    #Participant selected a response associated with racialized minority
    if(sum(df_all_clsa[i,6:15] %in% 1) > 0){
      #Participant selected a response associated with european ethnicity (white)
      if(sum(df_all_clsa[i,5] %in% 1) > 0){
        df_all_clsa$race1[i]<-"White"
        #Participant only selected responses associated with racialized minority
      } else{
        df_all_clsa$race1[i]<-"Racialized minority"
      }
    #Participants who did not select a racialized minority ethnicity
    } else if(df_all_clsa[i,5] %in% 1){
      #Only white ethnicities
      df_all_clsa$race1[i]<-"White"
    } else{
      #Else, classify as missing
      df_all_clsa$race1[i]<-"Missing"
    }

  }#close else for participants who responsded
  
  #Check text box responses of individuals who also used mark in
  if(sum((df_all_clsa[i,16] %in% c(-88888,-99999)) == 0)){ #if otsp used
    if((df_all_clsa[i,"race1"] %in% "Missing")==F){ #race1 != "Missing"
      if((df_all_clsa[i,"race1"] %in% "Racialized minority" &
          df_all_clsa[i,16] %in% w)==T){ #if race1 is "Racialized minority" and text box %in% w string
        df_all_clsa$race1[i]<-"White"
      } else{} #else leave the same
    } else if((df_all_clsa[i,"race1"] %in% "Missing")==T){ #tb used and race1 is still missing
      if(df_all_clsa[i,16] %in% w){ #if white string, white
        df_all_clsa$race1[i]<-"White"
      } else if(df_all_clsa[i,16] %in% rm_indn){ #if rm string, rm
        df_all_clsa$race1[i]<-"Racialized minority"
      } else{ #no response, still missing
        df_all_clsa$race1[i]<-"Missing"
      }
    } else{}
  } else{} #text box not used, remain the same
  
}

df_all_clsa <-df_all_clsa %>% 
  mutate(race = ifelse(race == "Missing",NA,race),
         race1 = ifelse(race1 == "Missing",NA,race1))

#classify interview start date (proxy for sampledate)
df_all_clsa$sampledate<-as.Date(df_all_clsa$start_datetime_COV,tz = "UTC")
df_all_clsa$month<-floor_date(df_all_clsa$sampledate,
                              unit = "2 months")

#Impute missing fsas/quintmat/quintsoc/province in antibody dataset with variables from other supplementary datasets
# (baseline, follow up 1, follow up 2)
missing_fsa_id<-df_all_clsa[is.na(df_all_clsa$FSA_COVID),"entity_id"] #IDs with missing fsa

#Read in
#Baseline
df_clsa_baselinec<-read.csv("1_data/private/CLSA/2209005_McGill_ARussell_Baseline_CoPv7_Qx_CANUE_PA_BS_CSD_FSA.csv") %>% 
  select(entity_id,SDC_FSA_COM) %>% 
  mutate(measure_t = "baseline") %>% 
  filter(!is.na(SDC_FSA_COM)) #comprehensive
colnames(df_clsa_baselinec)[2]<-"fsa"

df_clsa_baselinet<-read.csv("1_data/private/CLSA/2209005_McGill_ARussell_Baseline_Trav4_Qx_CANUE_CSD_FSA.csv") %>% 
  select(entity_id,SDC_FSA_TRM) %>% 
  mutate(measure_t = "baseline") %>% 
  filter(!is.na(SDC_FSA_TRM)) #tracking
colnames(df_clsa_baselinet)[2]<-"fsa"

#Follow up 1
df_clsa_fup1c<-read.csv("1_data/private/CLSA/2209005_McGill_ARussell_FUP1_CoPv4_Qx_CANUE_PA_BS_CSD_FSA.csv") %>% 
  select(entity_id,SDC_FSA_COF1,MSD16_10_COF1,MSD16_11_COF1) %>% 
  mutate(measure_t = "fup1")
colnames(df_clsa_fup1c)[2:4]<-c("fsa","quintmat","quintsoc")

df_clsa_fup1t<-read.csv("1_data/private/CLSA/2209005_McGill_ARussell_FUP1_Trav3_Qx_CANUE_FSA_CSD.csv") %>% 
  select(entity_id,SDC_FSA_TRF1,MSD16_10_TRF1,MSD16_11_TRF1) %>% 
  mutate(measure_t = "fup1")
colnames(df_clsa_fup1t)[2:4]<-c("fsa","quintmat","quintsoc")

#Follow up 2
df_clsa_fup2c<-read.csv("1_data/private/CLSA/2209005_McGill_ARussell_FUP2_CoPv1_Qx_FSA_CSD.csv") %>% 
  select(entity_id,SDC_FSA_COF2) %>% 
  mutate(measure_t = "fup2") %>% 
  filter(!is.na(SDC_FSA_COF2) & 
           substr(SDC_FSA_COF2,1,1) %in% LETTERS) #remove 1 blank FSA
colnames(df_clsa_fup2c)[2]<-"fsa"

df_clsa_fup2t<-read.csv("1_data/private/CLSA/2209005_McGill_ARussell_FUP2_Trav1_Qx_FSA_CSD.csv") %>% 
  select(entity_id,SDC_FSA_TRF2) %>% 
  mutate(measure_t = "fup2") %>% 
  filter(!is.na(SDC_FSA_TRF2))
colnames(df_clsa_fup2t)[2]<-"fsa"

#CLSA supplementary dataset cleaning
# Check no duplicate IDs in each dataset, fsa formatting, unique values

#Baseline datasets - verified formatted correctly (id, fsa)
df_clsa_baselinec %>% group_by(entity_id) %>% filter(n() > 1) #0
df_clsa_baselinet %>% group_by(entity_id) %>% filter(n() > 1) #0

#lapply(df_clsa_baselinec,unique)
#lapply(df_clsa_baselinet,unique)

#Check fsa formatting
which(nchar(df_clsa_baselinec$fsa) != 3)#0
which(nchar(df_clsa_baselinet$fsa) != 3)#0

#Ensure first and third are capital letters, second is a number between 0-9
sum((substr(df_clsa_baselinec$fsa,1,1) %in% LETTERS) & 
      (substr(df_clsa_baselinec$fsa,2,2) %in% c("0","1","2","3","4",
                                                "5","6","7","8","9")) &
      (substr(df_clsa_baselinec$fsa,3,3) %in% LETTERS)) == nrow(df_clsa_baselinec) #all formatted correctly

sum((substr(df_clsa_baselinet$fsa,1,1) %in% LETTERS) & 
      (substr(df_clsa_baselinet$fsa,2,2) %in% c("0","1","2","3","4",
                                                "5","6","7","8","9")) &
      (substr(df_clsa_baselinet$fsa,3,3) %in% LETTERS)) == nrow(df_clsa_baselinet) #all formatted correctly

#FUP1
#datasets - verified formatted correctly (id, fsa,quintmat/quintsoc)
df_clsa_fup1c %>% group_by(entity_id) %>% filter(n() > 1) #0
df_clsa_fup1t %>% group_by(entity_id) %>% filter(n() > 1) #0

#lapply(df_clsa_fup1c,unique)
#lapply(df_clsa_fup1t,unique)

#Check fsa formatting
which(nchar(df_clsa_fup1c$fsa) != 3)#0
which(nchar(df_clsa_fup1t$fsa) != 3)#0

#Ensure first and third are capital letters, second is a number between 0-9
sum((substr(df_clsa_fup1c$fsa,1,1) %in% LETTERS) & 
      (substr(df_clsa_fup1c$fsa,2,2) %in% c("0","1","2","3","4",
                                            "5","6","7","8","9")) &
      (substr(df_clsa_fup1c$fsa,3,3) %in% LETTERS)) == nrow(df_clsa_fup1c) #all formatted correctly

sum((substr(df_clsa_fup1t$fsa,1,1) %in% LETTERS) & 
      (substr(df_clsa_fup1t$fsa,2,2) %in% c("0","1","2","3","4",
                                            "5","6","7","8","9")) &
      (substr(df_clsa_fup1t$fsa,3,3) %in% LETTERS)) == nrow(df_clsa_fup1t) #all formatted correctly

#FUP2
#datasets - verified formatted correctly (id, fsa)
df_clsa_fup2c %>% group_by(entity_id) %>% filter(n() > 1) #0
df_clsa_fup2t %>% group_by(entity_id) %>% filter(n() > 1) #0

#lapply(df_clsa_fup2c,unique)
#lapply(df_clsa_fup2t,unique)

#Check fsa formatting
which(nchar(df_clsa_fup2c$fsa) != 3)#0
which(nchar(df_clsa_fup2t$fsa) != 3)#0

#Ensure first and third are capital letters, second is a number between 0-9
sum((substr(df_clsa_fup2c$fsa,1,1) %in% LETTERS) & 
      (substr(df_clsa_fup2c$fsa,2,2) %in% c("0","1","2","3","4",
                                            "5","6","7","8","9")) &
      (substr(df_clsa_fup2c$fsa,3,3) %in% LETTERS)) == nrow(df_clsa_fup2c) #all formatted correctly

sum((substr(df_clsa_fup2t$fsa,1,1) %in% LETTERS) & 
      (substr(df_clsa_fup2t$fsa,2,2) %in% c("0","1","2","3","4",
                                            "5","6","7","8","9")) &
      (substr(df_clsa_fup2t$fsa,3,3) %in% LETTERS)) == nrow(df_clsa_fup2t) #all formatted correctly

#Combine ids and fsas into 1 df.Generate province of residence & urban/rural variables
id_fsa_all<-do.call("rbind",list(df_clsa_baselinec,df_clsa_baselinet,
                                 df_clsa_fup1c[!is.na(df_clsa_fup1c$fsa),c("entity_id","fsa","measure_t")],
                                 df_clsa_fup1t[!is.na(df_clsa_fup1t$fsa),c("entity_id","fsa","measure_t")],
                                 df_clsa_fup2c,df_clsa_fup2t))

#For ids with multiple different FSAs, take the most recent FSA
id_fsa_all<-id_fsa_all %>%  
  group_by(entity_id) %>% 
  arrange(desc(measure_t),.by_group = T) %>% 
  slice(1) %>% #select first row of each entity_id group (most recent)
  ungroup()

# Prepare for merge with main df
id_fsa_all$urban_impute<-case_when(substr(id_fsa_all$fsa,2,2) == 0 ~ "Rural",
                                   substr(id_fsa_all$fsa,2,2) %in% 1:9 ~ "Urban",
                                   TRUE ~ NA)

id_fsa_all$province_impute<-province_fun(id_fsa_all$fsa)

id_fsa_all<-id_fsa_all[id_fsa_all$entity_id %in% missing_fsa_id,] #n = 4897

#Merge baseline, follow up 1, follow up 2 datasets with antibody dataset
df_all_clsa<-merge(df_all_clsa,id_fsa_all[,c("entity_id","province_impute","urban_impute")],
            by = "entity_id",all.x = T)

df_all_clsa$province_anti<-ifelse(is.na(df_all_clsa$province_anti),
                                  df_all_clsa$province_impute,
                                  df_all_clsa$province_anti)

df_all_clsa$urban<-ifelse(is.na(df_all_clsa$urban),
                          df_all_clsa$urban_impute,
                          df_all_clsa$urban)

#Merge follow up 1 datasets with id and quintmat/quintsoc to antibody dataset
fup1<-rbind(df_clsa_fup1c,df_clsa_fup1t)

# Check no participant is in both the comprehensive and tracking cohort
fup1 %>% group_by(entity_id) %>% filter(n() > 1) # 0

df_all_clsa<-merge(df_all_clsa,fup1[,c("entity_id","quintmat","quintsoc")],
            by = "entity_id",all.x = T)

#Code missing values with NA
df_all_clsa<-df_all_clsa %>% 
  mutate(quintmat = ifelse(quintmat == -88888,NA,
                           quintmat),
         quintsoc = ifelse(quintsoc == -88888,NA,
                           quintsoc))

#sensitivity analysis: keep questionnaire + serology participants together
df_all_clsa_qab <- df_all_clsa #19334

#primary analysis: restrict to antibody cohort that provided a serology sample
df_all_clsa <- df_all_clsa[df_all_clsa$SER_NUCLEOCAPSID_COV >= 0 |
                             df_all_clsa$SER_SPIKE_COV >= 0,] #17311

#generate final dfs
clsa_df<-df_all_clsa %>% 
  select(entity_id,age_groups,sex,province_anti,urban,race,month,
         sampledate,SER_AGE_COV,race1,quintmat,quintsoc) %>% 
  filter(province_anti != "YT")#n = 17310
colnames(clsa_df)[4]<-"province"

clsa_df_qab <-df_all_clsa_qab %>% 
  select(entity_id,age_groups,sex,province_anti,urban,race,month,
         sampledate,SER_AGE_COV,race1,quintmat,quintsoc) %>% 
  filter(province_anti != "YT")#n = 19332
colnames(clsa_df_qab)[4]<-"province"

#Generate counts by age-sex-urban strata
clsa_asu<-clsa_df %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

clsa_asu_qab<-clsa_df_qab %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata
clsa_allu<-clsa_df %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

clsa_allu_qab <- clsa_df_qab %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by age-sex, sex strata and combine
clsa_asall<-clsa_df %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  mutate(urban = "All regions") %>% 
  ungroup()

clsa_asall_qab<-clsa_df_qab %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  mutate(urban = "All regions") %>% 
  ungroup()

clsa_allsu<-clsa_df %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

clsa_allsu_qab<-clsa_df_qab %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

clsa_asu<-do.call("rbind",list(clsa_asu,clsa_allu,clsa_asall,clsa_allsu))

clsa_asu_qab<-do.call("rbind",list(clsa_asu_qab,clsa_allu_qab,clsa_asall_qab,clsa_allsu_qab))

#Counts by age-sex-race
#Generate counts by age-sex-race strata
clsa_asr<-clsa_df %>% 
  filter(!is.na(race) & race != "pnts") %>%
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

clsa_asr_qab<-clsa_df_qab %>% 
  filter(!is.na(race) & race != "pnts") %>%
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
clsa_allr<-clsa_df  %>% 
  filter(!is.na(race) & race != "pnts") %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

clsa_allr_qab<-clsa_df_qab  %>% 
  filter(!is.na(race) & race != "pnts") %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

clsa_asr<-rbind(clsa_asr,clsa_allr)

clsa_asr_qab<-rbind(clsa_asr_qab,clsa_allr_qab)

#Generate counts by sex-quintmat strata
clsa_sqm<-clsa_df %>% 
  filter(!is.na(quintmat)) %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

clsa_sqm_qab<-clsa_df_qab %>% 
  filter(!is.na(quintmat)) %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-quintsoc strata
clsa_sqs<-clsa_df %>% 
  filter(!is.na(quintsoc)) %>% 
  group_by(sex,quintsoc) %>% 
  summarize(count = n()) %>% 
  ungroup()

clsa_sqs_qab<-clsa_df_qab %>% 
  filter(!is.na(quintsoc)) %>% 
  group_by(sex,quintsoc) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Sensitivity analysis 1: generate alternative age-sex-race counts when mixed race classified as "White"
#Generate counts by age-sex-race1 strata
clsa_asr1<-clsa_df %>% 
  filter(!is.na(race1) & race1 != "pnts") %>%
  group_by(age_groups,sex,race1) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
clsa_allr1<-clsa_df  %>% 
  filter(!is.na(race1) & race1 != "pnts") %>% 
  group_by(sex,race1) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
clsa_asr1<-rbind(clsa_asr1,clsa_allr1)

#save to .csv
#write_csv(clsa_asu,"./1_data/private/clsa_asu_final.csv")
#write_csv(clsa_asr,"./1_data/private/clsa_asr_final.csv")
#write_csv(clsa_df,"./1_data/private/clsa_df_final.csv")
#write_csv(clsa_asr1,"./1_data/private/clsa_asr1_final.csv")
#write_csv(clsa_sqm,"./1_data/private/clsa_sqm_final.csv")
#write_csv(clsa_sqs,"./1_data/private/clsa_sqs_final.csv")
#write_csv(clsa_asu_qab,"./1_data/private/clsa_asu_final_qab.csv")
#write_csv(clsa_asr_qab,"./1_data/private/clsa_asr_final_qab.csv")
#write_csv(clsa_df_qab,"./1_data/private/clsa_df_final_qab.csv")
#write_csv(clsa_sqm_qab,"./1_data/private/clsa_sqm_final_qab.csv")
#write_csv(clsa_sqs_qab,"./1_data/private/clsa_sqs_final_qab.csv")

# 6. Probabilistic Survey 3 (CanPath) ------------------------------------------------
canpath_data<-canpath_data[,c("ResearcherID","C_ADM_STUDY_DATASET","C1_SDC_AGE", "C1_SDC_SEX", "C1_ADM_FSA",
                              "C1_SDC_EB_ARAB","C1_SDC_EB_BLACK","C1_SDC_EB_CHINESE",
                              "C1_SDC_EB_FILIPINO","C1_SDC_EB_JAPANESE",
                              "C1_SDC_EB_KOREAN","C1_SDC_EB_LATIN","C1_SDC_EB_S_ASIAN",
                              "C1_SDC_EB_SE_ASIAN","C1_SDC_EB_W_ASIAN","C1_SDC_EB_WHITE",
                              "C1_SDC_EB_OTHER", "C1_SDC_EB_OTHER_OTSP","C1_SDC_EB_CA")]

#Create data.frame with questionnaire demographic variables, serology administrative variables, 
## and serology results for COVID-19 sub-study participants
canpath_data<-merge(canpath_data,
                    canpath_seradmin[,c("ResearcherID","C1_ADM_COLLECT_DATE")],by = "ResearcherID",all.x = T)

canpath_data<-merge(canpath_serres[,c("ResearcherID","C1_SAMPLE_ANTIGEN_TESTED","C1_SAMPLE_RESULTS_DESCRIPTION",
                                      "C1_SAMPLE_SUGGESTED_STATUS")],
                    canpath_data,by = "ResearcherID",all.x = T)

#Create urban variable denoting urban or rural residence
canpath_data$urban<-case_when(substr(canpath_data$C1_ADM_FSA,2,2) == 0 ~ "Rural",
                              substr(canpath_data$C1_ADM_FSA,2,2) %in% 1:9 ~ "Urban",
                              TRUE ~ NA)
#Create age groups variable
canpath_data$age_groups<-age_groups_fun(canpath_data$C1_SDC_AGE)

#Clean sampledate by changing all "/" to "-" and transform all m-d-y elements to y-m-d.
canpath_data$C1_ADM_COLLECT_DATE<-gsub("/","-", canpath_data$C1_ADM_COLLECT_DATE)
canpath_data$sampledate<-as.Date(parse_date_time(canpath_data$C1_ADM_COLLECT_DATE,
                                                 c("%y-%m-%d","%m-%d-%y")),tz = "UTC")

#Bin date of sample collection into two month buckets
canpath_data$month<-floor_date(canpath_data$sampledate, unit = "2 months")

#Re-label sex variable
canpath_data$sex<-case_when(
  canpath_data$C1_SDC_SEX == 0 ~ "Male",
  canpath_data$C1_SDC_SEX == 1 ~ "Female"
)

#Clean fsa variable & replace FSA with Canadian province or territory
canpath_data$C1_ADM_FSA<-toupper(canpath_data$C1_ADM_FSA) #convert all fsas to uppercase
canpath_data$province<-province_fun(canpath_data$C1_ADM_FSA)

# Impute NA province with province of sub-study. Assume the participant resides
# in the province where the sub-study is taking place.
canpath_data$province1 <- case_when(
  is.na(canpath_data$province) & canpath_data$C_ADM_STUDY_DATASET == "ATL" ~ "ATL", #must put ATL because we do not know specific province
  is.na(canpath_data$province) & canpath_data$C_ADM_STUDY_DATASET == "ATP" ~ "AB",
  is.na(canpath_data$province) & canpath_data$C_ADM_STUDY_DATASET == "BCGP" ~ "BC",
  is.na(canpath_data$province) & canpath_data$C_ADM_STUDY_DATASET == "CAG" ~ "QC",
  is.na(canpath_data$province) & canpath_data$C_ADM_STUDY_DATASET == "OHS" ~ "ON",
  !is.na(canpath_data$province) ~ canpath_data$province,
  TRUE ~ NA
)

#Classify ethnicity as white or racialized minority
canpath_data$race<-NULL
canpath_data$race1<-NULL
for(i in 1:nrow(canpath_data)){
  #Participant preferred not to state ethnicity
  if(canpath_data[i,22] %in% 8){
    canpath_data$race[i]<-"pnts"
  
  #Participant provided a response  
  } else{
    #Participant selected a response associated with european ethnicity(white)
    if(canpath_data[i,19] %in% 1){
      if(sum(canpath_data[i,9:18] %in% 1)>0){#also selected rm
        canpath_data$race[i]<-"Racialized minority"
      #Participant only selected european ethnicities
      } else{
        canpath_data$race[i]<-"White"
      }
    
    #Participants who did not select a european (white) ethnicity  
    } else if(sum(canpath_data[i,9:18] %in% 1)>0){
        canpath_data$race[i]<-"Racialized minority"
      } else{
      #Else, classify as missing
      canpath_data$race[i]<-"Missing"
    }
    
  }
  
  #Check text box responses of individuals who also used mark in
  if(!is.na(canpath_data[i,21])){ #used text box
    if(canpath_data$race[i] != "Missing"){ #race != missing
      if((canpath_data$race[i] == "White" & 
          canpath_data[i,21] %in% rm_indn)==T){ #select white and text box rm
        canpath_data$race[i]<-"Racialized minority"
      } else{} #leave the same
    
    #Race missing and text box used  
    } else if(canpath_data$race[i] == "Missing"){
      if(canpath_data[i,21] %in% w){
        canpath_data$race[i]<-"White"
      } else if(canpath_data[i,21] %in% rm_indn){
        canpath_data$race[i]<-"Racialized minority"
      } else{#no response, still missing
        canpath_data$race[i]<-"Missing"
      }
    } else{}
  }
  else{} #tb not used, keep the same
  
}

#Sensitivity analysis 1
for(i in 1:nrow(canpath_data)){
  #Participant preferred not to state ethnicity
  if(canpath_data[i,22] %in% 8){
    canpath_data$race1[i]<-"pnts"
 
   #Participant provided a response  
   } else{
    #Participant selected a response associated with racialized minority
    if(sum(canpath_data[i,9:18] %in% 1)>0){
      #Participant selected a response associated with european ethnicity(white)
      if(canpath_data[i,19] %in% 1){
        canpath_data$race1[i]<-"White"
      
      #Participant only selected racialized minority ethnicities
      } else{
        canpath_data$race1[i]<-"Racialized minority"
      }
     
      #Participants who did not select a racialized minority ethnicity 
    } else if(canpath_data[i,19] %in% 1){
      #Selected white ethnicity
        canpath_data$race1[i]<-"White"
      } else{
      #Else, classify as missing
      canpath_data$race1[i]<-"Missing"
    }
    
  }#close participant response else portion
  
  #Check text box responses of individuals who also used mark in
  if(!is.na(canpath_data[i,21])){ #used text box
    if(canpath_data$race1[i] != "Missing"){ #race1 != missing
      if((canpath_data$race1[i] == "Racialized minority" & 
          canpath_data[i,21] %in% w)==T){ #select white and text box rm
        canpath_data$race1[i]<-"White"
      }  else{} #leave the same
    #Race missing and text box used
    } else if(canpath_data$race1[i] == "Missing"){
      if(canpath_data[i,21] %in% w){
        canpath_data$race1[i]<-"White"
      } else if(canpath_data[i,21] %in% rm_indn){
        canpath_data$race1[i]<-"Racialized minority"
      } else{#no response, still missing
        canpath_data$race1[i]<-"Missing"
      }
    }  else{}
  } else{} #tb not used, keep the same
  
}

#Main analysis: use df with imputed province
#Remove individuals with unknown province or who do not reside in a regional cohort.
canpath_data<-canpath_data %>% 
  filter(!is.na(province1) & province1 != "SK" & 
           province1 != "YT") %>% 
  mutate(race = ifelse(race == "Missing",NA,race),
         race1 = ifelse(race1 == "Missing",NA,race1)) #n = 72787

#Sensitivity analysis: Do not use imputed province. Remove individuals with unknown 
#province or who do not reside in a regional cohort.
canpath_datas4<-canpath_data %>% 
  filter(!is.na(province) & province != "SK" & 
           province != "YT") %>% 
  mutate(race = ifelse(race == "Missing",NA,race),
         race1 = ifelse(race1 == "Missing",NA,race1)) #n = 65587

#Check all participants provided a serology sample
canpath_data %>% 
  group_by(ResearcherID) %>% 
  summarize(Any_Sero = any(C1_SAMPLE_RESULTS_DESCRIPTION %in% c(1:3)) == TRUE) %>% 
  filter(Any_Sero == FALSE) #confirmed

canpath_datas4 %>% 
  group_by(ResearcherID) %>% 
  summarize(Any_Sero = any(C1_SAMPLE_RESULTS_DESCRIPTION %in% c(1:3)) == TRUE) %>% 
  filter(Any_Sero == FALSE) #confirmed

#Remove duplicate rows with the same ID at the same sampledate. This prevents over-estimating the 
# sample count when calculating proportion of specimens donated by each strata.
can_df<-canpath_data %>% distinct(ResearcherID,sampledate,age_groups,sex,province1,urban,month,race)#n = 25156
can_df1<-canpath_data %>% distinct(ResearcherID,sampledate,age_groups,sex,province1,urban,month,race1)
can_dfs4<-canpath_datas4 %>% distinct(ResearcherID,sampledate,age_groups,sex,province,urban,month,race)#n = 21720

#Generate counts by age-sex-urban strata
can_asu<-can_df %>%
  filter(!is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>%
  summarize(count = n()) %>% 
  ungroup()

can_asus4 <- can_dfs4 %>% 
  group_by(age_groups,sex,urban) %>%
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata
can_allu<-can_df %>% 
  filter(!is.na(urban)) %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

can_allus4<-can_dfs4 %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by age-sex, sex strata and combine
can_asall<-can_df %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  mutate(urban = "All regions") %>% 
  ungroup()

can_asalls4<-can_dfs4 %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  mutate(urban = "All regions") %>% 
  ungroup()

can_allsu<-can_df %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

can_allsus4<-can_dfs4 %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

can_asu<-do.call("rbind",list(can_asu,can_allu,can_asall,can_allsu))
can_asus4<-do.call("rbind",list(can_asus4,can_allus4,can_asalls4,can_allsus4))

#Generate counts by age-sex-race strata
can_asr<-can_df %>% 
  filter(race != "pnts" & !is.na(race)) %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

can_asrs4<-can_dfs4 %>% 
  filter(race != "pnts" & !is.na(race)) %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
can_allr<-can_df %>% 
  filter(race != "pnts" & !is.na(race)) %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

can_allrs4<-can_dfs4 %>% 
  filter(race != "pnts" & !is.na(race)) %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

can_asr <- rbind(can_asr, can_allr)
can_asrs4<-rbind(can_asrs4,can_allrs4)

#Sensitivity analysis 1: generate alternative age-sex-race counts when mixed race classified as "White"
#Generate counts by age-sex-race1 strata
can_asr1<-can_df1 %>% 
  filter(race1 != "pnts" & !is.na(race1)) %>% 
  group_by(age_groups,sex,race1) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
can_allr1<-can_df1 %>% 
  filter(race1 != "pnts" & !is.na(race1)) %>% 
  group_by(sex,race1) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
can_asr1<-rbind(can_asr1,can_allr1)

#Save to .csv
#write_csv(can_asu,"./1_data/private/can_asu_final.csv")
#write_csv(can_asr,"./1_data/private/can_asr_final.csv")
#write_csv(can_df,"./1_data/private/can_df_final.csv")
#write_csv(can_asr1,"./1_data/private/can_asr1_final.csv")
#write_csv(can_df1,"./1_data/private/can_df1_final.csv")
#write_csv(can_asus4,"./1_data/private/can_asu_finals4.csv")
#write_csv(can_asrs4,"./1_data/private/can_asr_finals4.csv")
#write_csv(can_dfs4,"./1_data/private/can_df_finals4.csv")

# 7. 2016 Canadian census ----------------------------------------------------
#Clean census data
census<-lapply(census,census_fun)
census_alt<-lapply(census_alt,census_fun)

#Census counts by age-sex-urban
#Setting A: 10 provinces, 18+ (Ab-c)
census_a<-census[[1]] %>% 
  filter(age_groups != "0-17 years" & 
          province != "NU" & province != "NT" & province != "YT") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_a_all<-census_a %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_a_all$age_groups<-"All ages"

census_a_allu<-census_a %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F) %>% 
  mutate(urban = "All regions")

census_a_alls<-census_a %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_a<-do.call("rbind",list(census_a,census_a_all,census_a_allu,census_a_alls))
#write_csv(census_a,"./1_data/private/2016 Canadian Census/censusasu_a_abc.csv")

#Setting B: 10 provinces, all ages (CCAHS)

#Setting C: 9 provinces (no Quebec), 18+ (CBS)
census_c<-census[[1]] %>% 
  filter(province != "QC"  & 
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_c_all<-census_c %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_c_all$age_groups<-"All ages"

census_c_allu<-census_c %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F) %>% 
  mutate(urban = "All regions")

census_c_alls<-census_c %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_c<-do.call("rbind",list(census_c,census_c_all,census_c_allu,census_c_alls))
#write_csv(census_c,"./1_data/private/2016 Canadian Census/censusasu_c_cbs.csv")

#Setting D: 9 provinces (no Saskatchewan), 18+ (Canpath)
# Main analysis uses df without indigenous-identifying participants
census_d<-census_alt[[1]] %>% 
  filter(province != "SK" & 
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_d_all<-census_d %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_d_all$age_groups<-"All ages"

census_d_allu<-census_d %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F) %>% 
  mutate(urban = "All regions")

census_d_alls<-census_d %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_d<-do.call("rbind",list(census_d,census_d_all,census_d_allu,census_d_alls))
#write_csv(census_d,"./1_data/private/2016 Canadian Census/censusasu_d_canpath.csv")

#Setting E: Alberta, all ages (APL)
census_e<-census[[1]] %>% 
  filter(province == "AB") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_e_all<-census_e %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_e_all$age_groups<-"All ages"

census_e_allu<-census_e %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F) %>% 
  mutate(urban = "All regions")

census_e_alls<-census_e %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_e<-do.call("rbind",list(census_e,census_e_all,census_e_allu,census_e_alls))
#write_csv(census_e,"./1_data/private/2016 Canadian Census/censusasu_e_apl.csv")

#Setting G: 10 provinces, 47+ (CLSA)
# Main analysis uses df without indigenous-identifying participants
census_g<-census_alt[[1]] %>%
  filter((age_groups == "47-56 years" | 
           age_groups == "57+ years") & 
           province != "NU" & province != "NT" & province != "YT") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_g_all<-census_g %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_g_all$age_groups<-"All ages"

census_g_allu<-census_g %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F) %>% 
  mutate(urban = "All regions")

census_g_alls<-census_g %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_g<-do.call("rbind",list(census_g,census_g_all,census_g_allu,census_g_alls))
#write_csv(census_g,"./1_data/private/2016 Canadian Census/censusasu_g_clsa.csv")

#Census counts by age-sex-race
#Setting A: 10 provinces, 18+ (Ab-c)
census_ar<-census[[2]] %>%
  filter(age_groups != "0-17 years" & 
           province != "NU" & province != "NT" & province != "YT") %>% 
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
census_ar_all<-census_ar %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
census_ar_all$age_groups<-"All ages"
census_ar<-rbind(census_ar,census_ar_all)
#write_csv(census_ar,"./1_data/private/2016 Canadian Census/censusasr_a_abc.csv")

#Setting C: 9 provinces (no Quebec), 18+ (CBS)
census_cr<-census[[2]] %>% 
  filter(province != "QC" & 
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
census_cr_all<-census_cr %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
census_cr_all$age_groups<-"All ages"
census_cr<-rbind(census_cr,census_cr_all)
#write_csv(census_cr,"./1_data/private/2016 Canadian Census/censusasr_c_cbs.csv")

#Setting D: 9 provinces (no Saskatchewan), 18+ (Canpath)
census_dr<-census_alt[[2]] %>% 
  filter(province != "SK" & 
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
census_dr_all<-census_dr %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
census_dr_all$age_groups<-"All ages"
census_dr<-rbind(census_dr,census_dr_all)
#write_csv(census_dr,"./1_data/private/2016 Canadian Census/censusasr_d_canpath.csv")

#Setting G: 10 provinces, 47+ (CLSA)
census_gr<-census_alt[[2]] %>%
  filter((age_groups == "47-56 years" | 
           age_groups == "57+ years") & 
           province != "NU" & province != "NT" & province != "YT") %>% 
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
census_gr_all<-census_gr %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
census_gr_all$age_groups<-"All ages"
census_gr<-rbind(census_gr,census_gr_all)
#write_csv(census_gr,"./1_data/private/2016 Canadian Census/censusasr_g_clsa.csv")

#Census counts by sex-quintmat

#Setting C: 9 provinces (no Quebec), 18+ (CBS)
census_cqm<-census[[3]] %>% 
  filter(province != "QC" &
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years" & quintmat != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
#write_csv(census_cqm,"./1_data/private/2016 Canadian Census/censussqm_c_cbs.csv")

#Setting E:  Alberta, all ages (APL)
census_eqm<-census[[3]] %>% 
  filter(province == "AB" & quintmat != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
#write_csv(census_eqm,"./1_data/private/2016 Canadian Census/censussqm_e_apl.csv")

#Setting G: CLSA, 47+ (10 provinces)
census_gqm<-census_alt[[3]] %>% 
  filter((age_groups == "47-56 years" | 
           age_groups == "57+ years") & 
           province != "NU" & province != "NT" & 
           province != "YT" & quintmat != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
#write_csv(census_gqm,"./1_data/private/2016 Canadian Census/censussqm_g_clsa.csv")

#Census counts by sex-quintsoc
#Setting B: 10 provinces, all ages (CCAHS)

#Setting C: 9 provinces (no Quebec), 18+ (CBS)
census_cqs<-census[[4]] %>% 
  filter(province != "QC" &
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years" &
           quintsoc != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintsoc,
            FUN = sum,
            drop = F)
#write_csv(census_cqs,"./1_data/private/2016 Canadian Census/censussqs_c_cbs.csv")

#Setting E:  Alberta, all ages (APL)
census_eqs<-census[[4]] %>% 
  filter(province == "AB" & quintsoc != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintsoc,
            FUN = sum,
            drop = F)
#write_csv(census_eqs,"./1_data/private/2016 Canadian Census/censussqs_e_apl.csv")

#Setting G: CLSA, 47+ (10 provinces)
census_gqs<-census_alt[[4]] %>% 
  filter((age_groups == "47-56 years" | 
           age_groups == "57+ years") & 
           province != "NU" & province != "NT" & 
           province != "YT" & quintsoc != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintsoc,
            FUN = sum,
            drop = F)
#write_csv(census_gqs,"./1_data/private/2016 Canadian Census/censussqs_g_clsa.csv")

#Sensitivity analysis #2: calculate Canpath and CLSA rep_ratios using
# census datasets which include indigenous counts
#Age-sex-urban
#Setting D: 9 provinces (no Saskatchewan), 18+ (Canpath)
census_ds2<-census[[1]] %>% 
  filter(province != "SK" & 
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_ds2_all<-census_ds2 %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_ds2_all$age_groups<-"All ages"

census_ds2_allu<-census_ds2 %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F) %>% 
  mutate(urban = "All regions")

census_ds2_alls<-census_ds2 %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_ds2<-do.call("rbind",list(census_ds2,census_ds2_all,census_ds2_allu,census_ds2_alls))
#write_csv(census_ds2,"./1_data/private/2016 Canadian Census/censusasu_d_canpath_s2.csv")

#Setting G: 10 provinces, 47+ (CLSA)
census_gs2<-census[[1]] %>%
  filter((age_groups == "47-56 years" | 
           age_groups == "57+ years") & 
           province != "NU" & province != "NT" & province != "YT") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_gs2_all<-census_gs2 %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_gs2_all$age_groups<-"All ages"

census_gs2_allu<-census_gs2 %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F) %>% 
  mutate(urban = "All regions")

census_gs2_alls<-census_gs2 %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_gs2<-do.call("rbind",list(census_gs2,census_gs2_all,census_gs2_allu,census_gs2_alls))
#write_csv(census_gs2,"./1_data/private/2016 Canadian Census/censusasu_g_clsa_s2.csv")

#Age-sex-race
#Setting D: 9 provinces (no Saskatchewan), 18+ (Canpath)
census_drs2<-census[[2]] %>% 
  filter(province != "SK" & 
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
census_drs2_all<-census_drs2 %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
census_drs2_all$age_groups<-"All ages"
census_drs2<-rbind(census_drs2,census_drs2_all)
#write_csv(census_drs2,"./1_data/private/2016 Canadian Census/censusasr_d_canpath_s2.csv")

#Setting G: 10 provinces, 47+ (CLSA)
census_grs2<-census[[2]] %>%
  filter((age_groups == "47-56 years" | 
           age_groups == "57+ years") & 
           province != "NU" & province != "NT" & province != "YT") %>% 
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
census_grs2_all<-census_grs2 %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
census_grs2_all$age_groups<-"All ages"
census_grs2<-rbind(census_grs2,census_grs2_all)
#write_csv(census_grs2,"./1_data/private/2016 Canadian Census/censusasr_g_clsa_s2.csv")

#Sex-quintmat
#Setting G: CLSA, 47+ (10 provinces)
census_gqm2<-census[[3]] %>% 
  filter((age_groups == "47-56 years" | 
            age_groups == "57+ years") & 
           province != "NU" & province != "NT" & 
           province != "YT" & quintmat != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
#write_csv(census_gqm2,"./1_data/private/2016 Canadian Census/censussqm_g_clsa_s2.csv")

#Sex-quintsoc
#Setting G: CLSA, 47+ (10 provinces)
census_gqs2<-census[[4]] %>% 
  filter((age_groups == "47-56 years" | 
            age_groups == "57+ years") & 
           province != "NU" & province != "NT" & 
           province != "YT" & quintsoc != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintsoc,
            FUN = sum,
            drop = F)
#write_csv(census_gqs2,"./1_data/private/2016 Canadian Census/censussqs_g_clsa_s2.csv")

# 8. Create summary table for supplement -------------------------------------
#Clean variables - need consistent levels (turn all NAs into "missing")
cbs_dfs<-read.csv("1_data/private/cbs_df_final.csv") %>% 
  mutate(cohort = "CBS blood donor")

#Replace NAs with "Missing"
cbs_dfs[,c("race", "urban","quintmat","quintsoc")]<-lapply(cbs_dfs[,c("race", "urban","quintmat","quintsoc")],sup_fun)

apl_dfs<-read.csv("1_data/private/apl_df_final.csv") %>% 
  mutate(cohort = "APL outpatient laboratory",
         sex = ifelse(is.na(sex),"Missing",sex),
         age_groups = ifelse(is.na(age_groups),"Missing",age_groups),
         race = "Missing") #Added only for visualization

apl_dfs[,c("urban","quintmat","quintsoc")]<-lapply(apl_dfs[,c("urban","quintmat","quintsoc")],sup_fun)

abc_dfs<-read.csv("1_data/private/abc_df_final.csv") %>% 
  filter(province != "YT") %>% 
  mutate(cohort = "Ab-c open cohort",
         quintmat = "Missing",
         quintsoc = "Missing",
         sex = ifelse(sex == "Self described", "Missing", sex)) #Added only for visualization

abc_dfs[,c("urban","race")]<-lapply(abc_dfs[,c("urban", "race")],sup_fun)

clsa_dfs<-read.csv("1_data/private/clsa_df_final.csv") %>% 
  mutate(cohort = "CLSA closed cohort")

clsa_dfs[,c("race","quintmat","quintsoc")]<-lapply(clsa_dfs[,c("race","quintmat","quintsoc")],sup_fun)

can_dfs<-read.csv("1_data/private/can_df_final.csv") %>% 
  mutate(cohort = "Canpath closed cohort",
         quintmat = "Missing",
         quintsoc = "Missing")

can_dfs[,c("race", "urban")]<- lapply(can_dfs[,c("race", "urban")], sup_fun)

all_dfs<-do.call("rbind",list(cbs_dfs[,c("age_groups","sex","urban","race","quintmat","quintsoc","cohort")],
                              apl_dfs[,c("age_groups","sex","urban","race","quintmat","quintsoc","cohort")],
                              abc_dfs[,c("age_groups","sex","urban","race","quintmat","quintsoc","cohort")],
                              clsa_dfs[,c("age_groups","sex","urban","race","quintmat","quintsoc","cohort")],
                              can_dfs[,c("age_groups","sex","urban","race","quintmat","quintsoc","cohort")]))

#Change categorical vars into factors and assign levels for ordering
all_dfs<-all_dfs %>% 
  mutate(sex = factor(sex,levels = c("Female","Male","Missing")),
         race = factor(race,levels = c("Racialized minority","White","Missing")),
         urban = factor(urban,levels = c("Rural","Urban","Missing")),
         cohort = factor(cohort,levels = c("CBS blood donor","APL outpatient laboratory",
                                           "Ab-c open cohort","Canpath closed cohort",
                                           "CLSA closed cohort")))
#Add library(tableone) to top and modify below code
st<-CreateTableOne(data = all_dfs,vars = c("age_groups","quintmat",
                                           "quintsoc","race","sex","urban"),
                   strata = "cohort",test = F,smd = F)
#write.csv(print(st,quote = T,noSpaces = T),"4_output/summary_supplemental_table.csv")

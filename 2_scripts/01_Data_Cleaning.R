"This script cleans all 6 cohort datasets and prepares them
for analysis and plotting."

# Load data and functions ---------------------------------------------------------------
setwd("~/serosurveillance-cohort-representativeness/1_data/private")
library(haven)
library(lubridate)
library(tidyverse)# loads readr
library(DBI)
library(RPostgres)
library(readxl)

#CBS CITF Serology dataset import
#Test connection arguments
"con <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'cbs0', 
  host = '132.216.183.71', 
  port = 5432 
)

#Import dataset
cbs_data <- dbReadTable(con, SQL('students.copy_cbs_combined'))

#Disconnect from database once data is loaded into R
dbDisconnect(con)
write_csv(cbs_data,'cbs_unmodified_df_backup_jan222024.csv')"
cbs_data<-read.csv("cbs_unmodified_df_backup_jan222024.csv")

#APL dataset import
load("./APL/RFD4682e1.RData")
apl_data<-RFD4682_e1
rm(RFD4682_e1)

#Ab-C dataset import
abc_data<-read.csv("./Ab-c/df_047_hs_jha_phases1234.csv")

#CLSA dataset import
#Combined cohort
df_clsa_cb<-read.csv("./CLSA/2209005_McGill_ARussell_Covid_Combined_v1-1.csv")
#Antibody cohort
df_clsa_anti<-read.csv("./CLSA/2209005_McGill_ARussell_Covid_Antibody_Combined_NoIndigenousIdentifiers_v1.csv")

#Canpath dataset import
canpath_data<-read.csv("./CANPATH/DAO-543759_ResearcherDataset_Qx_96014par_1125var.csv")
canpath_mtp<-read.csv("./CANPATH/DAO-543759_ResearcherDataset_Qx_1114par_1127varMTP.csv")#manitoba cohort
canpath_seradmin<-read.csv("./CANPATH/DAO-543759_ResearcherDataset_Serology_Admin_25727par.csv")
canpath_serres<-read.csv("./CANPATH/DAO-543759_ResearcherDataset_Serology_Results_74503par.csv")

#Census dataset import - provinces & territories
casup<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censasup.xlsx")
casrp<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censasrp.xlsx")
csaqm<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censsqmp.xlsx")
csaqs<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censsqsp.xlsx")
cast<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censast.xlsx")

#Alternative census province datasets
casup_alt<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censasup_alt.xlsx")
casrp_alt<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censasrp_alt.xlsx")
csaqm_alt<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censsqmp_alt.xlsx")
csaqs_alt<-read_xlsx("2021 Canadian Census/10285/Sortie_Census/censsqsp_alt.xlsx")

#Package census datasets into lists
census<-list(casup,casrp,csaqm,csaqs,cast)
census_alt<-list(casup_alt,casrp_alt,csaqm_alt,csaqs_alt)

#Census dataset 2 import (sensitivity analysis 1)
#XXXX

#Load functions
source("../../2_scripts/00_Helper_Functions.R")

# Data cleaning -----------------------------------------------------------
# -- Each dataset should have the following format --
# Age: character
# Sex: character
# Urban: character
# Material and social deprivation quintile: numeric
# Race: character
# Province: character
# --   -------------------------------------------------------------------

# Blood Donor (CBS) -------------------------------------------------------
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

#Categorize race as white, racialized minority, or missing
cbs_data$race<-case_when(
  cbs_data$ethnic1 == "0 missing" ~ "Missing",
  cbs_data$ethnic1 == "0 Missing" ~ "Missing",
  cbs_data$ethnic1 == "1 White" ~ "White",
  cbs_data$ethnic1 == "2 Aborigin" ~ "Racialized minority",
  cbs_data$ethnic1 == "2 Aboriginal" ~ "Racialized minority",
  cbs_data$ethnic1 == "3 Asian" ~ "Racialized minority",
  cbs_data$ethnic1 == "4 Others" ~ "Racialized minority",
  cbs_data$ethnic1 == "4 Other" ~ "Racialized minority",
)

#Create urban variable denoting urban or rural residence
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
nrow(cbs_data[is.na(cbs_data$cur_result_n) & is.na(cbs_data$cur_result_s),]) #0 - all participants have at least 1 serology result
cbs_df<-cbs_data %>% 
  select(pid,sampledate,sex,race,urban,quintmat,
         quintsoc,province,month,age_groups,fsa) %>% 
  filter(province != "YT" & province != "NU/NT" & province != "QC")#n = XX
cbs_df<-cbs_df %>% filter(age_groups != "0-17 years") #n = XXX

#Generate counts by age-sex-urban strata
cbs_asu<-cbs_df %>%
  filter(!is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>% #n = XX
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata
cbs_allu<-cbs_df %>% 
  filter(!is.na(urban)) %>%
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by sex strata and combine
cbs_allsu<-cbs_df %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
cbs_asu<-do.call("rbind",list(cbs_asu,cbs_allu,cbs_allsu))

#Generate counts by age-sex-race strata
cbs_asr<-cbs_df %>%  #n = XX
  filter(race != "Missing") %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
cbs_allr<-cbs_df %>% 
  filter(race != "Missing") %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
cbs_asr<-rbind(cbs_asr,cbs_allr)

#Generate counts by sex-quintmat strata
cbs_sqm<-cbs_df %>%  #n = XX
  filter(!is.na(quintmat)) %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex strata and combine
cbs_alls<-cbs_df %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(quintmat = "All quintiles") %>% 
  ungroup()
cbs_sqm<-rbind(cbs_sqm,cbs_alls)

#Generate counts by sex-quintsoc strata
cbs_sqs<-cbs_df %>% #n = XX
  filter(!is.na(quintsoc)) %>% 
  group_by(sex,quintsoc) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex strata and combine
cbs_allss<-cbs_df %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(quintsoc = "All quintiles") %>% 
  ungroup()
cbs_sqs<-rbind(cbs_sqs,cbs_allss)

#Save to .csv
write_csv(cbs_asu,"./cbs_asu_jan222024.csv")
write_csv(cbs_asr,"./cbs_asr_jan222024.csv")
write_csv(cbs_sqm,"./cbs_sqm_jan222024.csv")
write_csv(cbs_sqs,"./cbs_sqs_jan222024.csv")
write_csv(cbs_df,"./cbs_df_jan222024.csv")

# Outpatient Laboratory (APL) ---------------------------------------------
#Remove duplicate entries and regenerate record ID (order_ID)
apl_data<-apl_data %>% 
  filter(order_ID != 1253 & order_ID != 1521 & order_ID != 2728 & order_ID != 3247) %>% 
  mutate(order_ID = 1:214776) #n =

#Check all participants have at least 1 serology result
nrow(apl_data[is.na(apl_data$`N-IgG_INTERP`) & is.na(apl_data$`RBD-IgGII_INTERP`),]) #0

#Remove individuals without a participant ID
apl_data<-apl_data[!is.na(apl_data$clean_IDe),] #n = XX

#Remove participants residing outside of Alberta and generate urban variable
apl_data<-apl_data %>% 
  mutate(PAT_FSA = ifelse(
    substr(PAT_FSA,2,2) == "O", 
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

#Generate final df
apl_df<-apl_data %>% 
  select(clean_IDe,age_groups,GENDER,urban,province,month,
         COLLECTION_DATE,QUINTMAT,QUINTSOC,PAT_FSA)
colnames(apl_df)<-c("pid","age_groups","sex","urban","province","month",
                    "sampledate","quintmat","quintsoc","fsa") #n = XX

#Generate counts by age-sex-urban strata
apl_asu<-apl_df %>%  #n = XX
  filter(sex != "Unknown" &
           !is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup() 

#Generate counts by sex-urban strata
apl_allu<-apl_df %>% 
  filter(sex != "Unknown" &
           !is.na(urban)) %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by sex strata and combine
apl_allsu<-apl_df %>% 
  filter(sex != "Unknown") %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()
apl_asu<-do.call("rbind",list(apl_asu,apl_allu,apl_allsu))

#Generate counts by sex-quintmat strata
apl_sqm<-apl_df %>%  #n = 168126
  filter(sex != "Unknown" & !is.na(quintmat)) %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex strata and combine
apl_alls<-apl_df %>% 
  filter(sex != "Unknown") %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(quintmat = "All quintiles") %>% 
  ungroup()
apl_sqm<-rbind(apl_sqm,apl_alls)

#Generate counts by sex-quintsoc strata
apl_sqs<-apl_df %>% #n = 168126
  filter(sex != "Unknown" & !is.na(quintsoc)) %>% 
  group_by(sex,quintsoc) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex strata and combine
apl_allss<-apl_df %>% 
  filter(sex != "Unknown") %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(quintsoc = "All quintiles") %>% 
  ungroup()
apl_sqs<-rbind(apl_sqs,apl_allss)

#Save to .csv
write_csv(apl_asu,"./apl_asu_jan222024.csv")
write_csv(apl_sqm,"./apl_sqm_jan222024.csv")
write_csv(apl_sqs,"./apl_sqs_jan22024.csv")
write_csv(apl_df,"./apl_df_jan222024.csv")

# Probabilistic Survey 1 (Ab-c) ---------------------------------------

#Classify ethnicity as "white" or "racialized minority"
race<-NULL
race1<-NULL #for sensitivity analysis 1 - alternative race classification
names<-c("p1_ethnicity_1","p1_ethnicity_2",
         "p1_ethnicity_3","p1_ethnicity_4","p1_ethnicity_5","p1_ethnicity_6",
         "p1_ethnicity_7","p1_ethnicity_8","p1_ethnicity_9","p1_ethnicity_10",
         "p1_ethnicity_11","p1_ethnicity_12","p1_ethnicity_13","p1_ethnicity_14",
         "p1_ethnicity_15")

for(i in 1:nrow(abc_data)){
  race_i<-
    case_when(
      #select prefer not to say and nothing else
      sum(abc_data[i,names] %in% c(15)) > 0 & 
        (sum(is.na(abc_data[i,names])) == 14) ~ "pnts",
      #select only 1 white ethnicity
      sum(abc_data[i,names] %in% c(2,3,4,13)) > 0 &
        (sum(is.na(abc_data[i,names])) == 14) ~ "White", 
      #select only 1 racialized minority ethnicity
      sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) > 0 & 
        (sum(is.na(abc_data[i,names])) == 14) ~ "Racialized minority",
      #select only other as ethnicity - classify as racialized minority
      sum(abc_data[i,names] %in% c(14)) > 0 &
        (sum(is.na(abc_data[i,names])) == 14) ~ "Racialized minority",
      #select at least 1 white ethnicity and at least 1 racialized minority ethnicity
      sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) > 0 &
        sum(abc_data[i,names] %in% c(2,3,4,13)) > 0 &
        (sum(is.na(abc_data[i,names])) < 14) ~ "Racialized minority",
      #select at least 1 white ethnicity and select "other" ethnicity
      sum(abc_data[i,names] %in% c(2,3,4,13)) > 0 &
        sum(abc_data[i,names] %in% c(14)) > 0 &
        sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) == 0 &
        (sum(is.na(abc_data[i,names])) < 14) ~ "White",
      #select 2+ racialized minority ethnicities
      sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) > 1 &
        sum(abc_data[i,names] %in% c(2,3,4,13)) == 0 &
        (sum(is.na(abc_data[i,names])) < 14) ~ "Racialized minority",
      #select 2+ white ethnicities
      sum(abc_data[i,names] %in% c(2,3,4,13)) > 1 &
        sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) == 0 &
        sum(abc_data[i,names] %in% c(14)) == 0 &
        (sum(is.na(abc_data[i,names])) < 14) ~ "White",
      #select at least 1 racialized minority ethnicity and select "other" ethnicity
      sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) > 0 &
        sum(abc_data[i,names] %in% c(14)) > 0 &
        (sum(is.na(abc_data[i,names])) < 14) ~ "Racialized minority",
      TRUE ~ NA)
  race1_i<-case_when(
    #select prefer not to say and nothing else
    sum(abc_data[i,names] %in% c(15)) > 0 & 
      (sum(is.na(abc_data[i,names])) == 14) ~ "pnts",
    #select only 1 white ethnicity
    sum(abc_data[i,names] %in% c(2,3,4,13)) > 0 &
      (sum(is.na(abc_data[i,names])) == 14) ~ "White", 
    #select only 1 racialized minority ethnicity
    sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) > 0 & 
      (sum(is.na(abc_data[i,names])) == 14) ~ "Racialized minority",
    #select only other as ethnicity - classify as racialized minority
    sum(abc_data[i,names] %in% c(14)) > 0 &
      (sum(is.na(abc_data[i,names])) == 14) ~ "Racialized minority",
    #select at least 1 white ethnicity and at least 1 racialized minority ethnicity
    sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) > 0 &
      sum(abc_data[i,names] %in% c(2,3,4,13)) > 0 &
      (sum(is.na(abc_data[i,names])) < 14) ~ "White",
    #select at least 1 white ethnicity and select "other" ethnicity
    sum(abc_data[i,names] %in% c(2,3,4,13)) > 0 &
      sum(abc_data[i,names] %in% c(14)) > 0 &
      sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) == 0 &
      (sum(is.na(abc_data[i,names])) < 14) ~ "White",
    #select 2+ racialized minority ethnicities
    sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) > 1 &
      sum(abc_data[i,names] %in% c(2,3,4,13)) == 0 &
      (sum(is.na(abc_data[i,names])) < 14) ~ "Racialized minority",
    #select 2+ white ethnicities
    sum(abc_data[i,names] %in% c(2,3,4,13)) > 1 &
      sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) == 0 &
      sum(abc_data[i,names] %in% c(14)) == 0 &
      (sum(is.na(abc_data[i,names])) < 14) ~ "White",
    #select at least 1 racialized minority ethnicity and select "other" ethnicity
    sum(abc_data[i,names] %in% c(1,5,6,7,8,9,10,11,12)) > 0 &
      sum(abc_data[i,names] %in% c(14)) > 0 &
      (sum(is.na(abc_data[i,names])) < 14) ~ "Racialized minority",
    TRUE ~ NA)
  race<-c(race,race_i)
  race1<-c(race1,race1_i)
}
abc_data<-cbind(abc_data,race,race1)

#Period 1
abc_data1<-abc_data %>% 
  select(rseed,p1_result_sinai,p1_int_month,p1_province,p1_fsa,
         p1_age,p1_qe2,race,race1) %>% 
  #Remove individuals who did not provide a serology sample
  filter(p1_result_sinai != "" ) %>%  #n = 8955 
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
                    p1_qe2 == 3 ~ "Self described")
     )

#Categorize date sample received into 2 month time buckets
abc_data1$month<-floor_date(abc_data1$sampledate,unit = "2 months")
colnames(abc_data1)[5]<-"fsa"

#Period 2
#Impute missing p2 age and sex with p1 age and sex
abc_data<-abc_data %>% 
  mutate(p2_age = ifelse(is.na(p2_age),p1_age,p2_age),
         p2_qe2 = ifelse(is.na(p2_qe2),p1_qe2,p2_qe2))

abc_data2<-abc_data %>% 
  select(rseed,p2_np_igg_pred,p2_rbd_igg_pred,p2_smt1_igg_pred,
         p2_received_date,p2_province,p2_fsa,p2_age,p2_qe2,p2_suggested_status,
         race,race1) %>% 
  #Remove individuals who did not provide a serology sample
  filter(p2_np_igg_pred != "" & p2_rbd_igg_pred != "" & 
           p2_smt1_igg_pred != "") %>% #n = 7160 
  mutate(sampledate = as.Date(p2_received_date,tz = "UTC"),
         province = province_fun2(p2_province),
         urban = case_when(
           substr(p2_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p2_fsa,start = 2,stop = 2) %in% 1:9 ~ "Urban",
           TRUE ~ NA),
         age_groups = age_groups_fun(as.numeric(p2_age)),
         sex = case_when(p2_qe2 == 1 ~ "Male",
                         p2_qe2 == 2 ~ "Female",
                         p2_qe2 == 3 ~ "Self described")
  )

#Categorize date sample received into 2 month time buckets
abc_data2$month<-floor_date(abc_data2$sampledate,unit = "2 months")
colnames(abc_data2)[7]<-"fsa"

#Period 3
#Impute missing p3 age and sex with p1 age and sex
abc_data<-abc_data %>% 
  mutate(p3_age = ifelse(is.na(p3_age),p1_age,p3_age),
         p3_qe2 = ifelse(is.na(p3_qe2),p1_qe2,p3_qe2))

abc_data3<-abc_data %>% 
  select(rseed,p3_np_igg_pred,p3_rbd_igg_pred,p3_smt1_igg_pred,
         p3_dbs_received_date,p3_province,p3_fsa,p3_age,p3_qe2,p3_suggested_status,
         race,race1) %>% 
  #Remove individuals who did not provide a serology sample
  filter(p3_np_igg_pred != "" &
           p3_rbd_igg_pred != "" &
           p3_smt1_igg_pred != "") %>% #n = 5641
  mutate(sampledate = as.Date(p3_dbs_received_date,tz = "UTC"),
         province = province_fun2(p3_province),
         urban = case_when(
           substr(p3_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p3_fsa,start = 2,stop = 2) %in% 1:9 ~ "Urban",
           TRUE ~ NA),
         age_groups = age_groups_fun(as.numeric(p3_age)),
         sex = case_when(p3_qe2 == 1 ~ "Male",
                         p3_qe2 == 2 ~ "Female",
                         p3_qe2 == 3 ~ "Self described")
  )

#Place date sample received into 2 month time buckets
abc_data3$month<-floor_date(abc_data3$sampledate,unit = "2 months")
colnames(abc_data3)[7]<-"fsa"

#Period 4
#Impute missing p4 age and sex with p1 age and sex. For individuals with missing p1 race,
# assign race based on p4 ethnicity.
names4<-c("p4a_ethnicity_1","p4a_ethnicity_2",
          "p4a_ethnicity_3","p4a_ethnicity_4","p4a_ethnicity_5","p4a_ethnicity_6",
          "p4a_ethnicity_7","p4a_ethnicity_8","p4a_ethnicity_9","p4a_ethnicity_10",
          "p4a_ethnicity_11","p4a_ethnicity_12","p4a_ethnicity_13","p4a_ethnicity_14",
          "p4a_ethnicity_15")

for(i in 1:nrow(abc_data)){
  abc_data$race[i]<-ifelse(is.na(abc_data$race[i]) == F,
                           abc_data$race[i],
                           case_when(
                             #select pnts and nothing else
                             sum(abc_data[i,names4] %in% c(15)) > 0 & 
                               (sum(is.na(abc_data[i,names4])) == 14) ~ "pnts",
                             #select only 1 white ethnicity
                             sum(abc_data[i,names4] %in% c(2,3,4,13)) > 0 &
                               (sum(is.na(abc_data[i,names4])) == 14) ~ "White", 
                             #select only 1 racialized minority ethnicity
                             sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) > 0 & 
                               (sum(is.na(abc_data[i,names4])) == 14) ~ "Racialized minority",
                             #select only other as their ethnicity - defer to racialized minority variable
                             sum(abc_data[i,names4] %in% c(14)) > 0 &
                               (sum(is.na(abc_data[i,names4])) == 14) ~ "Racialized minority",
                             #select at least 1 white ethnicity and at least 1 racialized minority ethnicity
                             sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) > 0 &
                               sum(abc_data[i,names4] %in% c(2,3,4,13)) > 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "Racialized minority",
                             #select at least 1 white ethnicity and at least 1 "other" ethnicity
                             sum(abc_data[i,names4] %in% c(2,3,4,13)) > 0 &
                               sum(abc_data[i,names4] %in% c(14)) > 0 &
                               sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) == 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "White",
                             #select 2+ racialized minority ethnicities
                             sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) > 1 &
                               sum(abc_data[i,names4] %in% c(2,3,4,13)) == 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "Racialized minority",
                             #select 2+ white ethnicities
                             sum(abc_data[i,names4] %in% c(2,3,4,13)) > 1 &
                               sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) == 0 &
                               sum(abc_data[i,names4] %in% c(14)) == 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "White",
                             #select at least 1 racialized minority ethnicity and at least 1 "other" ethnicity
                             sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) > 0 &
                               sum(abc_data[i,names4] %in% c(14)) > 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "Racialized minority",
                             TRUE ~ NA))
  abc_data$race1[i]<-ifelse(is.na(abc_data$race1[i]) == F,
                           abc_data$race1[i],
                           case_when(
                             #select pnts and nothing else
                             sum(abc_data[i,names4] %in% c(15)) > 0 & 
                               (sum(is.na(abc_data[i,names4])) == 14) ~ "pnts",
                             #select only 1 white ethnicity
                             sum(abc_data[i,names4] %in% c(2,3,4,13)) > 0 &
                               (sum(is.na(abc_data[i,names4])) == 14) ~ "White", 
                             #select only 1 racialized minority ethnicity
                             sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) > 0 & 
                               (sum(is.na(abc_data[i,names4])) == 14) ~ "Racialized minority",
                             #select only other as their ethnicity - defer to racialized minority variable
                             sum(abc_data[i,names4] %in% c(14)) > 0 &
                               (sum(is.na(abc_data[i,names4])) == 14) ~ "Racialized minority",
                             #select at least 1 white ethnicity and at least 1 racialized minority ethnicity
                             sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) > 0 &
                               sum(abc_data[i,names4] %in% c(2,3,4,13)) > 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "White",
                             #select at least 1 white ethnicity and at least 1 "other" ethnicity
                             sum(abc_data[i,names4] %in% c(2,3,4,13)) > 0 &
                               sum(abc_data[i,names4] %in% c(14)) > 0 &
                               sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) == 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "White",
                             #select 2+ racialized minority ethnicities
                             sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) > 1 &
                               sum(abc_data[i,names4] %in% c(2,3,4,13)) == 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "Racialized minority",
                             #select 2+ white ethnicities
                             sum(abc_data[i,names4] %in% c(2,3,4,13)) > 1 &
                               sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) == 0 &
                               sum(abc_data[i,names4] %in% c(14)) == 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "White",
                             #select at least 1 racialized minority ethnicity and at least 1 "other" ethnicity
                             sum(abc_data[i,names4] %in% c(1,5,6,7,8,9,10,11,12)) > 0 &
                               sum(abc_data[i,names4] %in% c(14)) > 0 &
                               (sum(is.na(abc_data[i,names4])) < 14) ~ "Racialized minority",
                             TRUE ~ NA))
  }

abc_data<-abc_data %>% 
  mutate(p4a_age = ifelse(is.na(p4a_age),p1_age,p4a_age),
         p4a_qe2 = ifelse(is.na(p4a_qe2),p1_qe2,p4a_qe2))

abc_data4<-abc_data %>% 
  select(rseed,p4_np_igg_pred,p4_rbd_igg_pred,p4_smt1_igg_pred,
         p4_dbs_received_date,p4a_province,p4a_fsa,p4a_age,p4a_qe2,p4_suggested_status,
         race,race1) %>% 
  #Remove individuals who did not provide a serology sample
  filter(p4_np_igg_pred != "" &
           p4_rbd_igg_pred != "" &
           p4_smt1_igg_pred != "") %>% #n = 5373
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
abc_data4$month<-floor_date(abc_data4$sampledate,unit = "2 months")
colnames(abc_data4)[7]<-"fsa"

#Generate final working df
abc_df<-do.call("rbind",list(abc_data1[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa","race1")],
                             abc_data2[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa","race1")],
                             abc_data3[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa","race1")],
                             abc_data4[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa","race1")]))

#Remove individuals missing province
abc_df<-abc_df %>% 
  filter(!is.na(province) ) #n = XXX

#Generate counts by age-sex-urban strata
abc_asu<-abc_df %>%  #n = XXXX
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

#Generate counts by sex strata and combine
abc_allsu<-abc_df %>% 
  filter(province != "YT" & sex != "Self described") %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()
abc_asu<-do.call("rbind",list(abc_asu,abc_allu,abc_allsu))

#Generate counts by age-sex-race strata
abc_asr<-abc_df %>%  #n = 24489 - new one
  filter(province != "YT" & sex != "Self described" & race != "pnts" &
           !is.na(race)) %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
abc_allr<-abc_df %>% 
  filter(province != "YT" & sex != "Self described" & race != "pnts" &
           !is.na(race)) %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
abc_asr<-rbind(abc_asr,abc_allr)

#Generate counts by age-sex strata (territories only)
abc_ast<-abc_df %>% 
  filter(province == "YT" & sex != "Self described") %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Sensitivity analysis 1: generate alternative age-sex-race counts when mixed race classified as "White"
#Generate counts by age-sex-race1 strata
abc_asr1<-abc_df %>%  #n = xx
  filter(province != "YT" & sex != "Self described" & race1 != "pnts" &
           !is.na(race1)) %>% 
  group_by(age_groups,sex,race1) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race1 strata and combine
abc_allr1<-abc_df %>% 
  filter(province != "YT" & sex != "Self described" & race1 != "pnts" &
           !is.na(race1)) %>% 
  group_by(sex,race1) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
abc_asr1<-rbind(abc_asr1,abc_allr1)

#Write to csv
write_csv(abc_asu,"./abc_asu_jan222024.csv")
write_csv(abc_asr,"./abc_asr_jan222024.csv")
write_csv(abc_df,"./abc_df_jan222024.csv")
write_csv(abc_ast,"./abc_ast_jan222024.csv")
write_csv(abc_asr1,"./abc_asr1_jan22024.csv")

# Probabilistic Survey 2 (CLSA) -----------------------------
#Join demographic & serological vars from antibody and combined cohorts
df_all_clsa <- merge(df_clsa_anti[,c("entity_id","SER_AGE_COV", "SER_SEX_COV", "start_datetime_COV",  
                              "SER_ETHN_WH_COV", "SER_ETHN_SA_COV", "SER_ETHN_ZH_COV", "SER_ETHN_BL_COV", "SER_ETHN_FP_COV", "SER_ETHN_LA_COV", "SER_ETHN_AR_COV", "SER_ETHN_SE_COV",
                              "SER_ETHN_WA_COV", "SER_ETHN_KO_COV", "SER_ETHN_JA_COV", "SER_ETHN_OTSP_COV", "SER_ETHN_DK_NA_COV", 
                              "SER_ETHN_REFUSED_COV","SER_NUCLEOCAPSID_COV", "SER_SPIKE_COV", "SER_ABRSLT_COV")], 
                     df_clsa_cb[,c("entity_id", "PROV_COVID", "FSA_COVID")],
                     by='entity_id', all.x = TRUE)

#Create province variable
df_all_clsa$province<-province_fun(df_all_clsa$FSA_COVID)

#Create age variable
df_all_clsa$age_groups = age_groups_fun(df_all_clsa$SER_AGE_COV)

#Re-label sex variable
df_all_clsa$sex<-case_when(
  df_all_clsa$SER_SEX_COV == "F" ~ "Female",
  df_all_clsa$SER_SEX_COV == "M" ~ "Male",
  TRUE ~ NA)

#Classify residence as urban or rural
df_all_clsa$urban<-case_when(substr(df_all_clsa$FSA_COVID,2,2) == 0 ~ "Rural",
                             substr(df_all_clsa$FSA_COVID,2,2) %in% 1:9 ~ "Urban",
                             TRUE ~ NA)

#Classify race as white or racialized minority
df_all_clsa$race<-NULL
for(i in 1:nrow(df_all_clsa)){
  #Either did not know ethnicity, preferred not to say, or refused to provide an ethnicity
  if(sum(df_all_clsa[i,17:18] %in% 1) > 0){
    df_all_clsa$race[i] <- "pnts"
  } 
  #provided a response
  else {
    #categorize white ethnicity responses
    if(df_all_clsa[i,5] == 1){
      #select white and racialized minority
      if (sum(df_all_clsa[i,c(6:15)] %in% 1) > 0){
        df_all_clsa$race[i]<-"Racialized minority"
      } else {
        #only selected white ethnicity
        df_all_clsa$race[i]<-"White"
      }
    } 
    #categorize responses of individuals who did not select white
    else {
      #select only racialized minority ethnicities
      if (sum(df_all_clsa[i,c(6:15)] %in% 1) > 0){
        df_all_clsa$race[i]<-"Racialized minority"
      } else {
        df_all_clsa$race[i]<-"Missing"
      }
    } 
  }
  
  #check free text box responses
  if(sum(df_all_clsa[i,16] %in% c(-88888,-99999)) == 0){
    #selected white but wrote racialized minority in text box
    if((df_all_clsa$race[i] == "White" & df_all_clsa[i,16] %in% rm_indn) == T){
      df_all_clsa$race[i]<-"Racialized minority"
    } else{}
    
    #only used text box
    if(df_all_clsa$race[i] == "Missing"){
      if(df_all_clsa[i,16] %in% rm_indn){
        df_all_clsa$race[i]<-"Racialized minority"
      } else if (df_all_clsa[i,16] %in% w){
        df_all_clsa$race[i]<-"White"
      } else {
        df_all_clsa$race[i]<-"Missing"
      }
    } else{}
  }
}

df_all_clsa$race1<-NULL #for sensitivity analysis 1
for(i in 1:nrow(df_all_clsa)){
  #Either did not know ethnicity, preferred not to say, or refused to provide an ethnicity
  if(sum(df_all_clsa[i,17:18] %in% 1) > 0){
    df_all_clsa$race1[i] <- "pnts"
  } 
  #provided a response
  else {
    #categorize white ethnicity responses
    if(df_all_clsa[i,5] == 1){
      #select white and racialized minority
      if (sum(df_all_clsa[i,c(6:15)] %in% 1) > 0){
        df_all_clsa$race1[i]<-"White"
      } else {
        #only selected white ethnicity
        df_all_clsa$race1[i]<-"White"
      }
    } 
    #categorize responses of individuals who did not select white
    else {
      #select only racialized minority ethnicities
      if (sum(df_all_clsa[i,c(6:15)] %in% 1) > 0){
        df_all_clsa$race1[i]<-"Racialized minority"
      } else {
        df_all_clsa$race1[i]<-"Missing"
      }
    } 
  }
  
  #check free text box responses
  if(sum(df_all_clsa[i,16] %in% c(-88888,-99999)) == 0){
    #selected white but wrote racialized minority in text box
    if((df_all_clsa$race1[i] == "White" & df_all_clsa[i,16] %in% rm_indn) == T){
      df_all_clsa$race1[i]<-"White"
    } else{}
    
    #only used text box
    if(df_all_clsa$race1[i] == "Missing"){
      if(df_all_clsa[i,16] %in% rm_indn){
        df_all_clsa$race1[i]<-"Racialized minority"
      } else if (df_all_clsa[i,16] %in% w){
        df_all_clsa$race1[i]<-"White"
      } else {
        df_all_clsa$race1[i]<-"Missing"
      }
    } else{}
  }
}

df_all_clsa<-df_all_clsa %>% 
  mutate(race = ifelse(race == "Missing",NA,race),
         race1 = ifelse(race1 == "Missing",NA,race1))

table(df_all_clsa$race,useNA = "ifany")
table(df_all_clsa$race1,useNA = "ifany")
#exclude participants with missing samples
df_all_clsa <- df_all_clsa[df_all_clsa$SER_NUCLEOCAPSID_COV >= 0 |
                             df_all_clsa$SER_SPIKE_COV >= 0,] #17331

#classify interview start date (proxy for sampledate)
df_all_clsa$sampledate<-as.Date(df_all_clsa$start_datetime_COV,tz = "UTC")
df_all_clsa$month<-floor_date(df_all_clsa$sampledate,
                              unit = "2 months")
#generate final df
clsa_df<-df_all_clsa %>% 
  select(entity_id,age_groups,sex,province,urban,race,month,
         sampledate,SER_AGE_COV,race1) %>% 
  filter(!is.na(province))#n = XXX

#Generate counts by age-sex-urban strata
clsa_asu<-clsa_df %>% 
  filter(!is.na(urban)) %>% #n = XXX
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata
clsa_allu<-clsa_df %>% 
  filter(!is.na(urban)) %>%
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by sex strata and combine
clsa_allsu<-clsa_df %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()
clsa_asu<-do.call("rbind",list(clsa_asu,clsa_allu,clsa_allsu))

#Counts by age-sex-race
#Generate counts by age-sex-race strata
clsa_asr<-clsa_df %>% 
  filter(!is.na(race) & race != "pnts") %>% #n = XXX
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
clsa_asr<-rbind(clsa_asr,clsa_allr)

#Sensitivity analysis 1: generate alternative age-sex-race counts when mixed race classified as "White"
#Generate counts by age-sex-race1 strata
clsa_asr1<-clsa_df %>% 
  filter(!is.na(race1) & race1 != "pnts") %>% #n = XXX
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

#write to csv
write_csv(clsa_asu,"./clsa_asu_jan222024.csv")
write_csv(clsa_asr,"./clsa_asr_jan222024.csv")
write_csv(clsa_df,"./clsa_df_jan222024.csv")
write_csv(clsa_asr1,"./clsa_asr1_jan222024.csv")

# Probabilistic Survey 3 (CANPATH) ------------------------------------------------
names<-c("ResearcherID","C_ADM_STUDY_DATASET","C1_SDC_AGE", "C1_SDC_SEX", "C1_ADM_FSA",
         "C1_SDC_EB_ARAB","C1_SDC_EB_BLACK","C1_SDC_EB_CHINESE",
         "C1_SDC_EB_FILIPINO","C1_SDC_EB_JAPANESE",
         "C1_SDC_EB_KOREAN","C1_SDC_EB_LATIN","C1_SDC_EB_S_ASIAN",
         "C1_SDC_EB_SE_ASIAN","C1_SDC_EB_W_ASIAN","C1_SDC_EB_WHITE",
         "C1_SDC_EB_OTHER", "C1_SDC_EB_OTHER_OTSP","C1_SDC_EB_CA")

#Identify Manitoba participants omitted from main dataset and add them to main dataset
unq_mtp<-canpath_mtp[which(!(canpath_mtp$ResearcherID %in% canpath_data$ResearcherID)),] #n = XX
canpath_data<-merge(canpath_data[,names],canpath_mtp[,names],all = T) #n = XX

#Create data.frame with questionnaire demographic variables, serology administrative variables, 
## and serology results for COVID-19 sub-study participants
canpath_data<-merge(canpath_data[,names],
                    canpath_seradmin[,c("ResearcherID","C1_ADM_COLLECT_DATE")],by = "ResearcherID",all.x = T)
canpath_data<-merge(canpath_serres[,c("ResearcherID","C1_SAMPLE_ANTIGEN_TESTED","C1_SAMPLE_RESULTS_DESCRIPTION",
                                      "C1_SAMPLE_SUGGESTED_STATUS")],
                    canpath_data,by = "ResearcherID",all.x = T) #n = XX

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

#Classify ethnicity as white or racialized minority
canpath_data$race<-NA
for(i in 1:nrow(canpath_data)){
  #Classify individuals who preferred not to state ethnicity
  if((canpath_data[i,22] %in% 8) &
     (sum(canpath_data[i,9:22] %in% 1) == 0)){
    canpath_data$race[i]<-"pnts"
  } 
  #Classify individuals who provided a mark in response
  else{
    #categorize white ethnicity responses
    if(canpath_data[i,19] %in% 1){
      #Also selected racialized minority
      if(sum(canpath_data[i,9:18] %in% 1) > 0){
        canpath_data$race[i]<-"Racialized minority"
      } 
      #Only selected white
      else{
        canpath_data$race[i]<-"White"
      }
    }
    #categorize responses of individuals who selected racialized minority
    else if (sum(canpath_data[i,9:18] %in% 1) > 0){
      canpath_data$race[i]<-"Racialized minority"
    }
    #categorize individuals who did not use mark ins as missing(temporary)
    else{
      canpath_data$race[i]<-"Missing"
    }
  }
  
  #Check free text box responses
  if(!is.na(canpath_data[i,21])){
    #selected white but wrote racialized minority in text box
    if((canpath_data$race[i] == "White" & canpath_data[i,21] %in% rm_indn)==T){
      canpath_data$race[i] == "Racialized minority"
    } else{}
  
  #classify remaining text box responses
    if(canpath_data$race[i] == "Missing"){
      if(canpath_data[i,21] %in% rm_indn){
        canpath_data$race[i]<-"Racialized minority"
      }else if(canpath_data[i,21] %in% w){
        canpath_data$race[i]<-"White"
      } else{
        canpath_data$race[i]<-"Missing"
      }
    } else{}
  }
}

#Put sensitivity analysis in here
#Remove individuals with unknown province or do not reside in a regional cohort.
canpath_data<-canpath_data %>% 
  filter(!is.na(province) & province != "SK" & 
           province != "YT")

#Remove duplicate rows with the same ID at the same sampledate. This prevents over-estimating the 
# sample count when calculating proportion of specimens donated by each strata.
can_df<-canpath_data %>% distinct(ResearcherID,sampledate,age_groups,sex,province,urban,month,race)#n = XX specimens 
can_df1<-canpath_data %>% distinct(ResearcherID,sampledate,age_groups,sex,province,urban,month,race1)

#Generate counts by age-sex-urban strata
can_asu<-can_df %>%
  group_by(age_groups,sex,urban) %>%#n = 21166
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata
can_allu<-can_df %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()

#Generate counts by sex strata and combine
can_allsu<-can_df %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()
can_asu<-do.call("rbind",list(can_asu,can_allu,can_allsu))

#Generate counts by age-sex-race strata
can_asr<-can_df %>%   #n = 20817
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
can_asr<-rbind(can_asr,can_allr)

#Sensitivity analysis 1: generate alternative age-sex-race counts when mixed race classified as "White"
#Generate counts by age-sex-race1 strata
can_asr1<-can_df1 %>%   #n = 20817
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
write_csv(can_asu,"./can_asu_jan222024.csv")
write_csv(can_asr,"./can_asr_jan222024.csv")
write_csv(can_df,"./can_df_jan222024.csv")
write_csv(can_asr1,"./can_asr1_jan222024.csv")
write_csv(can_df1,"./can_df1_jan222024.csv")

# 2016 Canadian census ----------------------------------------------------
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

census_a_alls<-census_a %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_a<-do.call("rbind",list(census_a,census_a_all,census_a_alls))
write_csv(census_a,"2021 Canadian Census/censusasu_a_abc.csv")

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

census_c_alls<-census_c %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_c<-do.call("rbind",list(census_c,census_c_all,census_c_alls))
write_csv(census_c,"2021 Canadian Census/censusasu_c_cbs.csv")

#Setting D: 9 provinces (no Saskatchewan), 18+ (Canpath)
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

census_d_alls<-census_d %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_d<-do.call("rbind",list(census_d,census_d_all,census_d_alls))
write_csv(census_d,"2021 Canadian Census/censusasu_d_canpath.csv")

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

census_e_alls<-census_e %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_e<-do.call("rbind",list(census_e,census_e_all,census_e_alls))
write_csv(census_e,"2021 Canadian Census/censusasu_e_apl.csv")

#Setting F: Only territories, all ages (CCAHS-1, age-sex)
census_f<-census[[5]] %>%
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F)
write_csv(census_f,"2021 Canadian Census/censusas_f_ccahst.csv")

#Setting G: 10 provinces, 47+ (CLSA)
census_g<-census_alt[[1]] %>%
  filter(age_groups == "47-56 years" | 
           age_groups == "57+ years" & 
           province != "NU" & province != "NT" & province != "YT") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_g_all<-census_g %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_g_all$age_groups<-"All ages"

census_g_alls<-census_g %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_g<-do.call("rbind",list(census_g,census_g_all,census_g_alls))
write_csv(census_g,"2021 Canadian Census/censusasu_g_clsa.csv")

#Setting H: Yukon, 18+ (Ab-c, age-sex)
census_h<-census[[5]] %>% 
  filter(province == "YT") %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F)
  
write_csv(census_h,"2021 Canadian Census/censusas_h_abct.csv")

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
write_csv(census_ar,"2021 Canadian Census/censusasr_a_abc.csv")

'#Setting B: 10 provinces, all ages (CCAHS)
'

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
write_csv(census_cr,"2021 Canadian Census/censusasr_c_cbs.csv")

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
write_csv(census_dr,"2021 Canadian Census/censusasr_d_canpath.csv")

#Setting G: 10 provinces, 47+ (CLSA)
census_gr<-census_alt[[2]] %>%
  filter(age_groups == "47-56 years" | 
           age_groups == "57+ years" & 
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
write_csv(census_gr,"2021 Canadian Census/censusasr_g_clsa.csv")

#Census counts by sex-quintmat
#Setting B: 10 provinces, all ages (CCAHS)

#Setting C: 9 provinces (no Quebec), 18+ (CBS)
census_cqm<-census[[3]] %>% 
  filter(province != "QC" &
           province != "NU" & province != "NT" & province != "YT" &
           age_groups != "0-17 years" & quintmat != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
census_cqm_all<-census_cqm %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
census_cqm_all$quintmat<-"All quintiles"
census_cqm<-rbind(census_cqm,census_cqm_all)

write_csv(census_cqm,"2021 Canadian Census/censussqm_c_cbs.csv")

#Setting E:  Alberta, all ages (APL)
census_eqm<-census[[3]] %>% 
  filter(province == "AB" & quintmat != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
census_eqm_all<-census_eqm %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
census_eqm_all$quintmat<-"All quintiles"
census_eqm<-rbind(census_eqm,census_eqm_all)

write_csv(census_eqm,"2021 Canadian Census/censussqm_e_apl.csv")

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
census_cqs_all<-census_cqs %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
census_cqs_all$quintsoc<-"All quintiles"
census_cqs<-rbind(census_cqs,census_cqs_all)

write_csv(census_cqs,"2021 Canadian Census/censussqs_c_cbs.csv")

#Setting E:  Alberta, all ages (APL)
census_eqs<-census[[4]] %>% 
  filter(province == "AB" & quintsoc != "All quintiles") %>% 
  aggregate(count_census ~ sex + quintsoc,
            FUN = sum,
            drop = F)
census_eqs_all<-census_eqs %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
census_eqs_all$quintsoc<-"All quintiles"
census_eqs<-rbind(census_eqs,census_eqs_all)

write_csv(census_eqs,"2021 Canadian Census/censussqs_e_apl.csv")

#2016 Canadian census 2 (mixed race classified as white)
##NOTE: CALL RACE RACE1.
#Setting A: 10 provinces, 18+ (Ab-c)

#Setting D: 9 provinces, no Saskatchewan (Canpath)

#Setting G: 10 provinces, 47+ (CLSA)


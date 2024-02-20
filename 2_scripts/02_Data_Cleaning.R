"This script cleans all 6 cohort datasets and prepares them
for later analysis and plotting."

# Load data and functions ---------------------------------------------------------------
setwd("./1_data/private")
library(haven)
library(lubridate)
library(tidyverse)# loads readr
library(DBI)
library(RPostgres)
library(flextable)

"#CBS CITF Serology dataset import
#Test connection arguments
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'cbs0', 
  host = '132.216.183.71', 
  port = 5432 
)

#Import dataset
cbs_data <- dbReadTable(con, SQL('students.copy_cbs_combined'))

#Disconnect from database once data is loaded into R
dbDisconnect(con)"
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

#Census dataset import
census<-read.csv("./2021 Canadian Census/census_w_counts_race_urban.csv")

colnames(census)<-c("province","quintmat","age_groups","sex","race",
                    "urban","count_census")
census$age_groups<-case_when(census$age_groups == "56+ years" ~ "57+ years",
                          census$age_groups == "< 18 years" ~ "0-17 years",
                          census$age_groups == "18-26 years" ~ "18-26 years",
                          census$age_groups == "27-36 years" ~ "27-36 years",
                          census$age_groups == "37-46 years" ~ "37-46 years",
                          census$age_groups == "47-56 years" ~ "47-56 years")
#Functions
#Assign province of residence (version 1)
province_fun <- function(var) {
  fsa_f = as.character(substr(var,1,1))
  prov =  ifelse(fsa_f == "A", "NL", #first letter of FSA corresponds to Canadian province or territory
                 ifelse(fsa_f == "B", "NS",
                        ifelse(fsa_f == "C", "PE",
                               ifelse(fsa_f == "E", "NB",
                                      ifelse(fsa_f == "G" | fsa_f == "H" | fsa_f == "J", "QC",
                                             ifelse(fsa_f == "K" | fsa_f == "L" | fsa_f == "M" | fsa_f == "N" | fsa_f == "P", "ON",
                                                    ifelse(fsa_f == "R", "MB",
                                                           ifelse(fsa_f == "S", "SK",
                                                                  ifelse(fsa_f == "T", "AB",
                                                                         ifelse(fsa_f == "V", "BC",
                                                                                ifelse(fsa_f == "X", "NU/NT",
                                                                                       ifelse(fsa_f == "Y", "YT", NA
                                                                                       ))))))))))))
  return(prov)}

#Assign province of residence (version 2)
province_fun2 <- function(var) {
  p1 = as.integer(var)
  prov =  case_when(p1 == 1~ "AB",
                    p1 == 10~ "PE",
                    p1 == 7~ "NS",
                    p1 == 4~ "NB",
                    p1 == 11~ "QC",
                    p1 == 9 ~"ON",
                    p1 == 3~ "MB",
                    p1 == 12~ "SK",
                    p1 == 5~ "NL",
                    p1 == 2~ "BC",
                    p1 == 6~ "NT",
                    p1 == 8~ "NU",
                    p1 == 13~ "YT", 
                    TRUE~NA)
  return(prov)}

#Create age groups (version 1)
age_groups_fun <- function(variable){
 age_group = cut(variable,
                breaks = c(0,18,27,37,47,57,
                          Inf),
             labels = c("0-17 years","18-26 years","27-36 years",
                        "37-46 years","47-56 years","57+ years"),
           right = FALSE)
return(age_group)
}

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
#Replace FSA values with Canadian province or territory
cbs_data<-cbs_data %>% mutate(province = province_fun(fsa))

#Convert dob of participant to age at donation
cbs_data$year_donation<-as.numeric(format.Date(cbs_data$sampledate,"%Y")) #extract year of sample donation
cbs_data$donation_age <- cbs_data$year_donation - cbs_data$dob

#Fix erratic dob entries -- 6 individuals with date of birth in 1800s
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

#Generate final df, remove territories and individuals under 18 years old
nrow(cbs_data[is.na(cbs_data$cur_result_n) & is.na(cbs_data$cur_result_s),]) #0 - all participants have at least 1 serology result
cbs_df<-cbs_data %>% 
  select(pid,sampledate,sex,race,urban,quintmat,quintsoc,province,month,age_groups,fsa) %>% 
  filter(province != "YT" & province != "NU/NT" & province != "QC")#n = 1038989
cbs_df<-cbs_df %>% filter(age_groups != "0-17 years") #n = 1035580

#Generate counts by age-sex-urban strata
cbs_asu<-cbs_df %>%
  filter(!is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>% #n = 1035573
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata and combine
cbs_allu<-cbs_df %>% 
  filter(!is.na(urban)) %>%
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
cbs_asu<-rbind(cbs_asu,cbs_allu)

#Generate counts by age-sex-race strata
cbs_asr<-cbs_df %>%  #n = 973413
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
cbs_sqm<-cbs_df %>%  #n = 911938
  filter(!is.na(quintmat)) %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex strata and combine
cbs_alls<-cbs_df %>% 
  filter(!is.na(quintmat)) %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(quintmat = "All quintiles") %>% 
  ungroup()
cbs_sqm<-rbind(cbs_sqm,cbs_alls)

#Generate counts by sex-quintsoc strata
cbs_sqs<-cbs_df %>% #n = 911938
  filter(!is.na(quintsoc)) %>% 
  group_by(sex,quintsoc) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex strata and combine
cbs_allss<-cbs_df %>% 
  filter(!is.na(quintsoc)) %>% 
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
#write_csv(cbs_data,"./cbs_unmodified_df_backup_jan222024.csv")

# Outpatient Laboratory (APL) ---------------------------------------------
#Manually remove duplicate entries and regenerate record ID (order_ID)
nrow(apl_data[is.na(apl_data$`N-IgG_INTERP`) & is.na(apl_data$`RBD-IgGII_INTERP`),]) #0:all participants have at least 1 serology result
apl_data<-apl_data %>% 
  filter(order_ID != 1253 & order_ID != 1521 & order_ID != 2728 & order_ID != 3247) %>% 
  mutate(order_ID = 1:214776) #n = 214776

#Remove individuals with no unique ID
apl_data<-apl_data[!is.na(apl_data$clean_IDe),] #n = 211911 

#Remove participants outside Alberta, and generate urban variable
apl_data<-apl_data %>% 
  filter(substr(PAT_FSA,1,1) == "T"
         ) %>% #n = 208110
  mutate(PAT_FSA = ifelse(
    substr(PAT_FSA,2,2) == "O", 
    paste(substr(PAT_FSA,1,1),"0",substr(PAT_FSA,3,3),sep = ""),
    PAT_FSA)
  )
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
                    "sampledate","quintmat","quintsoc","fsa")

apl_asu<-apl_df %>%  #n = 208096
  filter(sex != "Unknown" &
           !is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup() 

#Generate counts by sex-urban strata and combine
apl_allu<-apl_df %>% 
  filter(sex != "Unknown" &
           !is.na(urban)) %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
apl_asu<-rbind(apl_asu,apl_allu)

#Generate counts by sex-quintmat strata
apl_sqm<-apl_df %>%  #n = 168135
  filter(!is.na(quintmat)) %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex strata and combine
apl_alls<-apl_df %>% 
  filter(!is.na(quintmat)) %>% 
  group_by(sex) %>% 
  summarize(count = n()) %>% 
  mutate(quintmat = "All quintiles") %>% 
  ungroup()
apl_sqm<-rbind(apl_sqm,apl_alls)

#Generate counts by sex-quintsoc strata
apl_sqs<-apl_df %>% #n = 168135
  filter(!is.na(quintsoc)) %>% 
  group_by(sex,quintsoc) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex strata and combine
apl_allss<-apl_df %>% 
  filter(!is.na(quintsoc)) %>%
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
names<-c("p1_ethnicity_1","p1_ethnicity_2",
         "p1_ethnicity_3","p1_ethnicity_4","p1_ethnicity_5","p1_ethnicity_6",
         "p1_ethnicity_7","p1_ethnicity_8","p1_ethnicity_9","p1_ethnicity_10",
         "p1_ethnicity_11","p1_ethnicity_12","p1_ethnicity_13","p1_ethnicity_14",
         "p1_ethnicity_15")

for(i in 1:nrow(abc_data)){
  race_i<-
    case_when(
      #select pnts and nothing else
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
        (sum(is.na(abc_data[i,names])) == 14) ~ NA,
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
  race<-c(race,race_i)
}
abc_data<-cbind(abc_data,race)

#Period 1
abc_data1<-abc_data %>% 
  select(rseed,p1_result_sinai,p1_int_month,p1_province,p1_fsa,
         p1_age,p1_qe2,race) %>% 
  #Remove individuals who did not provide a sample
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
         race) %>% 
  #Remove individuals who did not provide a sample
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
         race) %>% 
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
                               (sum(is.na(abc_data[i,names4])) == 14) ~ NA,
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
  }

abc_data<-abc_data %>% 
  mutate(p4a_age = ifelse(is.na(p4a_age),p1_age,p4a_age),
         p4a_qe2 = ifelse(is.na(p4a_qe2),p1_qe2,p4a_qe2))

abc_data4<-abc_data %>% 
  select(rseed,p4_np_igg_pred,p4_rbd_igg_pred,p4_smt1_igg_pred,
         p4_dbs_received_date,p4a_province,p4a_fsa,p4a_age,p4a_qe2,p4_suggested_status,
         race) %>% 
  filter(p4_np_igg_pred != "" &
           p4_rbd_igg_pred != "" &
           p4_smt1_igg_pred != "") %>% #n = 5353
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
                                        "month","race","province","fsa")],
                             abc_data2[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa")],
                             abc_data3[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa")],
                             abc_data4[,c("rseed","age_groups","sex","urban","sampledate",
                                        "month","race","province","fsa")]))

#Remove individuals missing province and sampledate
abc_df<-abc_df %>% 
  filter(!is.na(province) & !is.na(sampledate)) #n = 25109

#Generate counts by age-sex-urban strata
abc_asu<-abc_df %>%  #n = 24941
  filter(sex != "Self described" & !is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata and combine
abc_allu<-abc_df %>% 
  filter(sex != "Self described" & !is.na(urban)) %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
abc_asu<-rbind(abc_asu,abc_allu)

#Generate counts by age-sex-race strata
abc_asr<-abc_df %>%  #n = 24489 - new one
  filter(sex != "Self described" & race != "pnts" & !is.na(race)) %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-race strata and combine
abc_allr<-abc_df %>% 
  filter(sex != "Self described" & race != "pnts" & !is.na(race)) %>% 
  group_by(sex,race) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
abc_asr<-rbind(abc_asr,abc_allr)

#Write to csv
write_csv(abc_asu,"./abc_asu_jan222024.csv")
write_csv(abc_asr,"./abc_asr_jan222024.csv")
write_csv(abc_df,"./abc_df_jan222024.csv")

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

#Load ethnicity classification tables
source("../../2_scripts/S01_Race_Classification_TxtResponse.R")

#Classify race as white or racialized minority
df_all_clsa$race<-NULL
for(i in 1:nrow(df_all_clsa)){
  df_all_clsa$race[i]<-case_when(
    
    #Either did not know ethnicity, preferred not to say, or refused to provide an ethnicity
    sum(df_all_clsa[i,17:18] %in% 1) > 0 ~ "pnts",
    
    #Selected a white ethnicity, did not select a racialized minority ethnicity, and did not use text box
    df_all_clsa[i,5] == 1 & 
      sum(df_all_clsa[i,c(6:15,17)] %in% 0) == 11 &
      sum(df_all_clsa[i,16] %in% c(-88888,-99999)) > 0  ~ "White",
    
    #Selected a racialized minority ethnicity
    sum(df_all_clsa[i,c(6:15)] %in% 1) > 0 ~ "Racialized minority",
    
    #Selected a white ethnicity, did not select a racialized minority ethnicity, 
    ## and selected another white ethnicity in text box
    df_all_clsa[i,5] == 1 & 
      sum(df_all_clsa[i,c(6:15)] %in% 0) == 11 &
      df_all_clsa[i,16] %in% owstrings ~ "White",
    
    #Did not select any pre-specified ethnicities and listed a racialized minority ethnicity in text box
    df_all_clsa[i,16] %in% ormstrings ~ "Racialized minority",
    
    #Did not select any pre-specified ethnicities and listed a white ethnicity in text box
    df_all_clsa[i,16] %in% owstrings ~ "White",
    
    TRUE ~ NA)
}
table(df_all_clsa$race,useNA = "ifany")

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
         sampledate) %>% 
  filter(!is.na(sampledate) &
         !is.na(province))#n = 13051

#Generate counts by age-sex-urban strata
clsa_asu<-clsa_df %>% 
  filter(!is.na(urban)) %>% #n = 13051
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata and combine
clsa_allu<-clsa_df %>% 
  filter(!is.na(urban)) %>%
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
clsa_asu<-rbind(clsa_asu,clsa_allu)

#Counts by age-sex-race
#Generate counts by age-sex-race strata
clsa_asr<-clsa_df %>% 
  filter(!is.na(race) & race != "pnts") %>% #n = 12768
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

#write to csv
write_csv(clsa_asu,"./clsa_asu_jan222024.csv")
write_csv(clsa_asr,"./clsa_asr_jan222024.csv")
write_csv(clsa_df,"./clsa_df_jan222024.csv")

# Probabilistic Survey 3 (CANPATH) ------------------------------------------------
names<-c("ResearcherID","C_ADM_STUDY_DATASET","C1_SDC_AGE", "C1_SDC_SEX", "C1_ADM_FSA",
         "C1_SDC_EB_ARAB","C1_SDC_EB_BLACK","C1_SDC_EB_CHINESE",
         "C1_SDC_EB_FILIPINO","C1_SDC_EB_JAPANESE",
         "C1_SDC_EB_KOREAN","C1_SDC_EB_LATIN","C1_SDC_EB_S_ASIAN",
         "C1_SDC_EB_SE_ASIAN","C1_SDC_EB_W_ASIAN","C1_SDC_EB_WHITE",
         "C1_SDC_EB_OTHER", "C1_SDC_EB_OTHER_OTSP","C1_SDC_EB_CA")
#Identify Manitoba participants not included in main dataset and add them to main dataset
unq_mtp<-canpath_mtp[which(!(canpath_mtp$ResearcherID %in% canpath_data$ResearcherID)),] #n = 445
canpath_data<-merge(canpath_data[,names],canpath_mtp[,names],all = T) #n = 96459

#Create data.frame with questionnaire demographic variables, serology administrative variables, 
## and serology results for COVID-19 sub-study participants
canpath_data<-merge(canpath_data[,names],
                    canpath_seradmin[,c("ResearcherID","C1_ADM_COLLECT_DATE")],by = "ResearcherID",all.x = T)
canpath_data<-merge(canpath_serres[,c("ResearcherID","C1_SAMPLE_ANTIGEN_TESTED","C1_SAMPLE_RESULTS_DESCRIPTION",
                                      "C1_SAMPLE_SUGGESTED_STATUS")],
                    canpath_data,by = "ResearcherID",all.x = T) #n = 74522

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
canpath_data$race<-NULL

for(i in 1:nrow(canpath_data)){
  canpath_data$race[i]<-case_when(
    #select pnts and nothing else
    !is.na(canpath_data[i,22]) & 
      sum(is.na(canpath_data[i,9:22])) == 13 ~ "pnts",
    
    #select only 1 white ethnicity
    !is.na(canpath_data[i,19]) & 
      sum(is.na(canpath_data[i,9:22])) == 13 ~ "White",
    
    #select only 1 racialized minority ethnicity
    sum(!is.na(canpath_data[i,9:18])) == 1 &
      sum(is.na(canpath_data[i,9:22])) == 13 ~ "Racialized minority",
    
    #select 1 white ethnicity and at least 1 racialized minority ethnicity
    sum(!is.na(canpath_data[i,9:18])) > 0 & 
      !is.na(canpath_data[i,19]) &
      sum(is.na(canpath_data[i,22])) ~ "Racialized minority",
          
     #select 2+ racialized minority ethnicities
     sum(!is.na(canpath_data[i, 9:18])) > 1 &
       sum(is.na(canpath_data[i, c(19, 22)])) == 2 ~ "Racialized minority",
          
     #select at least 1 racialized minority ethnicity and at least 1 "other" ethnicity
     sum(!is.na(canpath_data[i, 9:18])) > 0 &
       sum(is.na(canpath_data[i, c(19, 22)])) == 2 &
       sum(!is.na(canpath_data[i, 20,21])) > 0 ~ "Racialized minority",
              
     #select white, but also identify as a racialized minority using text box
     sum(is.na(canpath_data[i, c(9:18, 22)])) == 11 &
        !is.na(canpath_data[i,19]) &
        sum(!is.na(canpath_data[i, c(20:21)])) >= 1 &
        canpath_data[i, c(21)] %in% (ormstrings) ~ "Racialized minority",
              
     #select white, but also identify as white using text box
     sum(is.na(canpath_data[i, c(9:18, 22)])) == 11 &
        !is.na(canpath_data[i,19]) &
        sum(!is.na(canpath_data[i, c(20:21)])) >= 1 &
        canpath_data[i, c(21)] %in% (owstrings) ~ "White",
    
     #select other and identify as a racialized minority using text box
     sum(is.na(canpath_data[i, c(9:19, 22)])) == 12 &
        canpath_data[i, 21] %in% (ormstrings) ~ "Racialized minority",
              
     #select other and identify as white using text box
     sum(is.na(canpath_data[i, c(9:19, 22)])) == 12 &
        canpath_data[i, 21] %in% (owstrings) ~ "White",
    
     TRUE ~ NA)
}

#Remove individuals with unknown province, do not reside in a regional cohort, or 
# are missing a sample date.
can_df<-canpath_data %>% 
  filter(!is.na(province) & province != "SK" & 
           province != "YT" & !is.na(sampledate))

#Remove duplicate rows with the same ID at the same sampledate. This prevents over-estimating the 
# sample count when calculating proportion of specimens donated by each strata.
can_df<-can_df %>% distinct(ResearcherID,sampledate,age_groups,sex,province,urban,month,race)#n = 21166 specimens 

#Generate counts by age-sex-urban strata
can_asu<-can_df %>%
  group_by(age_groups,sex,urban) %>%#n = 21166
  summarize(count = n()) %>% 
  ungroup()

#Generate counts by sex-urban strata and combine
can_allu<-can_df %>% 
  group_by(sex,urban) %>% 
  summarize(count = n()) %>% 
  mutate(age_groups = "All ages") %>% 
  ungroup()
can_asu<-rbind(can_asu,can_allu)

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

#Save to .csv
write_csv(can_asu,"./can_asu_jan222024.csv")
write_csv(can_asr,"./can_asr_jan222024.csv")
write_csv(can_df,"./can_df_jan222024.csv")
rm(list = ls())
# 2021 Canadian census ----------------------------------------------------
#Census counts by age-sex-urban
#Setting A: 10 provinces, 18+ (Ab-c)
census_a<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>%
  filter(age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_a_all<-census_a %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_a_all$age_groups<-"All ages"
census_a<-rbind(census_a,census_a_all)
write_csv(census_a,"2021 Canadian Census/censusasu_a_abc.csv")

#Setting B: 10 provinces, all ages (CCAHS)
'census_b<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province != "YT" & province != "NU/NT") %>%  #remove territories
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_b_all<-census_b %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_b_all$age_groups<-"All ages"
census_b<-rbind(census_b,census_b_all)
write_csv(census_b, "2021 Canadian Census/censusasu_b_ccahs.csv")'

#Setting C: 9 provinces (no Quebec), 18+ (CBS)
census_c<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province != "QC" & age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_c_all<-census_c %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_c_all$age_groups<-"All ages"
census_c<-rbind(census_c,census_c_all)
write_csv(census_c,"2021 Canadian Census/censusasu_c_cbs.csv")

#Setting D: 9 provinces (no Saskatchewan), 18+ (Canpath)
census_d<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province != "SK" & age_groups != "0-17 years") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_d_all<-census_d %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_d_all$age_groups<-"All ages"
census_d<-rbind(census_d,census_d_all)
write_csv(census_d,"2021 Canadian Census/censusasu_d_canpath.csv")

#Setting E: Alberta, all ages (APL)
census_e<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province == "AB") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_e_all<-census_e %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_e_all$age_groups<-"All ages"
census_e<-rbind(census_e,census_e_all)
write_csv(census_e,"2021 Canadian Census/censusasu_e_apl.csv")

#Setting F: Only territories, all ages (CCAHS-1)
'census_f<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province == "NU/NT" | province == "YT") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_f_all<-census_f %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_f_all$age_groups<-"All ages"
census_f<-rbind(census_f,census_f_all)
write_csv(census_f,"2021 Canadian Census/censusasu_f_ccahs.csv")'

#Setting G: 10 provinces, 47+ (CLSA)
census_g<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>%
  filter(age_groups == "47-56 years" | 
           age_groups == "57+ years") %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_g_all<-census_g %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_g_all$age_groups<-"All ages"
census_g<-rbind(census_g,census_g_all)
write_csv(census_g,"2021 Canadian Census/censusasu_g_clsa.csv")

#Census counts by age-sex-race
#Setting A: 10 provinces, 18+ (Ab-c)
census_ar<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>%
  filter(age_groups != "0-17 years") %>% 
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
census_br<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>% 
  filter(province != "YT" & province != "NU/NT") %>%  #remove territories
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
census_br_all<-census_br %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
census_br_all$age_groups<-"All ages"
census_br<-rbind(census_br,census_br_all)
write_csv(census_br, "2021 Canadian Census/censusasr_b_ccahs.csv")'

#Setting C: 9 provinces (no Quebec), 18+ (CBS)
census_cr<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>% 
  filter(province != "QC" & age_groups != "0-17 years") %>% 
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
census_dr<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>% 
  filter(province != "SK" & age_groups != "0-17 years") %>% 
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

'#Setting F: Only territories, all ages (CCAHS-1)
census_fr<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>% 
  filter(province == "NU/NT" | province == "YT") %>% 
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
census_fr_all<-census_fr %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
census_fr_all$age_groups<-"All ages"
census_fr<-rbind(census_fr,census_fr_all)
write_csv(census_fr,"2021 Canadian Census/censusasr_f_ccahs.csv")'

#Setting G: 10 provinces, 47+ (CLSA)
census_gr<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>%
  filter(age_groups == "47-56 years" | 
           age_groups == "57+ years") %>% 
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
'census_bq<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province != "YT" & province != "NU/NT") %>%  #remove territories
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)

write_csv(census_bq, "2021 Canadian Census/censussq_b_ccahs.csv")'

#Setting C: 9 provinces (no Quebec), 18+ (CBS)
census_cq<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province != "QC" & age_groups != "0-17 years") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
census_cq_all<-census_cq %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
census_cq_all$quintmat<-"All quintiles"
census_cq<-rbind(census_cq,census_cq_all)

write_csv(census_cq,"2021 Canadian Census/censussq_c_cbs.csv")

'#Setting F: Only territories, all ages (CCAHS-1)
census_fq<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province == "NU/NT" | province == "YT") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)

write_csv(census_fq,"2021 Canadian Census/censussq_f_ccahs.csv")'

#Setting E:  Alberta, all ages (APL)
census_eq<-census %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province == "AB") %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
census_eq_all<-census_eq %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
census_eq_all$quintmat<-"All quintiles"
census_eq<-rbind(census_eq,census_eq_all)

write_csv(census_eq,"2021 Canadian Census/censussq_e_apl.csv")


"This script documents all data cleaning and transformations performed
on cohort datasets prior to plotting"

# Load data ---------------------------------------------------------------
library(tidyverse)
library(haven)
library(lubridate)
library(tidyverse) # loads readr
library(DBI)
library(RPostgres)
library(boot)

#CBS CITF Serology dataset import
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
dbDisconnect(con)

#APL dataset import
load("~/serosurveillance-cohort-representativeness/1_data/APL/RFD4682e.RData")
apl_data<-RFD4682_e
rm(RFD4682_e)

#Ab-C dataset import
abc_data<-read.csv("1_data/Ab-c/df_047_hs_jha_phases1234.csv")

# Data cleaning -----------------------------------------------------------
# -- Each dataset should have the following format -- #
# Age: character (XXXX groups)
# Sex: character (Male, Female, Other(?))
# Urban: character (Urban = 1, Rural == 0)
# Material and social deprivation quintile: numeric (1-5)
# Race: character (White, Visible Minority)
# Region: character (1 of 13 Canadian provinces and territories)
# -- Generate counts by group when clean -- #

# Blood Donor (CBS) -------------------------------------------------------
#Replace FSA values with Canadian province or territory
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
cbs_data<-cbs_data %>% mutate(province = province_fun(fsa))

#Convert dob of participant to age at donation
cbs_data$year_donation<-as.numeric(format(as.Date(cbs_data$sampledate),"%Y")) #extract year of sample donation
cbs_data$donation_age <- cbs_data$year_donation - cbs_data$dob

#Fix erratic dob entries -- dob "1862" and "1854" should have been "1962" and "1954"
cbs_data$donation_age[cbs_data$donation_age > 120]
cbs_data$donation_age[cbs_data$donation_age < 16]
cbs_data$donation_age <- ifelse(cbs_data$donation_age > 120, cbs_data$donation_age - 100, cbs_data$donation_age)

#Create age groups
#age_groups_fun <- function(variable){ #10 year age buckets
# age_group = cut(variable,
#                breaks = c(0,10,20,30,40,50,
#                          60,70,80,Inf),
#              labels = c("0-9 years","10-19 years","20-29 years",
#                        "30-39 years","40-49 years","50-59 years",
#                       "60-69 years","70-79 years","80+ years"),
#           right = FALSE)
#return(age_group)
#}

age_groups_fun <- function(variable){ #age buckets corresponding with census
  age_group = cut(variable,
                  breaks = c(0,18,40,55,Inf),
                  labels = c("0-17 years","18-39 years","40-54 years",
                             "55+ years"),
                  right = FALSE)
  return(age_group)
}
cbs_data$age_groups <- age_groups_fun(cbs_data$donation_age)

#Convert date of sample collection to week of collection
cbs_data$week <- floor_date(cbs_data$sampledate,unit = "week",
                   week_start = 1) #Monday selected as start of week

#Convert date of sample collection to month of collection
cbs_data$month <- floor_date(cbs_data$sampledate, unit = "month")

#Categorize race as white, visible minority, or missing
cbs_data$race<-case_when(
  cbs_data$ethnic1 == "0 missing" ~ "Missing",
  cbs_data$ethnic1 == "0 Missing" ~ "Missing",
  cbs_data$ethnic1 == "1 White" ~ "White",
  cbs_data$ethnic1 == "2 Aborigin" ~ "Visible minority",
  cbs_data$ethnic1 == "2 Aboriginal" ~ "Visible minority",
  cbs_data$ethnic1 == "3 Asian" ~ "Visible minority",
  cbs_data$ethnic1 == "4 Others" ~ "Visible minority",
  cbs_data$ethnic1 == "4 Other" ~ "Visible minority",
)

#Categorize donors by urban vs rural residence -- urban coded as 1 and rural coded as 0
cbs_data$urban <- with(cbs_data,ifelse(substr(fsa,start = 2,stop = 2) != "0","Urban","Rural"))
cbs_data$sex<-case_when(
  cbs_data$sex == "F" ~ "Female",
  cbs_data$sex == "M" ~ "Male"
)
#Generate final df
cbs_df<-cbs_data %>% 
  select(age_groups,sex,race,urban,quintmat,quintsoc,province) %>% 
  mutate(quintmat = ifelse(is.na(quintmat),999,quintmat),
         quintsoc = ifelse(is.na(quintsoc),999,quintsoc)) %>%  #code missing as 999 for sensitivity analyses
  filter(province != ("YT") & province !=("NU/NT") & province != ("QC") &
           race != "Missing" & age_groups != "0-17 years")

#Counts by age-sex-urban
cbs_asu<-cbs_df %>%
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()
  
#Counts by age-sex-race
cbs_asr<-cbs_df %>%
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Sensitivity analysis - % of missing observations in each variable
nrow(cbs_df[cbs_df$race == "missing",])/nrow(cbs_df) #7%
nrow(cbs_df[cbs_df$quintmat == 999,])/nrow(cbs_df)#12%
nrow(cbs_df[cbs_df$quintsoc == 999,])/nrow(cbs_df)#12%

#Save grouped dfs to csv
#write_csv(cbs_asr,"1_data/CBS/cbs_asr_nov62023_count.csv")
#write_csv(cbs_asu,"1_data/CBS/cbs_asu_nov62023_count.csv")

# Outpatient Laboratory (APL) ---------------------------------------------
#Manually remove duplicate entries and regenerate order ID
apl_data<-apl_data %>% 
  filter(order_ID != 1253 & order_ID != 1521 & order_ID != 2728 & order_ID != 3247) %>% 
  mutate(order_ID = 1:214776)

#Generate urban variable
apl_data<-apl_data %>% 
  mutate(PAT_FSA = ifelse(
           substring(PAT_FSA,2,2) == "O",
           paste(substr(PAT_FSA,1,1),"0",substr(PAT_FSA,3,3),
                 sep = ""),PAT_FSA)) %>% 
  filter(nchar(apl_data$PAT_FSA) == 3 
         & substr(apl_data$PAT_FSA,1,1) == "T"
         & apl_data$GENDER != "Unknown")

apl_data<-apl_data %>%
  mutate(urban = case_when(
    substr(PAT_FSA,start = 2,stop = 2) == 0 ~ "Rural",
    substr(PAT_FSA,start = 2,stop = 2) != 0 ~ "Urban"))

#Convert collection date to as.Date()
attr(apl_data$COLLECTION_DATE[1],"tz") #UTC timezone
apl_data$COLLECTION_DATE<-as.Date(apl_data$COLLECTION_DATE,tz = "UTC")

#Generate age group variable
apl_data<-apl_data %>%
 mutate(age_groups = age_groups_fun(apl_data$AGE_AT_COLLECTION))

#Generate final df
apl_df<-apl_data %>% 
  select(age_groups,GENDER,urban)
colnames(apl_df)<-c("age_groups","sex","urban")

#Counts by age-sex-urban
apl_asu<-apl_df %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#save grouped dfs to csv
#write_csv(apl_asu,"1_data/APL/apl_asu_nov72023_count.csv")

# Probabilistic Survey 1 (Ab-c) ---------------------------------------
######################################################################
# Filter each time period then rbind back together.
######################################################################

#Period 1
abc_data1<-abc_data %>% 
  filter(p1_age != is.na(p1_age) & p1_qe2 != is.na(p1_qe2) &
           p1_province != is.na(p1_province) & p1_vizmin != is.na(p1_vizmin)
         & p1_fsa != is.na(p1_fsa) & p1_ethnicity_1 != is.na(p1_ethnicity_1)
         & p1_result_sinai != "") #NAs in result tagged with ""
abc_data1<-abc_data1 %>% 
  mutate(province = p1_province,
         urban = case_when(
           substr(p1_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p1_fsa,start = 2,stop = 2) != 0 ~ "Urban"),
         age_groups = age_groups_fun(as.numeric(abc_data1$p1_age)),
         sex = case_when(p1_qe2 == 1 ~ "Male",
                         p1_qe2 == 2 ~ "Female",
                         p1_qe2 == 3 ~ "Self described"),
         race = ifelse(p1_vizmin == 2 & p1_ethnicity_1 != 1,
                       "White","Visible minority"))

#Generate final df for p1
abc_df1<-abc_data1 %>% 
  select(age_groups,sex,urban,race,province)

#Period 2
abc_data$p2_received_date<-as.Date(abc_data$p2_received_date)
abc_data2<-abc_data %>% 
  filter(p2_age != is.na(p2_age) & p2_qe2 != is.na(p2_qe2) &
           p2_province != is.na(p2_province) & p2_vizmin != is.na(p2_vizmin)
         & p2_fsa != is.na(p2_fsa) & p2_ethnicity_1 != is.na(p2_ethnicity_1)
         & p2_suggested_status != "Fail - NSQ")
abc_data2<-abc_data2[which(abc_data2$p2_np_igg_pred != ""
      & abc_data2$p2_rbd_igg_pred != ""
      & abc_data2$p2_smt1_igg_pred != ""),] #remove individuals with no serology available
abc_data2<-abc_data2 %>% 
  mutate(province = p2_province,
         urban = case_when(
           substr(p2_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p2_fsa,start = 2,stop = 2) != 0 ~ "Urban"),
         age_groups = age_groups_fun(as.numeric(abc_data2$p2_age)),
         sex = case_when(p2_qe2 == 1 ~ "Male",
                         p2_qe2 == 2 ~ "Female",
                         p2_qe2 == 3 ~ "Self described"),
         race = ifelse(p2_vizmin == 2 & p2_ethnicity_1 != 1,
                       "White","Visible minority"))

#Generate final df for p2
abc_df2<-abc_data2 %>% 
  select(age_groups,sex,urban,race,province)

#Period 3
abc_data3<-abc_data %>% 
  filter(p3_age != is.na(p3_age) & p3_qe2 != is.na(p3_qe2) &
           p3_province != is.na(p3_province) & p3_vizmin != is.na(p3_vizmin)
         & p3_fsa != is.na(p3_fsa) & p3_ethnicity_1 != is.na(p3_ethnicity_1)
         & p3_suggested_status != "Technical Failure - NSQ")
abc_data3<-abc_data3[which(abc_data3$p3_np_igg_pred != ""
                           & abc_data3$p3_rbd_igg_pred != ""
                           & abc_data3$p3_smt1_igg_pred != ""),] #remove individuals with no serology available
abc_data3<-abc_data3 %>% 
  mutate(province = p3_province,
         urban = case_when(
           substr(p3_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p3_fsa,start = 2,stop = 2) != 0 ~ "Urban"),
         age_groups = age_groups_fun(as.numeric(abc_data3$p3_age)),
         sex = case_when(p3_qe2 == 1 ~ "Male",
                         p3_qe2 == 2 ~ "Female",
                         p3_qe2 == 3 ~ "Self described"),
         race = ifelse(p3_vizmin == 2 & p3_ethnicity_1 != 1,
                       "White","Visible minority"))

#Generate final p3 df
abc_df3<-abc_data3 %>% 
  select(age_groups,sex,urban,race,province)

#Period 4
abc_data4<-abc_data %>% 
  filter(p4a_age != is.na(p4a_age) & p4a_qe2 != is.na(p4a_qe2) &
           p4a_province != is.na(p4a_province) & p4a_vizmin != is.na(p4a_vizmin)
         & p4a_fsa != is.na(p4a_fsa) & p4a_ethnicity_1 != is.na(p4a_ethnicity_1)
         & p4_suggested_status != "Technical failure")
abc_data4<-abc_data4[which(abc_data4$p4_np_igg_pred != ""
                           & abc_data4$p4_rbd_igg_pred != ""
                           & abc_data4$p4_smt1_igg_pred != ""),] #remove individuals with no serology available

abc_data4<-abc_data4 %>% 
  mutate(province = p4a_province,
         urban = case_when(
           substr(p4a_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p4a_fsa,start = 2,stop = 2) != 0 ~ "Urban"),
         age_groups = age_groups_fun(as.numeric(abc_data4$p4a_age)),
         sex = case_when(p4a_qe2 == 1 ~ "Male",
                         p4a_qe2 == 2 ~ "Female",
                         p4a_qe2 == 3 ~ "Self described"),
         race = ifelse(p4a_vizmin == 2 & p4a_ethnicity_1 != 1,
                       "White","Visible minority"))
        
#Generate final p4 df
abc_df4<-abc_data4 %>% 
  select(age_groups,sex,urban,race,province)

#Generate final working df
abc_df<-do.call("rbind",list(abc_df1,abc_df2,abc_df3,abc_df4)) #counts match ab-c documentation
abc_df<-abc_df %>% filter(sex != "Self described")

#Group by age-sex-urban
abc_asu<-abc_df %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Group by age-sex-race
abc_asr<-abc_df %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Write to csv
#write_csv(abc_asu,"1_data/Ab-c/abc_asu_nov82023_count.csv")
#write_csv(abc_asr,"1_data/Ab-c/abc_asr_nov82023_count.csv")

# Probabilistic Survey 2 (CLSA) -----------------------------

# Probabilistic Survey 3 (CANPATH) ------------------------------------------------

# Probabilistic Survey 4 (CCAHS) --------------------------------------------






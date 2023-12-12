"This script documents all data cleaning and transformations performed
on cohort datasets prior to plotting"

# Load data and functions ---------------------------------------------------------------
setwd("~/serosurveillance-cohort-representativeness/1_data/private") #remove final pub
library(haven)
library(lubridate)
library(tidyverse)# loads readr
library(DBI)
library(RPostgres)
library(flextable)

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
load("./APL/RFD4682e.RData")
apl_data<-RFD4682_e
rm(RFD4682_e)

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

#Functions
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

#Create age groups

age_groups_fun <- function(variable){ #age buckets corresponding with census
  age_group = cut(variable,
                  breaks = c(0,18,40,55,Inf),
                  labels = c("0-17 years","18-39 years","40-54 years",
                             "55+ years"),
                  right = FALSE)
  return(age_group)
}

age_groups_fun2 <- function(variable){
 age_group = cut(variable,
                breaks = c(0,18,27,37,47,56,
                          Inf),
             labels = c("< 0-18 years","18-26 years","27-36 years",
                        "37-46 years","47-55 years","56+ years"),
           right = FALSE)
return(age_group)
}


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

province_fun3 <- function(var) {
  fsa_f = as.character(substr(var,1,1))
  prov =  case_when(fsa_f == "A"~ "NL",
                    fsa_f == "B"~ "NS",
                    fsa_f == "C"~ "PE",
                    fsa_f == "E"~ "NB",
                    fsa_f %in% c("G","H","J")~ "QC",
                    fsa_f %in% c("K","L","M","N","P")~"ON",
                    fsa_f == "R"~ "MB",
                    fsa_f == "S"~ "SK",
                    fsa_f == "T"~ "AB",
                    fsa_f == "V"~ "BC",
                    fsa_f == "X"~ "NU/NT",
                    fsa_f == "Y"~ "YT", 
                    TRUE~NA)
  return(prov)}
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

cbs_data<-cbs_data %>% mutate(province = province_fun(fsa))

#Convert dob of participant to age at donation
cbs_data$year_donation<-as.numeric(format(as.Date(cbs_data$sampledate),"%Y")) #extract year of sample donation
cbs_data$donation_age <- cbs_data$year_donation - cbs_data$dob

#Fix erratic dob entries -- dob "1862" and "1854" should have been "1962" and "1954"
cbs_data$donation_age[cbs_data$donation_age > 120]
cbs_data$donation_age[cbs_data$donation_age < 16]
cbs_data$donation_age <- ifelse(cbs_data$donation_age > 120, cbs_data$donation_age - 100, cbs_data$donation_age)

cbs_data$age_groups <- age_groups_fun(cbs_data$donation_age)

#Convert date of sample collection to month of collection
cbs_data$month <- floor_date(cbs_data$sampledate, unit = "2 months")

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
  select(age_groups,sex,race,urban,quintmat,quintsoc,province,month) %>% 
  mutate(quintmat = ifelse(is.na(quintmat),999,quintmat),
         quintsoc = ifelse(is.na(quintsoc),999,quintsoc)) %>%  #code missing as 999 for sensitivity analyses
  filter(province != ("YT") & province !=("NU/NT") & province != ("QC") &
           race != "Missing" & age_groups != "0-17 years" & quintmat != 999 &
           quintsoc != 999)

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

#Counts by age-sex-quintmat
cbs_asq<-cbs_df %>% 
  group_by(sex,quintmat) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Save grouped dfs to csv
#write_csv(cbs_asr,"./cbs_asr_nov62023_count.csv")
#write_csv(cbs_asu,"./cbs_asu_nov62023_count.csv")
write_csv(cbs_asq,"./cbs_asq_dec62023_count.csv")
#write_csv(cbs_df,"./cbs_df.csv")

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

apl_data$urban <- with(apl_data,
                       ifelse(substr(PAT_FSA,start = 2,stop = 2) != "0","Urban","Rural"))

#Convert collection date to as.Date()
attr(apl_data$COLLECTION_DATE[1],"tz") #UTC timezone
apl_data$COLLECTION_DATE<-as.Date(apl_data$COLLECTION_DATE,tz = "UTC")
apl_data$month<-floor_date(apl_data$COLLECTION_DATE,unit = "2 months")

#Generate age group and province variables
apl_data<-apl_data %>%
 mutate(age_groups = age_groups_fun(apl_data$AGE_AT_COLLECTION),
        province = province_fun(apl_data$PAT_FSA))

#Generate final df
apl_df<-apl_data %>% 
  select(age_groups,GENDER,urban,province,month)
colnames(apl_df)<-c("age_groups","sex","urban","province","month")

#Counts by age-sex-urban
apl_asu<-apl_df %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#save grouped dfs to csv
#write_csv(apl_asu,"./apl_asu_nov72023_count.csv")
#write_csv(apl_df,"./apl_df.csv")

# Probabilistic Survey 1 (Ab-c) ---------------------------------------

#Period 1
abc_data1<-abc_data %>% 
  filter(p1_age != is.na(p1_age) & p1_qe2 != is.na(p1_qe2) &
           p1_province != is.na(p1_province) & p1_vizmin != is.na(p1_vizmin)
         & p1_fsa != is.na(p1_fsa) & p1_ethnicity_1 != is.na(p1_ethnicity_1)
         & p1_result_sinai != "") #NAs in result tagged with "" - remove individuals with no sample
abc_data1<-abc_data1 %>% 
  mutate(month = as.Date(p1_int_month),
         province = p1_province,
         urban = case_when(
           substr(p1_fsa,start = 2,stop = 2) == 0 ~ "Rural",
           substr(p1_fsa,start = 2,stop = 2) != 0 ~ "Urban"),
         age_groups = age_groups_fun(as.numeric(abc_data1$p1_age)),
         sex = case_when(p1_qe2 == 1 ~ "Male",
                         p1_qe2 == 2 ~ "Female",
                         p1_qe2 == 3 ~ "Self described"),
         race = ifelse(p1_vizmin == 2 & p1_ethnicity_1 != 1,
                       "White","Visible minority"))

#categorize month sample received into two month buckets
abc_data1$month<-case_when(
  abc_data1$month == 5 ~ "2020-05-01",
  abc_data1$month == 6 ~ "2020-06-01",
  abc_data1$month == 7 ~ "2020-07-01",
  abc_data1$month == 8 ~ "2020-08-01",
  abc_data1$month == 9 ~ "2020-09-01",
  TRUE ~ NA)

#Place date sample received into 2 month time buckets
abc_data1$month<-floor_date(as.Date(abc_data1$month),unit = "2 months")

#Generate final df for p1
abc_df1<-abc_data1 %>% 
  select(age_groups,sex,urban,race,province,month)

#Period 2
abc_data2<-abc_data %>% 
  filter(p2_age != is.na(p2_age) & p2_qe2 != is.na(p2_qe2) &
           p2_province != is.na(p2_province) & p2_vizmin != is.na(p2_vizmin)
         & p2_fsa != is.na(p2_fsa) & p2_ethnicity_1 != is.na(p2_ethnicity_1)
         & p2_suggested_status != "Fail - NSQ") #remove individuals with failed samples
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

#Place date sample received into 2 month time buckets
abc_data2$month<-as.Date(abc_data2$p2_received_date)
abc_data2$month<-floor_date(abc_data2$month,unit = "2 months")

#Generate final df for p2
abc_df2<-abc_data2 %>% 
  select(age_groups,sex,urban,race,province,month)

#Period 3
abc_data3<-abc_data %>% 
  filter(p3_age != is.na(p3_age) & p3_qe2 != is.na(p3_qe2) &
           p3_province != is.na(p3_province) & p3_vizmin != is.na(p3_vizmin)
         & p3_fsa != is.na(p3_fsa) & p3_ethnicity_1 != is.na(p3_ethnicity_1)
         & p3_suggested_status != "Technical Failure - NSQ") #remove individuals with failed samples
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

#Place date sample received into 2 month time buckets
abc_data3$month<-as.Date(abc_data3$p3_dbs_received_date)
abc_data3$month<-floor_date(abc_data3$month,unit = "2 months")

#Generate final p3 df
abc_df3<-abc_data3 %>% 
  select(age_groups,sex,urban,race,province,month)

#Period 4
abc_data4<-abc_data %>% 
  filter(p4a_age != is.na(p4a_age) & p4a_qe2 != is.na(p4a_qe2) &
           p4a_province != is.na(p4a_province) & p4a_vizmin != is.na(p4a_vizmin)
         & p4a_fsa != is.na(p4a_fsa) & p4a_ethnicity_1 != is.na(p4a_ethnicity_1)
         & p4_suggested_status != "Technical failure") #remove individuals with failed samples
abc_data4<-abc_data4[which(abc_data4$p4_np_igg_pred != ""
                           & abc_data4$p4_rbd_igg_pred != ""
                           & abc_data4$p4_smt1_igg_pred != ""),] #remove individuals with no serology available

#Place date sample received into 2 month time buckets
abc_data4$month<-as.Date(abc_data4$p4_dbs_received_date)
abc_data4$month<-floor_date(abc_data4$month,unit = "2 months")

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
  select(age_groups,sex,urban,race,province,month)

#Generate final working df
abc_df<-do.call("rbind",list(abc_df1,abc_df2,abc_df3,abc_df4)) #counts match ab-c documentation
abc_df<-abc_df %>% filter(sex != "Self described")
abc_df$province<-province_fun2(abc_df$province)

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
#write_csv(abc_asu,"./abc_asu_nov82023_count.csv")
#write_csv(abc_asr,"./abc_asr_nov82023_count.csv")
#write_csv(abc_df,"./abc_df.csv")

# Probabilistic Survey 2 (CLSA) -----------------------------
#Colnames for combined dataset
col_list <- c("entity_id", "source_COVID", "PROV_COVID", "FSA_COVID", "CSD_COVID", "URBAN_RURAL_COVID", "POP_CNTR_COVID", 
              "POP_DENSITY_COVID", "cohort")
df_clsa_cb <- df_clsa_cb[col_list]

#Colnames for antibody cohort
col_list <- c("entity_id", "SER_ADM_COV", "SER_AGE_COV", "SER_SEX_COV", "start_datetime_COV", "SER_CURRSEX_COV", 
              "SER_CURRSEX_SP_COV", "SER_GENDER_COV", "SER_GENDER_SP_COV", "SER_ETHN_WH_COV", "SER_ETHN_SA_COV", 
              "SER_ETHN_ZH_COV", "SER_ETHN_BL_COV", "SER_ETHN_FP_COV", "SER_ETHN_LA_COV", "SER_ETHN_AR_COV", "SER_ETHN_SE_COV",
              "SER_ETHN_WA_COV", "SER_ETHN_KO_COV", "SER_ETHN_JA_COV", "SER_ETHN_OTSP_COV", "SER_ETHN_DK_NA_COV", 
              "SER_ETHN_REFUSED_COV","SER_EDU_COV", "SER_LIVH_NB_COV", "SER_BEDR_NB_COV", "SER_BATHR_NB_COV", "SER_WRK_HCW_COV", 
              "SER_WRK_FR_COV", "SER_WRK_CCW_COV", "SER_WRK_CO_COV", "SER_WRK_TC_COV", "SER_WRK_FS_COV", "SER_WRK_GS_COV", 
              "SER_WRK_PH_COV", "SER_WRK_AT_COV", "SER_WRK_FA_COV", "SER_WRK_FW_COV", "SER_WRK_TD_COV", "SER_WRK_HD_COV", 
              "SER_PG10_NB_COV", "SER_FAMPH_COV", "SER_FLUVAC_COV", "SER_MASK_COV", "SER_DIST_COV", "SER_CROWD_COV", 
              "SER_GREET_COV", "SER_LIMIT_COV", "SER_SLFISO_COV", "SER_SLFQA_COV", "SER_VAC_COV", "SER_VDOSE_COV",
              "ICQ_start_datetime_COV", "BLD_WNOB_COV", "BLD_WNOB_SP_COV", "BLD_FATT_COV", "BLD_FATT_NO_COMMT_COV", 
              "BLD_DECL_POS_COV", "BLD_TECH_REA_COV", "BLD_NEEDLE_COV", "BLD_SIT_REC_COV", "SER_VACTYPE_OTSP_COV", 
              "SER_NUCLEOCAPSID_COV", "SER_SPIKE_COV", "SER_ABRSLT_COV")
df_clsa <- df_clsa_anti[col_list]

#Join in fsa to antibody df
df_all_clsa <- merge(df_clsa, df_clsa_cb, by='entity_id', all.x = TRUE)

#Create province variable
df_all_clsa$province <- province_fun3(df_all_clsa$FSA_COVID)

#Create age variable
df_all_clsa$age = df_all_clsa$SER_AGE_COV
df_all_clsa$age_groups = cut(df_all_clsa$age, 
                            breaks = c(18,40,55,Inf),
                            labels = c('18-39 years',
                                       '40-54 years',
                                       '55+ years'),
                            right = FALSE)

#Restrict sex to male or female
df_all_clsa <- df_all_clsa[df_all_clsa$SER_SEX_COV %in% c('M','F'),] #11946

#Classify residence as urban or rural
df_all_clsa$urban <- with(df_all_clsa,ifelse(substr(FSA_COVID,start = 2,stop = 2) != "0",
                                             "Urban","Rural"))

#Classify race as white or visible minority
df_all_clsa$race = with(df_all_clsa, ifelse(SER_ETHN_WH_COV==1, 1, NA))

df_all_clsa$race = case_when(df_all_clsa$SER_ETHN_WH_COV==1 ~ 1,
                             df_all_clsa$SER_ETHN_SA_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_ZH_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_BL_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_FP_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_LA_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_AR_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_SE_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_WA_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_KO_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_JA_COV==1 ~ 0,
                             df_all_clsa$SER_ETHN_OTSP_COV %in% c("guianese of east indian descent","mixed black and white ancestry","indian","india","south america - biracial",
                                                                  "west indian of east indian descent","300 hundreds year ago my ancestors came from great britain, prior to that we all originated from africa.",
                                                                  "west  indian") ~ 0,
                             TRUE~NA)

#exclude participants with missing samples
df_all_clsa <- df_all_clsa[df_all_clsa$SER_NUCLEOCAPSID_COV >= 0 |
                             df_all_clsa$SER_SPIKE_COV >= 0,] # 10417

#classify interview start date (proxy for sampledate)
df_all_clsa$month<-floor_date(as.Date(df_all_clsa$start_datetime_COV),
                              unit = "2 months")

#generate final df
clsa_df<-df_all_clsa %>% 
  select(age_groups,SER_SEX_COV,province,urban,race,month) %>% 
  mutate(race = case_when(
    race == 1 ~ "White",
    race == 0 ~ "Visible minority",
    TRUE ~ NA
  ),
  SER_SEX_COV = case_when(
    SER_SEX_COV == "F" ~ "Female",
    SER_SEX_COV == "M" ~ "Male"
  )) %>% 
  filter(SER_SEX_COV != is.na(SER_SEX_COV),
         province != is.na(province),
         urban != is.na(urban),
         race != is.na(race))
colnames(clsa_df)[2]<-"sex"

#Counts by age-sex-urban
clsa_asu<-clsa_df %>%
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Counts by age-sex-race
clsa_asr<-clsa_df %>%
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Sensitivity analysis - % of missing observations in each variable

#write to csv
#write_csv(clsa_asu,"./clsa_asu_nov202023_count.csv")
#write_csv(clsa_asr,"./clsa_asr_nov202023_count.csv")
#write_csv(clsa_df,"./clsa_df.csv")

# Probabilistic Survey 3 (CANPATH) ------------------------------------------------

#Merge admin to serology results for main dataset (all cohorts except Manitoba)
canpath_data <- merge(canpath_data, canpath_seradmin, by='ResearcherID', all.x = TRUE)
canpath_data <- merge(canpath_serres,canpath_data, by='ResearcherID', all.x = TRUE)
cnames <- c("ResearcherID",'C1_SDC_AGE', 'C1_SDC_SEX', 'C1_SDC_GENDER', 'C1_ADM_FSA',
            "C1_SDC_EB_ARAB","C1_SDC_EB_BLACK","C1_SDC_EB_CHINESE",
            "C1_SDC_EB_FILIPINO","C1_SDC_EB_JAPANESE",
            "C1_SDC_EB_KOREAN","C1_SDC_EB_LATIN","C1_SDC_EB_S_ASIAN",
            "C1_SDC_EB_SE_ASIAN","C1_SDC_EB_W_ASIAN","C1_SDC_EB_WHITE",
            "C1_SDC_EB_OTHER", "C1_SDC_EB_OTHER_OTSP","C1_SDC_EB_CA","C1_SDC_EB_RESERVE_CURR",  
            "C1_ADM_COLLECT_DATE","C1_SAMPLE_ANTIGEN_TESTED","C1_SAMPLE_RESULT","C1_SAMPLE_ANTIGEN_CUTOFF",
              "C1_SAMPLE_RESULTS_DESCRIPTION", "C1_CITF_ASSAY_ID", 'C1_SAMPLE_SUGGESTED_STATUS')#C1 indicates C-19 substudy
canpath_data <- canpath_data[cnames]

#Clean demographics and include only participants with a sample result
canpath_data<-canpath_data %>% filter(is.na(C1_SDC_AGE)!=T & is.na(C1_SDC_SEX)!=T & 
                                        is.na(C1_ADM_FSA)==F& C1_ADM_FSA != 8 
                                      & C1_ADM_FSA != 7 & C1_ADM_FSA != "W7G" &
                                        C1_ADM_FSA != "?6Y" &
                                        C1_SAMPLE_ANTIGEN_TESTED == 1) #(no prov == W,8/7 == no answer / live outside Canada)

#Create province variable
canpath_data$C1_ADM_FSA<-toupper(canpath_data$C1_ADM_FSA)
canpath_data$province<-province_fun(canpath_data$C1_ADM_FSA)

#Remove individuals which do not reside in the province of a regional cohort
canpath_data<-canpath_data[!(canpath_data$province == "SK" | canpath_data$province == "YT"),]

#Create age group variable
canpath_data$age_groups<-age_groups_fun(canpath_data$C1_SDC_AGE)

#Classify residence as urban or rural
canpath_data$urban<-with(canpath_data,
                         ifelse(substr(C1_ADM_FSA,start = 2,stop = 2) != "0",
                                "Urban","Rural"))
#Re-format sex variable (validated)
canpath_data$C1_SDC_SEX<-ifelse(canpath_data$C1_SDC_SEX == 0,"Male","Female")

#Classify ethnicity as white or visible minority - mixed ethnicity labelled as visible minority
canpath_data$race<-NA

#Identify individuals who prefer not to identify their ethnicity
#Check these individuals did not select any other ethnicity options.
which(canpath_data$C1_SDC_EB_CA == 8 & 
        rowSums(canpath_data[,c(6:17,19)],na.rm = T) > 8 &
        is.na(canpath_data[,18]==F)
      )#0
    
canpath_data[which(canpath_data$C1_SDC_EB_CA == 8),"race"]<-"pnts" #115
##-- Do not need to specify col 19 in rowSum because we know that everyone who is pnts
## -- did not select anything else -- ##
#Identify individuals who selected their ethnicity as only white and did not use text box
canpath_data[which(canpath_data$C1_SDC_EB_WHITE == 1 &
        rowSums(canpath_data[,c(6:17)],na.rm = T) == 1 &
        is.na(canpath_data[,18]) == T),"race"]<-"White" #19778

#Identify individuals who selected their ethnicity as only visible minority and did not use text box
# or select "other"
canpath_data[which(is.na(canpath_data$C1_SDC_EB_WHITE) == T &
        is.na(canpath_data$C1_SDC_EB_OTHER) == T &
        is.na(canpath_data$C1_SDC_EB_OTHER_OTSP) == T),"race"]<-"Visible minority" #1380

#Identify individuals who selected their ethnicity as white and 1 or more visible minorities, did not select other,
# did not use text box
canpath_data[which(canpath_data$C1_SDC_EB_WHITE == 1 &
        rowSums(canpath_data[,6:17],na.rm = T) > 1 &
        is.na(canpath_data$C1_SDC_EB_OTHER) == T &
        is.na(canpath_data$C1_SDC_EB_OTHER_OTSP) == T),"race"]<-"Visible minority" #165

#Check remaining unclassified cases are in _other and/or textbox columns
sum(is.na(canpath_data$C1_SDC_EB_OTHER)==F) #332 selected other

#Check those who selected other did not select any other ethnicities and did not use text box
which(is.na(canpath_data$C1_SDC_EB_OTHER)==F & 
        rowSums(canpath_data[,6:17],na.rm = T) > 1 & 
        is.na(canpath_data$C1_SDC_EB_OTHER_OTSP) == T) #0

#Check how may people who selected other used the text box
length(which(is.na(canpath_data$C1_SDC_EB_OTHER)==F &
         is.na(canpath_data$C1_SDC_EB_OTHER_OTSP) == F)) #332 - all who selected other used text box

#Now check the opposite - who used the text box?
sum(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F)#504

#Check if anyone used text box and did not select other
sum(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F & 
      is.na(canpath_data$C1_SDC_EB_OTHER) == T) #172

#Classify those who selected text box and another selection, did not select other (172)

#Of this n = 172 subset, classify those who also only selected white
t1w<-which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F & 
             is.na(canpath_data$C1_SDC_EB_OTHER) == T &
             canpath_data$C1_SDC_EB_WHITE == 1 & 
             rowSums(canpath_data[,6:17],na.rm = T) == 1)
canpath_data[t1w,"race"]<-"White"

#Review text box and switch status if visible minority indicated in text box
canpath_data[t1w,18]

#Of this n = 172 subset, classify those who also selected only visible minorities
t1vm<-which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F & 
                     is.na(canpath_data$C1_SDC_EB_OTHER) == T &
                     is.na(canpath_data$C1_SDC_EB_WHITE) == T)
canpath_data[t1vm,"race"]<-"Visible minority"
#-Note: no txt review because these participants already indicated vm through set indicator variables-

#Of this n = 172 subset, classify those who selected white + visible minority
canpath_data[which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F &
        canpath_data$C1_SDC_EB_WHITE == 1 &
        rowSums(canpath_data[,6:17],na.rm = T) > 1 &
        is.na(canpath_data$C1_SDC_EB_OTHER) == T),"race"]<-"Visible minority"

#Check everyone in n = 172 subset has been classified
which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)== F &
        is.na(canpath_data$C1_SDC_EB_OTHER)==T &
        is.na(canpath_data$race)==T)#0

sum(is.na(canpath_data$race)) #332: just need to classify those who selected other + used text box

#Check if those who selected other and wrote in the textbox also selected other ethnicities
length(which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F &
        is.na(canpath_data$C1_SDC_EB_OTHER)==F &
        rowSums(canpath_data[,6:17],na.rm = T) > 1)) #48

#Of this n = 48 subset, assign those who are only white
t2w<-which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F &
        is.na(canpath_data$C1_SDC_EB_OTHER)==F & 
        canpath_data$C1_SDC_EB_WHITE == T & 
        rowSums(canpath_data[,6:17],na.rm = T) == 2)
canpath_data[t2w,"race"]<-"White"

#Review text box and switch status if they wrote visible minority
canpath_data[t2w,18]

canpath_data[canpath_data$C1_SDC_EB_OTHER_OTSP %in% 
               c("Metis ancestry","90% white 10%Jewish-Arab",
                 "Canadian and Afrikaans","Middle Eastern (Israeli)",
                 "mixed with Jamican"),]$race<-"Visible minority"

#Of this n = 48 subset, assign those who are only visible minority
t2vm<-which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F &
              is.na(canpath_data$C1_SDC_EB_OTHER)==F &
              is.na(canpath_data$C1_SDC_EB_WHITE)==T &
              rowSums(canpath_data[,6:17],na.rm = T) >= 2)
canpath_data[t2vm,"race"]<-"Visible minority"

#Of this n = 48 subset, assign those who selected white + visible minority
canpath_data[which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F &
        is.na(canpath_data$C1_SDC_EB_OTHER)==F &
        canpath_data$C1_SDC_EB_WHITE == 1 &
        rowSums(canpath_data[,6:17],na.rm = T) > 2),"race"]<-"Visible minority"

#Review textbox for remaining participants and assign visible minority status (n = 284)
txtvm<-which(is.na(canpath_data$C1_SDC_EB_OTHER_OTSP)==F &
               is.na(canpath_data$C1_SDC_EB_OTHER)==F &
               rowSums(canpath_data[,6:17],na.rm = T) == 1)

canpath_data[txtvm,"race"]<-"White"

#Review text box entry and change status if visible minority
canpath_data[txtvm,18]
canpath_data[canpath_data$C1_SDC_EB_OTHER_OTSP %in% 
               c("Afro Latino Caribbean decent","East Indian descent, born in Kenya",
                 "Indo-Caribbean","white/asian","Filipino mum European white father",
                 "SE Asian (Taiwanese)","MÃ\u0083Â©tis","Middle Eastern",
                 "i am South African, I carry in my veins European, African, Asian and indigenous Southern Afircan",
                 "West Indian from Guyana","Guyanese - West Indian","South American",
                 "Indo-Caribbean","caucasian - european, indian, iraqi","Metis",
                 "Ugandian of South Asian heritage","Trinidadian","White, Chinese, West Asian",
                 "Metis/European descent","Japanese/Irish","Irish,Scottish,English,Miâ\u0080\u0099kmaq",
                 "White and aboriginal","Middle Eastern Jewish not European",
                 "Hong Kongnese","Chinese, Indian, English","Sudanese/Scottish","Trinidadian",
                 "East Indian born in Guyana","West-Indian",
                 "European & MÃ\u0083Â©tis","South African of mixed races","half Japanese half White",
                 "Armenian","South Asian from East Africa","Black/white","HONGKONGER",
                 "East Asian","west indian","Indigenous / European","White, Indigeneous, Latin-American",
                 "Indian born in the Caribbean.","Metis French Canadian","White and Khoisan",
                 "European Hispanic","Iran","Persian","Guyanese,south asian descent","Mixed heritage (white and black)",
                 "middle eastern (Israel)","Indo Caribbean","Caribbean, Mixed Race","Greek born in Egypt",
                 "Assyrian","European, MÃ\u0083Â©tis, light brown","WEST INDIAN/CHINESE/","Latino and European",
                 "Of Indian decent - Born in Caribbean","White/Southeast Asian","mixed european/chinese",
                 "Indo-caribbean","Mixed Race (Black, White, Hispanic,Soth Asian)","HongKonger",
                 "South Asian/ Mixed South african","Canadian, German & Caribbean","Black Canadian",
                 "anglo burmese","Non-Arab Semite","East Indian","Mixed European & Southeast Asian",
                 "Middle Eastern Jewish"), "race"]<-"Visible minority"

#Generate final df
colnames(canpath_data)[3]<-"sex"
canpath_data<-canpath_data[canpath_data$race != "pnts",] #remove race who prefer not to say

#categorize sampledate into two months bins
canpath_data$month<-floor_date(as.Date(canpath_data$C1_ADM_COLLECT_DATE),
                               unit = "2 months")
can_df<-canpath_data %>% 
  select(ResearcherID,age_groups,sex,urban,race,province,month) #no NAs

#Check if any MTP participants from new dataset are not included in can_df
can_dfmtp<-read.csv("can_dfmtp.csv")
which(!(can_dfmtp$ResearcherID %in% can_df$ResearcherID)) #0

#Counts by age-sex-urban
can_asu<-can_df %>%
  group_by(age_groups,sex,urban) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Counts by age-sex-race
can_asr<-can_df %>%
  group_by(age_groups,sex,race) %>% 
  summarize(count = n()) %>% 
  ungroup()

#Save grouped dfs to csv
#write_csv(can_asu,"./can_asu_dec72023_count.csv")
#write_csv(can_asr,"./can_asr_dec72023_count.csv")
#write_csv(can_df,"./can_df.csv")

# Create table 1 ----------------------------------------------------------
abcreg<-c("AB, BC, MB, NB,\n NL, NT, NS, ON,\n PE, QC, SK, YT")
cbsreg<-c("AB, BC, MB, NB,\n NL, NS, ON, \n PE, SK")

t1<-data.frame(Cohort = c("Action to Beat Coronavirus (Ab-c)",
                          "Canadian Blood Services (CBS)"),
               sf = c("Participant age \u2265 18",
                      "Participant age \u2265 17"),
               rg = c(abcreg,cbsreg),
               sp = c("DBS","Serum"),
               stas = c("XXX specimens from YYY participants XXXX-XXXX",
                        "XXX specimens from YYY participants XXXX-XXXX"),
               nt = c(c("Research cohort \n \n Oversampled participants aged >= 60 "),
                 c("Convenience sample \n \n Random selection of residual blood donor specimens")))
                      
ft1<-flextable(t1) %>% 
  set_header_labels(sf = "Sampling frame",
                    rg = "Region",
                    sp = "Specimen type",
                    stas = "Study Time and Size",
                    nt = "Notes") %>% 
  theme_vanilla()
ft1



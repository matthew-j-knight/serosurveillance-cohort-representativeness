"Updated CCAHS pseudocode for Yuan. This script performs everything needed to extract
CCAHS subgroup counts for our representativeness analysis"

#Load libraries and functions
library(tidyverse)
library(lubridate)
library(tableone)

age_groups_fun <- function(variable){ #wide age buckets - in case counts are too low
  age_group = cut(variable,
                  breaks = c(0,18,40,55,Inf),
                  labels = c("0-17 years","18-39 years","40-54 years",
                             "55+ years"),
                  right = FALSE)
  return(age_group)
}

#Create age groups (version 2)
age_groups_fun2 <- function(variable){
  age_group = cut(variable,
                  breaks = c(0,18,27,37,47,57,
                             Inf),
                  labels = c("< 18 years","18-26 years","27-36 years",
                             "37-46 years","47-56 years","56+ years"),
                  right = FALSE)
  return(age_group)
}

###### Summary and Notes ########################
"In the below pseudocode:
“CCAHS_DF_NAME_HERE”: should be replaced by yuan with the actual name of the ccahs dataset
“XXX_VAR_NAME”: should be replaced by yuan with the actual name of the ccahs column
For example: if I have a dataset named x with a race column R1, the code
  ifelse(df$RACE_VAR_NAME,a,b) would mean: replace df with data frame name (x), 
  and replace RACE_VAR_NAME with the name of the race variable column (R1)."
#################################################

#Data cleaning & preparation
'Target to have the following column format before aggregation:
  Age_groups: character (either "< 0-18 years","18-26 years","27-36 years",
                             "37-46 years","47-56 years",or "57+ years")
  sex: character (Male or Female)
  race: character (White or visible minority)
  urban: character (urban or rural)
  quintmat: integer (1-5)
  quintsoc: integer (1-5)
  province: character
  id: whatever format is available'

#create a data.frame with participant sex, date of sample collection,
# race, quintsoc,quintmat,id,province/territory, age, fsa, and race as columns.
df<-CCAHS_DF_NAME_HERE %>% 
  select(CCAHS_SEX_VAR_NAME,CCAHS_SAMPLEDATE_VAR_NAME,
         CCAHS_QUINTMAT_VAR_NAME,CCAHS_QUINTSOC_VAR_NAME,CCAHS_ID_VAR_NAME,
         CCAHS_PROVINCE/TERRITORIY_VAR_NAME,CCAHS_AGE_VAR_NAME,CCAHS_FSA_VAR_NAME,
         PG_05A,PG_05B,PG_05C,PG_05D,PG_05E,PG_05F,PG_05G,PG_05H,PG_05I,
         PG_05J,PG_05K,PG_05L,WGT_M)

#rename columns for simplicity
#assuming order is sex, race, sampledate,quintma,quintsoc,pid,province,age,fsa
colnames(df)<-c("sex","sampledate",
                "quintmat","quintsoc","pid", "province","age","fsa",
                "PG_05A","PG_05B","PG_05C","PG_05D","PG_05E","PG_05F",
                "PG_05G","PG_05H","PG_05I","PG_05J","PG_05K","PG_05L","weight") 

#create variables and clean existing ones
#Classify sex as male or female. The below syntax assumes sex is coded numerically (f == 0, m == 1)
df$sex<-case_when(
  df$sex == 0 ~ "Female",
  df$sex == 1 ~ "Male",
  TRUE ~ NA)

# Race classification. If someone is white and only white, == white. 
# If someone is mixed with racialized minority and white, == racialized minority.
# If someone is only other, == racialized minority.
# Else, NA
# Line 105 is a sensitivity analysis - alternate classification of race where 
# individuals who are mixed race are classied as white.
eth_cols<-c("PG_05A","PG_05B","PG_05C","PG_05D","PG_05E","PG_05F",
            "PG_05G","PG_05H","PG_05I","PG_05J","PG_05K","PG_05L")
df$race<-NULL
df$race1<-NULL
for(i in 1:nrow(df)){
  #Participant selected a white ethnicity
  if(df[i,eth_cols[1]] %in% 1){
    #Participant also selected a racialized minority
    if(sum(df[i,eth_cols[2:11]] %in% 1) > 0){
      df$race[i]<-"Racialized minority"
    }
    else{
      df$race[i]<-"White"
    }
  }
  
  #Participant did not select a white ethnicity
  else if(sum(df[i,eth_cols[2:11]] %in% 1) > 0){ #selected rac. minority
    df$race[i]<-"Racialized minority"
  }
  #Participant only selected other
  else if(df[i,eth_cols[12]] %in% 1 & 
          all(!df[i,eth_cols[1:11]] %in% c(1))){
    df$race[i]<-"Racialized minority"
  }
  
  #Else,classify as NA
  else{
    df$race[i]<-NA
  }
}


#Sensitivity analysis 1: classify mixed race as 
for(i in 1:nrow(df)){
  #Participant selected a racialized minority ethnicity
  if(sum(df[i,eth_cols[2:11]] %in% 1) > 0){
    #Participant also selected a white ethnicity
    if(df[i,eth_cols[1]] %in% 1){
      df$race1[i]<-"White"
    }
    else{
      df$race1[i]<-"Racialized minority"
    }
  }
  
  #Participant did not select a racialized minority ethnicity
  else if(df[i,eth_cols[1]] %in% 1){ #selected white
    df$race1[i]<-"White"
  }
  #Participant only selected other
  else if(df[i,eth_cols[12]] %in% 1 & 
          all(!df[i,eth_cols[1:11]] %in% c(1))){
    df$race1[i]<-"Racialized minority"
  }
  
  #Else,classify as NA
  else{
    df$race1[i]<-NA
  }
}

#Revert anyone who is white but also indig to indig (also above), only for main analysis
df$race<-ifelse((df$FN_01 %in% 2:4 & df$race == "White")==T,
                "Racialized minority",
                df$race)

#create age groups
df$age_groups<-age_groups_fun2(df$age)

#categorize date of sample collection by month
df$month<-floor_date(as.Date(df$sampledate,tz = "UTC"),unit = "2 months")

#create urban & province
df$urban<-case_when(
  substr(df$fsa,start = 2,stop = 2) == 0 ~ "Rural",
  substr(df$fsa,start = 2,stop = 2) %in% c(1:9) ~ "Urban",
  TRUE ~ NA
)

#parse CCAHS df into two versions: 1) all provinces, 2) only territories
#NOTE FOR YUAN: we do this because we're running two separate representation analyses.
# 1) is calculate CCAHS repratio for all 10 provinces, 2) is to calculate CCAHS rep ratio
# for only the territories.
#first, remove any missing provinces
df<-df[!is.na(df$province),]
df_st<-df #copy clean df, use later
df1<-df[df$province != "NU" & df$province != "NT" & df$province != "YT",]
df2<-df[df$province == "NU" | df$province == "NT" | df$province == "YT",]

#Calculate basic descriptives & add to a dataframe
nobs<-nrow(df) #number of observations included for analysis from clean df
n_unique_ind<-length(unique(df$pid)) #number of unique individuals
sampledate_range<-range(df$sampledate,na.rm = T) #min and max sampledates included in analysis
month_s<-length(unique(floor_date
                       (as.Date(df[!is.na(df$sampledate),]$sampledate),"1 month"))) #number of months sampled
descrip<-data.frame(nobs = nobs,
                    n_un_id = n_unique_ind,
                    min_date = sampledate_range[1],
                    max_date = sampledate_range[2],
                    month_s = month_s)

#NOTE FOR YUAN: would you be able to also quickly calculate the % of missing observations
# removed for each variable and save this
#to a quick .csv? This is so I can report it in the results section. 

#create counts by subgroup for both versions
#age-sex-urban
df1_asu<-df1 %>% 
  filter(!is.na(age_groups) &
           !is.na(sex) & 
           !is.na(urban)) %>% #case-wise analysis: remove missing age/sex/urban values from count
  group_by(age_groups,sex,urban) %>% 
  summarize(count = sum(weight)) %>% 
  ungroup()

#across all age categories
df1_allu<-aggregate(df1_asu,count ~ sex + urban, FUN = sum,drop = F)
df1_allu$age_groups<-"All ages"

#across all age and urban categories
df1_allsu<-df1 %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarize(count = sum(weight)) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

df1_asu<-do.call("rbind",list(df1_asu,df1_allu,df1_allsu))

#age-sex-race
df1_asr<-df1 %>% 
  filter(!is.na(age_groups) & 
           !is.na(sex) &
           !is.na(race)) %>% #case-wise analysis: remove missing age/sex/race values from count
  group_by(age_groups,sex,race) %>% 
  summarize(count = sum(weight)) %>%
  ungroup()

#across all age categories
df1_allr<-aggregate(df1_asr,count ~ sex + race, FUN = sum,drop = F)
df1_allr$age_groups<-"All ages"
df1_asr<-rbind(df1_asr,df1_allr)

#sex-quintmat
df1_qm<-df1 %>% 
  filter(!is.na(quintmat) & !is.na(sex)) %>% #case-wise analysis: remove missing sex/quintmat from count
  group_by(sex,quintmat) %>% 
  summarize(count = sum(weight)) %>%
  ungroup()

#Generate counts by sex strata and combine
df1_alls<-df1 %>% 
  filter(!is.na(sex)) %>%
  group_by(sex) %>% 
  summarize(count = sum(weight)) %>%
  mutate(quintmat = "All quintiles") %>% 
  ungroup()
df1_qm<-rbind(df1_qm,df1_alls)

#sex-quintsoc
df1_qs<-df1 %>% 
  filter(!is.na(quintsoc) & !is.na(sex)) %>% #case-wise analysis: remove missing sex/quintsoc from count
  group_by(sex,quintsoc) %>% 
  summarize(count = sum(weight)) %>%
  ungroup()

#Generate counts by sex strata and combine
df1_allqs<-df1 %>% 
  filter(!is.na(sex)) %>%
  group_by(sex) %>% 
  summarize(count = sum(weight)) %>%
  mutate(quintsoc = "All quintiles") %>% 
  ungroup()
#Combine
df1_qs<-rbind(df1_qs,df1_allqs)

#age-sex
df2_as<-df2 %>% 
  filter(!is.na(age_groups) & !is.na(sex)) %>% #case-wise analysis: remove missing age/sex from count
  group_by(age_groups,sex) %>% 
  summarize(count = sum(weight)) %>%
  ungroup()

#across all age categories
df2_allas<-aggregate(df2_as,count ~ sex, FUN = sum,drop = F)
df2_allas$age_groups<-"All ages"
df2_as<-rbind(df2_as,df2_allas)

#Sensitivity analysis 1: alternative race classification of participants
df1_asr_s<-df1 %>% 
  filter(!is.na(age_groups) & 
           !is.na(sex) &
           !is.na(race1)) %>% #case-wise analysis: remove missing age/sex/race values from count
  group_by(age_groups,sex,race1) %>% 
  summarize(count = sum(weight)) %>%
  ungroup()

#across all age categories
df1_allr_s<-aggregate(df1_asr_s,count ~ sex + race1, FUN = sum,drop = F)
df1_allr_s$age_groups<-"All ages"
df1_asr_s<-rbind(df1_asr_s,df1_allr_s)

#Prepare census data for representation analyses
#age-sex-urban
census_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU") %>%  #remove territories
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)
census_1_all<-census_1 %>%  
  aggregate(count_census ~ sex + urban,
            FUN = sum,
            drop = F)
census_1_all$age_groups<-"All ages"

census_1_allsu<-census_1 %>% 
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
census_1_allsu<-census_1_allsu %>% 
  mutate(age_groups = "All ages",
         urban = "All regions")
census_1<-do.call("rbind",list(census_1,census_1_all,census_1_allsu))

#age-sex-race
censusr_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU") %>%  #remove territories
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)
censusr_1_all<-censusr_1 %>%  
  aggregate(count_census ~ sex + race,
            FUN = sum,
            drop = F)
censusr_1_all$age_groups<-"All ages"
censusr_1<-rbind(censusr_1,censusr_1_all)

#sex-quintmat
censusqm_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU"
         & !is.na(quintmat)) %>%  #remove territories
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)
censusqm_1_all<-censusqm_1 %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
censusqm_1_all$quintmat<-"All quintiles"
censusqm_1<-rbind(censusqm_1,censusqm_1_all)

#sex-quintsoc
censusqs_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU" &
           !is.na(quintsoc)) %>%  #remove territories
  aggregate(count_census ~ sex + quintsoc,
            FUN = sum,
            drop = F)
censusqs_1_all<-censusqs_1 %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
censusqs_1_all$quintsoc<-"All quintiles"
censusqs_1<-rbind(censusqs_1,censusqs_1_all)

#age-sex (territory analysis)
census_2<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province == "NU" | province == "NT" | province == "YT") %>% 
  aggregate(count_census ~ age_groups + sex,
            FUN = sum,
            drop = F)
census_2_all<-census_2 %>%  
  aggregate(count_census ~ sex,
            FUN = sum,
            drop = F)
census_2_all$age_groups<-"All ages"
census_2<-rbind(census_2,census_2_all)

# Analysis 1: Empty cell analysis -----------------------------------------
"In this analysis, we calculate the proportion of cells which have counts 
greater than 25 when stratified by various demographic variables. This analysis is 
performed only on the CCAHS-1 all province dataset (df1)."
#
#Counts by age-sex-province-samplemonth
cca1_as<-df1 %>% filter(!is.na(age_groups) & !is.na(sex) & !is.na(province) & !is.na(month)) %>% 
  group_by(age_groups,sex,province,month) %>%
  summarize(count = n()) %>% ungroup()
cca1_countas<-round((nrow(cca1_as %>% filter (count > 25)))/nrow(cca1_as),4) * 100

#Age-sex-urban-province-samplemonth
cca1_asu<-df1 %>% filter(!is.na(age_groups) & !is.na(sex) & 
                           !is.na(province) & !is.na(urban) & !is.na(month)) %>%
  group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()
cca1_countasu<-round((nrow(cca1_asu %>% filter (count > 25)))/nrow(cca1_asu),4) * 100

#Age-sex-race-province-samplemonth
cca1_asr<-df1 %>% filter(!is.na(age_groups) & !is.na(sex) & 
                           !is.na(province) & !is.na(race) & !is.na(month)) %>%
  group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
cca1_countasr<-round((nrow(cca1_asr %>% filter(count > 25)))/nrow(cca1_asr),4) * 100

#Age-sex-urban-province-race-samplemonth
cca1_asur<-df1 %>% filter(!is.na(age_groups) & !is.na(sex) & 
                            !is.na(province) & !is.na(urban) &
                            !is.na(race) & !is.na(month)) %>%
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
cca1_countasur<-round((nrow(cca1_asur %>%  filter(count > 25)))/nrow(cca1_asur),4) * 100

#Collect into a data frame for export
df_emptycell<-data.frame(Cohort = "CCAHS",
                         Month_S = length(unique(cca1_asu$month)),
                         Age_Sex_Prov = cca1_countas,
                         Age_Sex_Urban_Prov = cca1_countasu,
                         Age_Sex_Race_Prov = cca1_countasr,
                         Age_Sex_Race_Urban_Prov = cca1_countasur)

## NOTE: This is the first item to export. Table with proportion of demographic cells that
### have a count > 25.
write_csv(df_emptycell,"FILE_PATH_HERE_emptycell.csv")

#Sensitivity analysis 1: calculate asr and asur using alternative race classification
# and save to another table
#Age-sex-race-province-samplemonth
cca1_asr_s<-df1 %>% filter(!is.na(age_groups) & !is.na(sex) & 
                           !is.na(province) & !is.na(race1) & !is.na(month)) %>%
  group_by(age_groups,sex,province,race1,month) %>%
  summarize(count = n()) %>% ungroup()
cca1_countasr_s<-round((nrow(cca1_asr_s %>% filter(count > 25)))/nrow(cca1_asr_s),4) * 100

#Age-sex-urban-province-race-samplemonth
cca1_asur_s<-df1 %>% filter(!is.na(age_groups) & !is.na(sex) & 
                            !is.na(province) & !is.na(urban) &
                            !is.na(race1) & !is.na(month)) %>%
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
cca1_countasur_s<-round((nrow(cca1_asur_s %>%  filter(count > 25)))/nrow(cca1_asur_s),4) * 100

#Collect into a data frame for export
df_emptycell_s<-data.frame(Cohort = "CCAHS_sensitivity",
                           Month_s = length(unique(cca1_asu$month)),
                           Age_Sex_Race_Prov = cca1_countasr_s,
                           Age_Sex_Race_Urban_Prov = cca1_countasur_s)
write_csv(df_emptycell_s,"FILE_PATH_HERE_emptycell_sensitivity.csv")

# Analysis 2: Bootstrap representation ratios -----------------------------
"In this analysis, we perform bootstrapping of representation ratios. The 
representation ratio is defined as the proportion of a given demographic subgroup
which composes the entire study (CCAHS) dataset divided by the proportion of 
the same demographic subgroup in the census dataset."

#NOTE FOR YUAN: I perform two versions of this. 1) CCAHS all provinces,
# 2) CCAHS all territories.
#CCAHS all provinces runs
#Age-sex-urban
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 30)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) &
             !is.na(sex) & 
             !is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across all age and urban categories
  alt1_allsu<-df %>% 
    filter(!is.na(sex)) %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,census_1,by = c("age_groups","sex","urban"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$urban),]
  pct_resample<-c(subcount$count[1:24] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[25] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[26:27] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[28] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[29:30] / sum(subcount$count[1:24],na.rm = T))
  pct_pop<-c(subcount$count_census[1:24] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[25] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[26:27] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[28] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[29:30] / sum(subcount$count_census[1:24],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75,na.rm = T)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
cca_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(cca_fin)[4]<-"rr_prob"
cca_fin$cohort<-"CCAHSp"

#calculate summary statistics (quantiles,mean)
cca1sum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df,na.rm = T) #avg rep ratio
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cca1sum_output[i,]<-final
  
}

cca1sum_output<-data.frame(cca1sum_output)
colnames(cca1sum_output)<-c("mean","2.5","25","50","75","95")

#Age-sex-race
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 28)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) & 
             !is.na(sex) &
             !is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,censusr_1,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  pct_resample<-c(subcount$count[1:24] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[25:28] / sum(subcount$count[25:28],na.rm = T))
  pct_pop<-c(subcount$count_census[1:24] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[25:28] / sum(subcount$count_census[25:28],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75,na.rm = T)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
ccar_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(ccar_fin)[4]<-"rr_prob"
ccar_fin$cohort<-"CCAHSp"

#calculate summary statistics (quantiles,mean)
cca1rsum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df,na.rm = T)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cca1rsum_output[i,]<-final
  
}

cca1rsum_output<-data.frame(cca1rsum_output)
colnames(cca1rsum_output)<-c("mean","2.5","25","50","75","95")

#sex-quintmat
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(quintmat) & !is.na(sex)) %>%
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$quintmat<-"All quintiles"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,censusqm_1,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintmat,subcount$sex),]
  pct_resample<-c(subcount$count[1:10] / sum(subcount$count[1:10],na.rm = T),
                  subcount$count[11:12] / sum(subcount$count[11:12],na.rm = T))
  pct_pop<-c(subcount$count_census[1:10] / sum(subcount$count_census[1:10],na.rm = T),
             subcount$count_census[11:12] / sum(subcount$count_census[11:12],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75,na.rm = T)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
ccaqm_fin<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(ccaqm_fin)[3]<-"rr_prob"
ccaqm_fin$cohort<-"CCAHSp"

#calculate summary statistics (quantiles,mean)
cca1qmsum_output<-matrix(NA,nrow = ncol(output),
                         ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df,na.rm = T)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cca1qmsum_output[i,]<-final
  
}

cca1qmsum_output<-data.frame(cca1qmsum_output)
colnames(cca1qmsum_output)<-c("mean","2.5","25","50","75","95")

#sex-quintsoc
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(quintsoc) & !is.na(sex)) %>%
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$quintsoc<-"All quintiles"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,censusqs_1,by = c("sex","quintsoc"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintsoc,subcount$sex),]
  pct_resample<-c(subcount$count[1:10] / sum(subcount$count[1:10],na.rm = T),
                  subcount$count[11:12] / sum(subcount$count[11:12],na.rm = T))
  pct_pop<-c(subcount$count_census[1:10] / sum(subcount$count_census[1:10],na.rm = T),
             subcount$count_census[11:12] / sum(subcount$count_census[11:12],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75,na.rm = T)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
ccaqs_fin<-cbind(subcount[,c("sex","quintsoc")],sapply(output,rrfun))
colnames(ccaqs_fin)[3]<-"rr_prob"
ccaqs_fin$cohort<-"CCAHSp"

#calculate summary statistics (quantiles,mean)
cca1qssum_output<-matrix(NA,nrow = ncol(output),
                         ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df,na.rm = T)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cca1qssum_output[i,]<-final
  
}

cca1qssum_output<-data.frame(cca1qssum_output)
colnames(cca1qssum_output)<-c("mean","2.5","25","50","75","95")

#CCAHS only territories run
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-df2

output<-matrix(NA,nrow = n_replicates,
               ncol = 14)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) & !is.na(sex)) %>% 
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,census_2,by = c("age_groups","sex"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex),]
  pct_resample<-c(subcount$count[1:12] / sum(subcount$count[1:12],na.rm = T),
                  subcount$count[13:14] / sum(subcount$count[13:14],na.rm = T))
  pct_pop<-c(subcount$count_census[1:12] / sum(subcount$count_census[1:12],na.rm = T),
             subcount$count_census[13:14] / sum(subcount$count_census[13:14],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75,na.rm = T)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
ccat_fin<-cbind(subcount[,c("age_groups","sex")],sapply(output,rrfun))
colnames(ccat_fin)[3]<-"rr_prob"
ccat_fin$cohort<-"CCAHSt"

#calculate summary statistics (quantiles,mean)
cca2assum_output<-matrix(NA,nrow = ncol(output),
                         ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df,na.rm = T)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cca2assum_output[i,]<-final
  
}

cca2assum_output<-data.frame(cca2assum_output)
colnames(cca2assum_output)<-c("mean","2.5","25","50","75","95")

#Sensitivity analysis 1: bootstrap rep ratio with alternative definition of race
#Age-sex-race
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 28)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) & 
             !is.na(sex) &
             !is.na(race1)) %>% 
    group_by(age_groups,sex,race1) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race1, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  #Change race1 colname to race
  colnames(group)<-ifelse(colnames(group) == "race1","race",
                          colnames(group))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,censusr_1,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  pct_resample<-c(subcount$count[1:24] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[25:28] / sum(subcount$count[25:28],na.rm = T))
  pct_pop<-c(subcount$count_census[1:24] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[25:28] / sum(subcount$count_census[25:28],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75,na.rm = T)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
ccar_s_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(ccar_s_fin)[4]<-"rr_prob"
ccar_s_fin$cohort<-"CCAHSp"

#calculate summary statistics (quantiles,mean)
cca1r_s_sum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df,na.rm = T)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cca1r_s_sum_output[i,]<-final
  
}

cca1r_s_sum_output<-data.frame(cca1r_s_sum_output)
colnames(cca1r_s_sum_output)<-c("mean","2.5","25","50","75","95")

## NOTE: This is the second set of items to export. For each set of strata,
# export a table with the strata columns, proportion of representativeness ratios < 0.75,
# and the dataset name (either ccahsp or ccahst). Also export summary statistics 
# for each bootstrap run (2.5%,25%,50%,75%,95% quantiles and mean)

write_csv(cca_fin,"FILE_PATH_HERE_asuprovinces_5000.csv")
write_csv(ccar_fin,"FILE_PATH_HERE_asrprovinces_5000.csv")
write_csv(ccaqm_fin,"FILE_PATH_HERE_sqmprovinces_5000.csv")
write_csv(ccaqs_fin,"FILE_PATH_HERE_sqsprovinces_5000.csv")
write_csv(ccat_fin,"FILE_PATH_HERE_asterritories_5000.csv")
write_csv(ccar_s_fin,"FILE_PATH_HERE_asrprovinces_5000_sensitivity.csv")

write_csv(cca1sum_output,"FILE_PATH_HERE_asuprovstats.csv")
write_csv(cca1rsum_output,"FILE_PATH_HERE_asrprovstats.csv")
write_csv(cca1qmsum_output,"FILE_PATH_HERE_sqmprovstats.csv")
write_csv(cca1qssum_output,"FILE_PATH_HERE_sqsprovstats.csv")
write_csv(cca2assum_output,"FILE_PATH_HERE_asterrstats.csv")
write_csv(cca1r_s_sum_output,"FILE_PATH_HERE_asrprovstats_sensitivity.csv")

#CCAHS representation ratios
asu<-merge(df1_asu,census_1,by = c("age_groups","sex","urban"),
           all.y = T)
asu$count<-ifelse(is.na(asu$count),0,asu$count)
asu<-asu[order(asu$age_groups,asu$sex,asu$urban),]
asu$pct_resample<-c(asu$count[1:24] / sum(asu$count[1:24],na.rm = T),
                asu$count[25] / sum(asu$count[1:24],na.rm = T),
                asu$count[26:27] / sum(asu$count[1:24],na.rm = T),
                asu$count[28] / sum(asu$count[1:24],na.rm = T),
                asu$count[29:30] / sum(asu$count[1:24],na.rm = T))
asu$pct_pop<-c(asu$count_census[1:24] / sum(asu$count_census[1:24],na.rm = T),
           asu$count_census[25] / sum(asu$count_census[1:24],na.rm = T),
           asu$count_census[26:27] / sum(asu$count_census[1:24],na.rm = T),
           asu$count_census[28] / sum(asu$count_census[1:24],na.rm = T),
           asu$count_census[29:30] / sum(asu$count_census[1:24],na.rm = T))
asu$rr<-asu$pct_resample / asu$pct_pop

asr<-merge(df1_asr,censusr_1,by = c("age_groups","sex","race"),
           all.y = T)
asr$count<-ifelse(is.na(asr$count),0,asr$count)
asr<-asr[order(asr$age_groups,asr$sex,asr$race),]
asr$pct_resample<-c(asr$count[1:24] / sum(asr$count[1:24],na.rm = T),
                asr$count[25:28] / sum(asr$count[25:28],na.rm = T))
asr$pct_pop<-c(asr$count_census[1:24] / sum(asr$count_census[1:24],na.rm = T),
           asr$count_census[25:28] / sum(asr$count_census[25:28],na.rm = T))
asr$rr<-asr$pct_resample / asr$pct_pop

sqm<-merge(df1_qm,censusqm_1,by = c("sex","quintmat"),
           all.y = T)
sqm$count<-ifelse(is.na(sqm$count),0,sqm$count)
sqm<-sqm[order(sqm$quintmat,sqm$sex),]
sqm$pct_resample<-c(sqm$count[1:10] / sum(sqm$count[1:10],na.rm = T),
                sqm$count[11:12] / sum(sqm$count[11:12],na.rm = T))
sqm$pct_pop<-c(sqm$count_census[1:10] / sum(sqm$count_census[1:10],na.rm = T),
           sqm$count_census[11:12] / sum(sqm$count_census[11:12],na.rm = T))
sqm$rr<-sqm$pct_resample / sqm$pct_pop

sqs<-merge(df1_qs,censusqs_1,by = c("sex","quintsoc"),
           all.y = T)
sqs$count<-ifelse(is.na(sqs$count),0,sqs$count)
sqs<-sqs[order(sqs$quintsoc,sqs$sex),]
sqs$pct_resample<-c(sqs$count[1:10] / sum(sqs$count[1:10],na.rm = T),
                sqs$count[11:12] / sum(sqs$count[11:12],na.rm = T))
sqs$pct_pop<-c(sqs$count_census[1:10] / sum(sqs$count_census[1:10],na.rm = T),
           sqs$count_census[11:12] / sum(sqs$count_census[11:12],na.rm = T))
sqs$rr<-sqs$pct_resample / sqs$pct_pop

as<-merge(df2_as,census_2,by = c("age_groups","sex"),
          all.y = T)
as$count<-ifelse(is.na(as$count),0,as$count)
as<-as[order(as$age_groups,as$sex),]
as$pct_resample<-c(as$count[1:12] / sum(as$count[1:12],na.rm = T),
                   as$count[13:14] / sum(as$count[13:14],na.rm = T))
as$pct_pop<-c(as$count_census[1:12] / sum(as$count_census[1:12],na.rm = T),
              as$count_census[13:14] / sum(as$count_census[13:14],na.rm = T))
as$rr<-as$pct_resample / as$pct_pop

#Sensitivity analysis 1
colnames(df1_asr_s)<-ifelse(colnames(df1_asr_s) == "race1",
                            "race",colnames(df1_asr_s))
asr_s<-merge(df1_asr_s,censusr_1,by = c("age_groups","sex","race"),
           all.y = T)
asr_s$count<-ifelse(is.na(asr_s$count),0,asr_s$count)
asr_s<-asr_s[order(asr_s$age_groups,asr_s$sex,asr_s$race),]
asr_s$pct_resample<-c(asr_s$count[1:24] / sum(asr_s$count[1:24],na.rm = T),
                    asr_s$count[25:28] / sum(asr_s$count[25:28],na.rm = T))
asr_s$pct_pop<-c(asr_s$count_census[1:24] / sum(asr_s$count_census[1:24],na.rm = T),
               asr_s$count_census[25:28] / sum(asr_s$count_census[25:28],na.rm = T))
asr_s$rr<-asr_s$pct_resample / asr_s$pct_pop

#CCAHS representation ratios by strata
df1_asu<-asu[,c("age_groups","sex","urban","rr")]
df1_asr<-asr[,c("age_groups","sex","race","rr")]
df1_qm<-sqm[,c("quintmat","sex","rr")]
df1_qs<-sqs[,c("quintsoc","sex","rr")]
df2_as<-as[,c("age_groups","sex","rr")]
df1_asr_s<-asr_s[,c("age_groups","sex","race","rr")]

#Prepare census data for export
#age-sex-urban
census_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU") %>%  #remove territories
  aggregate(count_census ~ age_groups + sex + urban + province,
            FUN = sum,
            drop = F)
census_1_all<-census_1 %>%  
  aggregate(count_census ~ sex + urban + province,
            FUN = sum,
            drop = F)
census_1_all$age_groups<-"All ages"
census_1<-rbind(census_1,census_1_all)

#age-sex-race
censusr_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU") %>%  #remove territories
  aggregate(count_census ~ age_groups + sex + race + province,
            FUN = sum,
            drop = F)
censusr_1_all<-censusr_1 %>%  
  aggregate(count_census ~ sex + race + province,
            FUN = sum,
            drop = F)
censusr_1_all$age_groups<-"All ages"
censusr_1<-rbind(censusr_1,censusr_1_all)

#sex-quintmat
censusqm_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU"
         & !is.na(quintmat)) %>%  #remove territories
  aggregate(count_census ~ sex + quintmat + province + age_groups,
            FUN = sum,
            drop = F)
censusqm_1_all<-censusqm_1 %>%  
  aggregate(count_census ~ sex + province + age_groups,
            FUN = sum,
            drop = F)
censusqm_1_all$quintmat<-"All quintiles"
censusqm_1<-rbind(censusqm_1,censusqm_1_all)

#sex-quintsoc
censusqs_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU" &
           !is.na(quintsoc)) %>%  #remove territories
  aggregate(count_census ~ sex + quintsoc + province + age_groups,
            FUN = sum,
            drop = F)
censusqs_1_all<-censusqs_1 %>%  
  aggregate(count_census ~ sex + province + age_groups,
            FUN = sum,
            drop = F)
censusqs_1_all$quintsoc<-"All quintiles"
censusqs_1<-rbind(censusqs_1,censusqs_1_all)

#age-sex (territory analysis)
census_2<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  filter(province == "NU" | province == "NT" | province == "YT") %>% 
  aggregate(count_census ~ age_groups + sex + province,
            FUN = sum,
            drop = F)
census_2_all<-census_2 %>%  
  aggregate(count_census ~ sex + province,
            FUN = sum,
            drop = F)
census_2_all$age_groups<-"All ages"
census_2<-rbind(census_2,census_2_all)

## NOTE: This is the third set of items to export. Export CCAHS 
# rep ratios by strata (age-sex-urban,age-sex-race,sex-quintmat,sex-quintsoc,
# age-sex (territories only)). Also export census counts by strata 
# and province in separate tables, along with descriptive statistics

#Export
write_csv(df1_asu,"FILE_PATH_HERE_ccahs1asu.csv") #10 provinces age-sex-urban
write_csv(df1_asr,"FILE_PATH_HERE_ccahs1asr.csv") #10 provinces age-sex-race
write_csv(df1_qm,"FILE_PATH_HERE_ccahs1sqm.csv") #10 provinces sex-quintmat
write_csv(df1_qs,"FILE_PATH_HERE_ccahs1sqs.csv") #10 provinces sex-quintsoc
write_csv(df2_as,"FILE_PATH_HERE_ccahs2as.csv") #3 territories age-sex
write_csv(df1_asr_s,"FILE_PATH_HERE_ccahs1asr_sensitivity.csv") #10 provinces age-sex-race sensitivity #1

write_csv(census_1,"FILE_PATH_HERE_censasup.csv") #10 provinces age-sex-urban-province
write_csv(censusr_1,"FILE_PATH_HERE_censasrp.csv") #10 provinces age-sex-race-province
write_csv(censusqm_1,"FILE_PATH_HERE_censsqmp.csv") #10 provinces sex-quintmat-province
write_csv(censusqs_1,"FILE_PATH_HERE_censsqsp.csv") #10 provinces sex-quintsoc-province
write_csv(census_2,"FILE_PATH_HERE_censast.csv") #3 territories age-sex-territory
write_csv(descrip,"FILE_PATH_HERE_descriptives.csv") #basic descriptives for publication

# Build and export traditional table 1 -----------------------------------
df_st<-df_st %>% #clean df generated in line 164
  select(age_groups,sex,urban,race,quintmat,quintsoc) %>% 
  mutate(cohort = "CCAHS-1 closed cohort",
         sex = factor(sex,levels = c("Female","Male",NA)),
         race = factor(race,levels = c("Racialized minority","White",NA)),
         urban = factor(urban,levels = c("Rural","Urban",NA)),
         quintmat = factor(quintmat,levels = c("1","2","3","4","5",NA)),
         quintsoc = factor(quintsoc,levels = c("1","2","3","4","5",NA))
  )

st_cca<-CreateTableOne(data = df_st,vars = c("age_groups","quintmat",
                                       "quintsoc","race","sex","urban"),
                       test = F,smd = F,includeNA = T)
#Export
write.csv(print(st_cca,quote = T,noSpaces = T,showAllLevels = T),
          "FILE_PATH_HERE_CCAHS_summary_supplemental_table.csv")
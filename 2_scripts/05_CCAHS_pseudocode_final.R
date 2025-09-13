
# 0. Description ----------------------------------------------------------
# 1. Load
# 2. Empty cell analysis
# 3. Bootstrap representation ratios 
# 4. Calculate representation ratios
# 5. Export

#' This pseudocode was provided to our collaborator to run analyses using the CCAHS-1
#' dataset at Statistics Canada's Research Data Centre, along with pre-processing of 
#' census data. Unlike the other study datasets,
#' all analytic code for CCAHS-1 is included in this single script. 
#' 'X_DF_NAME_HERE' was replaced with object name of read in dataset (CCAHS-1 or census)
#' 'XXX_VAR_NAME' was replaced with variable name.


# 1. Load -----------------------------------------------------------------
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

age_groups_fun2 <- function(variable){
  age_group = cut(variable,
                  breaks = c(0,18,27,37,47,57,
                             Inf),
                  labels = c("< 18 years","18-26 years","27-36 years",
                             "37-46 years","47-56 years","56+ years"),
                  right = FALSE)
  return(age_group)
}

rrfun<-function(x){
  prop<-(sum(x < 0.75,na.rm = T)) / length(x)
  return(prop)
}


df<-CCAHS_DF_NAME_HERE %>% 
  select(CCAHS_SEX_VAR_NAME,CCAHS_SAMPLEDATE_VAR_NAME,
         CCAHS_QUINTMAT_VAR_NAME,CCAHS_QUINTSOC_VAR_NAME,CCAHS_ID_VAR_NAME,
         CCAHS_PROVINCE/TERRITORIY_VAR_NAME,CCAHS_AGE_VAR_NAME,CCAHS_FSA_VAR_NAME,
         PG_05A,PG_05B,PG_05C,PG_05D,PG_05E,PG_05F,PG_05G,PG_05H,PG_05I,
         PG_05J,PG_05K,PG_05L,WGT_M)

#Rename columns for simplicity
#Assuming order is sex, race, sampledate, quintmat, quintsoc, pid, province, age, fsa
colnames(df)<-c("sex","sampledate",
                "quintmat","quintsoc","pid", "province","age","fsa",
                "PG_05A","PG_05B","PG_05C","PG_05D","PG_05E","PG_05F",
                "PG_05G","PG_05H","PG_05I","PG_05J","PG_05K","PG_05L","weight") 

#Classify sex as male or female. Assumes sex is coded numerically (f == 0, m == 1)
df$sex<-case_when(
  df$sex == 0 ~ "Female",
  df$sex == 1 ~ "Male",
  TRUE ~ NA)

#Classify participant race/ethnicity 
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


#Sensitivity analysis 1: classify mixed race/ethnicity as white 
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

#Classify anyone who is white but also indig to indig (also above), only for main analysis
df$race<-ifelse((df$FN_01 %in% 2:4 & df$race == "White")==T,
                "Racialized minority",
                df$race)

#Create age groups
df$age_groups<-age_groups_fun2(df$age)

#Categorize date of sample collection by month
df$month<-floor_date(as.Date(df$sampledate,tz = "UTC"),unit = "2 months")

#Create urban & province
df$urban<-case_when(
  substr(df$fsa,start = 2,stop = 2) == 0 ~ "Rural",
  substr(df$fsa,start = 2,stop = 2) %in% c(1:9) ~ "Urban",
  TRUE ~ NA
)

# Parse df into two versions: 1) all provinces, 2) only territories
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

#Create counts by subgroup for both versions
#Age-sex-urban
df1_asu<-df1 %>% 
  filter(!is.na(age_groups) &
           !is.na(sex) & 
           !is.na(urban)) %>% 
  group_by(age_groups,sex,urban) %>% 
  summarize(count = sum(weight)) %>% 
  ungroup()

#Across all age categories
df1_allu<-aggregate(df1_asu,count ~ sex + urban, FUN = sum,drop = F)
df1_allu$age_groups<-"All ages"

#Across all age and urban categories
df1_allsu<-df1 %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarize(count = sum(weight)) %>% 
  mutate(age_groups = "All ages",
         urban = "All regions") %>% 
  ungroup()

df1_asu<-do.call("rbind",list(df1_asu,df1_allu,df1_allsu))

#Age-sex-race
df1_asr<-df1 %>% 
  filter(!is.na(age_groups) & 
           !is.na(sex) &
           !is.na(race)) %>% 
  group_by(age_groups,sex,race) %>% 
  summarize(count = sum(weight)) %>%
  ungroup()

#Across all age categories
df1_allr<-aggregate(df1_asr,count ~ sex + race, FUN = sum,drop = F)
df1_allr$age_groups<-"All ages"
df1_asr<-rbind(df1_asr,df1_allr)

#Sex-quintmat
df1_qm<-df1 %>% 
  filter(!is.na(quintmat) & !is.na(sex)) %>% 
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

#Sex-quintsoc
df1_qs<-df1 %>% 
  filter(!is.na(quintsoc) & !is.na(sex)) %>% 
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

#Age-sex
df2_as<-df2 %>% 
  filter(!is.na(age_groups) & !is.na(sex)) %>% 
  group_by(age_groups,sex) %>% 
  summarize(count = sum(weight)) %>%
  ungroup()

#Across all age categories
df2_allas<-aggregate(df2_as,count ~ sex, FUN = sum,drop = F)
df2_allas$age_groups<-"All ages"
df2_as<-rbind(df2_as,df2_allas)

#Sensitivity analysis 1: alternative race classification of participants
df1_asr_s<-df1 %>% 
  filter(!is.na(age_groups) & 
           !is.na(sex) &
           !is.na(race1)) %>% 
  group_by(age_groups,sex,race1) %>% 
  summarize(count = sum(weight)) %>%
  ungroup()

#Across all age categories
df1_allr_s<-aggregate(df1_asr_s,count ~ sex + race1, FUN = sum,drop = F)
df1_allr_s$age_groups<-"All ages"
df1_asr_s<-rbind(df1_asr_s,df1_allr_s)

#Prepare census data for representation analyses
#Age-sex-urban
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

#Age-sex-race
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

#Sex-quintmat
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

#Sex-quintsoc
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

#Age-sex (territory analysis)
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

# 2. Empty cell analysis -----------------------------------------

#Age-sex-province-samplemonth
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

#Sensitivity analysis 1: calculate asr and asur using alternative race/ethnicity classification

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


# 3. Bootstrap representation ratios -----------------------------

#CCAHS all provinces runs
#Age-sex-urban
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 30)

set.seed(4)
for(i in 1:n_replicates){
  #Draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #Calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) &
             !is.na(sex) & 
             !is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #Calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #Across all age and urban categories
  alt1_allsu<-df %>% 
    filter(!is.na(sex)) %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allsu))
  
  #Merge resample with corresponding census dataset
  subcount<-merge(group,census_1,by = c("age_groups","sex","urban"),all.y = TRUE)
  
  #Calculate proportions and RR
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
  
  #Save output to matrix
  output[i,]<-rr
}

#Label columns and make data.frame
output<-data.frame(output)

#Generate function to calculate proportion of RRs, for each subgroup, that are < 0.75

#Perform calculation and re-assign bootstrap probability to each subgroup combination
cca_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(cca_fin)[4]<-"rr_prob"
cca_fin$cohort<-"CCAHSp"

#Calculate summary statistics (quantiles,mean)
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
  #Draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #Calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) & 
             !is.na(sex) &
             !is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #Calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #Merge resample with corresponding census dataset
  subcount<-merge(group,censusr_1,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #Calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  pct_resample<-c(subcount$count[1:24] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[25:28] / sum(subcount$count[25:28],na.rm = T))
  pct_pop<-c(subcount$count_census[1:24] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[25:28] / sum(subcount$count_census[25:28],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #Save output to matrix
  output[i,]<-rr
}

#Label columns and make data.frame
output<-data.frame(output)

#Perform calculation and re-assign bootstrap probability to each subgroup combination
ccar_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(ccar_fin)[4]<-"rr_prob"
ccar_fin$cohort<-"CCAHSp"

#Calculate summary statistics (quantiles,mean)
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

#Sex-quintmat
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)
set.seed(4)
for(i in 1:n_replicates){
  #Draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #Calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(quintmat) & !is.na(sex)) %>%
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #Calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$quintmat<-"All quintiles"
  group<-rbind(group,alt)
  
  #Merge resample with corresponding census dataset
  subcount<-merge(group,censusqm_1,by = c("sex","quintmat"),all.y = TRUE)
  
  #Calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintmat,subcount$sex),]
  pct_resample<-c(subcount$count[1:10] / sum(subcount$count[1:10],na.rm = T),
                  subcount$count[11:12] / sum(subcount$count[11:12],na.rm = T))
  pct_pop<-c(subcount$count_census[1:10] / sum(subcount$count_census[1:10],na.rm = T),
             subcount$count_census[11:12] / sum(subcount$count_census[11:12],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #Save output to matrix
  output[i,]<-rr
  print(paste("Run",i,"complete"))
}

#Label columns and make data.frame
output<-data.frame(output)

#Perform calculation and re-assign bootstrap probability to each subgroup combination
ccaqm_fin<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(ccaqm_fin)[3]<-"rr_prob"
ccaqm_fin$cohort<-"CCAHSp"

#Calculate summary statistics (quantiles,mean)
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

#Sex-quintsoc
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)
set.seed(4)
for(i in 1:n_replicates){
  #Draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #Calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(quintsoc) & !is.na(sex)) %>%
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #Calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$quintsoc<-"All quintiles"
  group<-rbind(group,alt)
  
  #Merge resample with corresponding census dataset
  subcount<-merge(group,censusqs_1,by = c("sex","quintsoc"),all.y = TRUE)
  
  #Calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintsoc,subcount$sex),]
  pct_resample<-c(subcount$count[1:10] / sum(subcount$count[1:10],na.rm = T),
                  subcount$count[11:12] / sum(subcount$count[11:12],na.rm = T))
  pct_pop<-c(subcount$count_census[1:10] / sum(subcount$count_census[1:10],na.rm = T),
             subcount$count_census[11:12] / sum(subcount$count_census[11:12],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #Save output to matrix
  output[i,]<-rr
  print(paste("Run",i,"complete"))
}

#Label columns and make data.frame
output<-data.frame(output)

#Perform calculation and re-assign bootstrap probability to each subgroup combination
ccaqs_fin<-cbind(subcount[,c("sex","quintsoc")],sapply(output,rrfun))
colnames(ccaqs_fin)[3]<-"rr_prob"
ccaqs_fin$cohort<-"CCAHSp"

#Calculate summary statistics (quantiles,mean)
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
  #Draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #Calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) & !is.na(sex)) %>% 
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #Calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #Merge resample with corresponding census dataset
  subcount<-merge(group,census_2,by = c("age_groups","sex"),all.y = TRUE)
  
  #Calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex),]
  pct_resample<-c(subcount$count[1:12] / sum(subcount$count[1:12],na.rm = T),
                  subcount$count[13:14] / sum(subcount$count[13:14],na.rm = T))
  pct_pop<-c(subcount$count_census[1:12] / sum(subcount$count_census[1:12],na.rm = T),
             subcount$count_census[13:14] / sum(subcount$count_census[13:14],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #Save output to matrix
  output[i,]<-rr
}

#Label columns and make data.frame
output<-data.frame(output)

#Perform calculation and re-assign bootstrap probability to each subgroup combination
ccat_fin<-cbind(subcount[,c("age_groups","sex")],sapply(output,rrfun))
colnames(ccat_fin)[3]<-"rr_prob"
ccat_fin$cohort<-"CCAHSt"

#Calculate summary statistics (quantiles,mean)
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
  #Draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #Calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) & 
             !is.na(sex) &
             !is.na(race1)) %>% 
    group_by(age_groups,sex,race1) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #Calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race1, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  #Change race1 colname to race
  colnames(group)<-ifelse(colnames(group) == "race1","race",
                          colnames(group))
  
  #Merge resample with corresponding census dataset
  subcount<-merge(group,censusr_1,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #Calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  pct_resample<-c(subcount$count[1:24] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[25:28] / sum(subcount$count[25:28],na.rm = T))
  pct_pop<-c(subcount$count_census[1:24] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[25:28] / sum(subcount$count_census[25:28],na.rm = T))
  rr<-pct_resample / pct_pop
  
  #Save output to matrix
  output[i,]<-rr
}

#Label columns and make data.frame
output<-data.frame(output)

#Perform calculation and re-assign bootstrap probability to each subgroup combination
ccar_s_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(ccar_s_fin)[4]<-"rr_prob"
ccar_s_fin$cohort<-"CCAHSp"

#Calculate summary statistics (quantiles,mean)
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


# 4. Calculate representation ratios ------------------------------------------------
#Age-sex-urban
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

# Age-sex-race
asr<-merge(df1_asr,censusr_1,by = c("age_groups","sex","race"),
           all.y = T)

asr$count<-ifelse(is.na(asr$count),0,asr$count)

asr<-asr[order(asr$age_groups,asr$sex,asr$race),]

asr$pct_resample<-c(asr$count[1:24] / sum(asr$count[1:24],na.rm = T),
                asr$count[25:28] / sum(asr$count[25:28],na.rm = T))

asr$pct_pop<-c(asr$count_census[1:24] / sum(asr$count_census[1:24],na.rm = T),
           asr$count_census[25:28] / sum(asr$count_census[25:28],na.rm = T))

asr$rr<-asr$pct_resample / asr$pct_pop

# Sex-quintmat
sqm<-merge(df1_qm,censusqm_1,by = c("sex","quintmat"),
           all.y = T)

sqm$count<-ifelse(is.na(sqm$count),0,sqm$count)

sqm<-sqm[order(sqm$quintmat,sqm$sex),]

sqm$pct_resample<-c(sqm$count[1:10] / sum(sqm$count[1:10],na.rm = T),
                sqm$count[11:12] / sum(sqm$count[11:12],na.rm = T))

sqm$pct_pop<-c(sqm$count_census[1:10] / sum(sqm$count_census[1:10],na.rm = T),
           sqm$count_census[11:12] / sum(sqm$count_census[11:12],na.rm = T))

sqm$rr<-sqm$pct_resample / sqm$pct_pop

# Sex-quintsoc
sqs<-merge(df1_qs,censusqs_1,by = c("sex","quintsoc"),
           all.y = T)

sqs$count<-ifelse(is.na(sqs$count),0,sqs$count)

sqs<-sqs[order(sqs$quintsoc,sqs$sex),]

sqs$pct_resample<-c(sqs$count[1:10] / sum(sqs$count[1:10],na.rm = T),
                sqs$count[11:12] / sum(sqs$count[11:12],na.rm = T))

sqs$pct_pop<-c(sqs$count_census[1:10] / sum(sqs$count_census[1:10],na.rm = T),
           sqs$count_census[11:12] / sum(sqs$count_census[11:12],na.rm = T))

sqs$rr<-sqs$pct_resample / sqs$pct_pop

# Age-sex
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
#Age-sex-race
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

#Representation ratios by strata
df1_asu<-asu[,c("age_groups","sex","urban","rr")]
df1_asr<-asr[,c("age_groups","sex","race","rr")]
df1_qm<-sqm[,c("quintmat","sex","rr")]
df1_qs<-sqs[,c("quintsoc","sex","rr")]
df2_as<-as[,c("age_groups","sex","rr")]
df1_asr_s<-asr_s[,c("age_groups","sex","race","rr")]

#Prepare census data for export
#Age-sex-urban
census_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU") %>%
  aggregate(count_census ~ age_groups + sex + urban + province,
            FUN = sum,
            drop = F)

census_1_all<-census_1 %>%  
  aggregate(count_census ~ sex + urban + province,
            FUN = sum,
            drop = F)

census_1_all$age_groups<-"All ages"

census_1<-rbind(census_1,census_1_all)

#Age-sex-race
censusr_1<-CENSUS_DF_NAME_HERE %>% 
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Racialized minority",
                          race == "1" ~ "White")) %>% 
  filter(province != "YT" & province != "NT" & province != "NU") %>%
  aggregate(count_census ~ age_groups + sex + race + province,
            FUN = sum,
            drop = F)

censusr_1_all<-censusr_1 %>%  
  aggregate(count_census ~ sex + race + province,
            FUN = sum,
            drop = F)

censusr_1_all$age_groups<-"All ages"

censusr_1<-rbind(censusr_1,censusr_1_all)

#Sex-quintmat
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

#Sex-quintsoc
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

#Age-sex (territory analysis)
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


# 4. Table 1 -----------------------------------
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


# 5. Export ---------------------------------------------------------------
# Empty cell analysis
#write_csv(df_emptycell,"FILE_PATH_HERE_emptycell.csv")
#write_csv(df_emptycell_s,"FILE_PATH_HERE_emptycell_sensitivity.csv")

# Bootstrap
#write_csv(cca_fin,"FILE_PATH_HERE_asuprovinces_5000.csv")
#write_csv(ccar_fin,"FILE_PATH_HERE_asrprovinces_5000.csv")
#write_csv(ccaqm_fin,"FILE_PATH_HERE_sqmprovinces_5000.csv")
#write_csv(ccaqs_fin,"FILE_PATH_HERE_sqsprovinces_5000.csv")
#write_csv(ccat_fin,"FILE_PATH_HERE_asterritories_5000.csv")
#write_csv(ccar_s_fin,"FILE_PATH_HERE_asrprovinces_5000_sensitivity.csv")
#write_csv(cca1sum_output,"FILE_PATH_HERE_asuprovstats.csv")
#write_csv(cca1rsum_output,"FILE_PATH_HERE_asrprovstats.csv")
#write_csv(cca1qmsum_output,"FILE_PATH_HERE_sqmprovstats.csv")
#write_csv(cca1qssum_output,"FILE_PATH_HERE_sqsprovstats.csv")
#write_csv(cca2assum_output,"FILE_PATH_HERE_asterrstats.csv")
#write_csv(cca1r_s_sum_output,"FILE_PATH_HERE_asrprovstats_sensitivity.csv")

# Representation ratios
#write_csv(df1_asu,"FILE_PATH_HERE_ccahs1asu.csv") 
#write_csv(df1_asr,"FILE_PATH_HERE_ccahs1asr.csv") 
#write_csv(df1_qm,"FILE_PATH_HERE_ccahs1sqm.csv") 
#write_csv(df1_qs,"FILE_PATH_HERE_ccahs1sqs.csv") 
#write_csv(df2_as,"FILE_PATH_HERE_ccahs2as.csv")
#write_csv(df1_asr_s,"FILE_PATH_HERE_ccahs1asr_sensitivity.csv")

# Census
#write_csv(census_1,"FILE_PATH_HERE_censasup.csv") 
#write_csv(censusr_1,"FILE_PATH_HERE_censasrp.csv") 
#write_csv(censusqm_1,"FILE_PATH_HERE_censsqmp.csv") 
#write_csv(censusqs_1,"FILE_PATH_HERE_censsqsp.csv") 
#write_csv(census_2,"FILE_PATH_HERE_censast.csv") 

# Descriptives
#write_csv(descrip,"FILE_PATH_HERE_descriptives.csv")

# Table 1
#write.csv(print(st_cca,quote = T,noSpaces = T,showAllLevels = T),
# "FILE_PATH_HERE_CCAHS_summary_supplemental_table.csv")
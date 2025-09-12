
# 0. Description ----------------------------------------------------------
# 1. Load libraries and data
# 2. Bootstrap age-sex-urban representation ratios
# 3. Bootstrap age-sex-race representation ratios
# 4. Bootstrap sex-quintmat representation ratios
# 5. Bootstrap sex-quintsoc representation ratios
# 6. Bootstrap representation ratios for sensitivity analysis 1
# 7. Bootstrap representation ratios for sensitivity analysis 2

# This script bootstraps representation ratios produced for 
# each study's demographic strata. A subgroup is considered significantly 
# underrepresented if 95% of the representation ratios fall below 
# a threshold of 0.75."

# 1. Load libraries and data -------------------------------------------------
rm(list = ls())
library(tidyverse)

#Read in serosurveillance study datasets
cbs_df<-read.csv("./1_data/private/cbs_df_final.csv")
apl_df<-read.csv("./1_data/private/apl_df_final.csv")
abc_df<-read.csv("./1_data/private/abc_df_final.csv")
abc_df_qab <- read.csv("./1_data/private/abc_df_final_qab.csv")
clsa_df<-read.csv("./1_data/private/clsa_df_final.csv")
clsa_df_qab <- read.csv("./1_data/private/clsa_df_final_qab.csv")
can_df<-read.csv("./1_data/private/can_df_final.csv")
can_df1<-read.csv("./1_data/private/can_df1_final.csv")
can_dfs4<-read.csv("./1_data/private/can_df_finals4.csv")

#Read in 2016 census datasets (urban)
a_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_a_abc.csv") #ab-c
c_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_c_cbs.csv") #cbs
d_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_d_canpath.csv") #canpath
e_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_e_apl.csv") #apl
g_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_g_clsa.csv") #clsa

#Read in 2016 census datasets (race)
a_asr<-read.csv("./1_data/private/2016 Canadian Census/censusasr_a_abc.csv") #ab-c
c_asr<-read.csv("./1_data/private/2016 Canadian Census/censusasr_c_cbs.csv") #cbs
d_asr<-read.csv("./1_data/private/2016 Canadian Census/censusasr_d_canpath.csv") #canpath
g_asr<-read.csv("./1_data/private/2016 Canadian Census/censusasr_g_clsa.csv") #clsa

#Read in 2016 census datasets (quintmat) 
c_sqm<-read.csv("./1_data/private/2016 Canadian Census/censussqm_c_cbs.csv") #CBS
e_sqm<-read.csv("./1_data/private/2016 Canadian Census/censussqm_e_apl.csv") #APL
g_sqm<-read.csv("./1_data/private/2016 Canadian Census//censussqm_g_clsa.csv") #CLSA

#Read in 2016 census datasets (quintsoc)
c_sqs<-read.csv("./1_data/private/2016 Canadian Census/censussqs_c_cbs.csv") #CBS
e_sqs<-read.csv("./1_data/private/2016 Canadian Census/censussqs_e_apl.csv") #APL
g_sqs<-read.csv("./1_data/private/2016 Canadian Census//censussqs_g_clsa.csv") #CLSA

#Prepare 2016 census datasets for sensitivity analysis #1
a_asr1<-a_asr %>% 
  mutate(race1 = race)
d_asr1<-d_asr %>% 
  mutate(race1=race)
g_asr1<-g_asr %>% 
  mutate(race1=race)

#Read in census datasets for sensitivity analysis #2
d_asus2<-read.csv("./1_data/private/2016 Canadian Census/censusasu_d_canpath_s2.csv") #canpath
g_asus2<-read.csv("./1_data/private/2016 Canadian Census/censusasu_g_clsa_s2.csv") #clsa
d_asrs2<-read.csv("./1_data/private/2016 Canadian Census/censusasr_d_canpath_s2.csv") #canpath
g_asrs2<-read.csv("./1_data/private/2016 Canadian Census/censusasr_g_clsa_s2.csv") #clsa
g_sqm2<-read.csv("./1_data/private/2016 Canadian Census/censussqm_g_clsa_s2.csv") #clsa
g_sqs2<-read.csv("./1_data/private/2016 Canadian Census/censussqs_g_clsa_s2.csv") #clsa

#Load functions
source("2_scripts/00_Helper_Functions.R")

# 2. Bootstrap age-sex-urban representation ratios -----------------------
# Setting A (Ab-C)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-abc_df[abc_df$province != "YT",]
output<-matrix(NA,nrow = n_replicates,
               ncol = 36)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(sex != "Self described" & !is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across urban, and age-urban categories
  alt1_allu<-df %>% 
    filter(sex != "Self described") %>%
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    mutate(urban = "All regions") %>% 
    ungroup()
  
  alt1_allsu<-df %>% 
    filter(sex != "Self described") %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allu,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:20,]$count / sum(subcount[1:20,]$count),
                           subcount[21:30,]$count / sum(subcount[21:30,]$count),
                           subcount[31:34,]$count / sum(subcount[31:34,]$count),
                           subcount[35:36,]$count / sum(subcount[35:36,]$count))
  subcount$pct_pop<-c(subcount[1:20,]$count_census / sum(subcount[1:20,]$count_census),
                      subcount[21:30,]$count_census / sum(subcount[21:30,]$count_census),
                      subcount[31:34,]$count_census / sum(subcount[31:34,]$count_census),
                      subcount[35:36,]$count_census / sum(subcount[35:36,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
abc_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(abc_fin)[4]<-"rr_prob"
abc_fin$cohort<-"Ab-C open cohort"

#calculate summary statistics (quantiles,mean)
abcsum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df) 
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  abcsum_output[i,]<-final
  
}

abcsum_output<-data.frame(abcsum_output)
colnames(abcsum_output)<-c("mean","2.5","25","50","75","95")
abcsum_output<-cbind(abcsum_output,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_abc_asu_5000_rr.csv")
#write_csv(abc_fin,"./1_data/private/boot_abc_asu_5000.csv")
#write_csv(abcsum_output,"./1_data/private/boot_abc_asu_5000_sumstats.csv")

# Setting A sensitivity analysis 3 (Ab-C)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-abc_df_qab[abc_df_qab$province != "YT",]
output<-matrix(NA,nrow = n_replicates,
               ncol = 36)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(sex != "Self described" & !is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across urban, and age-urban categories
  alt1_allu<-df %>% 
    filter(sex != "Self described") %>%
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    mutate(urban = "All regions") %>% 
    ungroup()
  
  alt1_allsu<-df %>% 
    filter(sex != "Self described") %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allu,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:20,]$count / sum(subcount[1:20,]$count),
                           subcount[21:30,]$count / sum(subcount[21:30,]$count),
                           subcount[31:34,]$count / sum(subcount[31:34,]$count),
                           subcount[35:36,]$count / sum(subcount[35:36,]$count))
  subcount$pct_pop<-c(subcount[1:20,]$count_census / sum(subcount[1:20,]$count_census),
                      subcount[21:30,]$count_census / sum(subcount[21:30,]$count_census),
                      subcount[31:34,]$count_census / sum(subcount[31:34,]$count_census),
                      subcount[35:36,]$count_census / sum(subcount[35:36,]$count_census))
  
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
abc_fin_qab<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(abc_fin_qab)[4]<-"rr_prob"
abc_fin_qab$cohort<-"Ab-C open cohort"

#calculate summary statistics (quantiles,mean)
abcsum_output_qab<-matrix(NA,nrow = ncol(output),
                      ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df) 
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  abcsum_output_qab[i,]<-final
  
}

abcsum_output_qab<-data.frame(abcsum_output_qab)
colnames(abcsum_output_qab)<-c("mean","2.5","25","50","75","95")
abcsum_output_qab<-cbind(abcsum_output_qab,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_abc_asu_5000_rr_qab.csv")
#write_csv(abc_fin_qab,"./1_data/private/boot_abc_asu_5000_qab.csv")
#write_csv(abcsum_output_qab,"./1_data/private/boot_abc_asu_5000_sumstats_qab.csv")

#Setting C (CBS)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-cbs_df
output<-matrix(NA,nrow = n_replicates,
               ncol = 36)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across urban, and age-urban categories
  alt1_allu<-df %>% 
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    mutate(urban = "All regions") %>% 
    ungroup()
  
  alt1_allsu<-df %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allu,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                              subcount$age_groups != "All ages"),
  which(subcount$urban == "All regions" & 
          subcount$age_groups != "All ages"),
  which(subcount$age_groups == "All ages" & 
          subcount$urban != "All regions"),
  which(subcount$urban == "All regions" & 
          subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:20,]$count / sum(subcount[1:20,]$count),
                           subcount[21:30,]$count / sum(subcount[21:30,]$count),
                           subcount[31:34,]$count / sum(subcount[31:34,]$count),
                           subcount[35:36,]$count / sum(subcount[35:36,]$count))
  subcount$pct_pop<-c(subcount[1:20,]$count_census / sum(subcount[1:20,]$count_census),
                      subcount[21:30,]$count_census / sum(subcount[21:30,]$count_census),
                      subcount[31:34,]$count_census / sum(subcount[31:34,]$count_census),
                      subcount[35:36,]$count_census / sum(subcount[35:36,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
cbs_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(cbs_fin)[4]<-"rr_prob"
cbs_fin$cohort<-"CBS blood donor"

#calculate summary statistics (quantiles,mean)
cbssum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cbssum_output[i,]<-final
  
}

cbssum_output<-data.frame(cbssum_output)
colnames(cbssum_output)<-c("mean","2.5","25","50","75","95")
cbssum_output<-cbind(cbssum_output,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_cbs_asu_5000_rr.csv")
#write_csv(cbs_fin,"./1_data/private/boot_cbs_asu_5000.csv")
#write_csv(cbssum_output,"./1_data/private/boot_cbs_asu_5000_sumstats.csv")

#Setting D (CanPath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-can_df
output<-matrix(NA,nrow = n_replicates,
               ncol = 36)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across urban, and age-urban categories
  alt1_allu<-df %>% 
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    mutate(urban = "All regions") %>% 
    ungroup()
  
  alt1_allsu<-df %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allu,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:20,]$count / sum(subcount[1:20,]$count),
                           subcount[21:30,]$count / sum(subcount[21:30,]$count),
                           subcount[31:34,]$count / sum(subcount[31:34,]$count),
                           subcount[35:36,]$count / sum(subcount[35:36,]$count))
  subcount$pct_pop<-c(subcount[1:20,]$count_census / sum(subcount[1:20,]$count_census),
                      subcount[21:30,]$count_census / sum(subcount[21:30,]$count_census),
                      subcount[31:34,]$count_census / sum(subcount[31:34,]$count_census),
                      subcount[35:36,]$count_census / sum(subcount[35:36,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
can_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(can_fin)[4]<-"rr_prob"
can_fin$cohort<-"CanPath closed cohort"

#calculate summary statistics (quantiles,mean)
cansum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cansum_output[i,]<-final
}

cansum_output<-data.frame(cansum_output)
colnames(cansum_output)<-c("mean","2.5","25","50","75","95")
cansum_output<-cbind(cansum_output,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_can_asu_5000_rr.csv")
#write_csv(can_fin,"./1_data/private/boot_can_asu_5000.csv")
#write_csv(cansum_output,"./1_data/private/boot_can_asu_5000_sumstats.csv")

#Setting D sensitivity analysis 4 (CanPath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-can_dfs4
output<-matrix(NA,nrow = n_replicates,
               ncol = 36)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across urban, and age-urban categories
  alt1_allu<-df %>% 
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    mutate(urban = "All regions") %>% 
    ungroup()
  
  alt1_allsu<-df %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allu,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:20,]$count / sum(subcount[1:20,]$count),
                           subcount[21:30,]$count / sum(subcount[21:30,]$count),
                           subcount[31:34,]$count / sum(subcount[31:34,]$count),
                           subcount[35:36,]$count / sum(subcount[35:36,]$count))
  subcount$pct_pop<-c(subcount[1:20,]$count_census / sum(subcount[1:20,]$count_census),
                      subcount[21:30,]$count_census / sum(subcount[21:30,]$count_census),
                      subcount[31:34,]$count_census / sum(subcount[31:34,]$count_census),
                      subcount[35:36,]$count_census / sum(subcount[35:36,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
cans4_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(cans4_fin)[4]<-"rr_prob"
cans4_fin$cohort<-"CanPath closed cohort"

#calculate summary statistics (quantiles,mean)
cansums4_output<-matrix(NA,nrow = ncol(output),
                      ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cansums4_output[i,]<-final
}

cansums4_output<-data.frame(cansums4_output)
colnames(cansums4_output)<-c("mean","2.5","25","50","75","95")
cansums4_output<-cbind(cansums4_output,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_can_asu_5000_rr_s4.csv")
#write_csv(cans4_fin,"./1_data/private/boot_can_asu_5000_s4.csv")
#write_csv(cansums4_output,"./1_data/private/boot_can_asu_5000_sumstats_s4.csv")

#Setting E (APL)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-apl_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 42)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(age_groups) & !is.na(sex)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across urban, and age-urban categories
  alt1_allu<-df %>% 
    filter(!is.na(sex)) %>%
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    mutate(urban = "All regions") %>% 
    ungroup()
  
  alt1_allsu<-df %>% 
    filter(!is.na(sex)) %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allu,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,e_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:24,]$count / sum(subcount[1:24,]$count),
                           subcount[25:36,]$count / sum(subcount[25:36,]$count),
                           subcount[37:40,]$count / sum(subcount[37:40,]$count),
                           subcount[41:42,]$count / sum(subcount[41:42,]$count))
  subcount$pct_pop<-c(subcount[1:24,]$count_census / sum(subcount[1:24,]$count_census),
                      subcount[25:36,]$count_census / sum(subcount[25:36,]$count_census),
                      subcount[37:40,]$count_census / sum(subcount[37:40,]$count_census),
                      subcount[41:42,]$count_census / sum(subcount[41:42,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
apl_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(apl_fin)[4]<-"rr_prob"
apl_fin$cohort<- "APL outpatient laboratory"

#calculate summary statistics (quantiles,mean)
aplsum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  aplsum_output[i,]<-final
  
}

aplsum_output<-data.frame(aplsum_output)
colnames(aplsum_output)<-c("mean","2.5","25","50","75","95")
aplsum_output<-cbind(aplsum_output,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_apl_asu_5000_rr.csv")
#write_csv(apl_fin,"./1_data/private/boot_apl_asu_5000.csv")
#write_csv(aplsum_output,"./1_data/private/boot_apl_asu_5000_sumstats.csv")

# Setting G (CLSA)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df
output<-matrix(NA,nrow = n_replicates,
               ncol = 18)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across urban, and age-urban categories
  alt1_allu<-df %>% 
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    mutate(urban = "All regions") %>% 
    ungroup()
  
  alt1_allsu<-df %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allu,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:8,]$count / sum(subcount[1:8,]$count),
                           subcount[9:12,]$count / sum(subcount[9:12,]$count),
                           subcount[13:16,]$count / sum(subcount[13:16,]$count),
                           subcount[17:18,]$count / sum(subcount[17:18,]$count))
  subcount$pct_pop<-c(subcount[1:8,]$count_census / sum(subcount[1:8,]$count_census),
                      subcount[9:12,]$count_census / sum(subcount[9:12,]$count_census),
                      subcount[13:16,]$count_census / sum(subcount[13:16,]$count_census),
                      subcount[17:18,]$count_census / sum(subcount[17:18,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsa_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(clsa_fin)[4]<-"rr_prob"
clsa_fin$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsasum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsasum_output[i,]<-final
  
}

clsasum_output<-data.frame(clsasum_output)
colnames(clsasum_output)<-c("mean","2.5","25","50","75","95")
clsasum_output<-cbind(clsasum_output,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_clsa_asu_5000_rr.csv")
#write_csv(clsa_fin,"./1_data/private/boot_clsa_asu_5000.csv")
#write_csv(clsasum_output,"./1_data/private/boot_clsa_asu_5000_sumstats.csv")

#Setting G sensitivity analysis 3 (CLSA)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df_qab
output<-matrix(NA,nrow = n_replicates,
               ncol = 18)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across urban, and age-urban categories
  alt1_allu<-df %>% 
    group_by(age_groups,sex) %>% 
    summarize(count = n()) %>% 
    mutate(urban = "All regions") %>% 
    ungroup()
  
  alt1_allsu<-df %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allu,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:8,]$count / sum(subcount[1:8,]$count),
                           subcount[9:12,]$count / sum(subcount[9:12,]$count),
                           subcount[13:16,]$count / sum(subcount[13:16,]$count),
                           subcount[17:18,]$count / sum(subcount[17:18,]$count))
  subcount$pct_pop<-c(subcount[1:8,]$count_census / sum(subcount[1:8,]$count_census),
                      subcount[9:12,]$count_census / sum(subcount[9:12,]$count_census),
                      subcount[13:16,]$count_census / sum(subcount[13:16,]$count_census),
                      subcount[17:18,]$count_census / sum(subcount[17:18,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsa_fin_qab<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(clsa_fin_qab)[4]<-"rr_prob"
clsa_fin_qab$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsasum_output_qab<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsasum_output_qab[i,]<-final
  
}

clsasum_output_qab<-data.frame(clsasum_output_qab)
colnames(clsasum_output_qab)<-c("mean","2.5","25","50","75","95")
clsasum_output_qab<-cbind(clsasum_output_qab,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_clsa_asu_5000_rr_qab.csv")
#write_csv(clsa_fin_qab,"./1_data/private/boot_clsa_asu_5000_qab.csv")
#write_csv(clsasum_output_qab,"./1_data/private/boot_clsa_asu_5000_sumstats_qab.csv")

# 3. Bootstrap age-sex-race representation ratios ------------------------

#Setting A (Ab-C)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-abc_df[abc_df$province != "YT",]

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(sex != "Self described" & !is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:20]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:20]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
abcr_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(abcr_fin)[4]<-"rr_prob"
abcr_fin$cohort<-"Ab-C open cohort"

#calculate summary statistics (quantiles,mean)
abcrsum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  abcrsum_output[i,]<-final
  
}

abcrsum_output<-data.frame(abcrsum_output)
colnames(abcrsum_output)<-c("mean","2.5","25","50","75","95")
abcrsum_output<-cbind(abcrsum_output,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_abc_asr_5000_rr.csv")
#write_csv(abcr_fin,"./1_data/private/boot_abc_asr_5000.csv")
#write_csv(abcrsum_output,"./1_data/boot_abc_asr_5000_sumstats.csv")

#Setting A sensitivity analysis 3 (Ab-C)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-abc_df_qab[abc_df_qab$province != "YT",]

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(sex != "Self described"  & !is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:20]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:20]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
abcr_fin_qab<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(abcr_fin_qab)[4]<-"rr_prob"
abcr_fin_qab$cohort<-"Ab-C open cohort"

#calculate summary statistics (quantiles,mean)
abcrsum_output_qab<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  abcrsum_output_qab[i,]<-final
  
}

abcrsum_output_qab<-data.frame(abcrsum_output_qab)
colnames(abcrsum_output_qab)<-c("mean","2.5","25","50","75","95")
abcrsum_output_qab<-cbind(abcrsum_output_qab,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_abc_asr_5000_rr_qab.csv")
#write_csv(abcr_fin_qab,"./1_data/private/boot_abc_asr_5000_qab.csv")
#write_csv(abcrsum_output_qab,"./1_data/boot_abc_asr_5000_sumstats_qab.csv")

#Setting B (CCAHS)

#Setting C (CBS)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-cbs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:20]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:20]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
cbsr_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(cbsr_fin)[4]<-"rr_prob"
cbsr_fin$cohort<-"CBS blood donor"

#calculate summary statistics (quantiles,mean)
cbsrsum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cbsrsum_output[i,]<-final
  
}

cbsrsum_output<-data.frame(cbsrsum_output)
colnames(cbsrsum_output)<-c("mean","2.5","25","50","75","95")
cbsrsum_output<-cbind(cbsrsum_output,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_cbs_asr_5000_rr.csv")
#write_csv(cbsr_fin,"./1_data/private/boot_cbs_asr_5000.csv")
#write_csv(cbsrsum_output,"./1_data/private/boot_cbs_asr_5000_sumstats.csv")

#Setting D (CanPath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-can_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(race != "pnts" & !is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:20]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:20]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
canr_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(canr_fin)[4]<-"rr_prob"
canr_fin$cohort<-"CanPath closed cohort"

#calculate summary statistics (quantiles,mean)
canrsum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  canrsum_output[i,]<-final
  
}

canrsum_output<-data.frame(canrsum_output)
colnames(canrsum_output)<-c("mean","2.5","25","50","75","95")
canrsum_output<-cbind(canrsum_output,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_can_asr_5000_rr.csv")
#write_csv(canr_fin,"./1_data/private/boot_can_asr_5000.csv")
#write_csv(canrsum_output,"./1_data/private/boot_can_asr_5000_sumstats.csv")

#Setting D sensitivity analysis 4 (CanPath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-can_dfs4

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(race != "pnts" & !is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:20]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:20]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
canrs4_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(canrs4_fin)[4]<-"rr_prob"
canrs4_fin$cohort<-"CanPath closed cohort"

#calculate summary statistics (quantiles,mean)
canrsums4_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  canrsums4_output[i,]<-final
  
}

canrsums4_output<-data.frame(canrsums4_output)
colnames(canrsums4_output)<-c("mean","2.5","25","50","75","95")
canrsums4_output<-cbind(canrsums4_output,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_can_asr_5000_rr_s4.csv")
#write_csv(canrs4_fin,"./1_data/private/boot_can_asr_5000_s4.csv")
#write_csv(canrsums4_output,"./1_data/private/boot_can_asr_5000_sumstats_s4.csv")

#Setting G (CLSA)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(race) & race != "pnts") %>%
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:8]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:8]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsar_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(clsar_fin)[4]<-"rr_prob"
clsar_fin$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsarsum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsarsum_output[i,]<-final
  
}

clsarsum_output<-data.frame(clsarsum_output)
colnames(clsarsum_output)<-c("mean","2.5","25","50","75","95")
clsarsum_output<-cbind(clsarsum_output,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_clsa_asr_5000_rr.csv")
#write_csv(clsar_fin,"./1_data/private/boot_clsa_asr_5000.csv")
#write_csv(clsarsum_output,"./1_data/private/boot_clsa_asr_5000_sumstats.csv")

#Setting G sensitivity analysis 3 (CLSA)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df_qab

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(race) & race != "pnts") %>%
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:8]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:8]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsar_fin_qab<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(clsar_fin_qab)[4]<-"rr_prob"
clsar_fin_qab$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsarsum_output_qab<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsarsum_output_qab[i,]<-final
  
}

clsarsum_output_qab<-data.frame(clsarsum_output_qab)
colnames(clsarsum_output_qab)<-c("mean","2.5","25","50","75","95")
clsarsum_output_qab<-cbind(clsarsum_output_qab,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_clsa_asr_5000_rr_qab.csv")
#write_csv(clsar_fin_qab,"./1_data/private/boot_clsa_asr_5000_qab.csv")
#write_csv(clsarsum_output_qab,"./1_data/private/boot_clsa_asr_5000_sumstats_qab.csv")

# 4. Bootstrap sex-quintmat representation ratios ----------------------------

#Setting C (CBS)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-cbs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CBS
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(quintmat)) %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_sqm,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintmat,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintmat"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
cbsqm_fin<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(cbsqm_fin)[3]<-"rr_prob"
cbsqm_fin$cohort<-"CBS blood donor"

#calculate summary statistics (quantiles,mean)
cbsqmsum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cbsqmsum_output[i,]<-final
  
}

cbsqmsum_output<-data.frame(cbsqmsum_output)
colnames(cbsqmsum_output)<-c("mean","2.5","25","50","75","95")
cbsqmsum_output<-cbind(cbsqmsum_output,subcount[,c("sex","quintmat")])

#write_csv(output,"./1_data/private/boot_cbs_sqm_5000_rr.csv")
#write_csv(cbsqm_fin,"./1_data/private/boot_cbs_sqm_5000.csv")
#write_csv(cbsqmsum_output,"./1_data/private/boot_cbs_sqm_5000_sumstats.csv")

#Setting E (APL)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-apl_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for APL
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(sex) & !is.na(quintmat)) %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,e_sqm,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintmat,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintmat"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
aplqm_fin<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(aplqm_fin)[3]<-"rr_prob"
aplqm_fin$cohort<-"APL outpatient laboratory"

#calculate summary statistics (quantiles,mean)
aplqmsum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  aplqmsum_output[i,]<-final
  
}

aplqmsum_output<-data.frame(aplqmsum_output)
colnames(aplqmsum_output)<-c("mean","2.5","25","50","75","95")
aplqmsum_output<-cbind(aplqmsum_output,subcount[,c("sex","quintmat")])

#write_csv(output,"./1_data/private/boot_apl_sqm_5000_rr.csv")
#write_csv(aplqm_fin,"./1_data/private/boot_apl_sqm_5000.csv")
#write_csv(aplqmsum_output,"./1_data/private/boot_apl_sqm_5000_sumstats.csv")

#Setting G (CLSA)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CLSA
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(quintmat)) %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_sqm,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintmat,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintmat"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsaqm_fin<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(clsaqm_fin)[3]<-"rr_prob"
clsaqm_fin$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsaqmsum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsaqmsum_output[i,]<-final
  
}

clsaqmsum_output<-data.frame(clsaqmsum_output)
colnames(clsaqmsum_output)<-c("mean","2.5","25","50","75","95")
clsaqmsum_output<-cbind(clsaqmsum_output,subcount[,c("sex","quintmat")])

#write_csv(output,"./1_data/private/boot_clsa_sqm_5000_rr.csv")
#write_csv(clsaqm_fin,"./1_data/private/boot_clsa_sqm_5000.csv")
#write_csv(clsaqmsum_output,"./1_data/private/boot_clsa_sqm_5000_sumstats.csv")

#Setting G sensitivity analysis 3 (CLSA)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df_qab

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CLSA
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(quintmat)) %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_sqm,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintmat,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintmat"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsaqm_fin_qab<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(clsaqm_fin_qab)[3]<-"rr_prob"
clsaqm_fin_qab$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsaqmsum_output_qab<-matrix(NA,nrow = ncol(output),
                         ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsaqmsum_output_qab[i,]<-final
  
}

clsaqmsum_output_qab<-data.frame(clsaqmsum_output_qab)
colnames(clsaqmsum_output_qab)<-c("mean","2.5","25","50","75","95")
clsaqmsum_output_qab<-cbind(clsaqmsum_output_qab,subcount[,c("sex","quintmat")])

#write_csv(output,"./1_data/private/boot_clsa_sqm_5000_rr_qab.csv")
#write_csv(clsaqm_fin_qab,"./1_data/private/boot_clsa_sqm_5000_qab.csv")
#write_csv(clsaqmsum_output_qab,"./1_data/private/boot_clsa_sqm_5000_sumstats_qab.csv")

# 5. Bootstrap sex-quintsoc representation ratios ----------------------------
#Setting C (CBS)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-cbs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CBS
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(quintsoc)) %>% 
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_sqs,by = c("sex","quintsoc"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintsoc,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintsoc"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
cbsqs_fin<-cbind(subcount[,c("sex","quintsoc")],sapply(output,rrfun))
colnames(cbsqs_fin)[3]<-"rr_prob"
cbsqs_fin$cohort<-"CBS blood donor"

#calculate summary statistics (quantiles,mean)
cbsqssum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cbsqssum_output[i,]<-final
  
}

cbsqssum_output<-data.frame(cbsqssum_output)
colnames(cbsqssum_output)<-c("mean","2.5","25","50","75","95")
cbsqssum_output<-cbind(cbsqssum_output,subcount[,c("sex","quintsoc")])

#write_csv(output,"./1_data/private/boot_cbs_sqs_5000_rr.csv")
#write_csv(cbsqs_fin,"./1_data/private/boot_cbs_sqs_5000.csv")
#write_csv(cbsqssum_output,"./1_data/private/boot_cbs_sqs_5000_sumstats.csv")

#Setting E (APL)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-apl_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for APL
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(sex) & !is.na(quintsoc)) %>% 
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,e_sqs,by = c("sex","quintsoc"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintsoc,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintsoc"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
aplqs_fin<-cbind(subcount[,c("sex","quintsoc")],sapply(output,rrfun))
colnames(aplqs_fin)[3]<-"rr_prob"
aplqs_fin$cohort<-"APL outpatient laboratory"

#calculate summary statistics (quantiles,mean)
aplqssum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  aplqssum_output[i,]<-final
  
}

aplqssum_output<-data.frame(aplqssum_output)
colnames(aplqssum_output)<-c("mean","2.5","25","50","75","95")
aplqssum_output<-cbind(aplqssum_output,subcount[,c("sex","quintsoc")])

#write_csv(output,"./1_data/private/boot_apl_sqs_5000_rr.csv")
#write_csv(aplqs_fin,"./1_data/private/boot_apl_sqs_5000.csv")
#write_csv(aplqssum_output,"./1_data/private/boot_apl_sqs_5000_sumstats.csv")

#Setting G (CLSA)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CLSA
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(quintsoc)) %>% 
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_sqs,by = c("sex","quintsoc"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintsoc,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintsoc"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsaqs_fin<-cbind(subcount[,c("sex","quintsoc")],sapply(output,rrfun))
colnames(clsaqs_fin)[3]<-"rr_prob"
clsaqs_fin$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsaqssum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsaqssum_output[i,]<-final
  
}

clsaqssum_output<-data.frame(clsaqssum_output)
colnames(clsaqssum_output)<-c("mean","2.5","25","50","75","95")
clsaqssum_output<-cbind(clsaqssum_output,subcount[,c("sex","quintsoc")])

#write_csv(output,"./1_data/private/boot_clsa_sqs_5000_rr.csv")
#write_csv(clsaqs_fin,"./1_data/private/boot_clsa_sqs_5000.csv")
#write_csv(clsaqssum_output,"./1_data/private/boot_clsa_sqs_5000_sumstats.csv")

#Setting G sensitivity analysis (CLSA)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df_qab

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CLSA
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(quintsoc)) %>% 
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_sqs,by = c("sex","quintsoc"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintsoc,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintsoc"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsaqs_fin_qab<-cbind(subcount[,c("sex","quintsoc")],sapply(output,rrfun))
colnames(clsaqs_fin_qab)[3]<-"rr_prob"
clsaqs_fin_qab$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsaqssum_output_qab<-matrix(NA,nrow = ncol(output),
                         ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsaqssum_output_qab[i,]<-final
  
}

clsaqssum_output_qab<-data.frame(clsaqssum_output_qab)
colnames(clsaqssum_output_qab)<-c("mean","2.5","25","50","75","95")
clsaqssum_output_qab<-cbind(clsaqssum_output_qab,subcount[,c("sex","quintsoc")])

#write_csv(output,"./1_data/private/boot_clsa_sqs_5000_rr_qab.csv")
#write_csv(clsaqs_fin_qab,"./1_data/private/boot_clsa_sqs_5000_qab.csv")
#write_csv(clsaqssum_output_qab,"./1_data/private/boot_clsa_sqs_5000_sumstats_qab.csv")

# 6. Bootstrap representation ratios for sensitivity analysis 1 ------------------
#Bootstrap age-sex-race representation ratios
#Setting A (Ab-C)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-abc_df[abc_df$province != "YT",]

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(sex != "Self described" & !is.na(race1)) %>% 
    group_by(age_groups,sex,race1) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race1, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asr1,by = c("age_groups","sex","race1"),all.y = TRUE)
  
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race1),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:20]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:20]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race1"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
abcr1_fin<-cbind(subcount[,c("age_groups","sex","race1")],sapply(output,rrfun))
colnames(abcr1_fin)[4]<-"rr_prob"
abcr1_fin$cohort<-"Ab-C open cohort"

#calculate summary statistics (quantiles,mean)
abcr1sum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  abcr1sum_output[i,]<-final
  
}

abcr1sum_output<-data.frame(abcr1sum_output)
colnames(abcr1sum_output)<-c("mean","2.5","25","50","75","95")
abcr1sum_output<-cbind(abcr1sum_output,subcount[,c("age_groups","sex","race1")])

#write_csv(output, "./1_data/private_boot_abc_asr1_5000_rr.csv")
#write_csv(abcr1_fin,"./1_data/private/boot_abc_asr1_5000.csv")
#write_csv(abcr1sum_output,"boot_abc_asr1_5000_sumstats.csv")

#Setting D (CanPath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-can_df1

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(race1 != "pnts" & !is.na(race1)) %>% 
    group_by(age_groups,sex,race1) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race1, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asr1,by = c("age_groups","sex","race1"),all.y = TRUE)
  
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race1),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:20]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:20]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race1"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
canr1_fin<-cbind(subcount[,c("age_groups","sex","race1")],sapply(output,rrfun))
colnames(canr1_fin)[4]<-"rr_prob"
canr1_fin$cohort<-"CanPath closed cohort"

#calculate summary statistics (quantiles,mean)
canr1sum_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  canr1sum_output[i,]<-final
  
}

canr1sum_output<-data.frame(canr1sum_output)
colnames(canr1sum_output)<-c("mean","2.5","25","50","75","95")
canr1sum_output<-cbind(canr1sum_output,subcount[,c("age_groups","sex","race1")])

#write_csv(output,"./1_data/private/boot_can_asr1_5000_rr.csv")
#write_csv(canr1_fin,"./1_data/private/boot_can_asr1_5000.csv")
#write_csv(canr1sum_output,"boot_can_asr1_5000_sumstats.csv")

#Setting G (CLSA)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(race1) & race1 != "pnts") %>%
    group_by(age_groups,sex,race1) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race1, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_asr1,by = c("age_groups","sex","race1"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race1),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:8]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:8]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race1"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsar1_fin<-cbind(subcount[,c("age_groups","sex","race1")],sapply(output,rrfun))
colnames(clsar1_fin)[4]<-"rr_prob"
clsar1_fin$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsar1sum_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsar1sum_output[i,]<-final
  
}

clsar1sum_output<-data.frame(clsar1sum_output)
colnames(clsar1sum_output)<-c("mean","2.5","25","50","75","95")
clsar1sum_output<-cbind(clsar1sum_output,subcount[,c("age_groups","sex","race1")])

#write_csv(output, "./1_data/private/boot_clsa_asr1_5000_rr.csv")
#write_csv(clsar1_fin,"./1_data/private/boot_clsa_asr1_5000.csv")
#write_csv(clsar1sum_output,"boot_clsa_asr1_5000_sumstats.csv")

# 7. Bootstrap representation ratios for sensitivity analysis 2 --------
#Calculate CLSA and CanPath rep ratios using census datasets that include 
# indigenous individuals in census counts
#Age-sex-urban
#Setting D (CanPath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-can_df
output<-matrix(NA,nrow = n_replicates,
               ncol = 36)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across all age and urban categories
  alt1_allsu<-df %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asus2,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:20,]$count / sum(subcount[1:20,]$count),
                           subcount[21:30,]$count / sum(subcount[21:30,]$count),
                           subcount[31:34,]$count / sum(subcount[31:34,]$count),
                           subcount[35:36,]$count / sum(subcount[35:36,]$count))
  subcount$pct_pop<-c(subcount[1:20,]$count_census / sum(subcount[1:20,]$count_census),
                      subcount[21:30,]$count_census / sum(subcount[21:30,]$count_census),
                      subcount[31:34,]$count_census / sum(subcount[31:34,]$count_census),
                      subcount[35:36,]$count_census / sum(subcount[35:36,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
can_fins2<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(can_fins2)[4]<-"rr_prob"
can_fins2$cohort<-"CanPath closed cohort"

#calculate summary statistics (quantiles,mean)
cansums2_output<-matrix(NA,nrow = ncol(output),
                      ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  cansums2_output[i,]<-final
}

cansums2_output<-data.frame(cansums2_output)
colnames(cansums2_output)<-c("mean","2.5","25","50","75","95")
cansums2_output<-cbind(cansums2_output,subcount[,c("age_groups","sex","urban")])

#write_csv(output, "./1_data/private/boot_can_asu_5000_rr_s2")
#write_csv(can_fins2,"./1_data/private/boot_can_asu_5000_s2.csv")
#write_csv(cansums2_output,"boot_can_asu_5000_sumstats_s2.csv")

# Setting G (CLSA)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df
output<-matrix(NA,nrow = n_replicates,
               ncol = 18)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  
  #across all age and urban categories
  alt1_allsu<-df %>% 
    group_by(sex) %>% 
    summarize(count = n()) %>% 
    mutate(age_groups = "All ages",
           urban = "All regions") %>% 
    ungroup()
  
  group<-do.call("rbind",list(group,alt,alt1_allsu))
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_asus2,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[c(which(subcount$urban != "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups != "All ages"),
                       which(subcount$age_groups == "All ages" & 
                               subcount$urban != "All regions"),
                       which(subcount$urban == "All regions" & 
                               subcount$age_groups == "All ages")),]
  subcount$pct_resample<-c(subcount[1:8,]$count / sum(subcount[1:8,]$count),
                           subcount[9:12,]$count / sum(subcount[9:12,]$count),
                           subcount[13:16,]$count / sum(subcount[13:16,]$count),
                           subcount[17:18,]$count / sum(subcount[17:18,]$count))
  subcount$pct_pop<-c(subcount[1:8,]$count_census / sum(subcount[1:8,]$count_census),
                      subcount[9:12,]$count_census / sum(subcount[9:12,]$count_census),
                      subcount[13:16,]$count_census / sum(subcount[13:16,]$count_census),
                      subcount[17:18,]$count_census / sum(subcount[17:18,]$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"urban"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsa_fins2<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(clsa_fins2)[4]<-"rr_prob"
clsa_fins2$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsasums2_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsasums2_output[i,]<-final
  
}

clsasums2_output<-data.frame(clsasums2_output)
colnames(clsasums2_output)<-c("mean","2.5","25","50","75","95")
clsasums2_output<-cbind(clsasums2_output,subcount[,c("age_groups","sex","urban")])

#write_csv(output,"./1_data/private/boot_clsa_asu_5000_rr_s2.csv")
#write_csv(clsa_fins2,"./1_data/private/boot_clsa_asu_5000_s2.csv")
#write_csv(clsasums2_output,"boot_clsa_asu_5000_sumstats_s2.csv")

#Age-sex-race
#Setting D (CanPath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

data<-can_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(race != "pnts" & !is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asrs2,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:20]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:20]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
canrs2_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(canrs2_fin)[4]<-"rr_prob"
canrs2_fin$cohort<-"CanPath closed cohort"

#calculate summary statistics (quantiles,mean)
canrsums2_output<-matrix(NA,nrow = ncol(output),
                       ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  canrsums2_output[i,]<-final
  
}

canrsums2_output<-data.frame(canrsums2_output)
colnames(canrsums2_output)<-c("mean","2.5","25","50","75","95")
canrsums2_output<-cbind(canrsums2_output,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_can_asr_5000_rr_s2.csv")
#write_csv(canrs2_fin,"./1_data/private/boot_can_asr_5000_s2.csv")
#write_csv(canrsums2_output,"boot_can_asr_5000_sumstats_s2.csv")

#Setting G (CLSA)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(race) & race != "pnts") %>%
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_asrs2,by = c("age_groups","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count[1:8]))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census[1:8]))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"age_groups"],subcount[i,"sex"],subcount[i,"race"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsars2_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(clsars2_fin)[4]<-"rr_prob"
clsars2_fin$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsarsums2_output<-matrix(NA,nrow = ncol(output),
                        ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsarsums2_output[i,]<-final
  
}

clsarsums2_output<-data.frame(clsarsums2_output)
colnames(clsarsums2_output)<-c("mean","2.5","25","50","75","95")
clsarsums2_output<-cbind(clsarsums2_output,subcount[,c("age_groups","sex","race")])

#write_csv(output,"./1_data/private/boot_clsa_asr_5000_rr_s2.csv")
#write_csv(clsars2_fin,"./1_data/private/boot_clsa_asr_5000_s2.csv")
#write_csv(clsarsums2_output,"boot_clsa_asr_5000_sumstats_s2.csv")

#Sex-quintmat
#Setting G (CLSA)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CLSA
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(quintmat)) %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_sqm2,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintmat,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintmat"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsaqms2_fin<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(clsaqms2_fin)[3]<-"rr_prob"
clsaqms2_fin$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsaqms2sum_output<-matrix(NA,nrow = ncol(output),
                         ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsaqms2sum_output[i,]<-final
  
}

clsaqms2sum_output<-data.frame(clsaqms2sum_output)
colnames(clsaqms2sum_output)<-c("mean","2.5","25","50","75","95")
clsaqms2sum_output<-cbind(clsaqms2sum_output,subcount[,c("sex","quintmat")])

#write_csv(output,"./1_data/private/boot_clsa_sqm_5000_rr_s2.csv")
#write_csv(clsaqms2_fin,"./1_data/private/boot_clsa_sqm_5000_s2.csv")
#write_csv(clsaqms2sum_output,"boot_clsa_sqm_5000_sumstats_s2.csv")

#Sex-quintsoc
#Setting G (CLSA)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CLSA
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    filter(!is.na(quintsoc)) %>% 
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_sqs2,by = c("sex","quintsoc"),all.y = TRUE)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$quintsoc,subcount$sex),]
  subcount$pct_resample<-c(subcount$count / sum(subcount$count))
  subcount$pct_pop<-c(subcount$count_census / sum(subcount$count_census))
  subcount$rr<-subcount$pct_resample / subcount$pct_pop
  
  #save output to matrix
  output[i,]<-subcount$rr
  print(paste("Run",i,"complete"))
}

#label columns and make data.frame
output<-data.frame(output)

#Assign column names
string<-c()
for(i in 1:nrow(subcount)){
  s_i<-paste(subcount[i,"sex"],subcount[i,"quintsoc"],sep = "-")
  string<-c(string,s_i)
}
colnames(output)<-string

#perform calculation and re-assign bootstrap probability to each subgroup combination
clsaqss2_fin<-cbind(subcount[,c("sex","quintsoc")],sapply(output,rrfun))
colnames(clsaqss2_fin)[3]<-"rr_prob"
clsaqss2_fin$cohort<-"CLSA closed cohort"

#calculate summary statistics (quantiles,mean)
clsaqss2sum_output<-matrix(NA,nrow = ncol(output),
                         ncol = 6)
for(i in 1:ncol(output)){
  df<-output[,i]
  col_mean<-mean(df)
  col_quants<-quantile(df,probs = c(0.025,0.25,0.50,0.75,0.95),
                       na.rm = T)
  final<-c(col_mean,col_quants)
  clsaqss2sum_output[i,]<-final
  
}

clsaqss2sum_output<-data.frame(clsaqss2sum_output)
colnames(clsaqss2sum_output)<-c("mean","2.5","25","50","75","95")
clsaqss2sum_output<-cbind(clsaqss2sum_output,subcount[,c("sex","quintsoc")])

#write_csv(output,"./1_data/private/boot_clsa_sqs_5000_rr_s2.csv")
#write_csv(clsaqss2_fin,"./1_data/private/boot_clsa_sqs_5000_s2.csv")
#write_csv(clsaqss2sum_output,"boot_clsa_sqs_5000_sumstats_s2.csv")
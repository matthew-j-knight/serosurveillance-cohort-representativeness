"This script bootstraps representativeness ratios produced for each 
demographic subgroup and study. A subgroup is considered significantly 
underrepresented if 95% of the representativeness ratios fall below 
a threshold of 0.75."

# Load libraries and data -------------------------------------------------
setwd("./1_data/private/") #remove final pub
library(tidyverse)

#Read in serosurveillance study datasets
cbs_df<-read.csv("cbs_df_jan222024.csv")
apl_df<-read.csv("apl_df_jan222024.csv")
abc_df<-read.csv("abc_df_jan222024.csv")
clsa_df<-read.csv("clsa_df_jan222024.csv")
can_df<-read.csv("can_df_jan222024.csv")

#Read in 2021 census datasets (urban)
a_asu<-read.csv("2021 Canadian Census/censusasu_a_abc.csv") #ab-c
c_asu<-read.csv("2021 Canadian Census/censusasu_c_cbs.csv") #cbs
d_asu<-read.csv("2021 Canadian Census/censusasu_d_canpath.csv") #canpath
e_asu<-read.csv("2021 Canadian Census/censusasu_e_apl.csv") #apl
g_asu<-read.csv("2021 Canadian Census/censusasu_g_clsa.csv") #clsa

#Read in 2021 census datasets (race)
a_asr<-read.csv("2021 Canadian Census/censusasr_a_abc.csv") #ab-c
c_asr<-read.csv("2021 Canadian Census/censusasr_c_cbs.csv") #cbs
d_asr<-read.csv("2021 Canadian Census/censusasr_d_canpath.csv") #canpath
g_asr<-read.csv("2021 Canadian Census/censusasr_g_clsa.csv") #clsa

#Read in 2021 census datasets (quintmat) 
c_sqm<-read.csv("2021 Canadian Census/censussqm_c_cbs.csv") #CBS
e_sqm<-read.csv("2021 Canadian Census/censussqm_e_apl.csv") #APL

#Read in 2021 census datasets (quintsoc)
c_sqs<-read.csv("2021 Canadian Census/censussqs_c_cbs.csv") #CBS
e_sqs<-read.csv("2021 Canadian Census/censussqs_e_apl.csv") #APL

# Bootstrap age-sex-urban representativeness ratios -----------------------
# Setting A (Ab-c)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-abc_df
output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

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
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$urban),]
  pct_resample<-c(subcount$count[1:20] / sum(subcount$count[1:20],na.rm = T),
                  subcount$count[21:24] / sum(subcount$count[21:24],na.rm = T))
  pct_pop<-c(subcount$count_census[1:20] / sum(subcount$count_census[1:20],na.rm = T),
             subcount$count_census[21:24] / sum(subcount$count_census[21:24],na.rm = T))
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
abc_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(abc_fin)[4]<-"rr_prob"
abc_fin$cohort<-"Ab-c open cohort"

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

write_csv(abc_fin,"boot_abc_asu_5000.csv")
write_csv(abcsum_output,"boot_abc_asu_5000_sumstats.csv")

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
    filter(!is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$urban),]
  pct_resample<-c(subcount$count[1:20] / sum(subcount$count[1:20],na.rm = T),
                  subcount$count[21:24] / sum(subcount$count[21:24],na.rm = T))
  pct_pop<-c(subcount$count_census[1:20] / sum(subcount$count_census[1:20],na.rm = T),
             subcount$count_census[21:24] / sum(subcount$count_census[21:24],na.rm = T))
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

write_csv(cbs_fin,"boot_cbs_asu_5000.csv")
write_csv(cbssum_output,"boot_csb_asu_5000_sumstats.csv")

#Setting D (Canpath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
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
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$urban),]
  pct_resample<-c(subcount$count[1:20] / sum(subcount$count[1:20],na.rm = T),
                  subcount$count[21:24] / sum(subcount$count[21:24],na.rm = T))
  pct_pop<-c(subcount$count_census[1:20] / sum(subcount$count_census[1:20],na.rm = T),
             subcount$count_census[21:24] / sum(subcount$count_census[21:24],na.rm = T))
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
can_fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(can_fin)[4]<-"rr_prob"
can_fin$cohort<-"Canpath closed cohort"

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

write_csv(can_fin,"boot_can_asu_5000.csv")
write_csv(cansum_output,"boot_can_asu_5000_sumstats.csv")

#Setting E (APL)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-apl_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 28)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(sex != "Unknown" &
             !is.na(urban)) %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,e_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$urban),]
  pct_resample<-c(subcount$count[1:24] / sum(subcount$count[1:24],na.rm = T),
                  subcount$count[25:28] / sum(subcount$count[25:28],na.rm = T))
  pct_pop<-c(subcount$count_census[1:24] / sum(subcount$count_census[1:24],na.rm = T),
             subcount$count_census[25:28] / sum(subcount$count_census[25:28],na.rm = T))
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

write_csv(apl_fin,"boot_apl_asu_5000.csv")
write_csv(aplsum_output,"boot_apl_asu_5000age1_sumstats.csv")

#Setting F (CCAHS territories)

# Setting G (CLSA)
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
    filter(!is.na(urban)) %>%
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,g_asu,by = c("age_groups","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$urban),]
  pct_resample<-c(subcount$count[1:8] / sum(subcount$count[1:8],na.rm = T),
                  subcount$count[9:12] / sum(subcount$count[9:12],na.rm = T))
  pct_pop<-c(subcount$count_census[1:8] / sum(subcount$count_census[1:8],na.rm = T),
             subcount$count_census[9:12] / sum(subcount$count_census[9:12],na.rm = T))
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

write_csv(clsa_fin,"boot_clsa_asu_5000.csv")
write_csv(clsasum_output,"boot_clsa_asu_5000_sumstats.csv")

# Bootstrap age-sex-race representativeness ratios ------------------------

#Setting A (Ab-c)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-abc_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(sex != "Self described" & race != "pnts" & !is.na(race)) %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  pct_resample<-c(subcount$count[1:20] / sum(subcount$count[1:20],na.rm = T),
                  subcount$count[21:24] / sum(subcount$count[21:24],na.rm = T))
  pct_pop<-c(subcount$count_census[1:20] / sum(subcount$count_census[1:20],na.rm = T),
             subcount$count_census[21:24] / sum(subcount$count_census[21:24],na.rm = T))
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
abcr_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(abcr_fin)[4]<-"rr_prob"
abcr_fin$cohort<-"Ab-c open cohort"

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

write_csv(abcr_fin,"boot_abc_asr_5000.csv")
write_csv(abcrsum_output,"boot_abc_asr_5000_sumstats.csv")

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
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(race != "Missing") %>%
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  pct_resample<-c(subcount$count[1:20] / sum(subcount$count[1:20],na.rm = T),
                  subcount$count[21:24] / sum(subcount$count[21:24],na.rm = T))
  pct_pop<-c(subcount$count_census[1:20] / sum(subcount$count_census[1:20],na.rm = T),
             subcount$count_census[21:24] / sum(subcount$count_census[21:24],na.rm = T))
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

write_csv(cbsr_fin,"boot_cbs_asr_5000.csv")
write_csv(cbsrsum_output,"boot_cbs_asr_5000_sumstats.csv")

#Setting D (Canpath)
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-can_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
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
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-subcount[order(subcount$age_groups,subcount$sex,subcount$race),]
  pct_resample<-c(subcount$count[1:20] / sum(subcount$count[1:20],na.rm = T),
                  subcount$count[21:24] / sum(subcount$count[21:24],na.rm = T))
  pct_pop<-c(subcount$count_census[1:20] / sum(subcount$count_census[1:20],na.rm = T),
             subcount$count_census[21:24] / sum(subcount$count_census[21:24],na.rm = T))
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
canr_fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(canr_fin)[4]<-"rr_prob"
canr_fin$cohort<-"Canpath closed cohort"

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
write_csv(canr_fin,"boot_can_asr_5000.csv")
write_csv(canrsum_output,"boot_can_asr_5000age1_sumstats.csv")

#Setting F (CCAHS territories)

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
  
  #calculate counts bu subgroup in resample
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
  pct_resample<-c(subcount$count[1:8] / sum(subcount$count[1:8],na.rm = T),
                  subcount$count[9:12] / sum(subcount$count[9:12],na.rm = T))
  pct_pop<-c(subcount$count_census[1:8] / sum(subcount$count_census[1:8],na.rm = T),
             subcount$count_census[9:12] / sum(subcount$count_census[9:12],na.rm = T))
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
write_csv(clsar_fin,"boot_clsa_asr_5000.csv")
write_csv(clsarsum_output,"boot_clsa_asr_5000_sumstats.csv")

# Bootstrap sex-quintmat representation ratios ----------------------------

#Setting B (CCAHS)

#Setting C (CBS)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-cbs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run for CBS
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(quintmat)) %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$quintmat<-"All quintiles"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_sqm,by = c("sex","quintmat"),all.y = TRUE)
  
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
write_csv(cbsqm_fin,"boot_cbs_sqm_5000.csv")
write_csv(cbsqmsum_output,"boot_cbs_sqm_5000_sumstats.csv")

#Setting E (APL)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-apl_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run for APL
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(quintmat)) %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$quintmat<-"All quintiles"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount$count<-ifelse(is.na(subcount$count),0,subcount$count)
  subcount<-merge(group,e_sqm,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
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
  prop<-(sum(x < 0.75)) / length(x) #calculate proportion
  return(prop)
}

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
write_csv(aplqm_fin,"boot_apl_sqm_5000.csv")
write_csv(aplqmsum_output,"boot_apl_sqm_5000_sumstats")

#Setting F (CCAHS territories)

# Bootstrap sex-quintsoc representation ratios ----------------------------
#Setting B (CCAHS)

#Setting C (CBS)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-cbs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run for CBS
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(quintsoc)) %>% 
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$quintsoc<-"All quintiles"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_sqs,by = c("sex","quintsoc"),all.y = TRUE)
  
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
write_csv(cbsqs_fin,"boot_cbs_sqs_5000.csv")
write_csv(cbsqmsum_output,"boot_cbs_sq_5000_sumstats.csv")

#Setting E (APL)
#Initialize
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-apl_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run for APL
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    filter(!is.na(quintsoc)) %>% 
    group_by(sex,quintsoc) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex
  alt<-aggregate(group,count ~ sex, FUN = sum, drop = F)
  alt$quintsoc<-"All quintiles"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,e_sqs,by = c("sex","quintsoc"),all.y = TRUE)
  
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
  prop<-(sum(x < 0.75)) / length(x) #calculate proportion
  return(prop)
}

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

write_csv(aplqs_fin,"boot_apl_sqs_5000.csv")
write_csv(aplqssum_output,"boot_apl_sqs_5000_sumstats.csv")

#Setting F (CCAHS territories)


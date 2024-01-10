"This script bootstraps representativeness ratios produced for each 
demographic subgroup and cohort. A subgroup is considered significantly 
underrepresented if 95% of the representativeness ratios fall below 
a threshold of 0.75. This script is identical to 05_, except it uses
more precise age buckets"

# Load libraries and data -------------------------------------------------
setwd("~/serosurveillance-cohort-representativeness/1_data/private") #remove final pub
library(tidyverse)

#Read in serosurveillance cohort datasets
cbs_df<-read.csv("cbs_df.csv")
apl_df<-read.csv("apl_df.csv")
abc_df<-read.csv("abc_df.csv")
clsa_df<-read.csv("clsa_df.csv")
can_df<-read.csv("can_df.csv")
#ccahs_df<-read.csv("XXXX")

#Read in 2021 census datasets (urban)
#We filtered the census data to reflect the age and province variable levels
# reflected in each dataset. This is used to avoid including a subgroup, say an extra province,
# in the denominator (census) and not the numerator (study)
a_asu<-read.csv("2021 Canadian Census/censusasu_a_abcclsa.csv") #clsa,ab-c
#b_asu<-read.csv("2021 Canadian Census/censusasu_b_ccahs.csv") #ccahs provinces only
c_asu<-read.csv("2021 Canadian Census/censusasu_c_cbs.csv") #cbs
d_asu<-read.csv("2021 Canadian Census/censusasu_d_canpath.csv") #canpath
e_asu<-read.csv("2021 Canadian Census/censusasu_e_apl.csv") #apl
#f_asu<-read.csv("2021 Canadian Census/censusasu_f_ccahs.csv") #ccahs territories only

#Read in 2021 census datasets (race)
a_asr<-read.csv("2021 Canadian Census/censusasr_a_abcclsa.csv") #clsa,ab-c
#b_asr<-read.csv("2021 Canadian Census/censusasr_b_ccahs.csv") #ccahs provinces only
c_asr<-read.csv("2021 Canadian Census/censusasr_c_cbs.csv") #cbs
d_asr<-read.csv("2021 Canadian Census/censusasr_d_canpath.csv") #canpath
#f_asr<-read.csv("2021 Canadian Census/censusasr_f_ccahs.csv") #ccahs territories only

#Read in 2021 census datasets (quintmat) 
#b_sq<-read.csv("2021 Canadian Census/censussq_b_ccahs.csv") #CCAHS provinces only
c_sq<-read.csv("2021 Canadian Census/censussq_c_cbs.csv") #CBS
#f_sq<-read.csv("2021 Canadian Census/censussq_f_ccahs.csv") #ccahs territories only

# Bootstrap age-sex-urban representativeness ratios -----------------------
# Setting A (CLSA)
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df
output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    group_by(age_groups1,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asu,by = c("age_groups1","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
clsa_fin<-cbind(subcount[,c("age_groups1","sex","urban")],sapply(output,rrfun))
colnames(clsa_fin)[4]<-"rr_prob"
clsa_fin$cohort<-"CLSA closed cohort"
end<-Sys.time()
end - start
write_csv(clsa_fin,"boot_clsa_asu_5000age1.csv")

# Setting A (Ab-c)
start<-Sys.time()
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
    group_by(age_groups1,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asu,by = c("age_groups1","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
abc_fin<-cbind(subcount[,c("age_groups1","sex","urban")],sapply(output,rrfun))
colnames(abc_fin)[4]<-"rr_prob"
abc_fin$cohort<-"Ab-c open cohort"
end<-Sys.time()
end - start
write_csv(abc_fin,"boot_abc_asu_5000age1.csv")

#Setting B (CCAHS)
'#Run age-sex-urban bootstrap for CCAHS
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-ccahs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    group_by(age_groups1,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,b_asu,by = c("age_groups1","sex","urban"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = TRUE)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
cca_fin<-cbind(subcount[,c("age_groups1","sex","urban")],sapply(output,rrfun))
colnames(cca_fin)[4]<-"rr_prob"
cca_fin$cohort<-"CCAHSp"
end<-Sys.time()
end - start

#write_csv(cca_fin,"boot_ccahs_asu_5000age1.csv")'

#Setting C (CBS)
start<-Sys.time()
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
    group_by(age_groups1,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_asu,by = c("age_groups1","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
cbs_fin<-cbind(subcount[,c("age_groups1","sex","urban")],sapply(output,rrfun))
colnames(cbs_fin)[4]<-"rr_prob"
cbs_fin$cohort<-"CBS blood donor"
end<-Sys.time()
end - start
write_csv(cbs_fin,"boot_cbs_asu_5000age1.csv")

#Setting D (Canpath)
start<-Sys.time()
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
    group_by(age_groups1,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asu,by = c("age_groups1","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
can_fin<-cbind(subcount[,c("age_groups1","sex","urban")],sapply(output,rrfun))
colnames(can_fin)[4]<-"rr_prob"
can_fin$cohort<-"Canpath closed cohort"
end<-Sys.time()
end - start
write_csv(can_fin,"boot_can_asu_5000age1.csv")

#Setting E (APL)
start<-Sys.time()
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
    group_by(age_groups1,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,e_asu,by = c("age_groups1","sex","urban"),all.y = T)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = TRUE)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
apl_fin<-cbind(subcount[,c("age_groups1","sex","urban")],sapply(output,rrfun))
colnames(apl_fin)[4]<-"rr_prob"
apl_fin$cohort<- "APL outpatient laboratory"
end<-Sys.time()
end - start

write_csv(apl_fin,"boot_apl_asu_5000age1.csv")

#Setting F (CCAHS territories)
'#Run age-sex-urban bootstrap for CCAHS
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-ccahs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts by subgroup in resample
  group<-df %>% 
    group_by(age_groups1,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-urban
  alt<-aggregate(group,count ~ sex + urban, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,f_asu,by = c("age_groups1","sex","urban"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = TRUE)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
ccat_fin<-cbind(subcount[,c("age_groups1","sex","urban")],sapply(output,rrfun))
colnames(ccat_fin)[4]<-"rr_prob"
ccat_fin$cohort<-"CCAHSt"
end<-Sys.time()
end - start

#write_csv(ccat_fin,"boot_ccahst_asu_5000age1.csv")'


# Bootstrap age-sex-race representativeness ratios ------------------------

#Setting A (CLSA)
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-clsa_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

#Run
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(age_groups1,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asr,by = c("age_groups1","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
clsar_fin<-cbind(subcount[,c("age_groups1","sex","race")],sapply(output,rrfun))
colnames(clsar_fin)[4]<-"rr_prob"
clsar_fin$cohort<-"CLSA closed cohort"
end<-Sys.time()
end - start
write_csv(clsar_fin,"boot_clsa_asr_5000age1.csv")

#Setting A (Ab-c)
start<-Sys.time()
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
    group_by(age_groups1,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,a_asr,by = c("age_groups1","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
abcr_fin<-cbind(subcount[,c("age_groups1","sex","race")],sapply(output,rrfun))
colnames(abcr_fin)[4]<-"rr_prob"
abcr_fin$cohort<-"Ab-c closed cohort"
end<-Sys.time()
end - start
write_csv(abcr_fin,"boot_abc_asr_5000age1.csv")

#Setting B (CCAHS)
'start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-ccahs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(age_groups1,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,b_asr,by = c("age_groups1","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
ccar_fin<-cbind(group[,c("age_groups1","sex","race")],sapply(output,rrfun))
colnames(ccar_fin)[4]<-"rr_prob"
ccar_fin<-"CCAHSp"
end<-Sys.time()
end - start

#write_csv(ccar_fin,"boot_ccahs_asr_5000_age1.csv")'
#Setting C (CBS)
start<-Sys.time()
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
    group_by(age_groups1,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_asr,by = c("age_groups1","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
cbsr_fin<-cbind(subcount[,c("age_groups1","sex","race")],sapply(output,rrfun))
colnames(cbsr_fin)[4]<-"rr_prob"
cbsr_fin$cohort<-"CBS blood donor"
end<-Sys.time()
end - start
write_csv(cbsr_fin,"boot_cbs_asr_5000age1.csv")

#Setting D (Canpath)
start<-Sys.time()
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
    group_by(age_groups1,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,d_asr,by = c("age_groups1","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
canr_fin<-cbind(subcount[,c("age_groups1","sex","race")],sapply(output,rrfun))
colnames(canr_fin)[4]<-"rr_prob"
canr_fin$cohort<-"Canpath closed cohort"
end<-Sys.time()
end - start
write_csv(canr_fin,"boot_can_asr_5000age1.csv")

#Setting F (CCAHS territories)
'start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-ccahs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 24)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(age_groups1,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #calculate counts by sex-race
  alt<-aggregate(group,count ~ sex + race, FUN = sum, drop = F)
  alt$age_groups1<-"All ages"
  group<-rbind(group,alt)
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,f_asr,by = c("age_groups1","sex","race"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = T)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
  rr<-pct_resample / pct_pop
  
  #save output to matrix
  output[i,]<-rr
}

#label columns and make data.frame
output<-data.frame(output)

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75)) / length(x) #calculate proportion
  return(prop)
}

#perform calculation and re-assign bootstrap probability to each subgroup combination
ccatr_fin<-cbind(group[,c("age_groups1","sex","race")],sapply(output,rrfun))
colnames(ccatr_fin)[4]<-"rr_prob"
ccatr_fin<-"CCAHS"
end<-Sys.time()
end - start

#write_csv(ccatr_fin,"boot_ccahst_asr_5000_age1.csv")'

# Bootstrap sex-quintmat representation ratios ----------------------------

#Setting B (CCAHS)
'#Run sex-quintmat bootstrap for CCAHS
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-ccahs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CCAHS
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,b_sq,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = TRUE)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
ccaq_fin<-cbind(group[,c("sex","quintmat")],sapply(output,rrfun))
colnames(ccaq_fin)[3]<-"rr_prob"
ccaq_fin$cohort<-"CCAHSp"
end<-Sys.time()
end - start

write_csv(ccaq_fin,"boot_ccahs_sq_5000age1.csv")'

#Setting C (CBS)
#Initialize
start<-Sys.time()
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
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,c_sq,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = TRUE)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
cbsq_fin<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(cbsq_fin)[3]<-"rr_prob"
cbsq_fin$cohort<-"CBS blood donor"
end<-Sys.time()
end - start
write_csv(cbsq_fin,"boot_cbs_sq_5000age1.csv")

#Setting F (CCAHS territories)
'#Run sex-quintmat bootstrap for CCAHS
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-ccahs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 10)

#Run for CCAHS
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(sex,quintmat) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,f_sq,by = c("sex","quintmat"),all.y = TRUE)
  
  #calculate proportions and RR
  pct_resample<-subcount$count/sum(subcount$count,na.rm = TRUE)
  pct_pop<-subcount$count_census/sum(subcount$count_census)
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
ccatq_fin<-cbind(group[,c("sex","quintmat")],sapply(output,rrfun))
colnames(ccatq_fin)[3]<-"rr_prob"
ccatq_fin$cohort<-"CCAHS"
end<-Sys.time()
end - start

write_csv(ccatq_fin,"boot_ccahst_sq_5000age1.csv")'
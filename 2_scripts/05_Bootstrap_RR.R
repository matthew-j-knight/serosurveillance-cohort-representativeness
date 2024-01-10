"This script bootstraps representativeness ratios produced for each 
demographic subgroup and cohort. A subgroup is considered significantly 
underrepresented if 95% of the representativeness ratios fall below 
a threshold of 0.75"

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
censusu_asu18<-read.csv("census_allprov18+.csv")
censusu_asu<-read.csv("census_allprov1+.csv") #ccahs
censusu_asualb<-read.csv("census_alb1+.csv") #apl

#Read in 2021 census datasets (race)
censusr_asr18<-read.csv("censusr_allprov18+.csv")
censusr_asr<-read.csv("census_allprov1+.csv") #ccahs

#Read in 2021 census datasets (quintmat)
census_sq18<-read.csv("censusqm_allprov18+.csv") #cbs
census_sq<-read.csv("censusqm_allages.csv") #ccahs

# Bootstrap age-sex-urban groups ---------------------------------------------------------------
#Initialize
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-can_df

cols <- 12

output<-matrix(NA,nrow = n_replicates,
               ncol = cols)

#Run for CBS, CLSA, Canpath, Ab-c
set.seed(4)
for(i in 1:n_replicates){
    #draw bootstrap resample
    df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
    
    #calculate counts bu subgroup in resample
    group<-df %>% 
      group_by(age_groups,sex,urban) %>% 
      summarize(count = n()) %>% 
      ungroup()
    
    #merge resample with corresponding census dataset
    subcount<-merge(group,censusu_asu18,by = c("age_groups","sex","urban"),all.y = T)
    
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
fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(fin)[4]<-"rr_prob"
end<-Sys.time()
end - start

#### Runtimes (n = 1000) ###
# Ab-c 44 sec
# Can 41 sec
# CLSA 40 sec
# CBS 12 minutes
# APL 3 minutes 
# CCAHS XXXX

#### Runtimes (n = 5000) ###
# Ab-c 3.8 min
# Can 4 min
# CLSA 3 min
# CBS 1 hr
# APL 15 min
# CCAHS XX min

#label cohort and write_csv for specific run
#fin$cohort<-"CLSA probabilistic survey"
#fin$cohort<-"Ab-c probabilistic survey"
fin$cohort<-"Canpath probabilistic survey"
#fin$cohort<-"Blood Donor"

write_csv(fin,"boot_can_asu_5000.csv")

#Run age-sex-urban bootstrap for APL
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-apl_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 16)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,censusu_asualb,by = c("age_groups","sex","urban"),all.y = TRUE)
  
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
fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(fin)[4]<-"rr_prob"
end<-Sys.time()
end - start

#write APL to csv
fin$cohort<-"Outpatient Laboratory"
write_csv(fin,"boot_apl_asu_5000.csv")

'#Run age-sex-urban bootstrap for CCAHS
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL
data<-ccahs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(age_groups,sex,urban) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,censusu_asu,by = c("age_groups","sex","urban"),all.y = TRUE)
  
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
fin<-cbind(subcount[,c("age_groups","sex","urban")],sapply(output,rrfun))
colnames(fin)[4]<-"rr_prob"
end<-Sys.time()
end - start

#write CCAHS to csv
fin$cohort<-"CCAHS"
#write_csv(fin,"boot_ccahs_asu.csv")'

# Bootstrap age-sex-race groups ---------------------------------------------------------------
#Initialize
start<-Sys.time()
n_replicates<-5000 #number of bootstrap iterations
collect<-NULL

#Manually add in required dataset as "data" and set up .csv at the bottom (saves computational resources)
data<-can_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

#Run for CBS, CLSA, Canpath, Ab-c
set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,censusr_asr18,by = c("age_groups","sex","race"),all.y = TRUE)
  
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
fin<-cbind(subcount[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(fin)[4]<-"rr_prob"
end<-Sys.time()
end - start

#### Runtimes (n = 1000) ###
# Ab-c 50 sec 
# Can 42 sec 
# CLSA 50 sec 
# CBS 13 minutes 
# CCAHS XXXX

#### Runtimes (n = 5000)
# Ab-c 3.7 mins
# Can 3.6 mins
# CLSA 
# CBS 1 hr
# CCAHS

#label cohort and write_csv for specific run
#fin$cohort<-"CLSA probabilistic survey"
#fin$cohort<-"Ab-c probabilistic survey"
fin$cohort<-"Canpath probabilistic survey"
#fin$cohort<-"Blood Donor"

write_csv(fin,"boot_can_asr_5000.csv")
'
#Run age-sex-race bootstrap for CCAHS
start<-Sys.time()
n_replicates<-1000 #number of bootstrap iterations
collect<-NULL
data<-ccahs_df

output<-matrix(NA,nrow = n_replicates,
               ncol = 12)

set.seed(4)
for(i in 1:n_replicates){
  #draw bootstrap resample
  df<-data[sample(1:nrow(data),size = nrow(data),replace = T),]
  
  #calculate counts bu subgroup in resample
  group<-df %>% 
    group_by(age_groups,sex,race) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  #merge resample with corresponding census dataset
  subcount<-merge(group,censusr_asr,by = c("age_groups","sex","race"),all.y = TRUE)
  
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
fin<-cbind(group[,c("age_groups","sex","race")],sapply(output,rrfun))
colnames(fin)[4]<-"rr_prob"
end<-Sys.time()
end - start

#write CCAHS to csv
fin$cohort<-"CCAHS"
#write_csv(fin,"boot_ccahs_asr.csv")'

# Bootstrap sex-quintmat groups ---------------------------------------------------------------
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
  subcount<-merge(group,census_sq18,by = c("sex","quintmat"),all.y = TRUE)
  
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
fin<-cbind(subcount[,c("sex","quintmat")],sapply(output,rrfun))
colnames(fin)[3]<-"rr_prob"
end<-Sys.time()
end - start

#### Runtimes ###
# CBS 15 minutes (n = 1000)
# CBS 1 hour (n = 5000)
fin$cohort<-"Blood Donor"

write_csv(fin,"boot_cbs_sq_5000.csv")

'#Run sex-quintmat bootstrap for CCAHS
start<-Sys.time()
n_replicates<-1000 #number of bootstrap iterations
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
  subcount<-merge(group,census_sq,by = c("sex","quintmat"),all.y = TRUE)
  
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
fin<-cbind(group[,c("sex","quintmat")],sapply(output,rrfun))
colnames(fin)[3]<-"rr_prob"
end<-Sys.time()
end - start

#### Runtimes ###
# CBS XXXX minutes (n = 1000)
fin$cohort<-"CCAHS"

write_csv(fin,"boot_ccahs_asq.csv")'
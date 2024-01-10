"This script performs an empty cell analysis - what % of all available
cells contain counts less than 25. This is to identify the potential 
for adjustment for each cohort dataset"
# Read in count tables from csv ------------------------------------------
setwd("~/serosurveillance-cohort-representativeness/1_data/private")
library(tidyverse)
library(flextable)
library(scales)
cbs_df<-read.csv("cbs_df.csv")
apl_df<-read.csv("apl_df.csv")
abc_df<-read.csv("abc_df.csv")
clsa_df<-read.csv("clsa_df.csv")
can_df<-read.csv("can_df.csv")
# Analysis ----------------------------------------------------------------
#Age-sex-province datasets

cbs_as<-cbs_df %>% group_by(age_groups,sex,province,month) %>% summarize(count = n()) %>% ungroup()
apl_as<-apl_df %>% group_by(age_groups,sex,province,month) %>% summarize(count = n()) %>% ungroup()
abc_as<-abc_df %>% group_by(age_groups,sex,province,month) %>% summarize(count = n()) %>% ungroup()
clsa_as<-clsa_df %>% group_by(age_groups,sex,province,month) %>% summarize(count = n()) %>% ungroup()
can_as<-can_df %>% group_by(age_groups,sex,province,month) %>% summarize(count = n()) %>% ungroup()

cbs_countas<-round((nrow(cbs_as %>% filter (count > 25)))/nrow(cbs_as),2) * 100
apl_countas<-round((nrow(apl_as %>% filter (count > 25)))/nrow(apl_as),2) * 100
abc_countas<-round((nrow(abc_as %>% filter (count > 25)))/nrow(abc_as),2) * 100
clsa_countas<-round((nrow(clsa_as %>% filter (count > 25)))/nrow(clsa_as),2) * 100
can_countas<-round((nrow(can_as %>% filter (count > 25)))/nrow(can_as),2) * 100

#-Age-sex-urban-province datasets
cbs_asu<-cbs_df %>% group_by(age_groups,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()
apl_asu<-apl_df %>% group_by(age_groups,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()
abc_asu<-abc_df %>% group_by(age_groups,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()
clsa_asu<-clsa_df %>% group_by(age_groups,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()
can_asu<-can_df %>% group_by(age_groups,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()

cbs_countasu<-round((nrow(cbs_asu %>% filter (count > 25)))/nrow(cbs_asu),2) * 100 #% of cells with count less than 50
apl_countasu<-round((nrow(apl_asu %>% filter (count > 25)))/nrow(apl_asu),2) * 100
abc_countasu<-round((nrow(abc_asu %>% filter (count > 25)))/nrow(abc_asu),2) * 100
clsa_countasu<-round((nrow(clsa_asu %>% filter (count > 25)))/nrow(clsa_asu),2) * 100
can_countasu<-round((nrow(can_asu %>% filter (count > 25)))/nrow(can_asu),2) * 100

#Age-sex-race-province datasets
cbs_asr<-cbs_df %>% group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
abc_asr<-abc_df %>% group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asr<-clsa_df %>% group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
can_asr<-can_df %>% group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()

cbs_countasr<-round((nrow(cbs_asr %>% filter (count > 25))) / nrow(cbs_asr),2) * 100
abc_countasr<-round((nrow(abc_asr %>% filter (count > 25))) / nrow(abc_asr),2) * 100
clsa_countasr<-round((nrow(clsa_asr %>% filter (count > 25))) / nrow(clsa_asr),2) * 100
can_countasr<-round((nrow(can_asr %>% filter (count > 25))) / nrow(can_asr),2) * 100

#Age-sex-urban-province-race datasets
cbs_asur<-cbs_df %>% group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
abc_asur<-abc_df %>% group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asur<-clsa_df %>% group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
can_asur<-can_df %>% group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()

cbs_countasur<-round((nrow(cbs_asur %>% filter (count > 25)))/nrow(cbs_asur),2) * 100
abc_countasur<-round((nrow(abc_asur %>% filter (count > 25)))/nrow(abc_asur),2) * 100
clsa_countasur<-round((nrow(clsa_asur %>% filter (count > 25)))/nrow(clsa_asur),2) * 100
can_countasur<-round((nrow(can_asur %>% filter (count > 25)))/nrow(can_asur),2) * 100

#Collect into a data.frame
df<-data.frame(Cohort = c("CBS blood donor (606963)",
                          "APL outpatient laboratory (208097)",
                      "Ab-c open cohort (24879)",
                      "CLSA closed cohort (12803)",
                      "Canpath closed cohort (21899)"),
           Age_Sex_Prov = c(cbs_countas,apl_countas,abc_countas,clsa_countas,
                            can_countas),
           Age_Sex_Urban_Prov = c(cbs_countasu,apl_countasu,abc_countasu,
                                  clsa_countasu,can_countasu),
           Age_Sex_Race_Prov = c(cbs_countasr,NA,abc_countasr,
                                 clsa_countasr,clsa_countasr),
           Age_Sex_Race_Urban_Prov = c(cbs_countasur,NA,abc_countasur,
                                       clsa_countasur,can_countasur))
#Append % beside each value
df<-df %>% 
  mutate(Age_Sex_Prov = ifelse(is.na(Age_Sex_Prov),Age_Sex_Prov,
                               paste0(Age_Sex_Prov,"%")),
         Age_Sex_Urban_Prov = ifelse(is.na(Age_Sex_Urban_Prov),Age_Sex_Urban_Prov,
                               paste0(Age_Sex_Urban_Prov,"%")),
         Age_Sex_Race_Prov = ifelse(is.na(Age_Sex_Race_Prov),Age_Sex_Race_Prov,
                               paste0(Age_Sex_Race_Prov,"%")),
         Age_Sex_Race_Urban_Prov = ifelse(is.na(Age_Sex_Race_Urban_Prov),Age_Sex_Race_Urban_Prov,
                               paste0(Age_Sex_Race_Urban_Prov,"%")))

df<-df[order(df$Cohort),]

df
#write_csv(df,"table_2_analysisfun1.csv")

# Repeat using more precise age buckets (age_fun2) ------------------------
#Age-sex-province datasets
cbs_as<-cbs_df %>% group_by(age_groups1,sex,province,month) %>% summarize(count = n()) %>% ungroup()
apl_as<-apl_df %>% group_by(age_groups1,sex,province,month) %>% summarize(count = n()) %>% ungroup()
abc_as<-abc_df %>% group_by(age_groups1,sex,province,month) %>% summarize(count = n()) %>% ungroup()
clsa_as<-clsa_df %>% group_by(age_groups1,sex,province,month) %>% summarize(count = n()) %>% ungroup()
can_as<-can_df %>% group_by(age_groups1,sex,province,month) %>% summarize(count = n()) %>% ungroup()

cbs_countas<-round((nrow(cbs_as %>% filter (count > 25)))/nrow(cbs_as),2) * 100
apl_countas<-round((nrow(apl_as %>% filter (count > 25)))/nrow(apl_as),2) * 100
abc_countas<-round((nrow(abc_as %>% filter (count > 25)))/nrow(abc_as),2) * 100
clsa_countas<-round((nrow(clsa_as %>% filter (count > 25)))/nrow(clsa_as),2) * 100
can_countas<-round((nrow(can_as %>% filter (count > 25)))/nrow(can_as),2) * 100

#-Age-sex-urban-province datasets
cbs_asu<-cbs_df %>% group_by(age_groups1,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()
apl_asu<-apl_df %>% group_by(age_groups1,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()
abc_asu<-abc_df %>% group_by(age_groups1,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()
clsa_asu<-clsa_df %>% group_by(age_groups1,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()
can_asu<-can_df %>% group_by(age_groups1,sex,province,urban,month) %>% summarize(count = n()) %>% ungroup()

cbs_countasu<-round((nrow(cbs_asu %>% filter (count > 25)))/nrow(cbs_asu),2) * 100 #% of cells with count less than 50
apl_countasu<-round((nrow(apl_asu %>% filter (count > 25)))/nrow(apl_asu),2) * 100
abc_countasu<-round((nrow(abc_asu %>% filter (count > 25)))/nrow(abc_asu),2) * 100
clsa_countasu<-round((nrow(clsa_asu %>% filter (count > 25)))/nrow(clsa_asu),2) * 100
can_countasu<-round((nrow(can_asu %>% filter (count > 25)))/nrow(can_asu),2) * 100

#Age-sex-race-province datasets
cbs_asr<-cbs_df %>% group_by(age_groups1,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
abc_asr<-abc_df %>% group_by(age_groups1,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asr<-clsa_df %>% group_by(age_groups1,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
can_asr<-can_df %>% group_by(age_groups1,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()

cbs_countasr<-round((nrow(cbs_asr %>% filter (count > 25))) / nrow(cbs_asr),2) * 100
abc_countasr<-round((nrow(abc_asr %>% filter (count > 25))) / nrow(abc_asr),2) * 100
clsa_countasr<-round((nrow(clsa_asr %>% filter (count > 25))) / nrow(clsa_asr),2) * 100
can_countasr<-round((nrow(can_asr %>% filter (count > 25))) / nrow(can_asr),2) * 100

#Age-sex-urban-province-race datasets
cbs_asur<-cbs_df %>% group_by(age_groups1,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
abc_asur<-abc_df %>% group_by(age_groups1,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asur<-clsa_df %>% group_by(age_groups1,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
can_asur<-can_df %>% group_by(age_groups1,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()

cbs_countasur<-round((nrow(cbs_asur %>% filter (count > 25)))/nrow(cbs_asur),2) * 100
abc_countasur<-round((nrow(abc_asur %>% filter (count > 25)))/nrow(abc_asur),2) * 100
clsa_countasur<-round((nrow(clsa_asur %>% filter (count > 25)))/nrow(clsa_asur),2) * 100
can_countasur<-round((nrow(can_asur %>% filter (count > 25)))/nrow(can_asur),2) * 100

#Collect into a data.frame
df<-data.frame(Cohort = c("CBS blood donor (606963)",
                          "APL outpatient laboratory (208097)",
                          "Ab-c open cohort (24879)",
                          "CLSA closed cohort (12803)",
                          "Canpath closed cohort (21899)"),
               Month_S = c(length(unique(cbs_asu$month)),length(unique(apl_asu$month)),
                                  length(unique(abc_asu$month)),length(unique(clsa_asu$month)),
                                  length(unique(can_asu$month))),
               Age_Sex_Prov = c(cbs_countas,apl_countas,abc_countas,clsa_countas,
                                can_countas),
               Age_Sex_Urban_Prov = c(cbs_countasu,apl_countasu,abc_countasu,
                                      clsa_countasu,can_countasu),
               Age_Sex_Race_Prov = c(cbs_countasr,NA,abc_countasr,
                                     clsa_countasr,clsa_countasr),
               Age_Sex_Race_Urban_Prov = c(cbs_countasur,NA,abc_countasur,
                                           clsa_countasur,can_countasur))

df<-df[c(1:3,5,4),] #order by specimen count
colnames(df)[1]<-"Study (specimen count)"
#df<-df[order(df$Cohort),]

#write_csv(df,"table_2_analysisfun2.csv")


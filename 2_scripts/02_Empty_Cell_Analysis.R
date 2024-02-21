"This script performs a cell analysis. After stratifying each dataset
by multiple demographic variables, we calculate the proportion of cells 
with counts greater than 25."

# Read in count tables from csv ------------------------------------------
setwd("~/serosurveillance-cohort-representativeness/1_data/private")
library(tidyverse)
library(flextable)
library(scales)
cbs_df<-read.csv("cbs_df_jan222024.csv")
apl_df<-read.csv("apl_df_jan222024.csv")
abc_df<-read.csv("abc_df_jan222024.csv")
clsa_df<-read.csv("clsa_df_jan222024.csv")
can_df<-read.csv("can_df_jan222024.csv")

#Run analysis ------------------------
#Age-sex-province-month datasets
cbs_as<-cbs_df %>% group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()
apl_as<-apl_df %>% filter(sex != "Unknown") %>% 
  group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()
abc_as<-abc_df %>% filter(sex != "Self described") %>% 
  group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()
clsa_as<-clsa_df %>% group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()
can_as<-can_df %>% group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()

cbs_countas<-round((nrow(cbs_as %>% filter (count > 25)))/nrow(cbs_as),2) * 100
apl_countas<-round((nrow(apl_as %>% filter (count > 25)))/nrow(apl_as),2) * 100
abc_countas<-round((nrow(abc_as %>% filter (count > 25)))/nrow(abc_as),2) * 100
clsa_countas<-round((nrow(clsa_as %>% filter (count > 25)))/nrow(clsa_as),2) * 100
can_countas<-round((nrow(can_as %>% filter (count > 25)))/nrow(can_as),2) * 100

#-Age-sex-urban-province-month datasets
cbs_asu<-cbs_df %>%filter(!is.na(urban)) %>% 
  group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()
apl_asu<-apl_df %>% filter(sex != "Unknown" & !is.na(urban)) %>%
  group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()
abc_asu<-abc_df %>% filter(sex != "Self described" & !is.na(urban)) %>% 
  group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()
clsa_asu<-clsa_df %>% filter(!is.na(urban)) %>% 
  group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()
can_asu<-can_df %>% group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()

cbs_countasu<-round((nrow(cbs_asu %>% filter (count > 25)))/nrow(cbs_asu),2) * 100
apl_countasu<-round((nrow(apl_asu %>% filter (count > 25)))/nrow(apl_asu),2) * 100
abc_countasu<-round((nrow(abc_asu %>% filter (count > 25)))/nrow(abc_asu),2) * 100
clsa_countasu<-round((nrow(clsa_asu %>% filter (count > 25)))/nrow(clsa_asu),2) * 100
can_countasu<-round((nrow(can_asu %>% filter (count > 25)))/nrow(can_asu),2) * 100

#Age-sex-race-province-month datasets
cbs_asr<-cbs_df %>% filter(race != "Missing") %>% 
  group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
abc_asr<-abc_df %>% filter(sex != "Self described" &
                             race != "pnts" & !is.na(race)) %>% 
  group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asr<-clsa_df %>% filter(!is.na(race) & race != "pnts") %>% 
  group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
can_asr<-can_df %>% filter(race != "pnts" & !is.na(race)) %>% 
  group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()

cbs_countasr<-round((nrow(cbs_asr %>% filter (count > 25))) / nrow(cbs_asr),2) * 100
abc_countasr<-round((nrow(abc_asr %>% filter (count > 25))) / nrow(abc_asr),2) * 100
clsa_countasr<-round((nrow(clsa_asr %>% filter (count > 25))) / nrow(clsa_asr),2) * 100
can_countasr<-round((nrow(can_asr %>% filter (count > 25))) / nrow(can_asr),2) * 100

#Age-sex-urban-province-race-month datasets
cbs_asur<-cbs_df %>% filter(!is.na(urban) &
                              race != "Missing") %>% 
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
abc_asur<-abc_df %>% filter(sex != "Self described" & !is.na(urban) &
                              race != "pnts" & !is.na(race)) %>% 
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asur<-clsa_df %>% filter(!is.na(urban) &!is.na(race) & race != "pnts") %>% 
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
can_asur<-can_df %>% filter(race != "pnts" & !is.na(race)) %>% 
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()

cbs_countasur<-round((nrow(cbs_asur %>% filter (count > 25)))/nrow(cbs_asur),2) * 100
abc_countasur<-round((nrow(abc_asur %>% filter (count > 25)))/nrow(abc_asur),2) * 100
clsa_countasur<-round((nrow(clsa_asur %>% filter (count > 25)))/nrow(clsa_asur),2) * 100
can_countasur<-round((nrow(can_asur %>% filter (count > 25)))/nrow(can_asur),2) * 100

#Collect into a data.frame
df<-data.frame(Cohort = c("CBS blood donor (857,620)",
                          "APL outpatient laboratory (168,125)",
                          "Ab-c open cohort (24,455)",
                          "CLSA closed cohort (12,834)",
                          "Canpath closed cohort (20,817)"),
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

write_csv(df,"table_2_analysisjan222024.csv")


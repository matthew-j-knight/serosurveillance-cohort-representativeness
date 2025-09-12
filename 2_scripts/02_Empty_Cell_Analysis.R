
# 0. Description ----------------------------------------------------------
# 1. Load data and functions
# 2. Run primary analysis
# 3. Run sensitivity analysis 1

# 1. Load data and functions ------------------------------------------
rm(list = ls())
library(tidyverse)
library(scales)
library(readxl)
cbs_df<-read.csv("./1_data/private/cbs_df_final.csv")
apl_df<-read.csv("./1_data/private/apl_df_final.csv")
abc_df<-read.csv("./1_data/private/abc_df_final.csv")
clsa_df<-read.csv("./1_data/private/clsa_df_final.csv")
can_df<-read.csv("./1_data/private/can_df_final.csv")
can_df1<-read.csv("./1_data/private/can_df1_final.csv")
ccahs1_out<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_ratio/df_emptycell.xlsx")
ccahs1_outs<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_ratio/df_emptycell_s.xlsx") #sensitivity analysis 1

# 2. Run analysis ------------------------
#Age-sex-province-month datasets
cbs_as<-cbs_df %>% group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()
apl_as<-apl_df %>% filter(!is.na(sex) & !is.na(age_groups)) %>% 
  group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()
abc_as<-abc_df %>% 
  filter(sex != "Self described" & province != "YT" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()
clsa_as<-clsa_df %>% 
  filter(!is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,month) %>% 
  summarize(count = n()) %>% ungroup()
can_as<-can_df %>% filter(!is.na(sampledate) & !is.na(urban)) %>% 
  group_by(age_groups,sex,province1,month) %>% 
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
apl_asu<-apl_df %>% filter(!is.na(sex) & !is.na(age_groups)) %>%
  group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()
abc_asu<-abc_df %>% 
  filter(sex != "Self described" & !is.na(urban) & province != "YT" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()
clsa_asu<-clsa_df %>% 
  filter(!is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,urban,month) %>% 
  summarize(count = n()) %>% ungroup()
can_asu<-can_df %>% 
  filter(!is.na(sampledate) & !is.na(urban)) %>% 
  group_by(age_groups,sex,province1,urban,month) %>% 
  summarize(count = n()) %>% ungroup()

cbs_countasu<-round((nrow(cbs_asu %>% filter (count > 25)))/nrow(cbs_asu),2) * 100
apl_countasu<-round((nrow(apl_asu %>% filter (count > 25)))/nrow(apl_asu),2) * 100
abc_countasu<-round((nrow(abc_asu %>% filter (count > 25)))/nrow(abc_asu),2) * 100
clsa_countasu<-round((nrow(clsa_asu %>% filter (count > 25)))/nrow(clsa_asu),2) * 100
can_countasu<-round((nrow(can_asu %>% filter (count > 25)))/nrow(can_asu),2) * 100

#Age-sex-race-province-month datasets
cbs_asr<-cbs_df %>% 
  filter(!is.na(race)) %>% 
  group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
abc_asr<-abc_df %>% 
  filter(sex != "Self described" &
           !is.na(race) & province != "YT" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asr<-clsa_df %>% filter(!is.na(race) & race != "pnts" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,race,month) %>%
  summarize(count = n()) %>% ungroup()
can_asr<-can_df %>% filter(race != "pnts" & !is.na(race) & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province1,race,month) %>%
  summarize(count = n()) %>% ungroup()

cbs_countasr<-round((nrow(cbs_asr %>% filter (count > 25))) / nrow(cbs_asr),2) * 100
abc_countasr<-round((nrow(abc_asr %>% filter (count > 25))) / nrow(abc_asr),2) * 100
clsa_countasr<-round((nrow(clsa_asr %>% filter (count > 25))) / nrow(clsa_asr),2) * 100
can_countasr<-round((nrow(can_asr %>% filter (count > 25))) / nrow(can_asr),2) * 100

#Age-sex-urban-province-race-month datasets
cbs_asur<-cbs_df %>% filter(!is.na(urban) & !is.na(race)) %>% 
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
abc_asur<-abc_df %>% 
  filter(sex != "Self described" & !is.na(urban) & !is.na(race) & province != "YT" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asur<-clsa_df %>% 
  filter(!is.na(race) & race != "pnts" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()
can_asur<-can_df %>% 
  filter(race != "pnts" & !is.na(race) & !is.na(sampledate) & !is.na(urban)) %>% 
  group_by(age_groups,sex,province1,urban,race,month) %>%
  summarize(count = n()) %>% ungroup()

cbs_countasur<-round((nrow(cbs_asur %>% filter (count > 25)))/nrow(cbs_asur),2) * 100
abc_countasur<-round((nrow(abc_asur %>% filter (count > 25)))/nrow(abc_asur),2) * 100
clsa_countasur<-round((nrow(clsa_asur %>% filter (count > 25)))/nrow(clsa_asur),2) * 100
can_countasur<-round((nrow(can_asur %>% filter (count > 25)))/nrow(can_asur),2) * 100

#Collect into a data.frame
df<-data.frame(Cohort = c("CBS blood donor (1,035,580)",
                          "APL outpatient laboratory (210,906)",
                          "Ab-C open cohort (27,140)",
                          "CLSA closed cohort (17,310)",
                          "CanPath closed cohort (25,156)"),
               Month_S = c(length(unique(floor_date(as.Date(cbs_df$sampledate),"1 month"))),
                           length(unique(floor_date(as.Date(apl_df$sampledate),"1 month"))),
                           length(unique(floor_date(as.Date(abc_df[!is.na(abc_df$sampledate),]$sampledate),"1 month"))),
                           length(unique(floor_date(as.Date(clsa_df[!is.na(clsa_df$sampledate),]$sampledate),"1 month"))),
                           length(unique(floor_date(as.Date(can_df[!is.na(can_df$sampledate),]$sampledate),"1 month")))),
               Age_Sex_Prov = c(cbs_countas,apl_countas,abc_countas,clsa_countas,
                                can_countas),
               Age_Sex_Urban_Prov = c(cbs_countasu,apl_countasu,abc_countasu,
                                      clsa_countasu,can_countasu),
               Age_Sex_Race_Prov = c(cbs_countasr,NA,abc_countasr,
                                     clsa_countasr,can_countasr),
               Age_Sex_Race_Urban_Prov = c(cbs_countasur,NA,abc_countasur,
                                           clsa_countasur,can_countasur))
#Clean CCAHS-1 run and add to df
ccahs1_out$Cohort<-"CCAHS-1 closed cohort (11,050)"
ccahs1_out[,4:7]<-lapply(ccahs1_out[,4:7], function(x){
  x<-round(as.numeric(x,2))
  return(x)})

#Convert number of 2-month sampling periods to number of months sampled
ccahs1_out$Month_S<-ccahs1_out$Month_S * 2
df<-rbind(df,ccahs1_out[,2:7])
df<-df[c(1:3,5,4,6),] #order by specimen count
colnames(df)[1]<-"Study (specimen count)"

#write_csv(df,"./4_output/table_3_analysisfinal.csv")

# 3. Run sensitivity analysis 1 ------------------
#Age-sex-race-province-month datasets
abc_asr1<-abc_df %>% 
  filter(sex != "Self described"  &
           !is.na(race1) & province != "YT" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,race1,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asr1<-clsa_df %>% filter(!is.na(race1) & race1 != "pnts" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,race1,month) %>%
  summarize(count = n()) %>% ungroup()
can_asr1<-can_df1 %>% filter(race1 != "pnts" & !is.na(race1) & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province1,race1,month) %>%
  summarize(count = n()) %>% ungroup()

abc_countasr1<-round((nrow(abc_asr1 %>% filter (count > 25))) / nrow(abc_asr1),2) * 100
clsa_countasr1<-round((nrow(clsa_asr1 %>% filter (count > 25))) / nrow(clsa_asr1),2) * 100
can_countasr1<-round((nrow(can_asr1 %>% filter (count > 25))) / nrow(can_asr1),2) * 100

#Age-sex-urban-province-race-month datasets
abc_asur1<-abc_df %>% 
  filter(sex != "Self described" & !is.na(urban) & !is.na(race1) & province != "YT" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,urban,race1,month) %>%
  summarize(count = n()) %>% ungroup()
clsa_asur1<-clsa_df %>% 
  filter(!is.na(race1) & race1 != "pnts" & !is.na(sampledate)) %>% 
  group_by(age_groups,sex,province,urban,race1,month) %>%
  summarize(count = n()) %>% ungroup()
can_asur1<-can_df1 %>% 
  filter(race1 != "pnts" & !is.na(race1) & !is.na(sampledate) & !is.na(urban)) %>% 
  group_by(age_groups,sex,province1,urban,race1,month) %>%
  summarize(count = n()) %>% ungroup()

abc_countasur1<-round((nrow(abc_asur1 %>% filter (count > 25)))/nrow(abc_asur1),2) * 100
clsa_countasur1<-round((nrow(clsa_asur1 %>% filter (count > 25)))/nrow(clsa_asur1),2) * 100
can_countasur1<-round((nrow(can_asur1 %>% filter (count > 25)))/nrow(can_asur1),2) * 100

#Collect into a data.frame
df1<-data.frame(Cohort = c("CBS blood donor (1,035,580)",
                           "APL outpatient laboratory (210,906)",
                           "Ab-C open cohort (27,140)",
                           "CLSA closed cohort (17,310)",
                           "CanPath closed cohort (25,156)"),
                Month_S = c(length(unique(floor_date(as.Date(cbs_df$sampledate),"1 month"))),
                            length(unique(floor_date(as.Date(apl_df$sampledate),"1 month"))),
                            length(unique(floor_date(as.Date(abc_df[!is.na(abc_df$sampledate),]$sampledate),"1 month"))),
                            length(unique(floor_date(as.Date(clsa_df[!is.na(clsa_df$sampledate),]$sampledate),"1 month"))),
                            length(unique(floor_date(as.Date(can_df1[!is.na(can_df1$sampledate),]$sampledate),"1 month")))),
               Age_Sex_Prov = c(cbs_countas,apl_countas,abc_countas,clsa_countas,
                                can_countas),
               Age_Sex_Urban_Prov = c(cbs_countasu,apl_countasu,abc_countasu,
                                      clsa_countasu,can_countasu),
               Age_Sex_Race_Prov = c(cbs_countasr,NA,abc_countasr1,
                                     clsa_countasr1,can_countasr1),
               Age_Sex_Race_Urban_Prov = c(cbs_countasur,NA,abc_countasur1,
                                           clsa_countasur1,can_countasur1))

#Clean CCAHS-1 sensitivity analysis and add to df
ccahs1_outs$Cohort<-"CCAHS-1 closed cohort (11,050)"
ccahs1_outs[,4:5]<-lapply(ccahs1_outs[,4:5], function(x){
  x<-round(as.numeric(x,2))
  return(x)})

#Add columns from CCAHS-1 run above which remain unchanged by sensitivity analysis
ccahs1_outs<-ccahs1_outs %>% 
  mutate(Age_Sex_Prov = ccahs1_out$Age_Sex_Prov,
         Age_Sex_Urban_Prov = ccahs1_out$Age_Sex_Urban_Prov,
         Month_S = ccahs1_out$Month_S) %>% 
  select(Cohort,Month_S,Age_Sex_Prov,Age_Sex_Urban_Prov,Age_Sex_Race_Prov,
         Age_Sex_Race_Urban_Prov)
df1<-rbind(df1,ccahs1_outs)
df1<-df1[c(1:3,5,4,6),] #order by specimen count
colnames(df1)[1]<-"Study (specimen count)"
#write_csv(df1,"./4_output/table_3_analysisfinal1.csv")
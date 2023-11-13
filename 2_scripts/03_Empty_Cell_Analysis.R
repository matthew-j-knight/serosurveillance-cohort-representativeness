"This script performs an empty cell analysis - what % of all available
cells contain less than XXXX counts. This is to identify the potential 
for adjustment for each cohort dataset"
# Read in count tables from csv ------------------------------------------
library(tidyverse)
library(flextable)

#CBS - extracted Nov 6 2023
cbs_asr<-read.csv("1_data/CBS/cbs_asr_nov62023_count.csv")
cbs_asu<-read.csv("1_data/CBS/cbs_asu_nov62023_count.csv")

#APL - extracted Nov 7 2023
apl_asu<-read.csv("1_data/APL/apl_asu_nov72023_count.csv")

#Ab-c - extracted Nov 8 2023
abc_asu<-read.csv("1_data/Ab-c/abc_asu_nov82023_count.csv")
abc_asr<-read.csv("1_data/Ab-c/abc_asr_nov82023_count.csv")


# Analysis ----------------------------------------------------------------

#-Age-sex-urban datasets
cbs_countasu<-round((nrow(cbs_asu %>% filter (count < 50)))/nrow(cbs_asu),2) #% of cells with count less than 50
apl_countasu<-round((nrow(apl_asu %>% filter (count < 50)))/nrow(apl_asu),2)
abc_countasu<-round((nrow(abc_asu %>% filter (count < 50)))/nrow(abc_asu),2)

#Age-sex-race datasets
cbs_countasr<-round((nrow(cbs_asr %>% filter (count < 50))) / nrow(cbs_asr),2)
abc_countasr<-round((nrow(abc_asr %>% filter (count < 50))) / nrow(abc_asr),2)

#Age-sex datasets
source("2_scripts/01_Data_Cleaning.R")
cbs_as<-cbs_df %>% group_by(age_groups,sex) %>% summarize(count = n()) %>% ungroup()
apl_as<-apl_df %>% group_by(age_groups,sex) %>% summarize(count = n()) %>% ungroup()
abc_as<-abc_df %>% group_by(age_groups,sex) %>% summarize(count = n()) %>% ungroup()

cbs_countas<-round((nrow(cbs_as %>% filter (count < 50)))/nrow(cbs_as),2)
apl_countas<-round((nrow(apl_as %>% filter (count < 50)))/nrow(apl_as),2)
abc_countas<-round((nrow(abc_as %>% filter (count < 50)))/nrow(abc_as),2)

#Age-sex-urban-race datasets
cbs_asur<-cbs_df %>% group_by(age_groups,sex,urban,race) %>% summarize(count = n()) %>% ungroup()
abc_asur<-abc_df %>% group_by(age_groups,sex,urban,race) %>% summarize(count = n()) %>% ungroup()

cbs_countasur<-round((nrow(cbs_asur %>% filter (count < 50)))/nrow(cbs_asur),2)
abc_countasur<-round((nrow(abc_asur %>% filter (count < 50)))/nrow(abc_asur),2)

df<-data.frame(Cohort = c("Blood Donor","Outpatient Laboratory",
                      "Ab-c Probabilistic Survey"),
           Age_Sex = c(cbs_countas,apl_countas,abc_countas),
           Age_Sex_Urban = c(cbs_countasu,apl_countasu,abc_countasu),
           Age_Sex_Race = c(cbs_countasr,"NA",abc_countasr),
           Age_Sex_Race_Urban = c(cbs_countasur,"NA",abc_countasur))
df<-df[order(df$Cohort),]

# Make flextable
t2<-flextable(df)
t2<-add_header_row(t2,values = c("","Demographic Subgroups"),colwidths = c(1,4))
t2<-theme_booktabs(t2)
t2<-align(t2,i = 1, j = NULL,part = 'header',align = 'center')
t2<-hline(t2,i = 1, j = c(2:5),part = 'header')
t2<-autofit(t2,add_w = 0,add_h = 0)
t2<-align(t2,part = 'body',align = 'center',j = c(2:5))
t2<-labelizor(x = t2,labels = c("Age_Sex" = "Age, Sex",
                                       "Age_Sex_Urban" = "Age, Sex, Urban",
                                       "Age_Sex_Race" = "Age, Sex, Race",
                                       "Age_Sex_Race_Urban" = "Age, Sex, Race, Urban"))
t2

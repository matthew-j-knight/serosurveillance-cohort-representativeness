# MTP data cleaning #
#Load libraries and functions
setwd("~/serosurveillance-cohort-representativeness/1_data/private") #remove final pub
library(haven)
library(lubridate)
library(tidyverse)# loads readr
library(DBI)
library(RPostgres)
library(flextable)

#Read in MTP data and serology results
#Canpath dataset import
canpath_mtp<-read.csv("./CANPATH/DAO-543759_ResearcherDataset_Qx_1114par_1127varMTP.csv")#manitoba cohort
canpath_seradmin<-read.csv("./CANPATH/DAO-543759_ResearcherDataset_Serology_Admin_25727par.csv")
canpath_serres<-read.csv("./CANPATH/DAO-543759_ResearcherDataset_Serology_Results_74503par.csv")

#Functions
province_fun <- function(var) {
  fsa_f = as.character(substr(var,1,1))
  prov =  ifelse(fsa_f == "A", "NL", #first letter of FSA corresponds to Canadian province or territory
                 ifelse(fsa_f == "B", "NS",
                        ifelse(fsa_f == "C", "PE",
                               ifelse(fsa_f == "E", "NB",
                                      ifelse(fsa_f == "G" | fsa_f == "H" | fsa_f == "J", "QC",
                                             ifelse(fsa_f == "K" | fsa_f == "L" | fsa_f == "M" | fsa_f == "N" | fsa_f == "P", "ON",
                                                    ifelse(fsa_f == "R", "MB",
                                                           ifelse(fsa_f == "S", "SK",
                                                                  ifelse(fsa_f == "T", "AB",
                                                                         ifelse(fsa_f == "V", "BC",
                                                                                ifelse(fsa_f == "X", "NU/NT",
                                                                                       ifelse(fsa_f == "Y", "YT", NA
                                                                                       ))))))))))))
  return(prov)}

#Create age groups

age_groups_fun <- function(variable){ #age buckets corresponding with census
  age_group = cut(variable,
                  breaks = c(0,18,40,55,Inf),
                  labels = c("0-17 years","18-39 years","40-54 years",
                             "55+ years"),
                  right = FALSE)
  return(age_group)
}


#Identify MTP participants not in primary dataset and rbind to can_df
canpath_mtp <- merge(canpath_mtp, canpath_seradmin, by='ResearcherID', all.x = TRUE)
canpath_mtp <- merge(canpath_serres,canpath_mtp, by='ResearcherID', all.x = TRUE)
cnames <- c("ResearcherID",'C1_SDC_AGE', 'C1_SDC_SEX', 'C1_SDC_GENDER', 'C1_ADM_FSA',
            "C1_SDC_EB_ARAB","C1_SDC_EB_BLACK","C1_SDC_EB_CHINESE",
            "C1_SDC_EB_FILIPINO","C1_SDC_EB_JAPANESE",
            "C1_SDC_EB_KOREAN","C1_SDC_EB_LATIN","C1_SDC_EB_S_ASIAN",
            "C1_SDC_EB_SE_ASIAN","C1_SDC_EB_W_ASIAN","C1_SDC_EB_WHITE",
            "C1_SDC_EB_OTHER", "C1_SDC_EB_OTHER_OTSP","C1_SDC_EB_CA","C1_SDC_EB_RESERVE_CURR",  
            "C1_ADM_COLLECT_DATE","C1_SAMPLE_ANTIGEN_TESTED","C1_SAMPLE_RESULT","C1_SAMPLE_ANTIGEN_CUTOFF",
            "C1_SAMPLE_RESULTS_DESCRIPTION", "C1_CITF_ASSAY_ID", 'C1_SAMPLE_SUGGESTED_STATUS')#C1 indicates C-19 substudy
canpath_mtp <- canpath_mtp[cnames]

#Clean demographics and include only participants with a sample result
canpath_mtp<-canpath_mtp %>% filter(is.na(C1_SDC_AGE)!=T & is.na(C1_SDC_SEX)!=T & 
                                      is.na(C1_ADM_FSA)==F & is.na(C1_ADM_COLLECT_DATE) == F &
                                      C1_SAMPLE_ANTIGEN_TESTED == 1)
#Create province variable
canpath_mtp$C1_ADM_FSA<-toupper(canpath_mtp$C1_ADM_FSA)
canpath_mtp$province<-province_fun(canpath_mtp$C1_ADM_FSA)

#Remove individuals which do not reside in Manitoba
canpath_mtp<-canpath_mtp[!(canpath_mtp$province == "ON" | canpath_mtp$province == "YT"),]

#Create age group variable
canpath_mtp$age_groups<-age_groups_fun(canpath_mtp$C1_SDC_AGE)

#Classify residence as urban or rural
canpath_mtp$urban<-with(canpath_mtp,
                         ifelse(substr(C1_ADM_FSA,start = 2,stop = 2) != "0",
                                "Urban","Rural"))
#Re-format sex variable (validated)
canpath_mtp$C1_SDC_SEX<-ifelse(canpath_mtp$C1_SDC_SEX == 0,"Male","Female")

#Classify ethnicity as white or visible minority - mixed ethnicity labelled as visible minority
canpath_mtp$race<-NA

#Identify individuals who prefer not to identify their ethnicity
#Check these individuals did not select any other ethnicity options.
which(canpath_mtp$C1_SDC_EB_CA == 8 & 
        rowSums(canpath_mtp[,c(6:17,19)],na.rm = T) > 8 &
        is.na(canpath_mtp[,18]==F)
)#0

canpath_mtp[which(canpath_mtp$C1_SDC_EB_CA == 8),"race"]<-"pnts" #3

#Identify individuals who selected their ethnicity as only white and did not use text box
canpath_mtp[which(canpath_mtp$C1_SDC_EB_WHITE == 1 &
                     rowSums(canpath_mtp[,c(6:17)],na.rm = T) == 1 &
                     is.na(canpath_mtp[,18]) == T),"race"]<-"White" #591

#Identify individuals who selected their ethnicity as only visible minority and did not use text box
# or select "other"
canpath_mtp[which(is.na(canpath_mtp$C1_SDC_EB_WHITE) == T &
                     is.na(canpath_mtp$C1_SDC_EB_OTHER) == T &
                     is.na(canpath_mtp$C1_SDC_EB_OTHER_OTSP) == T),"race"]<-"Visible minority" #25

#Identify individuals who selected their ethnicity as white and 1 or more visible minorities, did not select other,
# did not use text box
canpath_mtp[which(canpath_mtp$C1_SDC_EB_WHITE == 1 &
                     rowSums(canpath_mtp[,6:17],na.rm = T) > 1 &
                     is.na(canpath_mtp$C1_SDC_EB_OTHER) == T &
                     is.na(canpath_mtp$C1_SDC_EB_OTHER_OTSP) == T),"race"]<-"Visible minority" #15

#Check remaining unclassified cases are in _other and/or textbox columns
sum(is.na(canpath_mtp$C1_SDC_EB_OTHER)==F) #20 selected other

#Check how may people who selected other used the text box
length(which(is.na(canpath_mtp$C1_SDC_EB_OTHER)==F &
               is.na(canpath_mtp$C1_SDC_EB_OTHER_OTSP) == F)) #20 - all who selected other used text box

#Check if anyone used text box and did not select other
sum(is.na(canpath_mtp$C1_SDC_EB_OTHER_OTSP)==F & 
      is.na(canpath_mtp$C1_SDC_EB_OTHER) == T) #0

#Check if those who selected other and wrote in the textbox also selected other ethnicities
length(which(is.na(canpath_mtp$C1_SDC_EB_OTHER_OTSP)==F &
               is.na(canpath_mtp$C1_SDC_EB_OTHER)==F &
               rowSums(canpath_mtp[,6:17],na.rm = T) > 1)) #0

#Review textbox for remaining participants and assign visible minority status (n = 284)
txtvm1<-which(is.na(canpath_mtp$C1_SDC_EB_OTHER_OTSP)==F &
               is.na(canpath_mtp$C1_SDC_EB_OTHER)==F &
               rowSums(canpath_mtp[,6:17],na.rm = T) == 1)

canpath_mtp[txtvm1,"race"]<-"White"

#Review text box entry and change status if visible minority
canpath_mtp[txtvm1,18]
canpath_mtp[canpath_mtp$C1_SDC_EB_OTHER_OTSP %in%
              c("MÃ\u0083Â©tis","South American","caucasian - european, indian, iraqi",
                "Metis","European & MÃ\u0083Â©tis","Metis French Canadian","Metis Canadian",
                "White/Southeast Asian"),"race"]<-"Visible minority"
#Generate final df
colnames(canpath_mtp)[3]<-"sex"
canpath_mtp<-canpath_mtp[canpath_mtp$race != "pnts",] #remove race who prefer not to say

#categorize sampledate into two months bins
canpath_mtp$month<-floor_date(as.Date(canpath_mtp$C1_ADM_COLLECT_DATE),
                               unit = "2 months")
can_dfm<-canpath_mtp %>% 
  select(ResearcherID,age_groups,sex,urban,race,province,month) #no NAs

#write_csv(can_dfm,"can_dfmtp.csv")

# Load packages and data -----------------------------------------------------------
library(ggplot2)
library(data.table)
library(viridis)
library(tidyverse)
library(colorspace)
#CBS - extracted Nov 6 2023
cbs_asr<-read.csv("1_data/CBS/cbs_asr_nov62023_count.csv")
cbs_asu<-read.csv("1_data/CBS/cbs_asu_nov62023_count.csv")

#APL - extracted Nov 7 2023
apl_asu<-read.csv("1_data/APL/apl_asu_nov72023_count.csv")

#Ab-c - extracted Nov 8 2023
abc_asu<-read.csv("1_data/Ab-c/abc_asu_nov82023_count.csv")
abc_asr<-read.csv("1_data/Ab-c/abc_asr_nov82023_count.csv")

#2021 census data counts
#Df with urban variable
censusu<-read.csv("1_data/2021 Canadian Census/census_w_final_counts_urban.csv")
colnames(censusu)<-c("province","quintmat","quintsoc","age_groups",
                     "sex","urban","count_census")
censusu18<-censusu %>% filter(age_groups != "< 18 years")
censusualb<-censusu %>% filter(province == "AB")

#Df with race variable
censusr<-read.csv("1_data/2021 Canadian Census/census_w_final_counts_race.csv")
colnames(censusr)<-c("province","quintmat","quintsoc","age_groups",
                     "sex","race","count_census")
censusr18<-censusr %>% filter(age_groups != "< 18 years")
censusralb<-censusr %>% filter(province == "AB")

#Prepare data for plotting
#- Age-sex-urban datasets -
cbs_groupasu<-expand.grid(age_groups = unique(cbs_asu$age_groups),sex = unique(cbs_asu$sex),
                       urban = unique(cbs_asu$urban))
cbs_dtasu<-setDT(merge(cbs_groupasu,cbs_asu[,c("age_groups","sex","urban","count")],
                    by = c("age_groups","sex","urban"),all.x = TRUE))

apl_groupasu<-expand.grid(age_groups = unique(apl_asu$age_groups),sex = unique(apl_asu$sex),
                       urban = unique(apl_asu$urban))

apl_dtasu<-setDT(merge(apl_groupasu,apl_asu[,c("age_groups","sex","urban","count")],
                    by = c("age_groups","sex","urban"),all.x = TRUE))

abc_groupasu<-expand.grid(age_groups = unique(abc_asu$age_groups),sex = unique(abc_asu$sex),
                       urban = unique(abc_asu$urban))

abc_dtasu<-setDT(merge(abc_groupasu,abc_asu[,c("age_groups","sex","urban","count")],
                    by = c("age_groups","sex","urban"),all.x = TRUE))

#- Age-sex-race datasets - 
cbs_groupasr<-expand.grid(age_groups = unique(cbs_asr$age_groups),sex = unique(cbs_asr$sex),
                       race = unique(cbs_asr$race))
cbs_dtasr<-setDT(merge(cbs_groupasr,cbs_asr[,c("age_groups","sex","race","count")],
                    by = c("age_groups","sex","race"),all.x = TRUE))

abc_groupasr<-expand.grid(age_groups = unique(abc_asr$age_groups),sex = unique(abc_asr$sex),
                          race = unique(abc_asr$race))
abc_dtasr<-setDT(merge(abc_groupasr,abc_asr[,c("age_groups","sex","race","count")],
                       by = c("age_groups","sex","race"),all.x = TRUE))

# Generate heatmaps -------------------------------------------------------
# -Set XXXX: Age-sex-urban -
# -- Raw counts --
ggplot(cbs_dtasu,aes(x = sex, y = age_groups, fill = count))+
  geom_tile()+
  facet_grid(rows = vars(urban))+
  scale_fill_gradientn(colors = turbo(10))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

ggplot(apl_dtasu,aes(x = sex, y = age_groups, fill = count))+
  geom_tile()+
  facet_grid(rows = vars(urban))+
  scale_fill_gradientn(colors = turbo(10))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

ggplot(abc_dtasu,aes(x = sex, y = age_groups, fill = count))+
  geom_tile()+
  facet_grid(rows = vars(urban))+
  scale_fill_gradientn(colors = turbo(10))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

#Plot all datasets in single plot
cbs_dtasu$cohort<-"Blood Donor"
apl_dtasu$cohort<-"Outpatient Laboratory"
abc_dtasu$cohort<-"Ab-c probabilistic survey"
all_dtasu<-do.call("rbind",list(cbs_dtasu,apl_dtasu,abc_dtasu))

ggplot(all_dtasu,aes(x = sex, y = age_groups, fill = count))+
  geom_tile()+
  facet_grid(rows = vars(urban),cols = vars(cohort))+
  scale_fill_gradientn(colors = turbo(10))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

# -- Heatmap with representation ratios -- 
#Calculate age-sex-urban census counts
censusu_asu18<-censusu18 %>% #to calculate rep ratio for cohorts with 18+
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)

censusu_asu<-censusu %>% #to calculate rep ratio for cohorts with all ages (APL and CCAHS)
  mutate(age_groups = ifelse(age_groups == "< 18 years",
                            "0-17 years",
                            age_groups),
         sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                   urban == "1" ~ "Urban")) %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)

censusu_asualb<-censusualb %>% #to calculate rep ratio for APL (use census counts only from AB)
  mutate(age_groups = ifelse(age_groups == "< 18 years",
                             "0-17 years",
                             age_groups),
         sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         urban = case_when(urban == "0" ~ "Rural",
                           urban == "1" ~ "Urban")) %>% 
  aggregate(count_census ~ age_groups + sex + urban,
            FUN = sum,
            drop = F)

#---CBS representation ratio---
cbspop_count<-merge(cbs_asu,censusu_asu18,by = c("age_groups","sex","urban"))
cbspop_count$pct_cbs<-cbspop_count$count/sum(cbspop_count$count) #percentage of total CBS samples in each subgroup
cbspop_count$pct_pop<-cbspop_count$count_census/sum(cbspop_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
cbspop_count$rep_ratio<-round(cbspop_count$pct_cbs / cbspop_count$pct_pop,2)
cbspop_count$rr_binned<-case_when(
  cbspop_count$rep_ratio <= 0.25 ~ "RR \u2264 0.25",
  cbspop_count$rep_ratio > 0.25 & cbspop_count$rep_ratio <= 0.75 ~  "0.25 < RR \u2264 0.75",
  cbspop_count$rep_ratio > 0.75 & cbspop_count$rep_ratio <= 1.25 ~ "0.75 < RR \u2264 1.25",
  cbspop_count$rep_ratio > 1.25 & cbspop_count$rep_ratio < 2.00 ~  "1.25 < RR \u2264 2.00",
  cbspop_count$rep_ratio > 2.00 ~ "RR > 2.00"
)

#---APL representation ratio---
aplpop_count<-merge(apl_asu,censusu_asualb,by = c("age_groups","sex","urban"))
aplpop_count$pct_apl<-aplpop_count$count/sum(aplpop_count$count) #percentage of total APL samples in each subgroup
aplpop_count$pct_pop<-aplpop_count$count_census/sum(aplpop_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates apl sample adequately represents the corresponding population subgroup
aplpop_count$rep_ratio<-round(aplpop_count$pct_apl / aplpop_count$pct_pop,2)
aplpop_count$rr_binned<-case_when(
  aplpop_count$rep_ratio <= 0.25 ~ "RR \u2264 0.25",
  aplpop_count$rep_ratio > 0.25 & aplpop_count$rep_ratio <= 0.75 ~ "0.25 < RR \u2264 0.75",
  aplpop_count$rep_ratio > 0.75 & aplpop_count$rep_ratio <= 1.25 ~ "0.75 < RR \u2264 1.25",
  aplpop_count$rep_ratio > 1.25 & aplpop_count$rep_ratio <= 2.00 ~ "1.25 < RR \u2264 2.00",
  aplpop_count$rep_ratio > 2.00 ~ "RR > 2.00"
)

#---Ab-c representation ratio---
abcpop_count<-merge(abc_asu,censusu_asu18,by = c("age_groups","sex","urban"))
abcpop_count$pct_abc<-abcpop_count$count/sum(abcpop_count$count) #percentage of total Ab-c samples in each subgroup
abcpop_count$pct_pop<-abcpop_count$count_census/sum(abcpop_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpop_count$rep_ratio<-round(abcpop_count$pct_abc / abcpop_count$pct_pop,2)
abcpop_count$rr_binned<-case_when(
  abcpop_count$rep_ratio <= 0.25 ~ "RR \u2264 0.25",
  abcpop_count$rep_ratio > 0.25 & abcpop_count$rep_ratio <= 0.75 ~ "0.25 < RR \u2264 0.75",
  abcpop_count$rep_ratio > 0.75 & abcpop_count$rep_ratio <= 1.25 ~ "0.75 < RR \u2264 1.25",
  abcpop_count$rep_ratio > 1.25 & abcpop_count$rep_ratio <= 2.00 ~ "1.25 < RR \u2264 2.00",
  abcpop_count$rep_ratio > 2.00 ~ "RR > 2.00"
)

#-- All datasets in 1 plot --
cbspop_count$cohort<-"Blood Donor"
aplpop_count$cohort<-"Outpatient Laboratory"
abcpop_count$cohort<-"Ab-c probabilistic survey"
allpop_countasu<-do.call("rbind",list(cbspop_count[,-c(6:7)],
                                      aplpop_count[,-c(6:7)],
                                      abcpop_count[,-c(6:7)]))

cbspop_count$cohort<-"Blood Donor"
aplpop_count$cohort<-"Outpatient Laboratory"
abcpop_count$cohort<-"Ab-c probabilistic survey"
allpop_countasu<-do.call("rbind",list(cbspop_count[,-c(6:7)],
                                      aplpop_count[,-c(6:7)],
                                      abcpop_count[,-c(6:7)]))

cols<-c("#FFC5D0","#009ADE","#A8E1BF","#50A315","#E16A86")
ggplot(allpop_countasu,aes(x = sex,y = age_groups, fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = cols,breaks = c("RR \u2264 0.25",
                                             "0.25 < RR \u2264 0.75",
                                             "0.75 < RR \u2264 1.25",
                                             "1.25 < RR \u2264 2.00",
                                             "RR > 2.00"))+
  geom_text(aes(label = rep_ratio),color = "black",size = 3.0)+
  facet_grid(rows = vars(urban),cols = vars(cohort))+
  labs(fill = "Representativeness ratio (RR)",
       x = "Sex",
       y = "Age group")

# - Set XXXX: Age-sex-race -
# -- Raw counts --
ggplot(cbs_dtasr,aes(x = sex, y = age_groups, fill = count))+
  geom_tile()+
  facet_grid(rows = vars(race))+
  scale_fill_gradientn(colors = turbo(10))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

ggplot(abc_dtasr,aes(x = sex, y = age_groups, fill = count))+
  geom_tile()+
  facet_grid(rows = vars(race))+
  scale_fill_gradientn(colors = turbo(10))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

#Plot all datasets in single plot
cbs_dtasr$cohort<-"Blood Donor"
abc_dtasr$cohort<-"Ab-c probabilistic survey"
all_dtasr<-rbind(cbs_dtasr,abc_dtasr)
all_dtasr<-all_dtasr[order(all_dtasr$count),]
all_dtasr$c_binned<-case_when(
  all_dtasr$count < 1000 ~ "< 1,000",
  all_dtasr$count >=1000 & all_dtasr$count <=5000 ~ "1,000 - 4,999",
  all_dtasr$count > 5000 & all_dtasr$count <=10000 ~ "5,000 - 9,999",
  all_dtasr$count > 10000 & all_dtasr$count <= 20000 ~ "10,000 - 19,999",
  all_dtasr$count > 20000 & all_dtasr$count <= 50000 ~ "20,000- 49,999",
  all_dtasr$count > 50000 & all_dtasr$count <= 100000 ~ "50,000 - 99,999",
  all_dtasr$count > 100000 ~ "100,000 - 200,000"
)

#cols<-rev(diverging_hcl(7,"Blue-Red"))
cols<-c("grey90","#E16A86","#FFC5D0","#A4DDEF","#009ADE","#A8E1BF","#50A315")
ggplot(all_dtasr,aes(x = sex, y = age_groups, fill = c_binned))+
  geom_tile()+
  facet_grid(rows = vars(race),cols = vars(cohort))+
  scale_fill_manual(values = cols,breaks = c("< 1,000","1,000 - 4,999",
                                                "5,000 - 9,999","10,000 - 19,999",
                                                "20,000- 49,999","50,000 - 99,999",
                                                "100,000 - 200,000"))+
  geom_text(aes(label = count),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")
  #theme(
   # panel.background = element_rect(fill = "white",
    #                                colour = "grey80",
     #                               linewidth = 0.5,linetype = "solid"),
    #strip.background = element_rect(fill = "grey80"),
    #strip.text = element_text(color = "black",size = 10)
  #)

# -- Heatmap with representation ratio --
#Calculate age-sex-race census counts
censusr_asr18<-censusr18 %>% #to calculate rep ratio for cohorts with 18+
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male"),
         race = case_when(race == "0" ~ "Visible minority",
                          race == "1" ~ "White")) %>% 
  aggregate(count_census ~ age_groups + sex + race,
            FUN = sum,
            drop = F)

# --- CBS representation ratio ---
cbspopr_count<-merge(cbs_asr,censusr_asr18,by = c("age_groups","sex","race"))
cbspopr_count$pct_cbs<-cbspopr_count$count/sum(cbspopr_count$count) #percentage of total CBS samples in each subgroup
cbspopr_count$pct_pop<-cbspopr_count$count_census/sum(cbspopr_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
cbspopr_count$rep_ratio<-round(cbspopr_count$pct_cbs / cbspopr_count$pct_pop,2)
cbspopr_count$rr_binned<-case_when(
  cbspopr_count$rep_ratio <= 0.25 ~ "RR \u2264 0.25",
  cbspopr_count$rep_ratio > 0.25 & cbspopr_count$rep_ratio <= 0.75 ~  "0.25 < RR \u2264 0.75",
  cbspopr_count$rep_ratio > 0.75 & cbspopr_count$rep_ratio <= 1.25 ~ "0.75 < RR \u2264 1.25",
  cbspopr_count$rep_ratio > 1.25 & cbspopr_count$rep_ratio < 2.00 ~  "1.25 < RR \u2264 2.00",
  cbspopr_count$rep_ratio > 2.00 ~ "RR > 2.00"
)

# --- Ab-c representation ratio ---
abcpopr_count<-merge(abc_asr,censusr_asr18,by = c("age_groups","sex","race"))
abcpopr_count$pct_abc<-abcpopr_count$count/sum(abcpopr_count$count) #percentage of total Ab-c samples in each subgroup
abcpopr_count$pct_pop<-abcpopr_count$count_census/sum(abcpopr_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpopr_count$rep_ratio<-round(abcpopr_count$pct_abc / abcpopr_count$pct_pop,2)
abcpopr_count$rr_binned<-case_when(
  abcpopr_count$rep_ratio <= 0.25 ~ "RR \u2264 0.25",
  abcpopr_count$rep_ratio > 0.25 & abcpopr_count$rep_ratio <= 0.75 ~  "0.25 < RR \u2264 0.75",
  abcpopr_count$rep_ratio > 0.75 & abcpopr_count$rep_ratio <= 1.25 ~ "0.75 < RR \u2264 1.25",
  abcpopr_count$rep_ratio > 1.25 & abcpopr_count$rep_ratio < 2.00 ~  "1.25 < RR \u2264 2.00",
  abcpopr_count$rep_ratio > 2.00 ~ "RR > 2.00"
)

#Plot datasets in a single plot
cbspopr_count$cohort<-"Blood Donor"
abcpopr_count$cohort<-"Ab-c probabilistic survey"
allpopr_count<-rbind(cbspopr_count[,-c(6,7)],abcpopr_count[,-c(6,7)])

cols<-c("#FFC5D0","#009ADE","#A8E1BF","#E16A86","#50A315")
ggplot(allpopr_count,aes(x = sex,y = age_groups, fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = cols,breaks = c("RR \u2264 0.25",
                                             "0.25 < RR \u2264 0.75",
                                             "0.75 < RR \u2264 1.25",
                                             "1.25 < RR \u2264 2.00",
                                             "RR > 2.00"))+
  geom_text(aes(label = rep_ratio),color = "black",size = 3.0)+
  facet_grid(rows = vars(race),cols = vars(cohort))+
  labs(fill = "Representativeness",
       x = "Sex",
       y = "Age group",
       caption = " Ab-c and CBS RR calculated using counts from adults")


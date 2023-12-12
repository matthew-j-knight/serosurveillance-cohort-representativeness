# Load packages and data -----------------------------------------------------------
setwd("~/serosurveillance-cohort-representativeness/1_data/private") #remove final pub
library(ggplot2)
library(data.table)
library(viridis)
library(tidyverse)
library(colorspace)
library(cowplot)

#CBS - extracted Nov 6 2023
cbs_asr<-read.csv("./cbs_asr_nov62023_count.csv")
cbs_asu<-read.csv("./cbs_asu_nov62023_count.csv")
cbs_asq<-read.csv("./cbs_asq_dec62023_count.csv")

#APL - extracted Nov 7 2023
apl_asu<-read.csv("./apl_asu_nov72023_count.csv")

#Ab-c - extracted Nov 8 2023
abc_asu<-read.csv("./abc_asu_nov82023_count.csv")
abc_asr<-read.csv("./abc_asr_nov82023_count.csv")

#CLSA - extracted Nov 20 2023
clsa_asu<-read.csv("./clsa_asu_nov202023_count.csv")
clsa_asr<-read.csv("./clsa_asr_nov202023_count.csv")

#Canpath - extracted Dec 10 2023
can_asu<-read.csv("./can_asu_dec72023_count.csv")
can_asr<-read.csv("./can_asr_dec72023_count.csv")

#2021 census data counts
#Df with urban variable
censusu<-read.csv("./2021 Canadian Census/census_w_final_counts_urban.csv")
colnames(censusu)<-c("province","quintmat","quintsoc","age_groups",
                     "sex","urban","count_census")
censusu18<-censusu %>% filter(age_groups != "< 18 years")
censusualb<-censusu %>% filter(province == "AB")

#Df with race variable
censusr<-read.csv("./2021 Canadian Census/census_w_final_counts_race.csv")
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

clsa_groupasu<-expand.grid(age_groups = unique(clsa_asu$age_groups),sex = unique(clsa_asu$sex),
                          urban = unique(clsa_asu$urban))

clsa_dtasu<-setDT(merge(clsa_groupasu,clsa_asu[,c("age_groups","sex","urban","count")],
                       by = c("age_groups","sex","urban"),all.x = TRUE))

can_groupasu<-expand.grid(age_groups = unique(can_asu$age_groups),sex = unique(can_asu$sex),
                           urban = unique(can_asu$urban))

can_dtasu<-setDT(merge(can_groupasu,can_asu[,c("age_groups","sex","urban","count")],
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

clsa_groupasr<-expand.grid(age_groups = unique(clsa_asr$age_groups),sex = unique(clsa_asr$sex),
                          race = unique(clsa_asr$race))
clsa_dtasr<-setDT(merge(clsa_groupasr,clsa_asr[,c("age_groups","sex","race","count")],
                       by = c("age_groups","sex","race"),all.x = TRUE))

can_groupasr<-expand.grid(age_groups = unique(can_asr$age_groups),sex = unique(can_asr$sex),
                           race = unique(can_asr$race))
can_dtasr<-setDT(merge(can_groupasr,can_asr[,c("age_groups","sex","race","count")],
                        by = c("age_groups","sex","race"),all.x = TRUE))

#Age-sex-quintmat datasets
cbs_groupasq<-expand.grid(sex = unique(cbs_asq$sex),
                          quintmat = unique(cbs_asq$quintmat))
cbs_dtasq<-setDT(merge(cbs_groupasq,cbs_asq[,c("sex","quintmat","count")],
                       by = c("sex","quintmat"),all.x = TRUE))
# Generate heatmaps -------------------------------------------------------
# -Set XXXX: Age-sex-urban -
# -- Raw counts --

#Calculate proportion each subgroup contributes to total dataset
cbs_dtasu$pct<-round(cbs_dtasu$count / sum(cbs_dtasu$count),3)

#Calculate proportion each subgroup contributes to total dataset
apl_dtasu$pct<-round(apl_dtasu$count / sum(apl_dtasu$count),3)

#Calculate proportion each subgroup contributes to total dataset
abc_dtasu$pct<-round(abc_dtasu$count / sum(abc_dtasu$count),3)

#Calculate proportion each subgroup contributes to total dataset
clsa_dtasu$pct<-round(clsa_dtasu$count / sum(clsa_dtasu$count),3)

#Calculate proportion each subgroup contributes to total dataset
can_dtasu$pct<-round(can_dtasu$count / sum(can_dtasu$count),3)

#Plot all datasets in single plot
cbs_dtasu$cohort<-"Blood Donor"
apl_dtasu$cohort<-"Outpatient Laboratory"
abc_dtasu$cohort<-"Ab-c probabilistic survey"
clsa_dtasu$cohort<-"CLSA probabilistic survey"
can_dtasu$cohort<-"Canpath probabilistic survey"
all_dtasu<-do.call("rbind",list(cbs_dtasu,apl_dtasu,abc_dtasu,clsa_dtasu,can_dtasu))

#create count binned for visualization
all_dtasu$count_binned<-factor(case_when(
  all_dtasu$count <= 1000 ~ "Count \u2264 1000",
  all_dtasu$count > 1000 & all_dtasu$count <= 5000 ~ "1000 < Count \u2264 5000",
  all_dtasu$count > 5000 & all_dtasu$count <= 10000 ~ "5000 < Count \u2264 10000",
  all_dtasu$count > 10000 & all_dtasu$count <= 20000 ~ "10000 < Count \u2264 20000",
  all_dtasu$count > 20000 & all_dtasu$count <= 50000 ~ "20000 < Count \u2264 50000",
  all_dtasu$count > 50000 & all_dtasu$count <= 100000 ~ "50000 < Count \u2264 100000",
  all_dtasu$count > 100000 ~ "Count > 100000"),
  levels = c("Count \u2264 1000",
             "1000 < Count \u2264 5000",
             "5000 < Count \u2264 10000",
             "10000 < Count \u2264 20000",
             "20000 < Count \u2264 50000",
             "50000 < Count \u2264 100000",
             "Count > 100000"))

#Create pct binned
all_dtasu$pct_binned<-factor(case_when(
  all_dtasu$pct <= 0.025 ~ "Proportion \u2264 2.5%",
  all_dtasu$pct > 0.025 & all_dtasu$pct <= 0.05 ~ "2.5% < Proportion \u2264 5.0%",
  all_dtasu$pct > 0.05 & all_dtasu$pct <= 0.10 ~ "5.0% < Proportion \u2264 10%",
  all_dtasu$pct > 0.10 & all_dtasu$pct <= 0.15 ~ "10% < Proportion \u2264 15%",
  all_dtasu$pct > 0.15 & all_dtasu$pct <= 0.20 ~ "15% < Proportion \u2264 20%",
  all_dtasu$pct > 0.20 ~ "Proportion > 20%"),
  levels = c("Proportion \u2264 2.5%",
             "2.5% < Proportion \u2264 5.0%",
             "5.0% < Proportion \u2264 10%",
             "10% < Proportion \u2264 15%",
             "15% < Proportion \u2264 20%",
             "Proportion > 20%"))

ggplot(all_dtasu,aes(x = sex, y = factor(age_groups,
                                         levels = c("0-17 years",
                                                    "18-39 years",
                                                    "40-54 years",
                                                    "55+ years")), 
                                         fill = count_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(rows = vars(urban),cols = vars(cohort))+
  geom_text(aes(label = count),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

ggplot(all_dtasu,aes(x = sex, y = factor(age_groups,
                                         levels = c("0-17 years",
                                                    "18-39 years",
                                                    "40-54 years",
                                                    "55+ years")), 
                     fill = pct_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(rows = vars(urban),cols = vars(cohort))+
  geom_text(aes(label = pct),color = "black",size = 3.0)+
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

censusu_asu<-censusu %>% #to calculate rep ratio for national cohorts with all ages (CCAHS)
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
  cbspop_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  cbspop_count$rep_ratio >= 1/2 & 
    cbspop_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  cbspop_count$rep_ratio >= 3/4 & 
    cbspop_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  cbspop_count$rep_ratio > 4/3 & 
    cbspop_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  cbspop_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

#---APL representation ratio---
aplpop_count<-merge(apl_asu,censusu_asualb,by = c("age_groups","sex","urban"))
aplpop_count$pct_apl<-aplpop_count$count/sum(aplpop_count$count) #percentage of total APL samples in each subgroup
aplpop_count$pct_pop<-aplpop_count$count_census/sum(aplpop_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates apl sample adequately represents the corresponding population subgroup
aplpop_count$rep_ratio<-round(aplpop_count$pct_apl / aplpop_count$pct_pop,2)
aplpop_count$rr_binned<-case_when(
  aplpop_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  aplpop_count$rep_ratio >= 1/2 & 
    aplpop_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  aplpop_count$rep_ratio >= 3/4 & 
    aplpop_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  aplpop_count$rep_ratio > 4/3 & 
    aplpop_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  aplpop_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

#---Ab-c representation ratio---
abcpop_count<-merge(abc_asu,censusu_asu18,by = c("age_groups","sex","urban"))
abcpop_count$pct_abc<-abcpop_count$count/sum(abcpop_count$count) #percentage of total Ab-c samples in each subgroup
abcpop_count$pct_pop<-abcpop_count$count_census/sum(abcpop_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpop_count$rep_ratio<-round(abcpop_count$pct_abc / abcpop_count$pct_pop,2)
abcpop_count$rr_binned<-case_when(
  abcpop_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  abcpop_count$rep_ratio >= 1/2 & 
    abcpop_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  abcpop_count$rep_ratio >= 3/4 & 
    abcpop_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  abcpop_count$rep_ratio > 4/3 & 
    abcpop_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  abcpop_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

#-- CLSA representation ratio --
clsapop_count<-merge(clsa_asu,censusu_asu18,by = c("age_groups","sex","urban"))
clsapop_count$pct_clsa<-clsapop_count$count/sum(clsapop_count$count) #percentage of total clsa samples in each subgroup
clsapop_count$pct_pop<-clsapop_count$count_census/sum(clsapop_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
clsapop_count$rep_ratio<-round(clsapop_count$pct_clsa / clsapop_count$pct_pop,2)
clsapop_count$rr_binned<-case_when(
  clsapop_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  clsapop_count$rep_ratio >= 1/2 & 
    clsapop_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  clsapop_count$rep_ratio >= 3/4 & 
    clsapop_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  clsapop_count$rep_ratio > 4/3 & 
    clsapop_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  clsapop_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

#-- Canpath representation ratio --
canpop_count<-merge(can_asu,censusu_asu18,by = c("age_groups","sex","urban"))
canpop_count$pct_can<-canpop_count$count/sum(canpop_count$count) #percentage of total Canpath samples in each subgroup
canpop_count$pct_pop<-canpop_count$count_census/sum(canpop_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates Canpath sample adequately represents the corresponding population subgroup
canpop_count$rep_ratio<-round(canpop_count$pct_can / canpop_count$pct_pop,2)
canpop_count$rr_binned<-case_when(
  canpop_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  canpop_count$rep_ratio >= 1/2 & 
    canpop_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  canpop_count$rep_ratio >= 3/4 & 
    canpop_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  canpop_count$rep_ratio > 4/3 & 
    canpop_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  canpop_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

#-- All datasets in 1 plot --
cbspop_count$cohort<-"Blood Donor"
aplpop_count$cohort<-"Outpatient Laboratory"
abcpop_count$cohort<-"Ab-c probabilistic survey"
clsapop_count$cohort<-"CLSA probabilistic survey"
canpop_count$cohort<-"Canpath probabilistic survey"
allpop_countasu<-do.call("rbind",list(cbspop_count[,-c(6:7)],
                                      aplpop_count[,-c(6:7)],
                                      abcpop_count[,-c(6:7)],
                                      clsapop_count[,-c(6:7)],
                                      canpop_count[,-c(6:7)]))
allpop_countasu$rr_binned<-factor(allpop_countasu$rr_binned,
                                  levels = c( "Strongly underrepresented (RR < 1/2)",
                                              "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                              "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                              "Moderately overrepresented (4/3 < RR ≤ 2)",
                                              "Strongly overrepresented (RR > 2)"))


cols<-c("#E16A86","#FFC5D0","#009ADE","#A8E1BF","#50A315")
ggplot(allpop_countasu,aes(x = sex,y = factor(age_groups,
                                              levels = c("0-17 years",
                                                         "18-39 years",
                                                         "40-54 years",
                                                         "55+ years")),
                           fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = cols)+
  geom_text(aes(label = rep_ratio),color = "black",size = 3.0)+
  facet_grid(rows = vars(urban),cols = vars(cohort))+
  labs(fill = "Representativeness ratio (RR)",
       x = "Sex",
       y = "Age group")

# - Set XXXX: Age-sex-race -

#Calculate proportion each subgroup contributes to total dataset
cbs_dtasr$pct<-round(cbs_dtasr$count / sum(cbs_dtasr$count),3)

#Calculate proportion each subgroup contributes to total dataset
abc_dtasr$pct<-round(abc_dtasr$count / sum(abc_dtasr$count),3)

#Calculate proportion each subgroup contributes to total dataset
clsa_dtasr$pct<-round(clsa_dtasr$count / sum(clsa_dtasr$count),3)

#Calculate proportion each subgroup contributes to total dataset
can_dtasr$pct<-round(can_dtasr$count / sum(can_dtasr$count),3)

#Plot all datasets in single plot
cbs_dtasr$cohort<-"Blood Donor"
abc_dtasr$cohort<-"Ab-c probabilistic survey"
clsa_dtasr$cohort<-"CLSA probabilistic survey"
can_dtasr$cohort<-"Canpath probabilistic survey"
all_dtasr<-do.call("rbind",list(cbs_dtasr,abc_dtasr,clsa_dtasr,can_dtasr))
all_dtasr$c_binned<-factor(case_when(
  all_dtasr$count <= 1000 ~ "Count \u2264 1000",
  all_dtasr$count > 1000 & all_dtasr$count <= 5000 ~ "1000 < Count \u2264 5000",
  all_dtasr$count > 5000 & all_dtasr$count <= 10000 ~ "5000 < Count \u2264 10000",
  all_dtasr$count > 10000 & all_dtasr$count <= 20000 ~ "10000 < Count \u2264 20000",
  all_dtasr$count > 20000 & all_dtasr$count <= 50000 ~ "20000 < Count \u2264 50000",
  all_dtasr$count > 50000 & all_dtasr$count <= 100000 ~ "50000 < Count \u2264 100000",
  all_dtasr$count > 100000 ~ "Count > 100000"),
  levels = c("Count \u2264 1000",
             "1000 < Count \u2264 5000",
             "5000 < Count \u2264 10000",
             "10000 < Count \u2264 20000",
             "20000 < Count \u2264 50000",
             "50000 < Count \u2264 100000",
             "Count > 100000"))

ggplot(all_dtasr,aes(x = sex, y = factor(age_groups,
                                         levels = c("0-17 years",
                                                    "18-39 years",
                                                    "40-54 years",
                                                    "55+ years")),
                     fill = c_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(rows = vars(race),cols = vars(cohort))+
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

#Plot heatmap with proportion of total specimens each subgroup accounts for
all_dtasr$pct_bin<-factor(case_when(
  all_dtasr$pct <= 0.025 ~ "Proportion \u2264 2.5%",
  all_dtasr$pct > 0.025 & all_dtasr$pct <= 0.05 ~ "2.5% < Proportion \u2264 5.0%",
  all_dtasr$pct > 0.05 & all_dtasr$pct <= 0.10 ~ "5.0% < Proportion \u2264 10%",
  all_dtasr$pct > 0.10 & all_dtasr$pct <= 0.15 ~ "10% < Proportion \u2264 15%",
  all_dtasr$pct > 0.15 ~ "Proportion > 15%"),
  levels = c("Proportion \u2264 2.5%",
             "2.5% < Proportion \u2264 5.0%",
             "5.0% < Proportion \u2264 10%",
             "10% < Proportion \u2264 15%",
             "Proportion > 15%"))

ggplot(all_dtasr,aes(x = sex, y = factor(age_groups,
                                         levels = c("0-17 years",
                                                    "18-39 years",
                                                    "40-54 years",
                                                    "55+ years")), 
                     fill = pct_bin))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(rows = vars(race),cols = vars(cohort))+
  geom_text(aes(label = pct),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

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
  cbspopr_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  cbspopr_count$rep_ratio >= 1/2 & 
    cbspopr_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  cbspopr_count$rep_ratio >= 3/4 & 
    cbspopr_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  cbspopr_count$rep_ratio > 4/3 & 
    cbspopr_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  cbspopr_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

# --- Ab-c representation ratio ---
abcpopr_count<-merge(abc_asr,censusr_asr18,by = c("age_groups","sex","race"))
abcpopr_count$pct_abc<-abcpopr_count$count/sum(abcpopr_count$count) #percentage of total Ab-c samples in each subgroup
abcpopr_count$pct_pop<-abcpopr_count$count_census/sum(abcpopr_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpopr_count$rep_ratio<-round(abcpopr_count$pct_abc / abcpopr_count$pct_pop,2)
abcpopr_count$rr_binned<-case_when(
  abcpopr_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  abcpopr_count$rep_ratio >= 1/2 & 
    abcpopr_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  abcpopr_count$rep_ratio >= 3/4 & 
    abcpopr_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  abcpopr_count$rep_ratio > 4/3 & 
    abcpopr_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  abcpopr_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

# --- CLSA representation ratio ---
clsapopr_count<-merge(clsa_asr,censusr_asr18,by = c("age_groups","sex","race"))
clsapopr_count$pct_clsa<-clsapopr_count$count/sum(clsapopr_count$count) #percentage of total clsa samples in each subgroup
clsapopr_count$pct_pop<-clsapopr_count$count_census/sum(clsapopr_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates clsa sample adequately represents the corresponding population subgroup
clsapopr_count$rep_ratio<-round(clsapopr_count$pct_clsa / clsapopr_count$pct_pop,2)
clsapopr_count$rr_binned<-case_when(
  clsapopr_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  clsapopr_count$rep_ratio >= 1/2 & 
    clsapopr_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  clsapopr_count$rep_ratio >= 3/4 & 
    clsapopr_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  clsapopr_count$rep_ratio > 4/3 & 
    clsapopr_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  clsapopr_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

# --- Canpath representation ratio ---
canpopr_count<-merge(can_asr,censusr_asr18,by = c("age_groups","sex","race"))
canpopr_count$pct_can<-canpopr_count$count/sum(canpopr_count$count) #percentage of total Canpath samples in each subgroup
canpopr_count$pct_pop<-canpopr_count$count_census/sum(canpopr_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates Canpath sample adequately represents the corresponding population subgroup
canpopr_count$rep_ratio<-round(canpopr_count$pct_can / canpopr_count$pct_pop,2)
canpopr_count$rr_binned<-case_when(
  canpopr_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  canpopr_count$rep_ratio >= 1/2 & 
    canpopr_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  canpopr_count$rep_ratio >= 3/4 & 
    canpopr_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  canpopr_count$rep_ratio > 4/3 & 
    canpopr_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  canpopr_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

#Plot datasets in a single plot
cbspopr_count$cohort<-"Blood Donor"
abcpopr_count$cohort<-"Ab-c probabilistic survey"
clsapopr_count$cohort<-"CLSA probabilistic survey"
canpopr_count$cohort<-"Canpath probabilistic survey"
allpopr_count<-do.call("rbind",list(cbspopr_count[,-c(6,7)],
                                    abcpopr_count[,-c(6,7)],
                                    clsapopr_count[,-c(6,7)],
                                    canpopr_count[,-c(6,7)]))

allpopr_count$rr_binned<-factor(allpopr_count$rr_binned,
                                  levels = c( "Strongly underrepresented (RR < 1/2)",
                                              "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                              "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                              "Moderately overrepresented (4/3 < RR ≤ 2)",
                                              "Strongly overrepresented (RR > 2)"))

cols<-c("#E16A86","#FFC5D0","#009ADE","#A8E1BF","#50A315")

ggplot(allpopr_count,aes(x = sex,y = factor(age_groups,
                                            levels = c("0-17 years",
                                                       "18-39 years",
                                                       "40-54 years",
                                                       "55+ years")),
                         fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = cols)+
  geom_text(aes(label = rep_ratio),color = "black",size = 3.0)+
  facet_grid(rows = vars(race),cols = vars(cohort))+
  labs(fill = "Representativeness",
       x = "Sex",
       y = "Age group")

# - Set XXXX: Age-sex-quintmat -

#Calculate proportion each subgroup contributes to total dataset
cbs_dtasq$pct<-round(cbs_dtasq$count / sum(cbs_dtasq$count),3)

#Plot all datasets in a single plot
cbs_dtasq$cohort<-"Blood donor"
all_dtasq<-do.call("rbind",list(cbs_dtasq))

#Create count binned for visualization
all_dtasq$c_binned<-factor(case_when(
  all_dtasq$count <= 1000 ~ "Count \u2264 1000",
  all_dtasq$count > 1000 & all_dtasq$count <= 5000 ~ "1000 < Count \u2264 5000",
  all_dtasq$count > 5000 & all_dtasq$count <= 10000 ~ "5000 < Count \u2264 10000",
  all_dtasq$count > 10000 & all_dtasq$count <= 20000 ~ "10000 < Count \u2264 20000",
  all_dtasq$count > 20000 & all_dtasq$count <= 50000 ~ "20000 < Count \u2264 50000",
  all_dtasq$count > 50000 & all_dtasq$count <= 100000 ~ "50000 < Count \u2264 100000",
  all_dtasq$count > 100000 ~ "Count > 100000"),
  levels = c("Count \u2264 1000",
             "1000 < Count \u2264 5000",
             "5000 < Count \u2264 10000",
             "10000 < Count \u2264 20000",
             "20000 < Count \u2264 50000",
             "50000 < Count \u2264 100000",
             "Count > 100000"))

ggplot(all_dtasq,aes(x = sex, y = factor(quintmat,
                                         levels = c(1,2,3,
                                                    4,5)),
                     fill = c_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = count),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Material deprivation quintile",
       x = "Sex")

#Plot heatmap with proportion of total specimens each subgroup accounts for
all_dtasq$pct_bin<-factor(case_when(
  all_dtasq$pct <= 0.025 ~ "Proportion \u2264 2.5%",
  all_dtasq$pct > 0.025 & all_dtasq$pct <= 0.05 ~ "2.5% < Proportion \u2264 5.0%",
  all_dtasq$pct > 0.05 & all_dtasq$pct <= 0.10 ~ "5.0% < Proportion \u2264 10%",
  all_dtasq$pct > 0.10 & all_dtasq$pct <= 0.15 ~ "10% < Proportion \u2264 15%",
  all_dtasq$pct > 0.15 ~ "Proportion > 15%"),
  levels = c("Proportion \u2264 2.5%",
             "2.5% < Proportion \u2264 5.0%",
             "5.0% < Proportion \u2264 10%",
             "10% < Proportion \u2264 15%",
             "Proportion > 15%"))

ggplot(all_dtasq,aes(x = sex, y = factor(quintmat,
                                         levels = c(1,2,
                                                    3,4,5)), 
                     fill = pct_bin))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = pct),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Material Deprivation quintile",
       x = "Sex")

# -- Heatmap with representation ratio --
#Calculate age-sex-quintmat census counts
censusr_asq18<-censusu18 %>% #to calculate rep ratio for cohorts with 18+
  mutate(sex = case_when(sex == "0" ~ "Female",
                         sex == "1" ~ "Male")) %>% 
  aggregate(count_census ~ sex + quintmat,
            FUN = sum,
            drop = F)

# --- CBS representation ratio ---
cbspopq_count<-merge(cbs_asq,censusr_asq18,by = c("sex","quintmat"))
cbspopq_count$pct_cbs<-cbspopq_count$count/sum(cbspopq_count$count) #percentage of total CBS samples in each subgroup
cbspopq_count$pct_pop<-cbspopq_count$count_census/sum(cbspopq_count$count_census) #percentage of total population in each subgroup

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
cbspopq_count$rep_ratio<-round(cbspopq_count$pct_cbs / cbspopq_count$pct_pop,2)
cbspopq_count$rr_binned<-case_when(
  cbspopq_count$rep_ratio < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
  cbspopq_count$rep_ratio >= 1/2 & 
    cbspopq_count$rep_ratio < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
  cbspopq_count$rep_ratio >= 3/4 & 
    cbspopq_count$rep_ratio <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
  cbspopq_count$rep_ratio > 4/3 & 
    cbspopq_count$rep_ratio <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
  cbspopq_count$rep_ratio > 2.00 ~ "Strongly overrepresented (RR > 2)"
)

#Plot datasets in a single plot
cbspopq_count$cohort<-"Blood Donor"
allpopq_count<-do.call("rbind",list(cbspopq_count[,-c(6)]
                                    ))

allpopq_count$rr_binned<-factor(allpopq_count$rr_binned,
                                  levels = c( "Strongly underrepresented (RR < 1/2)",
                                              "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                              "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                              "Moderately overrepresented (4/3 < RR ≤ 2)",
                                              "Strongly overrepresented (RR > 2)"))

cols<-c("#E16A86","#FFC5D0","#009ADE","#A8E1BF","#50A315")

ggplot(allpopq_count,aes(x = sex,y = factor(quintmat),
                         fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = cols)+
  geom_text(aes(label = rep_ratio),color = "black",size = 3.0)+
  facet_grid(cols = vars(cohort))+
  labs(fill = "Representativeness Ratio",
       x = "Sex",
       y = "Material deprivation quintile")

#- Create figure 1 - 
colnames(allpopr_count)[3]<-"strata"
colnames(allpop_countasu)[3]<-"strata"
colnames(allpopq_count)[3]<-"strata"
f1<-do.call("rbind",list(allpopr_count,allpop_countasu,allpopq_count
                         ))
f1$cohort<-factor(f1$cohort,
                  levels = c("Blood Donor",
                             "Ab-c probabilistic survey",
                             "CLSA probabilistic survey",
                             "Canpath probabilistic survey",
                             "Outpatient Laboratory"))
f1$strata<-factor(f1$strata,
                  levels = c("Rural","Urban","Visible minority",
                             "White","1","2","3","4","5"))
f1$rr_binned<-factor(f1$rr_binned,
                     levels = c("Strongly underrepresented (RR < 1/2)",
                                "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                "Moderately overrepresented (4/3 < RR \u2264 2)",
                                "Strongly overrepresented (RR > 2)"))

cols<-c("#E16A86","#FFC5D0","#009ADE","#A8E1BF","#50A315")

ggplot(f1,aes(x = sex,y = factor(age_groups,
                                 levels = c("0-17 years",
                                            "18-39 years",
                                            "40-54 years",
                                            "55+ years")),
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = cols,breaks = c("Strongly underrepresented (RR < 1/2)",
                                             "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                             "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                             "Moderately overrepresented (4/3 < RR \u2264 2)",
                                             "Strongly overrepresented (RR > 2)"))+
  geom_text(aes(label = rep_ratio),color = "black",size = 3.0)+
  facet_grid(rows = vars(strata),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")




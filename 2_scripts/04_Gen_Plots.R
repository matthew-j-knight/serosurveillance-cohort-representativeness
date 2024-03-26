"This script generates all figures used in the paper."

# Load packages and data -----------------------------------------------------------
setwd("~/serosurveillance-cohort-representativeness")
library(ggplot2)
library(tidyverse)
library(colorspace)
library(readxl)
theme_set(theme_bw())

#Load functions
source("2_scripts/00_Helper_Functions.R")

#CBS
cbs_asr1<-read.csv("./1_data/private/cbs_asr_final.csv")
cbs_asu1<-read.csv("./1_data/private/cbs_asu_final.csv")
cbs_sqm1<-read.csv("./1_data/private/cbs_sqm_final.csv")
cbs_sqs1<-read.csv("./1_data/private/cbs_sqs_final.csv")

#APL
apl_asu1<-read.csv("./1_data/private/apl_asu_final.csv")
apl_sqm1<-read.csv("./1_data/private/apl_sqm_final.csv")
apl_sqs1<-read.csv("./1_data/private/apl_sqs_final.csv")

#Ab-c
abc_asu1<-read.csv("./1_data/private/abc_asu_final.csv")
abc_asr1<-read.csv("./1_data/private/abc_asr_final.csv")
abc_asr2<-read.csv("./1_data/private/abc_asr1_final.csv")

#CLSA
clsa_asu1<-read.csv("./1_data/private/clsa_asu_final.csv")
clsa_asr1<-read.csv("./1_data/private/clsa_asr_final.csv")
clsa_asr2<-read.csv("./1_data/private/clsa_asr1_final.csv")

#Canpath
can_asu1<-read.csv("./1_data/private/can_asu_final.csv")
can_asr1<-read.csv("./1_data/private/can_asr_final.csv")
can_asr2<-read.csv("./1_data/private/can_asr1_final.csv")

#CCAHS-1 run results
ccapop_count<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_ratio/ccahs1asu.xlsx") %>% 
  mutate(age_groups = ccahs_age(age_groups))
colnames(ccapop_count)[5]<-"rep_ratio"
ccapopr_count<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_ratio/ccahs1asr.xlsx")%>% 
  mutate(age_groups = ccahs_age(age_groups))
colnames(ccapopr_count)[5]<-"rep_ratio"
ccapopqm_count<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_ratio/ccahs1sqm.xlsx")
colnames(ccapopqm_count)[4]<-"rep_ratio"
ccapopqs_count<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_ratio/ccahs1sqs.xlsx")
colnames(ccapopqs_count)[4]<-"rep_ratio"
ccapopast_count<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_ratio/ccahs2as.xlsx")%>% 
  mutate(age_groups = ccahs_age(age_groups))
colnames(ccapopast_count)[4]<-"rep_ratio"

#Census datasets
a_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_a_abc.csv") #ab-c
c_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_c_cbs.csv") #cbs
d_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_d_canpath.csv") #canpath
e_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_e_apl.csv") #apl
g_asu<-read.csv("./1_data/private/2016 Canadian Census/censusasu_g_clsa.csv") #clsa

#Read in 2016 census datasets (race)
a_asr<-read.csv("./1_data/private/2016 Canadian Census/censusasr_a_abc.csv") #ab-c
c_asr<-read.csv("./1_data/private/2016 Canadian Census/censusasr_c_cbs.csv") #cbs
d_asr<-read.csv("./1_data/private/2016 Canadian Census/censusasr_d_canpath.csv") #canpath
g_asr<-read.csv("./1_data/private/2016 Canadian Census/censusasr_g_clsa.csv") #clsa

#Read in 2016 census datasets (quintmat) 
c_sqm<-read.csv("./1_data/private/2016 Canadian Census/censussqm_c_cbs.csv") #CBS
e_sqm<-read.csv("./1_data/private/2016 Canadian Census/censussqm_e_apl.csv") #APL

#Read in 2016 census datasets (quintsoc)
c_sqs<-read.csv("./1_data/private/2016 Canadian Census/censussqs_c_cbs.csv") #CBS
e_sqs<-read.csv("./1_data/private/2016 Canadian Census/censussqs_e_apl.csv") #APL

#Read in 2016 census datasets (sensitivity analysis #1)
a_asr1<-a_asr %>% 
  mutate(race1 = race)
d_asr1<-d_asr %>% 
  mutate(race1=race)
g_asr1<-g_asr %>% 
  mutate(race1=race)

#Generate synthetic categories for age-sex-urban and 
# age-sex-race strata - required for visualization
asu_synth<-apl_asu1[,c("age_groups","sex","urban")]

asr_synth<-rbind(expand.grid(age_groups = "0-17 years",
                        race = unique(cbs_asr1$race),
                        sex = unique(cbs_asr1$sex)),
                 cbs_asr1[,-(4)])
asr_synth$race1<-asr_synth$race

#Prepare data for plotting
#- Age-sex-urban datasets -
cbs_dtasu<-merge(asu_synth,cbs_asu1,
                       by = c("age_groups","sex","urban"),all.x = TRUE)

apl_dtasu<-merge(asu_synth,apl_asu1,
                 by = c("age_groups","sex","urban"),all.x = TRUE)

abc_dtasu<-merge(asu_synth,abc_asu1,
                       by = c("age_groups","sex","urban"),all.x = TRUE)

clsa_dtasu<-merge(asu_synth,clsa_asu1,
                        by = c("age_groups","sex","urban"),all.x = TRUE)

can_dtasu<-merge(asu_synth,can_asu1,
                       by = c("age_groups","sex","urban"),all.x = TRUE)
#- Age-sex-race datasets - 
cbs_dtasr<-merge(asr_synth[,c("age_groups","sex","race")],cbs_asr1,
                       by = c("age_groups","sex","race"),all.x = TRUE)

abc_dtasr<-merge(asr_synth[,c("age_groups","sex","race")],abc_asr1,
                       by = c("age_groups","sex","race"),all.x = TRUE)

clsa_dtasr<-merge(asr_synth[,c("age_groups","sex","race")],clsa_asr1,
                        by = c("age_groups","sex","race"),all.x = TRUE)

can_dtasr<-merge(asr_synth[,c("age_groups","sex","race")],can_asr1,
                       by = c("age_groups","sex","race"),all.x = TRUE)

#Replace NA with 0 for subgroups that fall within eligibility criteria,
# but did not capture any participants in sample
can_dtasu[can_dtasu$age_groups == "18-26 years" &
            can_dtasu$urban == "Rural","count"]<-0
can_dtasr[can_dtasr$age_groups == "18-26 years" & 
            can_dtasr$sex == "Male" &
            can_dtasr$race == "White","count"]<-0

# Generate heatmaps -------------------------------------------------------
# -Set 1: Age-sex-urban -
# -- Raw counts --

#Calculate proportion each subgroup contributes to total dataset
cbs_dtasu$pct<-c(cbs_dtasu$count / sum(cbs_dtasu$count[1:24],na.rm = T))
apl_dtasu$pct<-c(apl_dtasu$count / sum(apl_dtasu$count[1:24],na.rm = T))
abc_dtasu$pct<-c(abc_dtasu$count / sum(abc_dtasu$count[1:24],na.rm = T))
clsa_dtasu$pct<-c(clsa_dtasu$count / sum(clsa_dtasu$count[1:24],na.rm = T))
can_dtasu$pct<-c(can_dtasu$count / sum(can_dtasu$count[1:24],na.rm = T))

#Plot all datasets in single plot
cbs_dtasu$cohort<-"CBS blood donor"
apl_dtasu$cohort<-"APL outpatient laboratory"
abc_dtasu$cohort<-"Ab-c open cohort"
clsa_dtasu$cohort<-"CLSA closed cohort"
can_dtasu$cohort<-"Canpath closed cohort"
all_dtasu<-do.call("rbind",list(cbs_dtasu,apl_dtasu,abc_dtasu,clsa_dtasu,can_dtasu))

#create count binned for visualization
all_dtasu$count_binned<-factor(case_when(
  all_dtasu$count <= 1000 ~ "Count \u2264 1000",
  all_dtasu$count > 1000 & all_dtasu$count <= 5000 ~ "1000 < Count \u2264 5000",
  all_dtasu$count > 5000 & all_dtasu$count <= 10000 ~ "5000 < Count \u2264 10000",
  all_dtasu$count > 10000 & all_dtasu$count <= 20000 ~ "10000 < Count \u2264 20000",
  all_dtasu$count > 20000 & all_dtasu$count <= 50000 ~ "20000 < Count \u2264 50000",
  all_dtasu$count > 50000 & all_dtasu$count <= 100000 ~ "50000 < Count \u2264 100000",
  all_dtasu$count > 100000 ~ "Count > 100000",
  TRUE ~ NA),
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
  all_dtasu$pct > 0.20 ~ "Proportion > 20%",
  TRUE ~ NA),
  levels = c("Proportion \u2264 2.5%",
             "2.5% < Proportion \u2264 5.0%",
             "5.0% < Proportion \u2264 10%",
             "10% < Proportion \u2264 15%",
             "15% < Proportion \u2264 20%",
             "Proportion > 20%"))

blues<-rev(c("#0072B4","#468FD0","#79ABE2","#A1C4F1","#C3DBFD","#E1EEFF","#F9F9F9"))
ggplot(all_dtasu[!all_dtasu$urban == "All regions",],aes(x = sex, y = factor(age_groups,
                                         levels = c("All ages",
                                                    "0-17 years",
                                                    "18-26 years",
                                                    "27-36 years",
                                                    "37-46 years",
                                                    "47-56 years",
                                                    "57+ years")), 
                     fill = count_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  facet_grid(rows = vars(urban),cols = vars(cohort))+
  geom_text(aes(label = count),color = "black",size = 3)+
  scale_fill_discrete(type = blues,na.value = "grey80")+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years","18-26 years",
                              "27-36 years","37-46 years","47-56 years",
                              "57+ years"))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

blues1<-rev(c("#468FD0","#79ABE2","#A1C4F1","#C3DBFD","#E1EEFF","#F9F9F9"))
ggplot(all_dtasu[!all_dtasu$urban == "All regions",],aes(x = sex, y = factor(age_groups,
                                         levels = c("All ages",
                                                    "0-17 years",
                                                    "18-26 years",
                                                    "27-36 years",
                                                    "37-46 years",
                                                    "47-56 years",
                                                    "57+ years")), 
                     fill = pct_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  facet_grid(rows = vars(urban),cols = vars(cohort))+
  geom_text(aes(label = round(pct,3)),
            color = "black", size = 3)+
  scale_fill_discrete(type = blues1,na.value = "grey80")+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years","18-26 years",
                              "27-36 years","37-46 years","47-56 years",
                              "57+ years"))+
  labs(fill = "Proportion",
       y = "Age Group",
       x = "Sex")

# -- Heatmap with representation ratios -- 
#---CBS representation ratio---
cbspop_count<-merge(cbs_dtasu,c_asu,by = c("age_groups","sex","urban"),all.x = T)
colnames(cbspop_count)[5]<-"pct_cbs" #percentage of total CBS samples in each subgroup
cbspop_count<-cbspop_count[order(cbspop_count$age_groups,cbspop_count$sex,
                                 cbspop_count$urban),]
cbspop_count$pct_pop<-c(cbspop_count$count_census / sum(cbspop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
cbspop_count$rep_ratio<-cbspop_count$pct_cbs / cbspop_count$pct_pop
cbspop_count$rr_binned<-rr_binned_fun(cbspop_count$rep_ratio)

#---APL representation ratio---
aplpop_count<-merge(apl_dtasu,e_asu,by = c("age_groups","sex","urban"),all.x =T)
colnames(aplpop_count)[5]<-"pct_apl" #percentage of total apl samples in each subgroup
aplpop_count<-aplpop_count[order(aplpop_count$age_groups,aplpop_count$sex,
                                 aplpop_count$urban),]
aplpop_count$pct_pop<-c(aplpop_count$count_census / sum(aplpop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates apl sample adequately represents the corresponding population subgroup
aplpop_count$rep_ratio<-aplpop_count$pct_apl / aplpop_count$pct_pop
aplpop_count$rr_binned<-rr_binned_fun(aplpop_count$rep_ratio)

#---Ab-c representation ratio---
abcpop_count<-merge(abc_dtasu,a_asu,by = c("age_groups","sex","urban"),all.x = T)
colnames(abcpop_count)[5]<-"pct_abc" #percentage of total abc samples in each subgroup
abcpop_count<-abcpop_count[order(abcpop_count$age_groups,abcpop_count$sex,
                                 abcpop_count$urban),]
abcpop_count$pct_pop<-c(abcpop_count$count_census / sum(abcpop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpop_count$rep_ratio<-abcpop_count$pct_abc / abcpop_count$pct_pop
abcpop_count$rr_binned<-rr_binned_fun(abcpop_count$rep_ratio)

#-- CLSA representation ratio --
clsapop_count<-merge(clsa_dtasu,g_asu,by = c("age_groups","sex","urban"),all.x = T)
colnames(clsapop_count)[5]<-"pct_clsa" #percentage of total clsa samples in each subgroup
clsapop_count<-clsapop_count[order(clsapop_count$age_groups,clsapop_count$sex,
                                 clsapop_count$urban),]
clsapop_count$pct_pop<-c(clsapop_count$count_census / sum(clsapop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
clsapop_count$rep_ratio<-clsapop_count$pct_clsa / clsapop_count$pct_pop
clsapop_count$rr_binned<-rr_binned_fun(clsapop_count$rep_ratio)

#-- Canpath representation ratio --
canpop_count<-merge(can_dtasu,d_asu,by = c("age_groups","sex","urban"),all.x = T)
colnames(canpop_count)[5]<-"pct_can" #percentage of total can samples in each subgroup
canpop_count<-canpop_count[order(canpop_count$age_groups,canpop_count$sex,
                                 canpop_count$urban),]
canpop_count$pct_pop<-c(canpop_count$count_census / sum(canpop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates Canpath sample adequately represents the corresponding population subgroup
canpop_count$rep_ratio<-canpop_count$pct_can / canpop_count$pct_pop
canpop_count$rr_binned<-rr_binned_fun(canpop_count$rep_ratio)

#-- All datasets in 1 plot --
cbspop_count$cohort<-"CBS blood donor"
aplpop_count$cohort<-"APL outpatient laboratory"
abcpop_count$cohort<-"Ab-c open cohort"
clsapop_count$cohort<-"CLSA closed cohort"
canpop_count$cohort<-"Canpath closed cohort"
ccapop_count<-ccapop_count %>% 
  mutate(cohort = "CCAHS-1 closed cohort",
         rr_binned = rr_binned_fun(rep_ratio))

colnames<-c("age_groups","sex","urban","rep_ratio","cohort","rr_binned")
allpopu_count<-do.call("rbind",list(cbspop_count[,colnames],
                                      aplpop_count[,colnames],
                                      abcpop_count[,colnames],
                                      clsapop_count[,colnames],
                                      canpop_count[,colnames],
                                    ccapop_count[,colnames]))

allpopu_count$rr_binned<-factor(allpopu_count$rr_binned,
                                  levels = c( "Strongly underrepresented (RR < 1/2)",
                                              "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                              "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                              "Moderately overrepresented (4/3 < RR ≤ 2)",
                                              "Strongly overrepresented (RR > 2)"))
# Join in results of bootstrap analysis
cbsu<-read.csv("./1_data/private/boot_cbs_asu_5000.csv")
abcu<-read.csv("./1_data/private/boot_abc_asu_5000.csv")
aplu<-read.csv("./1_data/private/boot_apl_asu_5000.csv")
clsau<-read.csv("./1_data/private/boot_clsa_asu_5000.csv")
canu<-read.csv("./1_data/private/boot_can_asu_5000.csv")
ccau<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_bootstrap/asu_provinces_5000.xlsx") %>% 
  mutate(age_groups = ccahs_age(age_groups))
ccau$cohort<-"CCAHS-1 closed cohort"
colnames(ccau)[6]<-"rr_prob"

boot_urban<-do.call("rbind",list(cbsu,abcu,
                                 aplu,clsau,
                                 canu,ccau[,2:6]))
allpopu_count<-merge(allpopu_count,boot_urban,by = c("age_groups","sex","urban","cohort"),
                       all.x = T)

# - Set 2: Age-sex-race -
#Calculate proportion each subgroup contributes to total dataset
cbs_dtasr$pct<-c(cbs_dtasr$count / sum(cbs_dtasr$count[1:24],na.rm = T))
abc_dtasr$pct<-c(abc_dtasr$count / sum(abc_dtasr$count[1:24],na.rm = T))
clsa_dtasr$pct<-c(clsa_dtasr$count / sum(clsa_dtasr$count[1:24],na.rm = T))
can_dtasr$pct<-c(can_dtasr$count / sum(can_dtasr$count[1:24],na.rm = T))

#Plot all datasets in single plot
cbs_dtasr$cohort<-"CBS blood donor"
abc_dtasr$cohort<-"Ab-c open cohort"
clsa_dtasr$cohort<-"CLSA closed cohort"
can_dtasr$cohort<-"Canpath closed cohort"
all_dtasr<-do.call("rbind",list(cbs_dtasr,abc_dtasr,can_dtasr,clsa_dtasr))

all_dtasr$count_binned<-factor(case_when(
  all_dtasr$count <= 1000 ~ "Count \u2264 1000",
  all_dtasr$count > 1000 & all_dtasr$count <= 5000 ~ "1000 < Count \u2264 5000",
  all_dtasr$count > 5000 & all_dtasr$count <= 10000 ~ "5000 < Count \u2264 10000",
  all_dtasr$count > 10000 & all_dtasr$count <= 20000 ~ "10000 < Count \u2264 20000",
  all_dtasr$count > 20000 & all_dtasr$count <= 50000 ~ "20000 < Count \u2264 50000",
  all_dtasr$count > 50000 & all_dtasr$count <= 100000 ~ "50000 < Count \u2264 100000",
  all_dtasr$count > 100000 ~ "Count > 100000",
  TRUE ~ NA),
  levels = c("Count \u2264 1000",
             "1000 < Count \u2264 5000",
             "5000 < Count \u2264 10000",
             "10000 < Count \u2264 20000",
             "20000 < Count \u2264 50000",
             "50000 < Count \u2264 100000",
             "Count > 100000"))

#Plot heatmap with proportion of total specimens each subgroup accounts for
all_dtasr$pct_binned<-factor(case_when(
  all_dtasr$pct <= 0.025 ~ "Proportion \u2264 2.5%",
  all_dtasr$pct > 0.025 & all_dtasr$pct <= 0.05 ~ "2.5% < Proportion \u2264 5.0%",
  all_dtasr$pct > 0.05 & all_dtasr$pct <= 0.10 ~ "5.0% < Proportion \u2264 10%",
  all_dtasr$pct > 0.10 & all_dtasr$pct <= 0.15 ~ "10% < Proportion \u2264 15%",
  all_dtasr$pct > 0.15 ~ "Proportion > 15%",
  TRUE ~ NA),
  levels = c("Proportion \u2264 2.5%",
             "2.5% < Proportion \u2264 5.0%",
             "5.0% < Proportion \u2264 10%",
             "10% < Proportion \u2264 15%",
             "Proportion > 15%"))

ggplot(all_dtasr,aes(x = sex, y = factor(age_groups,
                                         levels = c("All ages",
                                                    "0-17 years",
                                                    "18-26 years",
                                                    "27-36 years",
                                                    "37-46 years",
                                                    "47-56 years",
                                                    "57+ years")),
                     fill = count_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = blues,na.value = "grey80")+
  facet_grid(rows = vars(race),cols = vars(cohort))+
  geom_text(aes(label = count),
            color = "black", size = 3)+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years","18-26 years",
                              "27-36 years","37-46 years","47-56 years",
                              "57+ years"))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

ggplot(all_dtasr,aes(x = sex, y = factor(age_groups,
                                         levels = c("All ages",
                                                    "0-17 years",
                                                    "18-26 years",
                                                    "27-36 years",
                                                    "37-46 years",
                                                    "47-56 years",
                                                    "57+ years")), 
                     fill = pct_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = blues1,na.value = "grey80")+
  facet_grid(rows = vars(race),cols = vars(cohort))+
  geom_text(aes(label = round(pct,3)),
            color = "black", size = 3)+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years","18-26 years",
                              "27-36 years","37-46 years","47-56 years",
                              "57+ years"))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

# -- Heatmap with representation ratio --
# --- CBS representation ratio ---
cbspopr_count<-merge(cbs_dtasr,c_asr,by = c("age_groups","sex","race"),all.x = T)
colnames(cbspopr_count)[5]<-"pct_cbs" #percentage of total cbs samples in each subgroup
cbspopr_count<-cbspopr_count[order(cbspopr_count$age_groups,cbspopr_count$sex,
                                   cbspopr_count$race),]
cbspopr_count$pct_pop<-c(cbspopr_count$count_census/sum(cbspopr_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
cbspopr_count$rep_ratio<-cbspopr_count$pct_cbs / cbspopr_count$pct_pop
cbspopr_count$rr_binned<-rr_binned_fun(cbspopr_count$rep_ratio)

# --- Ab-c representation ratio ---
abcpopr_count<-merge(abc_dtasr,a_asr,by = c("age_groups","sex","race"),all.x = T)
colnames(abcpopr_count)[5]<-"pct_abc"#percentage of total abc samples in each subgroup
abcpopr_count<-abcpopr_count[order(abcpopr_count$age_groups,abcpopr_count$sex,
                                   abcpopr_count$race),]
abcpopr_count$pct_pop<-c(abcpopr_count$count_census/sum(abcpopr_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpopr_count$rep_ratio<-abcpopr_count$pct_abc / abcpopr_count$pct_pop
abcpopr_count$rr_binned<-rr_binned_fun(abcpopr_count$rep_ratio)

# --- CLSA representation ratio ---
clsapopr_count<-merge(clsa_dtasr,g_asr,by = c("age_groups","sex","race"),all.x = T)
colnames(clsapopr_count)[5]<-"pct_clsa" #percentage of total clsa samples in each subgroup
clsapopr_count<-clsapopr_count[order(clsapopr_count$age_groups,clsapopr_count$sex,
                                   clsapopr_count$race),]
clsapopr_count$pct_pop<-c(clsapopr_count$count_census/sum(clsapopr_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates clsa sample adequately represents the corresponding population subgroup
clsapopr_count$rep_ratio<-clsapopr_count$pct_clsa / clsapopr_count$pct_pop
clsapopr_count$rr_binned<-rr_binned_fun(clsapopr_count$rep_ratio)

# --- Canpath representation ratio ---
canpopr_count<-merge(can_dtasr,d_asr,by = c("age_groups","sex","race"),all.x = T)
colnames(canpopr_count)[5]<-"pct_can"#percentage of total can samples in each subgroup
canpopr_count<-canpopr_count[order(canpopr_count$age_groups,canpopr_count$sex,
                                   canpopr_count$race),]
canpopr_count$pct_pop<-c(canpopr_count$count_census/sum(canpopr_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates Canpath sample adequately represents the corresponding population subgroup
canpopr_count$rep_ratio<-canpopr_count$pct_can / canpopr_count$pct_pop
canpopr_count$rr_binned<-rr_binned_fun(canpopr_count$rep_ratio)

#Add in synthetic APL data for visualization purposes only
apl_synth<-asr_synth %>% 
  mutate(count = NA,pct = NA,cohort = "APL outpatient laboratory",
         count_census = NA,pct_pop = NA,rep_ratio = NA,rr_binned = NA)

#Plot datasets in a single plot
cbspopr_count$cohort<-"CBS blood donor"
abcpopr_count$cohort<-"Ab-c open cohort"
clsapopr_count$cohort<-"CLSA closed cohort"
canpopr_count$cohort<-"Canpath closed cohort"
ccapopr_count<-ccapopr_count %>% 
  mutate(cohort = "CCAHS-1 closed cohort",
         rr_binned = rr_binned_fun(rep_ratio))
colnamesr<-c("age_groups","sex","race","rep_ratio","cohort","rr_binned")
allpopr_count<-do.call("rbind",list(cbspopr_count[,colnamesr],
                                    abcpopr_count[,colnamesr],
                                    clsapopr_count[,colnamesr],
                                    canpopr_count[,colnamesr],
                                    apl_synth[,colnamesr],
                                    ccapopr_count[,colnamesr]))

allpopr_count$rr_binned<-factor(allpopr_count$rr_binned,
                                levels = c( "Strongly underrepresented (RR < 1/2)",
                                            "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                            "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                            "Moderately overrepresented (4/3 < RR ≤ 2)",
                                            "Strongly overrepresented (RR > 2)"))

# Join in results of bootstrap analysis
cbsr<-read.csv("./1_data/private/boot_cbs_asr_5000.csv")
abcr<-read.csv("./1_data/private/boot_abc_asr_5000.csv")
clsar<-read.csv("./1_data/private/boot_clsa_asr_5000.csv")
canr<-read.csv("./1_data/private/boot_can_asr_5000.csv")
ccar<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_bootstrap/asr_provinces_5000.xlsx") %>% 
  mutate(age_groups = ccahs_age(age_groups))
ccar$cohort<-"CCAHS-1 closed cohort"
colnames(ccar)[6]<-"rr_prob"
boot_race<-do.call("rbind",list(cbsr,abcr,
                                clsar,canr,
                                ccar[,2:6]))

allpopr_count<-merge(allpopr_count,boot_race,by = c("age_groups","sex","race","cohort"),
                     all.x = T)

# - Set 3: sex-quintmat -
#Calculate proportion each subgroup contributes to total dataset
cbs_sqm1$pct<-c(cbs_sqm1$count / sum(cbs_sqm1$count))
apl_sqm1$pct<-c(apl_sqm1$count / sum(apl_sqm1$count))

#Plot all datasets in a single plot
cbs_sqm1$cohort<-"CBS blood donor"
apl_sqm1$cohort<-"APL outpatient laboratory"
all_dtsqm<-do.call("rbind",list(cbs_sqm1,apl_sqm1))

#Create count binned for visualization
all_dtsqm$count_binned<-factor(case_when(
  all_dtsqm$count <= 1000 ~ "Count \u2264 1000",
  all_dtsqm$count > 1000 & all_dtsqm$count <= 5000 ~ "1000 < Count \u2264 5000",
  all_dtsqm$count > 5000 & all_dtsqm$count <= 10000 ~ "5000 < Count \u2264 10000",
  all_dtsqm$count > 10000 & all_dtsqm$count <= 20000 ~ "10000 < Count \u2264 20000",
  all_dtsqm$count > 20000 & all_dtsqm$count <= 50000 ~ "20000 < Count \u2264 50000",
  all_dtsqm$count > 50000 & all_dtsqm$count <= 100000 ~ "50000 < Count \u2264 100000",
  all_dtsqm$count > 100000 ~ "Count > 100000",
  TRUE ~ NA),
  levels = c("Count \u2264 1000",
             "1000 < Count \u2264 5000",
             "5000 < Count \u2264 10000",
             "10000 < Count \u2264 20000",
             "20000 < Count \u2264 50000",
             "50000 < Count \u2264 100000",
             "Count > 100000"))

#Plot heatmap with proportion of total specimens each subgroup accounts for
all_dtsqm$pct_binned<-factor(case_when(
  all_dtsqm$pct <= 0.025 ~ "Proportion \u2264 2.5%",
  all_dtsqm$pct > 0.025 & all_dtsqm$pct <= 0.05 ~ "2.5% < Proportion \u2264 5.0%",
  all_dtsqm$pct > 0.05 & all_dtsqm$pct <= 0.10 ~ "5.0% < Proportion \u2264 10%",
  all_dtsqm$pct > 0.10 & all_dtsqm$pct <= 0.15 ~ "10% < Proportion \u2264 15%",
  all_dtsqm$pct > 0.15 ~ "Proportion > 15%",
  TRUE ~ NA),
  levels = c("Proportion \u2264 2.5%",
             "2.5% < Proportion \u2264 5.0%",
             "5.0% < Proportion \u2264 10%",
             "10% < Proportion \u2264 15%",
             "Proportion > 15%"))

ggplot(all_dtsqm,aes(x = sex, y = factor(quintmat,
                                         levels = c(1,2,3,
                                                    4,5)),
                     fill = count_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = count),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Material deprivation quintile",
       x = "Sex")

ggplot(all_dtsqm,aes(x = sex, y = factor(quintmat,
                                         levels = c(1,2,
                                                    3,4,5)), 
                     fill = pct_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = round(pct,3)),color = "black",size = 3.0)+
  labs(fill = "Proportion",
       y = "Material Deprivation quintile",
       x = "Sex")

# -- Heatmap with representation ratio --
# --- CBS representation ratio ---
cbspopqm_count<-merge(cbs_sqm1,c_sqm,by = c("sex","quintmat"),all.x = T)
colnames(cbspopqm_count)[4]<-"pct_cbs" #percentage of total CBS samples in each subgroup
cbspopqm_count<-cbspopqm_count[order(cbspopqm_count$quintmat,cbspopqm_count$sex),]
cbspopqm_count$pct_pop<-c(cbspopqm_count$count_census / sum(cbspopqm_count$count_census))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
cbspopqm_count$rep_ratio<-cbspopqm_count$pct_cbs / cbspopqm_count$pct_pop
cbspopqm_count$rr_binned<-rr_binned_fun(cbspopqm_count$rep_ratio)

# --- APL representation ratio ---
aplpopqm_count<-merge(apl_sqm1,e_sqm,by = c("sex","quintmat"),all.x = T)
colnames(aplpopqm_count)[4]<-"pct_apl" #percentage of total apl samples in each subgroup
aplpopqm_count<-aplpopqm_count[order(aplpopqm_count$quintmat,aplpopqm_count$sex),]
aplpopqm_count$pct_pop<-c(aplpopqm_count$count_census / sum(aplpopqm_count$count_census))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
aplpopqm_count$rep_ratio<-aplpopqm_count$pct_apl / aplpopqm_count$pct_pop
aplpopqm_count$rr_binned<-rr_binned_fun(aplpopqm_count$rep_ratio)

#Plot datasets in a single plot
cbspopqm_count$cohort<-"CBS blood donor"
aplpopqm_count$cohort<-"APL outpatient laboratory"
ccapopqm_count<-ccapopqm_count %>% 
  filter(quintmat != "All quintiles") %>%
  mutate(cohort = "CCAHS-1 closed cohort",
         rr_binned = rr_binned_fun(rep_ratio))
namesqm<-c("sex","quintmat","cohort","rep_ratio","rr_binned")
allpopqm_count<-do.call("rbind",list(cbspopqm_count[,namesqm],aplpopqm_count[,namesqm],
                                     ccapopqm_count[,namesqm]))

allpopqm_count$rr_binned<-factor(allpopqm_count$rr_binned,
                                levels = c( "Strongly underrepresented (RR < 1/2)",
                                            "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                            "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                            "Moderately overrepresented (4/3 < RR ≤ 2)",
                                            "Strongly overrepresented (RR > 2)"))

# Join in results of bootstrap analysis
cbsqm<-read.csv("./1_data/private/boot_cbs_sqm_5000.csv")
aplqm<-read.csv("./1_data/private/boot_apl_sqm_5000.csv")
ccasqm<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_bootstrap/sqm_provinces_5000.xlsx")
ccasqm<-ccasqm %>% 
  filter(quintmat != "All quintiles") %>%
  mutate(cohort = "CCAHS-1 closed cohort")
colnames(ccasqm)[5]<-"rr_prob"
boot_qm<-do.call("rbind",list(cbsqm,aplqm,ccasqm[,2:5]))

allpopqm_count<-merge(allpopqm_count,boot_qm,by = c("sex","quintmat","cohort"),all.x = T)
allpopqm_count$cohort<-case_when(
    allpopqm_count$cohort == "CBS blood donor" ~ "CBS blood\ndonor",
    allpopqm_count$cohort == "APL outpatient laboratory" ~ "APL outpatient\nlaboratory",
    allpopqm_count$cohort == "CCAHS-1 closed cohort" ~ "CCAHS-1 closed\ncohort")
allpopqm_count$cohort<-factor(allpopqm_count$cohort, 
                             levels = c("CBS blood\ndonor","APL outpatient\nlaboratory",
                                        "CCAHS-1 closed\ncohort"))

#-Set 4: sex-quintsoc-
#Calculate proportion each subgroup contributes to total dataset
cbs_sqs1$pct<-c(cbs_sqs1$count / sum(cbs_sqs1$count))
apl_sqs1$pct<-c(apl_sqs1$count / sum(apl_sqs1$count))

#Plot all datasets in a single plot
cbs_sqs1$cohort<-"Blood donor"
apl_sqs1$cohort<-"APL outpatient laboratory"
all_dtsqs<-do.call("rbind",list(cbs_sqs1,apl_sqs1))

#Create count binned for visualization
all_dtsqs$count_binned<-factor(case_when(
  all_dtsqs$count <= 1000 ~ "Count \u2264 1000",
  all_dtsqs$count > 1000 & all_dtsqs$count <= 5000 ~ "1000 < Count \u2264 5000",
  all_dtsqs$count > 5000 & all_dtsqs$count <= 10000 ~ "5000 < Count \u2264 10000",
  all_dtsqs$count > 10000 & all_dtsqs$count <= 20000 ~ "10000 < Count \u2264 20000",
  all_dtsqs$count > 20000 & all_dtsqs$count <= 50000 ~ "20000 < Count \u2264 50000",
  all_dtsqs$count > 50000 & all_dtsqs$count <= 100000 ~ "50000 < Count \u2264 100000",
  all_dtsqs$count > 100000 ~ "Count > 100000",
  TRUE ~ NA),
  levels = c("Count \u2264 1000",
             "1000 < Count \u2264 5000",
             "5000 < Count \u2264 10000",
             "10000 < Count \u2264 20000",
             "20000 < Count \u2264 50000",
             "50000 < Count \u2264 100000",
             "Count > 100000"))

#Plot heatmap with proportion of total specimens each subgroup accounts for
all_dtsqs$pct_binned<-factor(case_when(
  all_dtsqs$pct <= 0.025 ~ "Proportion \u2264 2.5%",
  all_dtsqs$pct > 0.025 & all_dtsqs$pct <= 0.05 ~ "2.5% < Proportion \u2264 5.0%",
  all_dtsqs$pct > 0.05 & all_dtsqs$pct <= 0.10 ~ "5.0% < Proportion \u2264 10%",
  all_dtsqs$pct > 0.10 & all_dtsqs$pct <= 0.15 ~ "10% < Proportion \u2264 15%",
  all_dtsqs$pct > 0.15 ~ "Proportion > 15%",
  TRUE ~ NA),
  levels = c("Proportion \u2264 2.5%",
             "2.5% < Proportion \u2264 5.0%",
             "5.0% < Proportion \u2264 10%",
             "10% < Proportion \u2264 15%",
             "Proportion > 15%"))

ggplot(all_dtsqs,aes(x = sex, y = factor(quintsoc,
                                         levels = c(1,2,3,
                                                    4,5)),
                     fill = count_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = count),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Social deprivation quintile",
       x = "Sex")

ggplot(all_dtsqs,aes(x = sex, y = factor(quintsoc,
                                         levels = c(1,2,
                                                    3,4,5)), 
                     fill = pct_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete_sequential(palette = "Blues 3")+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = round(pct,3)),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Social Deprivation quintile",
       x = "Sex")

# -- Heatmap with representation ratio --
# --- CBS representation ratio ---
cbspopqs_count<-merge(cbs_sqs1,c_sqs,by = c("sex","quintsoc"),all.x = T)
colnames(cbspopqs_count)[4]<-"pct_cbs" #percentage of total CBS samples in each subgroup
cbspopqs_count<-cbspopqs_count[order(cbspopqs_count$quintsoc,cbspopqs_count$sex),]
cbspopqs_count$pct_pop<-c(cbspopqs_count$count_census / sum(cbspopqs_count$count_census))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
cbspopqs_count$rep_ratio<-cbspopqs_count$pct_cbs / cbspopqs_count$pct_pop
cbspopqs_count$rr_binned<-rr_binned_fun(cbspopqs_count$rep_ratio)

# --- APL representation ratio ---
aplpopqs_count<-merge(apl_sqs1,e_sqs,by = c("sex","quintsoc"),all.x = T)
colnames(aplpopqs_count)[4]<-"pct_apl" #percentage of total apl samples in each subgroup
aplpopqs_count<-aplpopqs_count[order(aplpopqs_count$quintsoc,aplpopqs_count$sex),]
aplpopqs_count$pct_pop<-c(aplpopqs_count$count_census / sum(aplpopqs_count$count_census))

#Calculate representation ratio -- value = 1 indicates apl sample adequately represents the corresponding population subgroup
aplpopqs_count$rep_ratio<-aplpopqs_count$pct_apl / aplpopqs_count$pct_pop
aplpopqs_count$rr_binned<-rr_binned_fun(aplpopqs_count$rep_ratio)

#Plot datasets in a single plot
cbspopqs_count$cohort<-"CBS blood donor"
aplpopqs_count$cohort<-"APL outpatient laboratory"
ccapopqs_count<-ccapopqs_count %>% 
  filter(quintsoc != "All quintiles") %>%
  mutate(cohort = "CCAHS-1 closed cohort",
         rr_binned = rr_binned_fun(rep_ratio))
namesqs<-c("sex","quintsoc","cohort","rep_ratio","rr_binned")
allpopqs_count<-do.call("rbind",list(cbspopqs_count[,namesqs],aplpopqs_count[,namesqs],
                                     ccapopqs_count[,namesqs]))

allpopqs_count$rr_binned<-factor(allpopqs_count$rr_binned,
                                 levels = c( "Strongly underrepresented (RR < 1/2)",
                                             "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                             "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                             "Moderately overrepresented (4/3 < RR ≤ 2)",
                                             "Strongly overrepresented (RR > 2)"))

# Join in results of bootstrap analysis
cbsqs<-read.csv("./1_data/private/boot_cbs_sqs_5000.csv")
aplqs<-read.csv("./1_data/private/boot_apl_sqs_5000.csv")
ccasqs<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_bootstrap/sqs_provinces_5000.xlsx")
ccasqs<-ccasqs %>% 
  filter(quintsoc != "All quintiles") %>% 
  mutate(cohort = "CCAHS-1 closed cohort")
colnames(ccasqs)[5]<-"rr_prob"
boot_qs<-do.call("rbind",list(cbsqs,aplqs,ccasqs[,2:5]))

allpopqs_count<-merge(allpopqs_count,boot_qs,by = c("sex","quintsoc","cohort"),all.x = T)
allpopqs_count$cohort<-case_when(
  allpopqs_count$cohort == "CBS blood donor" ~ "CBS blood\ndonor",
  allpopqs_count$cohort == "APL outpatient laboratory" ~ "APL outpatient\nlaboratory",
  allpopqs_count$cohort == "CCAHS-1 closed cohort" ~ "CCAHS-1 closed\ncohort")
allpopqs_count$cohort<-factor(allpopqs_count$cohort, 
                              levels = c("CBS blood\ndonor","APL outpatient\nlaboratory",
                                         "CCAHS-1 closed\ncohort"))
#-Set 5: territories by age-sex
ccapopast_count<-ccapopast_count %>% 
  mutate(cohort = "CCAHS-1 closed cohort",
         rr_binned = rr_binned_fun(rep_ratio))
namesast<-c("sex","age_groups","cohort","rep_ratio","rr_binned")
allpopast_count<-do.call("rbind",list(ccapopast_count[,namesast]))
allpopast_count$rr_binned<-factor(allpopast_count$rr_binned,
                                 levels = c( "Strongly underrepresented (RR < 1/2)",
                                             "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                             "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                             "Moderately overrepresented (4/3 < RR ≤ 2)",
                                             "Strongly overrepresented (RR > 2)"))
#Read in bootstrap results
ccaast<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_bootstrap/as_territories_5000.xlsx")
ccaast<-ccaast %>% 
  mutate(cohort = "CCAHS-1 closed cohort",
         age_groups = ccahs_age(age_groups))
colnames(ccaast)[5]<-"rr_prob"
boot_ast<-do.call("rbind",list(ccaast[,2:5]))

allpopast_count<-merge(allpopast_count,boot_ast,by = c("sex","age_groups","cohort"),all.x = T)
allpopast_count<-allpopast_count %>% 
  mutate(age_groups = factor(age_groups,
                             levels = c("All ages","0-17 years","18-26 years",
                                        "27-36 years","37-46 years","47-56 years",
                                        "57+ years")))

# #- Create figures 1 and 2- ----------------------------------------------
# Compose age-sex-urban and age-sex-race components of plot
colnames(allpopu_count)[3]<-"strata"
colnames(allpopr_count)[3]<-"strata"
f<-rbind(allpopu_count,allpopr_count)
f<-f %>% mutate(
  cohort = case_when(
    cohort == "CBS blood donor" ~ "CBS blood\ndonor",
    cohort == "APL outpatient laboratory" ~ "APL outpatient\nlaboratory",
    cohort == "Ab-c open cohort" ~ "Ab-c open\ncohort",
    cohort == "Canpath closed cohort" ~ "Canpath closed\ncohort",
    cohort == "CLSA closed cohort" ~ "CLSA closed\ncohort",
    cohort == "CCAHS-1 closed cohort" ~ "CCAHS-1 closed\ncohort",
    TRUE ~ NA),
  strata = case_when(
    strata == "Racialized minority" ~ "Racialized\nminority",
    strata == "Urban" ~ "Urban",
    strata == "All regions" ~ "All",
    strata == "Rural" ~ "Rural",
    strata == "White" ~ "White",
    TRUE ~ NA)
)

f<-f %>% mutate(
  strata = factor(strata,
                  levels = c("All","Rural","Urban","Racialized\nminority",
                             "White")),
  rr_binned = factor(rr_binned,
                     levels = c("Strongly underrepresented (RR < 1/2)",
                                "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                "Moderately overrepresented (4/3 < RR \u2264 2)",
                                "Strongly overrepresented (RR > 2)")),
  cohort=factor(cohort, levels = c("CBS blood\ndonor","APL outpatient\nlaboratory",
                                   "CCAHS-1 closed\ncohort","Ab-c open\ncohort",
                                   "Canpath closed\ncohort","CLSA closed\ncohort")),
  age_groups = factor(age_groups,
                      levels = c("All ages","0-17 years","18-26 years",
                                 "27-36 years","37-46 years","47-56 years",
                                 "57+ years")),
  rep_ratio = round(as.numeric(rep_ratio),2))

f1<-f %>% filter(age_groups == "All ages")
f2<-f %>% filter(age_groups != "All ages")

f2_qm<-allpopqm_count %>% 
  mutate(rep_ratio = round(as.numeric(rep_ratio),2),
         rr_binned = factor(rr_binned,
                            levels = c("Strongly underrepresented (RR < 1/2)",
                                       "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                       "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                       "Moderately overrepresented (4/3 < RR \u2264 2)",
                                       "Strongly overrepresented (RR > 2)")))
f2_qs<-allpopqs_count %>% 
  mutate(rep_ratio = round(as.numeric(rep_ratio),2),
         rr_binned = factor(rr_binned,
                            levels = c("Strongly underrepresented (RR < 1/2)",
                                       "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                       "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                       "Moderately overrepresented (4/3 < RR \u2264 2)",
                                       "Strongly overrepresented (RR > 2)")))

cols<-c("#E16A86","#FFC5D0","#009ADE","#A8E1BF","#50A315")
#Figure 1
ggplot(f1,aes(x = sex,y = age_groups,
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 2.5)+
  facet_grid(rows = vars(strata),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented (RR < 1/2)",
                               "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                               "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented (4/3 < RR ≤ 2)",
                               "Not applicable"),
                    na.value = "grey80")
#Quintsoc
ggplot(f2_qs,aes(x = sex,y = factor(quintsoc,levels = c(1:5)),
                         fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 2.5)+
  facet_grid(cols = vars(cohort))+
  labs(fill = "Representativeness Ratio",
       x = "",
       y = "Social deprivation quintile")+
  scale_fill_manual(values = cols[2:4],
                    labels = c("Moderately underrepresented (1/2 ≤ RR < 3/4)",
                               "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented (4/3 < RR ≤ 2)"),
                    na.value = "grey80")

#Territories by age-sex
ggplot(allpopast_count,aes(x = sex,y = age_groups,
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 2.5)+
  facet_grid(cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented (RR < 1/2)",
                               "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                               "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented (4/3 < RR ≤ 2)",
                               "Not applicable"),
                    na.value = "grey80")
#Figure 2 all ages
ggplot(f2,aes(x = sex,y = age_groups,
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = F,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 2.5)+
  facet_grid(rows = vars(strata),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented (RR < 1/2)",
                               "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                               "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented (4/3 < RR ≤ 2)",
                               "Strongly overrepresented (RR > 2)",
                               "Not applicable"),
                    na.value = "grey80")
#Figure 2 quintmat
ggplot(f2_qm,aes(x = sex,y = factor(quintmat,levels = c(1:5)),
                                  fill = rr_binned))+
  geom_tile(color = "black",show.legend = F,linewidth = 0.1)+
  scale_fill_discrete(type = cols)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 2.5)+
  facet_grid(cols = vars(cohort))+
  labs(fill = "Representativeness Ratio",
       x = "",
       y = "Material deprivation quintile")

'#Combine plots
g1<-ggplotGrob(f2_p) #f2 all ages
g2<-ggplotGrob(f2_qmp) #f2 qm
g2$widths[9]<-4*g1$widths[5]
g1$heights[1]<- test * 4
grid.arrange(g1,g2)
test<-g1$heights[1]'

# Sensitivity analysis 1: classify mixed race as "White" ------------------
abc_dtasr1<-merge(asr_synth[,c("age_groups","sex","race1")],abc_asr2,
                  by = c("age_groups","sex","race1"),all.x = TRUE)
clsa_dtasr1<-merge(asr_synth[,c("age_groups","sex","race1")],clsa_asr2,
                  by = c("age_groups","sex","race1"),all.x = TRUE)
can_dtasr1<-merge(asr_synth[,c("age_groups","sex","race1")],can_asr2,
                   by = c("age_groups","sex","race1"),all.x = TRUE)

#Replace NA with 0 for subgroups that fall within eligibility criteria,
# but did not capture any participants in sample
can_dtasr1[can_dtasr1$age_groups == "18-26 years" & 
            can_dtasr1$sex == "Male" &
            can_dtasr1$race1 == "White","count"]<-0

#Calculate proportion each subgroup contributes to total dataset
abc_dtasr1$pct<-c(abc_dtasr1$count / sum(abc_dtasr1$count[1:24],na.rm = T))
clsa_dtasr1$pct<-c(clsa_dtasr1$count / sum(clsa_dtasr1$count[1:24],na.rm = T))
can_dtasr1$pct<-c(can_dtasr1$count / sum(can_dtasr1$count[1:24],na.rm = T))

abc_dtasr1$cohort<-"Ab-c open cohort"
clsa_dtasr1$cohort<-"CLSA closed cohort"
can_dtasr1$cohort<-"Canpath closed cohort"
all_dtasr1<-do.call("rbind",list(abc_dtasr1,clsa_dtasr1,can_dtasr1))

all_dtasr1$cohort<-factor(all_dtasr1$cohort,
                          levels = c("Ab-c open cohort",
                                     "Canpath closed cohort",
                                     "CLSA closed cohort"
                          ))
all_dtasr1$count_binned<-factor(case_when(
  all_dtasr1$count <= 1000 ~ "Count \u2264 1000",
  all_dtasr1$count > 1000 & all_dtasr1$count <= 5000 ~ "1000 < Count \u2264 5000",
  all_dtasr1$count > 5000 & all_dtasr1$count <= 10000 ~ "5000 < Count \u2264 10000",
  all_dtasr1$count > 10000 & all_dtasr1$count <= 20000 ~ "10000 < Count \u2264 20000",
  all_dtasr1$count > 20000 & all_dtasr1$count <= 50000 ~ "20000 < Count \u2264 50000",
  all_dtasr1$count > 50000 & all_dtasr1$count <= 100000 ~ "50000 < Count \u2264 100000",
  all_dtasr1$count > 100000 ~ "Count > 100000",
  TRUE ~ NA),
  levels = c("Count \u2264 1000",
             "1000 < Count \u2264 5000",
             "5000 < Count \u2264 10000",
             "10000 < Count \u2264 20000",
             "20000 < Count \u2264 50000",
             "50000 < Count \u2264 100000",
             "Count > 100000"))

#Plot heatmap with proportion of total specimens each subgroup accounts for
all_dtasr1$pct_binned<-factor(case_when(
  all_dtasr1$pct <= 0.025 ~ "Proportion \u2264 2.5%",
  all_dtasr1$pct > 0.025 & all_dtasr1$pct <= 0.05 ~ "2.5% < Proportion \u2264 5.0%",
  all_dtasr1$pct > 0.05 & all_dtasr1$pct <= 0.10 ~ "5.0% < Proportion \u2264 10%",
  all_dtasr1$pct > 0.10 & all_dtasr1$pct <= 0.15 ~ "10% < Proportion \u2264 15%",
  all_dtasr1$pct > 0.15 ~ "Proportion > 15%",
  TRUE ~ NA),
  levels = c("Proportion \u2264 2.5%",
             "2.5% < Proportion \u2264 5.0%",
             "5.0% < Proportion \u2264 10%",
             "10% < Proportion \u2264 15%",
             "Proportion > 15%"))

ggplot(all_dtasr1,aes(x = sex, y = factor(age_groups,
                                          levels = c("All ages",
                                                     "0-17 years",
                                                     "18-26 years",
                                                     "27-36 years",
                                                     "37-46 years",
                                                     "47-56 years",
                                                     "57+ years")),
                      fill = count_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = blues,na.value = "grey80")+
  facet_grid(rows = vars(race1),cols = vars(cohort))+
  geom_text(aes(label = count),
            color = "black", size = 3)+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years","18-26 years",
                              "27-36 years","37-46 years","47-56 years",
                              "57+ years"))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")

ggplot(all_dtasr1,aes(x = sex, y = factor(age_groups,
                                          levels = c("All ages",
                                                     "0-17 years",
                                                     "18-26 years",
                                                     "27-36 years",
                                                     "37-46 years",
                                                     "47-56 years",
                                                     "57+ years")), 
                      fill = pct_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = blues1,na.value = "grey80")+
  facet_grid(rows = vars(race1),cols = vars(cohort))+
  geom_text(aes(label = round(pct,3)),
            color = "black", size = 3)+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years","18-26 years",
                              "27-36 years","37-46 years","47-56 years",
                              "57+ years"))+
  labs(fill = "Proportion",
       y = "Age Group",
       x = "Sex")

# -- Heatmap with representation ratio --
# --- Ab-c representation ratio ---
abcpopr_count1<-merge(abc_dtasr1,a_asr1,by = c("age_groups","sex","race1"),all.x = T)
colnames(abcpopr_count1)[5]<-"pct_abc" #percentage of total abc samples in each subgroup
abcpopr_count1<-abcpopr_count1[order(abcpopr_count1$age_groups,abcpopr_count1$sex,
                                      abcpopr_count1$race1),]
abcpopr_count1$pct_pop<-c(abcpopr_count1$count_census / sum(abcpopr_count1$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpopr_count1$rep_ratio<-abcpopr_count1$pct_abc / abcpopr_count1$pct_pop
abcpopr_count1$rr_binned<-rr_binned_fun(abcpopr_count1$rep_ratio)
abcpopr_count1$cohort<-"Ab-c open cohort"

# --- Canpath representation ratio ---
canpopr_count1<-merge(can_dtasr1,d_asr1,by = c("age_groups","sex","race1"),all.x = T)
colnames(canpopr_count1)[5]<-"pct_can" #percentage of total canpath samples in each subgroup
canpopr_count1<-canpopr_count1[order(canpopr_count1$age_groups,canpopr_count1$sex,
                                     canpopr_count1$race1),]
canpopr_count1$pct_pop<-c(canpopr_count1$count_census / sum(canpopr_count1$count_census[1:24],na.rm = T))

#Calculate representation ratio
canpopr_count1$rep_ratio<-canpopr_count1$pct_can / canpopr_count1$pct_pop
canpopr_count1$rr_binned<-rr_binned_fun(canpopr_count1$rep_ratio)
canpopr_count1$cohort<-"Canpath closed cohort"

# --- CLSA representation ratio ---
clsapopr_count1<-merge(clsa_dtasr1,g_asr1,by = c("age_groups","sex","race1"),all.x = T)
colnames(clsapopr_count1)[5]<-"pct_clsa" #percentage of total clsa samples in each subgroup
clsapopr_count1<-clsapopr_count1[order(clsapopr_count1$age_groups,clsapopr_count1$sex,
                                     clsapopr_count1$race1),]
clsapopr_count1$pct_pop<-c(clsapopr_count1$count_census / sum(clsapopr_count1$count_census[1:24],na.rm = T))

#Calculate representation ratio
clsapopr_count1$rep_ratio<-clsapopr_count1$pct_clsa / clsapopr_count1$pct_pop
clsapopr_count1$rr_binned<-rr_binned_fun(clsapopr_count1$rep_ratio)

#Combine all into 1 dataframe
allpopr_count1<-do.call("rbind",list(abcpopr_count1[,-5],clsapopr_count1[,-5],canpopr_count1[,-5]))

allpopr_count1$rr_binned<-factor(allpopr_count1$rr_binned,
                                levels = c( "Strongly underrepresented (RR < 1/2)",
                                            "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                                            "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                                            "Moderately overrepresented (4/3 < RR ≤ 2)",
                                            "Strongly overrepresented (RR > 2)"))

#Join in results of bootstrap analysis
abcr1<-read.csv("./1_data/private/boot_abc_asr1_5000.csv")
clsa1<-read.csv("./1_data/private/boot_clsa_asr1_5000.csv")
can1<-read.csv("./1_data/private/boot_can_asr1_5000.csv")
boot_race1<-do.call("rbind",list(abcr1,clsa1,can1))

allpopr_count1<-merge(allpopr_count1,boot_race1,by = c("age_groups","sex","race1","cohort"),all.x = T)

#Create alternate versions of figs 1 and 2 for sensitivity analysis 1
allpopr_count1<-allpopr_count1 %>% 
  mutate(
    cohort = case_when(
      cohort == "Ab-c open cohort" ~ "Ab-c open\ncohort",
      cohort == "Canpath closed cohort" ~ "Canpath closed\ncohort",
      cohort == "CLSA closed cohort" ~ "CLSA closed\ncohort",
      TRUE ~ NA),
    race1 = case_when(
      race1 == "White" ~ "White",
      race1 == "Racialized minority" ~ "Racialized\nminority",
      TRUE ~ NA)
  )

allpopr_count1<-allpopr_count1 %>% 
  mutate(
    rr_binned = factor(rr_binned,
                       levels = c("Strongly underrepresented (RR < 1/2)",
                                  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                  "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                  "Moderately overrepresented (4/3 < RR \u2264 2)",
                                  "Strongly overrepresented (RR > 2)")),
    cohort=factor(cohort, levels = c("Ab-c open\ncohort","Canpath closed\ncohort",
                                     "CLSA closed\ncohort")),
    age_groups = factor(age_groups,
                        levels = c("All ages","0-17 years","18-26 years",
                                   "27-36 years","37-46 years","47-56 years",
                                   "57+ years")),
    rep_ratio = round(as.numeric(rep_ratio),2)
  )

f1_s1<-allpopr_count1 %>% filter(age_groups == "All ages")
f2_s1<-allpopr_count1 %>% filter(age_groups != "All ages")


ggplot(f1_s1,aes(x = sex,y = age_groups,
                 fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 2.5)+
  facet_grid(rows = vars(race1),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented (RR < 1/2)",
                               "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                               "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented (4/3 < RR ≤ 2)",
                               "Not applicable"),
                    na.value = "grey80")

ggplot(f2_s1,aes(x = sex,y = age_groups,
                 fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 2.5)+
  facet_grid(rows = vars(race1),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented (RR < 1/2)",
                               "Moderately underrepresented (1/2 ≤ RR < 3/4)",
                               "Adequately represented (3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented (4/3 < RR ≤ 2)",
                               "Strongly overrepresented (RR > 2)",
                               "Not applicable"),
                    na.value = "grey80")

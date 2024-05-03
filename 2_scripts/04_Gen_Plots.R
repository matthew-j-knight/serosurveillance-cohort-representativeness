"This script generates all figures used in the paper."

# Load packages and data -----------------------------------------------------------
setwd("~/serosurveillance-cohort-representativeness")
library(ggplot2)
library(tidyverse)
library(colorspace)
library(readxl)
library(scales)
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
ccapopr_count1<-read_xlsx("1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_ratio/ccahs1asr_sensitivity.xlsx") %>% 
  mutate(age_groups = ccahs_age(age_groups))
colnames(ccapopr_count1)[5]<-"rep_ratio"

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

#Read in census datasets for sensitivity analysis #2
d_asus2<-read.csv("./1_data/private/2016 Canadian Census/censusasu_d_canpath_s2.csv") #canpath
g_asus2<-read.csv("./1_data/private/2016 Canadian Census/censusasu_g_clsa_s2.csv") #clsa
d_asrs2<-read.csv("./1_data/private/2016 Canadian Census/censusasr_d_canpath_s2.csv") #canpath
g_asrs2<-read.csv("./1_data/private/2016 Canadian Census/censusasr_g_clsa_s2.csv") #clsa

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
  all_dtasu$count <= 1000 ~ "Count \u2264 1,000",
  all_dtasu$count > 1000 & all_dtasu$count <= 5000 ~ "1,000 < Count \u2264 5,000",
  all_dtasu$count > 5000 & all_dtasu$count <= 10000 ~ "5,000 < Count \u2264 10,000",
  all_dtasu$count > 10000 & all_dtasu$count <= 20000 ~ "10,000 < Count \u2264 20,000",
  all_dtasu$count > 20000 & all_dtasu$count <= 50000 ~ "20,000 < Count \u2264 50,000",
  all_dtasu$count > 50000 & all_dtasu$count <= 100000 ~ "50,000 < Count \u2264 100,000",
  all_dtasu$count > 100000 ~ "Count > 100,000",
  TRUE ~ NA),
  levels = c("Count \u2264 1,000",
             "1,000 < Count \u2264 5,000",
             "5,000 < Count \u2264 10,000",
             "10,000 < Count \u2264 20,000",
             "20,000 < Count \u2264 50,000",
             "50,000 < Count \u2264 100,000",
             "Count > 100,000"))

all_dtasu<-all_dtasu %>% 
  mutate(cohort = case_when(
  cohort == "CBS blood donor" ~ "CBS blood\ndonor",
  cohort == "APL outpatient laboratory" ~ "APL outpatient\nlaboratory",
  cohort == "Ab-c open cohort" ~ "Ab-c open\ncohort",
  cohort == "Canpath closed cohort" ~ "Canpath closed\ncohort",
  cohort == "CLSA closed cohort" ~ "CLSA closed\ncohort",
  cohort == "CCAHS-1 closed cohort" ~ "CCAHS-1 closed\ncohort",
  TRUE ~ NA))

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

all_dtasu$cohort<-factor(all_dtasu$cohort,
                         levels = c(
                           "CBS blood\ndonor","APL outpatient\nlaboratory",
                           "Ab-c open\ncohort","Canpath closed\ncohort",
                           "CLSA closed\ncohort"))

blues <- rev(c("#4D8DC4", "#65A1D5", "#82B4E4", "#A2C8F1", "#C3DBFD", "#E1EEFF", "#F9F9F9"))
supp_asu<-ggplot(all_dtasu[!all_dtasu$urban == "All regions",],aes(x = sex, y = factor(age_groups,
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
  geom_text(aes(label = scales::number(count,big.mark = ",")),color = "black",size = 5)+
  scale_fill_discrete(type = blues,na.value = "grey80")+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years" = "0-17",
                              "18-26 years" = "18-26","27-36 years" = "27-36",
                              "37-46 years" = "37-46","47-56 years" = "47-56",
                              "57+ years" = "Age 57+"))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")+
  theme(legend.text = element_text(size = 11.0),
        legend.position = "right",
        legend.margin = margin(rep(0,4)),
        legend.box.margin = margin(rep(0,4)),
        legend.spacing.x = unit(0.1,"cm"),
        plot.margin = unit(rep(0.00,4),"cm"),
        strip.text = element_text(size = 13.0),
        axis.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.title = element_text(size = 15),
        axis.text.y = element_text(margin = margin(r = 0.1)))+
  guides(fill = guide_legend(byrow = TRUE))
supp_asu
#ggsave("4_output/figs/supp_asu.svg",width=12.5,height=8.5,unit="in")

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
  scale_fill_discrete(type = blues,na.value = "grey80")+
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

#---APL representation ratio---
aplpop_count<-merge(apl_dtasu,e_asu,by = c("age_groups","sex","urban"),all.x =T)
colnames(aplpop_count)[5]<-"pct_apl" #percentage of total apl samples in each subgroup
aplpop_count<-aplpop_count[order(aplpop_count$age_groups,aplpop_count$sex,
                                 aplpop_count$urban),]
aplpop_count$pct_pop<-c(aplpop_count$count_census / sum(aplpop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates apl sample adequately represents the corresponding population subgroup
aplpop_count$rep_ratio<-aplpop_count$pct_apl / aplpop_count$pct_pop

#---Ab-c representation ratio---
abcpop_count<-merge(abc_dtasu,a_asu,by = c("age_groups","sex","urban"),all.x = T)
colnames(abcpop_count)[5]<-"pct_abc" #percentage of total abc samples in each subgroup
abcpop_count<-abcpop_count[order(abcpop_count$age_groups,abcpop_count$sex,
                                 abcpop_count$urban),]
abcpop_count$pct_pop<-c(abcpop_count$count_census / sum(abcpop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpop_count$rep_ratio<-abcpop_count$pct_abc / abcpop_count$pct_pop

#-- CLSA representation ratio --
clsapop_count<-merge(clsa_dtasu,g_asu,by = c("age_groups","sex","urban"),all.x = T)
colnames(clsapop_count)[5]<-"pct_clsa" #percentage of total clsa samples in each subgroup
clsapop_count<-clsapop_count[order(clsapop_count$age_groups,clsapop_count$sex,
                                 clsapop_count$urban),]
clsapop_count$pct_pop<-c(clsapop_count$count_census / sum(clsapop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
clsapop_count$rep_ratio<-clsapop_count$pct_clsa / clsapop_count$pct_pop

#-- Canpath representation ratio --
canpop_count<-merge(can_dtasu,d_asu,by = c("age_groups","sex","urban"),all.x = T)
colnames(canpop_count)[5]<-"pct_can" #percentage of total can samples in each subgroup
canpop_count<-canpop_count[order(canpop_count$age_groups,canpop_count$sex,
                                 canpop_count$urban),]
canpop_count$pct_pop<-c(canpop_count$count_census / sum(canpop_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates Canpath sample adequately represents the corresponding population subgroup
canpop_count$rep_ratio<-canpop_count$pct_can / canpop_count$pct_pop

#-- All datasets in 1 plot --
cbspop_count$cohort<-"CBS blood donor"
aplpop_count$cohort<-"APL outpatient laboratory"
abcpop_count$cohort<-"Ab-c open cohort"
clsapop_count$cohort<-"CLSA closed cohort"
canpop_count$cohort<-"Canpath closed cohort"
ccapop_count<-ccapop_count %>% 
  mutate(cohort = "CCAHS-1 closed cohort")

colnames<-c("age_groups","sex","urban","rep_ratio","cohort")
allpopu_count<-do.call("rbind",list(cbspop_count[,colnames],
                                      aplpop_count[,colnames],
                                      abcpop_count[,colnames],
                                      clsapop_count[,colnames],
                                      canpop_count[,colnames],
                                    ccapop_count[,colnames]))

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
  all_dtasr$count <= 1000 ~ "Count \u2264 1,000",
  all_dtasr$count > 1000 & all_dtasr$count <= 5000 ~ "1,000 < Count \u2264 5,000",
  all_dtasr$count > 5000 & all_dtasr$count <= 10000 ~ "5,000 < Count \u2264 10,000",
  all_dtasr$count > 10000 & all_dtasr$count <= 20000 ~ "10,000 < Count \u2264 20,000",
  all_dtasr$count > 20000 & all_dtasr$count <= 50000 ~ "20,000 < Count \u2264 50,000",
  all_dtasr$count > 50000 & all_dtasr$count <= 100000 ~ "50,000 < Count \u2264 100,000",
  all_dtasr$count > 100000 ~ "Count > 100,000",
  TRUE ~ NA),
  levels = c("Count \u2264 1,000",
             "1,000 < Count \u2264 5,000",
             "5,000 < Count \u2264 10,000",
             "10,000 < Count \u2264 20,000",
             "20,000 < Count \u2264 50,000",
             "50,000 < Count \u2264 100,000",
             "Count > 100,000"))

all_dtasr<-all_dtasr %>% 
  mutate(cohort = case_when(
    cohort == "CBS blood donor" ~ "CBS blood\ndonor",
    cohort == "APL outpatient laboratory" ~ "APL outpatient\nlaboratory",
    cohort == "Ab-c open cohort" ~ "Ab-c open\ncohort",
    cohort == "Canpath closed cohort" ~ "Canpath closed\ncohort",
    cohort == "CLSA closed cohort" ~ "CLSA closed\ncohort",
    cohort == "CCAHS-1 closed cohort" ~ "CCAHS-1 closed\ncohort",
    TRUE ~ NA))

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

all_dtasr$cohort<-factor(all_dtasr$cohort,
                         levels = c(
                           "CBS blood\ndonor","APL outpatient\nlaboratory",
                           "Ab-c open\ncohort","Canpath closed\ncohort",
                           "CLSA closed\ncohort"))

supp_asr<-ggplot(all_dtasr,aes(x = sex, y = factor(age_groups,
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
  geom_text(aes(label = scales::number(count,big.mark = ",")),
            color = "black", size = 5.25)+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years" = "0-17",
                              "18-26 years" = "18-26","27-36 years" = "27-36",
                              "37-46 years" = "37-46","47-56 years" = "47-56",
                              "57+ years" = "Age 57+"))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")+
  theme(legend.text = element_text(size = 12.0),
        legend.position = "right",
        legend.margin = margin(rep(0,4)),
        legend.box.margin = margin(rep(0,4)),
        legend.spacing.x = unit(0.1,"cm"),
        legend.spacing.y = unit(0.3,"cm"),
        plot.margin = unit(rep(0.00,4),"cm"),
        strip.text = element_text(size = 13.0),
        axis.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.title = element_text(size = 16),
        axis.text.y = element_text(margin = margin(r = 0.1)))+
  guides(fill = guide_legend(byrow = TRUE))
supp_asr
#ggsave("4_output/figs/supp_asr.svg",width=12.5,height=8.5,unit="in")

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
  scale_fill_discrete(type = blues,na.value = "grey80")+
  facet_grid(rows = vars(race),cols = vars(cohort))+
  geom_text(aes(label = round(pct,3)),
            color = "black", size = 3)+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years","18-26 years",
                              "27-36 years","37-46 years","47-56 years",
                              "57+ years"))+
  labs(fill = "Proportion",
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

# --- Ab-c representation ratio ---
abcpopr_count<-merge(abc_dtasr,a_asr,by = c("age_groups","sex","race"),all.x = T)
colnames(abcpopr_count)[5]<-"pct_abc"#percentage of total abc samples in each subgroup
abcpopr_count<-abcpopr_count[order(abcpopr_count$age_groups,abcpopr_count$sex,
                                   abcpopr_count$race),]
abcpopr_count$pct_pop<-c(abcpopr_count$count_census/sum(abcpopr_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates ab-c sample adequately represents the corresponding population subgroup
abcpopr_count$rep_ratio<-abcpopr_count$pct_abc / abcpopr_count$pct_pop

# --- CLSA representation ratio ---
clsapopr_count<-merge(clsa_dtasr,g_asr,by = c("age_groups","sex","race"),all.x = T)
colnames(clsapopr_count)[5]<-"pct_clsa" #percentage of total clsa samples in each subgroup
clsapopr_count<-clsapopr_count[order(clsapopr_count$age_groups,clsapopr_count$sex,
                                   clsapopr_count$race),]
clsapopr_count$pct_pop<-c(clsapopr_count$count_census/sum(clsapopr_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates clsa sample adequately represents the corresponding population subgroup
clsapopr_count$rep_ratio<-clsapopr_count$pct_clsa / clsapopr_count$pct_pop

# --- Canpath representation ratio ---
canpopr_count<-merge(can_dtasr,d_asr,by = c("age_groups","sex","race"),all.x = T)
colnames(canpopr_count)[5]<-"pct_can"#percentage of total can samples in each subgroup
canpopr_count<-canpopr_count[order(canpopr_count$age_groups,canpopr_count$sex,
                                   canpopr_count$race),]
canpopr_count$pct_pop<-c(canpopr_count$count_census/sum(canpopr_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates Canpath sample adequately represents the corresponding population subgroup
canpopr_count$rep_ratio<-canpopr_count$pct_can / canpopr_count$pct_pop

#Add in synthetic APL data for visualization purposes only
apl_synth<-asr_synth %>% 
  mutate(count = NA,pct = NA,cohort = "APL outpatient laboratory",
         count_census = NA,pct_pop = NA,rep_ratio = NA)

#Plot datasets in a single plot
cbspopr_count$cohort<-"CBS blood donor"
abcpopr_count$cohort<-"Ab-c open cohort"
clsapopr_count$cohort<-"CLSA closed cohort"
canpopr_count$cohort<-"Canpath closed cohort"
ccapopr_count<-ccapopr_count %>% 
  mutate(cohort = "CCAHS-1 closed cohort")
colnamesr<-c("age_groups","sex","race","rep_ratio","cohort")
allpopr_count<-do.call("rbind",list(cbspopr_count[,colnamesr],
                                    abcpopr_count[,colnamesr],
                                    clsapopr_count[,colnamesr],
                                    canpopr_count[,colnamesr],
                                    apl_synth[,colnamesr],
                                    ccapopr_count[,colnamesr]))

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
  all_dtsqm$count <= 1000 ~ "Count \u2264 1,000",
  all_dtsqm$count > 1000 & all_dtsqm$count <= 5000 ~ "1,000 < Count \u2264 5,000",
  all_dtsqm$count > 5000 & all_dtsqm$count <= 10000 ~ "5,000 < Count \u2264 10,000",
  all_dtsqm$count > 10000 & all_dtsqm$count <= 20000 ~ "10,000 < Count \u2264 20,000",
  all_dtsqm$count > 20000 & all_dtsqm$count <= 50000 ~ "20,000 < Count \u2264 50,000",
  all_dtsqm$count > 50000 & all_dtsqm$count <= 100000 ~ "50,000 < Count \u2264 100,000",
  all_dtsqm$count > 100000 ~ "Count > 100,000",
  TRUE ~ NA),
  levels = c("Count \u2264 1,000",
             "1,000 < Count \u2264 5,000",
             "5,000 < Count \u2264 10,000",
             "10,000 < Count \u2264 20,000",
             "20,000 < Count \u2264 50,000",
             "50,000 < Count \u2264 100,000",
             "Count > 100,000"))

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

all_dtsqm<-all_dtsqm %>% 
  mutate(cohort = case_when(
    cohort == "CBS blood donor" ~ "CBS blood\ndonor",
    cohort == "APL outpatient laboratory" ~ "APL outpatient\nlaboratory",
    cohort == "Ab-c open cohort" ~ "Ab-c open\ncohort",
    cohort == "Canpath closed cohort" ~ "Canpath closed\ncohort",
    cohort == "CLSA closed cohort" ~ "CLSA closed\ncohort",
    cohort == "CCAHS-1 closed cohort" ~ "CCAHS-1 closed\ncohort",
    TRUE ~ NA))

all_dtsqm$cohort<-factor(all_dtsqm$cohort,
                         levels = c(
                           "CBS blood\ndonor","APL outpatient\nlaboratory",
                           "Ab-c open\ncohort","Canpath closed\ncohort",
                           "CLSA closed\ncohort"))

supp_qm<-ggplot(all_dtsqm,aes(x = sex, y = factor(quintmat,
                                         levels = c(1,2,3,
                                                    4,5)),
                     fill = count_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = blues[c(1,2,4,6,7)])+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = scales::number(count,big.mark = ",")),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Material deprivation quintile",
       x = "Sex")+
  theme(legend.text = element_text(size = 7.0),
        legend.position = "right",
        legend.margin = margin(rep(0,4)),
        legend.box.margin = margin(rep(0,4)),
        legend.spacing.x = unit(0.1,"cm"),
        legend.spacing.y = unit(0.3,"cm"),
        plot.margin = unit(rep(0.00,4),"cm"),
        strip.text = element_text(size = 8.0),
        axis.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.title = element_text(size = 9),
        axis.text.y = element_text(margin = margin(r = 0.8)))+
  guides(fill = guide_legend(byrow = T))
supp_qm
#ggsave("4_output/figs/supp_sqm.svg",width=5.0,height=4.0,unit="in")

ggplot(all_dtsqm,aes(x = sex, y = factor(quintmat,
                                         levels = c(1,2,
                                                    3,4,5)), 
                     fill = pct_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = blues[c(1,2,4,6,7)])+
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

# --- APL representation ratio ---
aplpopqm_count<-merge(apl_sqm1,e_sqm,by = c("sex","quintmat"),all.x = T)
colnames(aplpopqm_count)[4]<-"pct_apl" #percentage of total apl samples in each subgroup
aplpopqm_count<-aplpopqm_count[order(aplpopqm_count$quintmat,aplpopqm_count$sex),]
aplpopqm_count$pct_pop<-c(aplpopqm_count$count_census / sum(aplpopqm_count$count_census))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
aplpopqm_count$rep_ratio<-aplpopqm_count$pct_apl / aplpopqm_count$pct_pop

#Plot datasets in a single plot
cbspopqm_count$cohort<-"CBS blood donor"
aplpopqm_count$cohort<-"APL outpatient laboratory"
ccapopqm_count<-ccapopqm_count %>% 
  filter(quintmat != "All quintiles") %>%
  mutate(cohort = "CCAHS-1 closed cohort")
namesqm<-c("sex","quintmat","cohort","rep_ratio")
allpopqm_count<-do.call("rbind",list(cbspopqm_count[,namesqm],aplpopqm_count[,namesqm],
                                     ccapopqm_count[,namesqm]))

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
cbs_sqs1$cohort<-"CBS blood donor"
apl_sqs1$cohort<-"APL outpatient laboratory"
all_dtsqs<-do.call("rbind",list(cbs_sqs1,apl_sqs1))

#Create count binned for visualization
all_dtsqs$count_binned<-factor(case_when(
  all_dtsqs$count <= 1000 ~ "Count \u2264 1,000",
  all_dtsqs$count > 1000 & all_dtsqs$count <= 5000 ~ "1,000 < Count \u2264 5,000",
  all_dtsqs$count > 5000 & all_dtsqs$count <= 10000 ~ "5,000 < Count \u2264 10,000",
  all_dtsqs$count > 10000 & all_dtsqs$count <= 20000 ~ "10,000 < Count \u2264 20,000",
  all_dtsqs$count > 20000 & all_dtsqs$count <= 50000 ~ "20,000 < Count \u2264 50,000",
  all_dtsqs$count > 50000 & all_dtsqs$count <= 100000 ~ "50,000 < Count \u2264 100,000",
  all_dtsqs$count > 100000 ~ "Count > 100,000",
  TRUE ~ NA),
  levels = c("Count \u2264 1,000",
             "1,000 < Count \u2264 5,000",
             "5,000 < Count \u2264 10,000",
             "10,000 < Count \u2264 20,000",
             "20,000 < Count \u2264 50,000",
             "50,000 < Count \u2264 100,000",
             "Count > 100,000"))

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

all_dtsqs<-all_dtsqs %>% 
  mutate(cohort = case_when(
    cohort == "CBS blood donor" ~ "CBS blood\ndonor",
    cohort == "APL outpatient laboratory" ~ "APL outpatient\nlaboratory",
    cohort == "Ab-c open cohort" ~ "Ab-c open\ncohort",
    cohort == "Canpath closed cohort" ~ "Canpath closed\ncohort",
    cohort == "CLSA closed cohort" ~ "CLSA closed\ncohort",
    cohort == "CCAHS-1 closed cohort" ~ "CCAHS-1 closed\ncohort",
    TRUE ~ NA))

all_dtsqs$cohort<-factor(all_dtsqs$cohort,
                         levels = c(
                           "CBS blood\ndonor","APL outpatient\nlaboratory",
                           "Ab-c open\ncohort","Canpath closed\ncohort",
                           "CLSA closed\ncohort"))

supp_sqs<-ggplot(all_dtsqs,aes(x = sex, y = factor(quintsoc,
                                         levels = c(1,2,3,
                                                    4,5)),
                     fill = count_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = blues[c(1,2,4,6,7)])+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = scales::number(count,big.mark = ",")),color = "black",size = 3.0)+
  labs(fill = "Count",
       y = "Social deprivation quintile",
       x = "Sex")+
  theme(legend.text = element_text(size = 7.0),
        legend.position = "right",
        legend.margin = margin(rep(0,4)),
        legend.box.margin = margin(rep(0,4)),
        legend.spacing.x = unit(0.1,"cm"),
        legend.spacing.y = unit(0.3,"cm"),
        plot.margin = unit(rep(0.00,4),"cm"),
        strip.text = element_text(size = 8.0),
        axis.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.title = element_text(size = 9),
        axis.text.y = element_text(margin = margin(r = 0.8)))+
  guides(fill = guide_legend(byrow = T))
supp_sqs
#ggsave("4_output/figs/supp_sqs.svg",width=5.0,height=4.0,unit="in")

ggplot(all_dtsqs,aes(x = sex, y = factor(quintsoc,
                                         levels = c(1,2,
                                                    3,4,5)), 
                     fill = pct_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  scale_fill_discrete(type = blues[c(1,2,4,6,7)])+
  facet_grid(cols = vars(cohort))+
  geom_text(aes(label = round(pct,3)),color = "black",size = 3.0)+
  labs(fill = "Proportion",
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

# --- APL representation ratio ---
aplpopqs_count<-merge(apl_sqs1,e_sqs,by = c("sex","quintsoc"),all.x = T)
colnames(aplpopqs_count)[4]<-"pct_apl" #percentage of total apl samples in each subgroup
aplpopqs_count<-aplpopqs_count[order(aplpopqs_count$quintsoc,aplpopqs_count$sex),]
aplpopqs_count$pct_pop<-c(aplpopqs_count$count_census / sum(aplpopqs_count$count_census))

#Calculate representation ratio -- value = 1 indicates apl sample adequately represents the corresponding population subgroup
aplpopqs_count$rep_ratio<-aplpopqs_count$pct_apl / aplpopqs_count$pct_pop

#Plot datasets in a single plot
cbspopqs_count$cohort<-"CBS blood donor"
aplpopqs_count$cohort<-"APL outpatient laboratory"
ccapopqs_count<-ccapopqs_count %>% 
  filter(quintsoc != "All quintiles") %>%
  mutate(cohort = "CCAHS-1 closed cohort")
namesqs<-c("sex","quintsoc","cohort","rep_ratio")
allpopqs_count<-do.call("rbind",list(cbspopqs_count[,namesqs],aplpopqs_count[,namesqs],
                                     ccapopqs_count[,namesqs]))

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
  mutate(cohort = "CCAHS-1 closed\ncohort")
allpopast_count<-ccapopast_count[,c("sex","age_groups","cohort","rep_ratio")]

#Read in bootstrap results
ccaast<-read_xlsx("./1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_bootstrap/as_territories_5000.xlsx")
ccaast<-ccaast %>% 
  mutate(cohort = "CCAHS-1 closed\ncohort",
         age_groups = ccahs_age(age_groups))
colnames(ccaast)[5]<-"rr_prob"
boot_ast<-ccaast[,2:5]

allpopast_count<-merge(allpopast_count,boot_ast,by = c("sex","age_groups","cohort"),all.x = T)
allpopast_count<-allpopast_count %>% 
  mutate(age_groups = factor(age_groups,
                             levels = c("All ages","0-17 years","18-26 years",
                                        "27-36 years","37-46 years","47-56 years",
                                        "57+ years")),
         rr_binned = factor(rr_binned_fun(rep_ratio),
                            levels = c("Strongly underrepresented (RR < 1/2)",
                                       "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                       "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                       "Moderately overrepresented (4/3 < RR \u2264 2)",
                                       "Strongly overrepresented (RR > 2)")))

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
    TRUE ~ NA),
  rep_ratio = round(as.numeric(rep_ratio),1))

f<-f %>% mutate(
  strata = factor(strata,
                  levels = c("All","Rural","Urban","Racialized\nminority",
                             "White")),
  rr_binned = factor(rr_binned_fun(rep_ratio),
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
                                 "57+ years"))
)
  

f1<-f %>% filter(age_groups == "All ages")
f2<-f %>% filter(age_groups != "All ages")

allpopqm_count$rep_ratio<-round(as.numeric(allpopqm_count$rep_ratio),1)
f2_qm<-allpopqm_count %>% 
  mutate(rr_binned = factor(rr_binned_fun(rep_ratio),
                            levels = c("Strongly underrepresented (RR < 1/2)",
                                       "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                       "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                       "Moderately overrepresented (4/3 < RR \u2264 2)",
                                       "Strongly overrepresented (RR > 2)")))
allpopqs_count$rep_ratio<-round(as.numeric(allpopqs_count$rep_ratio),1)
f2_qs<-allpopqs_count %>% 
  mutate(rr_binned = factor(rr_binned_fun(rep_ratio),
                            levels = c("Strongly underrepresented (RR < 1/2)",
                                       "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                       "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                       "Moderately overrepresented (4/3 < RR \u2264 2)",
                                       "Strongly overrepresented (RR > 2)")))

cols<-c("#E16A86","#FFC5D0","#009ADE","#A8E1BF","#50A315")
#Figure 1
f1_p<-ggplot(f1,aes(x = sex,y = age_groups,
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 3.2)+
  facet_grid(rows = vars(strata),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented\n(RR < 1/2)",
                               "Moderately underrepresented\n(1/2 ≤ RR < 3/4)",
                               "Adequately represented\n(3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented\n(4/3 < RR ≤ 2)",
                               "Not applicable"),
                    na.value = "grey80")+
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.spacing.x = unit(0.2,"cm"),
        legend.text = element_text(size = 8.5),
        legend.margin = margin(t = 0.1,l = 0.1),
        legend.key.size = unit(0.20,"inch"))+
  guides(fill = guide_legend(title = "Representativeness\n Ratio (RR)",
                             title.position = "left",
                             label.position = "right",
                             nrow = 2,byrow = T))
f1_p<-cowplot::plot_grid(f1_p)
#ggsave("4_output/figs/f1.svg",width=7.5,height=5.5,unit="in")

#Quintsoc
s1_sqs<-ggplot(f2_qs,aes(x = sex,y = factor(quintsoc,levels = c(1:5)),
                         fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size =3.5)+
  facet_grid(cols = vars(cohort))+
  labs(fill = "Representativeness \nRatio",
       x = "Sex",
       y = "Social deprivation quintile")+
  scale_fill_manual(values = cols[2:4],
                    labels = c("Moderately underrepresented\n(1/2 ≤ RR < 3/4)",
                               "Adequately represented\n(3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented\n(4/3 < RR ≤ 2)"),
                    na.value = "grey80")+
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0.05,"cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.margin=margin(-0.5, 0, 0, 0),
        strip.text = element_text(size = 9.0),
        axis.title = element_blank())+
  guides(fill = guide_legend(label.position = "right",byrow = T))
s1_sqs
#ggsave("4_output/figs/s1_sqs.svg",width=7.0,height=4.0,unit="in")

#Territories by age-sex
ast_rr<-ggplot(allpopast_count,aes(x = sex,y = age_groups,
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 3.5)+
  facet_grid(cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented\n(RR < 1/2)",
                               "Moderately underrepresented\n(1/2 ≤ RR < 3/4)",
                               "Adequately represented\n(3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented\n(4/3 < RR ≤ 2)",
                               "Strongly overrepresented\n(RR > 2)",
                               "Not applicable"))+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years" = "0-17",
                              "18-26 years" = "18-26","27-36 years" = "27-36",
                              "37-46 years" = "37-46","47-56 years" = "47-56",
                              "57+ years" = "Age 57+"))+
  theme(axis.title = element_blank(),
        legend.spacing.x = unit(0.2,"cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8.5))+
  guides(fill = guide_legend(byrow = T))
ast_rr
#ggsave("4_output/figs/supp_astrr.svg",width = 6.0,height = 3.5)

#Figure 2 all ages
f2p<-ggplot(f2,aes(x = sex,y = age_groups,
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = F,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 3.2)+
  facet_grid(rows = vars(strata),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented\n(RR < 1/2)",
                               "Moderately underrepresented\n(1/2 ≤ RR < 3/4)",
                               "Adequately represented\n(3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented\n(4/3 < RR ≤ 2)",
                               "Strongly overrepresented\n(RR > 2)",
                               "Not applicable"),
                    na.value = "grey80")+
  scale_y_discrete(labels = c("0-17 years" = "0-17",
                              "18-26 years" = "18-26","27-36 years" = "27-36",
                              "37-46 years" = "37-46","47-56 years" = "47-56",
                              "57+ years" = "Age 57+"))+
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0.05,"cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.margin=margin(-0.5, 0, 0, 0),
        axis.title = element_blank())+
  guides(fill = guide_legend(label.position = "right",nrow = 2,byrow = T))
f2p
#Figure 2 quintmat
f2_qmp<-ggplot(f2_qm,aes(x = sex,y = factor(quintmat,levels = c(1:5)),
                                  fill = rr_binned))+
  geom_tile(color = "black",show.legend = F,linewidth = 0.1)+
  scale_fill_discrete(type = cols)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 3.2)+
  facet_grid(cols = vars(cohort))+
  labs(fill = "Representativeness Ratio")+
  theme(axis.title = element_blank(),
        plot.margin = margin(0.1,0.1,0.1,0.1))

#Combine plots
g1<-ggplotGrob(f2p)
g2<-ggplotGrob(f2_qmp)
w<-g1$heights[1]
g1$heights[1]<-w * 4 #shrink top graph by increasing size of white space on top
g2$widths[11]<-3.75*g1$widths[5] #shrink width of mat dep part
gtable::gtable_show_layout(g1)
g2$widths[4]<-g2$widths[4] * 4.73 #shift bottom portion to the right while keeping axis label the same
g2$widths[5]<-g1$widths[5] * 1.05 #widen first panel
g2$widths[7]<-g1$widths[7] * 1.05 #widen second panel
g2$widths[9]<-g1$widths[9] * 1.05 #widen third panel
g2$heights[9]<-g1$heights[7] * 7
gridExtra::grid.arrange(g1,g2)
f2f<-gridExtra::grid.arrange(g1,g2)

#ggsave("4_output/figs/f2.svg",plot = f2f,width=8,height=10,unit="in")

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

abc_dtasr1$cohort<-"Ab-c open\ncohort"
clsa_dtasr1$cohort<-"CLSA closed\ncohort"
can_dtasr1$cohort<-"Canpath closed\ncohort"
all_dtasr1<-do.call("rbind",list(abc_dtasr1,clsa_dtasr1,can_dtasr1))

all_dtasr1$cohort<-factor(all_dtasr1$cohort,
                          levels = c("Ab-c open\ncohort",
                                     "Canpath closed\ncohort",
                                     "CLSA closed\ncohort"
                          ))
all_dtasr1$count_binned<-factor(case_when(
  all_dtasr1$count <= 1000 ~ "Count \u2264 1,000",
  all_dtasr1$count > 1000 & all_dtasr1$count <= 5000 ~ "1,000 < Count \u2264 5,000",
  all_dtasr1$count > 5000 & all_dtasr1$count <= 10000 ~ "5,000 < Count \u2264 10,000",
  all_dtasr1$count > 10000 & all_dtasr1$count <= 20000 ~ "10,000 < Count \u2264 20,000",
  all_dtasr1$count > 20000 & all_dtasr1$count <= 50000 ~ "20,000 < Count \u2264 50,000",
  all_dtasr1$count > 50000 & all_dtasr1$count <= 100000 ~ "50,000 < Count \u2264 100,000",
  all_dtasr1$count > 100000 ~ "Count > 100,000",
  TRUE ~ NA),
  levels = c("Count \u2264 1,000",
             "1,000 < Count \u2264 5,000",
             "5,000 < Count \u2264 10,000",
             "10,000 < Count \u2264 20,000",
             "20,000 < Count \u2264 50,000",
             "50,000 < Count \u2264 100,000",
             "Count > 100,000"))

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

supp_sensr1<-ggplot(all_dtasr1,aes(x = sex, y = factor(age_groups,
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
  geom_text(aes(label = scales::number(count,big.mark = ",")),
            color = "black", size = 3.25)+
  scale_y_discrete(labels = c("All ages" = expression(bold("All ages")),"0-17 years" = "0-17",
                              "18-26 years" = "18-26","27-36 years" = "27-36",
                              "37-46 years" = "37-46","47-56 years" = "47-56",
                              "57+ years" = "Age 57+"))+
  labs(fill = "Count",
       y = "Age Group",
       x = "Sex")+
  theme(legend.text = element_text(size = 7.0),
        legend.position = "right",
        legend.margin = margin(rep(0,4)),
        legend.box.margin = margin(rep(0,4)),
        legend.spacing.x = unit(0.1,"cm"),
        legend.spacing.y = unit(0.3,"cm"),
        plot.margin = unit(rep(0.00,4),"cm"),
        strip.text = element_text(size = 8.0),
        axis.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.title = element_text(size = 9),
        axis.text.y = element_text(margin = margin(r = 0.1)))+
  guides(fill = guide_legend(byrow = T))
supp_sensr1
#ggsave("4_output/figs/supp_sensr1.svg",width=6.5,height=5.0,unit="in")

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
  scale_fill_discrete(type = blues,na.value = "grey80")+
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
abcpopr_count1$cohort<-"Ab-c open cohort"

# --- Canpath representation ratio ---
canpopr_count1<-merge(can_dtasr1,d_asr1,by = c("age_groups","sex","race1"),all.x = T)
colnames(canpopr_count1)[5]<-"pct_can" #percentage of total canpath samples in each subgroup
canpopr_count1<-canpopr_count1[order(canpopr_count1$age_groups,canpopr_count1$sex,
                                     canpopr_count1$race1),]
canpopr_count1$pct_pop<-c(canpopr_count1$count_census / sum(canpopr_count1$count_census[1:24],na.rm = T))

#Calculate representation ratio
canpopr_count1$rep_ratio<-canpopr_count1$pct_can / canpopr_count1$pct_pop
canpopr_count1$cohort<-"Canpath closed cohort"

# --- CLSA representation ratio ---
clsapopr_count1<-merge(clsa_dtasr1,g_asr1,by = c("age_groups","sex","race1"),all.x = T)
colnames(clsapopr_count1)[5]<-"pct_clsa" #percentage of total clsa samples in each subgroup
clsapopr_count1<-clsapopr_count1[order(clsapopr_count1$age_groups,clsapopr_count1$sex,
                                     clsapopr_count1$race1),]
clsapopr_count1$pct_pop<-c(clsapopr_count1$count_census / sum(clsapopr_count1$count_census[1:24],na.rm = T))

#Calculate representation ratio
clsapopr_count1$rep_ratio<-clsapopr_count1$pct_clsa / clsapopr_count1$pct_pop
clsapopr_count1$cohort<-"CLSA closed cohort"

#Combine all into 1 dataframe
ccapopr_count1<-ccapopr_count1 %>% 
  mutate(cohort = "CCAHS-1 closed cohort",
         race1 = race)
colnamesr_1<-c("age_groups","sex","race1","rep_ratio","cohort")
allpopr_count1<-do.call("rbind",list(abcpopr_count1[,colnamesr_1],
                                     clsapopr_count1[,colnamesr_1],
                                     canpopr_count1[,colnamesr_1],
                                     ccapopr_count1[,colnamesr_1]))

#Join in results of bootstrap analysis
abcr1<-read.csv("./1_data/private/boot_abc_asr1_5000.csv")
clsa1<-read.csv("./1_data/private/boot_clsa_asr1_5000.csv")
can1<-read.csv("./1_data/private/boot_can_asr1_5000.csv")
ccar1<-read_xlsx("1_data/private/2016 Canadian Census/10285/Sortie_CCAHS_Census_bootstrap/asr1_provinces_5000.xlsx") %>% 
  mutate(age_groups = ccahs_age(age_groups),
         cohort = "CCAHS-1 closed cohort")
colnames(ccar1)[6]<-"rr_prob"
boot_race1<-do.call("rbind",list(abcr1,clsa1,can1,ccar1[,2:6]))

allpopr_count1<-merge(allpopr_count1,boot_race1,by = c("age_groups","sex","race1","cohort"),all.x = T)

#Create alternate versions of figs 1 and 2 for sensitivity analysis 1
allpopr_count1<-allpopr_count1 %>% 
  mutate(
    cohort = case_when(
      cohort == "Ab-c open cohort" ~ "Ab-c open\ncohort",
      cohort == "Canpath closed cohort" ~ "Canpath closed\ncohort",
      cohort == "CLSA closed cohort" ~ "CLSA closed\ncohort",
      cohort == "CCAHS-1 closed cohort" ~ "CCAHS-1 closed\ncohort",
      TRUE ~ NA),
    race1 = case_when(
      race1 == "White" ~ "White",
      race1 == "Racialized minority" ~ "Racialized\nminority",
      TRUE ~ NA),
    rep_ratio = round(as.numeric(rep_ratio),1)
  )

allpopr_count1<-allpopr_count1 %>% 
  mutate(
    rr_binned = factor(rr_binned_fun(rep_ratio),
                       levels = c("Strongly underrepresented (RR < 1/2)",
                                  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
                                  "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
                                  "Moderately overrepresented (4/3 < RR \u2264 2)",
                                  "Strongly overrepresented (RR > 2)")),
    cohort=factor(cohort, levels = c("CCAHS-1 closed\ncohort",
                                     "Ab-c open\ncohort","Canpath closed\ncohort",
                                     "CLSA closed\ncohort")),
    age_groups = factor(age_groups,
                        levels = c("All ages","0-17 years","18-26 years",
                                   "27-36 years","37-46 years","47-56 years",
                                   "57+ years"))
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
  scale_fill_manual(values = cols[c(1,3,4)],
                    labels = c("Strongly underrepresented\n(RR < 1/2)",
                                "Adequately represented\n(3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented\n (4/3 < RR ≤ 2)"))+
  scale_y_discrete(labels = c("0-17 years" = "0-17",
                              "18-26 years" = "18-26","27-36 years" = "27-36",
                              "37-46 years" = "37-46","47-56 years" = "47-56",
                              "57+ years" = "Age 57+"))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        legend.spacing.x = unit(0.4,"cm"))+
  guides(fill = guide_legend(label.position = "right"))

supp_sensr1rr<-ggplot(f2_s1,aes(x = sex,y = age_groups,
                 fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 4.0)+
  facet_grid(rows = vars(race1),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")+
  scale_y_discrete(labels = c("0-17 years" = "0-17",
                              "18-26 years" = "18-26","27-36 years" = "27-36",
                              "37-46 years" = "37-46","47-56 years" = "47-56",
                              "57+ years" = "Age 57+"))+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented\n(RR < 1/2)",
                               "Moderately underrepresented\n(1/2 ≤ RR < 3/4)",
                               "Adequately represented\n(3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented\n(4/3 < RR ≤ 2)",
                               "Strongly overrepresented\n(RR > 2)",
                               "Not applicable"),
                    na.value = "grey80")+
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0.1,"cm"),
        legend.text = element_text(size = 9),
        axis.title = element_blank())+
  guides(fill = guide_legend(byrow = T,label.position = "right"))
supp_sensr1rr
#ggsave("4_output/figs/supp_sensr1rr.svg",width = 8, height = 6.0,units = "in")

# Sensitivity analysis #2: ------------------------------------------------
#Age-sex-urban
#-- CLSA representation ratio --
clsapops2_count<-merge(clsa_dtasu,g_asus2,by = c("age_groups","sex","urban"),all.x = T)
colnames(clsapops2_count)[5]<-"pct_clsa" #percentage of total clsa samples in each subgroup
clsapops2_count<-clsapops2_count[order(clsapops2_count$age_groups,clsapops2_count$sex,
                                   clsapops2_count$urban),]
clsapops2_count$pct_pop<-c(clsapops2_count$count_census / sum(clsapops2_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates cbs sample adequately represents the corresponding population subgroup
clsapops2_count$rep_ratio<-clsapops2_count$pct_clsa / clsapops2_count$pct_pop
clsapops2_count$cohort<-"CLSA closed cohort"

#-- Canpath representation ratio --
canpops2_count<-merge(can_dtasu,d_asus2,by = c("age_groups","sex","urban"),all.x = T)
colnames(canpops2_count)[5]<-"pct_can" #percentage of total can samples in each subgroup
canpops2_count<-canpops2_count[order(canpops2_count$age_groups,canpops2_count$sex,
                                 canpops2_count$urban),]
canpops2_count$pct_pop<-c(canpops2_count$count_census / sum(canpops2_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates Canpath sample adequately represents the corresponding population subgroup
canpops2_count$rep_ratio<-canpops2_count$pct_can / canpops2_count$pct_pop
canpops2_count$cohort<-"Canpath closed cohort"

#Merge datasets together and join with bootstrap results
allpopus2_count<-do.call("rbind",list(clsapops2_count[,colnames],
                                      canpops2_count[,colnames]))

clsaus2<-read.csv("./1_data/private/boot_clsa_asu_5000_s2.csv")
canus2<-read.csv("./1_data/private/boot_can_asu_5000_s2.csv")
boot_urbans2<-do.call("rbind",list(clsaus2,canus2))

allpopus2_count<-merge(allpopus2_count,boot_urbans2,by = c("age_groups","sex","urban","cohort"),
                    all.x = T)
#Age-sex-race
# --- CLSA representation ratio ---
clsapoprs2_count<-merge(clsa_dtasr,g_asrs2,by = c("age_groups","sex","race"),all.x = T)
colnames(clsapoprs2_count)[5]<-"pct_clsa" #percentage of total clsa samples in each subgroup
clsapoprs2_count<-clsapoprs2_count[order(clsapoprs2_count$age_groups,clsapoprs2_count$sex,
                                     clsapoprs2_count$race),]
clsapoprs2_count$pct_pop<-c(clsapoprs2_count$count_census/sum(clsapoprs2_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates clsa sample adequately represents the corresponding population subgroup
clsapoprs2_count$rep_ratio<-clsapoprs2_count$pct_clsa / clsapoprs2_count$pct_pop
clsapoprs2_count$cohort<-"CLSA closed cohort"

# --- Canpath representation ratio ---
canpoprs2_count<-merge(can_dtasr,d_asrs2,by = c("age_groups","sex","race"),all.x = T)
colnames(canpoprs2_count)[5]<-"pct_can"#percentage of total can samples in each subgroup
canpoprs2_count<-canpoprs2_count[order(canpoprs2_count$age_groups,canpoprs2_count$sex,
                                   canpoprs2_count$race),]
canpoprs2_count$pct_pop<-c(canpoprs2_count$count_census/sum(canpoprs2_count$count_census[1:24],na.rm = T))

#Calculate representation ratio -- value = 1 indicates Canpath sample adequately represents the corresponding population subgroup
canpoprs2_count$rep_ratio<-canpoprs2_count$pct_can / canpoprs2_count$pct_pop
canpoprs2_count$cohort<-"Canpath closed cohort"

allpoprs2_count<-do.call("rbind",list(clsapoprs2_count[,colnamesr],
                                    canpoprs2_count[,colnamesr]))

clsars2<-read.csv("./1_data/private/boot_clsa_asr_5000_s2.csv")
canrs2<-read.csv("./1_data/private/boot_can_asr_5000_s2.csv")

boot_races2<-do.call("rbind",list(clsars2,canrs2))

allpoprs2_count<-merge(allpoprs2_count,boot_races2,by = c("age_groups","sex","race","cohort"),
                     all.x = T)

#Plot
colnames(allpopus2_count)[3]<-"strata"
colnames(allpoprs2_count)[3]<-"strata"
fs2<-rbind(allpopus2_count,allpoprs2_count)
fs2<-fs2 %>% mutate(
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
    TRUE ~ NA),
  rep_ratio = round(as.numeric(rep_ratio),1)
)

fs2<-fs2 %>% mutate(
  strata = factor(strata,
                  levels = c("All","Rural","Urban","Racialized\nminority",
                             "White")),
  rr_binned = factor(rr_binned_fun(rep_ratio),
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
                                 "57+ years")))

f1s2<-fs2 %>% filter(age_groups == "All ages")
f2s2<-fs2 %>% filter(age_groups != "All ages")

cols<-c("#E16A86","#FFC5D0","#009ADE","#A8E1BF","#50A315")
#Figure 1
ggplot(f1s2,aes(x = sex,y = age_groups,
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 2.5)+
  facet_grid(rows = vars(strata),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented\n(RR < 1/2)",
                               "Moderately underrepresented\n (1/2 ≤ RR < 3/4)",
                               "Adequately represented\n (3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented\n (4/3 < RR ≤ 2)",
                               "Not applicable"),
                    na.value = "grey80")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        legend.spacing.x = unit(0.4,"cm"))+
  guides(fill = guide_legend(label.position = "right"))

#Figure 2 all ages
supp_sensr2rr<-ggplot(f2s2,aes(x = sex,y = age_groups,
              fill = rr_binned))+
  geom_tile(color = "black",show.legend = T,linewidth = 0.1)+
  geom_text(aes(label = rep_ratio,
                fontface = ifelse(rr_prob > 0.95,2,1)),
            color = "black",size = 3.0)+
  facet_grid(rows = vars(strata),cols = vars(cohort))+
  labs(fill = "Representativeness\n Ratio (RR)",
       x = "Sex",
       y = "Age group")+
  scale_fill_manual(values = cols,
                    labels = c("Strongly underrepresented\n(RR < 1/2)",
                               "Moderately underrepresented\n(1/2 ≤ RR < 3/4)",
                               "Adequately represented\n(3/4 ≤ RR ≤ 4/3)",
                               "Moderately overrepresented\n(4/3 < RR ≤ 2)",
                               "Strongly overrepresented\n(RR > 2)",
                               "Not applicable"),
                    na.value = "grey80")+
  scale_y_discrete(labels = c("0-17 years" = "0-17",
                              "18-26 years" = "18-26","27-36 years" = "27-36",
                              "37-46 years" = "37-46","47-56 years" = "47-56",
                              "57+ years" = "Age 57+"))+
  theme(legend.text = element_text(size = 7.5),
        legend.position = "bottom",
        legend.margin = margin(rep(0,4)),
        legend.box.margin = margin(rep(0,4)),
        legend.spacing.x = unit(0.1,"cm"),
        legend.spacing.y = unit(0.3,"cm"),
        legend.title = element_text(size = 10),
        axis.title = element_blank())+
  guides(fill = guide_legend(byrow = T,label.position = "right",nrow = 2))
supp_sensr2rr
#ggsave("4_output/figs/supp_sensr2rr.svg",width = 7.5, height = 6,units = "in")


# 0. Description ----------------------------------------------------------
# 1. Load libraries and data
# 2. Compute AARB
# 3. Compute Duncan Dissimilarity Index

# This script calculates the average absolute relative bias (AARB) and 
# Duncan dissimilarity index (DDI) for each study except CCAHS-1 (due to 
# data availability restrictions). Both metrics are computed monthly or 
# for periods where data are available.

# 1. Load libraries and data -------------------------------------------------
rm(list = ls())
library(tidyverse)

#Read in serosurveillance study datasets
cbs_df<-read.csv("./1_data/private/cbs_df_final.csv")
apl_df<-read.csv("./1_data/private/apl_df_final.csv")
abc_df<-read.csv("./1_data/private/abc_df_final.csv")
clsa_df<-read.csv("./1_data/private/clsa_df_final.csv")
can_df<-read.csv("./1_data/private/can_df_final.csv")

#Read in 2016 census datasets (urban)
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
g_sqm<-read.csv("./1_data/private/2016 Canadian Census//censussqm_g_clsa.csv") #CLSA

#Read in 2016 census datasets (quintsoc)
c_sqs<-read.csv("./1_data/private/2016 Canadian Census/censussqs_c_cbs.csv") #CBS
e_sqs<-read.csv("./1_data/private/2016 Canadian Census/censussqs_e_apl.csv") #APL
g_sqs<-read.csv("./1_data/private/2016 Canadian Census//censussqs_g_clsa.csv") #CLSA

#Load functions
source("2_scripts/00_Helper_Functions.R")

# 2. Compute AARB -----------------------------------------------------------------
# Remove observations from the territories (as these metrics are only for the primary analysis) 
abc_df <- abc_df %>% filter(province != "YT")

# Compute AARB for each study and store in combined df
aarb_cbs <- compute_aarb_fun(study_df = cbs_df,census_df_asu = c_asu,
                             census_df_asr = c_asr, census_df_qm = c_sqm, 
                             census_df_qs = c_sqs)

aarb_apl <- compute_aarb_fun(study_df = apl_df,census_df_asu = e_asu,
                             census_df_asr = NULL, census_df_qm = e_sqm, 
                             census_df_qs = e_sqs)

aarb_abc <- compute_aarb_fun(study_df = abc_df,census_df_asu = a_asu,
                             census_df_asr = a_asr, census_df_qm = NULL, 
                             census_df_qs = NULL)

aarb_clsa <- compute_aarb_fun(study_df = clsa_df,census_df_asu = g_asu,
                             census_df_asr = g_asr, census_df_qm = g_sqm, 
                             census_df_qs = g_sqs)

aarb_can <- compute_aarb_fun(study_df = can_df,census_df_asu = d_asu,
                             census_df_asr = d_asr, census_df_qm = NULL, 
                             census_df_qs = NULL)

aarb_df <- do.call("rbind",list(aarb_cbs, aarb_apl, aarb_abc, aarb_clsa, aarb_can))

#write_csv(aarb_df, file = "./1_data/private/aarb_analysis.csv")

# 3. Compute Duncan Dissimilarity Index --------------------------------------
# Compute for each study and store in combined df
ddi_cbs <- compute_ddi_fun(study_df = cbs_df,census_df_asu = c_asu,
                           census_df_asr = c_asr, census_df_qm = c_sqm, 
                           census_df_qs = c_sqs) %>% 
  mutate(study = "cbs") %>% 
  tidyr::pivot_longer(Age:Quintsoc,names_to = "variable", values_to = "ddi")

ddi_apl <- compute_ddi_fun(study_df = apl_df,census_df_asu = e_asu,
                           census_df_asr = NULL, census_df_qm = e_sqm, 
                           census_df_qs = e_sqs) %>% 
  mutate(study = "apl") %>% 
  tidyr::pivot_longer(Age:Quintsoc,names_to = "variable", values_to = "ddi")

ddi_abc <- compute_ddi_fun(study_df = abc_df,census_df_asu = a_asu,
                           census_df_asr = a_asr, census_df_qm = NULL, 
                           census_df_qs = NULL)%>% 
  mutate(study = "Ab-C") %>% 
  tidyr::pivot_longer(Age:Race,names_to = "variable", values_to = "ddi")

ddi_clsa <- compute_ddi_fun(study_df = clsa_df,census_df_asu = g_asu,
                            census_df_asr = g_asr, census_df_qm = g_sqm, 
                            census_df_qs = g_sqs)%>% 
  mutate(study = "CLSA") %>% 
  tidyr::pivot_longer(Age:Quintsoc, names_to = "variable", values_to = "ddi")

ddi_can <- compute_ddi_fun(study_df = can_df,census_df_asu = d_asu,
                           census_df_asr = d_asr, census_df_qm = NULL, 
                           census_df_qs = NULL) %>% 
  mutate(study = "CanPath") %>% 
  tidyr::pivot_longer(Age:Race,names_to = "variable", values_to = "ddi")

ddi_df <- do.call("rbind",list(ddi_cbs, ddi_apl, ddi_abc, ddi_clsa, ddi_can))

#write_csv(ddi_df,file = "./1_data/private/ddi_analysis.csv")
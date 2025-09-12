# This script loads helper functions.

#Assign province of residence (version 1)
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

#Assign province of residence (version 2)
province_fun2 <- function(var) {
  p1 = as.integer(var)
  prov =  case_when(p1 == 1~ "AB",
                    p1 == 10~ "PE",
                    p1 == 7~ "NS",
                    p1 == 4~ "NB",
                    p1 == 11~ "QC",
                    p1 == 9 ~"ON",
                    p1 == 3~ "MB",
                    p1 == 12~ "SK",
                    p1 == 5~ "NL",
                    p1 == 2~ "BC",
                    p1 == 6~ "NT",
                    p1 == 8~ "NU",
                    p1 == 13~ "YT", 
                    TRUE~NA)
  return(prov)}

#Create age groups (version 1)
age_groups_fun <- function(variable){
  age_group = cut(variable,
                  breaks = c(0,18,27,37,47,57,
                             Inf),
                  labels = c("0-17 years","18-26 years","27-36 years",
                             "37-46 years","47-56 years","57+ years"),
                  right = FALSE)
  return(age_group)
}

#Clean census data
census_fun<-function(df){ #input dataframe
  df<-df[df$age_groups != "All ages",-1] #remove row id column and all ages category
  df$age_groups<-case_when(df$age_groups == "56+ years" ~ "57+ years",
                           df$age_groups == "< 18 years" ~ "0-17 years",
                           df$age_groups == "18-26 years" ~ "18-26 years",
                           df$age_groups == "27-36 years" ~ "27-36 years",
                           df$age_groups == "37-46 years" ~ "37-46 years",
                           df$age_groups == "47-56 years" ~ "47-56 years",
                           TRUE ~ NA)
  return(df)
}

#Clean CCAHS-1 age_groups variable
ccahs_age<-function(x){ #input age_groups column
  x<-case_when(x == "56+ years" ~ "57+ years",
               x == "< 18 years" ~ "0-17 years",
               x == "18-26 years" ~ "18-26 years",
               x == "27-36 years" ~ "27-36 years",
               x == "37-46 years" ~ "37-46 years",
               x == "47-56 years" ~ "47-56 years",
               x == "All ages" ~ "All ages",
               TRUE ~ NA)
  return(x)
}

#generate function to calculate proportion of RRs, for each subgroup, that are < 0.75
rrfun<-function(x){
  prop<-(sum(x < 0.75)) / length(x) #calculate proportion
  return(prop)
}

#Classify ethnicity as white or racialize minority
#Racialized minorities - no indigenous responses included
rm_indn<-c("Afro Latino Caribbean decent","East Indian descent, born in Kenya","Indo-Caribbean","white/asian",
           "SE Asian (Taiwanese)","i am South African, I carry in my veins European, African, Asian and indigenous Southern Afircan",
           "West Indian from Guyana","Guyanese - West Indian","South American","caucasian - european, indian, iraqi",
           "Ugandian of South Asian heritage","Trinidadian","White, Chinese, West Asian", "Japanese/Irish","Chinese, Indian, English",
           "East Indian born in Guyana","West-Indian","South African of mixed races","half Japanese half White",
           "Armenian","South Asian from East Africa","Indo Caribbean","Japanese and European","Black/white",
           "Mixed race Chinese-Guyanese-White","East Asian","west indian",
           "White, Indigeneous, Latin-American","Indian born in the Caribbean.",
           "White and Khoisan","European Hispanic","Armenian mother, Irish father, but born in England",
           "Persian","Guyanese,south asian descent","Mixed heritage (white and black)","Caribbean, Mixed Race",
           "Assyrian","Caucasian African","WEST INDIAN/CHINESE/","Latino and European","Of Indian decent - Born in Caribbean",
           "White/Southeast Asian","mixed european/chinese","Indo-caribbean","Mixed Race (Black, White, Hispanic,Soth Asian)",
           "South Asian/ Mixed South african","Canadian, German & Caribbean","Black Canadian",
           "East Indian","Mixed European & Southeast Asian","Je suis Afro Latino-AmÃ\u0083Â\u0083Ã\u0082Â©ricain","latino-amÃ\u0083Â\u0083Ã\u0082Â©ricain occidental",
           "Nord africain","Berbere","Berbere (Afrique du nord)","Berbere Nord Africain","amazigh","maghrebine","Nord-Africain",
           "Juif marocain","Nord africaine","90% white 10%Jewish-Arab","Canadian and Afrikaans",
           "mixed with Jamaican", "guianese of east indian descent",
           "caribbean chinese", "chinese and french background and other",
           "arabe-africaine", "berbère","persian",
           "indo-european","colombienne","japanese & swedish",
           "east indian","middle east",
           "armenian",
           "north african","south american, african, chinese","middle eastern",
           "middle east, and central europe","cherokee",
           "irish and guyanese","african","zimbawayan",
           "dutch indonesian",
           "trinidadian","south america - biracial",
           "punjabi","west indian of east indian descent",
           "mixed race - mother part black/father white caucasian","jamaican",
           "canadian chinese","cambodgienne","africain","asian","mauricien",
           "taiwanese","middle east/ persion",
           "1/4 middle eastern (syria which is now lebanon) from paternal grandfather",
           "mixed afghan and english","tunisien",
           "jamaican.","west  indian","zoroastrian (ancient persian ancestry)",
           "white/west asian","kabyle (berbère) d'afrique du nord","white mexican jewish")

#Racialized minorities - indigenous responses included
rm_indy<-c("Metis/European descent","White and aboriginal","Metis","Indigenous / European","Metis French Canadian",
           "Metis Canadian","Blanche ascendance micmak","Metis ancestry","white a bit of indigenous","métis",
           "french metis", "30% indigenous metis/70% white","metis",
           "culturally white, 3/16 aboriginal","mik'maw","inuit / european", "autochtone",
           "white a bit of indigenous","french metis",
           rm_indn)

#White or Caucasian
w<-c("white newfoundlander","White British",
     "White passing","Caucasian","White - British Decent",
     "White, British descent","Caucasian white", "White - Canadian Irish descent",
     "white french canadian", "Caucasian female","White - Canadian","caucasian","White Canadian Scottish parents","Canadian Caucasian","White British Descent",
     "Canadian - White - 4 generations",
     "white, British descent","CAUCASIAN",
     "Caucasian - European descent","Canadian white Caucasian","white Canadian" ,
     "Canadian White","White Franco Ontarian","Caucasian Canadian",
     "white  Canadian       North American", "White Canadian born","White Canadian, Parents of Ukrainian and French background",
     "White Anglo-Saxon","White/Cdn born/European grandparents",
     "Canadian, white","White caucasian","white not Europe.",
     "Hebrew (Jewish) Canadian white","White (Canadian)","White","White from Canada",
     "White  English decent=","White bilingual Canadian - Family from Quebec","white canadian","White  Canadian",
     "White Canadian, no known ethnicities","White (Causcasian)","white","blanc Canadienne","Canadienne blanche",
     "BLANCHE","Blanc.", "Blanc (Canada)","Canadienne blanc","Caucasien","Blanc - Canadien",
     "Blanc canadienne","Blanc du canada","Blanc canadien","Blanc canadian",
     "Blanche (Canadienne)","une femme de race blanche","blanc","Blanche","Blanc canada","Blanc","Blanche, Canadienne",
     "blanc quebecois","caucasien",
     "White & Canadian","White Canadian","blanc du canada","White- true blue CANADIAN","White Scottish Descent",
     "blanche canadienne","Blanc Canadien","canadien francais blanc",
     "white, scottish and other",
     "ccaucasian","caucasienne",
     "white european","white, mainly scottish/english/irish",
     "white, of european decent with mi'kmaq ancestors several generations removed.",
     "canadian caucasian", "white, norwegian and scottish",
     "canadien francais, caucasien","european caucasian",
     "white celtic", "white/ other")

#Compute AARB
compute_aarb_fun <- function(study_df, census_df_asu, census_df_asr, census_df_qm, census_df_qs) {
  
  input_name <- deparse(substitute(study_df))
  
  # Compute number of unique variable levels in dataset
  if (input_name == "cbs_df"){
    var_levels <- c(unique(study_df$age_groups),unique(study_df$sex),
                    unique(study_df$urban),unique(study_df$race),
                    unique(study_df$quintmat),unique(study_df$quintsoc))
    
    # Remove levels indicating missing values and store month of sample collection
    # in main df
    var_levels <- var_levels[!(var_levels %in% NA)]
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate,tz = "UTC"), unit = "1 month")
    study <- "CBS"
    
  } else if (input_name == "apl_df"){
    var_levels <- c(unique(study_df$age_groups),unique(study_df$sex),
                    unique(study_df$urban),unique(study_df$quintmat),
                    unique(study_df$quintsoc))
    
    # Remove levels indicating missing values and store month of sample collection
    # in main df
    var_levels <- var_levels[!(var_levels %in% NA)]
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate, tz = "UTC"),unit = "1 month")
    study <- "APL"
    
  } else if (input_name == "abc_df"){
    var_levels <- c(unique(study_df$age_groups),unique(study_df$sex),
                    unique(study_df$urban),unique(study_df$race))
    
    # Remove levels indicating missing values and store month of sample collection
    # in main df
    var_levels <- var_levels[!(var_levels %in% c(NA,"Self described"))]
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate,tz = "UTC"),unit = "1 month")
    study <- "Ab-C"
    
  } else if (input_name == "clsa_df"){
    var_levels <- c(unique(study_df$age_groups),unique(study_df$sex),
                    unique(study_df$urban),unique(study_df$race),
                    unique(study_df$quintmat),unique(study_df$quintsoc))
    
    # Remove levels indicating missing values and store month of sample collection
    # in main df
    var_levels <- var_levels[!(var_levels %in% c(NA,"pnts"))]
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate,tz = "UTC"),unit = "1 month")
    study <- "CLSA"
    
  } else if (input_name == "can_df"){
    var_levels <- c(unique(study_df$age_groups),unique(study_df$sex),
                    unique(study_df$urban),unique(study_df$race))
    
    # Remove levels indicating missing values and store month of sample collection
    # in main df
    var_levels <- var_levels[!(var_levels %in% c(NA,"pnts"))]
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate,tz = "UTC"),unit = "1 month")
    study <- "CanPath"
  } 
  
  # Prepare output df
  out_df <- data.frame(study = study, sample_month = unique(study_df$sample_month)) %>% 
    arrange(sample_month)
  
  # Store number of unique categories across all variables
  K <- length(var_levels)
  
  # Calculate total number of samples per month for study. Definition below (study_count_month)
  # only counts number of non-na samples per month
  study_count_all <- study_df %>% 
    group_by(sample_month) %>% 
    summarize(month_count_all = n()) %>% 
    ungroup() %>% 
    as.data.frame()
  
  # Generate counts by age group, by age group & sample month, and proportions
  # of each. Perform for study and census data.
  study_age_month <- study_df %>% 
    filter(!is.na(age_groups)) %>% 
    group_by(age_groups, sample_month) %>% 
    summarize(study_count_age_month = n()) %>% 
    group_by(sample_month) %>% 
    mutate(study_count_month = sum(study_count_age_month)) %>% 
    ungroup()
  
  study_age_month <- study_age_month %>% 
    filter(!is.na(age_groups)) %>% 
    group_by(age_groups) %>% 
    mutate(study_count_age_overall = sum(study_count_age_month)) %>% 
    ungroup()
 
  study_age_month <- study_age_month %>% 
    filter(!is.na(age_groups)) %>% 
    group_by(age_groups, sample_month) %>% 
    mutate(prop_study_age_month = study_count_age_month / study_count_month) %>% 
    group_by(age_groups) %>% 
    mutate(prop_study_age_overall = study_count_age_overall / nrow(study_df[!is.na(study_df$age_groups),])) %>% 
    ungroup()
  
  N_census <- sum(census_df_asu[census_df_asu$age_groups != "All ages" & 
                                  census_df_asu$urban != "All regions", "count_census"])
  
  cens_age_month <- census_df_asu %>% 
    filter(age_groups != "All ages" & urban != "All regions") %>% 
    group_by(age_groups) %>% 
    summarize(census_count_age = sum(count_census),
              prop_census_age_overall = sum(count_census) / unique(N_census)) %>% 
    ungroup()
  
  # Combine and calculate absolute relative bias for k age categories
  study_age_month <- merge(study_age_month, cens_age_month, by = "age_groups") %>% 
    mutate(rel_abs_diff_age_month = ((abs(prop_study_age_month - prop_census_age_overall) / prop_census_age_overall) / K),
           rel_abs_diff_age = ((abs(prop_study_age_overall - prop_census_age_overall) / prop_census_age_overall) / K),
           study = study) %>% 
    group_by(sample_month) %>% 
    mutate(rel_abs_sum_age_month = sum(rel_abs_diff_age_month)) %>%  
    ungroup() %>% 
    select(study, sample_month, study_count_month, rel_abs_sum_age_month, rel_abs_diff_age)
  
  # Generate counts by sex, by sex & sample month, and proportions
  # of each. Perform for study and census data.
  study_sex_month <- study_df %>% 
    filter(!is.na(sex) & sex != "Self described") %>% 
    group_by(sex, sample_month) %>% 
    summarize(study_count_sex_month = n()) %>% 
    group_by(sample_month) %>% 
    mutate(study_count_month = sum(study_count_sex_month)) %>% 
    ungroup()
  
  study_sex_month <- study_sex_month %>% 
    filter(!is.na(sex) & sex != "Self described") %>% 
    group_by(sex) %>% 
    mutate(study_count_sex_overall = sum(study_count_sex_month)) %>% 
    ungroup()
  
  study_sex_month <- study_sex_month %>% 
    filter(!is.na(sex) & sex != "Self described") %>% 
    group_by(sex, sample_month) %>% 
    mutate(prop_study_sex_month = study_count_sex_month / study_count_month) %>% 
    group_by(sex) %>% 
    mutate(prop_study_sex_overall = study_count_sex_overall / nrow(study_df[!is.na(study_df$sex) & study_df$sex != "Self described",])) %>% 
    ungroup()
  
  cens_sex_month <- census_df_asu %>% 
    filter(age_groups != "All ages" & urban != "All regions") %>% 
    group_by(sex) %>% 
    summarize(census_count_sex = sum(count_census),
              prop_census_sex_overall = sum(count_census) / unique(N_census)) %>% 
    ungroup()
  
  # Combine and calculate absolute relative bias for k sex categories
  study_sex_month <- merge(study_sex_month, cens_sex_month, by = "sex") %>% 
    mutate(rel_abs_diff_sex_month = ((abs(prop_study_sex_month - prop_census_sex_overall) / prop_census_sex_overall) / K),
           rel_abs_diff_sex = ((abs(prop_study_sex_overall - prop_census_sex_overall) / prop_census_sex_overall) / K),
           study = study) %>% 
    group_by(sample_month) %>% 
    mutate(rel_abs_sum_sex_month = sum(rel_abs_diff_sex_month)) %>%  
    ungroup() %>% 
    select(study, sample_month, study_count_month, rel_abs_sum_sex_month, rel_abs_diff_sex)
  
  # Generate counts by urbanicity, by urbanicity & sample month, and proportions
  # of each. Perform for study and census data.
  study_urban_month <- study_df %>% 
    filter(!is.na(urban)) %>% 
    group_by(urban, sample_month) %>% 
    summarize(study_count_urban_month = n()) %>% 
    group_by(sample_month) %>% 
    mutate(study_count_month = sum(study_count_urban_month)) %>% 
    ungroup()
  
  study_urban_month <- study_urban_month %>% 
    filter(!is.na(urban)) %>% 
    group_by(urban) %>% 
    mutate(study_count_urban_overall = sum(study_count_urban_month)) %>% 
    ungroup()
  
  study_urban_month <- study_urban_month %>% 
    filter(!is.na(urban)) %>% 
    group_by(urban, sample_month) %>% 
    mutate(prop_study_urban_month = study_count_urban_month / study_count_month) %>% 
    group_by(urban) %>% 
    mutate(prop_study_urban_overall = study_count_urban_overall / nrow(study_df[!is.na(study_df$urban),])) %>% 
    ungroup()
  
  cens_urban_month <- census_df_asu %>% 
    filter(age_groups != "All ages" & urban != "All regions") %>% 
    group_by(urban) %>% 
    summarize(census_count_urban = sum(count_census),
              prop_census_urban_overall = sum(count_census) / unique(N_census)) %>% 
    ungroup()
  
  # Combine and calculate absolute relative bias for k urban categories
  study_urban_month <- merge(study_urban_month, cens_urban_month, by = "urban") %>% 
    mutate(rel_abs_diff_urban_month = ((abs(prop_study_urban_month - prop_census_urban_overall) / prop_census_urban_overall) / K),
           rel_abs_diff_urban = ((abs(prop_study_urban_overall - prop_census_urban_overall) / prop_census_urban_overall) / K),
           study = study) %>% 
    group_by(sample_month) %>% 
    mutate(rel_abs_sum_urban_month = sum(rel_abs_diff_urban_month)) %>%  
    ungroup() %>% 
    select(study, sample_month, study_count_month, rel_abs_sum_urban_month, rel_abs_diff_urban)
  
  
  if (!is.null(census_df_asr)){
    # Generate counts by race/ethnicity, by race/ethnicity & sample month, and proportions
    # of each. Perform for study and census data.
    study_race_month <- study_df %>% 
      filter(!is.na(race) & race != "pnts") %>% 
      group_by(race, sample_month) %>% 
      summarize(study_count_race_month = n()) %>% 
      group_by(sample_month) %>% 
      mutate(study_count_month = sum(study_count_race_month)) %>% 
      ungroup()
    
    study_race_month <- study_race_month %>% 
      filter(!is.na(race)& race != "pnts") %>% 
      group_by(race) %>% 
      mutate(study_count_race_overall = sum(study_count_race_month)) %>% 
      ungroup()
    
    study_race_month <- study_race_month %>% 
      filter(!is.na(race)& race != "pnts") %>% 
      group_by(race, sample_month) %>% 
      mutate(prop_study_race_month = study_count_race_month / study_count_month) %>% 
      group_by(race) %>% 
      mutate(prop_study_race_overall = study_count_race_overall / nrow(study_df[!is.na(study_df$race) & study_df$race != "pnts",])) %>% 
      ungroup()
    
    cens_race_month <- census_df_asr %>% 
      filter(age_groups != "All ages") %>% 
      group_by(race) %>% 
      summarize(census_count_race = sum(count_census),
                prop_census_race_overall = sum(count_census) / unique(N_census)) %>% 
      ungroup()
    
    # Combine and calculate absolute relative bias for k race/ethnicity categories
    study_race_month <- merge(study_race_month, cens_race_month, by = "race") %>% 
      mutate(rel_abs_diff_race_month = ((abs(prop_study_race_month - prop_census_race_overall) / prop_census_race_overall) / K),
             rel_abs_diff_race = ((abs(prop_study_race_overall - prop_census_race_overall) / prop_census_race_overall) / K),
             study = study) %>% 
      group_by(sample_month) %>% 
      mutate(rel_abs_sum_race_month = sum(rel_abs_diff_race_month)) %>%  
      ungroup() %>% 
      select(study, sample_month, study_count_month, rel_abs_sum_race_month, rel_abs_diff_race)
    
  }
  
  if (!is.null(census_df_qm)){
    # Generate counts by quintmat, by quintmat & sample month, and proportions
    # of each. Perform for study and census data.
    study_quintmat_month <- study_df %>% 
      filter(!is.na(quintmat)) %>% 
      group_by(quintmat, sample_month) %>% 
      summarize(study_count_quintmat_month = n()) %>% 
      group_by(sample_month) %>% 
      mutate(study_count_month = sum(study_count_quintmat_month)) %>% 
      ungroup()
    
    study_quintmat_month <- study_quintmat_month %>% 
      filter(!is.na(quintmat)) %>% 
      group_by(quintmat) %>% 
      mutate(study_count_quintmat_overall = sum(study_count_quintmat_month)) %>% 
      ungroup()
    
    study_quintmat_month <- study_quintmat_month %>% 
      filter(!is.na(quintmat)) %>% 
      group_by(quintmat, sample_month) %>% 
      mutate(prop_study_quintmat_month = study_count_quintmat_month / study_count_month) %>% 
      group_by(quintmat) %>% 
      mutate(prop_study_quintmat_overall = study_count_quintmat_overall / nrow(study_df[!is.na(study_df$quintmat),])) %>% 
      ungroup()
    
    cens_quintmat_month <- census_df_qm  %>% 
      group_by(quintmat) %>% 
      summarize(census_count_quintmat = sum(count_census),
                prop_census_quintmat_overall = sum(count_census) / unique(N_census)) %>% 
      ungroup()
    
    
    # Combine and calculate absolute relative bias for k quintmat categories
    study_quintmat_month <- merge(study_quintmat_month, cens_quintmat_month, by = "quintmat") %>% 
      mutate(rel_abs_diff_quintmat_month = ((abs(prop_study_quintmat_month - prop_census_quintmat_overall) / prop_census_quintmat_overall) / K),
             rel_abs_diff_quintmat = ((abs(prop_study_quintmat_overall - prop_census_quintmat_overall) / prop_census_quintmat_overall) / K),
             study = study) %>% 
      group_by(sample_month) %>% 
      mutate(rel_abs_sum_quintmat_month = sum(rel_abs_diff_quintmat_month)) %>%  
      ungroup() %>% 
      select(study, sample_month, study_count_month, rel_abs_sum_quintmat_month, rel_abs_diff_quintmat)
    
  }
  
  if(!is.null(census_df_qs)){
    # Generate counts by quintsoc, by quintsoc & sample month, and proportions
    # of each. Perform for study and census data.
    study_quintsoc_month <- study_df %>% 
      filter(!is.na(quintsoc)) %>% 
      group_by(quintsoc, sample_month) %>% 
      summarize(study_count_quintsoc_month = n()) %>% 
      group_by(sample_month) %>% 
      mutate(study_count_month = sum(study_count_quintsoc_month)) %>% 
      ungroup()
    
    study_quintsoc_month <- study_quintsoc_month %>% 
      filter(!is.na(quintsoc)) %>% 
      group_by(quintsoc) %>% 
      mutate(study_count_quintsoc_overall = sum(study_count_quintsoc_month)) %>% 
      ungroup()
    
    study_quintsoc_month <- study_quintsoc_month %>% 
      filter(!is.na(quintsoc)) %>% 
      group_by(quintsoc, sample_month) %>% 
      mutate(prop_study_quintsoc_month = study_count_quintsoc_month / study_count_month) %>% 
      group_by(quintsoc) %>% 
      mutate(prop_study_quintsoc_overall = study_count_quintsoc_overall / nrow(study_df[!is.na(study_df$quintsoc),])) %>% 
      ungroup()
    
    cens_quintsoc_month <- census_df_qs %>% 
      group_by(quintsoc) %>% 
      summarize(census_count_quintsoc = sum(count_census),
                prop_census_quintsoc_overall = sum(count_census) / unique(N_census)) %>% 
      ungroup()
    
    # Combine and calculate absolute relative bias for k quintsoc categories
    study_quintsoc_month <- merge(study_quintsoc_month, cens_quintsoc_month, by = "quintsoc") %>% 
      mutate(rel_abs_diff_quintsoc_month = ((abs(prop_study_quintsoc_month - prop_census_quintsoc_overall) / prop_census_quintsoc_overall) / K),
             rel_abs_diff_quintsoc = ((abs(prop_study_quintsoc_overall - prop_census_quintsoc_overall) / prop_census_quintsoc_overall) / K),
             study = study) %>% 
      group_by(sample_month) %>% 
      mutate(rel_abs_sum_quintsoc_month = sum(rel_abs_diff_quintsoc_month)) %>%  
      ungroup() %>% 
      select(study, sample_month, study_count_month, rel_abs_sum_quintsoc_month, rel_abs_diff_quintsoc)
  }
  
  # Compute overall and monthly AARB, and return output
  if (any(input_name %in% c("cbs_df","clsa_df"))){
    
    aarb_overall <- sum(c(unique(study_age_month$rel_abs_diff_age),
                          unique(study_sex_month$rel_abs_diff_sex),
                          unique(study_urban_month$rel_abs_diff_urban),
                          unique(study_race_month$rel_abs_diff_race),
                          unique(study_quintsoc_month$rel_abs_diff_quintsoc),
                          unique(study_quintmat_month$rel_abs_diff_quintmat))) * 100
    
    aarb_month <- Reduce(function(x,y) merge(x, y, by = c("study", "sample_month"), all.x = T),
                         list(out_df,
                              study_age_month[,!names(study_age_month) %in% "study_count_month"], 
                              study_sex_month[,!names(study_sex_month) %in% "study_count_month"],
                              study_urban_month[,!names(study_urban_month) %in% "study_count_month"],
                              study_race_month[,!names(study_race_month) %in% "study_count_month"],
                              study_quintmat_month[,!names(study_quintmat_month) %in% "study_count_month"],
                              study_quintsoc_month[,!names(study_quintsoc_month) %in% "study_count_month"])) %>% 
      select(!c(rel_abs_diff_age,rel_abs_diff_sex,rel_abs_diff_urban,
                rel_abs_diff_race,rel_abs_diff_quintmat,rel_abs_diff_quintsoc)) %>% 
      distinct() %>% 
      group_by(sample_month) %>% 
      rowwise() %>% 
      mutate(aarb_month = sum(rel_abs_sum_age_month,rel_abs_sum_sex_month,
                              rel_abs_sum_urban_month,rel_abs_sum_race_month,
                              rel_abs_sum_quintmat_month,rel_abs_sum_quintsoc_month) * 100) %>% 
      ungroup() %>% 
      as.data.frame()
    
  } else if (input_name == "apl_df"){
    
    aarb_overall <- sum(c(unique(study_age_month$rel_abs_diff_age),
                          unique(study_sex_month$rel_abs_diff_sex),
                          unique(study_urban_month$rel_abs_diff_urban),
                          unique(study_quintsoc_month$rel_abs_diff_quintsoc),
                          unique(study_quintmat_month$rel_abs_diff_quintmat))) * 100
    
    aarb_month <- Reduce(function(x,y) merge(x, y, by = c("study", "sample_month"), all.x = T),
                         list(out_df,
                              study_age_month[,!names(study_age_month) %in% "study_count_month"], 
                              study_sex_month[,!names(study_sex_month) %in% "study_count_month"],
                              study_urban_month[,!names(study_urban_month) %in% "study_count_month"],
                              study_quintmat_month[,!names(study_quintmat_month) %in% "study_count_month"],
                              study_quintsoc_month[,!names(study_quintsoc_month) %in% "study_count_month"])) %>% 
      select(!c(rel_abs_diff_age,rel_abs_diff_sex,rel_abs_diff_urban,
                rel_abs_diff_quintmat,rel_abs_diff_quintsoc)) %>% 
      distinct() %>% 
      group_by(sample_month) %>% 
      rowwise() %>% 
      mutate(aarb_month = sum(rel_abs_sum_age_month,rel_abs_sum_sex_month,
                              rel_abs_sum_urban_month,rel_abs_sum_quintmat_month,
                              rel_abs_sum_quintsoc_month,na.rm = T) * 100) %>% 
      ungroup() %>% 
      as.data.frame()
  } else if (any(input_name %in% c("abc_df","can_df"))){
    
    aarb_overall <- sum(c(unique(study_age_month$rel_abs_diff_age),
                          unique(study_sex_month$rel_abs_diff_sex),
                          unique(study_urban_month$rel_abs_diff_urban),
                          unique(study_race_month$rel_abs_diff_race))) * 100
    
    aarb_month <- Reduce(function(x,y) merge(x, y, by = c("study", "sample_month"), all.x = T),
                list(out_df,
                     study_age_month[,!names(study_age_month) %in% "study_count_month"], 
                     study_sex_month[,!names(study_sex_month) %in% "study_count_month"],
                     study_urban_month[,!names(study_urban_month) %in% "study_count_month"],
                     study_race_month[,!names(study_race_month) %in% "study_count_month"])) %>% 
      select(!c(rel_abs_diff_age,rel_abs_diff_sex,rel_abs_diff_urban,
                rel_abs_diff_race)) %>% 
      distinct() %>% 
      group_by(sample_month) %>% 
      rowwise() %>% 
      mutate(aarb_month = sum(rel_abs_sum_age_month,rel_abs_sum_sex_month,
                              rel_abs_sum_urban_month,rel_abs_sum_race_month,na.rm = T) * 100) %>% 
      ungroup() %>% 
      as.data.frame()
    
  }
  
  aarb_month$aarb_overall <- aarb_overall 
  aarb_month <- merge(aarb_month, study_count_all, by = "sample_month", all.x = T) %>% 
    select(aarb_month, study, sample_month, month_count_all, aarb_overall, aarb_month)
  
  return(aarb_month)
}

#Compute DDI
compute_ddi_fun <- function(study_df, census_df_asu, census_df_asr, census_df_qm, census_df_qs){
  
  input_name <- deparse(substitute(study_df))
  
  # Store month of sample collection
  if (input_name == "cbs_df"){
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate,tz = "UTC"), unit = "1 month")
    study <- "CBS"
    
  } else if (input_name == "apl_df"){
    
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate, tz = "UTC"),unit = "1 month")
    study <- "APL"
    
  } else if (input_name == "abc_df"){
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate,tz = "UTC"),unit = "1 month")
    study <- "Ab-C"
    
  } else if (input_name == "clsa_df"){
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate,tz = "UTC"),unit = "1 month")
    study <- "CLSA"
    
  } else if (input_name == "can_df"){
    study_df$sample_month <- floor_date(as.Date(study_df$sampledate,tz = "UTC"),unit = "1 month")
    study <- "CanPath"
  } 
  
  # Calculate total number of samples for study. 
  N <- nrow(study_df)
  
  # Generate counts by census for each variable
  N_census <- sum(census_df_asu[census_df_asu$age_groups != "All ages" & 
                                  census_df_asu$urban != "All regions", "count_census"])
  
  # Generate counts by age group per study
  study_age <- study_df %>% 
    filter(!is.na(age_groups)) %>% 
    group_by(age_groups) %>% 
    summarize(study_age = n() / nrow(study_df[!is.na(study_df$age_groups),])) %>% 
    ungroup() %>% 
    as.data.frame()
  
  # Generate counts by sex per study
  study_sex <- study_df %>% 
    filter(!is.na(sex) & sex != "Self described") %>% 
    group_by(sex) %>% 
    summarize(study_sex = n() / nrow(study_df[!is.na(study_df$sex) & study_df$sex != "Self described",])) %>% 
    ungroup() %>% 
    as.data.frame()
  
  study_urban <- study_df %>% 
    filter(!is.na(urban)) %>% 
    group_by(urban) %>% 
    summarize(study_urban = n() / nrow(study_df[!is.na(study_df$urban),])) %>% 
    ungroup() %>% 
    as.data.frame()
  
  if (!is.null(census_df_asr)){
    study_race <- study_df %>%
      filter(!is.na(race) & race != "pnts") %>% 
      group_by(race) %>% 
      summarize(study_race = n() / nrow(study_df[!is.na(study_df$race) & study_df$race != "pnts",])) %>% 
      ungroup() %>% 
      as.data.frame()
    
    cens_race_month <- census_df_asr %>% 
      filter(age_groups != "All ages") %>% 
      group_by(race) %>% 
      summarize(census_count_race = sum(count_census),
                prop_census = sum(count_census) / unique(N_census)) %>% 
      ungroup()
  }
  if (!is.null(census_df_qm)){
    study_qm <- study_df %>% 
      filter(!is.na(quintmat)) %>% 
      group_by(quintmat) %>% 
      summarize(study_qm = n() / nrow(study_df[!is.na(study_df$quintmat),])) %>% 
      ungroup() %>% 
      as.data.frame()
    
    cens_quintmat_month <- census_df_qm  %>% 
      group_by(quintmat) %>% 
      summarize(census_count_quintmat = sum(count_census),
                prop_census = sum(count_census) / unique(N_census)) %>% 
      ungroup()
  }
  if(!is.null(census_df_qs)){
    study_qs <- study_df %>% 
      filter(!is.na(quintsoc)) %>% 
      group_by(quintsoc) %>% 
      summarize(study_qs = n() / nrow(study_df[!is.na(study_df$quintsoc),])) %>% 
      ungroup() %>% 
      as.data.frame()
    
    cens_quintsoc_month <- census_df_qs %>% 
      group_by(quintsoc) %>% 
      summarize(census_count_quintsoc = sum(count_census),
                prop_census = sum(count_census) / unique(N_census)) %>% 
      ungroup()
    
  }
  
  cens_age_month <- census_df_asu %>% 
    filter(age_groups != "All ages" & urban != "All regions") %>% 
    group_by(age_groups) %>% 
    summarize(census_count_age = sum(count_census),
              prop_census = sum(count_census) / unique(N_census)) %>% 
    ungroup()
  
  cens_sex_month <- census_df_asu %>% 
    filter(age_groups != "All ages" & urban != "All regions") %>% 
    group_by(sex) %>% 
    summarize(census_count_sex = sum(count_census),
              prop_census = sum(count_census) / unique(N_census)) %>% 
    ungroup()
  
  cens_urban_month <- census_df_asu %>% 
    filter(age_groups != "All ages" & urban != "All regions") %>% 
    group_by(urban) %>% 
    summarize(census_count_urban = sum(count_census),
              prop_census = sum(count_census) / unique(N_census)) %>% 
    ungroup()
  
  # Calculate ddi for each variable
  study_sex <- merge(study_sex, cens_sex_month, by = "sex") %>% 
    group_by(sex) %>% 
    mutate(absd = abs(study_sex - prop_census)) %>% 
    ungroup() 
  
  sex <- sum(study_sex$absd) * 0.5
  
  study_age <- merge(study_age, cens_age_month, by = "age_groups") %>% 
    group_by(age_groups) %>% 
    mutate(absd = abs(study_age - prop_census)) %>% 
    ungroup() 
  
  age <- sum(study_age$absd) * 0.5
  
  study_urban <- merge(study_urban, cens_urban_month, by = "urban") %>% 
    group_by(urban) %>% 
    mutate(absd = abs(study_urban - prop_census)) %>% 
    ungroup() 
  
  urban <- sum(study_urban$absd) * 0.5
  
  if (!is.null(census_df_asr)){
    study_race <- merge(study_race, cens_race_month, by = "race") %>% 
      group_by(race) %>% 
      mutate(absd = abs(study_race - prop_census)) %>% 
      ungroup() 
    
    race <- sum(study_race$absd) * 0.5
    
  }
  
  if (!is.null(census_df_qm)){
    study_quintmat <- merge(study_qm, cens_quintmat_month, by = "quintmat") %>% 
      group_by(quintmat) %>% 
      mutate(absd = abs(study_qm - prop_census)) %>% 
      ungroup() 
    
    quintmat <- sum(study_quintmat$absd) * 0.5
    
  }
  
  if (!is.null(census_df_qs)){
    study_quintsoc <- merge(study_qs, cens_quintsoc_month, by = "quintsoc") %>% 
      group_by(quintsoc) %>% 
      mutate(absd = abs(study_qs - prop_census)) %>% 
      ungroup() 
    
    quintsoc <- sum(study_quintsoc$absd) * 0.5
    
  }
  
  # export
  if(input_name == "cbs_df" | input_name == "clsa_df"){
    
    df <- data.frame(Age = age,Sex = sex, Urban = urban, Race = race,
                     Quintmat = quintmat, Quintsoc = quintsoc)
  }
  
  if(input_name == "apl_df"){
    df <- data.frame(Age = age,Sex = sex, Urban = urban, 
                     Quintmat = quintmat, Quintsoc = quintsoc)
  }
  
  if(input_name == "abc_df" | input_name == "can_df"){
    df <- data.frame(Age = age,Sex = sex, Urban = urban, Race = race)
    
  }
  return(df)
}  


#Classify rep ratio as either underrepresented, well represented, or overrepresented
rr_binned_fun<-function(x){#input rep ratio column
  x<-case_when(
    x < 1/2 ~ "Strongly underrepresented (RR < 1/2)",
    x >= 1/2 & 
      x < 3/4 ~  "Moderately underrepresented (1/2 \u2264 RR < 3/4)",
    x >= 3/4 & 
      x <= 4/3 ~ "Adequately represented (3/4 \u2264 RR \u2264 4/3)",
    x > 4/3 & 
      x <= 2.00 ~  "Moderately overrepresented (4/3 < RR \u2264 2)",
    x > 2.00 ~ "Strongly overrepresented (RR > 2)",
    TRUE ~ NA
  )
}

#Change value "NA" to "Missing" for supplemental table
sup_fun<-function(x){
  x<-ifelse(is.na(x) | x == "pnts","Missing",x)
  return(x)
}

#Clean datasets before plotting
asu_clean<-function(df){
  df<-df[c(which(df$urban != "All regions" & 
                   df$age_groups != "All ages"),
           which(df$urban == "All regions" & 
                   df$age_groups != "All ages"),
           which(df$age_groups == "All ages" & 
                   df$urban != "All regions"),
           which(df$urban == "All regions" & 
                   df$age_groups == "All ages")),]
  df$pct<-c(df[1:24,]$count / sum(df[1:24,]$count,na.rm = T),
            df[25:36,]$count / sum(df[25:36,]$count,na.rm = T),
            df[37:40,]$count / sum(df[37:40,]$count),
            df[41:42,]$count / sum(df[41:42,]$count))
  return(df)
}

# Census version of asu_clean
asu_clean1<-function(df){
  df<-df[c(which(df$urban != "All regions" & 
                   df$age_groups != "All ages"),
           which(df$urban == "All regions" & 
                   df$age_groups != "All ages"),
           which(df$age_groups == "All ages" & 
                   df$urban != "All regions"),
           which(df$urban == "All regions" & 
                   df$age_groups == "All ages")),]
  df$pct_pop<-c(df[1:24,]$count_census / sum(df[1:24,]$count_census,na.rm = T),
                df[25:36,]$count_census / sum(df[25:36,]$count_census,na.rm = T),
                df[37:40,]$count_census / sum(df[37:40,]$count_census),
                df[41:42,]$count_census / sum(df[41:42,]$count_census))
  return(df)
}


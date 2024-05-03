"This script loads helper functions."

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
     "white celtic")

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

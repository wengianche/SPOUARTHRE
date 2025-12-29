#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20250929
#UPDATED: 20251218
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE ELSA 2002-2019 wave (1-9) DATA STRUCTURE, DATA PREPARATION
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#Harmonized ELSA waves 1 to 9
  
#Logbook
######################################################  
#20251027 No medication variables for arthritis in ELSA
#20251028 Proxy interviews are currently kept in the dataset, but consider dropping them for sensitivity analysis
#20251101 Update definition of physical activity, 3 and 4 go to some activity, and 5 goes to inactivity
#20251105 Add birth year
######################################################

#Things to pay attention
###################################################### 
#HICAP, can be derived from monetary variables before or after tax or combined  
#20251104 Recode missing value for occupation in Stata 
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
#2. The number of eligible spousal pairs in each wave (using harmonized data)
#3. The number of individuals died during follow-up (helsa_all_sp26)
#4. Descriptive data of selected baseline and outcome variables in helsa_all_sp26
#5. Check strange values (helsa_all_sp27)
#6. Missingness pattern (helsa_all_sp27)
#7. Check multicollinearity (helsa_all_sp27)

#DATA PREPARATION
#1. List of included variables  
#2. Define outcomes and potential confounders to be adjusted in helsa_all_sp4
#3. Multiple imputation for covariates with missing proportion  5% < x < 40% (helsa_all_sp27) 
#4. Post imputation modifications (helsa_all_sp27 and helsa_imp_all)
######################################################  
###################################################### 
#INSTALL AND LOAD LIBRARY
###################################################### 
library(pacman)
pacman::p_load(Hmisc, #attach labels to variables
               haven,
               tidyverse,
               ggalluvial,
               ggplot2,
               plotly,
               labelled,
               writexl,
               gmodels,
               kableExtra,
               janitor,
               gtsummary,
               gt,
               cardx,
               naniar,
               mice,
               mitools,
               broom.mixed,
               MatchIt,
               VIM,
               corrplot
) 
#Function to get 2X2 table with frequency and proportion presented
freq_table <- function(table) {
  list(
    counts = addmargins(table),
    row_proportions = addmargins(prop.table(table, 1)),
    column_proportions = addmargins(prop.table(table, 2)),
    overall_proportions = addmargins(prop.table(table))
  )
}
###################################################### 

######################################################
#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
#2. The number of eligible spousal pairs in each wave (using harmonized data)
#3. The number of individuals died during follow-up (helsa_all_sp27)
#4. Descriptive data of selected baseline and outcome variables in helsa_all_sp27
#5. Check strange values (helsa_all_sp27)
#6. Missingness pattern (helsa_all_sp27)
#7. Check multicollinearity (helsa_all_sp27)
######################################################
#1. Participation of individuals across waves (using harmonized data)
######################################################
#Load harmonized ELSA data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Waves/UKDA-5050-stata/stata/stata13_se')
helsa <- read_dta("h_elsa_g3.dta")

##Wave 1
#The total number of individual #12099
table(helsa$inw1) 
#The total number of spousal pairs #4035
w1 <- helsa %>% filter(inw1==1 & s1iwstat==1) 
spair_1 <- w1 %>% filter(!is.na(idauniq) & !is.na(s1idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s1idauniq), 
                  pmax(idauniq, s1idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #4035 (Altenative calculation)
helsa_1_sp <- helsa %>% filter(inw1==1) %>% add_count(h1coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h1coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 2
#The total number of individual #9432
table(helsa$inw2) 
#The total number of spousal pairs #3089
w2 <- helsa %>% filter(inw2==1 & s2iwstat==1) 
spair_2 <- w2 %>% filter(!is.na(idauniq) & !is.na(s2idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s2idauniq), 
                  pmax(idauniq, s2idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #3089 (Altenative calculation)
helsa_2_sp <- helsa %>% filter(inw2==1) %>% add_count(h2coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h2coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 3
#The total number of individual #9766
table(helsa$inw3) 
#The total number of spousal pairs #3191
w3 <- helsa %>% filter(inw3==1 & s3iwstat==1) 
spair_3 <- w3 %>% filter(!is.na(idauniq) & !is.na(s3idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s3idauniq), 
                  pmax(idauniq, s3idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #3191 (Altenative calculation)
helsa_3_sp <- helsa %>% filter(inw3==1) %>% add_count(h3coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h3coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 4
#The total number of individual #11050
table(helsa$inw4) 
#The total number of spousal pairs #3701
w4 <- helsa %>% filter(inw4==1 & s4iwstat==1) 
spair_4 <- w4 %>% filter(!is.na(idauniq) & !is.na(s4idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s4idauniq), 
                  pmax(idauniq, s4idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #3701 (Altenative calculation)
helsa_4_sp <- helsa %>% filter(inw4==1) %>% add_count(h4coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h4coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 5
#The total number of individual #10274
table(helsa$inw5) 
#The total number of spousal pairs #3482
w5 <- helsa %>% filter(inw5==1 & s5iwstat==1) 
spair_5 <- w5 %>% filter(!is.na(idauniq) & !is.na(s5idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s5idauniq), 
                  pmax(idauniq, s5idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #3482 (Altenative calculation)
helsa_5_sp <- helsa %>% filter(inw5==1) %>% add_count(h5coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h5coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 6
#The total number of individual #10601
table(helsa$inw6) 
#The total number of spousal pairs #3621
w6 <- helsa %>% filter(inw6==1 & s6iwstat==1) 
spair_6 <- w6 %>% filter(!is.na(idauniq) & !is.na(s6idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s6idauniq), 
                  pmax(idauniq, s6idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #3621 (Altenative calculation)
helsa_6_sp <- helsa %>% filter(inw6==1) %>% add_count(h6coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h6coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 7
#The total number of individual #9666
table(helsa$inw7) 
#The total number of spousal pairs #3280
w7 <- helsa %>% filter(inw7==1 & s7iwstat==1) 
spair_7 <- w7 %>% filter(!is.na(idauniq) & !is.na(s7idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s7idauniq), 
                  pmax(idauniq, s7idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #3280 (Altenative calculation)
helsa_7_sp <- helsa %>% filter(inw7==1) %>% add_count(h7coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h7coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 8
#The total number of individual #8445
table(helsa$inw8) 
#The total number of spousal pairs #2844
w8 <- helsa %>% filter(inw8==1 & s8iwstat==1) 
spair_8 <- w8 %>% filter(!is.na(idauniq) & !is.na(s8idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s8idauniq), 
                  pmax(idauniq, s8idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #2844 (Altenative calculation)
helsa_8_sp <- helsa %>% filter(inw8==1) %>% add_count(h8coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h8coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 9
#The total number of individual #8736
table(helsa$inw9) 
#The total number of spousal pairs #2910
w9 <- helsa %>% filter(inw9==1 & s9iwstat==1) 
spair_9 <- w9 %>% filter(!is.na(idauniq) & !is.na(s9idauniq)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(idauniq, s9idauniq), 
                  pmax(idauniq, s9idauniq), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #2910 (Altenative calculation)
helsa_9_sp <- helsa %>% filter(inw9==1) %>% add_count(h9coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h9coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 10
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Waves/UKDA-5050-tab/tab')
elsa10 <- read_tsv("wave_10_elsa_data_eul_v4.tab", 
                   col_names = TRUE,
                   na = c("", "NA", "NULL"),
                   trim_ws = TRUE)

#The total number of individual #7589
length(table(elsa10$idauniq))
#The total number of spousal pairs #2465 
elsa_10_sp <- elsa10 %>% add_count(idahhw10) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(idahhw10) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 11
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Waves/UKDA-5050-stata/stata/stata13_se')
elsa11 <- read_dta("wave_11_elsa_data_eul_v1.dta")

#The total number of individual #7842
length(table(elsa11$idauniq))
#The total number of spousal pairs #2455
elsa_11_sp <- elsa11 %>% add_count(idahhw11) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(idahhw11) %>%       # Get unique categories
  summarise(unique_count = n())
######################################################
#2. The number of eligible spousal pairs in each wave (using harmonized data)
######################################################
#Inclusion criteria: For each dataset, heterosexual spousal pairs in which both partners participated in at least two survey waves will be included. In cases where multiple spouses were recorded for an individual, only the first will be retained. Both spouses must have complete data on age, gender, and doctor-diagnosed arthritis.
#Exclusion criteria: To ensure the reliability of responses, spousal pairs in which either partner reported a doctor-diagnosed memory-related condition (e.g., dementia or Alzheimer’s disease) or was receiving treatment for such conditions will be excluded. Additionally, proxy interviews—often indicative of cognitive impairment—will also be excluded. 
######################################################
#Load data and select relevant variables
#Harmonized data_waves_1_to_9
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Waves/UKDA-5050-stata/stata/stata13_se')
helsa <- read_dta("h_elsa_g3.dta") %>% dplyr::select(1, 5:13, 23:31, 41:63, 78:95, 98:113, 142:159, 215:223, 296:313, 344:361, 396:443, 462:501, 522:549, 604:621, 694:711, 730:739, 750, 753:782, 1353:1406, 1569:1622, 1767:1820, 2199:2644, 2825:2860, 2915:2932, 2969:3040, 3045:3104, 3115:3194, 3267:3322, 3699:3786, 3803:3856, 4239:4256, 4846:4898, 4936:4998, 5071:5187, 5324:5341, 5533:5550, 5726:5761, 5780:5815, 6052:6069, 6204:6239, 6258:6275, 7198:7213, 8812:9011, 9418:9453, 10146:10165)
######################################################
##Wave 1 #7336 individuals and 3668 spousal pairs participating from wave 1
######################################################
#all spousal pairs
helsa_w1 <- helsa %>% filter(inw1==1 & !is.na(s1idauniq) & s1idauniq != 0 & !is.na(s1iwstat) & s1iwstat==1) #8070 individuals and 4035 spousal pairs
table(as.data.frame(table(helsa_w1$hh1hhid, exclude = NULL))$Freq, exclude=NULL)

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(helsa_w1$h1cpl) # all 1

#QC: age distribution, should be 45 or older
table(helsa_w1$r1agey, exclude=NULL)
table(helsa_w1$s1agey, exclude=NULL) #Cutoff 16 years, all above 16 years, the youngest was 20 years, no need to run the next exclusion step

#SKIPPED:Exclude spousal pairs with either of spouse younger than 22 years or with missing age values
#helsa_w1_2 <- helsa_w1 %>% filter(r1agey >=22 & s1agey >=22) #14660 individuals and 7330 spousal pairs
#helsa_w1_2 %>% add_count(h1coupid) %>%      # Add count column
#  filter(n == 2) %>%           # Filter for count=2
#  distinct(h1coupid) %>%       # Get unique categories
#  summarise(unique_count = n())

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
helsa_w1 <- helsa_w1 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(helsa_w1$missing_count_rarthre, exclude=NULL)
#2381 individuals with no missing, 671 individuals with 1 missing value, 535 individuals with 2 missing values, 510 individuals with 3 missing values
#478 individuals with 4 missing values, 507 individuals with 5 missing values, 739 individuals with 6 missing values, 968 individuals with 7 missing values, and 1281 with 8 missing values

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- helsa_w1 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye))|
                          (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye))| 
                          (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye))|
                          (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye))|
                          (r5memrye==1 & !is.na(r5memrye)) | (s5memrye==1 & !is.na(s5memrye))|
                          (r6memrye==1 & !is.na(r6memrye)) | (s6memrye==1 & !is.na(s6memrye))|
                          (r7memrye==1 & !is.na(r7memrye)) | (s7memrye==1 & !is.na(s7memrye))|
                          (r8memrye==1 & !is.na(r8memrye)) | (s8memrye==1 & !is.na(s8memrye))|
                          (r9memrye==1 & !is.na(r9memrye)) | (s9memrye==1 & !is.na(s9memrye))) %>% select("idauniq", "hh1hhid", "h1coupid","r1memrye", "s1memrye", "r2memrye", "s2memrye", "r3memrye", "s3memrye", "r4memrye", "s4memrye", "r5memrye", "s5memrye", "r6memrye", "s6memrye", "r7memrye", "s7memrye", "r8memrye", "s8memrye", "r9memrye", "s9memrye" ) 
table(as.data.frame(table(qc$hh1hhid, exclude = NULL))$Freq, exclude=NULL) #635 individuals, with 268 are spousal pairs and 99 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
helsa_w1_2 <- helsa_w1 %>% filter(!h1coupid %in% qc$h1coupid)  #7336 individuals and 3668 spousal pairs
table(as.data.frame(table(helsa_w1_2$h1coupid, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(helsa_w1_2$r1iwstat, exclude=NULL) #all 1
table(helsa_w1_2$s1iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
helsa_w1_2 <- helsa_w1_2 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                        r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(r1_part:r9_part)))
table(helsa_w1_2$total_participations, exclude=NULL) #1236 individuals participated in wave 1 only
qc <- as.data.frame(table(helsa_w1_2$spousal_part_pattern, exclude=NULL)) #150 patterns

#Check the number of individual participated only in wave 1
qc <- helsa_w1_2 %>% filter(total_participations ==1)
qc2 <- as.data.frame(table(qc$h1coupid, exclude=NULL)) %>% filter(Freq==1) #1236 individuals, 479 spousal pairs and 278 individuals
#Select those with h1coupid count=1
qc3 <- helsa_w1_2 %>% filter(h1coupid %in% qc2$Var1) %>% select("idauniq", "h1coupid", "inw1", 2098:2099) 
######################################################
##Wave 2 ##198 individuals and 99 spousal pairs participating from wave 2
######################################################
#All spousal pairs participating in wave 2
helsa_w2 <- helsa %>% filter(inw2==1 & !is.na(s2idauniq) & s2idauniq != 0 & !is.na(s2iwstat) & s2iwstat==1) #6178 individuals, with 3089 spousal pairs and 
table(as.data.frame(table(helsa_w2$h2coupid, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(helsa_w2$h2cpl) # all 1

#Exclude individuals included in helsa_w1_2
helsa_w2_2 <- helsa_w2 %>% filter(!h2coupid %in% helsa_w1_2$h2coupid) #816 individuals, with 408 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(helsa_w2_2$h2coupid))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 16 and above
table(helsa_w2_2$r2agey, exclude=NULL)
table(helsa_w2_2$s2agey, exclude=NULL) #All respondents aged above 16

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
helsa_w2_2 <- helsa_w2_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(helsa_w2_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(helsa_w2_2$inw1, exclude=NULL) #713 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- helsa_w2_2 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye))|
                            (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye))| 
                            (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye))|
                            (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye))|
                            (r5memrye==1 & !is.na(r5memrye)) | (s5memrye==1 & !is.na(s5memrye))|
                            (r6memrye==1 & !is.na(r6memrye)) | (s6memrye==1 & !is.na(s6memrye))|
                            (r7memrye==1 & !is.na(r7memrye)) | (s7memrye==1 & !is.na(s7memrye))|
                            (r8memrye==1 & !is.na(r8memrye)) | (s8memrye==1 & !is.na(s8memrye))|
                            (r9memrye==1 & !is.na(r9memrye)) | (s9memrye==1 & !is.na(s9memrye))) %>% select("idauniq", "hh2hhid", "h2coupid","r1memrye", "s1memrye", "r2memrye", "s2memrye", "r3memrye", "s3memrye", "r4memrye", "s4memrye", "r5memrye", "s5memrye", "r6memrye", "s6memrye", "r7memrye", "s7memrye", "r8memrye", "s8memrye", "r9memrye", "s9memrye" ) 
table(as.data.frame(table(qc$h2coupid, exclude = NULL))$Freq, exclude=NULL) #545 individuals, with 236 are spousal pairs and 73 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
helsa_w2_3 <- helsa_w2_2 %>% filter(!h2coupid %in% qc$h2coupid)  #198 individuals and 99 spousal pairs
table(as.data.frame(table(helsa_w2_3$h2coupid, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(helsa_w2_3$r2iwstat, exclude=NULL) #all 1
table(helsa_w2_3$s2iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
helsa_w2_3 <- helsa_w2_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(r1_part:r9_part)))
table(helsa_w2_3$total_participations, exclude=NULL) #22 individuals participated in wave 2 only
qc <- as.data.frame(table(helsa_w2_3$spousal_part_pattern, exclude=NULL)) #43 patterns

#Check the number of individual participated only in wave 2
qc <- helsa_w2_3 %>% filter(total_participations ==1)
qc2 <- as.data.frame(table(qc$h2coupid, exclude=NULL)) %>% filter(Freq==1) #22 individuals, no spousal pairs
#Select those with h2coupid count=1
qc3 <- helsa_w2_3 %>% filter(h2coupid %in% qc2$Var1) %>% select("idauniq", "h2coupid", "inw2", 2098:2099) 
######################################################
##Wave 3 ##1370 individuals and 685 spousal pairs participating from wave 3
######################################################
#All spousal pairs participating in wave 3
helsa_w3 <- helsa %>% filter(inw3==1 & !is.na(s3idauniq) & s3idauniq != 0 & !is.na(s3iwstat) & s3iwstat==1) #6382 individuals, with 3191 spousal pairs
table(as.data.frame(table(helsa_w3$h3coupid, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(helsa_w3$h3cpl) # All 1

#Exclude individuals included in helsa_w1_2 and helsa_w2_3
helsa_w3_2 <- helsa_w3 %>% filter(!(h3coupid %in% helsa_w1_2$h3coupid | h3coupid %in% helsa_w2_3$h3coupid)) #1938 individuals, with 969 spousal pairs

#Check the number of spousal pairs
table(as.data.frame(table(helsa_w3_2$h3coupid))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 16 and above
table(helsa_w3_2$r3agey, exclude=NULL)
table(helsa_w3_2$s3agey, exclude=NULL) #All respondents aged above 16 and 1 individual with age missing

#Exclude spousal pairs with either of spouse younger than 16 years or with missing values on r3agey or s3agey
qc <- helsa_w3_2 %>% filter(is.na(r3agey)|is.na(s3agey))
helsa_w3_3 <- helsa_w3_2 %>% filter(!h3coupid %in% qc$h3coupid) #1936 individuals and 968 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(helsa_w3_3$h3coupid))$Freq, exclude=NULL) 
#Check age distribution
table(helsa_w3_3$r3agey, exclude=NULL)
table(helsa_w3_3$s3agey, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
helsa_w3_3 <- helsa_w3_3 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(helsa_w3_3$missing_count_rarthre, exclude=NULL) #232 individuals with all rwarthre missing

#QC: Check the frequency of inw1
table(helsa_w3_3$inw1, exclude=NULL) #619 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- helsa_w3_3 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye))|
                              (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye))| 
                              (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye))|
                              (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye))|
                              (r5memrye==1 & !is.na(r5memrye)) | (s5memrye==1 & !is.na(s5memrye))|
                              (r6memrye==1 & !is.na(r6memrye)) | (s6memrye==1 & !is.na(s6memrye))|
                              (r7memrye==1 & !is.na(r7memrye)) | (s7memrye==1 & !is.na(s7memrye))|
                              (r8memrye==1 & !is.na(r8memrye)) | (s8memrye==1 & !is.na(s8memrye))|
                              (r9memrye==1 & !is.na(r9memrye)) | (s9memrye==1 & !is.na(s9memrye))) %>% select("idauniq", "hh3hhid", "h3coupid","r1memrye", "s1memrye", "r2memrye", "s2memrye", "r3memrye", "s3memrye", "r4memrye", "s4memrye", "r5memrye", "s5memrye", "r6memrye", "s6memrye", "r7memrye", "s7memrye", "r8memrye", "s8memrye", "r9memrye", "s9memrye" ) 
table(as.data.frame(table(qc$h3coupid, exclude = NULL))$Freq, exclude=NULL) #506 individuals, with 223 are spousal pairs and 60 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
helsa_w3_4 <- helsa_w3_3 %>% filter(!h3coupid %in% qc$h3coupid)  #1370 individuals and 685 spousal pairs
table(as.data.frame(table(helsa_w3_4$h3coupid, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(helsa_w3_4$r3iwstat, exclude=NULL) #all 1
table(helsa_w3_4$s3iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
helsa_w3_4 <- helsa_w3_4 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(r1_part:r9_part)))
table(helsa_w3_4$total_participations, exclude=NULL) #215 individuals participated in wave 3 only
qc <- as.data.frame(table(helsa_w3_4$spousal_part_pattern, exclude=NULL)) #61 patterns

#Check the number of individual participated only in wave 3
qc <- helsa_w3_4 %>% filter(total_participations ==1)
qc2 <- as.data.frame(table(qc$h3coupid, exclude=NULL)) %>% filter(Freq==1) #27 individuals, no spousal pairs
#Select those with h3coupid count=1
qc3 <- helsa_w3_4 %>% filter(h3coupid %in% qc2$Var1) %>% select("idauniq", "h3coupid", "inw3", 2098:2099) 
######################################################
##Wave 4 ##1960 individuals and 980 spousal pairs participating from wave 4
######################################################
#All spousal pairs participating in wave 4
helsa_w4 <- helsa %>% filter(inw4==1 & !is.na(s4idauniq) & s4idauniq != 0 & !is.na(s4iwstat) & s4iwstat==1) #7402 individuals, with 3701 spousal pairs
table(as.data.frame(table(helsa_w4$h4coupid, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(helsa_w4$h4cpl) # All 1

#Exclude individuals included in helsa_w1_2, helsa_w2_3, and helsa_w3_4
helsa_w4_2 <- helsa_w4 %>% filter(!(h4coupid %in% helsa_w1_2$h4coupid | h4coupid %in% helsa_w2_3$h4coupid | h4coupid %in% helsa_w3_4$h4coupid)) #2556 individuals, with 1278 spousal pairs

#Check the number of spousal pairs
table(as.data.frame(table(helsa_w4_2$h4coupid))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 16 and above
table(helsa_w4_2$r4agey, exclude=NULL)
table(helsa_w4_2$s4agey, exclude=NULL) #All respondents aged above 16

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
helsa_w4_2 <- helsa_w4_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(helsa_w4_2$missing_count_rarthre, exclude=NULL) #208 individuals with all rwarthre missing

#QC: Check the frequency of inw1
table(helsa_w4_2$inw1, exclude=NULL) #523 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- helsa_w4_2 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye))|
                              (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye))| 
                              (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye))|
                              (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye))|
                              (r5memrye==1 & !is.na(r5memrye)) | (s5memrye==1 & !is.na(s5memrye))|
                              (r6memrye==1 & !is.na(r6memrye)) | (s6memrye==1 & !is.na(s6memrye))|
                              (r7memrye==1 & !is.na(r7memrye)) | (s7memrye==1 & !is.na(s7memrye))|
                              (r8memrye==1 & !is.na(r8memrye)) | (s8memrye==1 & !is.na(s8memrye))|
                              (r9memrye==1 & !is.na(r9memrye)) | (s9memrye==1 & !is.na(s9memrye))) %>% select("idauniq", "hh4hhid", "h4coupid","r1memrye", "s1memrye", "r2memrye", "s2memrye", "r3memrye", "s3memrye", "r4memrye", "s4memrye", "r5memrye", "s5memrye", "r6memrye", "s6memrye", "r7memrye", "s7memrye", "r8memrye", "s8memrye", "r9memrye", "s9memrye" ) 
table(as.data.frame(table(qc$h4coupid, exclude = NULL))$Freq, exclude=NULL) #541 individuals, with 243 are spousal pairs and 55 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
helsa_w4_3 <- helsa_w4_2 %>% filter(!h4coupid %in% qc$h4coupid)  #1960 individuals and 980 spousal pairs
table(as.data.frame(table(helsa_w4_3$h4coupid, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(helsa_w4_3$r4iwstat, exclude=NULL) #all 1
table(helsa_w4_3$s4iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
helsa_w4_3 <- helsa_w4_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(r1_part:r9_part)))
table(helsa_w4_3$total_participations, exclude=NULL) #257 individuals participated in wave 4 only
qc <- as.data.frame(table(helsa_w4_3$spousal_part_pattern, exclude=NULL)) #49 patterns

#Check the number of individual participated only in wave 4
qc <- helsa_w4_3 %>% filter(total_participations ==1)
qc2 <- as.data.frame(table(qc$h4coupid, exclude=NULL)) %>% filter(Freq==1) #57 individuals, no spousal pairs
#Select those with h4coupid count=1
qc3 <- helsa_w4_3 %>% filter(h4coupid %in% qc2$Var1) %>% select("idauniq", "h4coupid", "inw4", 2098:2099) 
######################################################
##Wave 5 ##252 individuals and 126 spousal pairs participating from wave 5
######################################################
#All spousal pairs participating in wave 5
helsa_w5 <- helsa %>% filter(inw5==1 & !is.na(s5idauniq) & s5idauniq != 0 & !is.na(s5iwstat) & s5iwstat==1) #6964 individuals, with 3482 spousal pairs
table(as.data.frame(table(helsa_w5$h5coupid, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(helsa_w5$h5cpl) # All 1

#Exclude individuals included in helsa_w1_2, helsa_w2_3, helsa_w3_4, and helsa_w4_3
helsa_w5_2 <- helsa_w5 %>% filter(!(h5coupid %in% helsa_w1_2$h5coupid | h5coupid %in% helsa_w2_3$h5coupid | h5coupid %in% helsa_w3_4$h5coupid | h5coupid %in% helsa_w4_3$h5coupid)) #790 individuals, with 395 spousal pairs

#Check the number of spousal pairs
table(as.data.frame(table(helsa_w5_2$h5coupid))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 16 and above
table(helsa_w5_2$r5agey, exclude=NULL)
table(helsa_w5_2$s5agey, exclude=NULL) #All respondents aged above 16

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
helsa_w5_2 <- helsa_w5_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(helsa_w5_2$missing_count_rarthre, exclude=NULL) #205 individuals with all rwarthre missing

#QC: Check the frequency of inw1
table(helsa_w5_2$inw1, exclude=NULL) #461 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- helsa_w5_2 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye))|
                              (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye))| 
                              (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye))|
                              (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye))|
                              (r5memrye==1 & !is.na(r5memrye)) | (s5memrye==1 & !is.na(s5memrye))|
                              (r6memrye==1 & !is.na(r6memrye)) | (s6memrye==1 & !is.na(s6memrye))|
                              (r7memrye==1 & !is.na(r7memrye)) | (s7memrye==1 & !is.na(s7memrye))|
                              (r8memrye==1 & !is.na(r8memrye)) | (s8memrye==1 & !is.na(s8memrye))|
                              (r9memrye==1 & !is.na(r9memrye)) | (s9memrye==1 & !is.na(s9memrye))) %>% select("idauniq", "hh5hhid", "h5coupid","r1memrye", "s1memrye", "r2memrye", "s2memrye", "r3memrye", "s3memrye", "r4memrye", "s4memrye", "r5memrye", "s5memrye", "r6memrye", "s6memrye", "r7memrye", "s7memrye", "r8memrye", "s8memrye", "r9memrye", "s9memrye" ) 
table(as.data.frame(table(qc$h5coupid, exclude = NULL))$Freq, exclude=NULL) #501 individuals, with 232 are spousal pairs and 37 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
helsa_w5_3 <- helsa_w5_2 %>% filter(!h5coupid %in% qc$h5coupid)  #252 individuals and 126 spousal pairs
table(as.data.frame(table(helsa_w5_3$h5coupid, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(helsa_w5_3$r5iwstat, exclude=NULL) #all 1
table(helsa_w5_3$s5iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
helsa_w5_3 <- helsa_w5_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(r1_part:r9_part)))
table(helsa_w5_3$total_participations, exclude=NULL) #29 individuals participated in wave 5 only
qc <- as.data.frame(table(helsa_w5_3$spousal_part_pattern, exclude=NULL)) #43 patterns

#Check the number of individual participated only in wave 5
qc <- helsa_w5_3 %>% filter(total_participations ==1)
qc2 <- as.data.frame(table(qc$h5coupid, exclude=NULL)) %>% filter(Freq==1) #29 individuals, no spousal pairs
#Select those with h5coupid count=1
qc3 <- helsa_w5_3 %>% filter(h5coupid %in% qc2$Var1) %>% select("idauniq", "h5coupid", "inw5", 2098:2099) 
######################################################
##Wave 6 ##950 individuals and 475 spousal pairs participating from wave 6
######################################################
#All spousal pairs participating in wave 6
helsa_w6 <- helsa %>% filter(inw6==1 & !is.na(s6idauniq) & s6idauniq != 0 & !is.na(s6iwstat) & s6iwstat==1) #7242 individuals, with 3621 spousal pairs
table(as.data.frame(table(helsa_w6$h6coupid, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(helsa_w6$h6cpl) # All 1

#Exclude individuals included in helsa_w1_2, helsa_w2_3, helsa_w3_4, helsa_w4_3, and helsa_w5_3
helsa_w6_2 <- helsa_w6 %>% filter(!(h6coupid %in% helsa_w1_2$h6coupid | h6coupid %in% helsa_w2_3$h6coupid | h6coupid %in% helsa_w3_4$h6coupid | h6coupid %in% helsa_w4_3$h6coupid | h6coupid %in% helsa_w5_3$h6coupid)) #1424 individuals, with 712 spousal pairs

#Check the number of spousal pairs
table(as.data.frame(table(helsa_w6_2$h6coupid))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 16 and above
table(helsa_w6_2$r6agey, exclude=NULL)
table(helsa_w6_2$s6agey, exclude=NULL) #All respondents aged above 16

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
helsa_w6_2 <- helsa_w6_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(helsa_w6_2$missing_count_rarthre, exclude=NULL) #180 individuals with all rwarthre missing

#QC: Check the frequency of inw1
table(helsa_w6_2$inw1, exclude=NULL) #366 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- helsa_w6_2 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye))|
                              (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye))| 
                              (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye))|
                              (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye))|
                              (r5memrye==1 & !is.na(r5memrye)) | (s5memrye==1 & !is.na(s5memrye))|
                              (r6memrye==1 & !is.na(r6memrye)) | (s6memrye==1 & !is.na(s6memrye))|
                              (r7memrye==1 & !is.na(r7memrye)) | (s7memrye==1 & !is.na(s7memrye))|
                              (r8memrye==1 & !is.na(r8memrye)) | (s8memrye==1 & !is.na(s8memrye))|
                              (r9memrye==1 & !is.na(r9memrye)) | (s9memrye==1 & !is.na(s9memrye))) %>% select("idauniq", "hh6hhid", "h6coupid","r1memrye", "s1memrye", "r2memrye", "s2memrye", "r3memrye", "s3memrye", "r4memrye", "s4memrye", "r5memrye", "s5memrye", "r6memrye", "s6memrye", "r7memrye", "s7memrye", "r8memrye", "s8memrye", "r9memrye", "s9memrye" ) 
table(as.data.frame(table(qc$h6coupid, exclude = NULL))$Freq, exclude=NULL) #444 individuals, with 207 are spousal pairs and 30 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
helsa_w6_3 <- helsa_w6_2 %>% filter(!h6coupid %in% qc$h6coupid)  #950 individuals and 475 spousal pairs
table(as.data.frame(table(helsa_w6_3$h6coupid, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(helsa_w6_3$r6iwstat, exclude=NULL) #all 1
table(helsa_w6_3$s6iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
helsa_w6_3 <- helsa_w6_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(r1_part:r9_part)))
table(helsa_w6_3$total_participations, exclude=NULL) #159 individuals participated in wave 6 only
qc <- as.data.frame(table(helsa_w6_3$spousal_part_pattern, exclude=NULL)) #34 patterns

#Check the number of individual participated only in wave 6
qc <- helsa_w6_3 %>% filter(total_participations ==1)
qc2 <- as.data.frame(table(qc$h6coupid, exclude=NULL)) %>% filter(Freq==1) #31 individuals, no spousal pairs
#Select those with h6coupid count=1
qc3 <- helsa_w6_3 %>% filter(h6coupid %in% qc2$Var1) %>% select("idauniq", "h6coupid", "inw6", 2098:2099) 
######################################################
##Wave 7 ##428 individuals and 214 spousal pairs participating from wave 7
######################################################
#All spousal pairs participating in wave 7
helsa_w7 <- helsa %>% filter(inw7==1 & !is.na(s7idauniq) & s7idauniq != 0 & !is.na(s7iwstat) & s7iwstat==1) #6560 individuals, with 3280 spousal pairs
table(as.data.frame(table(helsa_w7$h7coupid, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(helsa_w7$h7cpl) # All 1

#Exclude individuals included in helsa_w1_2, helsa_w2_3, helsa_w3_4, helsa_w4_3, helsa_w5_3, and helsa_w6_3
helsa_w7_2 <- helsa_w7 %>% filter(!(h7coupid %in% helsa_w1_2$h7coupid | h7coupid %in% helsa_w2_3$h7coupid | h7coupid %in% helsa_w3_4$h7coupid | h7coupid %in% helsa_w4_3$h7coupid | h7coupid %in% helsa_w5_3$h7coupid | h7coupid %in% helsa_w6_3$h7coupid)) #808 individuals, with 404 spousal pairs

#Check the number of spousal pairs
table(as.data.frame(table(helsa_w7_2$h7coupid))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 16 and above
table(helsa_w7_2$r7agey, exclude=NULL)
table(helsa_w7_2$s7agey, exclude=NULL) #All respondents aged above 16

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
helsa_w7_2 <- helsa_w7_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(helsa_w7_2$missing_count_rarthre, exclude=NULL) #165 individuals with all rwarthre missing

#QC: Check the frequency of inw1
table(helsa_w7_2$inw1, exclude=NULL) #288 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- helsa_w7_2 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye))|
                              (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye))| 
                              (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye))|
                              (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye))|
                              (r5memrye==1 & !is.na(r5memrye)) | (s5memrye==1 & !is.na(s5memrye))|
                              (r6memrye==1 & !is.na(r6memrye)) | (s6memrye==1 & !is.na(s6memrye))|
                              (r7memrye==1 & !is.na(r7memrye)) | (s7memrye==1 & !is.na(s7memrye))|
                              (r8memrye==1 & !is.na(r8memrye)) | (s8memrye==1 & !is.na(s8memrye))|
                              (r9memrye==1 & !is.na(r9memrye)) | (s9memrye==1 & !is.na(s9memrye))) %>% select("idauniq", "hh7hhid", "h7coupid","r1memrye", "s1memrye", "r2memrye", "s2memrye", "r3memrye", "s3memrye", "r4memrye", "s4memrye", "r5memrye", "s5memrye", "r6memrye", "s6memrye", "r7memrye", "s7memrye", "r8memrye", "s8memrye", "r9memrye", "s9memrye" ) 
table(as.data.frame(table(qc$h7coupid, exclude = NULL))$Freq, exclude=NULL) #362 individuals, with 172 are spousal pairs and 18 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
helsa_w7_3 <- helsa_w7_2 %>% filter(!h7coupid %in% qc$h7coupid)  #428 individuals and 214 spousal pairs
table(as.data.frame(table(helsa_w7_3$h7coupid, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(helsa_w7_3$r7iwstat, exclude=NULL) #all 1
table(helsa_w7_3$s7iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
helsa_w7_3 <- helsa_w7_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(r1_part:r9_part)))
table(helsa_w7_3$total_participations, exclude=NULL) #83 individuals participated in wave 7 only
qc <- as.data.frame(table(helsa_w7_3$spousal_part_pattern, exclude=NULL)) #16 patterns

#Check the number of individual participated only in wave 7
qc <- helsa_w7_3 %>% filter(total_participations ==1)
qc2 <- as.data.frame(table(qc$h7coupid, exclude=NULL)) %>% filter(Freq==1) #17 individuals, no spousal pairs
#Select those with h7coupid count=1
qc3 <- helsa_w7_3 %>% filter(h7coupid %in% qc2$Var1) %>% select("idauniq", "h7coupid", "inw7", 2098:2099) 
######################################################
##Wave 8 ##56 individuals and 28 spousal pairs participating from wave 8
######################################################
#All spousal pairs participating in wave 8
helsa_w8 <- helsa %>% filter(inw8==1 & !is.na(s8idauniq) & s8idauniq != 0 & !is.na(s8iwstat) & s8iwstat==1) #5688 individuals, with 2844 spousal pairs
table(as.data.frame(table(helsa_w8$h8coupid, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(helsa_w8$h8cpl) # All 1

#Exclude individuals included in helsa_w1_2, helsa_w2_3, helsa_w3_4, helsa_w4_3, helsa_w5_3, helsa_w6_3, and helsa_w7_3
helsa_w8_2 <- helsa_w8 %>% filter(!(h8coupid %in% helsa_w1_2$h8coupid | h8coupid %in% helsa_w2_3$h8coupid | h8coupid %in% helsa_w3_4$h8coupid | h8coupid %in% helsa_w4_3$h8coupid | h8coupid %in% helsa_w5_3$h8coupid | h8coupid %in% helsa_w6_3$h8coupid | h8coupid %in% helsa_w7_3$h8coupid)) #360 individuals, with 180 spousal pairs

#Check the number of spousal pairs
table(as.data.frame(table(helsa_w8_2$h8coupid))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 16 and above
table(helsa_w8_2$r8agey, exclude=NULL)
table(helsa_w8_2$s8agey, exclude=NULL) #All respondents aged above 16

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
helsa_w8_2 <- helsa_w8_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(helsa_w8_2$missing_count_rarthre, exclude=NULL) #146 individuals with all rwarthre missing

#QC: Check the frequency of inw1
table(helsa_w8_2$inw1, exclude=NULL) #218 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- helsa_w8_2 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye))|
                              (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye))| 
                              (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye))|
                              (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye))|
                              (r5memrye==1 & !is.na(r5memrye)) | (s5memrye==1 & !is.na(s5memrye))|
                              (r6memrye==1 & !is.na(r6memrye)) | (s6memrye==1 & !is.na(s6memrye))|
                              (r7memrye==1 & !is.na(r7memrye)) | (s7memrye==1 & !is.na(s7memrye))|
                              (r8memrye==1 & !is.na(r8memrye)) | (s8memrye==1 & !is.na(s8memrye))|
                              (r9memrye==1 & !is.na(r9memrye)) | (s9memrye==1 & !is.na(s9memrye))) %>% select("idauniq", "hh8hhid", "h8coupid","r1memrye", "s1memrye", "r2memrye", "s2memrye", "r3memrye", "s3memrye", "r4memrye", "s4memrye", "r5memrye", "s5memrye", "r6memrye", "s6memrye", "r7memrye", "s7memrye", "r8memrye", "s8memrye", "r9memrye", "s9memrye" ) 
table(as.data.frame(table(qc$h8coupid, exclude = NULL))$Freq, exclude=NULL) #297 individuals, with 145 are spousal pairs and 7 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
helsa_w8_3 <- helsa_w8_2 %>% filter(!h8coupid %in% qc$h8coupid)  #56 individuals and 28 spousal pairs
table(as.data.frame(table(helsa_w8_3$h8coupid, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(helsa_w8_3$r8iwstat, exclude=NULL) #all 1
table(helsa_w8_3$s8iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
helsa_w8_3 <- helsa_w8_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(r1_part:r9_part)))
table(helsa_w8_3$total_participations, exclude=NULL) #10 individuals participated in wave 8 only
qc <- as.data.frame(table(helsa_w8_3$spousal_part_pattern, exclude=NULL)) #12 patterns

#Check the number of individual participated only in wave 8
qc <- helsa_w8_3 %>% filter(total_participations ==1)
qc2 <- as.data.frame(table(qc$h8coupid, exclude=NULL)) %>% filter(Freq==1) #17 individuals, no spousal pairs
#Select those with h7coupid count=1
qc3 <- helsa_w8_3 %>% filter(h8coupid %in% qc2$Var1) %>% select("idauniq", "h8coupid", "inw8", 2098:2099) 
######################################################
##All eligible spousal pairs, helsa_all_sp, 9996 individuals and 4998 spousal pairs
######################################################
#Add inclusion wave indicator and create unique householdID
helsa_w1_2 <- helsa_w1_2 %>% group_by(h1coupid) %>% mutate(householdID=paste(sort(idauniq), collapse = "_")) %>%
  ungroup() %>% mutate(inclusion_wave="Wave 1")
helsa_w2_3 <- helsa_w2_3 %>% group_by(h2coupid) %>% mutate(householdID=paste(sort(idauniq), collapse = "_")) %>%
  ungroup() %>% mutate(inclusion_wave="Wave 2")
helsa_w3_4 <- helsa_w3_4 %>% group_by(h3coupid) %>% mutate(householdID=paste(sort(idauniq), collapse = "_")) %>%
  ungroup() %>% mutate(inclusion_wave="Wave 3")
helsa_w4_3 <- helsa_w4_3 %>% group_by(h4coupid) %>% mutate(householdID=paste(sort(idauniq), collapse = "_")) %>%
  ungroup() %>% mutate(inclusion_wave="Wave 4")
helsa_w5_3 <- helsa_w5_3 %>% group_by(h5coupid) %>% mutate(householdID=paste(sort(idauniq), collapse = "_")) %>%
  ungroup() %>% mutate(inclusion_wave="Wave 5")
helsa_w6_3 <- helsa_w6_3 %>% group_by(h6coupid) %>% mutate(householdID=paste(sort(idauniq), collapse = "_")) %>%
  ungroup() %>% mutate(inclusion_wave="Wave 6")
helsa_w7_3 <- helsa_w7_3 %>% group_by(h7coupid) %>% mutate(householdID=paste(sort(idauniq), collapse = "_")) %>%
  ungroup() %>% mutate(inclusion_wave="Wave 7")
helsa_w8_3 <- helsa_w8_3 %>% group_by(h8coupid) %>% mutate(householdID=paste(sort(idauniq), collapse = "_")) %>%
  ungroup() %>% mutate(inclusion_wave="Wave 8")

##Dataset including all spousal pairs, combining helsa_w1_2, helsa_w2_3, helsa_w3_4, helsa_w4_3, helsa_w5_3, helsa_w6_3, helsa_w7_3, and helsa_w8_3)
helsa_all_sp <- rbind(helsa_w1_2, helsa_w2_3, helsa_w3_4, helsa_w4_3, helsa_w5_3, helsa_w6_3, helsa_w7_3, helsa_w8_3) #12550 individuals and 6275 spousal pairs
#Check frequency of inclusion_wave
table(helsa_all_sp$inclusion_wave, exclude=NULL)
#Check the number of spousal pairs
table(as.data.frame(table(helsa_all_sp$householdID, exclude = NULL))$Freq, exclude=NULL) 
#Check if all were heterosexual spouse pairs
hetero <- helsa_all_sp %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%
  summarise(hetero = n_distinct(ragender) == 2) %>% filter(hetero=="FALSE")
table(hetero$hetero, exclude=NULL) #34 spousal pairs were homosexual
qc <- helsa_all_sp %>% filter(householdID %in% hetero$householdID) %>% select("idauniq", "householdID","ragender","s1gender", "s2gender", "s3gender", "s4gender", "inclusion_wave")  

#Exclude the homosexual spousal pair
helsa_all_sp2 <- helsa_all_sp %>% filter(!householdID %in% hetero$householdID) #12582 individuals and 6241 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(helsa_all_sp2$householdID))$Freq, exclude=NULL) 
#Select individuals with total_participations==1
one_wave <- helsa_all_sp2 %>% filter(total_participations==1) %>% select("idauniq", "householdID", "inclusion_wave", "total_participations") #2009 individuals and 769 spousal pairs and 471 individuals with spouses participated in later waves
#Check the number of spousal pairs
table(as.data.frame(table(one_wave$householdID))$Freq, exclude=NULL) 

#Exclude individuals with total_participations==1
helsa_all_sp3 <- helsa_all_sp2 %>% filter(!(householdID %in% one_wave$householdID)) #10002 individuals and 5001 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(helsa_all_sp3$householdID))$Freq, exclude=NULL) 

##Create function to produce a new variable counting the number of missing rwarthre based on the participating wave
count_missing_by_participation <- function(data, num_waves = 9) {
  total_missing <- rep(0, nrow(data))
  
  cat("=== DEBUGGING OUTPUT ===\n")
  cat("Column names in data:\n")
  print(names(data))
  cat("\n")
  
  for(i in 1:num_waves) {
    part_var <- paste0("inw", i)
    disease_var <- paste0("r", i, "arthre")
    
    cat("Wave", i, ":\n")
    cat("  Looking for participation variable:", part_var, "\n")
    cat("  Looking for disease variable:", disease_var, "\n")
    
    # Check if columns exist
    part_exists <- part_var %in% names(data)
    disease_exists <- disease_var %in% names(data)
    
    cat("  Participation variable exists:", part_exists, "\n")
    cat("  Disease variable exists:", disease_exists, "\n")
    
    if(part_exists && disease_exists) {
      # Count how many people participated in this wave
      participated <- sum(data[[part_var]] == 1, na.rm = TRUE)
      # Count how many have missing disease data among participants
      missing_count <- sum(data[[part_var]] == 1 & (is.na(data[[disease_var]]) | (data[[disease_var]] !=1 & data[[disease_var]] !=0)), na.rm = TRUE)
      
      cat("  Number participated:", participated, "\n")
      cat("  Number with missing disease:", missing_count, "\n")
      
      total_missing <- total_missing + 
        ifelse(data[[part_var]] == 1 & (is.na(data[[disease_var]]) | (data[[disease_var]] !=1 & data[[disease_var]] !=0)), 1, 0)
    } else {
      cat("  SKIPPING: Variables not found\n")
    }
    cat("\n")
  }
  
  cat("Final missing counts summary:\n")
  print(table(total_missing))
  
  return(total_missing)
}

helsa_all_sp3$missing_count_rarthre2 <- count_missing_by_participation(helsa_all_sp3)
#Check the frequency of missing_count_rarthre2 by total_participations
table(helsa_all_sp3$missing_count_rarthre2, helsa_all_sp3$total_participations,exclude=NULL) #10 individuals with missing values, the maximum missing number is 1
#Select those with missing_count_rarthre2==1
qc <- helsa_all_sp3 %>% filter(missing_count_rarthre2==1) %>% select("idauniq","householdID","inclusion_wave","spousal_part_pattern","total_participations", "r1arthre", "r2arthre", "r3arthre", "r4arthre", "r5arthre", "r6arthre", "r7arthre", "r8arthre", "r9arthre", "missing_count_rarthre2")
#Three individuals with 2 wave participations, which cannot bear any missing in rwarthre in the participating waves
#Select those with missing_count_rarthre2==1 and total_participations==2
qc <- helsa_all_sp3 %>% filter(missing_count_rarthre2==1 & total_participations==2) %>% select("idauniq","householdID","inclusion_wave","spousal_part_pattern","total_participations", "r1arthre", "r2arthre", "r3arthre", "r4arthre", "r5arthre", "r6arthre", "r7arthre", "r8arthre", "r9arthre", "missing_count_rarthre2")

#Exclude individuals with missing_count_rarthre2==1 and total_participations==2 and their paired spouses
helsa_all_sp4 <- helsa_all_sp3 %>% filter(!householdID %in% qc$householdID) #9996 individuals and 4998 spousal pairs

#Check the frequencies of total_participation by inclusion_wave
table(helsa_all_sp3$total_participations, helsa_all_sp3$inclusion_wave, exclude=NULL)
######################################################
#3. The number of individuals died during follow-up (helsa_all_sp27)
######################################################
#Load harmonized end of life dataset
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Waves/UKDA-5050-stata/stata/stata13_se')
helsa_eol <- read_dta("h_elsa_eol_a2.dta")

#Check frequency of radage 
table(helsa_all_sp27$radyear, exclude = NULL) #191 individuals with death year recorded 
qc <- helsa_all_sp27 %>% filter(!is.na(radyear)) %>% select(idauniq, householdID, radyear)

#Check if any individuals in helsa_all_sp26 also in helsa_eol_a2 as well
qc2 <- helsa_all_sp27 %>% filter(idauniq %in% helsa_eol$idauniq) %>% select(idauniq, householdID, radyear) #193 individuals died during follow-up
qc3 <- qc2 %>% filter(!idauniq %in% qc$idauniq)
#Two individuals with exit interview but with missing death year in helsa_all_sp27

#Check among the 302 individuals died during follow-up, how many of them had arthritis 
death <- helsa_eol %>% select(idauniq, radage, racod_e, ragcod)  
sp <- helsa_all_sp27 %>% select(idauniq, householdID, arthritis, rage, ragender, inclusion_wave)
death_arthritis <- death %>% left_join(.,sp,by="idauniq")
table(death_arthritis$arthritis, exclude=NULL) #83 individuals with arthritis

#Select those with interview age older than death age
death_arthritis2 <- death_arthritis %>% filter(radage < rage) #no individuals with death happened before interview

#Check frequency of rwiwstat, values 5 and 6 indicating death, and value 9 indicating 'don't know alive or died'
table(helsa_all_sp27$r1iwstat, exclude=NULL) #No death indicators
table(helsa_all_sp27$r2iwstat, exclude=NULL) #No death indicators
table(helsa_all_sp27$r3iwstat, exclude=NULL) #100 individuals died in wave 3 and 476 individuals with unknown death status
table(helsa_all_sp27$r4iwstat, exclude=NULL) #138 individuals died in wave 4, 100 individual died in wave 3, and 843 individuals with unknown death status
table(helsa_all_sp27$r5iwstat, exclude=NULL) #187 individuals died in wave 5, 238 individual died in waves 3 and 4, and 1126 individuals with unknown death status
table(helsa_all_sp27$r6iwstat, exclude=NULL) #161 individuals died in wave 6, 425 individual died in waves 3-5, and 1640 individuals with unknown death status
table(helsa_all_sp27$r7iwstat, exclude=NULL) #no individuals died in wave 7, 586 individual died in waves 3-6, and 2371 individuals with unknown death status
table(helsa_all_sp27$r8iwstat, exclude=NULL) #no individuals died in wave 8, 586 individual died in waves 3-7, and 3091 individuals with unknown death status
table(helsa_all_sp27$r9iwstat, exclude=NULL) #no individuals died in wave 9, 586 individual died in waves 3-8, and 3922 individuals with unknown death status
######################################################
#4. Descriptive data of selected baseline and outcome variables in helsa_all_sp27
######################################################
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
load("helsa_all_sp27.rda")

#Convert haven:labelled variables into numeric variables 
helsa_all_sp27 <- helsa_all_sp27 %>%
  mutate(across(where(haven::is.labelled), as.numeric)) %>%
  mutate(rdrinkr=as.numeric(rdrinkr))
#Convert missing types into NA
helsa_all_sp27 <- helsa_all_sp27 %>% mutate(rabplace=case_when(
  rabplace==-13 | rabplace==-4 ~ NA,
  TRUE ~ rabplace),
  raeducl=case_when(
    raeducl==-4 | raeducl==-13| raeducl==-15 | raeducl==-18 ~ NA,
    TRUE ~ raeducl),
  rdrinke=case_when(
    rdrinke==-3 | rdrinke==-4 | rdrinke==-13 | rdrinke==-16 ~ NA,
    TRUE ~ rdrinke),
  rsmokev=case_when(
    rsmokev==-4 | rsmokev==-13 | rsmokev==-16 | rsmokev==-18 ~ NA,
    TRUE ~ rsmokev),
  rarelig_e=case_when(
    rarelig_e==-9 | rarelig_e==-13 | rarelig_e==-16 | rarelig_e==-18 | rarelig_e==-23 ~ NA,
    TRUE ~ rarelig_e),
  rmbmicat=case_when(
    rmbmicat==-9 | rmbmicat==-14 | rmbmicat==-16 | rmbmicat==-19 | rmbmicat==-24 ~ NA,
    TRUE ~ rmbmicat),
  roccup=case_when(
    roccup==-4 | roccup==-15 | roccup==-18 ~ NA,
    TRUE ~ roccup),
  rkcnt=case_when(
    rkcnt==-3 | rkcnt==-11 | rkcnt==-13 | rkcnt==-18 ~ NA,
    TRUE ~ rkcnt),
  ramomeduage=case_when(
    ramomeduage==-1 | ramomeduage==-4 | ramomeduage==-18 ~ NA,
    TRUE ~ ramomeduage),
  radadeduage=case_when(
    radadeduage==-1 | radadeduage==-4 | radadeduage==-18 ~ NA,
    TRUE ~ radadeduage),
  rshlt=case_when(
    rshlt==-4 | rshlt==-16 | rshlt==-13 | rshlt==-18 ~ NA,
    TRUE ~ rshlt))

#Define categorical variables
helsa_all_sp27 <- helsa_all_sp27 %>%
  mutate(across(c(rmbmicat, rarelig_e, roccup, ramomeduage, radadeduage), as.factor))

#Select baseline and outcome variables and convert variables with have_labelled to factor variables
descriptive <- helsa_all_sp27 %>%
  select(rage, ragender, rabplace, raracem, rarelig_e, rmcurln, raeducl, rmbmin, rmbmicat, rpact,  rdrinkr, rdrinke, rsmokev, roccup, hincome, rkcnt, ramomeduage, radadeduage, rclstress, rshlt, radlfive, riadlza, rmobilsev, rspsupport, arthritis, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, stroke, psyche, psyche_dm, hchole, hchole_dm, asthmae, asthmae_dm, catracte, parkine, osteoe, osteoe_dm)
#Create publication-ready descriptive table for baseline variables and outcomes 
descriptive_base_var_by_gender <- descriptive %>%  tbl_summary(
  #Comparsion by gender
  by = ragender,
  # Specify variable types
  type = list(all_continuous() ~ "continuous2",
              all_categorical() ~ "categorical"),
  
  # Define statistics format
  statistic = list(
    all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    all_categorical() ~ "{n} ({p}%)"
  ),
  
  # Set digits
  digits = list(all_continuous() ~ 1,
                all_categorical() ~ 1),
  
  # Handle missing values
  missing = "always",
  missing_text = "Missing values",
  
  # Add labels for better presentation
  label = list(
    rage ~ "Age at interview", 
    rabplace ~ "Birthplace",
    raracem ~ "Race",
    rarelig_e ~ "Religion",
    rmcurln ~ "Length of current marriage",
    raeducl ~ "Highest attained education level",
    rmbmin ~ "Measured BMI_cont",
    rmbmicat ~ "Measured BMI_cat",
    rpact ~ "Physcial activity",
    rdrinkr ~ "Number of drinks per day",
    rdrinke ~ "Ever drinks any alcohol",
    rsmokev ~ "Ever smoking",
    roccup ~ "Occupation",
    hincome ~ "Household income",
    rkcnt ~ "Weekly contact with children",
    ramomeduage ~ "Maternal education",
    radadeduage ~ "Paternal education",
    rclstress ~ "Childhood/lifetime stressful events (four vars)",
    rspsupport ~ "Spouse support 6-item score ",
    rshlt ~ "Self-reported health",
    radlfive ~ "ADL summary, five",
    riadlza ~ "IADL summary, five",
    rmobilsev ~ "Mobility summary, seven",
    arthritis ~ "Doctor-diagnosed arthritis",
    hibpe ~ "Doctor-diagnosed hypertension",
    hibpe_dm ~ "Doctor-diagnosed/medication-indicated hypertension",
    diabe ~ "Doctor-diagnosed diabetes",
    diabe_dm ~ "Doctor-diagnosed/medication-indicated diabetes",
    cancre ~ "Doctor-diagnosed cancer",
    cancre_dm ~ "Doctor-diagnosed/medication-indicated cancer",
    lunge ~ "Doctor-diagnosed lung disease",
    lunge_dm ~ "Doctor-diagnosed/medication-indicated lung disease",
    hearte ~ "Doctor-diagnosed heart disease",
    stroke ~ "Doctor-diagnosed stroke",
    psyche ~ "Doctor-diagnosed psychiatric problems",
    psyche_dm ~ "Doctor-diagnosed/medication-indicated psychiatric problems",
    hchole ~ "Doctor-diagnosed high cholesterol",
    hchole_dm ~ "Doctor-diagnosed/medication-indicated high cholesterol",
    asthmae ~ "Doctor-diagnosed asthmae",
    asthmae_dm ~ "Doctor-diagnosed/medication-indicated asthmae",
    parkine ~ "Doctor-diagnosed Parkinson's disease",
    osteoe ~ "Doctor-diagnosed osteoporosis",
    osteoe_dm ~ "Doctor-diagnosed/medication-indicated osteoporosis")) %>%
  # Additional formatting
  add_p(test = list(
    # Chi-square for larger tables
    c(rabplace, raracem,  raeducl,  rmbmicat, rpact, rdrinke, rsmokev, roccup, rkcnt, ramomeduage, radadeduage, rclstress, rshlt, radlfive, riadlza, rmobilsev, arthritis, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, stroke, psyche, psyche_dm, hchole, hchole_dm, asthmae, asthmae_dm, catracte, parkine, osteoe, osteoe_dm) ~ "chisq.test",
    # Fisher's for variables with small cell counts
    rarelig_e ~ "fisher.test",
    # Continuous variables
    all_continuous() ~ "wilcox.test"
    ), test.args = list(rarelig_e ~ list(simulate.p.value =TRUE, B=2000))) %>%  # Add p-values for group comparisons
  add_overall() %>%  # Add overall column
  bold_labels() %>%
  italicize_levels()

#Save descriptive table
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/ELSA')
write_xlsx(descriptive_base_var_by_gender[["table_body"]], path = "elsa_descriptive_base_out_var_by_gender.xlsx", col_names=T, format_headers=T)
######################################################
#5. Check strange values (helsa_all_sp27)
######################################################
#Load dataset helsa_all_sp27
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
load("helsa_all_sp27.rda")

#Select relevant variable
helsa_all_sp27 <- helsa_all_sp27 %>% select(idauniq, rage, rabyear, rabplace, ragender, rarelig_e,rmcurln, raeducl, rmbmin, rmbmicat, rpact, rdrinke, rdrinkr, rsmokev, roccup, hincome, rkcnt, rsoc, ramomeduage, radadeduage, rclstress, rspsupport, rshlt, radlfive, riadlza, rmobilsev, arthritis, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, stroke, psyche, psyche_dm, hchole, hchole_dm, catracte, parkine, osteoe, osteoe_dm, asthmae, asthmae_dm) %>% 
  mutate(across(where(haven::is.labelled), as.numeric))

#Function for checking frequency of each continuous variable
freq <- function(data, var){
  freq_table <- as.data.frame(table(data[[var]], exclude=NULL))
  return(freq_table)
}
freq_table <- freq(helsa_all_sp27, "rage")
freq_table <- freq(helsa_all_sp27, "rabyear")
freq_table <- freq(helsa_all_sp27, "rmcurln")
freq_table <- freq(helsa_all_sp27, "rmbmin") 
freq_table <- freq(helsa_all_sp27, "rdrinkr")
freq_table <- freq(helsa_all_sp27, "hincome") #with negative and zero values
######################################################
#6. Missingness pattern (helsa_all_sp27)
######################################################
#Load dataset helsa_all_sp27 and select relevant variables
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
load("helsa_all_sp27.rda")

helsa_miss <- helsa_all_sp27 %>% select(idauniq, rage, rabyear, ragender, rabplace, raracem, rarelig_e, rmcurln, raeducl, rmbmin, rmbmicat, rpact,  rdrinkr, rdrinke, rsmokev, roccup, roccup2_2, hincome, rkcnt, ramomeduage, radadeduage, rclstress, rshlt, radlfive, riadlza, rmobilsev, rspsupport, arthritis, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, stroke, psyche, psyche_dm, hchole, hchole_dm, asthmae, asthmae_dm, catracte, parkine, osteoe, osteoe_dm)

#Convert missing types into NA
helsa_miss <- helsa_miss %>% mutate(rabplace=case_when(
  rabplace==-13 | rabplace==-4 ~ NA,
  TRUE ~ rabplace),
  raeducl=case_when(
    raeducl==-4 | raeducl==-13| raeducl==-15 | raeducl==-18 ~ NA,
    TRUE ~ raeducl),
  rdrinke=case_when(
    rdrinke==-3 | rdrinke==-4 | rdrinke==-13 | rdrinke==-16 ~ NA,
    TRUE ~ rdrinke),
  rsmokev=case_when(
    rsmokev==-4 | rsmokev==-13 | rsmokev==-16 | rsmokev==-18 ~ NA,
    TRUE ~ rsmokev),
  rarelig_e=case_when(
    rarelig_e==-9 | rarelig_e==-13 | rarelig_e==-16 | rarelig_e==-18 | rarelig_e==-23 ~ NA,
    TRUE ~ rarelig_e),
  rmbmicat=case_when(
    rmbmicat==-9 | rmbmicat==-14 | rmbmicat==-16 | rmbmicat==-19 | rmbmicat==-24 ~ NA,
    TRUE ~ rmbmicat),
  roccup=case_when(
    roccup==-4 | roccup==-15 | roccup==-18 ~ NA,
    TRUE ~ roccup),
  rkcnt=case_when(
    rkcnt==-3 | rkcnt==-11 | rkcnt==-13 | rkcnt==-18 ~ NA,
    TRUE ~ rkcnt),
  ramomeduage=case_when(
    ramomeduage==-1 | ramomeduage==-4 | ramomeduage==-18 ~ NA,
    TRUE ~ ramomeduage),
  radadeduage=case_when(
    radadeduage==-1 | radadeduage==-4 | radadeduage==-18 ~ NA,
    TRUE ~ radadeduage),
  rshlt=case_when(
    rshlt==-4 | rshlt==-16 | rshlt==-13 | rshlt==-18 ~ NA,
    TRUE ~ rshlt))

#Visual Missingness Map
vis_miss(helsa_miss)
#Individuals with length of marriage missing, also likley have childhood/lifetime stressful event missing 

#Visualize missing patterns
helsa_mp <- md.pattern(helsa_miss)
helsa_mp_pair <- md.pairs(helsa_miss) #pairwise missing pattern
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Graphs')
tiff("helsa_missing_patterns.tiff", width = 12, height = 16, units = "in", res = 300)
gg_miss_upset(helsa_miss)
dev.off()

#Check missing distribution between rmbmin and hincome
marginplot(helsa_miss[, c("rmbmin", "rdrinkr")], col=mdc(1:2), cex=1.2, cex.lab=1.2, cex.numbers=1.3, pch=19)
#Similar distribution between observed and missing, likely missing at random

# Show percent of missing values per variable
gg_miss_var(helsa_miss, show_pct = TRUE)

# Get a summary table for each variable
miss_var_summary(helsa_miss)
######################################################
#7. Check multicollinearity (helsa_all_sp28)
######################################################
#Identify highly correlated variables
numeric_vars <- helsa_miss[sapply(helsa_miss, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

#Find variables with correlation > 0.9
high_cor <- which(abs(cor_matrix) > 0.9 & upper.tri(cor_matrix), arr.ind = TRUE)
print(high_cor)
#rage and rabyear is highly correlated, with cor > 0.9

#Save cor_matrix
cor_df <- as.data.frame(cor_matrix)
cor_df$Variable <- rownames(cor_matrix)
cor_df <- cor_df[, c("Variable", setdiff(names(cor_df), "Variable"))]
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/ELSA')
write.csv(cor_df, "elsa_cor_matrix.csv", row.names = FALSE)
######################################################
######################################################
#DATA PREPARATION
#1. List of included variables  
#2. Define outcomes and potential confounders to be adjusted in helsa_all_sp4
#3. Multiple imputation for covariates with missing proportion  5% < x < 40% (helsa_all_sp) 
#4. Post imputation modifications (helsa_all_sp11 and helsa_imp_all_long)
###################################################### 
#1. List of included variables  
###################################################### 
#Load data and select relevant variables
#Load harmonized ELSA data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Waves/UKDA-5050-stata/stata/stata13_se')
helsa <- read_dta("h_elsa_g3.dta") %>% select(1, 5:13, 23:31, 41:63, 78:113, 142:159, 215:223, 296:313, 344:361, 396:443, 462:501, 522:549, 604:621, 694:711, 730:739, 750, 753:782, 1335:1388, 1569:1622, 1767:1820, 2199:2522, 2555:2860, 2915:2932, 2969:3040, 3045:3104, 3115:3194, 3267:3322, 3699:3786, 3803:3856, 4239:4256, 4846:4899, 4936:4998, 5071:5187, 5324:5341, 5533:5550, 5726:5761, 5780:5815, 6052:6069, 6204:6239, 6258:6275, 7198:7213, 8812:8821, 8832:8841, 8872:8881, 8972:9011, 9418:9453)

# Check if labels exist as attributes
var_info <- sapply(helsa, function(x) {
  label <- attr(x, "label")
  ifelse(is.null(label), "", label)
})

# Extract value labels from attributes
val_info <- sapply(helsa, function(x) {
  labels <- attr(x, "labels")
  if(!is.null(labels)) {
    # Format as "1=Label1; 2=Label2; ..."
    paste(paste(labels, "=", names(labels)), collapse = "; ")
  } else {
    ""
  }
})

#Create a detailed table
helsa_variable_table <- data.frame(
  Variable = names(helsa),
  Label = sapply(var_info, function(x) ifelse(is.null(x), "", x)),
  Type = sapply(helsa, function(x) paste(class(x), collapse = ", ")),
  N_Missing = colSums(is.na(helsa)),
  Unique_Values = sapply(helsa, function(x) length(unique(na.omit(x)))),
  stringsAsFactors = FALSE
)

#Add value labels as a separate column
helsa_variable_table$Value_Labels <- sapply(names(helsa), function(var) {
  labels <- val_info[[var]]
  if(!is.null(labels)) {
    paste(paste0(names(labels), " (", labels, ")"), collapse = "; ")
  } else {
    ""
  }
})

#Save as excel file
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Basic information')
write.xlsx(helsa_variable_table, file = "helsa_selected_variable_table_251010.xlsx", colNames=T, format_headers=T)
###################################################### 
#2. Define outcomes and potential confounders to be adjusted in helsa_all_sp4
######################################################  
##Define doctor-diagnosed arthritis (outcome, cases/controls)
######################################################  
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 1 
helsa_all_sp4 <- helsa_all_sp4 %>% mutate(arthritis=case_when(
  inclusion_wave!="Wave 1"~ NA, 
  inclusion_wave=="Wave 1" & if_any(r1arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp4$arthritis, helsa_all_sp4$inclusion_wave, exclude=NULL) #For those included from wave 1, 3246 individuals with no arthritis across 9 waves and 2552 individuals with arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- helsa_all_sp4 %>% filter(arthritis==0) %>% select("idauniq", "householdID","spousal_part_pattern", "arthritis", r1arthre:r9arthre )
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- helsa_all_sp4 %>% filter(arthritis==1) %>% select("idauniq", "householdID","spousal_part_pattern", "arthritis", r1arthre:r9arthre)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL)  #All had 1 
######################################################  
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 2
helsa_all_sp5 <- helsa_all_sp4 %>% mutate(arthritis=case_when(
  inclusion_wave=="Wave 1" ~ arthritis,
  inclusion_wave!="Wave 2" ~ NA_real_,
  inclusion_wave=="Wave 2" & if_any(r2arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp5$arthritis, helsa_all_sp5$inclusion_wave, exclude=NULL) #For those included from wave 2, 90 individuals with no arthritis 64 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- helsa_all_sp5 %>% filter(arthritis==0 & inclusion_wave=="Wave 2") %>% select("idauniq", "householdID", "spousal_part_pattern", "arthritis", r1arthre:r9arthre)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- helsa_all_sp5 %>% filter(arthritis==1 & inclusion_wave=="Wave 2") %>% select("idauniq", "householdID", "spousal_part_pattern", "arthritis", r1arthre:r9arthre)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
###################################################### 
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 3
helsa_all_sp6 <- helsa_all_sp5 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ arthritis,
  inclusion_wave!="Wave 3"  ~ NA_real_, 
  inclusion_wave=="Wave 3" & if_any(r3arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp6$arthritis, helsa_all_sp6$inclusion_wave, exclude=NULL) #For those included from wave 3, 755 individuals with no arthritis and 367 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- helsa_all_sp6 %>% filter(arthritis==0 & inclusion_wave=="Wave 3") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- helsa_all_sp6 %>% filter(arthritis==1 & inclusion_wave=="Wave 3") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
###################################################### 
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 4
helsa_all_sp7 <- helsa_all_sp6 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ arthritis,
  inclusion_wave!="Wave 4"  ~ NA_real_, 
  inclusion_wave=="Wave 4" & if_any(r4arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp7$arthritis, helsa_all_sp7$inclusion_wave, exclude=NULL) #For those included from wave 4, 1000 individuals with no arthritis and 630 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- helsa_all_sp7 %>% filter(arthritis==0 & inclusion_wave=="Wave 4") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- helsa_all_sp7 %>% filter(arthritis==1 & inclusion_wave=="Wave 4") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 5
helsa_all_sp8 <- helsa_all_sp7 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3","Wave 4") ~ arthritis,
  inclusion_wave!="Wave 5" ~ NA_real_, 
  inclusion_wave=="Wave 5" & if_any(r5arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp8$arthritis, helsa_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 5, 120 individuals with no arthritis and 74 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- helsa_all_sp8 %>% filter(arthritis==0 & inclusion_wave=="Wave 5") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- helsa_all_sp8 %>% filter(arthritis==1 & inclusion_wave=="Wave 5") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 6
helsa_all_sp9 <- helsa_all_sp8 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3","Wave 4", "Wave 5") ~ arthritis,
  inclusion_wave!="Wave 6" ~ NA_real_, 
  inclusion_wave=="Wave 6" & if_any(r6arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp9$arthritis, helsa_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 6, 561 individuals with no arthritis and 183 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- helsa_all_sp9 %>% filter(arthritis==0 & inclusion_wave=="Wave 6") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- helsa_all_sp9 %>% filter(arthritis==1 & inclusion_wave=="Wave 6") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 7
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 7
helsa_all_sp10 <- helsa_all_sp9 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3","Wave 4", "Wave 5", "Wave 6") ~ arthritis,
  inclusion_wave!="Wave 7" ~ NA_real_, 
  inclusion_wave=="Wave 7" & if_any(r7arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp10$arthritis, helsa_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 7, 258 individuals with no arthritis and 62 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- helsa_all_sp10 %>% filter(arthritis==0 & inclusion_wave=="Wave 7") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- helsa_all_sp10 %>% filter(arthritis==1 & inclusion_wave=="Wave 7") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 8
helsa_all_sp11 <- helsa_all_sp10 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3","Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ arthritis,
  inclusion_wave!="Wave 8" ~ NA_real_, 
  inclusion_wave=="Wave 8" & if_any(r8arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp11$arthritis, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 8, 26 individuals with no arthritis and 8 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- helsa_all_sp11 %>% filter(arthritis==0 & inclusion_wave=="Wave 8") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- helsa_all_sp11 %>% filter(arthritis==1 & inclusion_wave=="Wave 8") %>% select("idauniq", "householdID", "spousal_part_pattern","arthritis", r1arthre:r9arthre)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
##Define doctor-diagnosed arthritis (exposure)
###################################################### 
helsa_all_sp11 <- helsa_all_sp11 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sarthritis = ifelse(person_num == 1, arthritis[2], arthritis[1])
  ) %>%
  ungroup()

#Check the frequency of sarthritis
table(helsa_all_sp11$sarthritis, exclude=NULL)

#Compare the proportion of men with arthritis who had wives affected by arthritis to those without
male <- helsa_all_sp11 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_arthritis = male$sarthritis
)
prop.table(freq, margin=1) #52% versus 41%
chisq.test(male$arthritis, male$sarthritis) #Significant

#Compare the proportion of women with arthritis who had husbands affected by arthritis to those without
female <- helsa_all_sp11 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_arthritis = female$sarthritis
)
prop.table(freq2, margin=1) #40% versus 30%
chisq.test(female$arthritis, female$sarthritis) #Significant
######################################################  
##Define doctor-diagnosed other chronic conditions
######################################################  
#Function for counting missing number for each chronic conditions for participating waves
count_missing_cd_by_part <- function(data, num_waves = 9, disease_suffix) {
  total_missing <- rep(0, nrow(data))
  
  cat("=== DEBUGGING OUTPUT ===\n")
  cat("Column names in data:\n")
  print(names(data))
  cat("Disease variable suffix:", disease_suffix, "\n\n")
  
  for(i in 1:num_waves) {
    part_var <- paste0("inw", i)
    disease_var <- paste0("r", i, disease_suffix)  # Use the custom suffix
    
    cat("Wave", i, ":\n")
    cat("  Looking for participation variable:", part_var, "\n")
    cat("  Looking for disease variable:", disease_var, "\n")
    
    # Check if columns exist
    part_exists <- part_var %in% names(data)
    disease_exists <- disease_var %in% names(data)
    
    cat("  Participation variable exists:", part_exists, "\n")
    cat("  Disease variable exists:", disease_exists, "\n")
    
    if(part_exists && disease_exists) {
      # Count how many people participated in this wave
      participated <- sum(data[[part_var]] == 1, na.rm = TRUE)
      # Count how many have missing disease data among participants
      missing_count <- sum(data[[part_var]] == 1 & (is.na(data[[disease_var]]) | (data[[disease_var]] !=1 & data[[disease_var]] !=0)), na.rm = TRUE)
      
      cat("  Number participated:", participated, "\n")
      cat("  Number with missing disease:", missing_count, "\n")
      
      total_missing <- total_missing + 
        ifelse(data[[part_var]] == 1 & (is.na(data[[disease_var]]) | (data[[disease_var]] !=1 & data[[disease_var]] !=0)), 1, 0)
    } else {
      cat("  SKIPPING: Variables not found\n")
    }
    cat("\n")
  }
  
  cat("Final missing counts summary:\n")
  print(table(total_missing))
  
  return(total_missing)
}

helsa_all_sp11$missing_count_hibpe <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="hibpe")
helsa_all_sp11$missing_count_diabe <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="diabe")
helsa_all_sp11$missing_count_cancre <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="cancre")
helsa_all_sp11$missing_count_lunge <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="lunge")
helsa_all_sp11$missing_count_hearte <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="hearte")
helsa_all_sp11$missing_count_stroke <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="stroke")
helsa_all_sp11$missing_count_psyche <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="psyche")
helsa_all_sp11$missing_count_asthmae <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="asthmae")
helsa_all_sp11$missing_count_hchole <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="hchole")
helsa_all_sp11$missing_count_catracte <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="catracte")
helsa_all_sp11$missing_count_parkine <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="parkine")
helsa_all_sp11$missing_count_osteoe <- count_missing_cd_by_part(data=helsa_all_sp11, disease_suffix="osteoe")

#All indiviudals with maximum 1 missing values in other chronic diseases across waves
#The number of individuals with missing values and participating in two waves only
table(helsa_all_sp11$missing_count_hibpe, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_diabe, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_cancre, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_lunge, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_hearte, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_stroke, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_psyche, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_asthmae, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_hchole, helsa_all_sp11$total_participations, exclude=NULL) #2
table(helsa_all_sp11$missing_count_catracte, helsa_all_sp11$total_participations, exclude=NULL) #2
table(helsa_all_sp11$missing_count_parkine, helsa_all_sp11$total_participations, exclude=NULL) #none
table(helsa_all_sp11$missing_count_osteoe, helsa_all_sp11$total_participations, exclude=NULL) #none
#Mark those participated in only two waves and with missing values of high cholesterol/catracts, 
helsa_all_sp11 <- helsa_all_sp11 %>% mutate(catracte_exclude=ifelse(missing_count_catracte==1 & total_participations==2,1,0), hchole_exclude=ifelse(missing_count_hchole==1 & total_participations==2,1,0))
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
helsa_all_sp11 <- helsa_all_sp11 %>% mutate(hibpe=case_when(
  inclusion_wave!="Wave 1"~ NA_real_,
  inclusion_wave=="Wave 1" & if_any(r1hibpe:r9hibpe, ~ .x==1) ~ 1, 
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1diabe:r9diabe, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1cancre:r9cancre, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1lunge:r9lunge, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1hearte:r9hearte, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1stroke:r9stroke, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1psyche:r9psyche, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  hchole=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r2hchole:r9hchole, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1asthmae:r9asthmae, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1catracte:r9catracte, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  parkine=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1parkine:r9parkine, ~ .x==1) ~ 1, 
    TRUE ~ 0),
  osteoe=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(r1osteoe:r9osteoe, ~ .x==1) ~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp11$hibpe, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 2796 individuals with no high blood pressure/hypertension across 9 waves and 3002 individuals with high blood pressure/hypertension
table(helsa_all_sp11$diabe, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 4993 individuals with no diabetes across 9 waves and 805 individuals with diabetes
table(helsa_all_sp11$cancre, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 4872 individuals with no cancer across 9 waves and 926 individuals with cancer
table(helsa_all_sp11$lunge, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5247 individuals with no lung diseases across 9 waves and 551 individuals with lung diseases
table(helsa_all_sp11$hearte, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 4120 individuals with no heart diseases across 9 waves and 1678 individuals with heart diseases
table(helsa_all_sp11$stroke, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5375 individuals with no stroke across 9 waves and 423 individuals with stroke
table(helsa_all_sp11$psyche, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5209 individuals with no psychiatric conditions across 9 waves and 589 individuals with psychiatric conditions
table(helsa_all_sp11$hchole, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 3418 individuals with no high cholesterol across 9 waves and 2380 individuals with high cholesterol, 2 individuals with NA
table(helsa_all_sp11$asthmae, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 4963 individuals with no asthma across 9 waves and 835 individuals with asthma
table(helsa_all_sp11$catracte, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 3701 individuals with no catracts across 9 waves and 2095 individuals with catracts, 2 individuals with NA
table(helsa_all_sp11$parkine, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5715 individuals with no Parkinson's disease across 9 waves and 83 individuals with Parkinson's disease
table(helsa_all_sp11$osteoe, helsa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5246 individuals with no osteoporosis across 9 waves and 552 individuals with osteoporosis
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2
helsa_all_sp12 <- helsa_all_sp11 %>% mutate(hibpe=case_when(
  inclusion_wave=="Wave 1" ~ hibpe,
  inclusion_wave!="Wave 2" ~ NA_real_, 
  inclusion_wave=="Wave 2" & if_any(r2hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave=="Wave 1" ~ diabe,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave=="Wave 1" ~ cancre,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave=="Wave 1" ~ lunge,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave=="Wave 1" ~ hearte,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave=="Wave 1" ~ stroke,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave=="Wave 1" ~ psyche,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ hchole,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave=="Wave 1" ~ asthmae,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2asthmae:r9asthmae, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ catracte,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    inclusion_wave=="Wave 1" ~ parkine,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  osteoe=case_when(
    inclusion_wave=="Wave 1" ~ osteoe,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(r2osteoe:r9osteoe, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp12$hibpe, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 72 individuals with no high blood pressure/hypertension across 9 waves and 82 individuals with high blood pressure/hypertension
table(helsa_all_sp12$diabe, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 132 individuals with no diabetes across 9 waves and 22 individuals with diabetes
table(helsa_all_sp12$cancre, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 137 individuals with no cancer across 9 waves and 17 individuals with cancer
table(helsa_all_sp12$lunge, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 144 individuals with no lung diseases across 9 waves and 10 individuals with lung diseases
table(helsa_all_sp12$hearte, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 109 individuals with no heart diseases across 9 waves and 45 individuals with heart diseases
table(helsa_all_sp12$stroke, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 144 individuals with no stroke across 9 waves and 10 individuals with stroke
table(helsa_all_sp12$psyche, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 139 individuals with no psychiatric conditions across 9 waves and 15 individuals with psychiatric conditions
table(helsa_all_sp12$hchole, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 87 individuals with no high cholesterol across 9 waves and 67 individuals with high cholesterol
table(helsa_all_sp12$asthmae, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 135 individuals with no asthma across 9 waves and 19 individuals with asthma
table(helsa_all_sp12$catracte, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 98 individuals with no catracts across 9 waves and 56 individuals with catracts
table(helsa_all_sp12$parkine, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 153 individuals with no Parkinson's disease across 9 waves and 1 individuals with Parkinson's disease
table(helsa_all_sp12$osteoe, helsa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 143 individuals with no osteoporosis across 9 waves and 11 individuals with osteoporosis
######################################################
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 3
helsa_all_sp13 <- helsa_all_sp12 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ hibpe,
  inclusion_wave!="Wave 3" ~ NA_real_, 
  inclusion_wave=="Wave 3" & if_any(r3hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ diabe,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ cancre,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ lunge,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hearte,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ stroke,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ psyche,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hchole,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ asthmae,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3asthmae:r9asthmae, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ catracte,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ parkine,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  osteoe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ osteoe,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3osteoe:r9osteoe, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp13$hibpe, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 706 individuals with no high blood pressure/hypertension across 9 waves and 416 individuals with high blood pressure/hypertension
table(helsa_all_sp13$diabe, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 986 individuals with no diabetes across 9 waves and 136 individuals with diabetes
table(helsa_all_sp13$cancre, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 1028 individuals with no cancer across 9 waves and 94 individuals with cancer
table(helsa_all_sp13$lunge, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 1079 individuals with no lung diseases across 9 waves and 43 individuals with lung diseases
table(helsa_all_sp13$hearte, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 903 individuals with no heart diseases across 9 waves and 219 individuals with heart diseases
table(helsa_all_sp13$stroke, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 1090 individuals with no stroke across 9 waves and 32 individuals with stroke
table(helsa_all_sp13$psyche, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 996 individuals with no psychiatric conditions across 9 waves and 126 individuals with psychiatric conditions
table(helsa_all_sp13$hchole, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 716 individuals with no high cholesterol across 9 waves and 406 individuals with high cholesterol
table(helsa_all_sp13$asthmae, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 969 individuals with no asthma across 9 waves and 153 individuals with asthma
table(helsa_all_sp13$catracte, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 929 individuals with no catracts across 9 waves and 193 individuals with catracts
table(helsa_all_sp13$parkine, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 1114 individuals with no Parkinson's disease across 9 waves and 8 individuals with Parkinson's disease
table(helsa_all_sp13$osteoe, helsa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 3, 1057 individuals with no osteoporosis across 9 waves and 65 individuals with osteoporosis
######################################################
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 4
helsa_all_sp14 <- helsa_all_sp13 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hibpe,
  inclusion_wave!="Wave 4" ~ NA_real_, 
  inclusion_wave=="Wave 4" & if_any(r4hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ diabe,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ cancre,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ lunge,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hearte,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ stroke,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ psyche,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hchole,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ asthmae,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4asthmae:r9asthmae, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ catracte,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ parkine,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  osteoe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ osteoe,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4osteoe:r9osteoe, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp14$hibpe, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 953 individuals with no high blood pressure/hypertension across 9 waves and 677 individuals with high blood pressure/hypertension
table(helsa_all_sp14$diabe, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1414 individuals with no diabetes across 9 waves and 216 individuals with diabetes
table(helsa_all_sp14$cancre, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1397 individuals with no cancer across 9 waves and 233 individuals with cancer
table(helsa_all_sp14$lunge, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1530 individuals with no lung diseases across 9 waves and 100 individuals with lung diseases
table(helsa_all_sp14$hearte, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1257 individuals with no heart diseases across 9 waves and 373 individuals with heart diseases
table(helsa_all_sp14$stroke, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1556 individuals with no stroke across 9 waves and 74 individuals with stroke
table(helsa_all_sp14$psyche, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1459 individuals with no psychiatric conditions across 9 waves and 171 individuals with psychiatric conditions
table(helsa_all_sp14$hchole, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 939 individuals with no high cholesterol across 9 waves and 691 individuals with high cholesterol
table(helsa_all_sp14$asthmae, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1410 individuals with no asthma across 9 waves and 220 individuals with asthma
table(helsa_all_sp14$catracte, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1094 individuals with no catracts across 9 waves and 536 individuals with catracts
table(helsa_all_sp14$parkine, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1619 individuals with no Parkinson's disease across 9 waves and 11 individuals with Parkinson's disease
table(helsa_all_sp14$osteoe, helsa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 4, 1497 individuals with no osteoporosis across 9 waves and 133 individuals with osteoporosis
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 5
helsa_all_sp15 <- helsa_all_sp14 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hibpe,
  inclusion_wave!="Wave 5" ~ NA_real_, 
  inclusion_wave=="Wave 5" & if_any(r5hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ diabe,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ cancre,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ lunge,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hearte,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ stroke,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ psyche,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hchole,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ asthmae,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5asthmae:r9asthmae, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ catracte,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ parkine,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  osteoe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ osteoe,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5osteoe:r9osteoe, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp15$hibpe, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 109 individuals with no high blood pressure/hypertension across 9 waves and 85 individuals with high blood pressure/hypertension
table(helsa_all_sp15$diabe, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 161 individuals with no diabetes across 9 waves and 33 individuals with diabetes
table(helsa_all_sp15$cancre, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 172 individuals with no cancer across 9 waves and 22 individuals with cancer
table(helsa_all_sp15$lunge, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 180 individuals with no lung diseases across 9 waves and 14 individuals with lung diseases
table(helsa_all_sp15$hearte, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 156 individuals with no heart diseases across 9 waves and 38 individuals with heart diseases
table(helsa_all_sp15$stroke, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 186 individuals with no stroke across 9 waves and 8 individuals with stroke
table(helsa_all_sp15$psyche, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 170 individuals with no psychiatric conditions across 9 waves and 24 individuals with psychiatric conditions
table(helsa_all_sp15$hchole, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 107 individuals with no high cholesterol across 9 waves and 87 individuals with high cholesterol
table(helsa_all_sp15$asthmae, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 176 individuals with no asthma across 9 waves and 18 individuals with asthma
table(helsa_all_sp15$catracte, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 150 individuals with no catracts across 9 waves and 44 individuals with catracts
table(helsa_all_sp15$parkine, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 191 individuals with no Parkinson's disease across 9 waves and 3 individuals with Parkinson's disease
table(helsa_all_sp15$osteoe, helsa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 5, 177 individuals with no osteoporosis across 9 waves and 17 individuals with osteoporosis
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 6
helsa_all_sp16 <- helsa_all_sp15 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ hibpe,
  inclusion_wave!="Wave 6" ~ NA_real_, 
  inclusion_wave=="Wave 6" & if_any(r6hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ diabe,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ cancre,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ lunge,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ hearte,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ stroke,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ psyche,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ hchole,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ asthmae,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6asthmae:r9asthmae, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ catracte,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ parkine,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  osteoe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ osteoe,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6osteoe:r9osteoe, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp16$hibpe, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 490 individuals with no high blood pressure/hypertension across 9 waves and 254 individuals with high blood pressure/hypertension
table(helsa_all_sp16$diabe, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 654 individuals with no diabetes across 9 waves and 90 individuals with diabetes
table(helsa_all_sp16$cancre, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 671 individuals with no cancer across 9 waves and 73 individuals with cancer
table(helsa_all_sp16$lunge, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 718 individuals with no lung diseases across 9 waves and 26 individuals with lung diseases
table(helsa_all_sp16$hearte, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 621 individuals with no heart diseases across 9 waves and 123 individuals with heart diseases
table(helsa_all_sp16$stroke, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 724 individuals with no stroke across 9 waves and 20 individuals with stroke
table(helsa_all_sp16$psyche, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 644 individuals with no psychiatric conditions across 9 waves and 80 individuals with psychiatric conditions
table(helsa_all_sp16$hchole, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 511 individuals with no high cholesterol across 9 waves and 233 individuals with high cholesterol
table(helsa_all_sp16$asthmae, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 643 individuals with no asthma across 9 waves and 101 individuals with asthma
table(helsa_all_sp16$catracte, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 660 individuals with no catracts across 9 waves and 84 individuals with catracts
table(helsa_all_sp16$parkine, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 741 individuals with no Parkinson's disease across 9 waves and 3 individuals with Parkinson's disease
table(helsa_all_sp16$osteoe, helsa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 6, 717 individuals with no osteoporosis across 9 waves and 27 individuals with osteoporosis
######################################################
#Inclusion in Wave 7
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 7
helsa_all_sp17 <- helsa_all_sp16 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ hibpe,
  inclusion_wave!="Wave 7" ~ NA_real_, 
  inclusion_wave=="Wave 7" & if_any(r7hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ diabe,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ cancre,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ lunge,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ hearte,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ stroke,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ psyche,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ hchole,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ asthmae,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7asthmae:r9asthmae, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ catracte,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ parkine,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  osteoe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ osteoe,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7osteoe:r9osteoe, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp17$hibpe, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 253 individuals with no high blood pressure/hypertension across 9 waves and 67 individuals with high blood pressure/hypertension
table(helsa_all_sp17$diabe, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 300 individuals with no diabetes across 9 waves and 20 individuals with diabetes
table(helsa_all_sp17$cancre, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 300 individuals with no cancer across 9 waves and 20 individuals with cancer
table(helsa_all_sp17$lunge, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 307 individuals with no lung diseases across 9 waves and 13 individuals with lung diseases
table(helsa_all_sp17$hearte, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 281 individuals with no heart diseases across 9 waves and 39 individuals with heart diseases
table(helsa_all_sp17$stroke, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 312 individuals with no stroke across 9 waves and 8 individuals with stroke
table(helsa_all_sp17$psyche, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 273 individuals with no psychiatric conditions across 9 waves and 47 individuals with psychiatric conditions
table(helsa_all_sp17$hchole, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 249 individuals with no high cholesterol across 9 waves and 71 individuals with high cholesterol
table(helsa_all_sp17$asthmae, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 275 individuals with no asthma across 9 waves and 45 individuals with asthma
table(helsa_all_sp17$catracte, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 292 individuals with no catracts across 9 waves and 28 individuals with catracts
table(helsa_all_sp17$parkine, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 318 individuals with no Parkinson's disease across 9 waves and 2 individuals with Parkinson's disease
table(helsa_all_sp17$osteoe, helsa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 7, 308 individuals with no osteoporosis across 9 waves and 12 individuals with osteoporosis
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 8
helsa_all_sp18 <- helsa_all_sp17 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hibpe,
  inclusion_wave!="Wave 8" ~ NA_real_, 
  inclusion_wave=="Wave 8" & if_any(r8hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ diabe,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ cancre,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ lunge,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hearte,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ stroke,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ psyche,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hchole,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ asthmae,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8asthmae:r9asthmae, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ catracte,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ parkine,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  osteoe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ osteoe,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8osteoe:r9osteoe, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp18$hibpe, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 28 individuals with no high blood pressure/hypertension across 9 waves and 6 individuals with high blood pressure/hypertension
table(helsa_all_sp18$diabe, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 33 individuals with no diabetes across 9 waves and 1 individuals with diabetes
table(helsa_all_sp18$cancre, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 31 individuals with no cancer across 9 waves and 3 individuals with cancer
table(helsa_all_sp18$lunge, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 32 individuals with no lung diseases across 9 waves and 2 individuals with lung diseases
table(helsa_all_sp18$hearte, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 28 individuals with no heart diseases across 9 waves and 6 individuals with heart diseases
table(helsa_all_sp18$stroke, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 32 individuals with no stroke across 9 waves and 2 individuals with stroke
table(helsa_all_sp18$psyche, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 29 individuals with no psychiatric conditions across 9 waves and 5 individuals with psychiatric conditions
table(helsa_all_sp18$hchole, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 23 individuals with no high cholesterol across 9 waves and 11 individuals with high cholesterol
table(helsa_all_sp18$asthmae, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 29 individuals with no asthma across 9 waves and 5 individuals with asthma
table(helsa_all_sp18$catracte, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 26 individuals with no catracts across 9 waves and 8 individuals with catracts
table(helsa_all_sp18$parkine, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 34 individuals with no Parkinson's disease across 9 waves and 0 individuals with Parkinson's disease
table(helsa_all_sp18$osteoe, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 8, 32 individuals with no osteoporosis across 9 waves and 2 individuals with osteoporosis
######################################################
##Define doctor-diagnosed other chronic conditions among spousal pairs
###################################################### 
helsa_all_sp18 <- helsa_all_sp18 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    shibpe = ifelse(person_num == 1, hibpe[2], hibpe[1]),
    sdiabe = ifelse(person_num == 1, diabe[2], diabe[1]),  
    scancre = ifelse(person_num == 1, cancre[2], cancre[1]),
    slunge = ifelse(person_num == 1, lunge[2], lunge[1]),
    shearte = ifelse(person_num == 1, hearte[2], hearte[1]),
    sstroke = ifelse(person_num == 1, stroke[2], stroke[1]),
    spsyche = ifelse(person_num == 1, psyche[2], psyche[1]),
    shchole = ifelse(person_num == 1, hchole[2], hchole[1]),
    sasthmae = ifelse(person_num == 1, asthmae[2], asthmae[1]),
    scatracte = ifelse(person_num == 1, catracte[2], catracte[1]),
    sparkine = ifelse(person_num == 1, parkine[2], parkine[1]),
    sosteoe = ifelse(person_num == 1, osteoe[2], osteoe[1])
  ) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(helsa_all_sp18$shibpe, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$sdiabe, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$scancre, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$slunge, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$shearte, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$sstroke, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$spsyche, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$shchole, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$scatracte, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$sparkine, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$sosteoe, helsa_all_sp18$inclusion_wave, exclude=NULL)
table(helsa_all_sp18$sasthmae, helsa_all_sp18$inclusion_wave, exclude=NULL)

##Compare the proportion of men with arthritis who had wives affected by other chronic conditions to those without
#High blood pressure/hypertension
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hibpe = male$shibpe
)
prop.table(freq, margin = 1) #45% versus 42%
chisq.test(male$arthritis, male$shibpe) #Significant

#High blood sugar/diabetes
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_diabe = male$sdiabe
)
prop.table(freq, margin = 1) #12% versus 9%
chisq.test(male$arthritis, male$sdiabe) #Significant

#Cancer
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_cancre = male$scancre
)
prop.table(freq, margin = 1) #14% versus 13%
chisq.test(male$arthritis, male$scancre) #Non-significant

#Lung diseases
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_lunge = male$slunge
)
prop.table(freq, margin = 1) #8% versus 5%
chisq.test(male$arthritis, male$slunge) #Significant

#Heart diseases
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hearte = male$shearte
)
prop.table(freq, margin = 1) #26% versus 19%
chisq.test(male$arthritis, male$shearte) #Significant

#Stroke
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_stroke= male$sstroke
)
prop.table(freq, margin = 1) #6% versus 4%
chisq.test(male$arthritis, male$sstroke) #Significant

#Psychiatric conditions
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_psyche= male$spsyche
)
prop.table(freq, margin = 1) #15% versus 12%
chisq.test(male$arthritis, male$spsyche) #Significant

#High cholesterol 
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hchole= male$shchole
)
prop.table(freq, margin = 1) #41% versus 36%
chisq.test(male$arthritis, male$shchole) #Significant

#Catracts
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_livere= male$scatracte
)
prop.table(freq, margin = 1) #36% versus 30%
chisq.test(male$arthritis, male$scatracte) #Significant

#Parkinson's diseases
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_kidneye= male$sparkine
)
prop.table(freq, margin = 1) #0.4% versus 0.5%
chisq.test(male$arthritis, male$sparkine) #Non-significant

#Osteoporosis 
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_digeste= male$sosteoe
)
prop.table(freq, margin = 1) #14% versus 12%
chisq.test(male$arthritis, male$sosteoe) #Non-significant

#Asthma
male <- helsa_all_sp18 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_asthmae= male$sasthmae
)
prop.table(freq, margin = 1) #17% versus 15%
chisq.test(male$arthritis, male$sasthmae) #Significant

##Compare the proportion of women with arthritis who had husbands affected by other chronic conditions to those without
#High blood pressure/hypertension
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hibpe = female$shibpe
)
prop.table(freq2, margin = 1) #53% versus 45%
chisq.test(female$arthritis, female$shibpe) #Significant

#High blood sugar/diabetes
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_diabe = female$sdiabe
)
prop.table(freq2, margin = 1) #19% versus 14%
chisq.test(female$arthritis, female$sdiabe) #Significant

#Cancer
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_cancre = female$scancre
)
prop.table(freq2, margin = 1) #17% versus 12%
chisq.test(female$arthritis, female$scancre) #Non-significant

#Lung diseases
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_lunge = female$slunge
)
prop.table(freq2, margin = 1) #12% versus 7%
chisq.test(female$arthritis, female$slunge) #Significant

#Heart diseases
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hearte = female$shearte
)
prop.table(freq2, margin = 1) #34% versus 25%
chisq.test(female$arthritis, female$shearte) #Significant

#Stroke
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_stroke= female$sstroke
)
prop.table(freq2, margin = 1) #8% versus 6%
chisq.test(female$arthritis, female$sstroke) #Significant

#Psychiatric conditions
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_psyche= female$spsyche
)
prop.table(freq2, margin = 1) #9% versus 8%
chisq.test(female$arthritis, female$spsyche) #Non-significant

#High cholesterol 
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hchole= female$shchole
)
prop.table(freq2, margin = 1) #45% versus 38%
chisq.test(female$arthritis, female$shchole) #Significant

#Catracts
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_livere= female$scatracte
)
prop.table(freq2, margin = 1) #35% versus 24%
chisq.test(female$arthritis, female$scatracte) #Significant

#Parkinson's diseases
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_kidneye= female$sparkine
)
prop.table(freq2, margin = 1) #2% versus 1%
chisq.test(female$arthritis, female$sparkine) #Significant

#Osteoporosis 
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_digeste= female$sosteoe
)
prop.table(freq2, margin = 1) #4% versus 3%
chisq.test(female$arthritis, female$sosteoe) #Significant

#Asthma
female <- helsa_all_sp18 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_asthmae= female$sasthmae
)
prop.table(freq2, margin = 1) #14% versus 11%
chisq.test(female$arthritis, female$sasthmae) #Significant
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions 
#Requiring individuals not exceeding the upper limit missing values of doctor-diagnosed variables only as operator "OR" not "AND" was used
#The number of NA becames smaller when including medication variables to define other chronic conditions, which is unexpected and should use the other chronic condition variables defined by doctor-diagnosed only to refine rows with NA
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
helsa_all_sp18 <- helsa_all_sp18 %>% mutate(hibpe_dm=case_when(
  inclusion_wave!="Wave 1"~ NA_real_,
  inclusion_wave=="Wave 1" & (if_any(r1hibpe:r9hibpe, ~ .x==1) | if_any(r1rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1diabe:r9diabe, ~ .x==1) | if_any(r1rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1cancre:r9cancre, ~ .x==1) | if_any(r1trcancr:r9trcancr, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1lunge:r9lunge, ~ .x==1) | if_any(r1rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1psyche:r9psyche, ~ .x==1) | if_any(c(r2rxdepres, r4rxdepres, r8rxdepres), ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r2hchole:r9hchole, ~ .x==1) | if_any(r3rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  asthmae_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1asthmae:r9asthmae, ~ .x==1) | if_any(r1rxasthma:r9rxasthma, ~ .x==1))~ 1, 
    TRUE ~ 0),
  osteoe_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1osteoe:r9osteoe, ~ .x==1) | if_any(c(r2rxosteo, r5rxosteo:r9rxosteo), ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp18$hibpe_dm, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 1, 2719 individuals with no high blood pressure/hypertension across 9 waves and 3079 individuals with high blood pressure/hypertension
table(helsa_all_sp18$diabe_dm, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 1, 4967 individuals with no diabetes across 9 waves and 831 individuals with diabetes
table(helsa_all_sp18$cancre_dm, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 1, 4852 individuals with no cancer across 9 waves and 946 individuals with cancer
table(helsa_all_sp18$lunge_dm, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 1, 5225 individuals with no lung diseases across 9 waves and 573 individuals with lung diseases
table(helsa_all_sp18$psyche_dm, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 1, 5188 individuals with no psychiatric conditions across 9 waves and 610 individuals with psychiatric conditions
table(helsa_all_sp18$hchole_dm, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 1, 3279 individuals with no high cholesterol across 9 waves and 2519 individuals with high cholesterol, 2 individuals with NA
table(helsa_all_sp18$asthmae_dm, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 1, 4929 individuals with no asthma across 9 waves and 869 individuals with asthma
table(helsa_all_sp18$osteoe_dm, helsa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 1, 5202 individuals with no osteoporosis across 9 waves and 596 individuals with osteoporosis
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2 
helsa_all_sp19 <- helsa_all_sp18 %>% mutate(hibpe_dm=case_when(
  inclusion_wave=="Wave 1" ~ hibpe_dm,
  inclusion_wave!="Wave 2"~ NA_real_,
  inclusion_wave=="Wave 2" & (if_any(r2hibpe:r9hibpe, ~ .x==1) | if_any(r2rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave=="Wave 1" ~ diabe_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2diabe:r9diabe, ~ .x==1) | if_any(r2rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave=="Wave 1" ~ cancre_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2cancre:r9cancre, ~ .x==1) | if_any(r2trcancr:r9trcancr, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave=="Wave 1" ~ lunge_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2lunge:r9lunge, ~ .x==1) | if_any(r2rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave=="Wave 1" ~ psyche_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2psyche:r9psyche, ~ .x==1) | if_any(c(r2rxdepres, r4rxdepres, r8rxdepres), ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ hchole_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2hchole:r9hchole, ~ .x==1) | if_any(r3rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  asthmae_dm=case_when(
    inclusion_wave=="Wave 1" ~ asthmae_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2asthmae:r9asthmae, ~ .x==1) | if_any(r2rxasthma:r9rxasthma, ~ .x==1))~ 1, 
    TRUE ~ 0),
  osteoe_dm=case_when(
    inclusion_wave=="Wave 1" ~ osteoe_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2osteoe:r9osteoe, ~ .x==1) | if_any(c(r2rxosteo, r5rxosteo:r9rxosteo), ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp19$hibpe_dm, helsa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 2, 70 individuals with no high blood pressure/hypertension across 9 waves and 84 individuals with high blood pressure/hypertension
table(helsa_all_sp19$diabe_dm, helsa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 2, 132 individuals with no diabetes across 9 waves and 22 individuals with diabetes
table(helsa_all_sp19$cancre_dm, helsa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 2, 137 individuals with no cancer across 9 waves and 17 individuals with cancer
table(helsa_all_sp19$lunge_dm, helsa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 2, 143 individuals with no lung diseases across 9 waves and 11 individuals with lung diseases
table(helsa_all_sp19$psyche_dm, helsa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 2, 138 individuals with no psychiatric conditions across 9 waves and 16 individuals with psychiatric conditions
table(helsa_all_sp19$hchole_dm, helsa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 2, 83 individuals with no high cholesterol across 9 waves and 71 individuals with high cholesterol
table(helsa_all_sp19$asthmae_dm, helsa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 2, 135 individuals with no asthma across 9 waves and 19 individuals with asthma
table(helsa_all_sp19$osteoe_dm, helsa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 2, 143 individuals with no osteoporosis across 9 waves and 11 individuals with osteoporosis
######################################################
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 3 
helsa_all_sp20 <- helsa_all_sp19 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ hibpe_dm,
  inclusion_wave!="Wave 3"~ NA_real_,
  inclusion_wave=="Wave 3" & (if_any(r3hibpe:r9hibpe, ~ .x==1) | if_any(r3rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ diabe_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3diabe:r9diabe, ~ .x==1) | if_any(r3rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ cancre_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3cancre:r9cancre, ~ .x==1) | if_any(r3trcancr:r9trcancr, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ lunge_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3lunge:r9lunge, ~ .x==1) | if_any(r3rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ psyche_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3psyche:r9psyche, ~ .x==1) | if_any(c(r4rxdepres, r8rxdepres), ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hchole_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3hchole:r9hchole, ~ .x==1) | if_any(r3rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  asthmae_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ asthmae_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3asthmae:r9asthmae, ~ .x==1) | if_any(r3rxasthma:r9rxasthma, ~ .x==1))~ 1, 
    TRUE ~ 0),
  osteoe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ osteoe_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3osteoe:r9osteoe, ~ .x==1) | if_any(c(r5rxosteo:r9rxosteo), ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp20$hibpe_dm, helsa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 3, 698 individuals with no high blood pressure/hypertension across 9 waves and 424 individuals with high blood pressure/hypertension
table(helsa_all_sp20$diabe_dm, helsa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 3, 985 individuals with no diabetes across 9 waves and 137 individuals with diabetes
table(helsa_all_sp20$cancre_dm, helsa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 3, 1028 individuals with no cancer across 9 waves and 94 individuals with cancer
table(helsa_all_sp20$lunge_dm, helsa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 3, 1077 individuals with no lung diseases across 9 waves and 45 individuals with lung diseases
table(helsa_all_sp20$psyche_dm, helsa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 3, 991 individuals with no psychiatric conditions across 9 waves and 131 individuals with psychiatric conditions
table(helsa_all_sp20$hchole_dm, helsa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 3, 694 individuals with no high cholesterol across 9 waves and 428 individuals with high cholesterol
table(helsa_all_sp20$asthmae_dm, helsa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 3, 962 individuals with no asthma across 9 waves and 160 individuals with asthma
table(helsa_all_sp20$osteoe_dm, helsa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 3, 1053 individuals with no osteoporosis across 9 waves and 69 individuals with osteoporosis
######################################################
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 4
helsa_all_sp21 <- helsa_all_sp20 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hibpe_dm,
  inclusion_wave!="Wave 4"~ NA_real_,
  inclusion_wave=="Wave 4" & (if_any(r4hibpe:r9hibpe, ~ .x==1) | if_any(r4rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ diabe_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4diabe:r9diabe, ~ .x==1) | if_any(r4rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ cancre_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4cancre:r9cancre, ~ .x==1) | if_any(r4trcancr:r9trcancr, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ lunge_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4lunge:r9lunge, ~ .x==1) | if_any(r4rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ psyche_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4psyche:r9psyche, ~ .x==1) | if_any(c(r4rxdepres, r8rxdepres), ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hchole_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4hchole:r9hchole, ~ .x==1) | if_any(r4rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  asthmae_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ asthmae_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4asthmae:r9asthmae, ~ .x==1) | if_any(r4rxasthma:r9rxasthma, ~ .x==1))~ 1, 
    TRUE ~ 0),
  osteoe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ osteoe_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4osteoe:r9osteoe, ~ .x==1) | if_any(c(r5rxosteo:r9rxosteo), ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp21$hibpe_dm, helsa_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 4, 932 individuals with no high blood pressure/hypertension across 9 waves and 698 individuals with high blood pressure/hypertension
table(helsa_all_sp21$diabe_dm, helsa_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 4, 1408 individuals with no diabetes across 9 waves and 222 individuals with diabetes
table(helsa_all_sp21$cancre_dm, helsa_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 4, 1390 individuals with no cancer across 9 waves and 240 individuals with cancer
table(helsa_all_sp21$lunge_dm, helsa_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 4, 1525 individuals with no lung diseases across 9 waves and 105 individuals with lung diseases
table(helsa_all_sp21$psyche_dm, helsa_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 4, 1452 individuals with no psychiatric conditions across 9 waves and 178 individuals with psychiatric conditions
table(helsa_all_sp21$hchole_dm, helsa_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 4, 906 individuals with no high cholesterol across 9 waves and 724 individuals with high cholesterol
table(helsa_all_sp21$asthmae_dm, helsa_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 4, 1402 individuals with no asthma across 9 waves and 228 individuals with asthma
table(helsa_all_sp21$osteoe_dm, helsa_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 4, 1491 individuals with no osteoporosis across 9 waves and 139 individuals with osteoporosis
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 5
helsa_all_sp22 <- helsa_all_sp21 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hibpe_dm,
  inclusion_wave!="Wave 5"~ NA_real_,
  inclusion_wave=="Wave 5" & (if_any(r5hibpe:r9hibpe, ~ .x==1) | if_any(r5rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ diabe_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5diabe:r9diabe, ~ .x==1) | if_any(r5rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ cancre_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5cancre:r9cancre, ~ .x==1) | if_any(r5trcancr:r9trcancr, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ lunge_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5lunge:r9lunge, ~ .x==1) | if_any(r5rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ psyche_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5psyche:r9psyche, ~ .x==1) | r8rxdepres==1) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hchole_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5hchole:r9hchole, ~ .x==1) | if_any(r5rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  asthmae_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ asthmae_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5asthmae:r9asthmae, ~ .x==1) | if_any(r5rxasthma:r9rxasthma, ~ .x==1))~ 1, 
    TRUE ~ 0),
  osteoe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ osteoe_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5osteoe:r9osteoe, ~ .x==1) | if_any(c(r5rxosteo:r9rxosteo), ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp22$hibpe_dm, helsa_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 5, 106 individuals with no high blood pressure/hypertension across 9 waves and 88 individuals with high blood pressure/hypertension
table(helsa_all_sp22$diabe_dm, helsa_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 5, 161 individuals with no diabetes across 9 waves and 33 individuals with diabetes
table(helsa_all_sp22$cancre_dm, helsa_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 5, 172 individuals with no cancer across 9 waves and 22 individuals with cancer
table(helsa_all_sp22$lunge_dm, helsa_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 5, 179 individuals with no lung diseases across 9 waves and 15 individuals with lung diseases
table(helsa_all_sp22$psyche_dm, helsa_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 5, 170 individuals with no psychiatric conditions across 9 waves and 24 individuals with psychiatric conditions
table(helsa_all_sp22$hchole_dm, helsa_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 5, 103 individuals with no high cholesterol across 9 waves and 91 individuals with high cholesterol
table(helsa_all_sp22$asthmae_dm, helsa_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 5, 175 individuals with no asthma across 9 waves and 19 individuals with asthma
table(helsa_all_sp22$osteoe_dm, helsa_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 5, 176 individuals with no osteoporosis across 9 waves and 18 individuals with osteoporosis
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 6
helsa_all_sp23 <- helsa_all_sp22 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ hibpe_dm,
  inclusion_wave!="Wave 6"~ NA_real_,
  inclusion_wave=="Wave 6" & (if_any(r6hibpe:r9hibpe, ~ .x==1) | if_any(r6rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ diabe_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6diabe:r9diabe, ~ .x==1) | if_any(r6rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ cancre_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6cancre:r9cancre, ~ .x==1) | if_any(r6trcancr:r9trcancr, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ lunge_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6lunge:r9lunge, ~ .x==1) | if_any(r6rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ psyche_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6psyche:r9psyche, ~ .x==1) | r8rxdepres==1) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ hchole_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6hchole:r9hchole, ~ .x==1) | if_any(r6rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  asthmae_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ asthmae_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6asthmae:r9asthmae, ~ .x==1) | if_any(r6rxasthma:r9rxasthma, ~ .x==1))~ 1, 
    TRUE ~ 0),
  osteoe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5") ~ osteoe_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6osteoe:r9osteoe, ~ .x==1) | if_any(c(r6rxosteo:r9rxosteo), ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp23$hibpe_dm, helsa_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 6, 488 individuals with no high blood pressure/hypertension across 9 waves and 256 individuals with high blood pressure/hypertension
table(helsa_all_sp23$diabe_dm, helsa_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 6, 654 individuals with no diabetes across 9 waves and 90 individuals with diabetes
table(helsa_all_sp23$cancre_dm, helsa_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 6, 670 individuals with no cancer across 9 waves and 74 individuals with cancer
table(helsa_all_sp23$lunge_dm, helsa_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 6, 718 individuals with no lung diseases across 9 waves and 26 individuals with lung diseases
table(helsa_all_sp23$psyche_dm, helsa_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 6, 664 individuals with no psychiatric conditions across 9 waves and 80 individuals with psychiatric conditions
table(helsa_all_sp23$hchole_dm, helsa_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 6, 503 individuals with no high cholesterol across 9 waves and 241 individuals with high cholesterol
table(helsa_all_sp23$asthmae_dm, helsa_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 6, 642 individuals with no asthma across 9 waves and 102 individuals with asthma
table(helsa_all_sp23$osteoe_dm, helsa_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 6, 715 individuals with no osteoporosis across 9 waves and 29 individuals with osteoporosis
######################################################
#Inclusion in Wave 7
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 7
helsa_all_sp24 <- helsa_all_sp23 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ hibpe_dm,
  inclusion_wave!="Wave 7"~ NA_real_,
  inclusion_wave=="Wave 7" & (if_any(r7hibpe:r9hibpe, ~ .x==1) | if_any(r7rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ diabe_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7diabe:r9diabe, ~ .x==1) | if_any(r7rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ cancre_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7cancre:r9cancre, ~ .x==1) | if_any(r7trcancr:r9trcancr, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ lunge_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7lunge:r9lunge, ~ .x==1) | if_any(r7rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ psyche_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7psyche:r9psyche, ~ .x==1) | r8rxdepres==1) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ hchole_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7hchole:r9hchole, ~ .x==1) | if_any(r7rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  asthmae_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ asthmae_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7asthmae:r9asthmae, ~ .x==1) | if_any(r7rxasthma:r9rxasthma, ~ .x==1))~ 1, 
    TRUE ~ 0),
  osteoe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6") ~ osteoe_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7osteoe:r9osteoe, ~ .x==1) | if_any(c(r7rxosteo:r9rxosteo), ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp24$hibpe_dm, helsa_all_sp24$inclusion_wave, exclude=NULL) #For those included from wave 7, 251 individuals with no high blood pressure/hypertension across 9 waves and 69 individuals with high blood pressure/hypertension
table(helsa_all_sp24$diabe_dm, helsa_all_sp24$inclusion_wave, exclude=NULL) #For those included from wave 7, 299 individuals with no diabetes across 9 waves and 21 individuals with diabetes
table(helsa_all_sp24$cancre_dm, helsa_all_sp24$inclusion_wave, exclude=NULL) #For those included from wave 7, 299 individuals with no cancer across 9 waves and 21 individuals with cancer
table(helsa_all_sp24$lunge_dm, helsa_all_sp24$inclusion_wave, exclude=NULL) #For those included from wave 7, 307 individuals with no lung diseases across 9 waves and 13 individuals with lung diseases
table(helsa_all_sp24$psyche_dm, helsa_all_sp24$inclusion_wave, exclude=NULL) #For those included from wave 7, 273 individuals with no psychiatric conditions across 9 waves and 47 individuals with psychiatric conditions
table(helsa_all_sp24$hchole_dm, helsa_all_sp24$inclusion_wave, exclude=NULL) #For those included from wave 7, 246 individuals with no high cholesterol across 9 waves and 74 individuals with high cholesterol
table(helsa_all_sp24$asthmae_dm, helsa_all_sp24$inclusion_wave, exclude=NULL) #For those included from wave 7, 272 individuals with no asthma across 9 waves and 48 individuals with asthma
table(helsa_all_sp24$osteoe_dm, helsa_all_sp24$inclusion_wave, exclude=NULL) #For those included from wave 7, 307 individuals with no osteoporosis across 9 waves and 13 individuals with osteoporosis
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 8
helsa_all_sp25 <- helsa_all_sp24 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hibpe_dm,
  inclusion_wave!="Wave 8"~ NA_real_,
  inclusion_wave=="Wave 8" & (if_any(r8hibpe:r9hibpe, ~ .x==1) | if_any(r8rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ diabe_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8diabe:r9diabe, ~ .x==1) | if_any(r8rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ cancre_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8cancre:r9cancre, ~ .x==1) | if_any(r8trcancr:r9trcancr, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ lunge_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8lunge:r9lunge, ~ .x==1) | if_any(r8rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ psyche_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8psyche:r9psyche, ~ .x==1) | r8rxdepres==1) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hchole_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8hchole:r9hchole, ~ .x==1) | if_any(r8rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  asthmae_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ asthmae_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8asthmae:r9asthmae, ~ .x==1) | if_any(r8rxasthma:r9rxasthma, ~ .x==1))~ 1, 
    TRUE ~ 0),
  osteoe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ osteoe_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8osteoe:r9osteoe, ~ .x==1) | if_any(c(r8rxosteo:r9rxosteo), ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(helsa_all_sp25$hibpe_dm, helsa_all_sp25$inclusion_wave, exclude=NULL) #For those included from wave 8, 28 individuals with no high blood pressure/hypertension across 9 waves and 6 individuals with high blood pressure/hypertension
table(helsa_all_sp25$diabe_dm, helsa_all_sp25$inclusion_wave, exclude=NULL) #For those included from wave 8, 33 individuals with no diabetes across 9 waves and 1 individuals with diabetes
table(helsa_all_sp25$cancre_dm, helsa_all_sp25$inclusion_wave, exclude=NULL) #For those included from wave 8, 31 individuals with no cancer across 9 waves and 3 individuals with cancer
table(helsa_all_sp25$lunge_dm, helsa_all_sp25$inclusion_wave, exclude=NULL) #For those included from wave 8, 32 individuals with no lung diseases across 9 waves and 2 individuals with lung diseases
table(helsa_all_sp25$psyche_dm, helsa_all_sp25$inclusion_wave, exclude=NULL) #For those included from wave 8, 29 individuals with no psychiatric conditions across 9 waves and 5 individuals with psychiatric conditions
table(helsa_all_sp25$hchole_dm, helsa_all_sp25$inclusion_wave, exclude=NULL) #For those included from wave 8, 23 individuals with no high cholesterol across 9 waves and 11 individuals with high cholesterol
table(helsa_all_sp25$asthmae_dm, helsa_all_sp25$inclusion_wave, exclude=NULL) #For those included from wave 8, 29 individuals with no asthma across 9 waves and 5 individuals with asthma
table(helsa_all_sp25$osteoe_dm, helsa_all_sp25$inclusion_wave, exclude=NULL) #For those included from wave 8, 32 individuals with no osteoporosis across 9 waves and 2 individuals with osteoporosis
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions for the paired spouses
###################################################### 
helsa_all_sp25 <- helsa_all_sp25 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    shibpe_dm = ifelse(person_num == 1, hibpe_dm[2], hibpe_dm[1]),
    sdiabe_dm = ifelse(person_num == 1, diabe_dm[2], diabe_dm[1]),  
    scancre_dm = ifelse(person_num == 1, cancre_dm[2], cancre_dm[1]),
    slunge_dm = ifelse(person_num == 1, lunge_dm[2], lunge_dm[1]),
    spsyche_dm = ifelse(person_num == 1, psyche_dm[2], psyche_dm[1]),
    shchole_dm = ifelse(person_num == 1, hchole_dm[2], hchole_dm[1]),
    sasthmae_dm = ifelse(person_num == 1, asthmae_dm[2], asthmae_dm[1]),
    sosteoe_dm = ifelse(person_num == 1, osteoe_dm[2], osteoe_dm[1])
  ) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(helsa_all_sp25$shibpe_dm, helsa_all_sp25$inclusion_wave, exclude=NULL)
table(helsa_all_sp25$sdiabe_dm, helsa_all_sp25$inclusion_wave, exclude=NULL)
table(helsa_all_sp25$scancre_dm, helsa_all_sp25$inclusion_wave, exclude=NULL)
table(helsa_all_sp25$slunge_dm, helsa_all_sp25$inclusion_wave, exclude=NULL)
table(helsa_all_sp25$spsyche_dm, helsa_all_sp25$inclusion_wave, exclude=NULL)
table(helsa_all_sp25$shchole_dm, helsa_all_sp25$inclusion_wave, exclude=NULL)
table(helsa_all_sp25$sosteoe_dm, helsa_all_sp25$inclusion_wave, exclude=NULL)
table(helsa_all_sp25$sasthmae_dm, helsa_all_sp25$inclusion_wave, exclude=NULL)

##Compare the proportion of men with arthritis who had wives affected by other chronic conditions to those without
#High blood pressure/hypertension
male <- helsa_all_sp25 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hibpe = male$shibpe_dm
)
prop.table(freq, margin=1) #47% versus 43%
chisq.test(male$arthritis, male$shibpe_dm) #Significant

#High blood sugar/diabetes
male <- helsa_all_sp25 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_diabe = male$sdiabe_dm
)
prop.table(freq, margin=1) #13% versus 9%
chisq.test(male$arthritis, male$sdiabe_dm) #Significant

#Cancer
male <- helsa_all_sp25 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_cancre = male$scancre_dm
)
prop.table(freq, margin=1) #15% versus 14%
chisq.test(male$arthritis, male$scancre_dm) #Non-significant

#Lung diseases
male <- helsa_all_sp25 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_lunge = male$slunge_dm
)
prop.table(freq, margin=1) #9% versus 5%
chisq.test(male$arthritis, male$slunge_dm) #Significant

#Psychiatric conditions
male <- helsa_all_sp25 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_psyche= male$spsyche_dm
)
prop.table(freq, margin=1) #16% versus 12%
chisq.test(male$arthritis, male$spsyche_dm) #Significant

#High cholesterol
male <- helsa_all_sp25 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hchole= male$shchole_dm
)
prop.table(freq, margin=1) #44% versus 37%
chisq.test(male$arthritis, male$shchole_dm) #Significant

#Asthmae
male <- helsa_all_sp25 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_asthmae= male$sasthmae_dm
)
prop.table(freq, margin=1) #17% versus 15%
chisq.test(male$arthritis, male$sasthmae_dm) #Non-significant

#Osteoporosis
male <- helsa_all_sp25 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_osteoe= male$sosteoe_dm
)
prop.table(freq, margin=1) #15% versus 13%
chisq.test(male$arthritis, male$sosteoe_dm) #Non-significant

##Compare the proportion of women with arthritis who had husbands affected by other chronic conditions to those without
#High blood pressure/hypertension
female <- helsa_all_sp25 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hibpe = female$shibpe_dm
)
prop.table(freq2, margin=1) #54% versus 46%
chisq.test(female$arthritis, female$shibpe_dm) #Significant

#High blood sugar/diabetes
female <- helsa_all_sp25 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_diabe = female$sdiabe_dm
)
prop.table(freq2, margin=1) #19% versus 14%
chisq.test(female$arthritis, female$sdiabe_dm) #Significant

#Cancer
female <- helsa_all_sp25 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_cancre = female$scancre_dm
)
prop.table(freq2, margin=1) #17% versus 12%
chisq.test(female$arthritis, female$scancre_dm) #Significant

#Lung diseases
female <- helsa_all_sp25 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_lunge = female$slunge_dm
)
prop.table(freq2, margin=1) #12% versus 7%
chisq.test(female$arthritis, female$slunge_dm) #Significant

#Psychiatric conditions
female <- helsa_all_sp25 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_psyche= female$spsyche_dm
)
prop.table(freq2, margin=1) #9% versus 8%
chisq.test(female$arthritis, female$spsyche_dm) #Non-significant

#High cholesterol
female <- helsa_all_sp25 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hchole= female$shchole_dm
)
prop.table(freq2, margin=1) #47% versus 41%
chisq.test(female$arthritis, female$shchole_dm) #Significant

#Asthmae
female <- helsa_all_sp25 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_asthmae= female$sasthmae_dm
)
prop.table(freq2, margin=1) #15% versus 12%
chisq.test(female$arthritis, female$sasthmae_dm) #Significant

#osteoporosis
female <- helsa_all_sp25 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_osteoe= female$sosteoe_dm
)
prop.table(freq2, margin=1) #5% versus 3%
chisq.test(female$arthritis, female$sosteoe_dm) #Significant
######################################################
##Define covariates at inclusion for respondents
#Many misisng values for length of current marriage
#Use birth place to replace living area
#drinking frequency available from wave 2, those included in wave 1 used data from wave 2
#weekly contact with children is at respondent-level
#BMI available only in waves 2, 4, 6, and 8, use wave 2 data for waves 1 and 3, wave 4 data for wave 5, wave 6 data for wave 7 
#r3shlt is not available as self-reported health in wave 3 was recorded using other scale, use wave 4 data for wave 3
######################################################
#Define respondent covariates at inclusion for the three inclusion_wave groups
helsa_all_sp26 <- helsa_all_sp25 %>% mutate(
  rage=case_when(
    inclusion_wave=="Wave 1" ~ r1agey,
    inclusion_wave=="Wave 2" ~ r2agey,
    inclusion_wave=="Wave 3" ~ r3agey,
    inclusion_wave=="Wave 4" ~ r4agey,
    inclusion_wave=="Wave 5" ~ r5agey,
    inclusion_wave=="Wave 6" ~ r6agey,
    inclusion_wave=="Wave 7" ~ r7agey,
    TRUE~ r8agey), 
  rmcurln=case_when(
    inclusion_wave=="Wave 1" ~ r1mcurln,
    inclusion_wave=="Wave 2" ~ r2mcurln,
    inclusion_wave=="Wave 3" ~ r3mcurln,
    inclusion_wave=="Wave 4" ~ r4mcurln,
    inclusion_wave=="Wave 5" ~ r5mcurln,
    inclusion_wave=="Wave 6" ~ r6mcurln,
    inclusion_wave=="Wave 7" ~ r7mcurln,
    inclusion_wave=="Wave 8" ~ r8mcurln,
    TRUE~ NA),
  rshlt=case_when(
    inclusion_wave=="Wave 1" ~ r1shlt,
    inclusion_wave=="Wave 2" ~ r2shlt,
    inclusion_wave=="Wave 3" ~ r4shlt,
    inclusion_wave=="Wave 4" ~ r4shlt,
    inclusion_wave=="Wave 5" ~ r5shlt,
    inclusion_wave=="Wave 6" ~ r6shlt,
    inclusion_wave=="Wave 7" ~ r7shlt,
    TRUE~ r8shlt),
  radlfive=case_when(
    inclusion_wave=="Wave 1" ~ r1adlfive,
    inclusion_wave=="Wave 2" ~ r2adlfive,
    inclusion_wave=="Wave 3" ~ r3adlfive,
    inclusion_wave=="Wave 4" ~ r4adlfive,
    inclusion_wave=="Wave 5" ~ r5adlfive,
    inclusion_wave=="Wave 6" ~ r6adlfive,
    inclusion_wave=="Wave 7" ~ r7adlfive,
    TRUE~ r8adlfive),
  riadlza=case_when(
    inclusion_wave=="Wave 1" ~ r1iadlza,
    inclusion_wave=="Wave 2" ~ r2iadlza,
    inclusion_wave=="Wave 3" ~ r3iadlza,
    inclusion_wave=="Wave 4" ~ r4iadlza,
    inclusion_wave=="Wave 5" ~ r5iadlza,
    inclusion_wave=="Wave 6" ~ r6iadlza,
    inclusion_wave=="Wave 7" ~ r7iadlza,
    TRUE~ r8iadlza),
  rmobilsev=case_when(
    inclusion_wave=="Wave 1" ~ r1mobilsev,
    inclusion_wave=="Wave 2" ~ r2mobilsev,
    inclusion_wave=="Wave 3" ~ r3mobilsev,
    inclusion_wave=="Wave 4" ~ r4mobilsev,
    inclusion_wave=="Wave 5" ~ r5mobilsev,
    inclusion_wave=="Wave 6" ~ r6mobilsev,
    inclusion_wave=="Wave 7" ~ r7mobilsev,
    TRUE~ r8mobilsev),
  rpact=case_when(
    inclusion_wave=="Wave 1" & r1vgactx_e==2 ~ 4, #vigorous
    inclusion_wave=="Wave 1" & r1mdactx_e==2 ~ 3, #moderate
    inclusion_wave=="Wave 1" & r1ltactx_e==2 ~ 2, #light
    inclusion_wave=="Wave 1" & (r1ltactx_e %in% c(3,4) | r1mdactx_e %in% c(3,4) | r1vgactx_e %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 1" & (r1ltactx_e==5 | r1mdactx_e==5 | r1vgactx_e==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 2" & r2vgactx_e==2 ~ 4, #vigorous
    inclusion_wave=="Wave 2" & r2mdactx_e==2 ~ 3, #moderate
    inclusion_wave=="Wave 2" & r2ltactx_e==2 ~ 2, #light
    inclusion_wave=="Wave 2" & (r2ltactx_e %in% c(3,4) | r2mdactx_e %in% c(3,4) | r2vgactx_e %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 2" & (r2ltactx_e==5 | r2mdactx_e==5 | r2vgactx_e==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 3" & r3vgactx_e==2 ~ 4, #vigorous
    inclusion_wave=="Wave 3" & r3mdactx_e==2 ~ 3, #moderate
    inclusion_wave=="Wave 3" & r3ltactx_e==2 ~ 2, #light
    inclusion_wave=="Wave 3" & (r3ltactx_e %in% c(3,4) | r3mdactx_e %in% c(3,4) | r3vgactx_e %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 3" & (r3ltactx_e==5 | r3mdactx_e==5 | r3vgactx_e==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 4" & r4vgactx_e==2 ~ 4, #vigorous
    inclusion_wave=="Wave 4" & r4mdactx_e==2 ~ 3, #moderate
    inclusion_wave=="Wave 4" & r4ltactx_e==2 ~ 2, #light
    inclusion_wave=="Wave 4" & (r4ltactx_e %in% c(3,4) | r4mdactx_e %in% c(3,4) | r4vgactx_e %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 4" & (r4ltactx_e==5 | r4mdactx_e==5 | r4vgactx_e==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 5" & r5vgactx_e==2 ~ 4, #vigorous
    inclusion_wave=="Wave 5" & r5mdactx_e==2 ~ 3, #moderate
    inclusion_wave=="Wave 5" & r5ltactx_e==2 ~ 2, #light
    inclusion_wave=="Wave 5" & (r5ltactx_e %in% c(3,4) | r5mdactx_e %in% c(3,4) | r5vgactx_e %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 5" & (r5ltactx_e==5 | r5mdactx_e==5 | r5vgactx_e==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 6" & r6vgactx_e==2 ~ 4, #vigorous
    inclusion_wave=="Wave 6" & r6mdactx_e==2 ~ 3, #moderate
    inclusion_wave=="Wave 6" & r6ltactx_e==2 ~ 2, #light
    inclusion_wave=="Wave 6" & (r6ltactx_e %in% c(3,4) | r6mdactx_e %in% c(3,4) | r6vgactx_e %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 6" & (r6ltactx_e==5 | r6mdactx_e==5 | r6vgactx_e==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 7" & r7vgactx_e==2 ~ 4, #vigorous
    inclusion_wave=="Wave 7" & r7mdactx_e==2 ~ 3, #moderate
    inclusion_wave=="Wave 7" & r7ltactx_e==2 ~ 2, #light
    inclusion_wave=="Wave 7" & (r7ltactx_e %in% c(3,4) | r7mdactx_e %in% c(3,4) | r7vgactx_e %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 7" & (r7ltactx_e==5 | r7mdactx_e==5 | r7vgactx_e==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 8" & r8vgactx_e==2 ~ 4, #vigorous
    inclusion_wave=="Wave 8" & r8mdactx_e==2 ~ 3, #moderate
    inclusion_wave=="Wave 8" & r8ltactx_e==2 ~ 2, #light
    inclusion_wave=="Wave 8" & (r8ltactx_e %in% c(3,4) | r8mdactx_e %in% c(3,4) | r8vgactx_e %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 8" & (r7ltactx_e==5 | r7mdactx_e==5 | r7vgactx_e==5) ~ 0, #Inactive    
    TRUE ~ NA),
  rdrinke=case_when(
    inclusion_wave=="Wave 1" ~ r1drink,
    inclusion_wave=="Wave 2" ~ r2drink,
    inclusion_wave=="Wave 3" ~ r3drink,
    inclusion_wave=="Wave 4" ~ r4drink,
    inclusion_wave=="Wave 5" ~ r5drink,
    inclusion_wave=="Wave 6" ~ r6drink,
    inclusion_wave=="Wave 7" ~ r7drink,
    TRUE ~ r8drink),
  rdrinkr=case_when(
    inclusion_wave=="Wave 1" ~ r2drinkd_e,
    inclusion_wave=="Wave 2" ~ r2drinkd_e,
    inclusion_wave=="Wave 3" ~ r3drinkd_e,
    inclusion_wave=="Wave 4" ~ r4drinkd_e,
    inclusion_wave=="Wave 5" ~ r5drinkd_e,
    inclusion_wave=="Wave 6" ~ r6drinkd_e,
    inclusion_wave=="Wave 7" ~ r7drinkd_e,
    TRUE ~ r8drinkd_e),
  rsmokev=case_when(
    inclusion_wave=="Wave 1" ~ r1smokev,
    inclusion_wave=="Wave 2" ~ r2smokev,
    inclusion_wave=="Wave 3" ~ r3smokev,
    inclusion_wave=="Wave 4" ~ r4smokev,
    inclusion_wave=="Wave 5" ~ r5smokev,
    inclusion_wave=="Wave 6" ~ r6smokev,
    inclusion_wave=="Wave 7" ~ r7smokev,
    TRUE ~ r8smokev),
  ritearn=case_when(
    inclusion_wave=="Wave 1" ~ r1itearn,
    inclusion_wave=="Wave 2" ~ r2itearn,
    inclusion_wave=="Wave 3" ~ r3itearn,
    inclusion_wave=="Wave 4" ~ r4itearn,
    inclusion_wave=="Wave 5" ~ r5itearn,
    inclusion_wave=="Wave 6" ~ r6itearn,
    inclusion_wave=="Wave 7" ~ r7itearn,
    TRUE ~ r8itearn),
  hitsemp=case_when(
    inclusion_wave=="Wave 1" ~ h1isemp,
    inclusion_wave=="Wave 2" ~ h2isemp,
    inclusion_wave=="Wave 3" ~ h3isemp,
    inclusion_wave=="Wave 4" ~ h4isemp,
    inclusion_wave=="Wave 5" ~ h5isemp,
    inclusion_wave=="Wave 6" ~ h6isemp,
    inclusion_wave=="Wave 7" ~ h7isemp,
    TRUE ~ h8isemp),
  hicap=case_when(
    inclusion_wave=="Wave 1" ~ h1icap,
    inclusion_wave=="Wave 2" ~ h2icap,
    inclusion_wave=="Wave 3" ~ h3icap,
    inclusion_wave=="Wave 4" ~ h4icap,
    inclusion_wave=="Wave 5" ~ h5icap,
    inclusion_wave=="Wave 6" ~ h6icap,
    inclusion_wave=="Wave 7" ~ h7icap,
    TRUE ~ h8icap),
  ripripen=case_when(
    inclusion_wave=="Wave 1" ~ r1itpena,
    inclusion_wave=="Wave 2" ~ r2itpena,
    inclusion_wave=="Wave 3" ~ r3itpena,
    inclusion_wave=="Wave 4" ~ r4itpena,
    inclusion_wave=="Wave 5" ~ r5itpena,
    inclusion_wave=="Wave 6" ~ r6itpena,
    inclusion_wave=="Wave 7" ~ r7itpena,
    TRUE ~ r8itpena),
  ripubpen=case_when(
    inclusion_wave=="Wave 1" ~ r1ipubpen,
    inclusion_wave=="Wave 2" ~ r2ipubpen,
    inclusion_wave=="Wave 3" ~ r3ipubpen,
    inclusion_wave=="Wave 4" ~ r4ipubpen,
    inclusion_wave=="Wave 5" ~ r5ipubpen,
    inclusion_wave=="Wave 6" ~ r6ipubpen,
    inclusion_wave=="Wave 7" ~ r7ipubpen,
    TRUE ~ r8ipubpen),
  rigxfr=case_when(
    inclusion_wave=="Wave 1" ~ r1igxfr,
    inclusion_wave=="Wave 2" ~ r2igxfr,
    inclusion_wave=="Wave 3" ~ r3igxfr,
    inclusion_wave=="Wave 4" ~ r4igxfr,
    inclusion_wave=="Wave 5" ~ r5igxfr,
    inclusion_wave=="Wave 6" ~ r6igxfr,
    inclusion_wave=="Wave 7" ~ r7igxfr,
    TRUE ~ r8igxfr),
  hiothhh=case_when(
    inclusion_wave=="Wave 1" ~ h1iothr,
    inclusion_wave=="Wave 2" ~ h2iothr,
    inclusion_wave=="Wave 3" ~ h3iothr,
    inclusion_wave=="Wave 4" ~ h4iothr,
    inclusion_wave=="Wave 5" ~ h5iothr,
    inclusion_wave=="Wave 6" ~ h6iothr,
    inclusion_wave=="Wave 7" ~ h7iothr,
    TRUE ~ h8iothr),
  hitot=case_when(
    inclusion_wave=="Wave 1" ~ h1itot,
    inclusion_wave=="Wave 2" ~ h2itot,
    inclusion_wave=="Wave 3" ~ h3itot,
    inclusion_wave=="Wave 4" ~ h4itot,
    inclusion_wave=="Wave 5" ~ h5itot,
    inclusion_wave=="Wave 6" ~ h6itot,
    inclusion_wave=="Wave 7" ~ h7itot,
    TRUE ~ h8itot),
  rkcnt=case_when(
    inclusion_wave=="Wave 1" ~ r1kcnt,
    inclusion_wave=="Wave 2" ~ r2kcnt,
    inclusion_wave=="Wave 3" ~ r3kcnt,
    inclusion_wave=="Wave 4" ~ r4kcnt,
    inclusion_wave=="Wave 5" ~ r5kcnt,
    inclusion_wave=="Wave 6" ~ r6kcnt,
    inclusion_wave=="Wave 7" ~ r7kcnt,
    TRUE ~ r8kcnt),
  rsoc=case_when(
    inclusion_wave=="Wave 1" ~ r1socyr,
    inclusion_wave=="Wave 2" ~ r2socyr,
    inclusion_wave=="Wave 3" ~ r3socyr,
    inclusion_wave=="Wave 4" ~ r4socyr,
    inclusion_wave=="Wave 5" ~ r5socyr,
    inclusion_wave=="Wave 6" ~ r6socyr,
    inclusion_wave=="Wave 7" ~ r7socyr,
    TRUE ~ r8socyr),
  roccup=case_when(
    inclusion_wave=="Wave 1" ~ r1lbrf_e,
    inclusion_wave=="Wave 2" ~ r2lbrf_e,
    inclusion_wave=="Wave 3" ~ r3lbrf_e,
    inclusion_wave=="Wave 4" ~ r4lbrf_e,
    inclusion_wave=="Wave 5" ~ r5lbrf_e,
    inclusion_wave=="Wave 6" ~ r6lbrf_e,
    inclusion_wave=="Wave 7" ~ r7lbrf_e,
    TRUE ~ r8lbrf_e),
  rmbmin=case_when(
    inclusion_wave=="Wave 1" ~ r2mbmi,
    inclusion_wave=="Wave 2" ~ r2mbmi,
    inclusion_wave=="Wave 3" ~ r4mbmi,
    inclusion_wave=="Wave 4" ~ r4mbmi,
    inclusion_wave=="Wave 5" ~ r6mbmi,
    inclusion_wave=="Wave 6" ~ r6mbmi,
    inclusion_wave=="Wave 7" ~ r8mbmi,
    TRUE ~ r8mbmi),
  rmbmicat=case_when(
    inclusion_wave=="Wave 1" ~ r2mbmicat,
    inclusion_wave=="Wave 2" ~ r2mbmicat,
    inclusion_wave=="Wave 3" ~ r4mbmicat,
    inclusion_wave=="Wave 4" ~ r4mbmicat,
    inclusion_wave=="Wave 5" ~ r6mbmicat,
    inclusion_wave=="Wave 6" ~ r6mbmicat,
    inclusion_wave=="Wave 7" ~ r8mbmicat,
    TRUE ~ r8mbmicat),
  rclstress=case_when(
    (ralsevent_e > 0 & !is.na(ralsevent_e)) | (racsevent_e > 0 & !is.na(racsevent_e))  ~ 1,
    (ralsevent_e==0 & !is.na(ralsevent_e)) | (racsevent_e==0 & !is.na(racsevent_e))  ~ 0,
    TRUE ~ NA_real_),
  rspsupport=case_when(
    inclusion_wave=="Wave 1" ~ r1ssupport6,
    inclusion_wave=="Wave 2" ~ r2ssupport6,
    inclusion_wave=="Wave 3" ~ r3ssupport6,
    inclusion_wave=="Wave 4" ~ r4ssupport6,
    inclusion_wave=="Wave 5" ~ r5ssupport6,
    inclusion_wave=="Wave 6" ~ r6ssupport6,
    inclusion_wave=="Wave 7" ~ r7ssupport6,
    TRUE ~ r8ssupport6))

#Check for inconsistencies in household variables
inconsistent_households <- helsa_all_sp26 %>%
  group_by(householdID) %>%
  summarise(
    across(c(hitsemp, hicap, hiothhh, hitot), 
           ~ length(unique(.)) > 1,  # Returns TRUE if more than one unique value
           .names = "{.col}_inconsistent"
    )
  ) %>%
  filter(if_any(ends_with("_inconsistent"), ~ .))  # Keep only households with any inconsistency
#30 spousal pairs with inconsistent hitot variable 
#Select those with inconsistent household hitot variable
qc <- helsa_all_sp26 %>% filter(householdID %in% inconsistent_households$householdID) %>% dplyr::select("householdID", "hitsemp", "hicap", "hiothhh", "hitot")

#Add height and weight variables
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Waves/UKDA-5050-stata/stata/stata13_se')
helsa <- read_dta("h_elsa_g3.dta") %>% select(idauniq, r2mheight, r4mheight,r6mheight,r8mheight, r2mweight, r4mweight,r6mweight,r8mweight)
helsa_all_sp28 <- helsa_all_sp27 %>% left_join(.,helsa,by="idauniq")

#Define height and weight
helsa_all_sp28 <- helsa_all_sp28 %>% mutate(
  rmheight=case_when(
    inclusion_wave=="Wave 1" ~ r2mheight,
    inclusion_wave=="Wave 2" ~ r2mheight,
    inclusion_wave=="Wave 3" ~ r4mheight,
    inclusion_wave=="Wave 4" ~ r4mheight,
    inclusion_wave=="Wave 5" ~ r6mheight,
    inclusion_wave=="Wave 6" ~ r6mheight,
    inclusion_wave=="Wave 7" ~ r8mheight,
    TRUE~ r8mheight),
  rmweight=case_when(
    inclusion_wave=="Wave 1" ~ r2mweight,
    inclusion_wave=="Wave 2" ~ r2mweight,
    inclusion_wave=="Wave 3" ~ r4mweight,
    inclusion_wave=="Wave 4" ~ r4mweight,
    inclusion_wave=="Wave 5" ~ r6mweight,
    inclusion_wave=="Wave 6" ~ r6mweight,
    inclusion_wave=="Wave 7" ~ r8mweight,
    TRUE~ r8mweight))

#Create variable of any chronic conditions
helsa_all_sp28 <- helsa_all_sp28 %>% mutate(
  rchrondis = case_when(
    # Check if any variable = 1 (no NA in this row for the 1s check)
    inclusion_wave=="Wave 1" & if_any(c(r1hibpe, r1diabe, r1cancre, r1lunge, r1hearte, r1stroke, r1psyche, r2hchole, r1parkine, r1osteoe, r1catracte, r1asthmae), ~ .x == 1) ~ 1,
    # Check if all variables = 0 (no NA in this row for the 0s check)
    inclusion_wave=="Wave 1" & if_all(c(r1hibpe, r1diabe, r1cancre, r1lunge, r1hearte, r1stroke, r1psyche, r2hchole, r1parkine, r1osteoe, r1catracte, r1asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 2" & if_any(c(r2hibpe, r2diabe, r2cancre, r2lunge, r2hearte, r2stroke, r2psyche, r2hchole, r2parkine, r2osteoe, r2catracte, r2asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 2" & if_all(c(r2hibpe, r2diabe, r2cancre, r2lunge, r2hearte, r2stroke, r2psyche, r2hchole, r2parkine, r2osteoe, r2catracte, r2asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 3" & if_any(c(r3hibpe, r3diabe, r3cancre, r3lunge, r3hearte, r3stroke, r3psyche, r3hchole, r3parkine, r3osteoe, r3catracte, r3asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 3" & if_all(c(r3hibpe, r3diabe, r3cancre, r3lunge, r3hearte, r3stroke, r3psyche, r3hchole, r3parkine, r3osteoe, r3catracte, r3asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 4" & if_any(c(r4hibpe, r4diabe, r4cancre, r4lunge, r4hearte, r4stroke, r4psyche, r4hchole, r4parkine, r4osteoe, r4catracte, r4asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 4" & if_all(c(r4hibpe, r4diabe, r4cancre, r4lunge, r4hearte, r4stroke, r4psyche, r4hchole, r4parkine, r4osteoe, r4catracte, r4asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 5" & if_any(c(r5hibpe, r5diabe, r5cancre, r5lunge, r5hearte, r5stroke, r5psyche, r5hchole, r5parkine, r5osteoe, r5catracte, r5asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 5" & if_all(c(r5hibpe, r5diabe, r5cancre, r5lunge, r5hearte, r5stroke, r5psyche, r5hchole, r5parkine, r5osteoe, r5catracte, r5asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 6" & if_any(c(r6hibpe, r6diabe, r6cancre, r6lunge, r6hearte, r6stroke, r6psyche, r6hchole, r6parkine, r6osteoe, r6catracte, r6asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 6" & if_all(c(r6hibpe, r6diabe, r6cancre, r6lunge, r6hearte, r6stroke, r6psyche, r6hchole, r6parkine, r6osteoe, r6catracte, r6asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 7" & if_any(c(r7hibpe, r7diabe, r7cancre, r7lunge, r7hearte, r7stroke, r7psyche, r7hchole, r7parkine, r7osteoe, r7catracte, r7asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 7" & if_all(c(r7hibpe, r7diabe, r7cancre, r7lunge, r7hearte, r7stroke, r7psyche, r7hchole, r7parkine, r7osteoe, r7catracte, r7asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 8" & if_any(c(r8hibpe, r8diabe, r8cancre, r8lunge, r8hearte, r8stroke, r8psyche, r8hchole, r8parkine, r8osteoe, r8catracte, r8asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 8" & if_all(c(r8hibpe, r8diabe, r8cancre, r8lunge, r8hearte, r8stroke, r8psyche, r8hchole, r8parkine, r8osteoe, r8catracte, r8asthmae),  ~ .x == 0) ~ 0,
    # Otherwise NA
    TRUE ~ NA_real_))

#Check frequency of archrondis
table(helsa_all_sp28$rchrondis, exclude=NULL) #171 NAs
qc <- helsa_all_sp28 %>% filter(is.na(rchrondis)) %>% select(rchrondis, r1hibpe, r1diabe, r1cancre, r1lunge, r1hearte, r1stroke, r1psyche, r2hchole, r1parkine, r1osteoe, r1catracte, r1asthmae,r2hibpe, r2diabe, r2cancre, r2lunge, r2hearte, r2stroke, r2psyche, r2hchole, r2parkine, r2osteoe, r2catracte, r2asthmae, r3hibpe, r3diabe, r3cancre, r3lunge, r3hearte, r3stroke, r3psyche, r3hchole, r3parkine, r3osteoe, r3catracte, r3asthmae, r4hibpe, r4diabe, r4cancre, r4lunge, r4hearte, r4stroke, r4psyche, r4hchole, r4parkine, r4osteoe, r4catracte, r4asthmae, r5hibpe, r5diabe, r5cancre, r5lunge, r5hearte, r5stroke, r5psyche, r5hchole, r5parkine, r5osteoe, r5catracte, r5asthmae, r6hibpe, r6diabe, r6cancre, r6lunge, r6hearte, r6stroke, r6psyche, r6hchole, r6parkine, r6osteoe, r6catracte, r6asthmae, r7hibpe, r7diabe, r7cancre, r7lunge, r7hearte, r7stroke, r7psyche, r7hchole, r7parkine, r7osteoe, r7catracte, r7asthmae, r8hibpe, r8diabe, r8cancre, r8lunge, r8hearte, r8stroke, r8psyche, r8hchole, r8parkine, r8osteoe, r8catracte, r8asthmae)
######################################################
##Define covariates at inclusion for the paired spouses
######################################################
#Define spouses' covariates at inclusion for the three inclusion_wave groups
helsa_all_sp26 <- helsa_all_sp26 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sbyear = ifelse(person_num == 1, rabyear[2], rabyear[1]),
    sage = ifelse(person_num == 1, rage[2], rage[1]),
    sgender = ifelse(person_num == 1, ragender[2], ragender[1]),  
    seducl = ifelse(person_num == 1, raeducl[2], raeducl[1]),
    smcurln = ifelse(person_num == 1, rmcurln[2], rmcurln[1]),
    srace = ifelse(person_num == 1,  raracem[2],  raracem[1]),
    sbplace= ifelse(person_num == 1,  rabplace[2],  rabplace[1]),
    srelig= ifelse(person_num == 1,  rarelig_e[2],  rarelig_e[1]),
    sshlt = ifelse(person_num == 1, rshlt[2], rshlt[1]),
    sadlfive = ifelse(person_num == 1, radlfive[2], radlfive[1]),
    siadlza = ifelse(person_num == 1, riadlza[2], riadlza[1]),
    smobilsev = ifelse(person_num == 1, rmobilsev[2], rmobilsev[1]),
    spact = ifelse(person_num == 1, rpact[2], rpact[1]),
    sdrinke = ifelse(person_num == 1, rdrinke[2], rdrinke[1]),
    sdrinkr = ifelse(person_num == 1, rdrinkr[2], rdrinkr[1]),
    ssmokev = ifelse(person_num == 1, rsmokev[2], rsmokev[1]),
    sitearn = ifelse(person_num == 1, ritearn[2], ritearn[1]),
    sipripen = ifelse(person_num == 1, ripripen[2], ripripen[1]),
    sipubpen = ifelse(person_num == 1, ripubpen[2], ripubpen[1]),
    sigxfr = ifelse(person_num == 1, rigxfr[2], rigxfr[1]),
    skcnt = ifelse(person_num == 1, rkcnt[2], rkcnt[1]),
    smomeducl = ifelse(person_num == 1, ramomeduage[2], ramomeduage[1]),
    sdadeducl = ifelse(person_num == 1, radadeduage[2], radadeduage[1]),
    soccup = ifelse(person_num == 1, roccup[2], roccup[1]),
    smbmin = ifelse(person_num == 1, rmbmin[2], rmbmin[1]),
    smbmicat = ifelse(person_num == 1, rmbmicat[2], rmbmicat[1]),
    sclstress = ifelse(person_num == 1, rclstress[2], rclstress[1]),
    sspsupport = ifelse(person_num == 1, rspsupport[2], rspsupport[1])) %>%
  ungroup()

#Define household income=ritearn+sitearn+hicap+ripen+sipen+rigxfr+sigxfr+hiothhh
helsa_all_sp26 <- helsa_all_sp26 %>% mutate(hincome=case_when(
  !is.na(ritearn) & !is.na(sitearn) & !is.na(hicap) & !is.na(ripripen) & !is.na(sipripen) & !is.na(ripubpen) & !is.na(sipubpen) & !is.na(rigxfr) & !is.na(sigxfr) & !is.na(hiothhh) ~ ritearn+sitearn+hicap+ripripen+sipripen+ripubpen+sipubpen+rigxfr+sigxfr+hiothhh,
  TRUE ~ NA))

##Check potential misdefined variables
#Age at interview
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & rage != r1agey) | (inclusion_wave=="Wave 2" & rage != r2agey )| (inclusion_wave=="Wave 3" & rage != r3agey)) #None
#Length of current marriage
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 4" & rmcurln != r4mcurln) | (inclusion_wave=="Wave 5" & rmcurln != r5mcurln)) #None
#Self-reported health
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 6" & rshlt != r6shlt) | (inclusion_wave=="Wave 7" & rshlt != r7shlt)| (inclusion_wave=="Wave 8" & rshlt != r8shlt)) #None
#ADL summary
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & radlfive != r1adlfive) | (inclusion_wave=="Wave 2" & radlfive != r2adlfive) | (inclusion_wave=="Wave 3" & radlfive != r3adlfive)) #None
#Mobility summary
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 3" & rmobilsev != r3mobilsev) | (inclusion_wave=="Wave 5" & rmobilsev != r5mobilsev) | (inclusion_wave=="Wave 6" & rmobilsev != r6mobilsev)) #None
#Physical activity 
qc <- helsa_all_sp26 %>% select(inclusion_wave, r1ltactx_e:r9ltactx_e, r1mdactx_e:r9mdactx_e, r1vgactx_e:r9vgactx_e, rpact)
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 1" & rpact==3) #All "r1vgactx_c" should equal to 2
table(qc$r1vgactx_e,exclude=NULL) #Passed QC
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 2" & rpact==3) #All "r1vgactx_c" should equal to 2
table(qc$r2vgactx_e,exclude=NULL) #Passed QC
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 3" & rpact==3) #All "r1vgactx_c" should equal to 2
table(qc$r3vgactx_e,exclude=NULL) #Passed QC
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 1" & is.na(rpact)) %>% select(inclusion_wave, r1ltactx_e:r9ltactx_e, r1mdactx_e:r9mdactx_e, r1vgactx_e:r9vgactx_e, rpact)#All wave 1 physical activity variables should be NA. #Passed QC
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 2" & is.na(rpact)) %>% select(inclusion_wave, r1ltactx_e:r9ltactx_e, r1mdactx_e:r9mdactx_e, r1vgactx_e:r9vgactx_e, rpact)#All wave 2 physical activity variables should be NA. #Passed QC
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 3" & is.na(rpact)) %>% select(inclusion_wave, r1ltactx_e:r9ltactx_e, r1mdactx_e:r9mdactx_e, r1vgactx_e:r9vgactx_e, rpact)#All wave 3 physical activity variables should be NA. #Passed QC
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 1" & rpact==0) %>% select(inclusion_wave, r1ltactx_e:r9ltactx_e, r1mdactx_e:r9mdactx_e, r1vgactx_e:r9vgactx_e, rpact)#All wave 1 physical activity variables should be 3/4/5. #Passed QC
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 2" & rpact==0) %>% select(inclusion_wave, r1ltactx_e:r9ltactx_e, r1mdactx_e:r9mdactx_e, r1vgactx_e:r9vgactx_e, rpact)#All wave 2 physical activity variables should be 3/4/5. #Passed QC
qc <- helsa_all_sp26 %>% filter(inclusion_wave=="Wave 3" & rpact==0) %>% select(inclusion_wave, r1ltactx_e:r9ltactx_e, r1mdactx_e:r9mdactx_e, r1vgactx_e:r9vgactx_e, rpact)#All wave 3 physical activity variables should be 3/4/5. #Passed QC
#Whether the respondent has had an alcoholic drink during the last 12 months.
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & rdrinke != r1drink) | (inclusion_wave=="Wave 2" & rdrinke != r2drink)| (inclusion_wave=="Wave 3" & rdrinke != r3drink)) #None
#The highest frequency of drinking behavior during the last year that the respondent reports for any one of the three types of alcohol.
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 8" & rdrinkr != r8drinkd_e) | (inclusion_wave=="Wave 7" & rdrinkr != r7drinkd_e)| (inclusion_wave=="Wave 5" & rdrinkr != r5drinkd_e)) #None
#Ever smoking
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & rsmokev != r1smokev) | (inclusion_wave=="Wave 2" & rsmokev != r2smokev)| (inclusion_wave=="Wave 3" & rsmokev != r3smokev)) #None
#Household income in the past year (before or after tax)
qc <- helsa_all_sp26 %>% filter(is.na(hincome)) %>% select("idauniq","householdID","inclusion_wave", "ritearn", "sitearn", "hicap", "ripripen", "sipripen","ripubpen", "sipubpen","rigxfr", "sigxfr", "hiothhh", "hincome") #68 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC
#Weekly contact with children in person/by phone/email
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & rkcnt != r1kcnt) | (inclusion_wave=="Wave 2" & rkcnt != r2kcnt)| (inclusion_wave=="Wave 3" & rkcnt != r3kcnt)) #None
#Occupation
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & roccup != r1lbrf_e) | (inclusion_wave=="Wave 2" & roccup != r2lbrf_e) | (inclusion_wave=="Wave 3" & roccup != r3lbrf_e)) #None
#Measured BMI
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & rmbmin != r2mbmi) | (inclusion_wave=="Wave 2" & rmbmin != r2mbmi)| (inclusion_wave=="Wave 3" & rmbmin != r4mbmi)) #None
#Childhood/lifetime stressful events including
##Childhood stressful events
#whether the respondent has been in a major fire, flood, earthquake or other natural
#disaster by the age of 16 (RANADISCH), whether the respondent has fired a weapon in combat or been fired
#upon in combat by the age of 16 (RACOMBATCH), whether the respondent was a victim of a serious physical
#attack or assault by the age of 16 (RAATTACKCH), whether the respondent had a life-threatening illness or
#accident by the age of 16 (RALIFETHCH), whether the respondent missed a month or more of school before
#age 16 because of a health condition (RAMISCHLTH), whether the respondent experienced severe financial
#hardship before age 16 (RASFNHCH), whether the respondent experienced a difficult living arrangement
#before age 16 (RALIVDIFFCH), whether the respondent’s parents separated or divorced before age 16
#(RAPADIVCH), whether the respondent was physically abused by either of their parents before age 16
#(RAPABUSED), whether either of the respondent's parents drank excessively, took drugs or had mental
#health problems before age 16 (RAPADRUG), and whether the respondent was ever separated from their mother
#for more than 6 months before age 16 (RASEPMOM)
##Lifetime stressful events
#whether the respondent has ever been in a major fire, flood, earthquake or other natural
#disaster (RANADISE), whether the respondent has ever fired a weapon in combat or been fired upon in
#combat (RACOMBATE), whether the respondent was a victim of a serious physical attack or assault
#(RAATTACKE), whether the respondent has ever had a life-threatening illness or accident (RALIFETHE),
#whether the respondent ever experienced severe financial hardship (RASFNHE), whether the respondent was
#physically abused by either of their parents before age 16 (RAPABUSED), whether either of the
#respondent's parents drank excessively, took drugs or had mental health problems before age 16
#(RAPADRUG), whether the respondent missed a month or more of school before age 16 because of a health
#condition (RAMISCHLTH), whether the respondent experienced a difficult living arrangement before age 16
#(RALIVDIFFCH), whether the respondent’s parents separated or divorced before age 16 (RAPADIVCH), and
#whether the respondent was ever separated from their mother for more than 6 months before age 16
#(RASEPMOM).
qc <- helsa_all_sp26 %>% filter(is.na(rclstress)) %>% select("idauniq","householdID","inclusion_wave", "ranadisch", "racombatch", "raattackch", "ralifethch", "ramischlth", "rasfnhch","ralivdiffch", "rapadivch","rapabused","rasepmom","racombate","raattacke","ralifethe","rasfnhe","rclstress") #4992 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC
#Whether the respondent is a member of an organization, club or society and attends at least one committee meeting in a year.
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & rsoc != r1socyr) | (inclusion_wave=="Wave 2" & rsoc != r2socyr) | (inclusion_wave=="Wave 3" & rsoc != r3socyr)) #None
#Indicates the mean of the six different spouse questions (RwSUSTDFE, RwSRELY, RwSOPENUP, RwSCRITZE, RwSLETDOW and RwSGETNEV) and can be used as a summary score. 
qc <- helsa_all_sp26 %>% filter((inclusion_wave=="Wave 1" & rspsupport != r1ssupport6) | (inclusion_wave=="Wave 2" & rspsupport != r2ssupport6) | (inclusion_wave=="Wave 3" & rspsupport != r3ssupport6)) #None

##Exclude individuals with death age younger than interview age
#Load harmonized end of life dataset
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/ELSA/Waves/UKDA-5050-stata/stata/stata13_se')
helsa_eol <- read_dta("h_elsa_eol_a2.dta")
#Check among the 302 individuals died during follow-up, how many of them had arthritis 
death <- helsa_eol %>% select(idauniq, radage, racod_e, ragcod)  
sp <- helsa_all_sp26 %>% select(idauniq, householdID, arthritis, rage, ragender, inclusion_wave)
death_arthritis <- death %>% left_join(.,sp,by="idauniq")
table(death_arthritis$arthritis, exclude=NULL) #125 individuals with arthritis
#Select those with interview age older than death age
death_arthritis2 <- death_arthritis %>% filter(radage < rage) #99 individuals with death happened before interview, 44 had arthritis
table(death_arthritis2$arthritis, exclude=NULL) #125 individuals with arthritis, 10 spousal pairs and 79 indiviudals
table(as.data.frame(table(death_arthritis2$householdID, exclude=NULL))$Freq,exclude = NULL)
#Exclude those in death_arthritis2 and their spouses
helsa_all_sp27 <- helsa_all_sp26 %>% filter(!householdID %in% death_arthritis2$householdID)
table(as.data.frame(table(helsa_all_sp27$householdID, exclude=NULL))$Freq,exclude = NULL) #9818 individuals and 4909 spousal pairs

#Recode occupation classification rwsoc2000
helsa_all_sp27 <- helsa_all_sp27 %>% mutate(
  roccup2=case_when(
    inclusion_wave=="Wave 1" ~ r1soc2000,
    inclusion_wave=="Wave 2" ~ r2soc2000,
    inclusion_wave=="Wave 3" ~ r3soc2000,
    inclusion_wave=="Wave 4" ~ r4soc2000,
    inclusion_wave=="Wave 5" ~ r5soc2000,
    inclusion_wave=="Wave 6" ~ r6soc2000,
    inclusion_wave=="Wave 7" ~ r7soc2000,
    TRUE~ r8soc2000))

helsa_all_sp27 <- helsa_all_sp27 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    soccup2 = ifelse(person_num == 1, roccup2[2], roccup2[1])) %>% ungroup()

helsa_all_sp27 <- helsa_all_sp27 %>% mutate(
  roccup2_2=case_when(
    roccup2==1 | roccup2==2~ 1, #1.Legislator, senior official or manager
    roccup2==3 | roccup2==4 | roccup2==5 | roccup2==6 ~ 2, #2.Professional
    roccup2==7 | roccup2==8 | roccup2==9 |roccup2==10 | roccup2==11 ~ 3, #3.Technician or associate professional
    roccup2==12 | roccup2==13 ~ 4, #4.Clerk
    roccup2==18 | roccup2==19 | roccup2==20 |roccup2==21 ~ 5, #5.Service worker and shop and market sales worker
    roccup2==14 ~ 6, #6.Skilled agricultural or fishery worker
    roccup2==15 | roccup2==16 | roccup2==17 ~ 7, #7.Craft and related trades worker
    roccup2==22 | roccup2==23 ~ 8, #8.Plant and machine operator or assembler
    roccup2==24 | roccup2==25 ~ 9, #9.Elementary occupation
    roccup2==-23 | roccup2==-8 ~ 10, #10.Unemployed
    roccup2==-13 | roccup2==-18 ~ NA_real_)) #Missing

#Check frequency of roccup2_2
table(helsa_all_sp27$roccup2_2, exclude=NULL)

helsa_all_sp27 <- helsa_all_sp27 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    soccup2_2 = ifelse(person_num == 1, roccup2_2[2], roccup2_2[1])) %>% ungroup()

#Define weight and height variables for paired spouses
helsa_all_sp28 <- helsa_all_sp28 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    smheight = if_else(person_num == 1, rmheight[2], rmheight[1]),
    smweight = if_else(person_num == 1, rmweight[2], rmweight[1]),
    schrondis = if_else(person_num == 1, rchrondis[2], rchrondis[1])) %>%
  ungroup()

#Save dataset helsa_all_sp27
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
save(helsa_all_sp27, file = "helsa_all_sp27.rda")

#Save dataset helsa_all_sp28
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
save(helsa_all_sp28, file = "helsa_all_sp28.rda")
######################################################
#3. Multiple imputation for covariates with missing proportion  5% < x < 40% (helsa_all_sp28) 
###################################################### 
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
load("helsa_all_sp28.rda")

#Create new disease variables for imputation
helsa_all_sp28 <- helsa_all_sp28 %>% mutate(
  arthre2=case_when(
    inclusion_wave=="Wave 1" ~ r1arthre,
    inclusion_wave=="Wave 2" ~ r2arthre,
    inclusion_wave=="Wave 3" ~ r3arthre,
    inclusion_wave=="Wave 4" ~ r4arthre,
    inclusion_wave=="Wave 5" ~ r5arthre,
    inclusion_wave=="Wave 6" ~ r6arthre,
    inclusion_wave=="Wave 7" ~ r7arthre,
    TRUE ~ r8arthre),
  hibpe2=case_when(
    inclusion_wave=="Wave 1" ~ r1hibpe,
    inclusion_wave=="Wave 2" ~ r2hibpe,
    inclusion_wave=="Wave 3" ~ r3hibpe,
    inclusion_wave=="Wave 4" ~ r4hibpe,
    inclusion_wave=="Wave 5" ~ r5hibpe,
    inclusion_wave=="Wave 6" ~ r6hibpe,
    inclusion_wave=="Wave 7" ~ r7hibpe,
    TRUE ~ r8hibpe),
  diabe2=case_when(
    inclusion_wave=="Wave 1" ~ r1diabe,
    inclusion_wave=="Wave 2" ~ r2diabe,
    inclusion_wave=="Wave 3" ~ r3diabe,
    inclusion_wave=="Wave 4" ~ r4diabe,
    inclusion_wave=="Wave 5" ~ r5diabe,
    inclusion_wave=="Wave 6" ~ r6diabe,
    inclusion_wave=="Wave 7" ~ r7diabe,
    TRUE ~ r8diabe),
  cancre2=case_when(
    inclusion_wave=="Wave 1" ~ r1cancre,
    inclusion_wave=="Wave 2" ~ r2cancre,
    inclusion_wave=="Wave 3" ~ r3cancre,
    inclusion_wave=="Wave 4" ~ r4cancre,
    inclusion_wave=="Wave 5" ~ r5cancre,
    inclusion_wave=="Wave 6" ~ r6cancre,
    inclusion_wave=="Wave 7" ~ r7cancre,
    TRUE ~ r8cancre),
  lunge2=case_when(
    inclusion_wave=="Wave 1" ~ r1lunge,
    inclusion_wave=="Wave 2" ~ r2lunge,
    inclusion_wave=="Wave 3" ~ r3lunge,
    inclusion_wave=="Wave 4" ~ r4lunge,
    inclusion_wave=="Wave 5" ~ r5lunge,
    inclusion_wave=="Wave 6" ~ r6lunge,
    inclusion_wave=="Wave 7" ~ r7lunge,
    TRUE ~ r8lunge),
  hearte2=case_when(
    inclusion_wave=="Wave 1" ~ r1hearte,
    inclusion_wave=="Wave 2" ~ r2hearte,
    inclusion_wave=="Wave 3" ~ r3hearte,
    inclusion_wave=="Wave 4" ~ r4hearte,
    inclusion_wave=="Wave 5" ~ r5hearte,
    inclusion_wave=="Wave 6" ~ r6hearte,
    inclusion_wave=="Wave 7" ~ r7hearte,
    TRUE ~ r8hearte),
  stroke2=case_when(
    inclusion_wave=="Wave 1" ~ r1stroke,
    inclusion_wave=="Wave 2" ~ r2stroke,
    inclusion_wave=="Wave 3" ~ r3stroke,
    inclusion_wave=="Wave 4" ~ r4stroke,
    inclusion_wave=="Wave 5" ~ r5stroke,
    inclusion_wave=="Wave 6" ~ r6stroke,
    inclusion_wave=="Wave 7" ~ r7stroke,
    TRUE ~ r8stroke),
  psyche2=case_when(
    inclusion_wave=="Wave 1" ~ r1psyche,
    inclusion_wave=="Wave 2" ~ r2psyche,
    inclusion_wave=="Wave 3" ~ r3psyche,
    inclusion_wave=="Wave 4" ~ r4psyche,
    inclusion_wave=="Wave 5" ~ r5psyche,
    inclusion_wave=="Wave 6" ~ r6psyche,
    inclusion_wave=="Wave 7" ~ r7psyche,
    TRUE ~ r8psyche),
  hchole2=case_when(
    inclusion_wave=="Wave 1" ~ r2hchole,
    inclusion_wave=="Wave 2" ~ r2hchole,
    inclusion_wave=="Wave 3" ~ r3hchole,
    inclusion_wave=="Wave 4" ~ r4hchole,
    inclusion_wave=="Wave 5" ~ r5hchole,
    inclusion_wave=="Wave 6" ~ r6hchole,
    inclusion_wave=="Wave 7" ~ r7hchole,
    TRUE ~ r8hchole),
  asthmae2=case_when(
    inclusion_wave=="Wave 1" ~ r1asthmae,
    inclusion_wave=="Wave 2" ~ r2asthmae,
    inclusion_wave=="Wave 3" ~ r3asthmae,
    inclusion_wave=="Wave 4" ~ r4asthmae,
    inclusion_wave=="Wave 5" ~ r5asthmae,
    inclusion_wave=="Wave 6" ~ r6asthmae,
    inclusion_wave=="Wave 7" ~ r7asthmae,
    TRUE ~ r8asthmae),
  catracte2=case_when(
    inclusion_wave=="Wave 1" ~ r1catracte,
    inclusion_wave=="Wave 2" ~ r2catracte,
    inclusion_wave=="Wave 3" ~ r3catracte,
    inclusion_wave=="Wave 4" ~ r4catracte,
    inclusion_wave=="Wave 5" ~ r5catracte,
    inclusion_wave=="Wave 6" ~ r6catracte,
    inclusion_wave=="Wave 7" ~ r7catracte,
    TRUE ~ r8catracte),
  parkine2=case_when(
    inclusion_wave=="Wave 1" ~ r1parkine,
    inclusion_wave=="Wave 2" ~ r2parkine,
    inclusion_wave=="Wave 3" ~ r3parkine,
    inclusion_wave=="Wave 4" ~ r4parkine,
    inclusion_wave=="Wave 5" ~ r5parkine,
    inclusion_wave=="Wave 6" ~ r6parkine,
    inclusion_wave=="Wave 7" ~ r7parkine,
    TRUE ~ r8parkine),
  osteoe2=case_when(
    inclusion_wave=="Wave 1" ~ r1osteoe,
    inclusion_wave=="Wave 2" ~ r2osteoe,
    inclusion_wave=="Wave 3" ~ r3osteoe,
    inclusion_wave=="Wave 4" ~ r4osteoe,
    inclusion_wave=="Wave 5" ~ r5osteoe,
    inclusion_wave=="Wave 6" ~ r6osteoe,
    inclusion_wave=="Wave 7" ~ r7osteoe,
    TRUE ~ r8osteoe))

#Convert missing types into NA
helsa_all_sp28 <- helsa_all_sp28 %>% mutate(rabplace=case_when(
  rabplace==-13 | rabplace==-4 ~ NA,
  TRUE ~ rabplace),
  raeducl=case_when(
    raeducl==-4 | raeducl==-13| raeducl==-15 | raeducl==-18 ~ NA,
    TRUE ~ raeducl),
  rdrinke=case_when(
    rdrinke==-3 | rdrinke==-4 | rdrinke==-13 | rdrinke==-16 ~ NA,
    TRUE ~ rdrinke),
  rsmokev=case_when(
    rsmokev==-4 | rsmokev==-13 | rsmokev==-16 | rsmokev==-18 ~ NA,
    TRUE ~ rsmokev),
  rarelig_e=case_when(
    rarelig_e==-9 | rarelig_e==-13 | rarelig_e==-16 | rarelig_e==-18 | rarelig_e==-23 ~ NA,
    TRUE ~ rarelig_e),
  rmbmicat=case_when(
    rmbmicat==-9 | rmbmicat==-14 | rmbmicat==-16 | rmbmicat==-19 | rmbmicat==-24 ~ NA,
    TRUE ~ rmbmicat),
  roccup=case_when(
    roccup==-4 | roccup==-15 | roccup==-18 ~ NA,
    TRUE ~ roccup),
  rkcnt=case_when(
    rkcnt==-3 | rkcnt==-11 | rkcnt==-13 | rkcnt==-18 ~ NA,
    TRUE ~ rkcnt),
  ramomeduage=case_when(
    ramomeduage==-1 | ramomeduage==-4 | ramomeduage==-18 ~ NA,
    TRUE ~ ramomeduage),
  radadeduage=case_when(
    radadeduage==-1 | radadeduage==-4 | radadeduage==-18 ~ NA,
    TRUE ~ radadeduage),
  rshlt=case_when(
    rshlt==-4 | rshlt==-16 | rshlt==-13 | rshlt==-18 ~ NA,
    TRUE ~ rshlt),
  arthre2=case_when(
    arthre2==-4 | arthre2==-18 ~ NA,
    TRUE ~ arthre2),
  hibpe2=case_when(
    hibpe2==-4 | hibpe2==-13 | hibpe2==-18 ~ NA,
    TRUE ~ hibpe2),
  diabe2=case_when(
    diabe2==-4 | diabe2==-13 | diabe2==-18 ~ NA,
    TRUE ~ diabe2),
  cancre2=case_when(
    cancre2==-4 | cancre2==-18 ~ NA,
    TRUE ~ cancre2),
  lunge2=case_when(
    lunge2==-4 |  lunge2==-18 ~ NA,
    TRUE ~ lunge2),
  hearte2=case_when(
    hearte2==-4 | hearte2==-13 | hearte2==-18 ~ NA,
    TRUE ~ hearte2),
  stroke2=case_when(
    stroke2==-4 | stroke2==-13 | stroke2==-18 ~ NA,
    TRUE ~ stroke2),
  psyche2=case_when(
    psyche2==-4 | psyche2==-18 ~ NA,
    TRUE ~ psyche2),
  hchole2=case_when(
    hchole2==-4 | hchole2==-18 ~ NA,
    TRUE ~ hchole2),
  catracte2=case_when(
    catracte2==-4 | catracte2==-18 ~ NA,
    TRUE ~ catracte2),
  parkine2=case_when(
    parkine2==-4 | parkine2==-18 ~ NA,
    TRUE ~ parkine2),
  osteoe2=case_when(
    osteoe2==-4 | osteoe2==-18 ~ NA,
    TRUE ~ osteoe2),
  asthmae2=case_when(
    asthmae2==-4 | asthmae2==-18 ~ NA,
    TRUE ~ asthmae2))

#Select relevant variables
helsa_miss <- helsa_all_sp28 %>% dplyr::select(idauniq, rage, rabyear, ragender, rabplace, raracem, ramomeduage, radadeduage, rclstress, rarelig_e, rmcurln, raeducl, rpact, rdrinkr, rdrinke, rsmokev, roccup, roccup2_2, rmheight,rmweight,rmbmin, hincome, rkcnt, arthre2, hibpe2, diabe2, cancre2, lunge2, hearte2, stroke2, psyche2, hchole2, parkine2, catracte2, asthmae2, osteoe2, rshlt, radlfive, riadlza, rmobilsev, rspsupport, arthritis, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, stroke, psyche, psyche_dm, hchole, hchole_dm, asthmae, asthmae_dm, catracte, parkine, osteoe, osteoe_dm) %>% 
  mutate(across(c(rage, rabyear, rmcurln, rdrinkr, rmheight, rmweight, rmbmin, hincome), as.numeric)) %>%
  mutate(across(c(ragender, rabplace, raracem, ramomeduage, radadeduage, rclstress, rarelig_e, raeducl, rpact, rdrinke, rsmokev, roccup, roccup2_2, rkcnt, arthre2, hibpe2, diabe2, cancre2, lunge2, hearte2, stroke2, psyche2, hchole2, parkine2, catracte2, asthmae2, osteoe2, rshlt, radlfive, riadlza, rmobilsev, rspsupport, arthritis, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, stroke, psyche, psyche_dm, hchole, hchole_dm, asthmae, asthmae_dm, catracte, parkine, osteoe, osteoe_dm), as.factor))

#Define ordered variables
helsa_miss$raeducl <- ordered(helsa_miss$raeducl, levels = c("1", "2", "3"))
helsa_miss$ramomeduage <- ordered(helsa_miss$ramomeduage, levels = c("1", "2", "3","4","5","6","7"))
helsa_miss$radadeduage <- ordered(helsa_miss$radadeduage, levels = c("1", "2", "3","4","5","6","7"))
helsa_miss$rshlt <- ordered(helsa_miss$rshlt, levels = c("1", "2", "3","4","5"))
helsa_miss$radlfive <- ordered(helsa_miss$radlfive, levels = c("0", "1", "2","3","4", "5"))
helsa_miss$riadlza <- ordered(helsa_miss$riadlza, levels = c("0", "1", "2","3","4", "5"))
helsa_miss$rmobilsev <- ordered(helsa_miss$rmobilsev, levels = c("0", "1", "2","3","4", "5", "6","7"))

#Create the mids object containing the defacult setting
ini <- mice(helsa_miss, max=0, print=FALSE)

#Extract meth data and revise accordingly
meth <- ini$meth #modelling methods
##Modelling
#Numeric variables: Predictive mean matching 
#Factor with 2 levels: Logistic regression 
#Factor with > 2 levels: Multinomial logit model
#Ordinal variables: Ordered logit model
#Change several variables simultaneously
meth_var_to_change <- c("rmcurln","rclstress", "arthritis", "hibpe","hibpe_dm","diabe", "diabe_dm", "cancre", "cancre_dm", "lunge", "lunge_dm", "hearte", "stroke", "psyche", "psyche_dm", "hchole", "hchole_dm", "catracte", "parkine", "osteoe", "osteoe_dm", "asthmae", "asthmae_dm")
meth[meth_var_to_change] <- ""
#Compute BMI using rmheight and rmweight
meth["rmbmin"] <- "~I(pmin(pmax(rmweight/((rmheight)^2),12),60))"

#Extract prediction matrix
pred <- ini$pred #prediction matrix
quickpred <- quickpred(helsa_miss) #take absolute correlation with the target or with the response indicator of at least 0.1 
##Update prediction matrix: wave-specific prediction 
#Include rage and ragender in all prediction sets
quickpred[,c("rage","ragender")] <- 1
#Remove ID, rage, and ragender from imputation
quickpred[c("idauniq","rage","rabyear","ragender"),c("idauniq","rage","rabyear","ragender")] <- 0
#Remove newly defined disease variables from predictor sets and from prediction
quickpred[,c("idauniq","rabyear","rmcurln", "rclstress","arthritis", "hibpe","hibpe_dm","diabe", "diabe_dm", "cancre", "cancre_dm", "lunge", "lunge_dm", "hearte", "stroke", "psyche", "psyche_dm", "hchole", "hchole_dm", "catracte", "parkine", "osteoe", "osteoe_dm", "asthmae", "asthmae_dm")] <- 0
quickpred[c("idauniq","rabyear","rmcurln", "rclstress","arthritis", "hibpe","hibpe_dm","diabe", "diabe_dm", "cancre", "cancre_dm", "lunge", "lunge_dm", "hearte", "stroke", "psyche", "psyche_dm", "hchole", "hchole_dm", "catracte", "parkine", "osteoe", "osteoe_dm", "asthmae", "asthmae_dm"),] <- 0

#Save quickpred
quickpred_df <- as.data.frame(quickpred)
quickpred_df$Variable <- rownames(quickpred)
quickpred_df <- quickpred_df[, c("Variable", setdiff(names(quickpred_df), "Variable"))]
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/ELSA')
write.csv(quickpred_df, "elsa_imp_quickpred.csv", row.names = FALSE)

#Multiple imputation
helsa_imp <- mice(helsa_miss, m=5, meth=meth, pred=quickpred, include=c("rage","ragender"), maxit=20, print=FALSE, seed=54321)

##Diagnostics
#Whether imputed values are plausible 
qc <- as.data.frame(table(helsa_imp[["imp"]][["ramomeduage"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["radadeduage"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rabplace"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rarelig_e"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rpact"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rdrinkr"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rdrinke"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rsmokev"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["roccup2_2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmheight"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmheight"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmheight"]][["3"]]), exclude=NULL) #1 value with height <1
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmheight"]][["4"]]), exclude=NULL) #2 value with height <1
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmheight"]][["5"]]), exclude=NULL) #2 value with height <1
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmweight"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmweight"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmweight"]][["3"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmweight"]][["4"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmweight"]][["5"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmbmin"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmbmin"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmbmin"]][["3"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmbmin"]][["4"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmbmin"]][["5"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["hincome"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["hincome"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["hincome"]][["3"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["hincome"]][["4"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["hincome"]][["5"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rkcnt"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rshlt"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["radlfive"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["riadlza"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rmobilsev"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(helsa_imp[["imp"]][["rspsupport"]]), exclude=NULL) #Pass QC

#Check model convergence
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Graphs')
tiff("helsa_imp_convergence.tiff", width = 16, height = 16, units = "in", res = 300)
plot(helsa_imp,layout=c(8,9))
dev.off()

#Check distributions of original and imputed data
xyplot(helsa_imp, rmweight ~ rmheight | .imp, pch = 20, cex = 1.4)
xyplot(helsa_imp, rmheight ~ rmbmin | .imp, pch = 20, cex = 1.4)
xyplot(helsa_imp, rmbmin ~ rarelig_e | .imp, pch = 20, cex = 1.4)
xyplot(helsa_imp, rmbmin ~ rshlt | .imp, pch = 20, cex = 1.4)
xyplot(helsa_imp, rdrinkr ~ rmbmin | .imp, pch = 20)
xyplot(helsa_imp, rdrinkr ~ rspsupport | .imp, pch = 20)
xyplot(helsa_imp, ramomeduage ~ radadeduage | .imp, pch = 20)
xyplot(helsa_imp, ramomeduage ~ rdrinkr | .imp, pch = 20)
xyplot(helsa_imp, rshlt ~ rdrinkr | .imp, pch = 20)

#Extracts imputed data (five imputations)
helsa_imp_all_long <- complete(helsa_imp,"long", include = TRUE)
###################################################### 
#4. Post imputation modifications (helsa_all_sp28 and helsa_imp_all_long)
###################################################### 
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
load("helsa_all_sp28.rda") 
helsa_imp_all_long <- readRDS("helsa_imp_all_long.rds")

#Extract rchondis from helsa_all_sp28 and add to the five imputed datasets
var <- helsa_all_sp28 %>% select(idauniq, householdID, rchrondis)
helsa_imp_all_long <- helsa_imp_all_long %>% left_join(., var, by = "idauniq")

#Create categorical BMI
helsa_imp_all_long <- helsa_imp_all_long %>% mutate(
  rmbmicat = case_when(
    rmbmin <= 18.5 ~ 1,
    rmbmin > 18.5 & rmbmin < 25 ~ 2,
    rmbmin >= 25 & rmbmin < 30 ~ 3,
    rmbmin >= 30 & rmbmin < 35 ~ 4,
    rmbmin >= 35 & rmbmin < 40 ~ 5,
    rmbmin >= 40  ~ 6),
  rabplace=case_when(
    rabplace=="11" ~ "0",
    TRUE ~ rabplace))

#Define spousal variables (RERUN!!!!!!!!!!!)
helsa_imp_all_long <- helsa_imp_all_long %>%
  group_by(.imp, householdID) %>%
  filter(n() == 2) %>%
  arrange(.imp, householdID, idauniq) %>% 
  mutate(
    person_num = row_number(),
    sbyear = if_else(person_num == 1, lead(rabyear), lag(rabyear)),
    sage = if_else(person_num == 1, lead(rage), lag(rage)),
    sgender = if_else(person_num == 1, lead(ragender), lag(ragender)),  
    seducl = if_else(person_num == 1, lead(raeducl), lag(raeducl)),
    srace = if_else(person_num == 1,  lead(raracem),  lag(raracem)),
    sbplace= if_else(person_num == 1,  lead(rabplace),  lag(rabplace)),
    srelig= if_else(person_num == 1,  lead(rarelig_e),  lag(rarelig_e)),
    smcurln = if_else(person_num == 1, lead(rmcurln), lag(rmcurln)),
    sshlt = if_else(person_num == 1, lead(rshlt), lag(rshlt)),
    sadlfive = if_else(person_num == 1, lead(radlfive), lag(radlfive)),
    siadlza = if_else(person_num == 1, lead(riadlza), lag(riadlza)),
    smobilsev = if_else(person_num == 1, lead(rmobilsev), lag(rmobilsev)),
    spact = if_else(person_num == 1, lead(rpact), lag(rpact)),
    sdrinke = if_else(person_num == 1, lead(rdrinke), lag(rdrinke)),
    sdrinkr = if_else(person_num == 1, lead(rdrinkr), lag(rdrinkr)),
    ssmokev = if_else(person_num == 1, lead(rsmokev), lag(rsmokev)),
    skcnt = if_else(person_num == 1, lead(rkcnt), lag(rkcnt)),
    smomeducl = if_else(person_num == 1, lead(ramomeduage), lag(ramomeduage)),
    sdadeducl = if_else(person_num == 1, lead(radadeduage), lag(radadeduage)),
    soccup = if_else(person_num == 1, lead(roccup), lag(roccup)),
    soccup2_2 = if_else(person_num == 1, lead(roccup2_2), lag(roccup2_2)),
    smbmin = if_else(person_num == 1, lead(rmbmin), lag(rmbmin)),
    smbmicat = if_else(person_num == 1, lead(rmbmicat), lag(rmbmicat)),
    sclstress = if_else(person_num == 1, lead(rclstress), lag(rclstress)),
    sspsupport = if_else(person_num == 1, lead(rspsupport), lag(rspsupport)),
    schrondis = if_else(person_num == 1, lead(rchrondis), lag(rchrondis)),
    sarthritis = if_else(person_num == 1, lead(arthritis), lag(arthritis)),
    shibpe = if_else(person_num == 1, lead(hibpe), lag(hibpe)),
    sdiabe = if_else(person_num == 1, lead(diabe), lag(diabe)),  
    scancre = if_else(person_num == 1, lead(cancre), lag(cancre)),
    slunge = if_else(person_num == 1, lead(lunge), lag(lunge)),
    shearte = if_else(person_num == 1, lead(hearte), lag(hearte)),
    sstroke = if_else(person_num == 1, lead(stroke), lag(stroke)),
    spsyche = if_else(person_num == 1, lead(psyche), lag(psyche)),
    shchole = if_else(person_num == 1, lead(hchole), lag(hchole)),
    sparkine = if_else(person_num == 1, lead(parkine), lag(parkine)),
    scatracte = if_else(person_num == 1, lead(catracte), lag(catracte)),
    sasthmae = if_else(person_num == 1, lead(asthmae), lag(asthmae)),
    sosteoe = if_else(person_num == 1, lead(osteoe), lag(osteoe)),
    shibpe_dm = if_else(person_num == 1, lead(hibpe_dm), lag(hibpe_dm)),
    sdiabe_dm = if_else(person_num == 1, lead(diabe_dm), lag(diabe_dm)),  
    scancre_dm = if_else(person_num == 1, lead(cancre_dm), lag(cancre_dm)),
    slunge_dm = if_else(person_num == 1, lead(lunge_dm), lag(lunge_dm)),
    spsyche_dm = if_else(person_num == 1, lead(psyche_dm), lag(psyche_dm)),
    shchole_dm = if_else(person_num == 1, lead(hchole_dm), lag(hchole_dm)),
    sasthmae_dm = if_else(person_num == 1, lead(asthmae_dm), lag(asthmae_dm)),
    sosteoe_dm = if_else(person_num == 1, lead(osteoe_dm), lag(osteoe_dm))) %>%
  ungroup()

#Save dataset helsa_imp_all_long
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
saveRDS(helsa_imp_all_long, file = "helsa_imp_all_long.rds")
###################################################### 
###################################################### 


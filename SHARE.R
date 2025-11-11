#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20251021
#UPDATED: 20251111
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE SHARE 2004-2022 wave (1-2,4-9), version G, DATA STRUCTURE, DATA PREPARATION, PERFORM STATISTICAL ANALYSES 
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#Harmonized SHARE waves 1 to 9
  
#Logbook
###################################################### 
#20251031 For wave 7, include only those who had completed interviews, those participated in condensed interview were not counted to avoid missingness of key covariates
#20251111 Add Birth year
######################################################

#Things to pay attention
######################################################
#20251101 drop hhwitothhh income from other household members after tax dropped due to it only recorded in waves 2, 4-5
#20251101 drop rwsocwk and rwsocyr social activity due to inclusion only in some waves
#20251104 Recode missing value for occupation in Stata
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
#2. Spousal pairs participated in each wave by country (using harmonized data)
#3. The number of eligible spousal pairs in each wave (using harmonized data)
#4. The number of individuals died during follow-up (hshare_all_sp4)
#5. Descriptive data of selected baseline and outcome variables in hshare_all_sp25

#DATA PREPARATION
#1. List of included variables  
#2. Define outcomes and potential confounders to be adjusted in hshare_all_sp5
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
               cardx
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
#2. Spousal pairs participated in each wave by country (using harmonized data)
#3. The number of eligible spousal pairs in each wave (using harmonized data)
#4. The number of individuals died during follow-up (hshare_all_sp4)
#5. Descriptive data of selected baseline and outcome variables in hshare_all_sp25
######################################################
#1. Participation of individuals across waves (using harmonized data)
######################################################
#Load harmonized ELSA data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/SHARE/Waves/GH_SHARE_g_rel9-0-0_ALL_datasets_stata')
hshare <- read_dta("GH_SHARE_g.dta")

##Wave 1
#The total number of individual #30416
table(hshare$inw1) 
#The total number of spousal pairs #9272
w1 <- hshare %>% filter(inw1==1 & s1iwstat==1) 
spair_1 <- w1 %>% filter(!is.na(mergeid) & !is.na(s1mergeid)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(mergeid, s1mergeid), 
                  pmax(mergeid, s1mergeid), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 2
#The total number of individual #37132
table(hshare$inw2) 
#The total number of spousal pairs #11724
w2 <- hshare %>% filter(inw2==1 & s2iwstat==1) 
spair_2 <- w2 %>% filter(!is.na(mergeid) & !is.na(s2mergeid)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(mergeid, s2mergeid), 
                  pmax(mergeid, s2mergeid), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 4
#The total number of individual #57982
table(hshare$inw4) 
#The total number of spousal pairs #18148
w4 <- hshare %>% filter(inw4==1 & s4iwstat==1) 
spair_4 <- w4 %>% filter(!is.na(mergeid) & !is.na(s4mergeid)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(mergeid, s4mergeid), 
                  pmax(mergeid, s4mergeid), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 5
#The total number of individual #66038
table(hshare$inw5) 
#The total number of spousal pairs #20830
w5 <- hshare %>% filter(inw5==1 & s5iwstat==1) 
spair_5 <- w5 %>% filter(!is.na(mergeid) & !is.na(s5mergeid)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(mergeid, s5mergeid), 
                  pmax(mergeid, s5mergeid), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pair

##Wave 6
#The total number of individual #68055
table(hshare$inw6) 
#The total number of spousal pairs #21542
w6 <- hshare %>% filter(inw6==1 & s6iwstat==1) 
spair_6 <- w6 %>% filter(!is.na(mergeid) & !is.na(s6mergeid)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(mergeid, s6mergeid), 
                  pmax(mergeid, s6mergeid), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pair

##Wave 7 
#The total number of individual #77181 have done condensed questionnaire, of which 13948 completed panel interviews as well
qc <- hshare %>% filter(inw7==1 |inw7c==1)
table(hshare$inw7) 
table(hshare$inw7c) 
#The total number of spousal pairs #24183
w7 <- hshare %>% filter(inw7c==1 & s7iwstat==1) 
spair_7 <- w7 %>% filter(!is.na(mergeid) & !is.na(s7mergeid)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(mergeid, s7mergeid), 
                  pmax(mergeid, s7mergeid), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 8
#The total number of individual #53695
table(hshare$inw8) 
#The total number of spousal pairs #16073
w8 <- hshare %>% filter(inw8==1 & s8iwstat==1) 
spair_8 <- w8 %>% filter(!is.na(mergeid) & !is.na(s8mergeid)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(mergeid, s8mergeid), 
                  pmax(mergeid, s8mergeid), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 9
#The total number of individual #69447
table(hshare$inw9) 
#The total number of spousal pairs #21067
w9 <- hshare %>% filter(inw9==1 & s9iwstat==1) 
spair_9 <- w9 %>% filter(!is.na(mergeid) & !is.na(s9mergeid)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(mergeid, s9mergeid), 
                  pmax(mergeid, s9mergeid), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs
#The total number of spousal pairs #2844 (Altenative calculation)
helsa_8_sp <- helsa %>% filter(inw8==1) %>% add_count(h8coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h8coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 
######################################################
#2. Spousal pairs participated in each wave by country (using harmonized data)
######################################################
#Data sets including spousal pairs for each wave
w1 <- hshare %>% filter(inw1==1 & s1iwstat==1) %>% mutate(wave="Wave 1", country=as.factor(country)) %>% select(wave, country)
w2 <- hshare %>% filter(inw2==1 & s2iwstat==1) %>% mutate(wave="Wave 2", country=as.factor(country)) %>% select(wave, country)
w4 <- hshare %>% filter(inw4==1 & s4iwstat==1) %>% mutate(wave="Wave 4", country=as.factor(country)) %>% select(wave, country)
w5 <- hshare %>% filter(inw5==1 & s5iwstat==1) %>% mutate(wave="Wave 5", country=as.factor(country)) %>% select(wave, country)
w6 <- hshare %>% filter(inw6==1 & s6iwstat==1) %>% mutate(wave="Wave 6", country=as.factor(country)) %>% select(wave, country)
w7 <- hshare %>% filter(inw7c==1 & s7iwstat==1) %>% mutate(wave="Wave 7", country=as.factor(country)) %>% select(wave, country)
w8 <- hshare %>% filter(inw8==1 & s8iwstat==1) %>% mutate(wave="Wave 8", country=as.factor(country)) %>% select(wave, country)
w9 <- hshare %>% filter(inw9==1 & s9iwstat==1) %>% mutate(wave="Wave 9", country=as.factor(country)) %>% select(wave, country)

#Combine datasets
w1to9 <- rbind(w1, w2, w4, w5, w6, w7,w8, w9)

#Create combined table including information on country counts and proportions for spousal pairs in each wave
country_wave_table <- w1to9 %>%
  tbl_strata(
    strata = wave,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(
        include = country,
        statistic = list(all_categorical() ~ "{n} ({p}%)"),
        digits = list(all_categorical() ~ c(0, 1))
      ) %>%
      modify_header(update = list(label ~ "**Variable**"))
  ) %>%
  modify_spanning_header(
    list(
      starts_with("stat_0_1") ~ "**Wave 1**",
      starts_with("stat_0_2") ~ "**Wave 2**", 
      starts_with("stat_0_3") ~ "**Wave 4**",
      starts_with("stat_0_4") ~ "**Wave 5**",
      starts_with("stat_0_5") ~ "**Wave 6**",
      starts_with("stat_0_6") ~ "**Wave 7**",
      starts_with("stat_0_7") ~ "**Wave 8**",
      starts_with("stat_0_8") ~ "**Wave 9**"
    )
  )

#Save frequency table
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/SHARE')
write_xlsx(country_wave_table[["table_body"]], path = "SHARE_country_sp_by_wave.xlsx", col_names=T, format_headers=T)
######################################################
#3. The number of eligible spousal pairs in each wave (using harmonized data)
######################################################
#Inclusion criteria: For each dataset, heterosexual spousal pairs in which both partners participated in at least two survey waves will be included. In cases where multiple spouses were recorded for an individual, only the first will be retained. Both spouses must have complete data on age, gender, and doctor-diagnosed arthritis.
#Exclusion criteria: To ensure the reliability of responses, spousal pairs in which either partner reported a doctor-diagnosed memory-related condition (e.g., dementia or Alzheimer’s disease) or was receiving treatment for such conditions will be excluded. Additionally, proxy interviews—often indicative of cognitive impairment—will also be excluded. 
######################################################
#Load data and select relevant variables
#Harmonized data_waves_1_to_9
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/SHARE/Waves/GH_SHARE_g_rel9-0-0_ALL_datasets_stata')
hshare <- read_dta("GH_SHARE_g.dta") %>% dplyr::select(1, 2:12, 22:31, 52:61, 71:122, 145:152, 162:169, 188:195, 244:251, 276:327, 344:352, 398:406, 416:431, 464:488, 507:546, 1063:1110, 1255:1302, 1423:1470, 1807:2070, 2127:2397, 2424:2445, 2478:2525, 2690:2721, 2744:2759, 2774:2787, 2802:2845, 3054:3036, 3914:3915, 3918:3931, 3946:3947, 3950:3963, 4042:4050, 4058, 4060:4066, 4074:4091, 4170:4171, 4174:4187, 4202:4203, 4206:4219, 4234:4235, 4238:4251, 4266, 4268:4311, 4385:4392, 4561:4568, 4719:4734, 4747:4762, 4779:4810, 4927:4942, 5017:5032, 5063:5078, 6775:6850, 7183:7252)
######################################################
##Wave 1 #16864 individuals and 8432 spousal pairs participating from wave 1
######################################################
#all spousal pairs
hshare_w1 <- hshare %>% filter(inw1==1 & !is.na(s1mergeid) & s1mergeid != 0 & !is.na(s1iwstat) & s1iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(mergeid, s1mergeid), 
                  pmax(mergeid, s1mergeid), 
                  sep = "_")) #18544 individuals and 9272 spousal pairs
table(as.data.frame(table(hshare_w1$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hshare_w1$h1cpl) # all 1

#QC: age distribution, should be 50 or older
table(hshare_w1$r1agey, exclude=NULL)
table(hshare_w1$s1agey, exclude=NULL) #Cutoff 18 years, all above 18 years, the youngest was 25 years, 1 individual with age missing

#Exclude spousal pairs with either of spouse with missing age values
hshare_w1_2 <- hshare_w1 %>% filter(r1agey >=18 & s1agey >=18) #18542 individuals and 9271 spousal pairs
table(as.data.frame(table(hshare_w1_2$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hshare_w1_2 <- hshare_w1_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hshare_w1_2$missing_count_rarthre, exclude=NULL)
#1860 individuals with no missing, 1577 individuals with 1 missing value, 2087 individuals with 2 missing values, 1792 individuals with 3 missing values
#1684 individuals with 4 missing values, 1808 individuals with 5 missing values, 3608 individuals with 6 missing values, 4073 individuals with 7 missing values, and 53 with 8 missing values

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- hshare_w1_2 %>% filter((r2alzdeme==1 & !is.na(r2alzdeme))|
                             (r4alzdeme==1 & !is.na(r4alzdeme))| 
                             (r5alzdeme==1 & !is.na(r5alzdeme))|
                             (r6alzdeme==1 & !is.na(r6alzdeme))|
                             (r7alzdeme==1 & !is.na(r7alzdeme))|
                             (r8alzdeme==1 & !is.na(r8alzdeme))|
                             (r9alzdeme==1 & !is.na(r9alzdeme))) %>% select(mergeid, householdID, r2alzdeme, r4alzdeme:r9alzdeme, s2alzdeme, s4alzdeme:s9alzdeme) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #905 individuals, with 66 are spousal pairs and 773 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
hshare_w1_3 <- hshare_w1_2 %>% filter(!householdID %in% qc$householdID)  #16864 individuals and 8432 spousal pairs
table(as.data.frame(table(hshare_w1_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hshare_w1_3$r1iwstat, exclude=NULL) #all 1
table(hshare_w1_3$s1iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hshare_w1_3 <- hshare_w1_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hshare_w1_3$total_participations, exclude=NULL) #3981 individuals participated in wave 1 only
qc <- as.data.frame(table(hshare_w1_3$spousal_part_pattern, exclude=NULL)) #124 patterns

#Check the number of individual participated only in wave 1
qc <- hshare_w1_3 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #3981 individuals, 1506 spousal pairs and 969 individuals
######################################################
##Wave 2 ##10694 individuals and 5347 spousal pairs participating from wave 2
######################################################
#All spousal pairs participating in wave 2
hshare_w2 <- hshare %>% filter(inw2==1 & !is.na(s2mergeid) & s2mergeid != 0 & !is.na(s2iwstat) & s2iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(mergeid, s2mergeid), 
                      pmax(mergeid, s2mergeid), 
                      sep = "_")) #23448 individuals, with 11724 spousal pairs and 
table(as.data.frame(table(hshare_w2$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hshare_w2$h2cpl) # all 1

#Exclude individuals included in hshare_w1_2
hshare_w2_2 <- hshare_w2 %>% filter(!householdID %in% hshare_w1_3$householdID) #13068 individuals, with 6534 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_w2_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hshare_w2_2$r2agey, exclude=NULL)
table(hshare_w2_2$s2agey, exclude=NULL) #All respondents aged above 18 and 1 individual with missing age

#Exclude spousal pairs with either of spouse with missing age values
hshare_w2_3 <- hshare_w2_2 %>% filter(r2agey >=18 & s2agey >=18) #13066 individuals and 6533 spousal pairs
table(as.data.frame(table(hshare_w2_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hshare_w2_3 <- hshare_w2_3 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hshare_w2_3$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hshare_w2_3$inw1, exclude=NULL) #2660 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- hshare_w2_3 %>% filter((r2alzdeme==1 & !is.na(r2alzdeme))|
                               (r4alzdeme==1 & !is.na(r4alzdeme))| 
                               (r5alzdeme==1 & !is.na(r5alzdeme))|
                               (r6alzdeme==1 & !is.na(r6alzdeme))|
                               (r7alzdeme==1 & !is.na(r7alzdeme))|
                               (r8alzdeme==1 & !is.na(r8alzdeme))|
                               (r9alzdeme==1 & !is.na(r9alzdeme))) %>% select(mergeid, householdID, r2alzdeme, r4alzdeme:r9alzdeme, s2alzdeme, s4alzdeme:s9alzdeme) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #1289 individuals, with 103 are spousal pairs and 1083 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
hshare_w2_4 <- hshare_w2_3 %>% filter(!householdID %in% qc$householdID)  #10694 individuals and 5347 spousal pairs
table(as.data.frame(table(hshare_w2_4$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hshare_w2_4$r2iwstat, exclude=NULL) #all 1
table(hshare_w2_4$s2iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hshare_w2_4 <- hshare_w2_4 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hshare_w2_4$total_participations, exclude=NULL) #3565 individuals participated in wave 2 only
qc <- as.data.frame(table(hshare_w2_4$spousal_part_pattern, exclude=NULL)) #117 patterns

#Check the number of individual participated only in wave 2
qc <- hshare_w2_4 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #3565 individuals, 1362 spousal pairs and 841 individuals
######################################################
##Wave 4 ##10694 individuals and 5347 spousal pairs participating from wave 4
######################################################
#All spousal pairs participating in wave 4
hshare_w4 <- hshare %>% filter(inw4==1 & !is.na(s4mergeid) & s4mergeid != 0 & !is.na(s4iwstat) & s4iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(mergeid, s4mergeid), 
                      pmax(mergeid, s4mergeid), 
                      sep = "_")) #36296 individuals, with 18148 spousal pairs and 
table(as.data.frame(table(hshare_w4$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hshare_w4$h4cpl) # all 1

#Exclude individuals included in hshare_w1_3 or hshare_w2_4
hshare_w4_2 <- hshare_w4 %>% filter(!(householdID %in% hshare_w1_3$householdID | householdID %in% hshare_w2_4$householdID)) #25948 individuals, with 12974 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_w4_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hshare_w4_2$r4agey, exclude=NULL)
table(hshare_w4_2$s4agey, exclude=NULL) #All respondents aged above 18

#SKIPPED:Exclude spousal pairs with either of spouse with missing age values
#hshare_w2_3 <- hshare_w2_2 %>% filter(r2agey >=18 & s2agey >=18) #13066 individuals and 6533 spousal pairs
#table(as.data.frame(table(hshare_w2_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hshare_w4_2 <- hshare_w4_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hshare_w4_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hshare_w4_2$inw1, exclude=NULL) #1359 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- hshare_w4_2 %>% filter((r2alzdeme==1 & !is.na(r2alzdeme))|
                               (r4alzdeme==1 & !is.na(r4alzdeme))| 
                               (r5alzdeme==1 & !is.na(r5alzdeme))|
                               (r6alzdeme==1 & !is.na(r6alzdeme))|
                               (r7alzdeme==1 & !is.na(r7alzdeme))|
                               (r8alzdeme==1 & !is.na(r8alzdeme))|
                               (r9alzdeme==1 & !is.na(r9alzdeme))) %>% select(mergeid, householdID, r2alzdeme, r4alzdeme:r9alzdeme, s2alzdeme, s4alzdeme:s9alzdeme) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #1936 individuals, with 127 are spousal pairs and 1682 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
hshare_w4_3 <- hshare_w4_2 %>% filter(!householdID %in% qc$householdID)  #22330 individuals and 11165 spousal pairs
table(as.data.frame(table(hshare_w4_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hshare_w4_3$r4iwstat, exclude=NULL) #all 1
table(hshare_w4_3$s4iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hshare_w4_3 <- hshare_w4_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hshare_w4_3$total_participations, exclude=NULL) #4408 individuals participated in wave 4 only
qc <- as.data.frame(table(hshare_w4_3$spousal_part_pattern, exclude=NULL)) #99 patterns

#Check the number of individual participated only in wave 4
qc <- hshare_w4_3 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #4408 individuals, 1505 spousal pairs and 1398 individuals
######################################################
##Wave 5 ##15510 individuals and 7755 spousal pairs participating from wave 5
######################################################
#All spousal pairs participating in wave 5
hshare_w5 <- hshare %>% filter(inw5==1 & !is.na(s5mergeid) & s5mergeid != 0 & !is.na(s5iwstat) & s5iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(mergeid, s5mergeid), 
                      pmax(mergeid, s5mergeid), 
                      sep = "_")) #41660 individuals, with 20830 spousal pairs and 
table(as.data.frame(table(hshare_w5$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hshare_w5$h5cpl) # all 1

#Exclude individuals included in hshare_w1_3 or hshare_w2_4 or hshare_w4_3
hshare_w5_2 <- hshare_w5 %>% filter(!(householdID %in% hshare_w1_3$householdID | householdID %in% hshare_w2_4$householdID | householdID %in% hshare_w4_3$householdID)) #19550 individuals, with  9775 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_w5_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hshare_w5_2$r5agey, exclude=NULL)
table(hshare_w5_2$s5agey, exclude=NULL) #All respondents aged above 18 and 1 individual with missing age

#Exclude spousal pairs with either of spouse with missing age values
hshare_w5_3 <- hshare_w5_2 %>% filter(r5agey >=18 & s5agey >=18) #19548 individuals and 9774 spousal pairs
table(as.data.frame(table(hshare_w5_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hshare_w5_3 <- hshare_w5_3 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hshare_w5_3$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hshare_w5_3$inw1, exclude=NULL) #1249 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- hshare_w5_3 %>% filter((r2alzdeme==1 & !is.na(r2alzdeme))|
                               (r4alzdeme==1 & !is.na(r4alzdeme))| 
                               (r5alzdeme==1 & !is.na(r5alzdeme))|
                               (r6alzdeme==1 & !is.na(r6alzdeme))|
                               (r7alzdeme==1 & !is.na(r7alzdeme))|
                               (r8alzdeme==1 & !is.na(r8alzdeme))|
                               (r9alzdeme==1 & !is.na(r9alzdeme))) %>% select(mergeid, householdID, r2alzdeme, r4alzdeme:r9alzdeme, s2alzdeme, s4alzdeme:s9alzdeme) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #2156 individuals, with 137 are spousal pairs and 1882 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
hshare_w5_4 <- hshare_w5_3 %>% filter(!householdID %in% qc$householdID)  #15510 individuals and 7755 spousal pairs
table(as.data.frame(table(hshare_w5_4$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hshare_w5_4$r5iwstat, exclude=NULL) #all 1
table(hshare_w5_4$s5iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hshare_w5_4 <- hshare_w5_4 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hshare_w5_4$total_participations, exclude=NULL) #3105 individuals participated in wave 5 only
qc <- as.data.frame(table(hshare_w5_4$spousal_part_pattern, exclude=NULL)) #96 patterns

#Check the number of individual participated only in wave 5
qc <- hshare_w5_4 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #3105 individuals, 1024 spousal pairs and 1057 individuals
######################################################
##Wave 6 ##9058 individuals and 4529 spousal pairs participating from wave 6
######################################################
#All spousal pairs participating in wave 6
hshare_w6 <- hshare %>% filter(inw6==1 & !is.na(s6mergeid) & s6mergeid != 0 & !is.na(s6iwstat) & s6iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(mergeid, s6mergeid), 
                      pmax(mergeid, s6mergeid), 
                      sep = "_")) #43084 individuals, with 21542 spousal pairs and 
table(as.data.frame(table(hshare_w6$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hshare_w6$h6cpl) # all 1

#Exclude individuals included in hshare_w1_3 or hshare_w2_4 or hshare_w4_3 or hshare_w5_4
hshare_w6_2 <- hshare_w6 %>% filter(!(householdID %in% hshare_w1_3$householdID | householdID %in% hshare_w2_4$householdID | householdID %in% hshare_w4_3$householdID | householdID %in% hshare_w5_4$householdID)) #13146 individuals, with  6573 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_w6_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hshare_w6_2$r6agey, exclude=NULL)
table(hshare_w6_2$s6agey, exclude=NULL) #All respondents aged above 18 and 2 individuals with missing age

#Exclude spousal pairs with either of spouse with missing age values
hshare_w6_3 <- hshare_w6_2 %>% filter(r6agey >=18 & s6agey >=18) #13142 individuals and 6571 spousal pairs
table(as.data.frame(table(hshare_w6_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hshare_w6_3 <- hshare_w6_3 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hshare_w6_3$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hshare_w6_3$inw1, exclude=NULL) #977 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- hshare_w6_3 %>% filter((r2alzdeme==1 & !is.na(r2alzdeme))|
                               (r4alzdeme==1 & !is.na(r4alzdeme))| 
                               (r5alzdeme==1 & !is.na(r5alzdeme))|
                               (r6alzdeme==1 & !is.na(r6alzdeme))|
                               (r7alzdeme==1 & !is.na(r7alzdeme))|
                               (r8alzdeme==1 & !is.na(r8alzdeme))|
                               (r9alzdeme==1 & !is.na(r9alzdeme))) %>% select(mergeid, householdID, r2alzdeme, r4alzdeme:r9alzdeme, s2alzdeme, s4alzdeme:s9alzdeme) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #2188 individuals, with 146 are spousal pairs and 1896 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
hshare_w6_4 <- hshare_w6_3 %>% filter(!householdID %in% qc$householdID)  #9058 individuals and 4529 spousal pairs
table(as.data.frame(table(hshare_w6_4$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hshare_w6_4$r6iwstat, exclude=NULL) #all 1
table(hshare_w6_4$s6iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hshare_w6_4 <- hshare_w6_4 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hshare_w6_4$total_participations, exclude=NULL) #1457 individuals participated in wave 6 only
qc <- as.data.frame(table(hshare_w6_4$spousal_part_pattern, exclude=NULL)) #87 patterns

#Check the number of individual participated only in wave 6
qc <- hshare_w6_4 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #1457 individuals, 431 spousal pairs and 595 individuals
######################################################
##Wave 7 ##14 individuals and 7 spousal pairs participating from wave 7
######################################################
#All spousal pairs participating in wave 7
hshare_w7 <- hshare %>% filter(inw7==1 & !is.na(s7mergeid) & s7mergeid != 0 & !is.na(s7iwstat) & s7iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(mergeid, s7mergeid), 
                      pmax(mergeid, s7mergeid), 
                      sep = "_")) #8477 individuals, with 4030 spousal pairs and 417 individuals
table(as.data.frame(table(hshare_w7$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hshare_w7$h7cpl) # all 1

#Exclude individuals included in hshare_w1_3 or hshare_w2_4 or hshare_w4_3 or hshare_w5_4 or hshare_w6_4
hshare_w7_2 <- hshare_w7 %>% filter(!(householdID %in% hshare_w1_3$householdID | householdID %in% hshare_w2_4$householdID | householdID %in% hshare_w4_3$householdID | householdID %in% hshare_w5_4$householdID | householdID %in% hshare_w6_4$householdID)) #948 individuals, with 434 spousal pairs and 80 individuals
#Check the number of spousal pairs
table(as.data.frame(table(hshare_w7_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hshare_w7_2$r7agey, exclude=NULL)
table(hshare_w7_2$s7agey, exclude=NULL) #All respondents aged above 18 and no missing

#SKIPPED:Exclude spousal pairs with either of spouse with missing age values
#hshare_w6_3 <- hshare_w6_2 %>% filter(r6agey >=18 & s6agey >=18) #13142 individuals and 6571 spousal pairs
#table(as.data.frame(table(hshare_w6_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hshare_w7_2 <- hshare_w7_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hshare_w7_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hshare_w7_2$inw1, exclude=NULL) #569 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- hshare_w7_2 %>% filter((r2alzdeme==1 & !is.na(r2alzdeme))|
                               (r4alzdeme==1 & !is.na(r4alzdeme))| 
                               (r5alzdeme==1 & !is.na(r5alzdeme))|
                               (r6alzdeme==1 & !is.na(r6alzdeme))|
                               (r7alzdeme==1 & !is.na(r7alzdeme))|
                               (r8alzdeme==1 & !is.na(r8alzdeme))|
                               (r9alzdeme==1 & !is.na(r9alzdeme))) %>% select(mergeid, householdID, r2alzdeme, r4alzdeme:r9alzdeme, s2alzdeme, s4alzdeme:s9alzdeme) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #477 individuals, with 31 are spousal pairs and 415 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
hshare_w7_3 <- hshare_w7_2 %>% filter(!householdID %in% qc$householdID)  #75 individuals and 7 spousal pairs and 61 indiviudals
table(as.data.frame(table(hshare_w7_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#Select those with no paired spouses
qc <- as.data.frame(table(hshare_w7_3$householdID, exclude = NULL)) %>% filter(Freq==1)

#Exclude those with no paired spouses
hshare_w7_4 <- hshare_w7_3 %>% filter(!householdID %in% qc$Var1)
table(as.data.frame(table(hshare_w7_4$householdID, exclude = NULL))$Freq, exclude=NULL) #14 individuals and 7 spousal pairs

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hshare_w7_4$r7iwstat, exclude=NULL) #all 1
table(hshare_w7_4$s7iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hshare_w7_4 <- hshare_w7_4 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hshare_w7_4$total_participations, exclude=NULL) #2 individuals participated in wave 7 only
qc <- as.data.frame(table(hshare_w7_4$spousal_part_pattern, exclude=NULL)) #11 patterns

#Check the number of individual participated only in wave 7
qc <- hshare_w7_4 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #2 individuals and they were no spouses
######################################################
##Wave 8 ##10678 individuals and 5339 spousal pairs participating from wave 8
######################################################
#All spousal pairs participating in wave 6
hshare_w8 <- hshare %>% filter(inw8==1 & !is.na(s8mergeid) & s8mergeid != 0 & !is.na(s8iwstat) & s8iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(mergeid, s8mergeid), 
                      pmax(mergeid, s8mergeid), 
                      sep = "_")) #32146 individuals, with 16073 spousal pairs and 
table(as.data.frame(table(hshare_w8$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hshare_w8$h8cpl) # all 1

#Exclude individuals included in hshare_w1_3 or hshare_w2_4 or hshare_w4_3 or hshare_w5_4 or hshare_w6_4 or hshare_w7_4
hshare_w8_2 <- hshare_w8 %>% filter(!(householdID %in% hshare_w1_3$householdID | householdID %in% hshare_w2_4$householdID | householdID %in% hshare_w4_3$householdID | householdID %in% hshare_w5_4$householdID | householdID %in% hshare_w6_4$householdID | householdID %in% hshare_w7_4$householdID)) #12966 individuals, with  6483 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_w8_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hshare_w8_2$r8agey, exclude=NULL)
table(hshare_w8_2$s8agey, exclude=NULL) #All respondents aged above 18 and no missing

#SKIPPED:Exclude spousal pairs with either of spouse with missing age values
#hshare_w6_3 <- hshare_w6_2 %>% filter(r6agey >=18 & s6agey >=18) #13142 individuals and 6571 spousal pairs
#table(as.data.frame(table(hshare_w6_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hshare_w8_2 <- hshare_w8_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hshare_w8_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hshare_w8_2$inw1, exclude=NULL) #440 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of spousal pairs with doctor-diagnosed memory-related disease (no medication data available) across 9 waves
qc <- hshare_w8_2 %>% filter((r2alzdeme==1 & !is.na(r2alzdeme))|
                               (r4alzdeme==1 & !is.na(r4alzdeme))| 
                               (r5alzdeme==1 & !is.na(r5alzdeme))|
                               (r6alzdeme==1 & !is.na(r6alzdeme))|
                               (r7alzdeme==1 & !is.na(r7alzdeme))|
                               (r8alzdeme==1 & !is.na(r8alzdeme))|
                               (r9alzdeme==1 & !is.na(r9alzdeme))) %>% select(mergeid, householdID, r2alzdeme, r4alzdeme:r9alzdeme, s2alzdeme, s4alzdeme:s9alzdeme) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #1218 individuals, with 74 are spousal pairs and 1070 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across 9 waves 
hshare_w8_3 <- hshare_w8_2 %>% filter(!householdID %in% qc$householdID)  #10678 individuals and 5339 spousal pairs
table(as.data.frame(table(hshare_w8_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hshare_w8_3$r8iwstat, exclude=NULL) #all 1
table(hshare_w8_3$s8iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hshare_w8_3 <- hshare_w8_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  r7_part = as.numeric(r7iwstat == 1),
  r8_part = as.numeric(r8iwstat == 1),
  r9_part = as.numeric(r9iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r4_part, 
                                r5_part, r6_part, r7_part, r8_part, r9_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hshare_w8_3$total_participations, exclude=NULL) #1251 individuals participated in wave 8 only
qc <- as.data.frame(table(hshare_w8_3$spousal_part_pattern, exclude=NULL)) #65 patterns

#Check the number of individual participated only in wave 6
qc <- hshare_w8_3 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #1251 individuals, 449 spousal pairs and 353 individuals
######################################################
##All eligible spousal pairs, hshare_all_sp4, 61796 individuals and 30898 spousal pairs
######################################################
#Add inclusion wave indicator and create unique householdID
hshare_w1_3 <- hshare_w1_3 %>% mutate(inclusion_wave="Wave 1")
hshare_w2_4 <- hshare_w2_4 %>% mutate(inclusion_wave="Wave 2")
hshare_w4_3 <- hshare_w4_3 %>% mutate(inclusion_wave="Wave 4")
hshare_w5_4 <- hshare_w5_4 %>% mutate(inclusion_wave="Wave 5")
hshare_w6_4 <- hshare_w6_4 %>% mutate(inclusion_wave="Wave 6")
hshare_w7_4 <- hshare_w7_4 %>% mutate(inclusion_wave="Wave 7")
hshare_w8_3 <- hshare_w8_3 %>% mutate(inclusion_wave="Wave 8")

##Dataset including all spousal pairs, combining hshare_w1_3, hshare_w2_4, hshare_w4_3, hshare_w5_4, hshare_w6_4, hshare_w7_4, and hshare_w8_3)
hshare_all_sp <- rbind(hshare_w1_3, hshare_w2_4, hshare_w4_3, hshare_w5_4, hshare_w6_4, hshare_w7_4, hshare_w8_3) #85148 individuals and 42574 spousal pairs#Check frequency of inclusion_wave
table(hshare_all_sp$inclusion_wave, exclude=NULL)
#Check the number of spousal pairs
table(as.data.frame(table(hshare_all_sp$householdID, exclude = NULL))$Freq, exclude=NULL) 
#Check if all were heterosexual spouse pairs
hetero <- hshare_all_sp %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%
  summarise(hetero = n_distinct(ragender) == 2) %>% filter(hetero=="FALSE")
table(hetero$hetero, exclude=NULL) #91 spousal pairs were homosexual
qc <- hshare_all_sp %>% filter(householdID %in% hetero$householdID) %>% select("mergeid", "householdID","ragender",s1gender, s2gender, s4gender:s9gender, "inclusion_wave")  

#Exclude the homosexual spousal pair
hshare_all_sp2 <- hshare_all_sp %>% filter(!householdID %in% hetero$householdID) #84966 individuals and 42483 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_all_sp2$householdID))$Freq, exclude=NULL) 
#Select individuals with total_participations==1
one_wave <- hshare_all_sp2 %>% filter(total_participations==1) %>% select("mergeid", "householdID", "inclusion_wave", "total_participations") #17714 individuals and 6257 spousal pairs and 5200 individuals with spouses participated in later waves
#Check the number of spousal pairs
table(as.data.frame(table(one_wave$householdID))$Freq, exclude=NULL) 

#Exclude individuals with total_participations==1
hshare_all_sp3 <- hshare_all_sp2 %>% filter(!(householdID %in% one_wave$householdID)) #62052 individuals and 31026 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_all_sp3$householdID))$Freq, exclude=NULL) 

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

hshare_all_sp3$missing_count_rarthre2 <- count_missing_by_participation(hshare_all_sp3)
#Check the frequency of missing_count_rarthre2 by total_participations
table(hshare_all_sp3$missing_count_rarthre2, hshare_all_sp3$total_participations,exclude=NULL) 
#130 individuals participated in two waves with 1 (n=126) or 2 (n=4) missing values
#3 individuals participated in three waves with 2 (n=2) or 3 (n=1) missing values

#Select those with missing_count_rarthre2 exceeds the upper limit
qc <- hshare_all_sp3 %>% filter((missing_count_rarthre2==1 & total_participations==2) | (missing_count_rarthre2==2 & total_participations==2) | (missing_count_rarthre2==2 & total_participations==3) | (missing_count_rarthre2==3 & total_participations==3)) %>% select("mergeid","householdID","inclusion_wave","spousal_part_pattern","total_participations", "r1arthre", "r2arthre", "r4arthre", "r5arthre", "r6arthre", "r7arthre", "r8arthre", "r9arthre", "missing_count_rarthre2")
#Check the number of spousal pairs
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #133 individuals, 5 spousal pairs and 123 individuals

#Exclude individuals with missing_count_rarthre2 exceeds the upper limit, as well as their spouses
hshare_all_sp4 <- hshare_all_sp3 %>% filter(!householdID %in% qc$householdID) #61796 individuals and 30898 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_all_sp4$householdID))$Freq, exclude=NULL) 

#Check the frequencies of total_participation by inclusion_wave
table(hshare_all_sp4$total_participations, hshare_all_sp4$inclusion_wave, exclude=NULL)
######################################################
#4. The number of individuals died during follow-up (hshare_all_sp4)
######################################################
#Load harmonized end of life dataset
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/SHARE/Waves/GH_SHARE_g_rel9-0-0_ALL_datasets_stata')
hshare_eol <- read_dta("GH_SHARE_EOL_g.dta") %>% select(mergeid, raxyear,radage, racod_s)

#Check frequency of radyear 
table(hshare_all_sp4$radyear, exclude = NULL) #6221 individuals with death year recorded 
qc <- hshare_all_sp4 %>% filter(!is.na(radyear)) %>% select(mergeid, householdID, radyear, inclusion_wave, r1iwy, r2iwy, r4iwy:r9iwy, r1agey, r2agey, r4agey:r9agey, r1arthre, r2arthre, r4arthre:r9arthre)

#Check if any individuals in hshare_all_sp4 also in hshare_eol as well
qc2 <- hshare_all_sp4 %>% filter(mergeid %in% hshare_eol$mergeid) %>% select(mergeid, householdID, radyear, inclusion_wave, r1iwy, r2iwy, r4iwy:r9iwy, r1agey, r2agey, r4agey:r9agey, r1arthre, r2arthre, r4arthre:r9arthre) #6099 individuals died during follow-up
qc3 <- qc %>% filter(!mergeid %in% qc2$mergeid)
#350 individuals with death year recorded but with no exit interview 
qc4 <- qc2 %>% filter(!mergeid %in% qc$mergeid)
#228 individuals with exit interview but no death year recorded
qc5 <- qc2 %>% filter(mergeid %in% qc$mergeid)

#Select those with exit interview or death year record
death <- rbind(qc5,qc3, qc4) #6449 individuals

#Add death information from exit interview
death2 <- death %>% left_join(.,hshare_eol, by="mergeid") 

#Select those with interview age older than death age
death3 <- death2 %>% mutate(exclude=case_when(
  !is.na(radage) & inclusion_wave=="Wave 1" & radage < r1agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 2" & radage < r2agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 4" & radage < r4agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 5" & radage < r5agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 6" & radage < r6agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 7" & radage < r7agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 8" & radage < r8agey ~ 1,
  !is.na(radyear) & inclusion_wave=="Wave 1" & radyear < r1iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 2" & radyear < r2iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 4" & radyear < r4iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 5" & radyear < r5iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 6" & radyear < r6iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 7" & radyear < r7iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 8" & radyear < r8iwy ~ 2,
  TRUE ~ NA_real_)) %>% filter(exclude==1|exclude==2) #34 individuals with death happened before interview
#Check the number of spousal pairs
table(as.data.frame(table(death3$householdID, exclude = NULL))$Freq, exclude=NULL) 

#Check frequency of rwiwstat, values 5 and 6 indicating death, and value 9 indicating 'don't know alive or died'
table(hshare_all_sp4$r1iwstat, exclude=NULL) #No death indicators
table(hshare_all_sp4$r2iwstat, exclude=NULL) #No death indicators
table(hshare_all_sp4$r4iwstat, exclude=NULL) #313 individuals died in wave 4, 232 individual died in wave 3, and 677 individuals with unknown death status
table(hshare_all_sp4$r5iwstat, exclude=NULL) #591 individuals died in wave 5, 545 individual died in waves 3 and 4, and 1505 individuals with unknown death status
table(hshare_all_sp4$r6iwstat, exclude=NULL) #910 individuals died in wave 6, 1136 individual died in waves 3-5, and 2561 individuals with unknown death status
table(hshare_all_sp4$r7iwstat, exclude=NULL) #1535 individuals died in wave 7, 2046 individual died in waves 3-6, and 4465 individuals with unknown death status
table(hshare_all_sp4$r8iwstat, exclude=NULL) #1744 individuals died in wave 8, 3581 individual died in waves 3-7, and 10644 individuals with unknown death status
table(hshare_all_sp4$r9iwstat, exclude=NULL) #1929 individuals died in wave 9, 5325 individual died in waves 3-8, and 15394 individuals with unknown death status

#Exclude 34 individuals died before interview, as well as their spouses
hshare_all_sp5 <- hshare_all_sp4 %>% filter(!householdID %in% death3$householdID) #61728 individuals and 30864 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hshare_all_sp5$householdID, exclude = NULL))$Freq, exclude=NULL) 

#Check among the 302 individuals died during follow-up, how many of them had arthritis 
#death2 <- helsa_eol %>% select(idauniq, radage, racod_e, ragcod)  
#sp <- helsa_all_sp27 %>% select(idauniq, householdID, arthritis, rage, ragender, inclusion_wave)
#death_arthritis <- death %>% left_join(.,sp,by="idauniq")
#table(death_arthritis$arthritis, exclude=NULL) #83 individuals with arthritis
######################################################
#5. Descriptive data of selected baseline and outcome variables in hshare_all_sp25
######################################################
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/SHARE')
load("hshare_all_sp25.rda")

#Convert haven:labelled variables into numeric variables 
hshare_all_sp25 <- hshare_all_sp25 %>%
  mutate(across(where(haven::is.labelled), as.numeric)) 

#Define categorical variables
hshare_all_sp25 <- hshare_all_sp25 %>%
  mutate(across(c(rmbmicat, rarelig_s, roccup, ramomeducl, radadeducl), as.factor))

#Select baseline and outcome variables and convert variables with have_labelled to factor variables
descriptive <- hshare_all_sp25 %>%
  select(rage, ragender, rabplace2, hrural, rarelig_s, rmcurln, raeducl, rmbmin, rmbmicat, rpact,  rdrinkb, rdrinkw, rsmokev, roccup, hincome_max, hincome_min, hkcnt, ramomeducl, radadeducl, rclstress, rshlt, radlfive, riadlza, rmobilsev, arthritis, hibpe, hibpe_dm, diabe, diabe_dm, cancre, lunge, lunge_dm, hearte, hearte_dm, stroke, psyche, psyche_dm, hchole, hchole_dm, catracte, parkine, ulcere, ulcere_dm)
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
    rabplace2 ~ "Birthplace",
    rarelig_s ~ "Religion",
    rmcurln ~ "Length of current marriage",
    raeducl ~ "Highest attained education level",
    rmbmin ~ "Measured BMI_cont",
    rmbmicat ~ "Measured BMI_cat",
    rpact ~ "Physcial activity",
    rdrinkw ~ "Drinks alcohol weekly ",
    rdrinkb ~ "Ever binge drinks",
    rsmokev ~ "Ever smoking",
    roccup ~ "Occupation",
    hincome_max ~ "Household income (take maximum value for inconsisteny)",
    hincome_min ~ "Household income (take minimum value for inconsisteny)",
    hkcnt ~ "Weekly contact with children",
    ramomeducl ~ "Maternal education",
    radadeducl ~ "Paternal education",
    rclstress ~ "Childhood/lifetime stressful events (four vars)",
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
    lunge ~ "Doctor-diagnosed lung disease",
    lunge_dm ~ "Doctor-diagnosed/medication-indicated lung disease",
    hearte ~ "Doctor-diagnosed heart disease",
    hearte_dm ~ "Doctor-diagnosed/medication-indicated heart disease",
    stroke ~ "Doctor-diagnosed stroke",
    psyche ~ "Doctor-diagnosed psychiatric problems",
    psyche_dm ~ "Doctor-diagnosed/medication-indicated psychiatric problems",
    hchole ~ "Doctor-diagnosed high cholesterol",
    hchole_dm ~ "Doctor-diagnosed/medication-indicated high cholesterol",
    catracte ~ "Doctor-diagnosed catracts",
    parkine ~ "Doctor-diagnosed Parkinson's disease",
    ulcere ~ "Doctor-diagnosed ulcer",
    ulcere_dm ~ "Doctor-diagnosed/medication-indicated ulcer")) %>%
  # Additional formatting
  add_p() %>%  # Add p-values for group comparisons
  add_overall() %>%  # Add overall column
  bold_labels() %>%
  italicize_levels()

#Save descriptive table
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/SHARE')
write_xlsx(descriptive_base_var_by_gender[["table_body"]], path = "share_descriptive_base_out_var_by_gender.xlsx", col_names=T, format_headers=T)
######################################################
#DATA PREPARATION
#1. List of included variables  
#2. Define outcomes and potential confounders to be adjusted in hshare_all_sp5
###################################################### 
#1. List of included variables  (to be updated)
###################################################### 
#Load data and select relevant variables
#Load harmonized ELSA data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/SHARE/Waves/GH_SHARE_g_rel9-0-0_ALL_datasets_stata')
hshare <- read_dta("GH_SHARE_g.dta") %>% dplyr::select(1, 2:12, 22:31, 52:61, 71:122, 145:152, 162:169, 188:195, 244:251, 276:327, 344:352, 398:406, 416:431, 464:488, 507:546, 1063:1110, 1255:1302, 1423:1470, 1807:2070, 2127:2397, 2424:2445, 2478:2525, 2690:2721, 2744:2759, 2774:2787, 2802:2845, 3054:3036, 3914:3915, 3918:3931, 3946:3947, 3950:3963, 4042:4050, 4058, 4060:4066, 4074:4091, 4170:4171, 4174:4187, 4202:4203, 4206:4219, 4234:4235, 4238:4251, 4266, 4268:4311, 4385:4392, 4561:4568, 4719:4734, 4747:4762, 4779:4810, 4927:4942, 5017:5032, 5063:5078, 6775:6850, 7183:7252)

# Check if labels exist as attributes
var_info <- sapply(hshare, function(x) {
  label <- attr(x, "label")
  ifelse(is.null(label), "", label)
})

# Extract value labels from attributes
val_info <- sapply(hshare, function(x) {
  labels <- attr(x, "labels")
  if(!is.null(labels)) {
    # Format as "1=Label1; 2=Label2; ..."
    paste(paste(labels, "=", names(labels)), collapse = "; ")
  } else {
    ""
  }
})

#Create a detailed table
hshare_variable_table <- data.frame(
  Variable = names(hshare),
  Label = sapply(var_info, function(x) ifelse(is.null(x), "", x)),
  Type = sapply(hshare, function(x) paste(class(x), collapse = ", ")),
  N_Missing = colSums(is.na(hshare)),
  Unique_Values = sapply(hshare, function(x) length(unique(na.omit(x)))),
  stringsAsFactors = FALSE
)

#Add value labels as a separate column
hshare_variable_table$Value_Labels <- sapply(names(hshare), function(var) {
  labels <- val_info[[var]]
  if(!is.null(labels)) {
    paste(paste0(names(labels), " (", labels, ")"), collapse = "; ")
  } else {
    ""
  }
})

#Save as excel file
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/SHARE/Basic information')
write.xlsx(hshare_variable_table, file = "hshare_selected_variable_table_251104.xlsx", colNames=T, format_headers=T)
###################################################### 
#2. Define outcomes and potential confounders to be adjusted in hshare_all_sp5
######################################################  
##Define doctor-diagnosed arthritis (outcome, cases/controls)
######################################################  
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 1 
hshare_all_sp5 <- hshare_all_sp5 %>% mutate(arthritis=case_when(
  inclusion_wave!="Wave 1"~ NA, 
  inclusion_wave=="Wave 1" & if_any(c(r1arthre, r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre), ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp5$arthritis, hshare_all_sp5$inclusion_wave, exclude=NULL) #For those included from wave 1, 6877 individuals with no arthritis across 8 waves and  4917 individuals with arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hshare_all_sp5 %>% filter(arthritis==0) %>% select("mergeid", "householdID","spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hshare_all_sp5 %>% filter(arthritis==1) %>% select("mergeid", "householdID","spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
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
hshare_all_sp6 <- hshare_all_sp5 %>% mutate(arthritis=case_when(
  inclusion_wave=="Wave 1" ~ arthritis,
  inclusion_wave!="Wave 2" ~ NA_real_,
  inclusion_wave=="Wave 2" & if_any(c(r2arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre), ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp6$arthritis, hshare_all_sp6$inclusion_wave, exclude=NULL) #For those included from wave 2, 3216 individuals with no arthritis 3030 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hshare_all_sp6 %>% filter(arthritis==0 & inclusion_wave=="Wave 2") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r2arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hshare_all_sp6 %>% filter(arthritis==1 & inclusion_wave=="Wave 2") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r2arthre, exclude=NULL)
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
hshare_all_sp7 <- hshare_all_sp6 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ arthritis,
  inclusion_wave!="Wave 4"  ~ NA_real_, 
  inclusion_wave=="Wave 4" & if_any(r4arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp7$arthritis, hshare_all_sp7$inclusion_wave, exclude=NULL) #For those included from wave 4, 9060 individuals with no arthritis and 7364 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hshare_all_sp7 %>% filter(arthritis==0 & inclusion_wave=="Wave 4") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hshare_all_sp7 %>% filter(arthritis==1 & inclusion_wave=="Wave 4") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
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
hshare_all_sp8 <- hshare_all_sp7 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2","Wave 4") ~ arthritis,
  inclusion_wave!="Wave 5" ~ NA_real_, 
  inclusion_wave=="Wave 5" & if_any(r5arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp8$arthritis, hshare_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 5, 6758 individuals with no arthritis and 4524 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hshare_all_sp8 %>% filter(arthritis==0 & inclusion_wave=="Wave 5") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hshare_all_sp8 %>% filter(arthritis==1 & inclusion_wave=="Wave 5") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 6
hshare_all_sp9 <- hshare_all_sp8 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ arthritis,
  inclusion_wave!="Wave 6" ~ NA_real_, 
  inclusion_wave=="Wave 6" & if_any(r6arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp9$arthritis, hshare_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 6, 4638 individuals with no arthritis and 2322 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hshare_all_sp9 %>% filter(arthritis==0 & inclusion_wave=="Wave 6") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hshare_all_sp9 %>% filter(arthritis==1 & inclusion_wave=="Wave 6") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 7
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 7
hshare_all_sp10 <- hshare_all_sp9 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ arthritis,
  inclusion_wave!="Wave 7" ~ NA_real_, 
  inclusion_wave=="Wave 7" & if_any(r7arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp10$arthritis, hshare_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 7, 7 individuals with no arthritis and 3 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hshare_all_sp10 %>% filter(arthritis==0 & inclusion_wave=="Wave 7") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hshare_all_sp10 %>% filter(arthritis==1 & inclusion_wave=="Wave 7") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 8
hshare_all_sp11 <- hshare_all_sp10 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ arthritis,
  inclusion_wave!="Wave 8" ~ NA_real_, 
  inclusion_wave=="Wave 8" & if_any(r8arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp11$arthritis, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 8, 6008 individuals with no arthritis and 3004 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hshare_all_sp11 %>% filter(arthritis==0 & inclusion_wave=="Wave 8") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hshare_all_sp11 %>% filter(arthritis==1 & inclusion_wave=="Wave 8") %>% select("mergeid", "householdID", "spousal_part_pattern", "arthritis", r1arthre, r2arthre, r4arthre:r9arthre)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
##Define doctor-diagnosed arthritis (exposure)
###################################################### 
hshare_all_sp11 <- hshare_all_sp11 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sarthritis = ifelse(person_num == 1, arthritis[2], arthritis[1])
  ) %>%
  ungroup()

#Check the frequency of sarthritis
table(hshare_all_sp11$sarthritis, exclude=NULL)

#Compare the proportion of men with arthritis who had wives affected by arthritis to those without
male <- hshare_all_sp11 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_arthritis = male$sarthritis
)
freq_table(freq) #61% versus 40%
chisq.test(male$arthritis, male$sarthritis) #Significant

#Compare the proportion of women with arthritis who had husbands affected by arthritis to those without
female <- hshare_all_sp11 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_arthritis = female$sarthritis
)
freq_table(freq2) #44% versus 25%
chisq.test(female$arthritis, female$sarthritis) #Significant
######################################################  
##Define doctor-diagnosed other chronic conditions
#Exclude asthma and osteoporosis as they were recorded only in waves 1-2 and 4, as well as kidney disease as it was recorded only in waves 6-9
#rwpsyche available from wave 2
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

hshare_all_sp11$missing_count_hibpe <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="hibpe")
hshare_all_sp11$missing_count_diabe <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="diabe")
hshare_all_sp11$missing_count_cancre <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="cancre")
hshare_all_sp11$missing_count_lunge <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="lunge")
hshare_all_sp11$missing_count_hearte <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="hearte")
hshare_all_sp11$missing_count_stroke <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="stroke")
hshare_all_sp11$missing_count_psyche <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="psyche")
hshare_all_sp11$missing_count_hchole <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="hchole")
hshare_all_sp11$missing_count_catracte <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="catracte")
hshare_all_sp11$missing_count_parkine <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="parkine")
hshare_all_sp11$missing_count_ulcere <- count_missing_cd_by_part(data=hshare_all_sp11, disease_suffix="ulcere")

#All indiviudals with maximum 1 missing values in other chronic diseases across waves
#The number of individuals with missing values and participating in two waves only
table(hshare_all_sp11$missing_count_hibpe, hshare_all_sp11$total_participations, exclude=NULL) #5 individuals participated in two waves with 1 missing value
table(hshare_all_sp11$missing_count_diabe, hshare_all_sp11$total_participations, exclude=NULL) #7 individuals participated in two waves with 1 missing value and 1 individual participated in three waves with 2 missing value
table(hshare_all_sp11$missing_count_cancre, hshare_all_sp11$total_participations, exclude=NULL) #8 individuals participated in two waves with 1 missing value and 1 individual participated in three waves with 2 missing value
table(hshare_all_sp11$missing_count_lunge, hshare_all_sp11$total_participations, exclude=NULL) #7 individuals participated in two waves with 1 missing value
table(hshare_all_sp11$missing_count_hearte, hshare_all_sp11$total_participations, exclude=NULL) #5 individuals participated in two waves with 1 missing value
table(hshare_all_sp11$missing_count_stroke, hshare_all_sp11$total_participations, exclude=NULL) #6 individuals participated in two waves with 1 missing value and 1 individual participated in three waves with 2 missing value
table(hshare_all_sp11$missing_count_psyche, hshare_all_sp11$total_participations, exclude=NULL) #60 individuals participated in two waves with 1 missing value and 3 individuals participated in three waves with 2 missing value
table(hshare_all_sp11$missing_count_hchole, hshare_all_sp11$total_participations, exclude=NULL) #7 individuals participated in two waves with 1 missing value 
table(hshare_all_sp11$missing_count_catracte, hshare_all_sp11$total_participations, exclude=NULL) #8 individuals participated in two waves with 1 missing value and 1 individual participated in three waves with 2 missing value
table(hshare_all_sp11$missing_count_parkine, hshare_all_sp11$total_participations, exclude=NULL) #8 individuals participated in two waves with 1 missing value and 1 individual participated in three waves with 2 missing value
table(hshare_all_sp11$missing_count_ulcere, hshare_all_sp11$total_participations, exclude=NULL) #6 individuals participated in two waves with 1 missing value and 1 individual participated in three waves with 2 missing value

#Mark those exceeds the upper limit of missing values
hshare_all_sp11 <- hshare_all_sp11 %>% mutate(
  hibpe_exclude=case_when(
    missing_count_hibpe==1 & total_participations==2 ~ 1,
    TRUE ~ 0), 
  diabe_exclude=case_when(
    missing_count_diabe==1 & total_participations==2 ~ 1,
    missing_count_diabe==2 & total_participations==3 ~ 1,
    TRUE ~ 0),
  cancre_exclude=case_when(
    missing_count_cancre==1 & total_participations==2 ~ 1,
    missing_count_cancre==2 & total_participations==3 ~ 1,
    TRUE ~ 0),
  lunge_exclude=case_when(
    missing_count_lunge==1 & total_participations==2 ~ 1,
    TRUE ~ 0),
  hearte_exclude=case_when(
    missing_count_hearte==1 & total_participations==2 ~ 1,
    TRUE ~ 0),
  stroke_exclude=case_when(
    missing_count_stroke==1 & total_participations==2 ~ 1,
    missing_count_stroke==2 & total_participations==3 ~ 1,
    TRUE ~ 0),
  psyche_exclude=case_when(
    missing_count_psyche==1 & total_participations==2 ~ 1,
    missing_count_psyche==2 & total_participations==3 ~ 1,
    TRUE ~ 0),
  hchole_exclude=case_when(
    missing_count_hchole==1 & total_participations==2 ~ 1,
    TRUE ~ 0),
  catracte_exclude=case_when(
    missing_count_catracte==1 & total_participations==2 ~ 1,
    missing_count_catracte==2 & total_participations==3 ~ 1,
    TRUE ~ 0),
  parkine_exclude=case_when(
    missing_count_parkine==1 & total_participations==2 ~ 1,
    missing_count_parkine==2 & total_participations==3 ~ 1,
    TRUE ~ 0),
  ulcere_exclude=case_when(
    missing_count_ulcere==1 & total_participations==2 ~ 1,
    missing_count_ulcere==2 & total_participations==3 ~ 1,
    TRUE ~ 0))
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
hshare_all_sp11 <- hshare_all_sp11 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave!="Wave 1"~ NA_real_,
  inclusion_wave=="Wave 1" & if_any(c(r1hibpe, r2hibpe, r4hibpe, r5hibpe, r6hibpe, r7hibpe, r8hibpe, r9hibpe), ~ .x==1) ~ 1, 
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1diabe, r2diabe, r4diabe, r5diabe, r6diabe, r7diabe, r8diabe, r9diabe), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1cancre, r2cancre, r4cancre, r5cancre, r6cancre, r7cancre, r8cancre, r9cancre), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  lunge=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1lunge, r2lunge, r4lunge, r5lunge, r6lunge, r7lunge, r8lunge, r9lunge), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  hearte=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1hearte, r2hearte, r4hearte, r5hearte, r6hearte, r7hearte, r8hearte,r9hearte), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1stroke, r2stroke, r4stroke, r5stroke, r6stroke, r7stroke, r8stroke, r9stroke), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  psyche=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r2psyche, r4psyche, r5psyche, r6psyche, r7psyche, r8psyche, r9psyche), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  hchole=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1hchole,r2hchole, r4hchole, r5hchole, r6hchole, r7hchole, r8hchole, r9hchole), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1catracte, r2catracte, r4catracte, r5catracte, r6catracte, r7catracte, r8catracte, r9catracte), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  parkine=case_when(
    parkine_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1parkine, r2parkine, r4parkine, r5parkine, r6parkine, r7parkine, r8parkine, r9parkine), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  ulcere=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1ulcere, r2ulcere, r4ulcere, r5ulcere, r6ulcere, r7ulcere,  r8ulcere,  r9ulcere), ~ .x==1) ~ 1, 
    TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp11$hibpe, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5038 individuals with no high blood pressure/hypertension across 8 waves and 6752 individuals with high blood pressure/hypertension, 4 individuals with NA
table(hshare_all_sp11$diabe, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 9390 individuals with no diabetes across 8 waves and 2399 individuals with diabetes, 5 individuals with NA
table(hshare_all_sp11$cancre, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 10054 individuals with no cancer across 8 waves and 1735 individuals with cancer, 5 individuals with NA
table(hshare_all_sp11$lunge, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 10383 individuals with no lung diseases across 8 waves and 1407 individuals with lung diseases, 4 individuals with NA
table(hshare_all_sp11$hearte, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 8875 individuals with no heart diseases across 8 waves and 2916 individuals with heart diseases, 3 individuals with NA
table(hshare_all_sp11$stroke, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 10660 individuals with no stroke across 8 waves and 1130 individuals with stroke, 4 individuals with NA
table(hshare_all_sp11$psyche, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 10200 individuals with no psychiatric conditions across 8 waves and 1569 individuals with psychiatric conditions, 25 individuals with NA
table(hshare_all_sp11$hchole, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 6414 individuals with no high cholesterol across 8 waves and 5377 individuals with high cholesterol, 3 individuals with NA
table(hshare_all_sp11$catracte, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 9451 individuals with no catracts across 8 waves and 2338 individuals with catracts, 5 individuals with NA
table(hshare_all_sp11$parkine, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 11599 individuals with no Parkinson's disease across 8 waves and 190 individuals with Parkinson's disease, 5 individuals with NA
table(hshare_all_sp11$ulcere, hshare_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 10488 individuals with no osteoporosis across 8 waves and 1301 individuals with ulcer, 5 individuals with NA
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2
hshare_all_sp12 <- hshare_all_sp11 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave=="Wave 1" ~ hibpe,
  inclusion_wave!="Wave 2" ~ NA_real_, 
  inclusion_wave=="Wave 2" & if_any(c(r2hibpe,r4hibpe,r5hibpe, r6hibpe, r7hibpe, r8hibpe,r9hibpe), ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ diabe,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2diabe,r4diabe, r5diabe,r6diabe,r7diabe,r8diabe,r9diabe), ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ cancre,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2cancre, r4cancre,r5cancre,r6cancre,r7cancre, r8cancre,r9cancre), ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ lunge,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2lunge, r4lunge,r5lunge,r6lunge,r7lunge,r8lunge,r9lunge), ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ hearte,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2hearte, r4hearte,r5hearte, r6hearte, r7hearte, r8hearte,r9hearte), ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ stroke,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2stroke, r4stroke, r5stroke, r6stroke, r7stroke, r8stroke, r9stroke), ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ psyche,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2psyche, r4psyche, r5psyche, r6psyche, r7psyche, r8psyche, r9psyche), ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ hchole,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2hchole, r4hchole, r5hchole, r6hchole, r7hchole, r8hchole, r9hchole), ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ catracte,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2catracte, r4catracte, r5catracte, r6catracte, r7catracte, r8catracte, r9catracte), ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    parkine_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ parkine,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2parkine, r4parkine, r5parkine, r6parkine, r7parkine, r8parkine, r9parkine), ~ .x==1) ~ 1,
    TRUE ~ 0),
  ulcere=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ ulcere,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2ulcere, r4ulcere, r5ulcere, r6ulcere, r7ulcere, r8ulcere, r9ulcere ), ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp12$hibpe, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 2406 individuals with no high blood pressure/hypertension across 8 waves and 3839 individuals with high blood pressure/hypertension, 1 individual with NA
table(hshare_all_sp12$diabe, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 4871 individuals with no diabetes across 8 waves and 1373 individuals with diabetes, 2 individuals with NA
table(hshare_all_sp12$cancre, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 5360 individuals with no cancer across 8 waves and 884 individuals with cancer, 2 individuals with NA
table(hshare_all_sp12$lunge, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 5363 individuals with no lung diseases across 8 waves and 881 individuals with lung diseases, 2 individuals with NA
table(hshare_all_sp12$hearte, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 4636 individuals with no heart diseases across 8 waves and 1609 individuals with heart diseases, 1 individual with NA
table(hshare_all_sp12$stroke, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 5607 individuals with no stroke across 8 waves and 638 individuals with stroke, 1 individual with NA
table(hshare_all_sp12$psyche, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 5159 individuals with no psychiatric conditions across 8 waves and 1072 individuals with psychiatric conditions, 15 individuals with NA
table(hshare_all_sp12$hchole, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 3184 individuals with no high cholesterol across 8 waves and 3060 individuals with high cholesterol, 2 individuals with NA
table(hshare_all_sp12$catracte, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 4873 individuals with no catracts across 8 waves and 1371 individuals with catracts, 2 individuals with NA
table(hshare_all_sp12$parkine, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 6143 individuals with no Parkinson's disease across 8 waves and 101 individuals with Parkinson's disease, 2 individuals with NA
table(hshare_all_sp12$ulcere, hshare_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 5533 individuals with no osteoporosis across 8 waves and 712 individuals with ulcer
######################################################
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 4
hshare_all_sp13 <- hshare_all_sp12 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ hibpe,
  inclusion_wave!="Wave 4" ~ NA_real_, 
  inclusion_wave=="Wave 4" & if_any(r4hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ diabe,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ cancre,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ lunge,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hearte,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ stroke,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ psyche,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hchole,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ catracte,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    parkine_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ parkine,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  ulcere=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ ulcere,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4ulcere:r9ulcere, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp13$hibpe, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 6677 individuals with no high blood pressure/hypertension across 8 waves and 9747 individuals with high blood pressure/hypertension
table(hshare_all_sp13$diabe, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 13013 individuals with no diabetes across 8 waves and 3411 individuals with diabetes
table(hshare_all_sp13$cancre, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 14248 individuals with no cancer across 8 waves and 2175 individuals with cancer
table(hshare_all_sp13$lunge, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 14358 individuals with no lung diseases across 8 waves and 2065 individuals with lung diseases
table(hshare_all_sp13$hearte, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 12304 individuals with no heart diseases across 8 waves and 4119 individuals with heart diseases
table(hshare_all_sp13$stroke, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 14853 individuals with no stroke across 8 waves and 1570 individuals with stroke
table(hshare_all_sp13$psyche, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 13569 individuals with no psychiatric conditions across 8 waves and 2834 individuals with psychiatric conditions
table(hshare_all_sp13$hchole, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 9161 individuals with no high cholesterol across 8 waves and 7262 individuals with high cholesterol
table(hshare_all_sp13$catracte, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 13432 individuals with no catracts across 8 waves and 2991 individuals with catracts
table(hshare_all_sp13$parkine, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 16177 individuals with no Parkinson's disease across 8 waves and 246 individuals with Parkinson's disease
table(hshare_all_sp13$ulcere, hshare_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 4, 14469 individuals with no osteoporosis across 8 waves and 1954 individuals with osteoporosis
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 5
hshare_all_sp14 <- hshare_all_sp13 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ hibpe,
  inclusion_wave!="Wave 5" ~ NA_real_, 
  inclusion_wave=="Wave 5" & if_any(r5hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ diabe,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ cancre,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ lunge,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ hearte,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ stroke,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ psyche,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ hchole,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ catracte,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    parkine_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ parkine,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  ulcere=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ ulcere,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5ulcere:r9ulcere, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hshare_all_sp14$hibpe, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 5190 individuals with no high blood pressure/hypertension across 8 waves and 6092 individuals with high blood pressure/hypertension
table(hshare_all_sp14$diabe, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 9340 individuals with no diabetes across 8 waves and 1942 individuals with diabetes
table(hshare_all_sp14$cancre, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 9673 individuals with no cancer across 8 waves and 1609 individuals with cancer
table(hshare_all_sp14$lunge, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 10059 individuals with no lung diseases across 8 waves and 1223 individuals with lung diseases
table(hshare_all_sp14$hearte, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 9152 individuals with no heart diseases across 8 waves and 2130 individuals with heart diseases
table(hshare_all_sp14$stroke, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 10496 individuals with no stroke across 8 waves and 786 individuals with stroke
table(hshare_all_sp14$psyche, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 9908 individuals with no psychiatric conditions across 8 waves and 1373 individuals with psychiatric conditions
table(hshare_all_sp14$hchole, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 6846 individuals with no high cholesterol across 8 waves and 4436 individuals with high cholesterol
table(hshare_all_sp14$catracte, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 9343 individuals with no catracts across 8 waves and 1939 individuals with catracts
table(hshare_all_sp14$parkine, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 11154 individuals with no Parkinson's disease across 8 waves and 128 individuals with Parkinson's disease
table(hshare_all_sp14$ulcere, hshare_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 5, 10395 individuals with no ulcer across 8 waves and 887 individuals with ulcer
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 6
hshare_all_sp15 <- hshare_all_sp14 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ hibpe,
  inclusion_wave!="Wave 6" ~ NA_real_, 
  inclusion_wave=="Wave 6" & if_any(r6hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ diabe,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ cancre,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ lunge,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ hearte,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ stroke,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ psyche,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ hchole,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ catracte,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    parkine_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ parkine,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  ulcere=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ ulcere,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6ulcere:r9ulcere, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp15$hibpe, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 3033 individuals with no high blood pressure/hypertension across 8 waves and 3927 individuals with high blood pressure/hypertension
table(hshare_all_sp15$diabe, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 5709 individuals with no diabetes across 8 waves and 1251 individuals with diabetes
table(hshare_all_sp15$cancre, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 6230 individuals with no cancer across 8 waves and 730 individuals with cancer
table(hshare_all_sp15$lunge, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 6369 individuals with no lung diseases across 8 waves and 591 individuals with lung diseases
table(hshare_all_sp15$hearte, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 5726 individuals with no heart diseases across 8 waves and 1234 individuals with heart diseases
table(hshare_all_sp15$stroke, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 6536 individuals with no stroke across 8 waves and 424 individuals with stroke
table(hshare_all_sp15$psyche, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 6142 individuals with no psychiatric conditions across 8 waves and 818 individuals with psychiatric conditions
table(hshare_all_sp15$hchole, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 4077 individuals with no high cholesterol across 8 waves and 2883 individuals with high cholesterol
table(hshare_all_sp15$catracte, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 6204 individuals with no catracts across 8 waves and 756 individuals with catracts
table(hshare_all_sp15$parkine, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 6894 individuals with no Parkinson's disease across 8 waves and 66 individuals with Parkinson's disease
table(hshare_all_sp15$ulcere, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 6, 6358 individuals with no ulcer across 8 waves and 602 individuals with ulcer
######################################################
#Inclusion in Wave 7
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 7
hshare_all_sp16 <- hshare_all_sp15 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ hibpe,
  inclusion_wave!="Wave 7" ~ NA_real_, 
  inclusion_wave=="Wave 7" & if_any(r7hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ diabe,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ cancre,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ lunge,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ hearte,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ stroke,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ psyche,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ hchole,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ catracte,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    parkine_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ parkine,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  ulcere=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ ulcere,
    inclusion_wave!="Wave 7" ~ NA_real_, 
    inclusion_wave=="Wave 7" & if_any(r7ulcere:r9ulcere, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp16$hibpe, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 3 individuals with no high blood pressure/hypertension across 8 waves and 7 individuals with high blood pressure/hypertension
table(hshare_all_sp16$diabe, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 6 individuals with no diabetes across 8 waves and 4 individuals with diabetes
table(hshare_all_sp16$cancre, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 9 individuals with no cancer across 8 waves and 1 individuals with cancer
table(hshare_all_sp16$lunge, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 10 individuals with no lung diseases across 8 waves and 0 individuals with lung diseases
table(hshare_all_sp16$hearte, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 6 individuals with no heart diseases across 8 waves and 4 individuals with heart diseases
table(hshare_all_sp16$stroke, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 8 individuals with no stroke across 8 waves and 2 individuals with stroke
table(hshare_all_sp16$psyche, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 9 individuals with no psychiatric conditions across 8 waves and 1 individuals with psychiatric conditions
table(hshare_all_sp16$hchole, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 6 individuals with no high cholesterol across 8 waves and 4 individuals with high cholesterol
table(hshare_all_sp16$catracte, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 10 individuals with no catracts across 8 waves and 0 individuals with catracts
table(hshare_all_sp16$parkine, hshare_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 7, 10 individuals with no Parkinson's disease across 8 waves and 0 individuals with Parkinson's disease
table(hshare_all_sp16$ulcere, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 7, 9 individuals with no ulcer across 8 waves and 1 individuals with ulcer
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 8
hshare_all_sp17 <- hshare_all_sp16 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hibpe,
  inclusion_wave!="Wave 8" ~ NA_real_, 
  inclusion_wave=="Wave 8" & if_any(r8hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ diabe,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ cancre,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ lunge,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hearte,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ stroke,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ psyche,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hchole=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hchole,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8hchole:r9hchole, ~ .x==1) ~ 1,
    TRUE ~ 0),
  catracte=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ catracte,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8catracte:r9catracte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  parkine=case_when(
    parkine_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ parkine,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8parkine:r9parkine, ~ .x==1) ~ 1,
    TRUE ~ 0),
  ulcere=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ ulcere,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8ulcere:r9ulcere, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hshare_all_sp17$hibpe, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 3904 individuals with no high blood pressure/hypertension across 8 waves and 5108 individuals with high blood pressure/hypertension
table(hshare_all_sp17$diabe, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 7443 individuals with no diabetes across 8 waves and 1568 individuals with diabetes, 1 individual with NA
table(hshare_all_sp17$cancre, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 8185 individuals with no cancer across 8 waves and 826 individuals with cancer, 1 individual with NA
table(hshare_all_sp17$lunge, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 8306 individuals with no lung diseases across 8 waves and 706 individuals with lung diseases
table(hshare_all_sp17$hearte, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 7302 individuals with no heart diseases across 8 waves and 1710 individuals with heart diseases
table(hshare_all_sp17$stroke, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 8410 individuals with no stroke across 8 waves and 601 individuals with stroke, 1 individual with NA
table(hshare_all_sp17$psyche, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 8318 individuals with no psychiatric conditions across 8 waves and 693 individuals with psychiatric conditions, 1 individual with NA
table(hshare_all_sp17$hchole, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 6223 individuals with no high cholesterol across 8 waves and 2788 individuals with high cholesterol, 1 individual with NA
table(hshare_all_sp17$catracte, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 8287 individuals with no catracts across 8 waves and 724 individuals with catracts, 1 individual with NA
table(hshare_all_sp17$parkine, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 8, 8952 individuals with no Parkinson's disease across 8 waves and 59 individuals with Parkinson's disease, 1 individual with NA
table(hshare_all_sp17$ulcere, hshare_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 8378 individuals with no ulcer across 8 waves and 634 individuals with ulcer

######################################################
##Define doctor-diagnosed other chronic conditions among spousal pairs
###################################################### 
hshare_all_sp17 <- hshare_all_sp17 %>%
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
    scatracte = ifelse(person_num == 1, catracte[2], catracte[1]),
    sparkine = ifelse(person_num == 1, parkine[2], parkine[1]),
    sulcere = ifelse(person_num == 1, ulcere[2], ulcere[1])
  ) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(hshare_all_sp17$shibpe, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$sdiabe, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$scancre, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$slunge, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$shearte, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$sstroke, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$spsyche, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$shchole, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$scatracte, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$sparkine, hshare_all_sp17$inclusion_wave, exclude=NULL)
table(hshare_all_sp17$sulcere, hshare_all_sp17$inclusion_wave, exclude=NULL)

##Compare the proportion of men with arthritis who had wives affected by other chronic conditions to those without
#High blood pressure/hypertension
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hibpe = male$shibpe
)
freq_table(freq) #61% versus 51%
chisq.test(male$arthritis, male$shibpe) #Significant

#High blood sugar/diabetes
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_diabe = male$sdiabe
)
freq_table(freq) #20% versus 15%
chisq.test(male$arthritis, male$sdiabe) #Significant

#Cancer
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_cancre = male$scancre
)
freq_table(freq) #14% versus 11%
chisq.test(male$arthritis, male$scancre) #Non-significant

#Lung diseases
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_lunge = male$slunge
)
freq_table(freq) #13% versus 8%
chisq.test(male$arthritis, male$slunge) #Significant

#Heart diseases
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hearte = male$shearte
)
freq_table(freq) #21% versus 15%
chisq.test(male$arthritis, male$shearte) #Significant

#Stroke
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_stroke= male$sstroke
)
freq_table(freq) #8% versus 5%
chisq.test(male$arthritis, male$sstroke) #Significant

#Psychiatric conditions
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_psyche= male$spsyche
)
freq_table(freq) #%22 versus 16%
chisq.test(male$arthritis, male$spsyche) #Significant

#High cholesterol 
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hchole= male$shchole
)
freq_table(freq) #%46 versus 38%
chisq.test(male$arthritis, male$shchole) #Significant

#Catracts
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_livere= male$scatracte
)
freq_table(freq) #21% versus 14%
chisq.test(male$arthritis, male$scatracte) #Significant

#Parkinson's diseases
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_kidneye= male$sparkine
)
freq_table(freq) #1.3% versus 0.9%
chisq.test(male$arthritis, male$sparkine) #Significant

#Ulcer 
male <- hshare_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_digeste= male$ulcere
)
freq_table(freq) #15% versus 8%
chisq.test(male$arthritis, male$ulcere) #Significant

##Compare the proportion of women with arthritis who had husbands affected by other chronic conditions to those without
#High blood pressure/hypertension
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hibpe = female$shibpe
)
freq_table(freq2) #65% versus 56%
chisq.test(female$arthritis, female$shibpe) #Significant

#High blood sugar/diabetes
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_diabe = female$sdiabe
)
freq_table(freq2) #25% versus 19%
chisq.test(female$arthritis, female$sdiabe) #Significant

#Cancer
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_cancre = female$scancre
)
freq_table(freq2) #16% versus 12%
chisq.test(female$arthritis, female$scancre) #Significant

#Lung diseases
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_lunge = female$slunge
)
freq_table(freq2) #15% versus 9%
chisq.test(female$arthritis, female$slunge) #Significant

#Heart diseases
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hearte = female$shearte
)
freq_table(freq2) #33% versus 23%
chisq.test(female$arthritis, female$shearte) #Significant

#Stroke
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_stroke= female$sstroke
)
freq_table(freq2) #13% versus 9%
chisq.test(female$arthritis, female$sstroke) #Significant

#Psychiatric conditions
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_psyche= female$spsyche
)
freq_table(freq2) #11% versus 8%
chisq.test(female$arthritis, female$spsyche) #Significant

#High cholesterol 
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hchole= female$shchole
)
freq_table(freq2) #47% versus 39%
chisq.test(female$arthritis, female$shchole) #Significant

#Catracts
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_livere= female$scatracte
)
freq_table(freq2) #20% versus 13%
chisq.test(female$arthritis, female$scatracte) #Significant

#Parkinson's diseases
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_kidneye= female$sparkine
)
freq_table(freq2) #19% versus 12%
chisq.test(female$arthritis, female$sparkine) #Significant

#Ulcer
female <- hshare_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_digeste= female$ulcere
)
freq_table(freq2) #14% versus 5%
chisq.test(female$arthritis, female$ulcere) #Significant
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions 
#Requiring individuals not exceeding the upper limit missing values of doctor-diagnosed variables only as operator "OR" not "AND" was used
#The number of NA becames smaller when including medication variables to define other chronic conditions, which is unexpected and should use the other chronic condition variables defined by doctor-diagnosed only to refine rows with NA
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
hshare_all_sp17 <- hshare_all_sp17 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave!="Wave 1"~ NA_real_,
  inclusion_wave=="Wave 1" & (if_any(c(r1hibpe, r2hibpe, r4hibpe, r5hibpe, r6hibpe, r7hibpe, r8hibpe, r9hibpe), ~ .x==1) | if_any(c(r1rxhibp, r2rxhibp, r4rxhibp, r5rxhibp, r6rxhibp, r7rxhibp, r8rxhibp, r9rxhibp), ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(c(r1diabe, r2diabe, r4diabe, r5diabe, r6diabe, r7diabe, r8diabe, r9diabe), ~ .x==1) | if_any(c(r1rxdiab, r2rxdiab, r4rxdiab, r5rxdiab, r6rxdiab, r7rxdiab, r8rxdiab, r9rxdiab), ~ .x==1)) ~ 1,
    TRUE ~ 0),
  hearte_dm=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(c(r1hearte, r2hearte, r4hearte, r5hearte, r6hearte, r7hearte, r8hearte, r9hearte), ~ .x==1) | if_any(c(r1rxheart, r2rxheart,  r4rxheart, r5rxheart, r6rxheart, r7rxheart, r8rxheart, r9rxheart), ~ .x==1)) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(c(r1lunge, r2lunge, r4lunge, r5lunge, r6lunge, r7lunge, r8lunge, r9lunge), ~ .x==1) | if_any(c(r1rxlung, r2rxlung,  r4rxlung, r5rxlung, r6rxlung, r7rxlung, r8rxlung, r9rxlung), ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  psyche_dm=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(c(r2psyche, r4psyche, r5psyche, r6psyche, r7psyche, r8psyche, r9psyche), ~ .x==1) | if_any(c(r1rxpsych, r2rxpsych, r4rxpsych, r5rxpsych, r6rxpsych, r7rxpsych, r8rxpsych, r9rxpsych), ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(c(r1hchole, r2hchole, r4hchole, r5hchole, r6hchole, r7hchole, r8hchole, r9hchole), ~ .x==1) | if_any(c(r1rxhchol, r2rxhchol, r4rxhchol, r5rxhchol, r6rxhchol, r7rxhchol, r8rxhchol, r9rxhchol), ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  ulcere_dm=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(c(r1ulcere, r2ulcere, r4ulcere, r5ulcere, r6ulcere, r7ulcere, r8ulcere, r9ulcere), ~ .x==1) | if_any(c(r1rxulcer, r2rxulcer, r4rxulcer, r5rxulcer, r6rxulcer, r7rxulcer, r8rxulcer, r9rxulcer), ~ .x==1)) ~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hshare_all_sp17$hibpe_dm, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 4524 individuals with no high blood pressure/hypertension across 8 waves and 7266 individuals with high blood pressure/hypertension, 4 individuals with NA
table(hshare_all_sp17$diabe_dm, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 9269 individuals with no diabetes across 8 waves and 2520 individuals with diabetes, 5 individuals with NA
table(hshare_all_sp17$lunge_dm, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 10315 individuals with no lung diseases across 8 waves and 1475 individuals with lung diseases, 4 individuals with NA
table(hshare_all_sp17$psyche_dm, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 9617 individuals with no psychiatric conditions across 8 waves and 2152 individuals with psychiatric conditions, 25 individuals with NA
table(hshare_all_sp17$hchole_dm, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 5733 individuals with no high cholesterol across 8 waves and 6058 individuals with high cholesterol, 3 individuals with NA
table(hshare_all_sp17$ulcere_dm, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 8929 individuals with no ulcer across 8 waves and 2860 individuals with ulcer, 5 individuals with NA
table(hshare_all_sp17$hearte_dm, hshare_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 7617 individuals with no heart disease across 8 waves and 4174 individuals with heart disease, 3 individuals with NA
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2 
hshare_all_sp18 <- hshare_all_sp17 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave=="Wave 1" ~ hibpe_dm,
  inclusion_wave!="Wave 2"~ NA_real_,
  inclusion_wave=="Wave 2" & (if_any(c(r2hibpe, r4hibpe, r5hibpe, r6hibpe, r7hibpe, r8hibpe, r9hibpe), ~ .x==1) | if_any(c(r2rxhibp, r4rxhibp, r5rxhibp, r6rxhibp, r7rxhibp, r8rxhibp, r9rxhibp), ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ diabe_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(c(r2diabe, r4diabe, r5diabe, r6diabe, r7diabe, r8diabe, r9diabe), ~ .x==1) | if_any(c(r2rxdiab, r4rxdiab, r5rxdiab, r6rxdiab, r7rxdiab, r8rxdiab, r9rxdiab), ~ .x==1)) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ lunge_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(c(r2lunge, r4lunge, r5lunge, r6lunge, r7lunge, r8lunge, r9lunge), ~ .x==1) | if_any(c(r2rxlung,  r4rxlung, r5rxlung, r6rxlung, r7rxlung, r8rxlung, r9rxlung), ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  psyche_dm=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ psyche_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(c(r2psyche, r4psyche, r5psyche, r6psyche, r7psyche, r8psyche, r9psyche), ~ .x==1) | if_any(c(r2rxpsych, r4rxpsych, r5rxpsych, r6rxpsych, r7rxpsych, r8rxpsych, r9rxpsych), ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ hchole_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(c(r2hchole, r4hchole, r5hchole, r6hchole, r7hchole, r8hchole, r9hchole), ~ .x==1) | if_any(c(r2rxhchol, r4rxhchol, r5rxhchol, r6rxhchol, r7rxhchol, r8rxhchol, r9rxhchol), ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  hearte_dm=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ hearte_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(c(r2hearte, r4hearte, r5hearte, r6hearte, r7hearte, r8hearte, r9hearte), ~ .x==1) | if_any(c(r2rxheart,  r4rxheart, r5rxheart, r6rxheart, r7rxheart, r8rxheart, r9rxheart), ~ .x==1)) ~ 1,
    TRUE ~ 0),
  ulcere_dm=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ ulcere_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(c(r2ulcere, r4ulcere, r5ulcere, r6ulcere, r7ulcere, r8ulcere, r9ulcere), ~ .x==1) | if_any(c(r2rxulcer, r4rxulcer, r5rxulcer, r6rxulcer, r7rxulcer, r8rxulcer, r9rxulcer), ~ .x==1)) ~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hshare_all_sp18$hibpe_dm, hshare_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2,  2184 individuals with no high blood pressure/hypertension across 8 waves and 4061 individuals with high blood pressure/hypertension, 1 individuals with NA
table(hshare_all_sp18$diabe_dm, hshare_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 4806 individuals with no diabetes across 8 waves and 1438 individuals with diabetes, 2 individuals with NA
table(hshare_all_sp18$lunge_dm, hshare_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 5312 individuals with no lung diseases across 8 waves and 932 individuals with lung diseases, 2 individuals with NA
table(hshare_all_sp18$psyche_dm, hshare_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 4913 individuals with no psychiatric conditions across 8 waves and 1318 individuals with psychiatric conditions, 15 individuals with NA
table(hshare_all_sp18$hchole_dm, hshare_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 2821 individuals with no high cholesterol across 8 waves and 3423 individuals with high cholesterol, 2 individuals with NA
table(hshare_all_sp18$ulcere_dm, hshare_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 4600 individuals with no ucler across 8 waves and 1645 individuals with ucler, 1 individuals with NA
table(hshare_all_sp18$hearte_dm, hshare_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 3959 individuals with no heart disease across 8 waves and 2286 individuals with heart disease, 1 individuals with NA
######################################################
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 4
hshare_all_sp19 <- hshare_all_sp18 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ hibpe_dm,
  inclusion_wave!="Wave 4"~ NA_real_,
  inclusion_wave=="Wave 4" & (if_any(r4hibpe:r9hibpe, ~ .x==1) | if_any(r4rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ diabe_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4diabe:r9diabe, ~ .x==1) | if_any(r4rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hearte_dm=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hearte_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4hearte:r9hearte, ~ .x==1) | if_any(r4rxheart:r9rxheart, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ lunge_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4lunge:r9lunge, ~ .x==1) | if_any(r4rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ psyche_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4psyche:r9psyche, ~ .x==1) | if_any(r4rxpsych:r9rxpsych, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hchole_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4hchole:r9hchole, ~ .x==1) | if_any(r4rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  ulcere_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ ulcere_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4ulcere:r9ulcere, ~ .x==1) | if_any(r5rxulcer:r9rxulcer, ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hshare_all_sp19$hibpe_dm, hshare_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 4, 6063 individuals with no high blood pressure/hypertension across 8 waves and 10361 individuals with high blood pressure/hypertension
table(hshare_all_sp19$diabe_dm, hshare_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 4, 12864 individuals with no diabetes across 8 waves and 3560 individuals with diabetes
table(hshare_all_sp19$lunge_dm, hshare_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 4, 14214 individuals with no lung diseases across 8 waves and 2209 individuals with lung diseases, 1 individuals with NA
table(hshare_all_sp19$psyche_dm, hshare_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 4, 12945 individuals with no psychiatric conditions across 8 waves and 3458 individuals with psychiatric conditions, 21 individuals with NA
table(hshare_all_sp19$hchole_dm, hshare_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 4, 9257 individuals with no high cholesterol across 8 waves and 8166 individuals with high cholesterol, 1 individuals with NA
table(hshare_all_sp19$ulcere_dm, hshare_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 4, 12845 individuals with no ucler across 8 waves and 3579 individuals with ucler
table(hshare_all_sp19$hearte_dm, hshare_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 4, 10516 individuals with no heart disease across 8 waves and 5907 individuals with heart disease, 1 individuals with NA
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 5
hshare_all_sp20 <- hshare_all_sp19 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ hibpe_dm,
  inclusion_wave!="Wave 5"~ NA_real_,
  inclusion_wave=="Wave 5" & (if_any(r5hibpe:r9hibpe, ~ .x==1) | if_any(r5rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ diabe_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5diabe:r9diabe, ~ .x==1) | if_any(r5rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hearte_dm=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ hearte_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5hearte:r9hearte, ~ .x==1) | if_any(r5rxheart:r9rxheart, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ lunge_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5lunge:r9lunge, ~ .x==1) | if_any(r5rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ psyche_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5psyche:r9psyche, ~ .x==1) | if_any(r5rxpsych:r9rxpsych, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ hchole_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5hchole:r9hchole, ~ .x==1) | if_any(r5rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  ulcere_dm=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4") ~ ulcere_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5ulcere:r9ulcere, ~ .x==1) | if_any(r5rxulcer:r9rxulcer, ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hshare_all_sp20$hibpe_dm, hshare_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 5, 4762 individuals with no high blood pressure/hypertension across 8 waves and 6520 individuals with high blood pressure/hypertension
table(hshare_all_sp20$diabe_dm, hshare_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 5, 9261 individuals with no diabetes across 8 waves and 2021 individuals with diabetes
table(hshare_all_sp20$lunge_dm, hshare_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 5, 9964 individuals with no lung diseases across 8 waves and 1318 individuals with lung diseases
table(hshare_all_sp20$psyche_dm, hshare_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 5, 9508 individuals with no psychiatric conditions across 8 waves and 1773 individuals with psychiatric conditions, 1 individuals with NA
table(hshare_all_sp20$hchole_dm, hshare_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 5, 6247 individuals with no high cholesterol across 8 waves and 5035 individuals with high cholesterol, 
table(hshare_all_sp20$ulcere_dm, hshare_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 5, 8960 individuals with no ucler across 8 waves and 2322 individuals with ucler
table(hshare_all_sp20$hearte_dm, hshare_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 5, 8102 individuals with no heart disease across 8 waves and 3180 individuals with heart disease
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 6
hshare_all_sp21 <- hshare_all_sp20 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ hibpe_dm,
  inclusion_wave!="Wave 6"~ NA_real_,
  inclusion_wave=="Wave 6" & (if_any(r6hibpe:r9hibpe, ~ .x==1) | if_any(r6rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ diabe_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6diabe:r9diabe, ~ .x==1) | if_any(r6rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hearte_dm=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ hearte_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6hearte:r9hearte, ~ .x==1) | if_any(r6rxheart:r9rxheart, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ lunge_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6lunge:r9lunge, ~ .x==1) | if_any(r6rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ psyche_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6psyche:r9psyche, ~ .x==1) | if_any(r6rxpsych:r9rxpsych, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ hchole_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6hchole:r9hchole, ~ .x==1) | if_any(r6rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  ulcere_dm=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5") ~ ulcere_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6ulcere:r9ulcere, ~ .x==1) | if_any(r6rxulcer:r9rxulcer, ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hshare_all_sp21$hibpe_dm, hshare_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 6, 2817 individuals with no high blood pressure/hypertension across 8 waves and 4143 individuals with high blood pressure/hypertension
table(hshare_all_sp21$diabe_dm, hshare_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 6, 5663 individuals with no diabetes across 8 waves and 1297 individuals with diabetes
table(hshare_all_sp21$lunge_dm, hshare_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 6, 6328 individuals with no lung diseases across 8 waves and 632 individuals with lung diseases
table(hshare_all_sp21$psyche_dm, hshare_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 6, 5830 individuals with no psychiatric conditions across 8 waves and 1130 individuals with psychiatric conditions
table(hshare_all_sp21$hchole_dm, hshare_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 6, 3772 individuals with no high cholesterol across 8 waves and 3188 individuals with high cholesterol, 
table(hshare_all_sp21$ulcere_dm, hshare_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 6, 5700 individuals with no ucler across 8 waves and 1260 individuals with ucler
table(hshare_all_sp21$hearte_dm, hshare_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 6, 5116 individuals with no heart disease across 8 waves and 1844 individuals with heart disease
######################################################
#Inclusion in Wave 7
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 7
hshare_all_sp22 <- hshare_all_sp21 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ hibpe_dm,
  inclusion_wave!="Wave 7"~ NA_real_,
  inclusion_wave=="Wave 7" & (if_any(r7hibpe:r9hibpe, ~ .x==1) | if_any(r7rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ diabe_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7diabe:r9diabe, ~ .x==1) | if_any(r7rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hearte_dm=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ hearte_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7hearte:r9hearte, ~ .x==1) | if_any(r7rxheart:r9rxheart, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ lunge_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7lunge:r9lunge, ~ .x==1) | if_any(r7rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ psyche_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7psyche:r9psyche, ~ .x==1) | if_any(r7rxpsych:r9rxpsych, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ hchole_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7hchole:r9hchole, ~ .x==1) | if_any(r7rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  ulcere_dm=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6") ~ ulcere_dm,
    inclusion_wave!="Wave 7"~ NA_real_,
    inclusion_wave=="Wave 7" & (if_any(r7ulcere:r9ulcere, ~ .x==1) | if_any(r7rxulcer:r9rxulcer, ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hshare_all_sp22$hibpe_dm, hshare_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 7, 3 individuals with no high blood pressure/hypertension across 8 waves and 7 individuals with high blood pressure/hypertension
table(hshare_all_sp22$diabe_dm, hshare_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 7, 6 individuals with no diabetes across 8 waves and 4 individuals with diabetes
table(hshare_all_sp22$lunge_dm, hshare_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 7, 10 individuals with no lung diseases across 8 waves and 0 individuals with lung diseases
table(hshare_all_sp22$psyche_dm, hshare_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 7, 8 individuals with no psychiatric conditions across 8 waves and 2 individuals with psychiatric conditions, 1 individuals with NA
table(hshare_all_sp22$hchole_dm, hshare_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 7, 5 individuals with no high cholesterol across 8 waves and 5 individuals with high cholesterol, 
table(hshare_all_sp22$ulcere_dm, hshare_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 7, 8 individuals with no ucler across 8 waves and 2 individuals with ucler
table(hshare_all_sp22$hearte_dm, hshare_all_sp22$inclusion_wave, exclude=NULL) #For those included from wave 7, 5 individuals with no heart disease across 8 waves and 5 individuals with heart disease
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 8
hshare_all_sp23 <- hshare_all_sp22 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hibpe_dm,
  inclusion_wave!="Wave 8"~ NA_real_,
  inclusion_wave=="Wave 8" & (if_any(r8hibpe:r9hibpe, ~ .x==1) | if_any(r8rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ diabe_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8diabe:r9diabe, ~ .x==1) | if_any(r8rxdiab:r9rxdiab, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hearte_dm=case_when(
    hearte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hearte_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8hearte:r9hearte, ~ .x==1) | if_any(r8rxheart:r9rxheart, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  lunge_dm=case_when(
    lunge_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ lunge_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8lunge:r9lunge, ~ .x==1) | if_any(r8rxlung:r9rxlung, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  psyche_dm=case_when(
    psyche_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ psyche_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8psyche:r9psyche, ~ .x==1) | if_any(r8rxpsych:r9rxpsych, ~ .x==1)) ~ 1, 
    TRUE ~ 0),
  hchole_dm=case_when(
    hchole_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ hchole_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8hchole:r9hchole, ~ .x==1) | if_any(r8rxhchol:r9rxhchol, ~ .x==1))~ 1, 
    TRUE ~ 0),
  ulcere_dm=case_when(
    ulcere_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 4", "Wave 5", "Wave 6", "Wave 7") ~ ulcere_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8ulcere:r9ulcere, ~ .x==1) | if_any(r8rxulcer:r9rxulcer, ~ .x==1))~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hshare_all_sp23$hibpe_dm, hshare_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 8, 3698 individuals with no high blood pressure/hypertension across 8 waves and 5314 individuals with high blood pressure/hypertension
table(hshare_all_sp23$diabe_dm, hshare_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 8, 7396 individuals with no diabetes across 8 waves and 1615 individuals with diabetes, 1 individual with NA
table(hshare_all_sp23$lunge_dm, hshare_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 8, 8274 individuals with no lung diseases across 8 waves and 738 individuals with lung diseases
table(hshare_all_sp23$psyche_dm, hshare_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 8, 8100 individuals with no psychiatric conditions across 8 waves and 911 individuals with psychiatric conditions, 1 individuals with NA
table(hshare_all_sp23$hchole_dm, hshare_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 8, 5741 individuals with no high cholesterol across 8 waves and 3270 individuals with high cholesterol, 1 individual with NA
table(hshare_all_sp23$ulcere_dm, hshare_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 8, 7891 individuals with no ucler across 8 waves and 1121 individuals with ucler
table(hshare_all_sp23$hearte_dm, hshare_all_sp23$inclusion_wave, exclude=NULL) #For those included from wave 8, 6536 individuals with no heart disease across 8 waves and 2476 individuals with heart disease
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions for the paired spouses
###################################################### 
hshare_all_sp24 <- hshare_all_sp23 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    shibpe_dm = ifelse(person_num == 1, hibpe_dm[2], hibpe_dm[1]),
    sdiabe_dm = ifelse(person_num == 1, diabe_dm[2], diabe_dm[1]),  
    shearte_dm = ifelse(person_num == 1, hearte_dm[2], hearte_dm[1]),
    slunge_dm = ifelse(person_num == 1, lunge_dm[2], lunge_dm[1]),
    spsyche_dm = ifelse(person_num == 1, psyche_dm[2], psyche_dm[1]),
    shchole_dm = ifelse(person_num == 1, hchole_dm[2], hchole_dm[1]),
    sulcere_dm = ifelse(person_num == 1, ulcere_dm[2], ulcere_dm[1])
  ) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(hshare_all_sp24$shibpe_dm, hshare_all_sp24$inclusion_wave, exclude=NULL)
table(hshare_all_sp24$sdiabe_dm, hshare_all_sp24$inclusion_wave, exclude=NULL)
table(hshare_all_sp24$shearte_dm, hshare_all_sp24$inclusion_wave, exclude=NULL)
table(hshare_all_sp24$slunge_dm, hshare_all_sp24$inclusion_wave, exclude=NULL)
table(hshare_all_sp24$spsyche_dm, hshare_all_sp24$inclusion_wave, exclude=NULL)
table(hshare_all_sp24$shchole_dm, hshare_all_sp24$inclusion_wave, exclude=NULL)
table(hshare_all_sp24$sulcere_dm, hshare_all_sp24$inclusion_wave, exclude=NULL)

##Compare the proportion of men with arthritis who had wives affected by other chronic conditions to those without
#High blood pressure/hypertension
male <- hshare_all_sp24 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hibpe = male$shibpe_dm
)
freq_table(freq) #64% versus 54%
chisq.test(male$arthritis, male$shibpe_dm) #Significant

#High blood sugar/diabetes
male <- hshare_all_sp24 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_diabe = male$sdiabe_dm
)
freq_table(freq) #21% versus 16%
chisq.test(male$arthritis, male$sdiabe_dm) #Significant

#Heart disease
male <- hshare_all_sp24 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_cancre = male$shearte_dm
)
freq_table(freq) #33% versus 23%
chisq.test(male$arthritis, male$shearte_dm) #Significant

#Lung diseases
male <- hshare_all_sp24 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_lunge = male$slunge_dm
)
freq_table(freq) #14% versus 9%
chisq.test(male$arthritis, male$slunge_dm) #Significant

#Psychiatric conditions
male <- hshare_all_sp24 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_psyche= male$spsyche_dm
)
freq_table(freq) #27% versus 20%
chisq.test(male$arthritis, male$spsyche_dm) #Significant

#High cholesterol
male <- hshare_all_sp24 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hchole= male$shchole_dm
)
freq_table(freq) #51% versus 42%
chisq.test(male$arthritis, male$shchole_dm) #Significant

#Ulcer
male <- hshare_all_sp24 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_osteoe= male$sulcere_dm
)
freq_table(freq) #26% versus 19%
chisq.test(male$arthritis, male$sulcere_dm) #Significant

##Compare the proportion of women with arthritis who had husbands affected by other chronic conditions to those without
#High blood pressure/hypertension
female <- hshare_all_sp24 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hibpe = female$shibpe_dm
)
freq_table(freq2) #70% versus 60%
chisq.test(female$arthritis, female$shibpe_dm) #Significant

#High blood sugar/diabetes
female <- hshare_all_sp24 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_diabe = female$sdiabe_dm
)
freq_table(freq2) #26% versus 20%
chisq.test(female$arthritis, female$sdiabe_dm) #Significant

#Heart disease
female <- hshare_all_sp24 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_cancre = female$shearte_dm
)
freq_table(freq2) #44% versus 32%
chisq.test(female$arthritis, female$shearte_dm) #Significant

#Lung diseases
female <- hshare_all_sp24 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_lunge = female$slunge_dm
)
freq_table(freq2) #16% versus 10%
chisq.test(female$arthritis, female$slunge_dm) #Significant

#Psychiatric conditions
female <- hshare_all_sp24 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_psyche= female$spsyche_dm
)
freq_table(freq2) #15% versus 10%
chisq.test(female$arthritis, female$spsyche_dm) #Significant

#High cholesterol
female <- hshare_all_sp24 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hchole= female$shchole_dm
)
freq_table(freq2) #54% versus 45%
chisq.test(female$arthritis, female$shchole_dm) #Significant

#Ulcer
female <- hshare_all_sp24 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_ulcere= female$sulcere_dm
)
freq_table(freq2) #24% versus 17%
chisq.test(female$arthritis, female$sulcere_dm) #Significant
######################################################
##Define covariates at inclusion for respondents
#binge drinking, individual earned income after tax, earning from self-employment after tax, couple-level capital income after tax, private and public pensions after tax, other govemental transfer after tax, other incomes after tax and total capital income at coulple level after tax, available from wave 2, use wave 3 data for those included in wave 1
#Use birth place to replace living area
#drinking frequency available from wave 2, those included in wave 1 used data from wave 2
#weekly contact with children is at respondent-level
#BMI available only in waves 2, 4, 6, and 8, use wave 2 data for waves 1 and 3, wave 4 data for wave 5, wave 6 data for wave 7 
#r3shlt is not available as self-reported health in wave 3 was recorded using other scale, use wave 4 data for wave 3
######################################################
#Define respondent covariates at inclusion for the three inclusion_wave groups
hshare_all_sp25 <- hshare_all_sp24 %>% mutate(
  rage=case_when(
    inclusion_wave=="Wave 1" ~ r1agey,
    inclusion_wave=="Wave 2" ~ r2agey,
    inclusion_wave=="Wave 4" ~ r4agey,
    inclusion_wave=="Wave 5" ~ r5agey,
    inclusion_wave=="Wave 6" ~ r6agey,
    inclusion_wave=="Wave 7" ~ r7agey,
    TRUE~ r8agey), 
  rabplace2=case_when(
    is.na(rabplace) ~ NA_real_,
    # Africa
    rabplace %in% c("2", "12", "24", "108", "120", "132", "140", "148", 
                    "174", "178", "180", "204", "230", "231", "232", "266", 
                    "270", "288", "324", "384", "404", "430", "434", "450", 
                    "466", "478", "480", "504", "508", "566","624", "638", "646", "678", 
                    "686", "694", "706", "710", "716", "729", "768", "788", 
                    "800", "818", "834", "854", "894", "1010") ~ "Africa",
    
    # Asia
    rabplace %in% c("4", "31", "50", "51", "64", "116", "144", "156", 
                    "158", "268", "275", "344", "356", "360", "364", "368", 
                    "376", "392", "398", "400", "410", "417", "418", "422", 
                    "446", "458", "586", "608", "702", "704", "760", "762", "764", "792", "795", 
                    "860", "887", "1024", "1025", "1050", "1060", "1080", 
                    "1090", "1095", "1100") ~ "Asia",
    
    # Europe
    rabplace %in% c("8", "40", "56", "70", "100", "112", "191", "196", 
                    "200", "203", "208", "233", "234", "246", "250", "276", 
                    "300", "348", "352", "372", "380", "428", "438", "440", 
                    "442", "470", "492", "498", "499", "528", "578", "616", 
                    "620", "642", "643", "688", "703", "705", "724", "752", 
                    "756", "804", "807", "810", "826", "890", "891", "1020", 
                    "1030", "1031", "1040", "1070", "1101") ~ "Europe",
    
    # North America
    rabplace %in% c("124", "188", "192", "214", "222", "304", "308", "312", 
                    "332", "340", "388", "474", "484", "530", "531", "533", 
                    "558", "591", "840", "850") ~ "North America",
    
    # South America
    rabplace %in% c("5", "32", "68", "76", "152", "170", "218", "254", 
                    "328", "600", "604", "740", "858", "862") ~ "South America",
    
    # Oceania
    rabplace %in% c("36", "258", "554") ~ "Oceania",
    
    # Default for any unmatched cases
    TRUE ~ "Other"),
  rmcurln=case_when(
    inclusion_wave=="Wave 1" ~ r1mcurln,
    inclusion_wave=="Wave 2" ~ r2mcurln,
    inclusion_wave=="Wave 4" ~ r4mcurln,
    inclusion_wave=="Wave 5" ~ r5mcurln,
    inclusion_wave=="Wave 6" ~ r6mcurln,
    inclusion_wave=="Wave 7" ~ r7mcurln,
    TRUE~ r8mcurln),
  hrural=case_when(
    inclusion_wave=="Wave 1" ~ h1rural,
    inclusion_wave=="Wave 2" ~ h2rural,
    inclusion_wave=="Wave 4" ~ h4rural,
    inclusion_wave=="Wave 5" ~ h5rural,
    inclusion_wave=="Wave 6" ~ h6rural,
    inclusion_wave=="Wave 7" ~ h7rural,
    TRUE ~ h8rural),
  rshlt=case_when(
    inclusion_wave=="Wave 1" ~ r1shlt,
    inclusion_wave=="Wave 2" ~ r2shlt,
    inclusion_wave=="Wave 4" ~ r4shlt,
    inclusion_wave=="Wave 5" ~ r5shlt,
    inclusion_wave=="Wave 6" ~ r6shlt,
    inclusion_wave=="Wave 7" ~ r7shlt,
    TRUE~ r8shlt),
  radlfive=case_when(
    inclusion_wave=="Wave 1" ~ r1adlfive,
    inclusion_wave=="Wave 2" ~ r2adlfive,
    inclusion_wave=="Wave 4" ~ r4adlfive,
    inclusion_wave=="Wave 5" ~ r5adlfive,
    inclusion_wave=="Wave 6" ~ r6adlfive,
    inclusion_wave=="Wave 7" ~ r7adlfive,
    TRUE~ r8adlfive),
  riadlza=case_when(
    inclusion_wave=="Wave 1" ~ r1iadlza,
    inclusion_wave=="Wave 2" ~ r2iadlza,
    inclusion_wave=="Wave 4" ~ r4iadlza,
    inclusion_wave=="Wave 5" ~ r5iadlza,
    inclusion_wave=="Wave 6" ~ r6iadlza,
    inclusion_wave=="Wave 7" ~ r7iadlza,
    TRUE~ r8iadlza),
  rmobilsev=case_when(
    inclusion_wave=="Wave 1" ~ r1mobilsev,
    inclusion_wave=="Wave 2" ~ r2mobilsev,
    inclusion_wave=="Wave 4" ~ r4mobilsev,
    inclusion_wave=="Wave 5" ~ r5mobilsev,
    inclusion_wave=="Wave 6" ~ r6mobilsev,
    inclusion_wave=="Wave 7" ~ r7mobilsev,
    TRUE~ r8mobilsev),
  rpact=case_when(
    inclusion_wave=="Wave 1" & r1vgactx==2 ~ 3, #vigorous
    inclusion_wave=="Wave 1" & r1mdactx==2 ~ 2, #moderate
    inclusion_wave=="Wave 1" & (r1mdactx %in% c(3,4) | r1vgactx %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 1" & (r1mdactx==5 | r1vgactx==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 2" & r2vgactx==2 ~ 3, #vigorous
    inclusion_wave=="Wave 2" & r2mdactx==2 ~ 2, #moderate
    inclusion_wave=="Wave 2" & (r2mdactx %in% c(3,4) | r2vgactx %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 2" & (r2mdactx==5 | r2vgactx==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 4" & r4vgactx==2 ~ 3, #vigorous
    inclusion_wave=="Wave 4" & r4mdactx==2 ~ 2, #moderate
    inclusion_wave=="Wave 4" & (r4mdactx %in% c(3,4) | r4vgactx %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 4" & (r4mdactx==5 | r4vgactx==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 5" & r5vgactx==2 ~ 3, #vigorous
    inclusion_wave=="Wave 5" & r5mdactx==2 ~ 2, #moderate
    inclusion_wave=="Wave 5" & (r5mdactx %in% c(3,4) | r5vgactx %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 5" & (r5mdactx==5 | r5vgactx==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 6" & r6vgactx==2 ~ 3, #vigorous
    inclusion_wave=="Wave 6" & r6mdactx==2 ~ 2, #moderate
    inclusion_wave=="Wave 6" & (r6mdactx %in% c(3,4,5) | r6vgactx %in% c(3,4,5)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 6" & (r6mdactx==5 | r6vgactx==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 7" & r7vgactx==2 ~ 3, #vigorous
    inclusion_wave=="Wave 7" & r7mdactx==2 ~ 2, #moderate
    inclusion_wave=="Wave 7" & (r7mdactx %in% c(3,4) | r7vgactx %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 7" & (r7mdactx==5 | r7vgactx==5) ~ 0, #Inactive    
    
    inclusion_wave=="Wave 8" & r8vgactx==2 ~ 3, #vigorous
    inclusion_wave=="Wave 8" & r8mdactx==2 ~ 2, #moderate
    inclusion_wave=="Wave 8" & (r8mdactx %in% c(3,4) | r8vgactx %in% c(3,4)) ~ 1, #Some activity but low frequency    
    inclusion_wave=="Wave 8" & (r8mdactx==5 | r8vgactx==5) ~ 0, #Inactive    
    TRUE ~ NA),
  rdrinkb=case_when(
    inclusion_wave=="Wave 1" ~ r2drinkb,
    inclusion_wave=="Wave 2" ~ r2drinkb,
    inclusion_wave=="Wave 4" ~ r4drinkb,
    inclusion_wave=="Wave 5" ~ r5drinkb,
    inclusion_wave=="Wave 6" ~ r6drinkb,
    inclusion_wave=="Wave 7" ~ r7drinkb,
    TRUE ~ r8drinkb),
  rdrinkw=case_when(
    inclusion_wave=="Wave 1" ~ r1drinkxw,
    inclusion_wave=="Wave 2" ~ r2drinkxw,
    inclusion_wave=="Wave 4" ~ r4drinkxw,
    inclusion_wave=="Wave 5" ~ r5drinkxw,
    inclusion_wave=="Wave 6" ~ r6drinkxw,
    inclusion_wave=="Wave 7" ~ r7drinkxw,
    TRUE ~ r8drinkxw),
  rsmokev=case_when(
    inclusion_wave=="Wave 1" ~ r1smokev,
    inclusion_wave=="Wave 2" ~ r2smokev,
    inclusion_wave=="Wave 4" ~ r4smokev,
    inclusion_wave=="Wave 5" ~ r5smokev,
    inclusion_wave=="Wave 6" ~ r6smokev,
    inclusion_wave=="Wave 7" ~ r7smokev,
    TRUE ~ r8smokev),
  ritearn=case_when(
    inclusion_wave=="Wave 1" ~ r2itearn,
    inclusion_wave=="Wave 2" ~ r2itearn,
    inclusion_wave=="Wave 4" ~ r4itearn,
    inclusion_wave=="Wave 5" ~ r5itearn,
    inclusion_wave=="Wave 6" ~ r6itearn,
    inclusion_wave=="Wave 7" ~ r7itearn,
    TRUE ~ r8itearn),
  ritsemp=case_when(
    inclusion_wave=="Wave 1" ~ r2itsemp,
    inclusion_wave=="Wave 2" ~ r2itsemp,
    inclusion_wave=="Wave 4" ~ r4itsemp,
    inclusion_wave=="Wave 5" ~ r5itsemp,
    inclusion_wave=="Wave 6" ~ r6itsemp,
    inclusion_wave=="Wave 7" ~ r7itsemp,
    TRUE ~ r8itsemp),
  hitcap=case_when(
    inclusion_wave=="Wave 1" ~ h2itcap,
    inclusion_wave=="Wave 2" ~ h2itcap,
    inclusion_wave=="Wave 4" ~ h4itcap,
    inclusion_wave=="Wave 5" ~ h5itcap,
    inclusion_wave=="Wave 6" ~ h6itcap,
    inclusion_wave=="Wave 7" ~ h7itcap,
    TRUE ~ h8itcap),
  ripripen=case_when(
    inclusion_wave=="Wave 1" ~ r2itpena,
    inclusion_wave=="Wave 2" ~ r2itpena,
    inclusion_wave=="Wave 4" ~ r4itpena,
    inclusion_wave=="Wave 5" ~ r5itpena,
    inclusion_wave=="Wave 6" ~ r6itpena,
    inclusion_wave=="Wave 7" ~ r7itpena,
    TRUE ~ r8itpena),
  ripubpen=case_when(
    inclusion_wave=="Wave 1" ~ r2itpubpen,
    inclusion_wave=="Wave 2" ~ r2itpubpen,
    inclusion_wave=="Wave 4" ~ r4itpubpen,
    inclusion_wave=="Wave 5" ~ r5itpubpen,
    inclusion_wave=="Wave 6" ~ r6itpubpen,
    inclusion_wave=="Wave 7" ~ r7itpubpen,
    TRUE ~ r8itpubpen),
  rigxfr=case_when(
    inclusion_wave=="Wave 1" ~ r2itgxfr,
    inclusion_wave=="Wave 2" ~ r2itgxfr,
    inclusion_wave=="Wave 4" ~ r4itgxfr,
    inclusion_wave=="Wave 5" ~ r5itgxfr,
    inclusion_wave=="Wave 6" ~ r6itgxfr,
    inclusion_wave=="Wave 7" ~ r7itgxfr,
    TRUE ~ r8itgxfr),
  ritothr=case_when(
    inclusion_wave=="Wave 1" ~ r2itothr,
    inclusion_wave=="Wave 2" ~ r2itothr,
    inclusion_wave=="Wave 4" ~ r4itothr,
    inclusion_wave=="Wave 5" ~ r5itothr,
    inclusion_wave=="Wave 6" ~ r6itothr,
    inclusion_wave=="Wave 7" ~ r7itothr,
    TRUE ~ r8itothr),
  hitot=case_when(
    inclusion_wave=="Wave 1" ~ h2ittot,
    inclusion_wave=="Wave 2" ~ h2ittot,
    inclusion_wave=="Wave 4" ~ h4ittot,
    inclusion_wave=="Wave 5" ~ h5ittot,
    inclusion_wave=="Wave 6" ~ h6ittot,
    inclusion_wave=="Wave 7" ~ h7ittot,
    TRUE ~ h8ittot),
  hkcnt=case_when(
    inclusion_wave=="Wave 1" ~ h1kcnt,
    inclusion_wave=="Wave 2" ~ h2kcnt,
    inclusion_wave=="Wave 4" ~ h4kcnt,
    inclusion_wave=="Wave 5" ~ h5kcnt,
    inclusion_wave=="Wave 6" ~ h6kcnt,
    inclusion_wave=="Wave 7" ~ h7kcnt,
    TRUE ~ h8kcnt),
  roccup=case_when(
    inclusion_wave=="Wave 1" ~ r1lbrf_s,
    inclusion_wave=="Wave 2" ~ r2lbrf_s,
    inclusion_wave=="Wave 4" ~ r4lbrf_s,
    inclusion_wave=="Wave 5" ~ r5lbrf_s,
    inclusion_wave=="Wave 6" ~ r6lbrf_s,
    inclusion_wave=="Wave 7" ~ r7lbrf_s,
    TRUE ~ r8lbrf_s),
  rmbmin=case_when(
    inclusion_wave=="Wave 1" ~ r1bmi,
    inclusion_wave=="Wave 2" ~ r2bmi,
    inclusion_wave=="Wave 4" ~ r4bmi,
    inclusion_wave=="Wave 5" ~ r5bmi,
    inclusion_wave=="Wave 6" ~ r6bmi,
    inclusion_wave=="Wave 7" ~ r7bmi,
    TRUE ~ r8bmi),
  rmbmicat=case_when(
    inclusion_wave=="Wave 1" ~ r1bmicat,
    inclusion_wave=="Wave 2" ~ r2bmicat,
    inclusion_wave=="Wave 4" ~ r4bmicat,
    inclusion_wave=="Wave 5" ~ r5bmicat,
    inclusion_wave=="Wave 6" ~ r6bmicat,
    inclusion_wave=="Wave 7" ~ r7bmicat,
    TRUE ~ r8bmicat),
  rclstress=case_when(
    (ralsevent_s > 0 & !is.na(ralsevent_s)) | (racsevent_s > 0 & !is.na(racsevent_s))  ~ 1,
    (ralsevent_s==0 & !is.na(ralsevent_s)) | (racsevent_s==0 & !is.na(racsevent_s))  ~ 0,
    TRUE ~ NA_real_))

#Check for inconsistencies in household variables
inconsistent_households <- hshare_all_sp25 %>%
  group_by(householdID) %>%
  summarise(
    across(c(hrural, hitcap, hitot, hkcnt), 
           ~ length(unique(.)) > 1,  # Returns TRUE if more than one unique value
           .names = "{.col}_inconsistent"
    )
  ) %>%
  filter(if_any(ends_with("_inconsistent"), ~ .))  # Keep only households with any inconsistency
#7056 spousal pairs with inconsistent hitcap, hitot, and hkcnt variables
#Check the number of spousal pairs with inconsistent hitcap, hitot and hkcnt variables
table(inconsistent_households$hrural_inconsistent, exclude=NULL) #3 spousal pairs
table(inconsistent_households$hitcap_inconsistent, exclude=NULL) #7041 spousal pairs
table(inconsistent_households$hitot_inconsistent, exclude=NULL) #7041 spousal pairs
table(inconsistent_households$hkcnt_inconsistent, exclude=NULL) #15 spousal pairs

#Select those with inconsistent household hrural variable
qc <- inconsistent_households %>% filter(hrural_inconsistent=="TRUE")
qc2 <- hshare_all_sp25 %>% filter(householdID %in% qc$householdID) %>% dplyr::select("householdID", "hrural") #2 spousal pairs with incosistent living area, 1 spousal pair with either of spouse with missing living area
#Select those with inconsistent household hitcap variable
qc <- inconsistent_households %>% filter(hitcap_inconsistent=="TRUE")
qc2 <- hshare_all_sp25 %>% filter(householdID %in% qc$householdID) %>% dplyr::select("householdID", "hitcap")
#Select those with inconsistent household hitot variable
qc <- inconsistent_households %>% filter(hitot_inconsistent=="TRUE")
qc2 <- hshare_all_sp25 %>% filter(householdID %in% qc$householdID) %>% dplyr::select("householdID", "hitot")
#Select those with inconsistent household hkcnt variable
qc <- inconsistent_households %>% filter(hkcnt_inconsistent=="TRUE")
qc2 <- hshare_all_sp25 %>% filter(householdID %in% qc$householdID) %>% dplyr::select("householdID", "hkcnt") #inconsistencies due to missing value in either of spouse

#Refine income variables hitcap and hitot based on the higher/lower figure within spousal pairs, and for those spousal pairs of which either spouse with missing value, define based on the available value
#Refine hkcnt variable based on the available value
hshare_all_sp25 <- hshare_all_sp25 %>%
  group_by(householdID) %>%
  mutate(
    hitcap_max = case_when(
      # Both spouses have values - choose higher (or lower)
      !is.na(first(hitcap)) & !is.na(last(hitcap)) ~ max(hitcap, na.rm = TRUE),
      # Only one spouse has value - use that value
      !is.na(first(hitcap)) & is.na(last(hitcap)) ~ first(hitcap),
      is.na(first(hitcap)) & !is.na(last(hitcap)) ~ last(hitcap),
      # Both are NA - keep as NA
      TRUE ~ NA_real_),
    hitcap_min = case_when(
      !is.na(first(hitcap)) & !is.na(last(hitcap)) ~ min(hitcap, na.rm = TRUE),
      !is.na(first(hitcap)) & is.na(last(hitcap)) ~ first(hitcap),
      is.na(first(hitcap)) & !is.na(last(hitcap)) ~ last(hitcap),
      TRUE ~ NA_real_),
    hitot_max = case_when(
      # Both spouses have values - choose higher (or lower)
      !is.na(first(hitot)) & !is.na(last(hitot)) ~ max(hitot, na.rm = TRUE),
      # Only one spouse has value - use that value
      !is.na(first(hitot)) & is.na(last(hitot)) ~ first(hitot),
      is.na(first(hitot)) & !is.na(last(hitot)) ~ last(hitot),
      # Both are NA - keep as NA
      TRUE ~ NA_real_),
    hitot_min = case_when(
      !is.na(first(hitot)) & !is.na(last(hitot)) ~ min(hitot, na.rm = TRUE),
      !is.na(first(hitot)) & is.na(last(hitot)) ~ first(hitot),
      is.na(first(hitot)) & !is.na(last(hitot)) ~ last(hitot),
      TRUE ~ NA_real_),
    hkcnt2=case_when(
      !is.na(first(hkcnt)) & !is.na(last(hkcnt)) ~ hkcnt,
      !is.na(first(hkcnt)) & is.na(last(hkcnt)) ~ first(hkcnt),
      is.na(first(hkcnt)) & !is.na(last(hkcnt)) ~ last(hkcnt),
      TRUE ~ NA_real_),
    hrural2=case_when(
      first(hrural)==last(hrural) ~ hrural,
      first(hrural) != last(hrural) ~ NA_real_,
      !is.na(first(hrural)) & is.na(last(hrural)) ~ first(hrural),
      is.na(first(hrural)) & !is.na(last(hrural)) ~ last(hrural))) %>%
  ungroup()

#Check for inconsistencies in household variables again
inconsistent_households2 <- hshare_all_sp25 %>%
  group_by(householdID) %>%
  summarise(
    across(c(hitcap_max, hitcap_min, hitot_max, hitot_min, hkcnt2, hrural2), 
           ~ length(unique(.)) > 1,  # Returns TRUE if more than one unique value
           .names = "{.col}_inconsistent"
    )
  ) %>%
  filter(if_any(ends_with("_inconsistent"), ~ .)) #No inconsistencies 

#Double check if household variables are correctly redefined
qc <- hshare_all_sp25 %>% filter(householdID %in% inconsistent_households$householdID) %>% select(householdID, hrural, hrural2, hitcap, hitcap_max, hitcap_min, hitot, hitot_max, hitot_min, hkcnt, hkcnt2) #Passed QC
######################################################
##Define covariates at inclusion for the paired spouses
######################################################
#Define spouses' covariates at inclusion for the three inclusion_wave groups
hshare_all_sp25 <- hshare_all_sp25 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sbyear = ifelse(person_num == 1, rabyear[2], rabyear[1]),
    sage = ifelse(person_num == 1, rage[2], rage[1]),
    sgender = ifelse(person_num == 1, ragender[2], ragender[1]),  
    seducl = ifelse(person_num == 1, raeducl[2], raeducl[1]),
    smcurln = ifelse(person_num == 1, rmcurln[2], rmcurln[1]),
    sbplace= ifelse(person_num == 1,  rabplace2[2],  rabplace2[1]),
    srelig= ifelse(person_num == 1,  rarelig_s[2],  rarelig_s[1]),
    sshlt = ifelse(person_num == 1, rshlt[2], rshlt[1]),
    sadlfive = ifelse(person_num == 1, radlfive[2], radlfive[1]),
    siadlza = ifelse(person_num == 1, riadlza[2], riadlza[1]),
    smobilsev = ifelse(person_num == 1, rmobilsev[2], rmobilsev[1]),
    spact = ifelse(person_num == 1, rpact[2], rpact[1]),
    sdrinkb = ifelse(person_num == 1, rdrinkb[2], rdrinkb[1]),
    sdrinkw = ifelse(person_num == 1, rdrinkw[2], rdrinkw[1]),
    ssmokev = ifelse(person_num == 1, rsmokev[2], rsmokev[1]),
    sitearn = ifelse(person_num == 1, ritearn[2], ritearn[1]),
    sitsemp = ifelse(person_num == 1, ritsemp[2], ritsemp[1]),
    sipripen = ifelse(person_num == 1, ripripen[2], ripripen[1]),
    sipubpen = ifelse(person_num == 1, ripubpen[2], ripubpen[1]),
    sigxfr = ifelse(person_num == 1, rigxfr[2], rigxfr[1]),
    sitothr = ifelse(person_num == 1, ritothr[2], ritothr[1]),
    smomeducl = ifelse(person_num == 1, ramomeducl[2], ramomeducl[1]),
    sdadeducl = ifelse(person_num == 1, radadeducl[2], radadeducl[1]),
    soccup = ifelse(person_num == 1, roccup[2], roccup[1]),
    smbmin = ifelse(person_num == 1, rmbmin[2], rmbmin[1]),
    smbmicat = ifelse(person_num == 1, rmbmicat[2], rmbmicat[1]),
    sclstress = ifelse(person_num == 1, rclstress[2], rclstress[1])) %>%
  ungroup()

#Define household income=ritearn+sitearn+hicap+ripen+sipen+rigxfr+sigxfr+hiothhh
hshare_all_sp25 <- hshare_all_sp25 %>% mutate(hincome_max=case_when(
  !is.na(ritearn) & !is.na(sitearn) & !is.na(hitcap_max) & !is.na(ripripen) & !is.na(sipripen) & !is.na(ripubpen) & !is.na(sipubpen) & !is.na(rigxfr) & !is.na(sigxfr) & !is.na(ritothr) & !is.na(sitothr) ~ ritearn+sitearn+hitcap_max+ripripen+sipripen+ripubpen+sipubpen+rigxfr+sigxfr+ritothr+sitothr,
  TRUE ~ NA),
  hincome_min=case_when(
    !is.na(ritearn) & !is.na(sitearn) & !is.na(hitcap_min) & !is.na(ripripen) & !is.na(sipripen) & !is.na(ripubpen) & !is.na(sipubpen) & !is.na(rigxfr) & !is.na(sigxfr) & !is.na(ritothr) & !is.na(sitothr) ~ ritearn+sitearn+hitcap_min+ripripen+sipripen+ripubpen+sipubpen+rigxfr+sigxfr+ritothr+sitothr,
    TRUE ~ NA))

##Check potential misdefined variables
#Age at interview
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 1" & rage != r1agey) | (inclusion_wave=="Wave 2" & rage != r2agey )| (inclusion_wave=="Wave 4" & rage != r4agey)) #None
#Length of current marriage
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 4" & rmcurln != r4mcurln) | (inclusion_wave=="Wave 5" & rmcurln != r5mcurln)) #None
#Self-reported health
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 6" & rshlt != r6shlt) | (inclusion_wave=="Wave 7" & rshlt != r7shlt)| (inclusion_wave=="Wave 8" & rshlt != r8shlt)) #None
#ADL summary
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 1" & radlfive != r1adlfive) | (inclusion_wave=="Wave 2" & radlfive != r2adlfive) | (inclusion_wave=="Wave 4" & radlfive != r4adlfive)) #None
#Mobility summary
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 4" & rmobilsev != r4mobilsev) | (inclusion_wave=="Wave 5" & rmobilsev != r5mobilsev) | (inclusion_wave=="Wave 6" & rmobilsev != r6mobilsev)) #None
#Physical activity 
qc <- hshare_all_sp25 %>% select(inclusion_wave, r1mdactx,r2mdactx, r4mdactx, r5mdactx, r6mdactx, r7mdactx, r8mdactx, r9mdactx, r1vgactx, r2vgactx, r4vgactx, r5vgactx, r6vgactx, r7vgactx, r8vgactx, r9vgactx, rpact)
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 1" & rpact==3) #All "r1vgactx" should equal to 2
table(qc$r1vgactx,exclude=NULL) #Passed QC
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 2" & rpact==3) #All "r1vgactx" should equal to 2
table(qc$r2vgactx,exclude=NULL) #Passed QC
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 4" & rpact==3) #All "r1vgactx" should equal to 2
table(qc$r4vgactx,exclude=NULL) #Passed QC
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 1" & is.na(rpact)) %>% select(inclusion_wave, r1mdactx,r2mdactx, r4mdactx, r5mdactx, r6mdactx, r7mdactx, r8mdactx, r9mdactx, r1vgactx, r2vgactx, r4vgactx, r5vgactx, r6vgactx, r7vgactx, r8vgactx, r9vgactx, rpact)#All wave 1 physical activity variables should be NA. #Passed QC
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 2" & is.na(rpact)) %>% select(inclusion_wave, r1mdactx,r2mdactx, r4mdactx, r5mdactx, r6mdactx, r7mdactx, r8mdactx, r9mdactx, r1vgactx, r2vgactx, r4vgactx, r5vgactx, r6vgactx, r7vgactx, r8vgactx, r9vgactx, rpact)#All wave 2 physical activity variables should be NA. #Passed QC
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 4" & is.na(rpact)) %>% select(inclusion_wave, r1mdactx,r2mdactx, r4mdactx, r5mdactx, r6mdactx, r7mdactx, r8mdactx, r9mdactx, r1vgactx, r2vgactx, r4vgactx, r5vgactx, r6vgactx, r7vgactx, r8vgactx, r9vgactx, rpact)#All Wave 4 physical activity variables should be NA. #Passed QC
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 1" & rpact==0) %>% select(inclusion_wave, r1mdactx,r2mdactx, r4mdactx, r5mdactx, r6mdactx, r7mdactx, r8mdactx, r9mdactx, r1vgactx, r2vgactx, r4vgactx, r5vgactx, r6vgactx, r7vgactx, r8vgactx, r9vgactx, rpact)#All wave 1 physical activity variables should be 3/4/5. #Passed QC
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 2" & rpact==0) %>% select(inclusion_wave, r1mdactx,r2mdactx, r4mdactx, r5mdactx, r6mdactx, r7mdactx, r8mdactx, r9mdactx, r1vgactx, r2vgactx, r4vgactx, r5vgactx, r6vgactx, r7vgactx, r8vgactx, r9vgactx, rpact)#All wave 2 physical activity variables should be 3/4/5. #Passed QC
qc <- hshare_all_sp25 %>% filter(inclusion_wave=="Wave 4" & rpact==0) %>% select(inclusion_wave, r1mdactx,r2mdactx, r4mdactx, r5mdactx, r6mdactx, r7mdactx, r8mdactx, r9mdactx, r1vgactx, r2vgactx, r4vgactx, r5vgactx, r6vgactx, r7vgactx, r8vgactx, r9vgactx, rpact)#All Wave 4 physical activity variables should be 3/4/5. #Passed QC
#Whether the respondent drinks weekly or has had an alcoholic drink during the last 7 days, depending on the wave.
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 1" & rdrinkw != r1drinkxw) | (inclusion_wave=="Wave 2" & rdrinkw != r2drinkxw)| (inclusion_wave=="Wave 4" & rdrinkw != r4drinkxw)) #None
#Whether the respondent ever binge drinks, and is asked starting in Wave 2
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 8" & rdrinkb != r8drinkb) | (inclusion_wave=="Wave 7" & rdrinkb != r7drinkb)| (inclusion_wave=="Wave 5" & rdrinkb != r5drinkb)) #None
#Ever smoking
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 1" & rsmokev != r1smokev) | (inclusion_wave=="Wave 2" & rsmokev != r2smokev)| (inclusion_wave=="Wave 4" & rsmokev != r4smokev)) #None
#Household income in the past year (after tax)
qc <- hshare_all_sp25 %>% filter(is.na(hincome_max)) %>% select("mergeid","householdID","inclusion_wave", "ritearn", "sitearn", "hitcap", "ripripen", "sipripen","ripubpen", "sipubpen","rigxfr", "sigxfr", "ritothr", "sitothr","hincome_max","hincome_min") #68 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC
#Weekly contact with children in person/by phone/email
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 1" & hkcnt != h1kcnt) | (inclusion_wave=="Wave 2" & hkcnt != h2kcnt)| (inclusion_wave=="Wave 4" & hkcnt != h4kcnt)) #None
#Occupation
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 1" & roccup != r1lbrf_s) | (inclusion_wave=="Wave 2" & roccup != r2lbrf_s) | (inclusion_wave=="Wave 4" & roccup != r4lbrf_s)) #None
#Measured BMI
qc <- hshare_all_sp25 %>% filter((inclusion_wave=="Wave 1" & rmbmin != r1bmi) | (inclusion_wave=="Wave 2" & rmbmin != r2bmi)| (inclusion_wave=="Wave 4" & rmbmin != r4bmi)) #None
#Childhood/lifetime stressful events including
##Childhood stressful events
#whether the respondent has missed a month or more of school before age 16 because of a health condition (ramischlth)
#whether either of the respondent's parents/guardians drank heavily/had mental health problems during childhood (rapadrug)
#whether the respondent experienced financial hardship before age 16 (rasfnhch) and 
#whether the respondent had experienced a difficult living arrangement before age 16 (ralivdiffch)
##Lifetime stressful events
#whether the respondent has missed a month or more of school before the age of 16 because of a health condition (ramischlth)
#whether either of the respondent's parents/guardians drank heavily/had mental health problems during childhood (rapadrug), 
#whether the respondent ever experienced financial hardship (rWsfnhe)
#whether the respondent had experienced a difficult living arrangement before age 16 (ralivdiffch).
qc <- hshare_all_sp25 %>% filter(is.na(rclstress)) %>% select("mergeid","householdID","inclusion_wave",  "ramischlth", "rasfnhch","ralivdiffch","rapadrug","r3sfnhe","r7sfnhe","rclstress") #14405 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC

#Save dataset hshare_all_sp25
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/SHARE')
save(hshare_all_sp25, file = "hshare_all_sp25.rda")
######################################################
###################################################### 
###################################################### 


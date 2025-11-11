#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20251002
#UPDATED: 20251111
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE MHAS 2001-2022 wave (1-6) DATA STRUCTURE, DATA PREPARATION, PERFORM STATISTICAL ANALYSES 
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#Harmonized MHAS waves 1 to 6
  
#Logbook
######################################################  
######################################################

#Things to pay attention
######################################################
#20251103 No variables for diagnosis of memory-related condition, only self-reported
#20251105 Use hitot in the analysis given there is no significant difference in value and missingness compared to hincome

######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
#2. The number of eligible spousal pairs in each wave (using harmonized data)
#3. The number of individuals died during follow-up (hmhas_all_sp4)
#4. Descriptive data of selected baseline and outcome variables in hmhas_all_sp23


#DATA PREPARATION
#1. List of included variables  
#2. Define outcomes and potential confounders to be adjusted in hmhas_all_sp5
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
               openxlsx,
               gmodels,
               readr,
               purrr
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
#3. The number of individuals died during follow-up (hmhas_all_sp4)
#4. Descriptive data of selected baseline and outcome variables in hmhas_all_sp23
######################################################
#1. Participation of individuals across waves (using harmonized data)
######################################################
#Load harmonized ELSA data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/MHAS/Data/H_MHAS_d')
hmhas <- read_dta("H_MHAS_d.dta")

##Wave 1
#The total number of individual #15186
table(hmhas$inw1) 
#The total number of spousal pairs #5312
w1 <- hmhas %>% filter(inw1==1 & s1iwstat==1) %>% select("unhhid","unhhidnp","h1hhid", "subhog_01","s1hhidnp") 
spair_1 <- w1 %>% filter(!is.na(unhhidnp) & !is.na(s1hhidnp)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(unhhidnp, s1hhidnp), 
                  pmax(unhhidnp, s1hhidnp), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 2
#The total number of individual #13704
table(hmhas$inw2) 
#The total number of spousal pairs #4781
w2 <- hmhas %>% filter(inw2==1 & s2iwstat==1) %>% select("unhhid","unhhidnp","h2hhid", "subhog_03","s2hhidnp") 
spair_2 <- w2 %>% filter(!is.na(unhhidnp) & !is.na(s2hhidnp)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(unhhidnp, s2hhidnp), 
                  pmax(unhhidnp, s2hhidnp), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 3
#The total number of individual #15723
table(hmhas$inw3) 
#The total number of spousal pairs #5296
w3 <- hmhas %>% filter(inw3==1 & s3iwstat==1) %>% select("unhhid","unhhidnp","h3hhid", "subhog_12","s3hhidnp") 
spair_3 <- w3 %>% filter(!is.na(unhhidnp) & !is.na(s3hhidnp)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(unhhidnp, s3hhidnp), 
                  pmax(unhhidnp, s3hhidnp), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 4
#The total number of individual #14779
table(hmhas$inw4) 
#The total number of spousal pairs #4826
w4 <- hmhas %>% filter(inw4==1 & s4iwstat==1) %>% select("unhhid","unhhidnp","h4hhid", "subhog_15","s4hhidnp") 
spair_4 <- w4 %>% filter(!is.na(unhhidnp) & !is.na(s4hhidnp)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(unhhidnp, s4hhidnp), 
                  pmax(unhhidnp, s4hhidnp), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 5
#The total number of individual #17114
table(hmhas$inw5) 
#The total number of spousal pairs #5693
w5 <- hmhas %>% filter(inw5==1 & s5iwstat==1) %>% select("unhhid","unhhidnp","h5hhid", "subhog_18","s5hhidnp") 
spair_5 <- w5 %>% filter(!is.na(unhhidnp) & !is.na(s5hhidnp)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(unhhidnp, s5hhidnp), 
                  pmax(unhhidnp, s5hhidnp), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

##Wave 6
#The total number of individual #15739
table(hmhas$inw6) 
#The total number of spousal pairs #4768
w6 <- hmhas %>% filter(inw5==1 & s6iwstat==1) %>% select("unhhid","unhhidnp","h6hhid", "subhog_21","s6hhidnp") 
spair_6 <- w6 %>% filter(!is.na(unhhidnp) & !is.na(s6hhidnp)) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  pair_id = paste(pmin(unhhidnp, s6hhidnp), 
                  pmax(unhhidnp, s6hhidnp), 
                  sep = "_")
) %>%
  distinct(pair_id)  # Keep only unique pairs

######################################################
#2. The number of eligible spousal pairs in each wave (using harmonized data)
######################################################
#Inclusion criteria: For each dataset, heterosexual spousal pairs in which both partners participated in at least two survey waves will be included. In cases where multiple spouses were recorded for an individual, only the first will be retained. Both spouses must have complete data on age, gender, and doctor-diagnosed arthritis.
#Exclusion criteria: To ensure the reliability of responses, spousal pairs in which either partner reported a doctor-diagnosed memory-related condition (e.g., dementia or Alzheimer’s disease) or was receiving treatment for such conditions will be excluded. Additionally, proxy interviews—often indicative of cognitive impairment—will also be excluded. 
######################################################
#Load data and select relevant variables
#Harmonized data_waves_1_to_9
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/MHAS/Data/H_MHAS_d')
hmhas <- read_dta("H_MHAS_d.dta") %>% dplyr::select(6, 14, 29:40, 42:72, 79:102, 115:126, 139:195, 217:223, 256:267, 280:297, 304:315, 928:963, 1036:1071, 1216:1251, 1396:1497, 1522:1641, 1690:1725, 1970:1981, 2006:2041, 2054:2101, 2150:2185, 2540:2587, 3634:3669, 3694:3705, 3802:3885, 3904:3909, 4037:4042, 4105:4112, 4189:4224, 4247:4256, 4291:4302, 4333:4342, 4435:4446, 6096:6103, 6128:6133, 6222:6265, 6295:6308)
######################################################
##Wave 1 ##10340 individuals and 5170 spousal pairs participating from wave 1
######################################################
#all spousal pairs
hmhas_w1 <- hmhas %>% filter(inw1==1 & !is.na(s1hhidnp) & s1hhidnp != 0 & !is.na(s1iwstat) & s1iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(unhhidnp, s1hhidnp), 
                      pmax(unhhidnp, s1hhidnp), 
                      sep = "_")) #10482 individuals and 5170 spousal pairs and 142 individuals
table(as.data.frame(table(hmhas_w1$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hmhas_w1$h1cpl) # all 1

#QC: age distribution, should be 50 or older
table(hmhas_w1$r1agey, exclude=NULL)
table(hmhas_w1$s1agey, exclude=NULL) #Cutoff 18 years, all above 18 years

#SKIPPED:Exclude spousal pairs with either of spouse with missing age values
#hshare_w1_2 <- hshare_w1 %>% filter(r1agey >=18 & s1agey >=18) #18542 individuals and 9271 spousal pairs
#table(as.data.frame(table(hshare_w1_2$householdID, exclude = NULL))$Freq, exclude=NULL)

#Select those without paired spouses as respondents
qc <- as.data.frame(table(hmhas_w1$householdID, exclude = NULL)) %>% filter(Freq==1)

#Exclude those without paired spouses as respondent in wave 1
hmhas_w1_2 <- hmhas_w1 %>% filter(!householdID %in% qc$Var1) #10340 individuals and 5170 spousal pairs
table(as.data.frame(table(hmhas_w1_2$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hmhas_w1_2 <- hmhas_w1_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre))))) 
table(hmhas_w1_2$missing_count_rarthre, exclude=NULL)
#3834 individuals with no missing, 1555 individuals with 1 missing value, 986 individuals with 2 missing values, 2139 individuals with 3 missing values
#911 individuals with 4 missing values, 31 individuals with 5 missing values

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hmhas_w1_2$r1iwstat, exclude=NULL) #all 1
table(hmhas_w1_2$s1iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hmhas_w1_2 <- hmhas_w1_2 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part))))
table(hmhas_w1_2$total_participations, exclude=NULL) #877 individuals participated in wave 1 only
qc <- as.data.frame(table(hmhas_w1_2$spousal_part_pattern, exclude=NULL)) #30 patterns

#Check the number of individual participated only in wave 1
qc <- hmhas_w1_2 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #877 individuals, 280 spousal pairs and 317 individuals
######################################################
##Wave 2 ##746 individuals and 373 spousal pairs participating from wave 2
######################################################
#All spousal pairs participating in wave 2
hmhas_w2 <- hmhas %>% filter(inw2==1 & !is.na(s2hhidnp) & s2hhidnp != 0 & !is.na(s2iwstat) & s2iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(unhhidnp, s2hhidnp), 
                      pmax(unhhidnp, s2hhidnp), 
                      sep = "_")) #9561 individuals, with 4780 spousal pairs and 1 individual 
table(as.data.frame(table(hmhas_w2$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hmhas_w2$h2cpl) # all 1

#Exclude individuals included in hmhas_w1_2
hmhas_w2_2 <- hmhas_w2 %>% filter(!householdID %in% hmhas_w1_2$householdID) #753 individuals, with 376 spousal pairs and 1 individual
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_w2_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hmhas_w2_2$r2agey, exclude=NULL)
table(hmhas_w2_2$s2agey, exclude=NULL) #All respondents aged above 18 and 3 individuals with missing age

#Exclude spousal pairs with either of spouse with missing age values
hmhas_w2_3 <- hmhas_w2_2 %>% filter(r2agey >=18 & s2agey >=18) #747 individuals and 373 spousal pairs and 1 individuals
table(as.data.frame(table(hmhas_w2_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#Select those without paired spouses as respondents
qc <- as.data.frame(table(hmhas_w2_3$householdID, exclude = NULL)) %>% filter(Freq==1)

#Exclude those without paired spouses as respondent in wave 1
hmhas_w2_4 <- hmhas_w2_3 %>% filter(!householdID %in% qc$Var1) #746 individuals and 373 spousal pairs
table(as.data.frame(table(hmhas_w2_4$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hmhas_w2_4 <- hmhas_w2_4 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre,r3arthre, r4arthre, r5arthre, r6arthre))))) 
table(hmhas_w2_4$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hmhas_w2_4$inw1, exclude=NULL) #492 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hmhas_w2_4$r2iwstat, exclude=NULL) #all 1
table(hmhas_w2_4$s2iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hmhas_w2_4 <- hmhas_w2_4 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part))))
table(hmhas_w2_4$total_participations, exclude=NULL) #74 individuals participated in wave 2 only
qc <- as.data.frame(table(hmhas_w2_4$spousal_part_pattern, exclude=NULL)) #24 patterns

#Check the number of individual participated only in wave 2
qc <- hmhas_w2_4 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #95 individuals and no spousal pairs
######################################################
##Wave 3 ##4734 individuals and 2367 spousal pairs participating from wave 3
######################################################
#All spousal pairs participating in wave 3
hmhas_w3 <- hmhas %>% filter(inw3==1 & !is.na(s3hhidnp) & s3hhidnp != 0 & !is.na(s3iwstat) & s3iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(unhhidnp, s3hhidnp), 
                      pmax(unhhidnp, s3hhidnp), 
                      sep = "_")) #10592 individuals, with 5296 spousal pairs
table(as.data.frame(table(hmhas_w3$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hmhas_w3$h3cpl) # all 1

#Exclude individuals included in hmhas_w1_2, hmhas_w2_4
hmhas_w3_2 <- hmhas_w3 %>% filter(!(householdID %in% hmhas_w1_2$householdID | householdID %in% hmhas_w2_4$householdID)) #4742 individuals, with 2371 spousal pairs 
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_w3_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hmhas_w3_2$r3agey, exclude=NULL)
table(hmhas_w3_2$s3agey, exclude=NULL) #All respondents aged above 18 and 4 individuals with missing age

#Exclude spousal pairs with either of spouse with missing age values
hmhas_w3_3 <- hmhas_w3_2 %>% filter(r3agey >=18 & s3agey >=18) #4734 individuals and 2367 spousal pairs 
table(as.data.frame(table(hmhas_w3_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hmhas_w3_3 <- hmhas_w3_3 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre,r3arthre, r4arthre, r5arthre, r6arthre))))) 
table(hmhas_w3_3$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hmhas_w3_3$inw1, exclude=NULL) #148 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hmhas_w3_3$r3iwstat, exclude=NULL) #all 1
table(hmhas_w3_3$s3iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hmhas_w3_3 <- hmhas_w3_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part))))
table(hmhas_w3_3$total_participations, exclude=NULL) #356 individuals participated in wave 3 only
qc <- as.data.frame(table(hmhas_w3_3$spousal_part_pattern, exclude=NULL)) #23 patterns

#Check the number of individual participated only in wave 3
qc <- hmhas_w3_3 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #356 individuals and 95 spousal pairs and 166 individuals
######################################################
##Wave 4 ##762 individuals and 381 spousal pairs participating from wave 4
######################################################
#All spousal pairs participating in wave 4
hmhas_w4 <- hmhas %>% filter(inw4==1 & !is.na(s4hhidnp) & s4hhidnp != 0 & !is.na(s4iwstat) & s4iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(unhhidnp, s4hhidnp), 
                      pmax(unhhidnp, s4hhidnp), 
                      sep = "_")) #9652 individuals, with 4826 spousal pairs 
table(as.data.frame(table(hmhas_w4$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hmhas_w4$h4cpl) # all 1

#Exclude individuals included in hmhas_w1_2 or hmhas_w2_4 or hmhas_3_3
hmhas_w4_2 <- hmhas_w4 %>% filter(!(householdID %in% hmhas_w1_2$householdID | householdID %in% hmhas_w2_4$householdID | householdID %in% hmhas_w3_3$householdID)) #762 individuals, with 381 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_w4_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hmhas_w4_2$r4agey, exclude=NULL)
table(hmhas_w4_2$s4agey, exclude=NULL) #All respondents aged above 18

#SKIPPED:Exclude spousal pairs with either of spouse with missing age values
#hshare_w2_3 <- hshare_w2_2 %>% filter(r2agey >=18 & s2agey >=18) #13066 individuals and 6533 spousal pairs
#table(as.data.frame(table(hshare_w2_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hmhas_w4_2 <- hmhas_w4_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre))))) 
table(hmhas_w4_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hmhas_w4_2$inw1, exclude=NULL) #77 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hmhas_w4_2$r4iwstat, exclude=NULL) #all 1
table(hmhas_w4_2$s4iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hmhas_w4_2 <- hmhas_w4_2 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part))))
table(hmhas_w4_2$total_participations, exclude=NULL) #74 individuals participated in wave 4 only
qc <- as.data.frame(table(hmhas_w4_2$spousal_part_pattern, exclude=NULL)) #25 patterns

#Check the number of individual participated only in wave 4
qc <- hmhas_w4_2 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #74 individuals, 14 spousal pairs and 46 individuals
######################################################
##Wave 5 ##3810 individuals and 1905 spousal pairs participating from wave 5
######################################################
#All spousal pairs participating in wave 5
hmhas_w5 <- hmhas %>% filter(inw5==1 & !is.na(s5hhidnp) & s5hhidnp != 0 & !is.na(s5iwstat) & s5iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(unhhidnp, s5hhidnp), 
                      pmax(unhhidnp, s5hhidnp), 
                      sep = "_")) #11386 individuals, with 5693 spousal pairs and 
table(as.data.frame(table(hmhas_w5$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hmhas_w5$h5cpl) # all 1

#Exclude individuals included in hmhas_w1_2 or hmhas_w2_4 or hmhas_w3_3 or hmhas_w4_2
hmhas_w5_2 <- hmhas_w5 %>% filter(!(householdID %in% hmhas_w1_2$householdID | householdID %in% hmhas_w2_4$householdID | householdID %in% hmhas_w3_3$householdID | householdID %in% hmhas_w4_2$householdID)) #3826 individuals, with  1913 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_w5_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 50 or older, spouses' age should be 18 and above
table(hmhas_w5_2$r5agey, exclude=NULL)
table(hmhas_w5_2$s5agey, exclude=NULL) #2 respondents aged below18 and 7 individual with missing age

#Exclude spousal pairs with either of spouse with missing age values
hmhas_w5_3 <- hmhas_w5_2 %>% filter(r5agey >=18 & s5agey >=18) #3810 individuals and 1905 spousal pairs
table(as.data.frame(table(hmhas_w5_3$householdID, exclude = NULL))$Freq, exclude=NULL)
table(hmhas_w5_3$r5agey, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hmhas_w5_3 <- hmhas_w5_3 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre))))) 
table(hmhas_w5_3$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: Check the frequency of inw1
table(hmhas_w5_3$inw1, exclude=NULL) #23 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hmhas_w5_3$r5iwstat, exclude=NULL) #all 1
table(hmhas_w5_3$s5iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hmhas_w5_3 <- hmhas_w5_3 %>%   mutate(
  # Create binary participation indicators (1 = participated, 0 = did not participate)
  r1_part = as.numeric(r1iwstat == 1),
  r2_part = as.numeric(r2iwstat == 1),
  r3_part = as.numeric(r3iwstat == 1),
  r4_part = as.numeric(r4iwstat == 1),
  r5_part = as.numeric(r5iwstat == 1),
  r6_part = as.numeric(r6iwstat == 1),
  # Create pattern string using binary participation
  spousal_part_pattern = paste0(r1_part, r2_part, r3_part, r4_part, 
                                r5_part, r6_part),
  # Count participations
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part))))
table(hmhas_w5_3$total_participations, exclude=NULL) #343 individuals participated in wave 5 only
qc <- as.data.frame(table(hmhas_w5_3$spousal_part_pattern, exclude=NULL)) #15 patterns

#Check the number of individual participated only in wave 5
qc <- hmhas_w5_3 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #343 individuals, 80 spousal pairs and 183 individuals
######################################################
##All eligible spousal pairs, hmhas_all_sp4, 17756 individuals and 8878 spousal pairs
######################################################
#Add inclusion wave indicator and create unique householdID
hmhas_w1_2 <- hmhas_w1_2 %>% mutate(inclusion_wave="Wave 1")
hmhas_w2_4 <- hmhas_w2_4 %>% mutate(inclusion_wave="Wave 2")
hmhas_w3_3 <- hmhas_w3_3 %>% mutate(inclusion_wave="Wave 3")
hmhas_w4_2 <- hmhas_w4_2 %>% mutate(inclusion_wave="Wave 4")
hmhas_w5_3 <- hmhas_w5_3 %>% mutate(inclusion_wave="Wave 5")

##Dataset including all spousal pairs, combining hmhas_w1_2, hmhas_w2_4, hmhas_w3_3, hmhas_w4_2, hmhas_w5_3)
hmhas_all_sp <- rbind(hmhas_w1_2, hmhas_w2_4, hmhas_w3_3, hmhas_w4_2, hmhas_w5_3) #20392 individuals and 10196 spousal pairs#Check frequency of inclusion_wave
table(hmhas_all_sp$inclusion_wave, exclude=NULL)
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_all_sp$householdID, exclude = NULL))$Freq, exclude=NULL) 
#Check if all were heterosexual spouse pairs
hetero <- hmhas_all_sp %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%
  summarise(hetero = n_distinct(ragender) == 2) %>% filter(hetero=="FALSE")
table(hetero$hetero, exclude=NULL) #91 spousal pairs were homosexual
qc <- hmhas_all_sp %>% filter(householdID %in% hetero$householdID) %>% select(unhhidnp, householdID, ragender, s1gender:s6gender, inclusion_wave)  

#Exclude the homosexual spousal pair
hmhas_all_sp2 <- hmhas_all_sp %>% filter(!householdID %in% hetero$householdID) #20382 individuals and 10191 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_all_sp2$householdID))$Freq, exclude=NULL) 
#Select individuals with total_participations==1
one_wave <- hmhas_all_sp2 %>% filter(total_participations==1) %>% select(unhhidnp, householdID, inclusion_wave, total_participations) #1720 individuals and 468 spousal pairs and 784 individuals with spouses participated in later waves
#Check the number of spousal pairs
table(as.data.frame(table(one_wave$householdID))$Freq, exclude=NULL) 

#Exclude individuals with total_participations==1
hmhas_all_sp3 <- hmhas_all_sp2 %>% filter(!(householdID %in% one_wave$householdID)) #17878 individuals and 8939 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_all_sp3$householdID))$Freq, exclude=NULL) 

##Create function to produce a new variable counting the number of missing rwarthre based on the participating wave
count_missing_by_participation <- function(data, num_waves = 6) {
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

hmhas_all_sp3$missing_count_rarthre2 <- count_missing_by_participation(hmhas_all_sp3)
#Check the frequency of missing_count_rarthre2 by total_participations
table(hmhas_all_sp3$missing_count_rarthre2, hmhas_all_sp3$total_participations,exclude=NULL) 
#66 individuals participated in two waves with 1 missing value
#1 individuals participated in four waves with 3 missing values

#Select those with missing_count_rarthre2 exceeds the upper limit
qc <- hmhas_all_sp3 %>% filter((missing_count_rarthre2==1 & total_participations==2) | (missing_count_rarthre2==3 & total_participations==4)) %>% select(unhhidnp,householdID,inclusion_wave,spousal_part_pattern,total_participations, r1arthre:r6arthre, missing_count_rarthre2)
#Check the number of spousal pairs
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #67 individuals, 6 spousal pairs and 55 individuals

#Exclude individuals with missing_count_rarthre2 exceeds the upper limit, as well as their spouses
hmhas_all_sp4 <- hmhas_all_sp3 %>% filter(!householdID %in% qc$householdID) #17756 individuals and 8878 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_all_sp4$householdID))$Freq, exclude=NULL) 

#Check the frequencies of total_participation by inclusion_wave
table(hmhas_all_sp4$total_participations, hmhas_all_sp4$inclusion_wave, exclude=NULL)
######################################################
#3. The number of individuals died during follow-up (hmhas_all_sp4)
######################################################
#Load harmonized end of life dataset
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/MHAS/Data/H_MHAS_d')
hmhas_eol <- read_dta("GH_MHAS_EOL_c.dta") %>% select(unhhidnp, raxyear,radage, racod_m)

#Check frequency of radyear 
table(hmhas_all_sp4$radyear, exclude = NULL) #4105 individuals with death year recorded 
qc <- hmhas_all_sp4 %>% filter(!is.na(radyear)) %>% select(unhhidnp, householdID, radyear, inclusion_wave, r1iwy:r6iwy, r1agey:r6agey, r1arthre:r6arthre)

#Check if any individuals in hmhas_all_sp4 also in hmhas_eol as well
qc2 <- hmhas_all_sp4 %>% filter(unhhidnp %in% hmhas_eol$unhhidnp) %>% select(unhhidnp, householdID, radyear, inclusion_wave, r1iwy:r6iwy, r1agey:r6agey, r1arthre:r6arthre) #4128 individuals died during follow-up
qc3 <- qc %>% filter(!unhhidnp %in% qc2$unhhidnp)
#All individuals with death year recorded also had exit interview 
qc4 <- qc2 %>% filter(!unhhidnp %in% qc$unhhidnp)
#23 individuals with exit interview but no death year recorded/with raxyear missing 
qc5 <- qc2 %>% filter(unhhidnp %in% qc$unhhidnp)

#qc2 includes all individuals died during follow-up
#Add death information from exit interview
death <- qc2 %>% left_join(.,hmhas_eol, by="unhhidnp") #4128 individuals

#Select those with interview age older than death age
death2 <- death %>% mutate(exclude=case_when(
  !is.na(radage) & inclusion_wave=="Wave 1" & radage < r1agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 2" & radage < r2agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 3" & radage < r3agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 4" & radage < r4agey ~ 1,
  !is.na(radage) & inclusion_wave=="Wave 5" & radage < r5agey ~ 1,
  !is.na(radyear) & inclusion_wave=="Wave 1" & radyear < r1iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 2" & radyear < r2iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 3" & radyear < r3iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 4" & radyear < r4iwy ~ 2,
  !is.na(radyear) & inclusion_wave=="Wave 5" & radyear < r5iwy ~ 2,
  TRUE ~ NA_real_)) %>% filter(exclude==1|exclude==2) #87 individuals with death happened before interview, 5 spousal pairs and 77 individuals
#Check the number of spousal pairs
table(as.data.frame(table(death2$householdID, exclude = NULL))$Freq, exclude=NULL) 

#Check frequency of rwiwstat, values 5 and 6 indicating death, and value 9 indicating 'don't know alive or died'
table(hmhas_all_sp4$r1iwstat, exclude=NULL) #No death indicators
table(hmhas_all_sp4$r2iwstat, exclude=NULL) #No death indicators
table(hmhas_all_sp4$r3iwstat, exclude=NULL) #1524 individuals died in wave 3, and 231 individuals with unknown death status
table(hmhas_all_sp4$r4iwstat, exclude=NULL) #709 individuals died in wave 4, 1524 individual died in wave 3, and 406 individuals with unknown death status
table(hmhas_all_sp4$r5iwstat, exclude=NULL) #759 individuals died in wave 5, 2233 individual died in waves 3 and 4, and 533 individuals with unknown death status

#Exclude 87 individuals died before interview, as well as their spouses
hmhas_all_sp5 <- hmhas_all_sp4 %>% filter(!householdID %in% death2$householdID) #17592 individuals and 8796 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hmhas_all_sp5$householdID, exclude = NULL))$Freq, exclude=NULL) 

#Check among those excluded how many of them had arthritis 
death2 <- death2 %>% mutate(arthritis=ifelse(r1arthre==1 | r2arthre==1 | r3arthre==1| r4arthre==1| r5arthre==1| r6arthre==1,1,0))
table(death2$arthritis, exclude=NULL) #27 individuals with arthritis
######################################################
#4. Descriptive data of selected baseline and outcome variables in hmhas_all_sp23
######################################################
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/MHAS')
load("hmhas_all_sp23.rda")

#Convert haven:labelled variables into numeric variables 
hmhas_all_sp23 <- hmhas_all_sp23 %>%
  mutate(across(where(haven::is.labelled), as.numeric)) 

#Define categorical variables
hmhas_all_sp23 <- hmhas_all_sp23 %>%
  mutate(across(c(rbmicat, roccup, rameduc_m, rafeduc_m), as.factor))

#Select baseline and outcome variables and convert variables with have_labelled to factor variables
descriptive <- hmhas_all_sp23 %>%
  select(rage, ragender, hrural, raeducl, rbmin, rbmicat, rpactvg,  rdrinkr, rdrinkb, rbinged, rsmokev, roccup, hitot, hkcnt, rameduc_m, rafeduc_m, rclstress, rshlt, radlfive, riadlfour, rmobilsev, arthritis,arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, respe, respe_dm, hrtatte, hrtatte_dm, stroke, stroke_dm)
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
    raeducl ~ "Highest attained education level",
    rbmin ~ "Self-reported BMI_cont",
    rbmicat ~ "Self-reported BMI_cat",
    rpactvg ~ "Vigorous physcial activity",
    rdrinkr ~ "Number of drinks/day when drinks",
    rdrinkb ~ "Ever binge drinks",
    rbinged ~ "Number of days binge drinks",
    rsmokev ~ "Ever smoking",
    roccup ~ "Occupation",
    hitot ~ "Household income",
    hkcnt ~ "Weekly contact with children",
    rameduc_m ~ "Maternal education",
    rafeduc_m ~ "Paternal education",
    rclstress ~ "Childhood/lifetime stressful events (three vars)",
    rshlt ~ "Self-reported health",
    radlfive ~ "ADL summary, five",
    riadlfour ~ "IADL summary, four",
    rmobilsev ~ "Mobility summary, seven",
    arthritis ~ "Doctor-diagnosed arthritis",
    arthritis_dm ~ "Doctor-diagnosed/medication-indicated arthritis",
    hibpe ~ "Doctor-diagnosed hypertension",
    hibpe_dm ~ "Doctor-diagnosed/medication-indicated hypertension",
    diabe ~ "Doctor-diagnosed diabetes",
    diabe_dm ~ "Doctor-diagnosed/medication-indicated diabetes",
    cancre ~ "Doctor-diagnosed cancer",
    cancre_dm ~ "Doctor-diagnosed/medication-indicated cancer",
    respe ~ "Doctor-diagnosed respiratory disease",
    respe_dm ~ "Doctor-diagnosed/medication-indicated respiratory disease",
    hrtatte ~ "Doctor-diagnosed heart attack",
    hrtatte_dm ~ "Doctor-diagnosed/medication-indicated heart attack",
    stroke ~ "Doctor-diagnosed stroke",
    stroke_dm ~ "Doctor-diagnosed/medication-indicated stroke")) %>%
  # Additional formatting
  add_p() %>%  # Add p-values for group comparisons
  add_overall() %>%  # Add overall column
  bold_labels() %>%
  italicize_levels()

#Save descriptive table
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/MHAS')
write_xlsx(descriptive_base_var_by_gender[["table_body"]], path = "mhas_descriptive_base_out_var_by_gender.xlsx", col_names=T, format_headers=T)
######################################################
######################################################
#DATA PREPARATION
#1. List of included variables  
#2. Define outcomes and potential confounders to be adjusted in hmhas_all_sp5
###################################################### 
#1. List of included variables  
###################################################### 
#Load data and select relevant variables
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/MHAS/Data/H_MHAS_d')
hmhas <- read_dta("H_MHAS_d.dta") %>% select(6, 14, 29:40, 42:72, 79:102, 115:126, 139:195, 217:223, 256:267, 280:297, 304:315, 928:963, 1036:1071, 1216:1251, 1396:1497, 1522:1641, 1690:1725, 1970:1981, 2006:2041, 2066:2077, 2150:2185, 2540:2587, 3634:3669, 3802:3825, 3874:3885, 3904:3909, 4037:4042, 4105:4112, 4189:4224, 4247:4256, 4291:4302, 4333:4342, 4435:4446, 6072:6079, 6128:6133, 6222:6228, 6234:6264)

# Check if labels exist as attributes
var_info <- sapply(hmhas, function(x) {
  label <- attr(x, "label")
  ifelse(is.null(label), "", label)
})

# Extract value labels from attributes
val_info <- sapply(hmhas, function(x) {
  labels <- attr(x, "labels")
  if(!is.null(labels)) {
    # Format as "1=Label1; 2=Label2; ..."
    paste(paste(labels, "=", names(labels)), collapse = "; ")
  } else {
    ""
  }
})

#Create a detailed table
hmhas_variable_table <- data.frame(
  Variable = names(hmhas),
  Label = sapply(var_info, function(x) ifelse(is.null(x), "", x)),
  Type = sapply(hmhas, function(x) paste(class(x), collapse = ", ")),
  N_Missing = colSums(is.na(hmhas)),
  Unique_Values = sapply(hmhas, function(x) length(unique(na.omit(x)))),
  stringsAsFactors = FALSE
)

#Add value labels as a separate column
hmhas_variable_table$Value_Labels <- sapply(names(hmhas), function(var) {
  labels <- val_info[[var]]
  if(!is.null(labels)) {
    paste(paste0(names(labels), " (", labels, ")"), collapse = "; ")
  } else {
    ""
  }
})

#Save as excel file
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/MHAS/Basic information')
write.xlsx(hmhas_variable_table, file = "hmhas_selected_variable_table_251010.xlsx", colNames=T, format_headers=T)
###################################################### 
#2. Define outcomes and potential confounders to be adjusted in hmhas_all_sp5
######################################################  
##Define doctor-diagnosed arthritis (outcome, cases/controls)
######################################################  
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 1 
hmhas_all_sp5 <- hmhas_all_sp5 %>% mutate(arthritis=case_when(
  inclusion_wave!="Wave 1"~ NA_real_, 
  inclusion_wave=="Wave 1" & if_any(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre), ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hmhas_all_sp5$arthritis, hmhas_all_sp5$inclusion_wave, exclude=NULL) #For those included from wave 1, 5659 individuals with no arthritis across 6 waves and  3249 individuals with arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hmhas_all_sp5 %>% filter(arthritis==0) %>% select(unhhidnp, householdID,spousal_part_pattern, arthritis, r1arthre:r6arthre)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL) 
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp5 %>% filter(arthritis==1) %>% select(unhhidnp, householdID,spousal_part_pattern, arthritis, r1arthre:r6arthre)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL) 
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL)  #All had 1 
######################################################  
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 2
hmhas_all_sp6 <- hmhas_all_sp5 %>% mutate(arthritis=case_when(
  inclusion_wave=="Wave 1" ~ arthritis,
  inclusion_wave!="Wave 2" ~ NA_real_,
  inclusion_wave=="Wave 2" & if_any(c(r2arthre:r6arthre), ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hmhas_all_sp6$arthritis, hmhas_all_sp6$inclusion_wave, exclude=NULL) #For those included from wave 2, 366 individuals with no arthritis 214 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hmhas_all_sp6 %>% filter(arthritis==0 & inclusion_wave=="Wave 2") %>% select(unhhidnp, householdID,spousal_part_pattern, arthritis, r2arthre:r6arthre)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp6 %>% filter(arthritis==1 & inclusion_wave=="Wave 2") %>% select(unhhidnp, householdID,spousal_part_pattern, arthritis, r1arthre:r6arthre)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All had 1 
###################################################### 
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 3
hmhas_all_sp7 <- hmhas_all_sp6 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ arthritis,
  inclusion_wave!="Wave 3"  ~ NA_real_, 
  inclusion_wave=="Wave 3" & if_any(r3arthre:r6arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hmhas_all_sp7$arthritis, hmhas_all_sp7$inclusion_wave, exclude=NULL) #For those included from wave 3, 3339 individuals with no arthritis and 857 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hmhas_all_sp7 %>% filter(arthritis==0 & inclusion_wave=="Wave 3") %>% select(unhhidnp, householdID, spousal_part_pattern, arthritis, r3arthre:r6arthre)
table(qc$r3arthre, exclude=NULL) 
table(qc$r4arthre, exclude=NULL)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp7 %>% filter(arthritis==1 & inclusion_wave=="Wave 3") %>% select(unhhidnp, householdID, spousal_part_pattern, arthritis, r3arthre:r6arthre)
table(qc$r3arthre, exclude=NULL) 
table(qc$r4arthre, exclude=NULL)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 4
hmhas_all_sp8 <- hmhas_all_sp7 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2","Wave 3") ~ arthritis,
  inclusion_wave!="Wave 4" ~ NA_real_, 
  inclusion_wave=="Wave 4" & if_any(r4arthre:r6arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hmhas_all_sp8$arthritis, hmhas_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 4, 523 individuals with no arthritis and 113 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hmhas_all_sp8 %>% filter(arthritis==0 & inclusion_wave=="Wave 4") %>% select(unhhidnp, householdID, spousal_part_pattern, arthritis, r4arthre:r6arthre)
table(qc$r4arthre, exclude=NULL)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp8 %>% filter(arthritis==1 & inclusion_wave=="Wave 4") %>% select(unhhidnp, householdID, spousal_part_pattern, arthritis, r4arthre:r6arthre)
table(qc$r4arthre, exclude=NULL)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 5
hmhas_all_sp9 <- hmhas_all_sp8 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ arthritis,
  inclusion_wave!="Wave 5" ~ NA_real_, 
  inclusion_wave=="Wave 5" & if_any(r5arthre:r6arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hmhas_all_sp9$arthritis, hmhas_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 5, 2931 individuals with no arthritis and 341 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hmhas_all_sp9 %>% filter(arthritis==0 & inclusion_wave=="Wave 5") %>% select(unhhidnp, householdID, spousal_part_pattern, arthritis, r5arthre:r6arthre)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp9 %>% filter(arthritis==1 & inclusion_wave=="Wave 5") %>% select(unhhidnp, householdID, spousal_part_pattern, arthritis, r5arthre:r6arthre)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) #All had 1 
######################################################
##Define doctor-diagnosed arthritis (exposure)
###################################################### 
hmhas_all_sp9 <- hmhas_all_sp9 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sarthritis = ifelse(person_num == 1, arthritis[2], arthritis[1])
  ) %>%
  ungroup()

#Check the frequency of sarthritis
table(hmhas_all_sp9$sarthritis, exclude=NULL)

#Compare the proportion of men with arthritis who had wives affected by arthritis to those without
male <- hmhas_all_sp9 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_arthritis = male$sarthritis
)
freq_table(freq) #46% versus 29%
chisq.test(male$arthritis, male$sarthritis) #Significant

#Compare the proportion of women with arthritis who had husbands affected by arthritis to those without
female <- hmhas_all_sp9 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_arthritis = female$sarthritis
)
freq_table(freq2) #31% versus 17%
chisq.test(female$arthritis, female$sarthritis) #Significant
######################################################  
##Define doctor-diagnosed/medication-indicated arthritis (outcome, cases/controls)
#No additional cases added when including rwrxarthr
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 1 
hmhas_all_sp9 <- hmhas_all_sp9 %>% mutate(arthritis_dm=case_when(
  inclusion_wave!="Wave 1"~ NA, 
  inclusion_wave=="Wave 1" & (if_any(r1arthre:r6arthre, ~ .x==1) | if_any(r1rxarthr:r6rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hmhas_all_sp9$arthritis_dm, hmhas_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 1, 5659 individuals with no doctor-diagnosed/medication-indicated arthritis across 6 waves and 3249 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hmhas_all_sp9 %>% filter(arthritis_dm==0) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r1rxarthr, exclude=NULL) 
table(qc$r2rxarthr, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp9 %>% filter(arthritis_dm==1) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r1rxarthr, exclude=NULL) 
table(qc$r2rxarthr, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 2
hmhas_all_sp10 <- hmhas_all_sp9 %>% mutate(arthritis_dm=case_when(
  inclusion_wave=="Wave 1"~ arthritis_dm, 
  inclusion_wave!="Wave 2"~ NA, 
  inclusion_wave=="Wave 2" & (if_any(r2arthre:r6arthre, ~ .x==1) | if_any(r2rxarthr:r6rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hmhas_all_sp10$arthritis_dm, hmhas_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 366 individuals with no doctor-diagnosed/medication-indicated arthritis across 5 waves and 214 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hmhas_all_sp10 %>% filter(arthritis_dm==0) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r2rxarthr, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp10 %>% filter(arthritis_dm==1) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r2rxarthr, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 3
hmhas_all_sp11 <- hmhas_all_sp10 %>% mutate(arthritis_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ arthritis_dm,
  inclusion_wave!="Wave 3"~ NA, 
  inclusion_wave=="Wave 3" & (if_any(r3arthre:r6arthre, ~ .x==1) | if_any(r3rxarthr:r6rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hmhas_all_sp11$arthritis_dm, hmhas_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 3, 3339 individuals with no doctor-diagnosed/medication-indicated arthritis across 4 waves and 857 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hmhas_all_sp11 %>% filter(arthritis_dm==0) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp11 %>% filter(arthritis_dm==1) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 4
hmhas_all_sp12 <- hmhas_all_sp11 %>% mutate(arthritis_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ arthritis_dm,
  inclusion_wave!="Wave 4"~ NA, 
  inclusion_wave=="Wave 4" & (if_any(r4arthre:r6arthre, ~ .x==1) | if_any(r4rxarthr:r6rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hmhas_all_sp12$arthritis_dm, hmhas_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 4, 523 individuals with no doctor-diagnosed/medication-indicated arthritis across 3 waves and 113 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hmhas_all_sp12 %>% filter(arthritis_dm==0) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp12 %>% filter(arthritis_dm==1) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 5
hmhas_all_sp13 <- hmhas_all_sp12 %>% mutate(arthritis_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ arthritis_dm,
  inclusion_wave!="Wave 5"~ NA, 
  inclusion_wave=="Wave 5" & (if_any(r5arthre:r6arthre, ~ .x==1) | if_any(r5rxarthr:r6rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hmhas_all_sp13$arthritis_dm, hmhas_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 2931 individuals with no doctor-diagnosed/medication-indicated arthritis across 2 waves and 341 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hmhas_all_sp13 %>% filter(arthritis_dm==0) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hmhas_all_sp13 %>% filter(arthritis_dm==1) %>% select(unhhidnp, householdID, arthritis, arthritis_dm, r1arthre:r6arthre, r1rxarthr:r6rxarthr)
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) #All had 1 
######################################################
##Define doctor-diagnosed/medication-indicated arthritis (exposure)
###################################################### 
hmhas_all_sp13 <- hmhas_all_sp13 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sarthritis_dm = ifelse(person_num == 1, arthritis_dm[2], arthritis_dm[1])
  ) %>%
  ungroup()

#Check the frequency of sarthritis
table(hmhas_all_sp13$sarthritis_dm, exclude=NULL)
######################################################  
##Define doctor-diagnosed other chronic conditions
#Exclude hearte, heart problems as it was only recorded in waves 4-6
######################################################  
#Function for counting missing number for each chronic conditions for participating waves
count_missing_cd_by_part <- function(data, num_waves = 6, disease_suffix) {
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

hmhas_all_sp9$missing_count_hibpe <- count_missing_cd_by_part(data=hmhas_all_sp9, disease_suffix="hibpe")
hmhas_all_sp9$missing_count_diabe <- count_missing_cd_by_part(data=hmhas_all_sp9, disease_suffix="diabe")
hmhas_all_sp9$missing_count_cancre <- count_missing_cd_by_part(data=hmhas_all_sp9, disease_suffix="cancre")
hmhas_all_sp9$missing_count_respe <- count_missing_cd_by_part(data=hmhas_all_sp9, disease_suffix="respe")
hmhas_all_sp9$missing_count_hrtatte <- count_missing_cd_by_part(data=hmhas_all_sp9, disease_suffix="hrtatte")
hmhas_all_sp9$missing_count_stroke <- count_missing_cd_by_part(data=hmhas_all_sp9, disease_suffix="stroke")

#All indiviudals with maximum 1 missing values in other chronic diseases across waves
#The number of individuals with missing values and participating in two waves only
table(hmhas_all_sp9$missing_count_hibpe, hmhas_all_sp9$total_participations, exclude=NULL) #10 individuals participated in two waves with 1 missing value, 1 individual participated in three waves with 2 missing values
table(hmhas_all_sp9$missing_count_diabe, hmhas_all_sp9$total_participations, exclude=NULL) #20 individuals participated in two waves with 1 missing value and 1 individual participated in three waves with 2 missing values
table(hmhas_all_sp9$missing_count_cancre, hmhas_all_sp9$total_participations, exclude=NULL) #14 individuals participated in two waves with 1 missing value and 1 individual participated in three waves with 2 missing values
table(hmhas_all_sp9$missing_count_respe, hmhas_all_sp9$total_participations, exclude=NULL) #9 individuals participated in two waves with 1 missing value
table(hmhas_all_sp9$missing_count_hrtatte, hmhas_all_sp9$total_participations, exclude=NULL) #9 individuals participated in two waves with 1 missing value
table(hmhas_all_sp9$missing_count_stroke, hmhas_all_sp9$total_participations, exclude=NULL) #4 individuals participated in two waves with 1 missing value

#Mark those exceeds the upper limit of missing values
hmhas_all_sp9 <- hmhas_all_sp9 %>% mutate(
  hibpe_exclude=case_when(
    missing_count_hibpe==1 & total_participations==2 ~ 1,
    missing_count_hibpe==2 & total_participations==3 ~ 1,
    TRUE ~ 0), 
  diabe_exclude=case_when(
    missing_count_diabe==1 & total_participations==2 ~ 1,
    missing_count_diabe==2 & total_participations==3 ~ 1,
    TRUE ~ 0),
  cancre_exclude=case_when(
    missing_count_cancre==1 & total_participations==2 ~ 1,
    missing_count_cancre==2 & total_participations==3 ~ 1,
    TRUE ~ 0),
  respe_exclude=case_when(
    missing_count_respe==1 & total_participations==2 ~ 1,
    TRUE ~ 0),
  hrtatte_exclude=case_when(
    missing_count_hrtatte==1 & total_participations==2 ~ 1,
    TRUE ~ 0),
  stroke_exclude=case_when(
    missing_count_stroke==1 & total_participations==2 ~ 1,
    TRUE ~ 0))
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
hmhas_all_sp13 <- hmhas_all_sp13 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave!="Wave 1"~ NA_real_,
  inclusion_wave=="Wave 1" & if_any(c(r1hibpe, r2hibpe, r3hibpe, r4hibpe, r5hibpe, r6hibpe), ~ .x==1) ~ 1, 
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1diabe, r2diabe, r3diabe, r4diabe, r5diabe, r6diabe), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1cancre, r2cancre, r3cancre, r4cancre, r5cancre, r6cancre), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  respe=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1respe, r2respe, r3respe, r4respe, r5respe, r6respe), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  hrtatte=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1hrtatte, r2hrtatte, r3hrtatte, r4hrtatte, r5hrtatte, r6hrtatte), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1stroke, r2stroke, r3stroke, r4stroke, r5stroke, r6stroke), ~ .x==1) ~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp13$hibpe, hmhas_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 1, 2733 individuals with no high blood pressure/hypertension across 6 waves and 6170 individuals with high blood pressure/hypertension, 5 individuals with NA
table(hmhas_all_sp13$diabe, hmhas_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 1, 5760 individuals with no diabetes across 6 waves and 3137 individuals with diabetes, 11 individuals with NA
table(hmhas_all_sp13$cancre, hmhas_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 1, 8348 individuals with no cancer across 6 waves and 548 individuals with cancer, 12 individuals with NA
table(hmhas_all_sp13$respe, hmhas_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 1, 7529 individuals with no respiratory diseases across 6 waves and 1372 individuals with respiratory diseases, 7 individuals with NA
table(hmhas_all_sp13$hrtatte, hmhas_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 1, 7879 individuals with no heart attack across 6 waves and 1024 individuals with heart attack, 5 individuals with NA
table(hmhas_all_sp13$stroke, hmhas_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 1, 8239 individuals with no stroke across 6 waves and 666 individuals with stroke, 3 individuals with NA
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2
hmhas_all_sp14 <- hmhas_all_sp13 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave=="Wave 1" ~ hibpe,
  inclusion_wave!="Wave 2" ~ NA_real_, 
  inclusion_wave=="Wave 2" & if_any(c(r2hibpe, r3hibpe, r4hibpe,r5hibpe, r6hibpe), ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ diabe,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2diabe, r3diabe, r4diabe, r5diabe,r6diabe), ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ cancre,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2cancre, r3cancre, r4cancre,r5cancre,r6cancre), ~ .x==1) ~ 1,
    TRUE ~ 0),
  respe=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ respe,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2respe, r3respe, r4respe, r5respe, r6respe), ~ .x==1) ~ 1,
    TRUE ~ 0),
  hrtatte=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ hrtatte,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2hrtatte, r3hrtatte, r4hrtatte, r5hrtatte, r6hrtatte), ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ stroke,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2stroke, r3stroke, r4stroke, r5stroke, r6stroke), ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hmhas_all_sp14$hibpe, hmhas_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 2, 171 individuals with no high blood pressure/hypertension across 5 waves and 407 individuals with high blood pressure/hypertension, 2 individuals with NA
table(hmhas_all_sp14$diabe, hmhas_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 2, 382 individuals with no diabetes across 5 waves and 198 individuals with diabetes
table(hmhas_all_sp14$cancre, hmhas_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 2, 538 individuals with no cancer across 5 waves and 42 individuals with cancer
table(hmhas_all_sp14$respe, hmhas_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 2, 491 individuals with no respiratory diseases across 5 waves and 89 individuals with respiratory diseases
table(hmhas_all_sp14$hrtatte, hmhas_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 2, 506 individuals with no heart attack across 5 waves and 73 individuals with heart attack, 1 individual with NA
table(hmhas_all_sp14$stroke, hmhas_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 2, 548 individuals with no stroke across 5 waves and 32 individuals with stroke
######################################################
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 3
hmhas_all_sp15 <- hmhas_all_sp14 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ hibpe,
  inclusion_wave!="Wave 3" ~ NA_real_, 
  inclusion_wave=="Wave 3" & if_any(r3hibpe:r6hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ diabe,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3diabe:r6diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ cancre,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3cancre:r6cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  respe=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ respe,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3respe:r6respe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hrtatte=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hrtatte,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3hrtatte:r6hrtatte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ stroke,
    inclusion_wave!="Wave 3" ~ NA_real_, 
    inclusion_wave=="Wave 3" & if_any(r3stroke:r6stroke, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp15$hibpe, hmhas_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 3, 1778 individuals with no high blood pressure/hypertension across 4 waves and 2416 individuals with high blood pressure/hypertension, 2 individuals with NA
table(hmhas_all_sp15$diabe, hmhas_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 3, 2851 individuals with no diabetes across 4 waves and 1344 individuals with diabetes, 1 individuals with NA
table(hmhas_all_sp15$cancre, hmhas_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 3, 4021 individuals with no cancer across 4 waves and 173 individuals with cancer, 2 individuals with NA
table(hmhas_all_sp15$respe, hmhas_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 3, 3764 individuals with no respiratory diseases across 4 waves and 430 individuals with respiratory diseases, 2 individuals with NA
table(hmhas_all_sp15$hrtatte, hmhas_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 3, 3926 individuals with no heart attack across 4 waves and 270 individuals with heart attack
table(hmhas_all_sp15$stroke, hmhas_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 3, 4023 individuals with no stroke across 4 waves and 173 individuals with stroke
######################################################
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 4
hmhas_all_sp16 <- hmhas_all_sp15 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hibpe,
  inclusion_wave!="Wave 4" ~ NA_real_, 
  inclusion_wave=="Wave 4" & if_any(r4hibpe:r6hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ diabe,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4diabe:r6diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ cancre,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4cancre:r6cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  respe=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ respe,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4respe:r6respe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hrtatte=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hrtatte,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4hrtatte:r6hrtatte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ stroke,
    inclusion_wave!="Wave 4" ~ NA_real_, 
    inclusion_wave=="Wave 4" & if_any(r4stroke:r6stroke, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp16$hibpe, hmhas_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 4, 288 individuals with no high blood pressure/hypertension across 3 waves and 348 individuals with high blood pressure/hypertension
table(hmhas_all_sp16$diabe, hmhas_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 4, 440 individuals with no diabetes across 3 waves and 196 individuals with diabetes
table(hmhas_all_sp16$cancre, hmhas_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 4, 613 individuals with no cancer across 3 waves and 23 individuals with cancer
table(hmhas_all_sp16$respe, hmhas_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 4, 568 individuals with no respiratory diseases across 3 waves and 68 individuals with respiratory diseases
table(hmhas_all_sp16$hrtatte, hmhas_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 4, 585 individuals with no heart attack across 3 waves and 51 individuals with heart attack
table(hmhas_all_sp16$stroke, hmhas_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 4, 615 individuals with no stroke across 3 waves and 21 individuals with stroke
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 5
hmhas_all_sp17 <- hmhas_all_sp16 %>% mutate(hibpe=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hibpe,
  inclusion_wave!="Wave 5" ~ NA_real_, 
  inclusion_wave=="Wave 5" & if_any(r5hibpe:r6hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ diabe,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5diabe:r6diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ cancre,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5cancre:r6cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  respe=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ respe,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5respe:r6respe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hrtatte=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hrtatte,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5hrtatte:r6hrtatte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ stroke,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5stroke:r6stroke, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp17$hibpe, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 5, 1957 individuals with no high blood pressure/hypertension across 2 waves and 1313 individuals with high blood pressure/hypertension, 2 individuals with NA
table(hmhas_all_sp17$diabe, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 5, 2457 individuals with no diabetes across 2 waves and 806 individuals with diabetes, 9 individuals with NA
table(hmhas_all_sp17$cancre, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 5, 3185 individuals with no cancer across 2 waves and 86 individuals with cancer, 1 individual with NA
table(hmhas_all_sp17$respe, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 5, 3072 individuals with no respiratory diseases across 2 waves and 200 individuals with respiratory diseases
table(hmhas_all_sp17$hrtatte, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 5, 3142 individuals with no heart attack across 2 waves and 127 individuals with heart attack, 3 individuals with NA
table(hmhas_all_sp17$stroke, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 5, 3193 individuals with no stroke across 2 waves and 78 individuals with stroke, 1 individual with NA
######################################################
##Define doctor-diagnosed other chronic conditions among spousal pairs
###################################################### 
hmhas_all_sp17 <- hmhas_all_sp17 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    shibpe = ifelse(person_num == 1, hibpe[2], hibpe[1]),
    sdiabe = ifelse(person_num == 1, diabe[2], diabe[1]),  
    scancre = ifelse(person_num == 1, cancre[2], cancre[1]),
    srespe = ifelse(person_num == 1, respe[2], respe[1]),
    shrtatte = ifelse(person_num == 1, hrtatte[2], hrtatte[1]),
    sstroke = ifelse(person_num == 1, stroke[2], stroke[1])) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(hmhas_all_sp17$shibpe, hmhas_all_sp17$inclusion_wave, exclude=NULL)
table(hmhas_all_sp17$sdiabe, hmhas_all_sp17$inclusion_wave, exclude=NULL)
table(hmhas_all_sp17$scancre, hmhas_all_sp17$inclusion_wave, exclude=NULL)
table(hmhas_all_sp17$srespe, hmhas_all_sp17$inclusion_wave, exclude=NULL)
table(hmhas_all_sp17$shrtatte, hmhas_all_sp17$inclusion_wave, exclude=NULL)
table(hmhas_all_sp17$sstroke, hmhas_all_sp17$inclusion_wave, exclude=NULL)

##Compare the proportion of men with arthritis who had wives affected by other chronic conditions to those without
#High blood pressure/hypertension
male <- hmhas_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hibpe = male$shibpe
)
freq_table(freq) #74% versus 64%
chisq.test(male$arthritis, male$shibpe) #Significant

#High blood sugar/diabetes
male <- hmhas_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_diabe = male$sdiabe
)
freq_table(freq) #40% versus 34%
chisq.test(male$arthritis, male$sdiabe) #Significant

#Cancer
male <- hmhas_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_cancre = male$scancre
)
freq_table(freq) #7% versus 6%
chisq.test(male$arthritis, male$scancre) #Non-significant

#Respiratory diseases
male <- hmhas_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_respe = male$srespe
)
freq_table(freq) #18% versus 12%
chisq.test(male$arthritis, male$srespe) #Significant

#Heart attack
male <- hmhas_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hearte = male$shrtatte
)
freq_table(freq) #10% versus 7%
chisq.test(male$arthritis, male$shrtatte) #Significant

#Stroke
male <- hmhas_all_sp17 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_stroke= male$sstroke
)
freq_table(freq) #7% versus 5%
chisq.test(male$arthritis, male$sstroke) #Significant

##Compare the proportion of women with arthritis who had husbands affected by other chronic conditions to those without
#High blood pressure/hypertension
female <- hmhas_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hibpe = female$shibpe
)
freq_table(freq2) #61% versus 52%
chisq.test(female$arthritis, female$shibpe) #Significant

#High blood sugar/diabetes
female <- hmhas_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_diabe = female$sdiabe
)
freq_table(freq2) #31% versus 28%
chisq.test(female$arthritis, female$sdiabe) #Significant

#Cancer
female <- hmhas_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_cancre = female$scancre
)
freq_table(freq2) #4% versus 4%
chisq.test(female$arthritis, female$scancre) #Non-significant

#Respiratory diseases
female <- hmhas_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_lunge = female$srespe
)
freq_table(freq2) #15% versus 9%
chisq.test(female$arthritis, female$srespe) #Significant

#Heart attack
female <- hmhas_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hrtatte = female$shrtatte
)
freq_table(freq2) #12% versus 9%
chisq.test(female$arthritis, female$shrtatte) #Significant

#Stroke
female <- hmhas_all_sp17 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_stroke= female$sstroke
)
freq_table(freq2) #8% versus 5%
chisq.test(female$arthritis, female$sstroke) #Significant
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions 
#Requiring individuals not exceeding the upper limit missing values of doctor-diagnosed variables only as operator "OR" not "AND" was used
#The number of NA became smaller when including medication variables to define other chronic conditions, which is unexpected and should use the other chronic condition variables defined by doctor-diagnosed only to refine rows with NA
##No additional cases added when including medication/treatment variables
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
hmhas_all_sp17 <- hmhas_all_sp17 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave!="Wave 1"~ NA_real_,
  inclusion_wave=="Wave 1" & (if_any(r1hibpe:r6hibpe, ~ .x==1) | if_any(r1rxhibp:r6rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1diabe:r6diabe, ~ .x==1) | if_any(r1rxdiab:r6rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1cancre:r6cancre, ~ .x==1) | if_any(r1cncrchem:r6cncrchem, ~ .x==1) | if_any(r1cncrsurg:r6cncrsurg, ~ .x==1) | if_any(r1cncrradn:r6cncrradn, ~ .x==1) | if_any(r1cncrmeds:r6cncrmeds, ~ .x==1) | if_any(r1cncrothr:r6cncrothr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  respe_dm=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1respe:r6respe, ~ .x==1) | if_any(r1rxresp:r6rxresp, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hrtatte_dm=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1hrtatte:r6hrtatte, ~ .x==1) | if_any(r1rxhrtat:r6rxhrtat, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1stroke:r6stroke, ~ .x==1) | if_any(r1rxstrok:r6rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp17$hibpe_dm, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 2733 individuals with no high blood pressure/hypertension across 6 waves and 6170 individuals with high blood pressure/hypertension, 5 individuals with NA
table(hmhas_all_sp17$diabe_dm, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 5760 individuals with no diabetes across 6 waves and 3137 individuals with diabetes, 11 individuals with NA
table(hmhas_all_sp17$cancre_dm, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 8348 individuals with no cancer across 6 waves and 548 individuals with cancer, 12 individuals with NA
table(hmhas_all_sp17$respe_dm, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 7529 individuals with no respiratory diseases across 6 waves and 1372 individuals with respiratory diseases, 7 individuals with NA
table(hmhas_all_sp17$hrtatte_dm, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 7879 individuals with no heart attack across 6 waves and 1024 individuals with heart attack, 5 individuals with NA
table(hmhas_all_sp17$stroke_dm, hmhas_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 1, 8239 individuals with no stroke across 6 waves and 666 individuals with stroke, 3 individuals with NA
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2
hmhas_all_sp18 <- hmhas_all_sp17 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave=="Wave 1" ~ hibpe_dm,
  inclusion_wave!="Wave 2"~ NA_real_,
  inclusion_wave=="Wave 2" & (if_any(r2hibpe:r6hibpe, ~ .x==1) | if_any(r2rxhibp:r6rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ diabe_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2diabe:r6diabe, ~ .x==1) | if_any(r2rxdiab:r6rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ cancre_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2cancre:r6cancre, ~ .x==1) | if_any(r2cncrchem:r6cncrchem, ~ .x==1) | if_any(r2cncrsurg:r6cncrsurg, ~ .x==1) | if_any(r2cncrradn:r6cncrradn, ~ .x==1) | if_any(r2cncrmeds:r6cncrmeds, ~ .x==1) | if_any(r2cncrothr:r6cncrothr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  respe_dm=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ respe_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2respe:r6respe, ~ .x==1) | if_any(r2rxresp:r6rxresp, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hrtatte_dm=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ hrtatte_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2hrtatte:r6hrtatte, ~ .x==1) | if_any(r2rxhrtat:r6rxhrtat, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave=="Wave 1" ~ stroke_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2stroke:r6stroke, ~ .x==1) | if_any(r2rxstrok:r6rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp18$hibpe_dm, hmhas_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 171 individuals with no high blood pressure/hypertension across 5 waves and 407 individuals with high blood pressure/hypertension, 2 individuals with NA
table(hmhas_all_sp18$diabe_dm, hmhas_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 5760 individuals with no diabetes across 5 waves and 3137 individuals with diabetes, 11 individuals with NA
table(hmhas_all_sp18$cancre_dm, hmhas_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 538 individuals with no cancer across 5 waves and 42 individuals with cancer, 0 individuals with NA
table(hmhas_all_sp18$respe_dm, hmhas_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 491 individuals with no respiratory diseases across 5 waves and 89 individuals with respiratory diseases, 0 individuals with NA
table(hmhas_all_sp18$hrtatte_dm, hmhas_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 506 individuals with no heart attack across 5 waves and 73 individuals with heart attack, 1 individuals with NA
table(hmhas_all_sp18$stroke_dm, hmhas_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 2, 548 individuals with no stroke across 5 waves and 32 individuals with stroke, 0 individuals with NA
######################################################
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 3
hmhas_all_sp19 <- hmhas_all_sp18 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ hibpe_dm,
  inclusion_wave!="Wave 3"~ NA_real_,
  inclusion_wave=="Wave 3" & (if_any(r3hibpe:r6hibpe, ~ .x==1) | if_any(r3rxhibp:r6rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ diabe_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3diabe:r6diabe, ~ .x==1) | if_any(r3rxdiab:r6rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ cancre_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3cancre:r6cancre, ~ .x==1) | if_any(r3cncrchem:r6cncrchem, ~ .x==1) | if_any(r3cncrsurg:r6cncrsurg, ~ .x==1) | if_any(r3cncrradn:r6cncrradn, ~ .x==1) | if_any(r3cncrmeds:r6cncrmeds, ~ .x==1) | if_any(r3cncrothr:r6cncrothr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  respe_dm=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ respe_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3respe:r6respe, ~ .x==1) | if_any(r3rxresp:r6rxresp, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hrtatte_dm=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hrtatte_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3hrtatte:r6hrtatte, ~ .x==1) | if_any(r3rxhrtat:r6rxhrtat, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ stroke_dm,
    inclusion_wave!="Wave 3"~ NA_real_,
    inclusion_wave=="Wave 3" & (if_any(r3stroke:r6stroke, ~ .x==1) | if_any(r3rxstrok:r6rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp19$hibpe_dm, hmhas_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 3, 1778 individuals with no high blood pressure/hypertension across 4 waves and 2416 individuals with high blood pressure/hypertension, 2 individuals with NA
table(hmhas_all_sp19$diabe_dm, hmhas_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 3, 2851 individuals with no diabetes across 4 waves and 1344 individuals with diabetes, 1 individuals with NA
table(hmhas_all_sp19$cancre_dm, hmhas_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 3, 4021 individuals with no cancer across 4 waves and 173 individuals with cancer, 2 individuals with NA
table(hmhas_all_sp19$respe_dm, hmhas_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 3, 3764 individuals with no respiratory diseases across 4 waves and 430 individuals with respiratory diseases, 02individuals with NA
table(hmhas_all_sp19$hrtatte_dm, hmhas_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 3, 3926 individuals with no heart attack across 4 waves and 270 individuals with heart attack, 1 individuals with NA
table(hmhas_all_sp19$stroke_dm, hmhas_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 3, 4023 individuals with no stroke across 4 waves and 173 individuals with stroke, 0 individuals with NA
######################################################
#Inclusion in Wave 4
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 4
hmhas_all_sp20 <- hmhas_all_sp19 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hibpe_dm,
  inclusion_wave!="Wave 4"~ NA_real_,
  inclusion_wave=="Wave 4" & (if_any(r4hibpe:r6hibpe, ~ .x==1) | if_any(r4rxhibp:r6rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ diabe_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4diabe:r6diabe, ~ .x==1) | if_any(r4rxdiab:r6rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ cancre_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4cancre:r6cancre, ~ .x==1) | if_any(r4cncrchem:r6cncrchem, ~ .x==1) | if_any(r4cncrsurg:r6cncrsurg, ~ .x==1) | if_any(r4cncrradn:r6cncrradn, ~ .x==1) | if_any(r4cncrmeds:r6cncrmeds, ~ .x==1) | if_any(r4cncrothr:r6cncrothr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  respe_dm=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ respe_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4respe:r6respe, ~ .x==1) | if_any(r4rxresp:r6rxresp, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hrtatte_dm=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ hrtatte_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4hrtatte:r6hrtatte, ~ .x==1) | if_any(r4rxhrtat:r6rxhrtat, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3") ~ stroke_dm,
    inclusion_wave!="Wave 4"~ NA_real_,
    inclusion_wave=="Wave 4" & (if_any(r4stroke:r6stroke, ~ .x==1) | if_any(r4rxstrok:r6rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp20$hibpe_dm, hmhas_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 4, 288 individuals with no high blood pressure/hypertension across 3 waves and 348 individuals with high blood pressure/hypertension, 0 individuals with NA
table(hmhas_all_sp20$diabe_dm, hmhas_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 4, 440 individuals with no diabetes across 3 waves and 196 individuals with diabetes, 0 individuals with NA
table(hmhas_all_sp20$cancre_dm, hmhas_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 4, 613 individuals with no cancer across 3 waves and 23 individuals with cancer, 0 individuals with NA
table(hmhas_all_sp20$respe_dm, hmhas_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 4, 568 individuals with no respiratory diseases across 3 waves and 68 individuals with respiratory diseases, 0 individuals with NA
table(hmhas_all_sp20$hrtatte_dm, hmhas_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 4, 585 individuals with no heart attack across 3 waves and 51 individuals with heart attack, 0 individuals with NA
table(hmhas_all_sp20$stroke_dm, hmhas_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 4, 615 individuals with no stroke across 3 waves and 21 individuals with stroke, 0 individuals with NA
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 5
hmhas_all_sp21 <- hmhas_all_sp20 %>% mutate(hibpe_dm=case_when(
  hibpe_exclude==1 ~ NA_real_,
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hibpe_dm,
  inclusion_wave!="Wave 5"~ NA_real_,
  inclusion_wave=="Wave 5" & (if_any(r5hibpe:r6hibpe, ~ .x==1) | if_any(r5rxhibp:r6rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    diabe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ diabe_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5diabe:r6diabe, ~ .x==1) | if_any(r5rxdiab:r6rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    cancre_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ cancre_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5cancre:r6cancre, ~ .x==1) | if_any(r5cncrchem:r6cncrchem, ~ .x==1) | if_any(r5cncrsurg:r6cncrsurg, ~ .x==1) | if_any(r5cncrradn:r6cncrradn, ~ .x==1) | if_any(r5cncrmeds:r6cncrmeds, ~ .x==1) | if_any(r5cncrothr:r6cncrothr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  respe_dm=case_when(
    respe_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ respe_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5respe:r6respe, ~ .x==1) | if_any(r5rxresp:r6rxresp, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hrtatte_dm=case_when(
    hrtatte_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ hrtatte_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5hrtatte:r6hrtatte, ~ .x==1) | if_any(r5rxhrtat:r6rxhrtat, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    stroke_exclude==1 ~ NA_real_,
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 3", "Wave 4") ~ stroke_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5stroke:r6stroke, ~ .x==1) | if_any(r5rxstrok:r6rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hmhas_all_sp21$hibpe_dm, hmhas_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 5, 1957 individuals with no high blood pressure/hypertension across 2 waves and 1313 individuals with high blood pressure/hypertension, 2 individuals with NA
table(hmhas_all_sp21$diabe_dm, hmhas_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 5, 2457 individuals with no diabetes across 2 waves and 806 individuals with diabetes, 9 individuals with NA
table(hmhas_all_sp21$cancre_dm, hmhas_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 5, 3185 individuals with no cancer across 2 waves and 86 individuals with cancer, 1 individuals with NA
table(hmhas_all_sp21$respe_dm, hmhas_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 5, 3072 individuals with no respiratory diseases across 2 waves and 200 individuals with respiratory diseases, 0 individuals with NA
table(hmhas_all_sp21$hrtatte_dm, hmhas_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 5, 3142 individuals with no heart attack across 2 waves and 127 individuals with heart attack, 3 individuals with NA
table(hmhas_all_sp21$stroke_dm, hmhas_all_sp21$inclusion_wave, exclude=NULL) #For those included from wave 5, 3193 individuals with no stroke across 2 waves and 78 individuals with stroke, 1 individuals with NA
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions for the paired spouses
###################################################### 
hmhas_all_sp22 <- hmhas_all_sp21 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    shibpe_dm = ifelse(person_num == 1, hibpe_dm[2], hibpe_dm[1]),
    sdiabe_dm = ifelse(person_num == 1, diabe_dm[2], diabe_dm[1]),  
    scancre_dm = ifelse(person_num == 1, cancre_dm[2], cancre_dm[1]),
    srespe_dm = ifelse(person_num == 1, respe_dm[2], respe_dm[1]),
    shrtatte_dm = ifelse(person_num == 1, hrtatte_dm[2], hrtatte_dm[1]),
    sstroke_dm = ifelse(person_num == 1, stroke_dm[2], stroke_dm[1])) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(hmhas_all_sp22$shibpe_dm, hmhas_all_sp22$inclusion_wave, exclude=NULL)
table(hmhas_all_sp22$sdiabe_dm, hmhas_all_sp22$inclusion_wave, exclude=NULL)
table(hmhas_all_sp22$scancre_dm, hmhas_all_sp22$inclusion_wave, exclude=NULL)
table(hmhas_all_sp22$srespe_dm, hmhas_all_sp22$inclusion_wave, exclude=NULL)
table(hmhas_all_sp22$shrtatte_dm, hmhas_all_sp22$inclusion_wave, exclude=NULL)
table(hmhas_all_sp22$sstroke_dm, hmhas_all_sp22$inclusion_wave, exclude=NULL)

#No addtional cases added after including medication/treatment variables, check the frequencies of doctor-diagnosis and treatment variables for each chronic disease
table(hmhas_all_sp22$r1hibpe,hmhas_all_sp22$r1rxhibp, exclude=NULL)
table(hmhas_all_sp22$r2hibpe,hmhas_all_sp22$r2rxhibp, exclude=NULL)
table(hmhas_all_sp22$r3hibpe,hmhas_all_sp22$r3rxhibp, exclude=NULL)
table(hmhas_all_sp22$r4hibpe,hmhas_all_sp22$r4rxhibp, exclude=NULL)
table(hmhas_all_sp22$r5hibpe,hmhas_all_sp22$r5rxhibp, exclude=NULL)
table(hmhas_all_sp22$r6hibpe,hmhas_all_sp22$r6rxhibp, exclude=NULL)

table(hmhas_all_sp22$r1diabe,hmhas_all_sp22$r1rxdiab, exclude=NULL)
table(hmhas_all_sp22$r2diabe,hmhas_all_sp22$r2rxdiab, exclude=NULL)
table(hmhas_all_sp22$r3diabe,hmhas_all_sp22$r3rxdiab, exclude=NULL)
table(hmhas_all_sp22$r4diabe,hmhas_all_sp22$r4rxdiab, exclude=NULL)
table(hmhas_all_sp22$r5diabe,hmhas_all_sp22$r5rxdiab, exclude=NULL)
table(hmhas_all_sp22$r6diabe,hmhas_all_sp22$r6rxdiab, exclude=NULL)

table(hmhas_all_sp22$r1cancre,hmhas_all_sp22$r1cncrchem, exclude=NULL)
table(hmhas_all_sp22$r2cancre,hmhas_all_sp22$r2cncrchem, exclude=NULL)
table(hmhas_all_sp22$r3cancre,hmhas_all_sp22$r3cncrchem, exclude=NULL)
table(hmhas_all_sp22$r4cancre,hmhas_all_sp22$r4cncrchem, exclude=NULL)
table(hmhas_all_sp22$r5cancre,hmhas_all_sp22$r5cncrchem, exclude=NULL)
table(hmhas_all_sp22$r6cancre,hmhas_all_sp22$r6cncrchem, exclude=NULL)

table(hmhas_all_sp22$r1cancre,hmhas_all_sp22$r1cncrsurg, exclude=NULL)
table(hmhas_all_sp22$r2cancre,hmhas_all_sp22$r2cncrsurg, exclude=NULL)
table(hmhas_all_sp22$r3cancre,hmhas_all_sp22$r3cncrsurg, exclude=NULL)
table(hmhas_all_sp22$r4cancre,hmhas_all_sp22$r4cncrsurg, exclude=NULL)
table(hmhas_all_sp22$r5cancre,hmhas_all_sp22$r5cncrsurg, exclude=NULL)
table(hmhas_all_sp22$r6cancre,hmhas_all_sp22$r6cncrsurg, exclude=NULL)

table(hmhas_all_sp22$r1cancre,hmhas_all_sp22$r1cncrradn, exclude=NULL)
table(hmhas_all_sp22$r2cancre,hmhas_all_sp22$r2cncrradn, exclude=NULL)
table(hmhas_all_sp22$r3cancre,hmhas_all_sp22$r3cncrradn, exclude=NULL)
table(hmhas_all_sp22$r4cancre,hmhas_all_sp22$r4cncrradn, exclude=NULL)
table(hmhas_all_sp22$r5cancre,hmhas_all_sp22$r5cncrradn, exclude=NULL)
table(hmhas_all_sp22$r6cancre,hmhas_all_sp22$r6cncrradn, exclude=NULL)

table(hmhas_all_sp22$r1cancre,hmhas_all_sp22$r1cncrmeds, exclude=NULL)
table(hmhas_all_sp22$r2cancre,hmhas_all_sp22$r2cncrmeds, exclude=NULL)
table(hmhas_all_sp22$r3cancre,hmhas_all_sp22$r3cncrmeds, exclude=NULL)
table(hmhas_all_sp22$r4cancre,hmhas_all_sp22$r4cncrmeds, exclude=NULL)
table(hmhas_all_sp22$r5cancre,hmhas_all_sp22$r5cncrmeds, exclude=NULL)
table(hmhas_all_sp22$r6cancre,hmhas_all_sp22$r6cncrmeds, exclude=NULL)

table(hmhas_all_sp22$r1cancre,hmhas_all_sp22$r1cncrothr, exclude=NULL)
table(hmhas_all_sp22$r2cancre,hmhas_all_sp22$r2cncrothr, exclude=NULL)
table(hmhas_all_sp22$r3cancre,hmhas_all_sp22$r3cncrothr, exclude=NULL)
table(hmhas_all_sp22$r4cancre,hmhas_all_sp22$r4cncrothr, exclude=NULL)
table(hmhas_all_sp22$r5cancre,hmhas_all_sp22$r5cncrothr, exclude=NULL)
table(hmhas_all_sp22$r6cancre,hmhas_all_sp22$r6cncrothr, exclude=NULL)

table(hmhas_all_sp22$r1respe,hmhas_all_sp22$r1rxresp, exclude=NULL)
table(hmhas_all_sp22$r2respe,hmhas_all_sp22$r2rxresp, exclude=NULL)
table(hmhas_all_sp22$r3respe,hmhas_all_sp22$r3rxresp, exclude=NULL)
table(hmhas_all_sp22$r4respe,hmhas_all_sp22$r4rxresp, exclude=NULL)
table(hmhas_all_sp22$r5respe,hmhas_all_sp22$r5rxresp, exclude=NULL)
table(hmhas_all_sp22$r6respe,hmhas_all_sp22$r6rxresp, exclude=NULL)

table(hmhas_all_sp22$r1hrtatte,hmhas_all_sp22$r1rxhrtat, exclude=NULL)
table(hmhas_all_sp22$r2hrtatte,hmhas_all_sp22$r2rxhrtat, exclude=NULL)
table(hmhas_all_sp22$r3hrtatte,hmhas_all_sp22$r3rxhrtat, exclude=NULL)
table(hmhas_all_sp22$r4hrtatte,hmhas_all_sp22$r4rxhrtat, exclude=NULL)
table(hmhas_all_sp22$r5hrtatte,hmhas_all_sp22$r5rxhrtat, exclude=NULL)
table(hmhas_all_sp22$r6hrtatte,hmhas_all_sp22$r6rxhrtat, exclude=NULL)

table(hmhas_all_sp22$r1stroke,hmhas_all_sp22$r1rxstrok, exclude=NULL)
table(hmhas_all_sp22$r2stroke,hmhas_all_sp22$r2rxstrok, exclude=NULL)
table(hmhas_all_sp22$r3stroke,hmhas_all_sp22$r3rxstrok, exclude=NULL)
table(hmhas_all_sp22$r4stroke,hmhas_all_sp22$r4rxstrok, exclude=NULL)
table(hmhas_all_sp22$r5stroke,hmhas_all_sp22$r5rxstrok, exclude=NULL)
table(hmhas_all_sp22$r6stroke,hmhas_all_sp22$r6rxstrok, exclude=NULL) #All passed QC. No observations with medication/treatment==1 & doctor dianogsis==0
######################################################
##Define covariates at inclusion for respondents
#Measured BMI available only in waves 1-3, thus excluded and include only self-reported BMI (rwbmi, and rwbmicat)
#r3shlt is not available as self-reported health in wave 3 was recorded using other scale, use wave 4 data for wave 3
######################################################
#Define respondent covariates at inclusion for the three inclusion_wave groups
hmhas_all_sp23 <- hmhas_all_sp22 %>% mutate(
  rage=case_when(
    inclusion_wave=="Wave 1" ~ r1agey,
    inclusion_wave=="Wave 2" ~ r2agey,
    inclusion_wave=="Wave 3" ~ r3agey,
    inclusion_wave=="Wave 4" ~ r4agey,
    TRUE~ r5agey), 
  hrural=case_when(
    inclusion_wave=="Wave 1" ~ h1rural,
    inclusion_wave=="Wave 2" ~ h2rural,
    inclusion_wave=="Wave 3" ~ h3rural,
    inclusion_wave=="Wave 4" ~ h4rural,
    TRUE ~ h5rural),
  rshlt=case_when(
    inclusion_wave=="Wave 1" ~ r1shlt,
    inclusion_wave=="Wave 2" ~ r2shlt,
    inclusion_wave=="Wave 3" ~ r2shlt,
    inclusion_wave=="Wave 4" ~ r4shlt,
    TRUE~ r5shlt),
  radlfive=case_when(
    inclusion_wave=="Wave 1" ~ r1adlfive,
    inclusion_wave=="Wave 2" ~ r2adlfive,
    inclusion_wave=="Wave 3" ~ r3adlfive,
    inclusion_wave=="Wave 4" ~ r4adlfive,
    TRUE~ r5adlfive),
  riadlfour=case_when(
    inclusion_wave=="Wave 1" ~ r1iadlfour,
    inclusion_wave=="Wave 2" ~ r2iadlfour,
    inclusion_wave=="Wave 3" ~ r3iadlfour,
    inclusion_wave=="Wave 4" ~ r4iadlfour,
    TRUE~ r5iadlfour),
  rmobilsev=case_when(
    inclusion_wave=="Wave 1" ~ r1mobilsev,
    inclusion_wave=="Wave 2" ~ r2mobilsev,
    inclusion_wave=="Wave 3" ~ r3mobilsev,
    inclusion_wave=="Wave 4" ~ r4mobilsev,
    TRUE~ r5mobilsev),
  rpactvg=case_when(
    inclusion_wave=="Wave 1" ~ r1vigact, 
    inclusion_wave=="Wave 2" ~ r2vigact, 
    inclusion_wave=="Wave 3" ~ r3vigact, 
    inclusion_wave=="Wave 4" ~ r4vigact, 
    TRUE ~ r5vigact),
  rdrinkr=case_when(
    inclusion_wave=="Wave 1" ~ r1drinkn,
    inclusion_wave=="Wave 2" ~ r2drinkn,
    inclusion_wave=="Wave 3" ~ r3drinkn,
    inclusion_wave=="Wave 4" ~ r4drinkn,
    TRUE ~ r5drinkn),
  rdrinkb=case_when(
    inclusion_wave=="Wave 1" ~ r1drinkb,
    inclusion_wave=="Wave 2" ~ r2drinkb,
    inclusion_wave=="Wave 3" ~ r3drinkb,
    inclusion_wave=="Wave 4" ~ r4drinkb,
    TRUE ~ r5drinkb),
  rbinged=case_when(
    inclusion_wave=="Wave 1" ~ r1binged,
    inclusion_wave=="Wave 2" ~ r2binged,
    inclusion_wave=="Wave 3" ~ r3binged,
    inclusion_wave=="Wave 4" ~ r4binged,
    TRUE ~ r5binged),
  rsmokev=case_when(
    inclusion_wave=="Wave 1" ~ r1smokev,
    inclusion_wave=="Wave 2" ~ r2smokev,
    inclusion_wave=="Wave 3" ~ r3smokev,
    inclusion_wave=="Wave 4" ~ r4smokev,
    TRUE ~ r5smokev),
  ritearn=case_when(
    inclusion_wave=="Wave 1" ~ r1iearn,
    inclusion_wave=="Wave 2" ~ r2iearn,
    inclusion_wave=="Wave 3" ~ r3iearn,
    inclusion_wave=="Wave 4" ~ r4iearn,    
    TRUE ~ r5iearn),
  hitsemp=case_when(
    inclusion_wave=="Wave 1" ~ h1isemp,
    inclusion_wave=="Wave 2" ~ h2isemp,
    inclusion_wave=="Wave 3" ~ h3isemp,
    inclusion_wave=="Wave 4" ~ h4isemp,
    TRUE ~ h5isemp),
  hitcap=case_when(
    inclusion_wave=="Wave 1" ~ h1icap,
    inclusion_wave=="Wave 2" ~ h2icap,
    inclusion_wave=="Wave 3" ~ h3icap,
    inclusion_wave=="Wave 4" ~ h4icap,
    TRUE ~ h5icap),
  ripen=case_when(
    inclusion_wave=="Wave 1" ~ r1ipent,
    inclusion_wave=="Wave 2" ~ r2ipent,
    inclusion_wave=="Wave 3" ~ r3ipent,
    inclusion_wave=="Wave 4" ~ r4ipent,
    TRUE ~ r5ipent),
  rigxfr=case_when(
    inclusion_wave=="Wave 1" ~ r1igxfr,
    inclusion_wave=="Wave 2" ~ r2igxfr,
    inclusion_wave=="Wave 3" ~ r3igxfr,
    inclusion_wave=="Wave 4" ~ r4igxfr,
    TRUE ~ r5igxfr),
  ritothr=case_when(
    inclusion_wave=="Wave 1" ~ r1iothr,
    inclusion_wave=="Wave 2" ~ r2iothr,
    inclusion_wave=="Wave 3" ~ r3iothr,
    inclusion_wave=="Wave 4" ~ r4iothr,
    TRUE ~ r5iothr),
  hitot=case_when(
    inclusion_wave=="Wave 1" ~ h1itot,
    inclusion_wave=="Wave 2" ~ h2itot,
    inclusion_wave=="Wave 3" ~ h3itot,
    inclusion_wave=="Wave 4" ~ h4itot,
    TRUE ~ h5itot),
  hkcnt=case_when(
    inclusion_wave=="Wave 1" ~ h1kcnt,
    inclusion_wave=="Wave 2" ~ h2kcnt,
    inclusion_wave=="Wave 3" ~ h3kcnt,
    inclusion_wave=="Wave 4" ~ h4kcnt,
    TRUE ~ h5kcnt),
  roccup=case_when(
    inclusion_wave=="Wave 1" ~ r1lbrf_m,
    inclusion_wave=="Wave 2" ~ r2lbrf_m,
    inclusion_wave=="Wave 3" ~ r3lbrf_m,
    inclusion_wave=="Wave 4" ~ r4lbrf_m,
    TRUE ~ r5lbrf_m),
  rbmin=case_when(
    inclusion_wave=="Wave 1" ~ r1bmi,
    inclusion_wave=="Wave 2" ~ r2bmi,
    inclusion_wave=="Wave 3" ~ r3bmi,
    inclusion_wave=="Wave 4" ~ r4bmi,
    TRUE ~ r5bmi),
  rbmicat=case_when(
    inclusion_wave=="Wave 1" ~ r1bmicat,
    inclusion_wave=="Wave 2" ~ r2bmicat,
    inclusion_wave=="Wave 3" ~ r3bmicat,
    inclusion_wave=="Wave 4" ~ r4bmicat,
    TRUE ~ r5bmicat),
  rclstress=case_when(
    inclusion_wave=="Wave 1" & (h1chdeathe==1 | (!is.na(raserchlth) & raserchlth==1) | (!is.na(rachhdinj) & rachhdinj==1)) ~ 1,
    inclusion_wave=="Wave 2" & (h2chdeathe==1 | (!is.na(raserchlth) & raserchlth==1) | (!is.na(rachhdinj) & rachhdinj==1)) ~ 1,
    inclusion_wave=="Wave 3" & (h3chdeathe==1 | (!is.na(raserchlth) & raserchlth==1) | (!is.na(rachhdinj) & rachhdinj==1)) ~ 1,
    inclusion_wave=="Wave 4" & (h4chdeathe==1 | (!is.na(raserchlth) & raserchlth==1) | (!is.na(rachhdinj) & rachhdinj==1)) ~ 1,
    inclusion_wave=="Wave 5" & (h5chdeathe==1 | (!is.na(raserchlth) & raserchlth==1) | (!is.na(rachhdinj) & rachhdinj==1)) ~ 1,
    inclusion_wave=="Wave 1" & (h1chdeathe==0 & (!is.na(raserchlth) & raserchlth==0) & (!is.na(rachhdinj) & rachhdinj==0)) ~ 0,
    inclusion_wave=="Wave 2" & (h2chdeathe==0 & (!is.na(raserchlth) & raserchlth==0) & (!is.na(rachhdinj) & rachhdinj==0)) ~ 0,
    inclusion_wave=="Wave 3" & (h3chdeathe==0 & (!is.na(raserchlth) & raserchlth==0) & (!is.na(rachhdinj) & rachhdinj==0)) ~ 0,
    inclusion_wave=="Wave 4" & (h4chdeathe==0 & (!is.na(raserchlth) & raserchlth==0) & (!is.na(rachhdinj) & rachhdinj==0)) ~ 0,
    inclusion_wave=="Wave 5" & (h5chdeathe==0 & (!is.na(raserchlth) & raserchlth==0) & (!is.na(rachhdinj) & rachhdinj==0)) ~ 0,
    TRUE ~ NA_real_))

#Check for inconsistencies in household variables
inconsistent_households <- hmhas_all_sp23 %>%
  group_by(householdID) %>%
  summarise(
    across(c(hrural, hitsemp, hitcap, hitot, hkcnt), 
           ~ length(unique(.)) > 1,  # Returns TRUE if more than one unique value
           .names = "{.col}_inconsistent"
    )
  ) %>%
  filter(if_any(ends_with("_inconsistent"), ~ .))  # Keep only households with any inconsistency
#4 individuals with inconsistent hrural

#Select those with inconsistent household hrural variable
qc <- inconsistent_households %>% filter(hrural_inconsistent=="TRUE")
qc2 <- hmhas_all_sp23 %>% filter(householdID %in% qc$householdID) %>% dplyr::select("householdID", "hrural") #4 spousal pairs with incosistent living area

#Refine hrural variable in individuals with inconsistency and convert the values to NA for both spouses
hmhas_all_sp23 <- hmhas_all_sp23 %>%
  group_by(householdID) %>%
  mutate(
    hrural2=case_when(
      first(hrural)==last(hrural) ~ hrural,
      first(hrural) != last(hrural) ~ NA_real_)) %>%
  ungroup()
######################################################
##Define covariates at inclusion for the paired spouses
######################################################
#Define spouses' covariates at inclusion for the three inclusion_wave groups
hmhas_all_sp23 <- hmhas_all_sp23 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sbyear = ifelse(person_num == 1, rabyear[2], rabyear[1]),
    sage = ifelse(person_num == 1, rage[2], rage[1]),
    sgender = ifelse(person_num == 1, ragender[2], ragender[1]),  
    seducl = ifelse(person_num == 1, raeducl[2], raeducl[1]),
    sshlt = ifelse(person_num == 1, rshlt[2], rshlt[1]),
    sadlfive = ifelse(person_num == 1, radlfive[2], radlfive[1]),
    siadlfour = ifelse(person_num == 1, riadlfour[2], riadlfour[1]),
    smobilsev = ifelse(person_num == 1, rmobilsev[2], rmobilsev[1]),
    spactvg = ifelse(person_num == 1, rpactvg[2], rpactvg[1]),
    sdrinkb = ifelse(person_num == 1, rdrinkb[2], rdrinkb[1]),
    sdrinkr = ifelse(person_num == 1, rdrinkr[2], rdrinkr[1]),
    sbinged = ifelse(person_num == 1, rbinged[2], rbinged[1]),
    ssmokev = ifelse(person_num == 1, rsmokev[2], rsmokev[1]),
    sitearn = ifelse(person_num == 1, ritearn[2], ritearn[1]),
    sipen = ifelse(person_num == 1, ripen[2], ripen[1]),
    sigxfr = ifelse(person_num == 1, rigxfr[2], rigxfr[1]),
    sitothr = ifelse(person_num == 1, ritothr[2], ritothr[1]),
    smomeducl = ifelse(person_num == 1, rameduc_m[2], rameduc_m[1]),
    sdadeducl = ifelse(person_num == 1, rafeduc_m[2], rafeduc_m[1]),
    soccup = ifelse(person_num == 1, roccup[2], roccup[1]),
    sbmin = ifelse(person_num == 1, rbmin[2], rbmin[1]),
    sbmicat = ifelse(person_num == 1, rbmicat[2], rbmicat[1]),
    sclstress = ifelse(person_num == 1, rclstress[2], rclstress[1])) %>%
  ungroup()

#Define household income=ritearn+sitearn+hitcap+ripen+sipen+rigxfr+sigxfr+hiothhh
hmhas_all_sp23 <- hmhas_all_sp23 %>% mutate(hincome=case_when(
  !is.na(ritearn) & !is.na(sitearn) & !is.na(hitcap) & !is.na(ripen) & !is.na(sipen) & !is.na(rigxfr) & !is.na(sigxfr) & !is.na(ritothr) & !is.na(sitothr) ~ ritearn+sitearn+hitcap+ripen+sipen+rigxfr+sigxfr+ritothr+sitothr,
  TRUE ~ NA))

##Check potential misdefined variables
#Age at interview
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 1" & rage != r1agey) | (inclusion_wave=="Wave 2" & rage != r2agey )| (inclusion_wave=="Wave 4" & rage != r4agey)) #None
#Self-reported health
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 6" & rshlt != r6shlt) | (inclusion_wave=="Wave 5" & rshlt != r5shlt)| (inclusion_wave=="Wave 4" & rshlt != r4shlt)) #None
#ADL summary
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 1" & radlfive != r1adlfive) | (inclusion_wave=="Wave 2" & radlfive != r2adlfive) | (inclusion_wave=="Wave 4" & radlfive != r4adlfive)) #None
#Mobility summary
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 4" & rmobilsev != r4mobilsev) | (inclusion_wave=="Wave 5" & rmobilsev != r5mobilsev) | (inclusion_wave=="Wave 6" & rmobilsev != r6mobilsev)) #None
#Physical activity 
qc <- hmhas_all_sp23 %>% select(inclusion_wave, r1vigact:r6vigact, rpactvg)
qc <- hmhas_all_sp23 %>% filter(inclusion_wave=="Wave 1" & rpactvg==1) #All "r1vigact" should equal to 1
table(qc$r1vigact,exclude=NULL) #Passed QC
qc <- hmhas_all_sp23 %>% filter(inclusion_wave=="Wave 2" & rpactvg==1) #All "r1vigact" should equal to 1
table(qc$r2vigact,exclude=NULL) #Passed QC
qc <- hmhas_all_sp23 %>% filter(inclusion_wave=="Wave 5" & rpactvg==1) #All "r1vigact" should equal to 1
table(qc$r5vigact,exclude=NULL) #Passed QC
#Number of drinks/day when drinks
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 1" & rdrinkr != r1drinkn) | (inclusion_wave=="Wave 2" & rdrinkr != r2drinkn)| (inclusion_wave=="Wave 4" & rdrinkr != r4drinkn)) #None
#Whether the respondent ever binge drinks
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 6" & rdrinkb != r6drinkb) | (inclusion_wave=="Wave 5" & rdrinkb != r5drinkb)| (inclusion_wave=="Wave 4" & rdrinkb != r4drinkb)) #None
#Number of days binge drinks
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 6" & rbinged != r6binged) | (inclusion_wave=="Wave 5" & rbinged != r5binged)| (inclusion_wave=="Wave 4" & rbinged != r4binged)) #None
#Ever smoking
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 1" & rsmokev != r1smokev) | (inclusion_wave=="Wave 2" & rsmokev != r2smokev)| (inclusion_wave=="Wave 4" & rsmokev != r4smokev)) #None
#Household's total income at an annual-level
qc <- hmhas_all_sp23 %>% filter(is.na(hincome)) %>% select(unhhidnp,householdID,inclusion_wave, ritearn, sitearn, hitcap, ripen, sipen,rigxfr, sigxfr, ritothr, sitothr, hitot, hincome) #28 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC
qc <- hmhas_all_sp23 %>% filter(hitot != hincome) %>% mutate(difference=hitot-hincome) %>%
       select(unhhidnp,householdID,inclusion_wave, ritearn, sitearn, hitcap, ripen, sipen,rigxfr, sigxfr, ritothr, sitothr, hitot, hincome, difference) #728 individuals with inconsistent hitot and hincome, but the difference was small
qc <- hmhas_all_sp23 %>% filter(is.na(hitot)) #28 missing values
qc <- hmhas_all_sp23 %>% filter(is.na(hincome)) #28 missing values
#Use hitot in the analysis given there is no significant difference in value and missingness compared to hincome

#Weekly contact with children in person/by phone/email
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 1" & hkcnt != h1kcnt) | (inclusion_wave=="Wave 2" & hkcnt != h2kcnt)| (inclusion_wave=="Wave 4" & hkcnt != h4kcnt)) #None
#Occupation
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 1" & roccup != r1lbrf_m) | (inclusion_wave=="Wave 2" & roccup != r2lbrf_m) | (inclusion_wave=="Wave 4" & roccup != r4lbrf_m)) #None
#Self-reported BMI
qc <- hmhas_all_sp23 %>% filter((inclusion_wave=="Wave 1" & rbmin != r1bmi) | (inclusion_wave=="Wave 2" & rbmin != r2bmi)| (inclusion_wave=="Wave 4" & rbmin != r4bmi)) #None
#Childhood/lifetime stressful events including
#Ever experienced death of own child
#Had serious health problem for 1+ mo before age 10
#Had a serious head injury before age 10 
qc <- hmhas_all_sp23 %>% filter(is.na(rclstress)) %>% select(unhhidnp,householdID,inclusion_wave, h1chdeathe:h6chdeathe, raserchlth, rachhdinj, rclstress) #966 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC

#Save dataset hshare_all_sp25
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/MHAS')
save(hmhas_all_sp23, file = "hmhas_all_sp23.rda")
######################################################

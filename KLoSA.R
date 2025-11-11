#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20251002
#UPDATED: 20251111
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE KLoSA 2006-2022 wave (1-9) DATA STRUCTURE, DATA PREPARATION, PERFORM STATISTICAL ANALYSES 
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#Harmonized KLoSA waves 1 to 9
  
#Logbook
######################################################  
######################################################

#Things to pay attention
###################################################### 
#20251105 There was no sample refreshment after wave 4
#20251111 Cataract is defined based on the variable "rwcatrcte" (ever had cataract surgery), those without surgery would be missed, exclude from analysis
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
#2. The number of eligible spousal pairs in each wave (using harmonized data)
#3. The number of individuals died during follow-up (hklosa_all_sp3)
#4. Descriptive data of selected baseline and outcome variables in hklosa_all_sp22


#DATA PREPARATION
#1. List of included variables  
#2. Define outcomes and potential confounders to be adjusted in hklosa_all_sp3
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
#3. The number of individuals died during follow-up (hklosa_all_sp3)
#4. Descriptive data of selected baseline and outcome variables in hklosa_all_sp22
######################################################
#1. Participation of individuals across waves (using harmonized data)
######################################################
#Load harmonized ELSA data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/KLoSA/Data')
hklosa <- read_stata("GH_KLoSA_f.dta")

##Wave 1
#The total number of individual #10254
table(hklosa$inw1) 
#QC: Frequency of h1coupid (max num should be 2)
table(as.data.frame(table(hklosa$h1coupid))$Freq) #Passed QC
#The total number of spousal pairs #3490
hklosa_1_sp <- hklosa %>% filter(inw1==1) %>% add_count(h1coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h1coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 2
#The total number of individual #8688
table(hklosa$inw2) 
#QC: Frequency of h2coupid (max num should be 2)
table(as.data.frame(table(hklosa$h2coupid))$Freq) #Passed QC
#The total number of spousal pairs #2999
hklosa_2_sp <- hklosa %>% filter(inw2==1) %>% add_count(h2coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h2coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 3
#The total number of individual #7920
table(hklosa$inw3) 
#QC: Frequency of h3coupid (max num should be 2)
table(as.data.frame(table(hklosa$h3coupid))$Freq) #Passed QC
#The total number of spousal pairs #2712
hklosa_3_sp <- hklosa %>% filter(inw3==1) %>% add_count(h3coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h3coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 4
#The total number of individual #7486
table(hklosa$inw4) 
#QC: Frequency of h4coupid (max num should be 2)
table(as.data.frame(table(hklosa$h4coupid))$Freq) #Passed QC
#The total number of spousal pairs #2560
hklosa_4_sp <- hklosa %>% filter(inw4==1) %>% add_count(h4coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h4coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 5
#The total number of individual #7949
table(hklosa$inw5) 
#QC: Frequency of h5coupid (max num should be 2)
table(as.data.frame(table(hklosa$h5coupid))$Freq) #Passed QC
#The total number of spousal pairs #2652
hklosa_5_sp <- hklosa %>% filter(inw5==1) %>% add_count(h5coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h5coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 6
#The total number of individual #7490
table(hklosa$inw6) 
#QC: Frequency of h6coupid (max num should be 2)
table(as.data.frame(table(hklosa$h6coupid))$Freq) #Passed QC
#The total number of spousal pairs #2442
hklosa_6_sp <- hklosa %>% filter(inw6==1) %>% add_count(h6coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h6coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 7
#The total number of individual #6940
table(hklosa$inw7) 
#QC: Frequency of h7coupid (max num should be 2)
table(as.data.frame(table(hklosa$h7coupid))$Freq) #Passed QC
#The total number of spousal pairs #2272
hklosa_7_sp <- hklosa %>% filter(inw7==1) %>% add_count(h7coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h7coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 8
#The total number of individual #6488
table(hklosa$inw8) 
#QC: Frequency of h8coupid (max num should be 2)
table(as.data.frame(table(hklosa$h8coupid))$Freq) #Passed QC
#The total number of spousal pairs #2116
hklosa_8_sp <- hklosa %>% filter(inw8==1) %>% add_count(h8coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h8coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 9
#The total number of individual #6057
table(hklosa$inw9) 
#QC: Frequency of h9coupid (max num should be 2)
table(as.data.frame(table(hklosa$h9coupid))$Freq) #Passed QC
#The total number of spousal pairs #1951
hklosa_9_sp <- hklosa %>% filter(inw9==1) %>% add_count(h9coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h9coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

######################################################
#2. The number of eligible spousal pairs in each wave (using harmonized data)
######################################################
#Inclusion criteria: For each dataset, heterosexual spousal pairs in which both partners participated in at least two survey waves will be included. In cases where multiple spouses were recorded for an individual, only the first will be retained. Both spouses must have complete data on age, gender, and doctor-diagnosed arthritis.
#Exclusion criteria: To ensure the reliability of responses, spousal pairs in which either partner reported a doctor-diagnosed memory-related condition (e.g., dementia or Alzheimer’s disease) or was receiving treatment for such conditions will be excluded. Additionally, proxy interviews—often indicative of cognitive impairment—will also be excluded. 
#Self-reported doctor diagnosed memory-related conditions available from waves 7 to 9
######################################################
#Load data and select relevant variables
#Harmonized data_waves_1_to_9
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/KLoSA/Data')
hklosa <- read_dta("GH_KLoSA_f.dta") %>% select(1,2, 5:13, 23:67, 70:88, 122:130, 200:267, 288:315, 370:387, 406:441, 892:945, 1108:1161, 1216:1617, 1850:1867, 1904:1939, 2012:2079, 2368:2403, 2458:2475, 2584:2637, 3072:3107, 4123:4186, 4251:4474, 4540:4548, 4684:4743, 4838:4854, 4926:4979, 5151:5222, 5637:5672, 5817:5834, 5871:5888)
######################################################
##Wave 1 ##6584 individuals and 3292 spousal pairs participating from wave 1
######################################################
#all spousal pairs
hklosa_w1 <- hklosa %>% filter(inw1==1 & !is.na(s1pid) & s1pid != 0 & !is.na(s1iwstat) & s1iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(pid, s1pid), 
                      pmax(pid, s1pid), 
                      sep = "_")) #6980 individuals and 3490 spousal pairs
table(as.data.frame(table(hklosa_w1$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hklosa_w1$h1cpl) # all 1

#QC: age distribution, should be 45 or older
table(hklosa_w1$r1agey, exclude=NULL)
table(hklosa_w1$s1agey, exclude=NULL) #Cutoff 18 years, all above 18 years

#SKIPPED:Exclude spousal pairs with either of spouse with missing age values
#hshare_w1_2 <- hshare_w1 %>% filter(r1agey >=18 & s1agey >=18) #18542 individuals and 9271 spousal pairs
#table(as.data.frame(table(hshare_w1_2$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hklosa_w1 <- hklosa_w1 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre:r9arthre)))))
table(hklosa_w1$missing_count_rarthre, exclude=NULL)
#3716 individuals with no missing, 539 individuals with 1 missing value, 389 individuals with 2 missing values, 352 individuals with 3 missing values
#280 individuals with 4 missing values, 312 individuals with 5 missing values, 325 individuals with 6 misisng values
#425 individuals with 7 missing values, 641 individuals with 8 missing values, and 1 individual with 9 missing values

#Exclude the individual with all rwarthre missing, as well as the paired spouse
qc <- hklosa_w1 %>% filter(missing_count_rarthre==9)
hklosa_w1_2 <- hklosa_w1 %>% filter(!householdID %in% qc$householdID) #6978 individuals and 3489 spousal pairs
table(as.data.frame(table(hklosa_w1_2$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: the number of spousal pairs with doctor-diagnosed/medication-indicated memory-related disease (no medication data available) across 9 waves
qc <- hklosa_w1_2 %>% filter((r7alzdeme==1 & !is.na(r7alzdeme))| (r7rxalzdem==1 & !is.na(r7rxalzdem)) |
                             (r8alzdeme==1 & !is.na(r8alzdeme))| (r8rxalzdem==1 & !is.na(r8rxalzdem)) |
                             (r9alzdeme==1 & !is.na(r9alzdeme)) | (r8rxalzdem==1 & !is.na(r8rxalzdem)))%>% select(pid, householdID, r7alzdeme:r9alzdeme, r7rxalzdem:r9rxalzdem, s7alzdeme:s9alzdeme, s7rxalzdem:s9rxalzdem) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #207 individuals, with 10 are spousal pairs and 187 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed/medication-indicated memory-related disease across 9 waves 
hklosa_w1_3 <- hklosa_w1_2 %>% filter(!householdID %in% qc$householdID)  #6584 individuals and 3292 spousal pairs
table(as.data.frame(table(hklosa_w1_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hklosa_w1_3$r1iwstat, exclude=NULL) #all 1
table(hklosa_w1_3$s1iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hklosa_w1_3 <- hklosa_w1_3 %>%   mutate(
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
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hklosa_w1_2$total_participations, exclude=NULL) #640 individuals participated in wave 1 only
qc <- as.data.frame(table(hklosa_w1_3$spousal_part_pattern, exclude=NULL)) #107 patterns

#Check the number of individual participated only in wave 1
qc <- hklosa_w1_3 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #640 individuals, 250 spousal pairs and 134 individuals
######################################################
##Wave 2 ##2 individuals and 1 spousal pairs participating from wave 2
######################################################
#all spousal pairs
hklosa_w2 <- hklosa %>% filter(inw2==1 & !is.na(s2pid) & s2pid != 0 & !is.na(s2iwstat) & s2iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(pid, s2pid), 
                      pmax(pid, s2pid), 
                      sep = "_")) #5998 individuals and 2999 spousal pairs
table(as.data.frame(table(hklosa_w2$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hklosa_w2$h2cpl) # all 1

#Exclude individuals included in hmhas_w1_2
hklosa_w2_2 <- hklosa_w2 %>% filter(!householdID %in% hklosa_w1_3$householdID) #376 individuals, with 188 spousal pairs 
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_w2_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, should be 45 or older
table(hklosa_w2_2$r2agey, exclude=NULL)
table(hklosa_w2_2$s2agey, exclude=NULL) #Cutoff 18 years, all above 18 years

#SKIPPED:Exclude spousal pairs with either of spouse with missing age values
#hshare_w1_2 <- hshare_w1 %>% filter(r1agey >=18 & s1agey >=18) #18542 individuals and 9271 spousal pairs
#table(as.data.frame(table(hshare_w1_2$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hklosa_w2_2 <- hklosa_w2_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre:r9arthre)))))
table(hklosa_w2_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: the number of spousal pairs with doctor-diagnosed/medication-indicated memory-related disease (no medication data available) across 9 waves
qc <- hklosa_w2_2 %>% filter((r7alzdeme==1 & !is.na(r7alzdeme))| (r7rxalzdem==1 & !is.na(r7rxalzdem)) |
                               (r8alzdeme==1 & !is.na(r8alzdeme))| (r8rxalzdem==1 & !is.na(r8rxalzdem)) |
                               (r9alzdeme==1 & !is.na(r9alzdeme)) | (r8rxalzdem==1 & !is.na(r8rxalzdem)))%>% select(pid, householdID, r7alzdeme:r9alzdeme, r7rxalzdem:r9rxalzdem, s7alzdeme:s9alzdeme, s7rxalzdem:s9rxalzdem) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #197 individuals, with 10 are spousal pairs and 177 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed/medication-indicated memory-related disease across 9 waves 
hklosa_w2_3 <- hklosa_w2_2 %>% filter(!householdID %in% qc$householdID)  #2 individuals and 1 spousal pair
table(as.data.frame(table(hklosa_w2_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hklosa_w2_3$r3iwstat, exclude=NULL) #all 1
table(hklosa_w2_3$s3iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hklosa_w2_3 <- hklosa_w2_3 %>%   mutate(
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
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hklosa_w2_3$total_participations, exclude=NULL) #0 individuals participated in wave 1 only
qc <- as.data.frame(table(hklosa_w2_3$spousal_part_pattern, exclude=NULL)) #73 patterns
######################################################
##Wave 3 ##No spousal pairs participating from wave 3
######################################################
#All spousal pairs participating in wave 3
hklosa_w3 <- hklosa %>% filter(inw3==1 & !is.na(s3pid) & s3pid != 0 & !is.na(s3iwstat) & s3iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(pid, s3pid), 
                      pmax(pid, s3pid), 
                      sep = "_")) #5424 individuals, with 2712 spousal pairs
table(as.data.frame(table(hklosa_w3$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hklosa_w3$h3cpl) # all 1

#Exclude individuals included in hklosa_w1_3, hklosa_w2_3
hklosa_w3_2 <- hklosa_w3 %>% filter(!(householdID %in% hklosa_w1_3$householdID | householdID %in% hklosa_w2_3$householdID)) #352 individuals, with 176 spousal pairs 
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_w3_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 18 and above
table(hklosa_w3_2$r3agey, exclude=NULL)
table(hklosa_w3_2$s3agey, exclude=NULL) #All respondents aged above 18 

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hklosa_w3_2 <- hklosa_w3_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre,r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hklosa_w3_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: the number of spousal pairs with doctor-diagnosed/medication-indicated memory-related disease (no medication data available) across 9 waves
qc <- hklosa_w3_2 %>% filter((r7alzdeme==1 & !is.na(r7alzdeme))| (r7rxalzdem==1 & !is.na(r7rxalzdem)) |
                               (r8alzdeme==1 & !is.na(r8alzdeme))| (r8rxalzdem==1 & !is.na(r8rxalzdem)) |
                               (r9alzdeme==1 & !is.na(r9alzdeme)) | (r8rxalzdem==1 & !is.na(r8rxalzdem)))%>% select(pid, householdID, r7alzdeme:r9alzdeme, r7rxalzdem:r9rxalzdem, s7alzdeme:s9alzdeme, s7rxalzdem:s9rxalzdem) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #186 individuals, with 10 are spousal pairs and 166 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed/medication-indicated memory-related disease across 9 waves 
hklosa_w3_3 <- hklosa_w3_2 %>% filter(!householdID %in% qc$householdID)  #0 individuals
######################################################
##Wave 4 ##No spousal pairs participating from wave 4
######################################################
#All spousal pairs participating in wave 4
hklosa_w4 <- hklosa %>% filter(inw4==1 & !is.na(s4pid) & s4pid != 0 & !is.na(s4iwstat) & s4iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(pid, s4pid), 
                      pmax(pid, s4pid), 
                      sep = "_")) #5120 individuals, with 2560 spousal pairs 
table(as.data.frame(table(hklosa_w4$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hklosa_w4$h4cpl) # all 1

#Exclude individuals included in hklosa_w1_3 or hklosa_w2_3 
hklosa_w4_2 <- hklosa_w4 %>% filter(!(householdID %in% hklosa_w1_3$householdID | householdID %in% hklosa_w2_3$householdID)) #340 individuals, with 170 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_w4_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 18 and above
table(hklosa_w4_2$r4agey, exclude=NULL)
table(hklosa_w4_2$s4agey, exclude=NULL) #All respondents aged above 18

#SKIPPED:Exclude spousal pairs with either of spouse with missing age values
#hshare_w2_3 <- hshare_w2_2 %>% filter(r2agey >=18 & s2agey >=18) #13066 individuals and 6533 spousal pairs
#table(as.data.frame(table(hshare_w2_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hklosa_w4_2 <- hklosa_w4_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hklosa_w4_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: the number of spousal pairs with doctor-diagnosed/medication-indicated memory-related disease (no medication data available) across 9 waves
qc <- hklosa_w4_2 %>% filter((r7alzdeme==1 & !is.na(r7alzdeme))| (r7rxalzdem==1 & !is.na(r7rxalzdem)) |
                               (r8alzdeme==1 & !is.na(r8alzdeme))| (r8rxalzdem==1 & !is.na(r8rxalzdem)) |
                               (r9alzdeme==1 & !is.na(r9alzdeme)) | (r8rxalzdem==1 & !is.na(r8rxalzdem)))%>% select(pid, householdID, r7alzdeme:r9alzdeme, r7rxalzdem:r9rxalzdem, s7alzdeme:s9alzdeme, s7rxalzdem:s9rxalzdem) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #180 individuals, with 10 are spousal pairs and 160 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed/medication-indicated memory-related disease across 9 waves 
hklosa_w4_3 <- hklosa_w4_2 %>% filter(!householdID %in% qc$householdID)  #0 individual
######################################################
##Wave 5 ##546 individuals and 273 spousal pairs participating from wave 5
######################################################
#All spousal pairs participating in wave 5
hklosa_w5 <- hklosa %>% filter(inw5==1 & !is.na(s5pid) & s5pid != 0 & !is.na(s5iwstat) & s5iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(pid, s5pid), 
                      pmax(pid, s5pid), 
                      sep = "_")) #5304 individuals, with 2652 spousal pairs  
table(as.data.frame(table(hklosa_w5$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hklosa_w5$h5cpl) # all 1

#Exclude individuals included in hklosa_w1_3 or hklosa_w2_3
hklosa_w5_2 <- hklosa_w5 %>% filter(!(householdID %in% hklosa_w1_3$householdID | householdID %in% hklosa_w2_3$householdID)) #878 individuals, with  439 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_w5_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 18 and above
table(hklosa_w5_2$r5agey, exclude=NULL)
table(hklosa_w5_2$s5agey, exclude=NULL) #All above 18

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hklosa_w5_2 <- hklosa_w5_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre, r5arthre, r6arthre, r7arthre, r8arthre, r9arthre))))) 
table(hklosa_w5_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: the number of spousal pairs with doctor-diagnosed/medication-indicated memory-related disease (no medication data available) across 9 waves
qc <- hklosa_w5_2 %>% filter((r7alzdeme==1 & !is.na(r7alzdeme))| (r7rxalzdem==1 & !is.na(r7rxalzdem)) |
                               (r8alzdeme==1 & !is.na(r8alzdeme))| (r8rxalzdem==1 & !is.na(r8rxalzdem)) |
                               (r9alzdeme==1 & !is.na(r9alzdeme)) | (r8rxalzdem==1 & !is.na(r8rxalzdem)))%>% select(pid, householdID, r7alzdeme:r9alzdeme, r7rxalzdem:r9rxalzdem, s7alzdeme:s9alzdeme, s7rxalzdem:s9rxalzdem) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #176 individuals, with 10 are spousal pairs and 156 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed/medication-indicated memory-related disease across 9 waves 
hklosa_w5_3 <- hklosa_w5_2 %>% filter(!householdID %in% qc$householdID)  #546 individuals and 273 spousal pair
table(as.data.frame(table(hklosa_w5_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Check the frequency of inw1
table(hklosa_w5_3$inw1, exclude=NULL) #3 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hklosa_w5_3$r5iwstat, exclude=NULL) #all 1
table(hklosa_w5_3$s5iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hklosa_w5_3 <- hklosa_w5_3 %>%   mutate(
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
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hklosa_w5_3$total_participations, exclude=NULL) #12 individuals participated in wave 5 only
qc <- as.data.frame(table(hklosa_w5_3$spousal_part_pattern, exclude=NULL)) #14 patterns

#Check the number of individual participated only in wave 5
qc <- hklosa_w5_3 %>% filter(total_participations ==1)
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #12 individuals, 5 spousal pairs and 2 individuals
######################################################
##Wave 6 ##8 individuals and 4 spousal pairs participating from wave 6
######################################################
#All spousal pairs participating in wave 6
hklosa_w6 <- hklosa %>% filter(inw6==1 & !is.na(s6pid) & s6pid != 0 & !is.na(s6iwstat) & s6iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(pid, s6pid), 
                      pmax(pid, s6pid), 
                      sep = "_")) #4884 individuals, with 2442 spousal pairs  
table(as.data.frame(table(hklosa_w6$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hklosa_w6$h6cpl) # all 1

#Exclude individuals included in hklosa_w1_3 or hklosa_w2_3 or hklosa_w5_3
hklosa_w6_2 <- hklosa_w6 %>% filter(!(householdID %in% hklosa_w1_3$householdID | householdID %in% hklosa_w2_3$householdID | householdID %in% hklosa_w5_3$householdID)) #306 individuals, with  153 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_w6_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 18 and above
table(hklosa_w6_2$r6agey, exclude=NULL)
table(hklosa_w6_2$s6agey, exclude=NULL) #All above 18

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hklosa_w6_2 <- hklosa_w6_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre:r9arthre))))) 
table(hklosa_w6_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: the number of spousal pairs with doctor-diagnosed/medication-indicated memory-related disease (no medication data available) across 9 waves
qc <- hklosa_w6_2 %>% filter((r7alzdeme==1 & !is.na(r7alzdeme))| (r7rxalzdem==1 & !is.na(r7rxalzdem)) |
                               (r8alzdeme==1 & !is.na(r8alzdeme))| (r8rxalzdem==1 & !is.na(r8rxalzdem)) |
                               (r9alzdeme==1 & !is.na(r9alzdeme)) | (r8rxalzdem==1 & !is.na(r8rxalzdem)))%>% select(pid, householdID, r7alzdeme:r9alzdeme, r7rxalzdem:r9rxalzdem, s7alzdeme:s9alzdeme, s7rxalzdem:s9rxalzdem) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #159 individuals, with 10 are spousal pairs and 139 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed/medication-indicated memory-related disease across 9 waves 
hklosa_w6_3 <- hklosa_w6_2 %>% filter(!householdID %in% qc$householdID)  #8 individuals and 4 spousal pairs
table(as.data.frame(table(hklosa_w6_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Check the frequency of inw1
table(hklosa_w6_3$inw1, exclude=NULL) #5 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hklosa_w6_3$r6iwstat, exclude=NULL) #all 1
table(hklosa_w6_3$s6iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hklosa_w6_3 <- hklosa_w6_3 %>%   mutate(
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
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hklosa_w6_3$total_participations, exclude=NULL) #0 individuals participated in wave 6 only
qc <- as.data.frame(table(hklosa_w6_3$spousal_part_pattern, exclude=NULL)) #7 patterns
######################################################
##Wave 7 ##No spousal pairs participating from wave 7
######################################################
#All spousal pairs participating in wave 7
hklosa_w7 <- hklosa %>% filter(inw7==1 & !is.na(s7pid) & s7pid != 0 & !is.na(s7iwstat) & s7iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(pid, s7pid), 
                      pmax(pid, s7pid), 
                      sep = "_")) #4544 individuals, with 2272 spousal pairs  
table(as.data.frame(table(hklosa_w7$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hklosa_w7$h7cpl) # all 1

#Exclude individuals included in hklosa_w1_3 or hklosa_w2_3 or hkolsa_w5_3 or hklosa_w6_3
hklosa_w7_2 <- hklosa_w7 %>% filter(!(householdID %in% hklosa_w1_3$householdID | householdID %in% hklosa_w2_3$householdID | householdID %in% hklosa_w5_3$householdID | householdID %in% hklosa_w6_3$householdID)) #10 individuals, with  5 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_w7_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 18 and above
table(hklosa_w7_2$r7agey, exclude=NULL)
table(hklosa_w7_2$s7agey, exclude=NULL) #All above 18

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hklosa_w7_2 <- hklosa_w7_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre:r9arthre))))) 
table(hklosa_w7_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: the number of spousal pairs with doctor-diagnosed/medication-indicated memory-related disease (no medication data available) across 9 waves
qc <- hklosa_w7_2 %>% filter((r7alzdeme==1 & !is.na(r7alzdeme))| (r7rxalzdem==1 & !is.na(r7rxalzdem)) |
                               (r8alzdeme==1 & !is.na(r8alzdeme))| (r8rxalzdem==1 & !is.na(r8rxalzdem)) |
                               (r9alzdeme==1 & !is.na(r9alzdeme)) | (r8rxalzdem==1 & !is.na(r8rxalzdem)))%>% select(pid, householdID, r7alzdeme:r9alzdeme, r7rxalzdem:r9rxalzdem, s7alzdeme:s9alzdeme, s7rxalzdem:s9rxalzdem) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #5 individuals

#Exclude spousal pairs in which either spouse with doctor-diagnosed/medication-indicated memory-related disease across 9 waves 
hklosa_w7_3 <- hklosa_w7_2 %>% filter(!householdID %in% qc$householdID)  #0 individuals
######################################################
##Wave 8 ##66 individuals and 33 spousal pairs participating from wave 8
######################################################
#All spousal pairs participating in wave 8
hklosa_w8 <- hklosa %>% filter(inw8==1 & !is.na(s8pid) & s8pid != 0 & !is.na(s8iwstat) & s8iwstat==1) %>% mutate(
  # Create a standardized pair ID (lowest ID first)
  householdID = paste(pmin(pid, s8pid), 
                      pmax(pid, s8pid), 
                      sep = "_")) #4232 individuals, with 2116 spousal pairs  
table(as.data.frame(table(hklosa_w8$householdID, exclude = NULL))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hklosa_w8$h8cpl) # all 1

#Exclude individuals included in hklosa_w1_3 or hklosa_w2_3 or hkolsa_w5_3 or hklosa_w6_3
hklosa_w8_2 <- hklosa_w8 %>% filter(!(householdID %in% hklosa_w1_3$householdID | householdID %in% hklosa_w2_3$householdID | householdID %in% hklosa_w5_3$householdID | householdID %in% hklosa_w6_3$householdID)) #74 individuals, with  37 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_w8_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 18 and above
table(hklosa_w8_2$r8agey, exclude=NULL)
table(hklosa_w8_2$s8agey, exclude=NULL) #All above 18

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hklosa_w8_2 <- hklosa_w8_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre:r9arthre))))) 
table(hklosa_w8_2$missing_count_rarthre, exclude=NULL) #no one with all rwarthre missing

#QC: the number of spousal pairs with doctor-diagnosed/medication-indicated memory-related disease (no medication data available) across 3 waves
qc <- hklosa_w8_2 %>% filter((r7alzdeme==1 & !is.na(r7alzdeme))| (r7rxalzdem==1 & !is.na(r7rxalzdem)) |
                               (r8alzdeme==1 & !is.na(r8alzdeme))| (r8rxalzdem==1 & !is.na(r8rxalzdem)) |
                               (r9alzdeme==1 & !is.na(r9alzdeme)) | (r8rxalzdem==1 & !is.na(r8rxalzdem)))%>% select(pid, householdID, r7alzdeme:r9alzdeme, r7rxalzdem:r9rxalzdem, s7alzdeme:s9alzdeme, s7rxalzdem:s9rxalzdem) 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #4 individuals 

#Exclude spousal pairs in which either spouse with doctor-diagnosed/medication-indicated memory-related disease across 9 waves 
hklosa_w8_3 <- hklosa_w8_2 %>% filter(!householdID %in% qc$householdID)  #66 individuals and 33 spousal pair
table(as.data.frame(table(hklosa_w8_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Check the frequency of inw1
table(hklosa_w8_3$inw1, exclude=NULL) #32 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hklosa_w8_3$r8iwstat, exclude=NULL) #all 1
table(hklosa_w8_3$s8iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Check participation pattern
hklosa_w8_3 <- hklosa_w8_3 %>%   mutate(
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
  total_participations = rowSums(across(c(r1_part, r2_part, r3_part, r4_part, r5_part, r6_part, r7_part, r8_part, r9_part))))
table(hklosa_w8_3$total_participations, exclude=NULL)
qc <- as.data.frame(table(hklosa_w8_3$spousal_part_pattern, exclude=NULL)) #4 patterns
######################################################
##All eligible spousal pairs, hklosa_all_sp3, 6422 individuals and 3211 spousal pairs
######################################################
#Add inclusion wave indicator and create unique householdID
hklosa_w1_3 <- hklosa_w1_3 %>% mutate(inclusion_wave="Wave 1")
hklosa_w2_3 <- hklosa_w2_3 %>% mutate(inclusion_wave="Wave 2")
hklosa_w5_3 <- hklosa_w5_3 %>% mutate(inclusion_wave="Wave 5")
hklosa_w6_3 <- hklosa_w6_3 %>% mutate(inclusion_wave="Wave 6")
hklosa_w8_3 <- hklosa_w8_3 %>% mutate(inclusion_wave="Wave 8")

##Dataset including all spousal pairs, combining hklosa_w1_3, hklosa_w2_3, hklosa_w5_3, hklosa_w6_3, hklosa_w8_3
hklosa_all_sp <- rbind(hklosa_w1_3, hklosa_w2_3, hklosa_w5_3, hklosa_w6_3, hklosa_w8_3) #7206 individuals and 3603 spousal pairs#Check frequency of inclusion_wave
table(hklosa_all_sp$inclusion_wave, exclude=NULL)
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_all_sp$householdID, exclude = NULL))$Freq, exclude=NULL) 
#Check if all were heterosexual spouse pairs
hetero <- hklosa_all_sp %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%
  summarise(hetero = n_distinct(ragender) == 2) %>% filter(hetero=="FALSE")
table(hetero$hetero, exclude=NULL) #1 spousal pairs were homosexual
qc <- hklosa_all_sp %>% filter(householdID %in% hetero$householdID) %>% select(pid, householdID, ragender, s1gender:s6gender, inclusion_wave)  

#Exclude the homosexual spousal pair
hklosa_all_sp2 <- hklosa_all_sp %>% filter(!householdID %in% hetero$householdID) #7204 individuals and 3602 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_all_sp2$householdID))$Freq, exclude=NULL) 
#Select individuals with total_participations==1
one_wave <- hklosa_all_sp2 %>% filter(total_participations==1) %>% select(pid, householdID, inclusion_wave, total_participations) #646 individuals and 255 spousal pairs and 136 individuals with spouses participated in later waves
#Check the number of spousal pairs
table(as.data.frame(table(one_wave$householdID))$Freq, exclude=NULL) 

#Exclude individuals with total_participations==1
hklosa_all_sp3 <- hklosa_all_sp2 %>% filter(!(householdID %in% one_wave$householdID)) #6422 individuals and 3211 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hklosa_all_sp3$householdID))$Freq, exclude=NULL) 

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

hklosa_all_sp3$missing_count_rarthre2 <- count_missing_by_participation(hklosa_all_sp3)
#Check the frequency of missing_count_rarthre2 by total_participations
table(hklosa_all_sp3$missing_count_rarthre2, hklosa_all_sp3$total_participations,exclude=NULL) 
#1 individual participated in 9 waves with 1 missing value
#No one with missing_count_rarthre2 exceeds the upper limit

#Check the frequencies of total_participation by inclusion_wave
table(hklosa_all_sp3$total_participations, hklosa_all_sp3$inclusion_wave, exclude=NULL)
######################################################
#3. The number of individuals died during follow-up (hklosa_all_sp3)
######################################################
#Load harmonized end of life dataset
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/KLoSA/Data')
hklosa_exit_w2 <- read_dta("w02_exit_e.dta") %>% mutate(deathy=w02Xa010y) %>% mutate(across(where(is.labelled), as.numeric)) %>% select(pid, hhid,deathy)
hklosa_exit_w3 <- read_dta("w03_exit_e.dta") %>% mutate(deathy=w03Xa010y) %>% mutate(across(where(is.labelled), as.numeric)) %>% select(pid, hhid,deathy)
hklosa_exit_w4 <- read_dta("w04_exit_e.dta") %>% mutate(deathy=w04Xa010y) %>% mutate(across(where(is.labelled), as.numeric)) %>% select(pid, hhid,deathy)
hklosa_exit_w5 <- read_dta("w05_exit_e.dta") %>% mutate(deathy=w05xA010Y) %>% mutate(across(where(is.labelled), as.numeric)) %>% select(pid, hhid,deathy)
hklosa_exit_w6 <- read_dta("w06_exit_e.dta") %>% mutate(deathy=w06x_A010Y) %>% mutate(across(where(is.labelled), as.numeric)) %>% select(pid, hhid,deathy)
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/KLoSA/Data/2018 KLoSA 7 wave EXIT')
hklosa_exit_w7 <- read_dta("w07_exit_e.dta") %>% mutate(deathy=w07x_a010y) %>% mutate(across(where(is.labelled), as.numeric)) %>% select(pid, hhid,deathy)
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/KLoSA/Data/KLoSA 8th wave_EXIT')
hklosa_exit_w8 <- read_dta("w08_exit_e.dta") %>% mutate(deathy=w08x_a010Y) %>% mutate(across(where(is.labelled), as.numeric)) %>% select(pid, hhid,deathy)
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/KLoSA/Data/KLoSA 9 wave Exit')
hklosa_exit_w9 <- read_dta("Exit09_e.dta") %>% mutate(deathy=w09X_A010Y) %>% mutate(across(where(is.labelled), as.numeric)) %>% select(pid, hhid,deathy)

#Combine all exit datasets
hklosa_all_exit <- rbind(hklosa_exit_w2, hklosa_exit_w3, hklosa_exit_w4, hklosa_exit_w5, hklosa_exit_w6, hklosa_exit_w7, hklosa_exit_w8, hklosa_exit_w9)

#Check frequency of radyear 
table(hklosa_all_sp3$radyear, exclude = NULL) #1324 individuals with death year recorded 
qc <- hklosa_all_sp3 %>% filter(!is.na(radyear)) %>% select(pid, householdID, radyear, inclusion_wave, r1iwy:r9iwy, r1agey:r9agey, r1arthre:r9arthre)

#Check if any individuals in hklosa_all_sp3 also in hklosa_all_exit as well
qc2 <- hklosa_all_sp3 %>% filter(pid %in% hklosa_all_exit$pid) %>% select(pid, householdID, radyear, inclusion_wave, r1iwy:r9iwy, r1agey:r9agey, r1arthre:r9arthre) #1337 individuals died during follow-up
qc3 <- qc %>% filter(!pid %in% qc2$pid)
#All individuals with death year recorded also had exit interview 
qc4 <- qc2 %>% filter(!pid %in% qc$pid)
#13 individuals with exit interview but no death year recorded/with raxyear missing 
qc5 <- qc2 %>% filter(pid %in% qc$pid)

#qc2 includes all individuals died during follow-up
#Add death information from exit interview
death <- qc2 %>% left_join(.,hklosa_all_exit, by="pid") #1337 individuals
table(death$radyear, exclude=NULL) #13 missing values

#Select those with interview age older than death age
death2 <- death %>% mutate(exclude=case_when(
  !is.na(radyear) & inclusion_wave=="Wave 1" & radyear < r1iwy ~ 1,
  !is.na(radyear) & inclusion_wave=="Wave 2" & radyear < r2iwy ~ 1,
  !is.na(radyear) & inclusion_wave=="Wave 5" & radyear < r5iwy ~ 1,
  !is.na(radyear) & inclusion_wave=="Wave 6" & radyear < r6iwy ~ 1,
  !is.na(radyear) & inclusion_wave=="Wave 8" & radyear < r8iwy ~ 1,
  TRUE ~ NA_real_)) %>% filter(exclude==1) #No individuals with death happened before interview

#Check frequency of rwiwstat, values 5 and 6 indicating death, and value 9 indicating 'don't know alive or died'
table(hklosa_all_sp3$r1iwstat, exclude=NULL) #No death indicators
table(hklosa_all_sp3$r2iwstat, exclude=NULL) #No death indicators
table(hklosa_all_sp3$r3iwstat, exclude=NULL) #141 individuals died in wave 3, and 217 individuals with unknown death status
table(hklosa_all_sp3$r4iwstat, exclude=NULL) #158 individuals died in wave 4, 141 individual died in wave 3, and 310 individuals with unknown death status
table(hklosa_all_sp3$r5iwstat, exclude=NULL) #183 individuals died in wave 5, 299 individual died in waves 3 and 4, and 395 individuals with unknown death status
table(hklosa_all_sp3$r6iwstat, exclude=NULL) #181 individuals died in wave 5, 482 individual died in waves 3 and 4, and 456 individuals with unknown death status
table(hklosa_all_sp3$r7iwstat, exclude=NULL) #234 individuals died in wave 5, 663 individual died in waves 3 and 4, and 513 individuals with unknown death status
table(hklosa_all_sp3$r8iwstat, exclude=NULL) #223 individuals died in wave 5, 897 individual died in waves 3 and 4, and 595 individuals with unknown death status
######################################################
#4. Descriptive data of selected baseline and outcome variables in hklosa_all_sp22
######################################################
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/KLoSA')
load("hklosa_all_sp22.rda")

#Convert haven:labelled variables into numeric variables 
hklosa_all_sp22 <- hklosa_all_sp22 %>%
  mutate(across(where(haven::is.labelled), as.numeric)) 

#Define categorical variables
hklosa_all_sp22 <- hklosa_all_sp22 %>%
  mutate(across(c(rbmicat, roccup, rrelig, ramomeducl, radadeducl), as.factor))

#Select baseline and outcome variables and convert variables with have_labelled to factor variables
descriptive <- hklosa_all_sp22 %>%
  select(rabyear, rage, ragender, rrural, raeducl, rrelig, rbmin, rbmicat, rpactvg,  rdrinkr, rsmokev, roccup, hincome2, rkcnt, rsoc, ramomeducl, radadeducl, rshlt, radlfive, riadlza, arthritis,arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, livere, livere_dm, catracte_dm)
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
    rabyear ~ "Birth year", 
    rage ~ "Age at interview", 
    rural ~ "Living area",
    raeducl ~ "Highest attained education level",
    rbmin ~ "Self-reported BMI_cont",
    rbmicat ~ "Self-reported BMI_cat",
    rpactvg ~ "Vigorous physcial activity",
    rdrinkr ~ "Number of drinks/day when drinks",
    rsmokev ~ "Ever smoking",
    roccup ~ "Occupation",
    hincome2 ~ "Household income",
    rrelig ~ "Religion",
    rkcnt ~ "Weekly contact with children",
    rsoc ~ "Weekly social activity",
    ramomeducl ~ "Maternal education",
    radadeducl ~ "Paternal education",
    rshlt ~ "Self-reported health",
    radlfive ~ "ADL summary, five",
    riadlza ~ "IADL summary, five",
    arthritis ~ "Doctor-diagnosed arthritis",
    arthritis_dm ~ "Doctor-diagnosed/medication-indicated arthritis",
    hibpe ~ "Doctor-diagnosed hypertension",
    hibpe_dm ~ "Doctor-diagnosed/medication-indicated hypertension",
    diabe ~ "Doctor-diagnosed diabetes",
    diabe_dm ~ "Doctor-diagnosed/medication-indicated diabetes",
    cancre ~ "Doctor-diagnosed cancer",
    cancre_dm ~ "Doctor-diagnosed/medication-indicated cancer",
    lunge ~ "Doctor-diagnosed lung disease",
    lunge_dm ~ "Doctor-diagnosed/medication-indicated lung disease",
    hearte ~ "Doctor-diagnosed heart disease",
    hearte_dm ~ "Doctor-diagnosed/medication-indicated heart disease",
    stroke ~ "Doctor-diagnosed stroke",
    stroke_dm ~ "Doctor-diagnosed/medication-indicated stroke",
    psyche ~ "Doctor-diagnosed psychiatric problems",
    psyche_dm ~ "Doctor-diagnosed/medication-indicated psychiatric problems",
    livere ~ "Doctor-diagnosed liver disease",
    livere_dm ~ "Doctor-diagnosed/medication-indicated liver disease",
    catracte_dm ~ "Treatment-indicated cataract")) %>%
  # Additional formatting
  add_p(test = list(
    # Chi-square for larger tables
    c(raeducl,rpactvg, rdrinkr, rsmokev, roccup, rkcnt, rsoc, ramomeducl, radadeducl, rshlt, radlfive, riadlza, arthritis, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, livere, livere_dm, catracte_dm) ~ "chisq.test",
    # Fisher's for variables with small cell counts
    rrelig ~ "fisher.test",
    rbmicat ~ "fisher.test",
    # Continuous variables
    all_continuous() ~ "wilcox.test"
  ), test.args = list(c(rrelig,rbmicat) ~ list(simulate.p.value =TRUE, B=2000))) %>%  # Add p-values for group comparisons
  add_overall() %>%  # Add overall column
  bold_labels() %>%
  italicize_levels()

#Save descriptive table
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/KLoSA')
write_xlsx(descriptive_base_var_by_gender[["table_body"]], path = "klosa_descriptive_base_out_var_by_gender.xlsx", col_names=T, format_headers=T)
######################################################
#DATA PREPARATION
#1. List of included variables  
#2. Define outcomes and potential confounders to be adjusted in hklosa_all_sp3
###################################################### 
#1. List of included variables  
###################################################### 
#Load data and select relevant variables
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/KLoSA/Data')
hklosa <- read_dta("GH_KLoSA_f.dta") %>% select(1,2, 5:13, 23:67, 70:88, 122:130, 200:267, 288:315, 370:387, 406:441, 892:945, 1108:1161, 1216:1617, 1850:1867, 1904:1939, 2012:2079, 2368:2403, 2458:2475, 2584:2637, 3072:3107, 4123:4186, 4251:4474, 4540:4548, 4684:4743, 4838:4854, 4926:4979, 5151:5222, 5637:5672, 5817:5834, 5871:5888)

# Check if labels exist as attributes
var_info <- sapply(hklosa, function(x) {
  label <- attr(x, "label")
  ifelse(is.null(label), "", label)
})

# Extract value labels from attributes
val_info <- sapply(hklosa, function(x) {
  labels <- attr(x, "labels")
  if(!is.null(labels)) {
    # Format as "1=Label1; 2=Label2; ..."
    paste(paste(labels, "=", names(labels)), collapse = "; ")
  } else {
    ""
  }
})

#Create a detailed table
hklosa_variable_table <- data.frame(
  Variable = names(hklosa),
  Label = sapply(var_info, function(x) ifelse(is.null(x), "", x)),
  Type = sapply(hklosa, function(x) paste(class(x), collapse = ", ")),
  N_Missing = colSums(is.na(hklosa)),
  Unique_Values = sapply(hklosa, function(x) length(unique(na.omit(x)))),
  stringsAsFactors = FALSE
)

#Add value labels as a separate column
hklosa_variable_table$Value_Labels <- sapply(names(hklosa), function(var) {
  labels <- val_info[[var]]
  if(!is.null(labels)) {
    paste(paste0(names(labels), " (", labels, ")"), collapse = "; ")
  } else {
    ""
  }
})

#Save as excel file
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/KLoSA/Basic information')
write.xlsx(hklosa_variable_table, file = "hklosa_selected_variable_table_251106.xlsx", colNames=T, format_headers=T)
###################################################### 
#2. Define outcomes and potential confounders to be adjusted in hklosa_all_sp3
######################################################  
##Define doctor-diagnosed arthritis (outcome, cases/controls)
######################################################  
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 1 
hklosa_all_sp3 <- hklosa_all_sp3 %>% mutate(arthritis=case_when(
  inclusion_wave!="Wave 1"~ NA_real_, 
  inclusion_wave=="Wave 1" & if_any(c(r1arthre:r9arthre), ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hklosa_all_sp3$arthritis, hklosa_all_sp3$inclusion_wave, exclude=NULL) #For those included from wave 1, 4381 individuals with no arthritis across 9 waves and  1435 individuals with arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hklosa_all_sp3 %>% filter(arthritis==0) %>% select(pid, householdID,spousal_part_pattern, arthritis, r1arthre:r9arthre)
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
qc <- hklosa_all_sp3 %>% filter(arthritis==1) %>% select(pid, householdID,spousal_part_pattern, arthritis, r1arthre:r9arthre)
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
hklosa_all_sp4 <- hklosa_all_sp3 %>% mutate(arthritis=case_when(
  inclusion_wave=="Wave 1" ~ arthritis,
  inclusion_wave!="Wave 2" ~ NA_real_,
  inclusion_wave=="Wave 2" & if_any(c(r2arthre:r9arthre), ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hklosa_all_sp4$arthritis, hklosa_all_sp4$inclusion_wave, exclude=NULL) #For those included from wave 2, 0 individuals with no arthritis and 2 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp4 %>% filter(arthritis==1 & inclusion_wave=="Wave 2") %>% select(pid, householdID,spousal_part_pattern, arthritis, r2arthre:r9arthre)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
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
hklosa_all_sp5 <- hklosa_all_sp4 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ arthritis,
  inclusion_wave!="Wave 5" ~ NA_real_, 
  inclusion_wave=="Wave 5" & if_any(r5arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hklosa_all_sp5$arthritis, hklosa_all_sp5$inclusion_wave, exclude=NULL) #For those included from wave 5, 509 individuals with no arthritis and 21 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hklosa_all_sp5 %>% filter(arthritis==0 & inclusion_wave=="Wave 5") %>% select(pid, householdID, spousal_part_pattern, arthritis, r5arthre:r9arthre)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp5 %>% filter(arthritis==1 & inclusion_wave=="Wave 5") %>% select(pid, householdID, spousal_part_pattern, arthritis, r5arthre:r9arthre)
table(qc$r5arthre, exclude=NULL)
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL)
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 6
hklosa_all_sp6 <- hklosa_all_sp5 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ arthritis,
  inclusion_wave!="Wave 6" ~ NA_real_, 
  inclusion_wave=="Wave 6" & if_any(r6arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hklosa_all_sp6$arthritis, hklosa_all_sp6$inclusion_wave, exclude=NULL) #For those included from wave 6, 7 individuals with no arthritis and 1 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hklosa_all_sp6 %>% filter(arthritis==0 & inclusion_wave=="Wave 6") %>% select(pid, householdID, spousal_part_pattern, arthritis, r6arthre:r9arthre)
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp6 %>% filter(arthritis==1 & inclusion_wave=="Wave 6") %>% select(pid, householdID, spousal_part_pattern, arthritis, r6arthre:r9arthre)
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL)
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 8
hklosa_all_sp7 <- hklosa_all_sp6 %>% mutate(arthritis=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ arthritis,
  inclusion_wave!="Wave 8" ~ NA_real_, 
  inclusion_wave=="Wave 8" & if_any(r8arthre:r9arthre, ~ .x==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hklosa_all_sp7$arthritis, hklosa_all_sp7$inclusion_wave, exclude=NULL) #For those included from wave 8, 62 individuals with no arthritis and 4 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hklosa_all_sp7 %>% filter(arthritis==0 & inclusion_wave=="Wave 8") %>% select(pid, householdID, spousal_part_pattern, arthritis, r8arthre:r9arthre)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp7 %>% filter(arthritis==1 & inclusion_wave=="Wave 8") %>% select(pid, householdID, spousal_part_pattern, arthritis, r8arthre:r9arthre)
table(qc$r8arthre, exclude=NULL)
table(qc$r9arthre, exclude=NULL) #All had 1 
######################################################
##Define doctor-diagnosed arthritis (exposure)
###################################################### 
hklosa_all_sp7 <- hklosa_all_sp7 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sarthritis = ifelse(person_num == 1, arthritis[2], arthritis[1])
  ) %>%
  ungroup()

#Check the frequency of sarthritis
table(hklosa_all_sp7$sarthritis, exclude=NULL)

#Compare the proportion of men with arthritis who had wives affected by arthritis to those without
male <- hklosa_all_sp7 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_arthritis = male$sarthritis
)
freq_table(freq) #59% versus 30%
chisq.test(male$arthritis, male$sarthritis) #Significant

#Compare the proportion of women with arthritis who had husbands affected by arthritis to those without
female <- hklosa_all_sp7 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_arthritis = female$sarthritis
)
freq_table(freq2) #21% versus 7%
chisq.test(female$arthritis, female$sarthritis) #Significant
######################################################  
##Define doctor-diagnosed/medication-indicated arthritis (outcome, cases/controls)
#No additional arthritis cases when including rwrxarthr
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 1 
hklosa_all_sp7 <- hklosa_all_sp7 %>% mutate(arthritis_dm=case_when(
  inclusion_wave!="Wave 1"~ NA, 
  inclusion_wave=="Wave 1" & (if_any(r1arthre:r9arthre, ~ .x==1) | if_any(r1rxarthr:r9rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hklosa_all_sp7$arthritis_dm, hklosa_all_sp7$inclusion_wave, exclude=NULL) #For those included from wave 1, 4381 individuals with no doctor-diagnosed/medication-indicated arthritis across 9 waves and 1435 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hklosa_all_sp7 %>% filter(arthritis_dm==0) %>% select(pid, householdID, arthritis, arthritis_dm, r1arthre:r9arthre, r1rxarthr:r9rxarthr)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) 
table(qc$r1rxarthr, exclude=NULL) 
table(qc$r2rxarthr, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) 
table(qc$r7rxarthr, exclude=NULL) 
table(qc$r8rxarthr, exclude=NULL)
table(qc$r9rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp7 %>% filter(arthritis_dm==1) %>% select(pid, householdID, arthritis, arthritis_dm, r1arthre:r9arthre, r1rxarthr:r9rxarthr)
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) 
table(qc$r1rxarthr, exclude=NULL) 
table(qc$r2rxarthr, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL)  
table(qc$r7rxarthr, exclude=NULL) 
table(qc$r8rxarthr, exclude=NULL)
table(qc$r9rxarthr, exclude=NULL) #All had 1
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 2
hklosa_all_sp8 <- hklosa_all_sp7 %>% mutate(arthritis_dm=case_when(
  inclusion_wave=="Wave 1"~ arthritis_dm, 
  inclusion_wave!="Wave 2"~ NA, 
  inclusion_wave=="Wave 2" & (if_any(r2arthre:r9arthre, ~ .x==1) | if_any(r2rxarthr:r9rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hklosa_all_sp8$arthritis_dm, hklosa_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 2, 0 individuals with no doctor-diagnosed/medication-indicated arthritis across 8 waves and 2 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hklosa_all_sp8 %>% filter(arthritis_dm==0) %>% select(pid, householdID, arthritis, arthritis_dm, r2arthre:r9arthre, r2rxarthr:r9rxarthr)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL)
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL)
table(qc$r2rxarthr, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL)
table(qc$r7rxarthr, exclude=NULL) 
table(qc$r8rxarthr, exclude=NULL)
table(qc$r9rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp8 %>% filter(arthritis_dm==1) %>% select(pid, householdID, arthritis, arthritis_dm, r2arthre:r9arthre, r2rxarthr:r9rxarthr)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL)
table(qc$r2rxarthr, exclude=NULL) 
table(qc$r3rxarthr, exclude=NULL)
table(qc$r4rxarthr, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL)  
table(qc$r7rxarthr, exclude=NULL) 
table(qc$r8rxarthr, exclude=NULL)
table(qc$r9rxarthr, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 5
hklosa_all_sp9 <- hklosa_all_sp8 %>% mutate(arthritis_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ arthritis_dm,
  inclusion_wave!="Wave 5"~ NA, 
  inclusion_wave=="Wave 5" & (if_any(r5arthre:r9arthre, ~ .x==1) | if_any(r5rxarthr:r9rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hklosa_all_sp9$arthritis_dm, hklosa_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 5, 509 individuals with no doctor-diagnosed/medication-indicated arthritis across 5 waves and 21 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hklosa_all_sp9 %>% filter(arthritis_dm==0) %>% select(pid, householdID, arthritis, arthritis_dm, r5arthre:r9arthre, r5rxarthr:r9rxarthr)
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL)
table(qc$r9arthre, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL) 
table(qc$r7rxarthr, exclude=NULL)
table(qc$r8rxarthr, exclude=NULL) 
table(qc$r9rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp9 %>% filter(arthritis_dm==1) %>% select(pid, householdID, arthritis, arthritis_dm, r1arthre:r9arthre, r1rxarthr:r9rxarthr)
table(qc$r5arthre, exclude=NULL) 
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) 
table(qc$r5rxarthr, exclude=NULL)
table(qc$r6rxarthr, exclude=NULL)  
table(qc$r7rxarthr, exclude=NULL)
table(qc$r8rxarthr, exclude=NULL)
table(qc$r9rxarthr, exclude=NULL) #All had 1
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 6
hklosa_all_sp10 <- hklosa_all_sp9 %>% mutate(arthritis_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ arthritis_dm,
  inclusion_wave!="Wave 6"~ NA, 
  inclusion_wave=="Wave 6" & (if_any(r6arthre:r9arthre, ~ .x==1) | if_any(r6rxarthr:r9rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hklosa_all_sp10$arthritis_dm, hklosa_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 6, 7 individuals with no doctor-diagnosed/medication-indicated arthritis across 4 waves and 1 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hklosa_all_sp10 %>% filter(arthritis_dm==0) %>% select(pid, householdID, arthritis, arthritis_dm, r1arthre:r9arthre, r1rxarthr:r9rxarthr)
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL)
table(qc$r9arthre, exclude=NULL) 
table(qc$r6rxarthr, exclude=NULL) 
table(qc$r7rxarthr, exclude=NULL)
table(qc$r8rxarthr, exclude=NULL) 
table(qc$r9rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp10 %>% filter(arthritis_dm==1) %>% select(pid, householdID, arthritis, arthritis_dm, r1arthre:r9arthre, r1rxarthr:r9rxarthr)
table(qc$r6arthre, exclude=NULL) 
table(qc$r7arthre, exclude=NULL) 
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) 
table(qc$r6rxarthr, exclude=NULL)  
table(qc$r7rxarthr, exclude=NULL)
table(qc$r8rxarthr, exclude=NULL)
table(qc$r9rxarthr, exclude=NULL) #All had 1
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 8
hklosa_all_sp11 <- hklosa_all_sp10 %>% mutate(arthritis_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ arthritis_dm,
  inclusion_wave!="Wave 8"~ NA, 
  inclusion_wave=="Wave 8" & (if_any(r8arthre:r9arthre, ~ .x==1) | if_any(r8rxarthr:r9rxarthr, ~ .x==1)) ~ 1, 
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hklosa_all_sp11$arthritis_dm, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 8, 62 individuals with no doctor-diagnosed/medication-indicated arthritis across 2 waves and 4 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across 6 waves
qc <- hklosa_all_sp11 %>% filter(arthritis_dm==0) %>% select(pid, householdID, arthritis, arthritis_dm, r1arthre:r9arthre, r1rxarthr:r9rxarthr)
table(qc$r8arthre, exclude=NULL)
table(qc$r9arthre, exclude=NULL) 
table(qc$r8rxarthr, exclude=NULL) 
table(qc$r9rxarthr, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hklosa_all_sp11 %>% filter(arthritis_dm==1) %>% select(pid, householdID, arthritis, arthritis_dm, r1arthre:r9arthre, r1rxarthr:r9rxarthr)
table(qc$r8arthre, exclude=NULL) 
table(qc$r9arthre, exclude=NULL) 
table(qc$r8rxarthr, exclude=NULL)
table(qc$r9rxarthr, exclude=NULL) #All had 1
######################################################
##Define doctor-diagnosed/medication-indicated arthritis (exposure)
###################################################### 
hklosa_all_sp11 <- hklosa_all_sp11 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sarthritis_dm = ifelse(person_num == 1, arthritis_dm[2], arthritis_dm[1])
  ) %>%
  ungroup()

#Check the frequency of sarthritis
table(hklosa_all_sp11$sarthritis_dm, exclude=NULL)
######################################################  
##Define doctor-diagnosed other chronic conditions
#Exclude digestive diseases as it was available in waves 5 to 9
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

hklosa_all_sp11$missing_count_hibpe <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="hibpe")
hklosa_all_sp11$missing_count_diabe <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="diabe")
hklosa_all_sp11$missing_count_cancre <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="cancre")
hklosa_all_sp11$missing_count_lunge <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="lunge")
hklosa_all_sp11$missing_count_hearte <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="hearte")
hklosa_all_sp11$missing_count_stroke <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="stroke")
hklosa_all_sp11$missing_count_psyche <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="psyche")
hklosa_all_sp11$missing_count_livere <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="livere")
hklosa_all_sp11$missing_count_catracte <- count_missing_cd_by_part(data=hklosa_all_sp11, disease_suffix="catrcte")

#All indiviudals with maximum 1 missing values in other chronic diseases across waves
#The number of individuals with missing values and participating in two waves only
table(hklosa_all_sp11$missing_count_hibpe, hklosa_all_sp11$total_participations, exclude=NULL) #No missing values
table(hklosa_all_sp11$missing_count_diabe, hklosa_all_sp11$total_participations, exclude=NULL) #No one exceeded missing upper limit
table(hklosa_all_sp11$missing_count_cancre, hklosa_all_sp11$total_participations, exclude=NULL) #No one exceeded missing upper limit
table(hklosa_all_sp11$missing_count_lunge, hklosa_all_sp11$total_participations, exclude=NULL) #No one exceeded missing upper limit
table(hklosa_all_sp11$missing_count_hearte, hklosa_all_sp11$total_participations, exclude=NULL) #No missing values
table(hklosa_all_sp11$missing_count_stroke, hklosa_all_sp11$total_participations, exclude=NULL) #No one exceeded missing upper limit
table(hklosa_all_sp11$missing_count_psyche, hklosa_all_sp11$total_participations, exclude=NULL) #No missing values
table(hklosa_all_sp11$missing_count_livere, hklosa_all_sp11$total_participations, exclude=NULL) #No one exceeded missing upper limit
table(hklosa_all_sp11$missing_count_catracte, hklosa_all_sp11$total_participations, exclude=NULL) #1 individuals participating in two waves with 1 missing value, 1 individual participating in 6 waves with 6 missing values

#Mark those exceeds the upper limit of missing values
hklosa_all_sp11 <- hklosa_all_sp11 %>% mutate(
  catracte_exclude=case_when(
    missing_count_catracte==1 & total_participations==2 ~ 1,
    missing_count_catracte==6 & total_participations==6 ~ 1,
    TRUE ~ 0))
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
hklosa_all_sp11 <- hklosa_all_sp11 %>% mutate(hibpe=case_when(
  inclusion_wave!="Wave 1"~ NA_real_,
  inclusion_wave=="Wave 1" & if_any(c(r1hibpe:r9hibpe), ~ .x==1) ~ 1, 
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1diabe:r9diabe), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1cancre:r9cancre), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1lunge:r9lunge), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1hearte:r9hearte), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1stroke:r9stroke), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1psyche:r9psyche), ~ .x==1) ~ 1, 
    TRUE ~ 0),
  livere=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & if_any(c(r1livere:r9livere), ~ .x==1) ~ 1, 
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp11$hibpe, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 2955 individuals with no high blood pressure/hypertension across 9 waves and 2861 individuals with high blood pressure/hypertension
table(hklosa_all_sp11$diabe, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 4401 individuals with no diabetes across 9 waves and 1415 individuals with diabetes
table(hklosa_all_sp11$cancre, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5212 individuals with no cancer across 9 waves and 604 individuals with cancer
table(hklosa_all_sp11$lunge, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5603 individuals with no respiratory diseases across 9 waves and 213 individuals with respiratory diseases
table(hklosa_all_sp11$hearte, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5128 individuals with no heart attack across 9 waves and 688 individuals with heart attack
table(hklosa_all_sp11$stroke, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5253 individuals with no stroke across 9 waves and 563 individuals with stroke
table(hklosa_all_sp11$psyche, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5490 individuals with no stroke across 9 waves and 326 individuals with stroke
table(hklosa_all_sp11$livere, hklosa_all_sp11$inclusion_wave, exclude=NULL) #For those included from wave 1, 5630 individuals with no stroke across 9 waves and 186 individuals with stroke
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2
hklosa_all_sp12 <- hklosa_all_sp11 %>% mutate(hibpe=case_when(
  inclusion_wave=="Wave 1" ~ hibpe,
  inclusion_wave!="Wave 2" ~ NA_real_, 
  inclusion_wave=="Wave 2" & if_any(c(r2hibpe:r9hibpe), ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave=="Wave 1" ~ diabe,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2diabe:r9diabe), ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave=="Wave 1" ~ cancre,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2cancre:r9cancre), ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave=="Wave 1" ~ lunge,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2lunge:r9lunge), ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave=="Wave 1" ~ hearte,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2hearte:r6hearte), ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave=="Wave 1" ~ stroke,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2stroke:r9stroke), ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave=="Wave 1" ~ psyche,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2psyche:r9psyche), ~ .x==1) ~ 1,
    TRUE ~ 0),
  livere=case_when(
    inclusion_wave=="Wave 1" ~ livere,
    inclusion_wave!="Wave 2" ~ NA_real_, 
    inclusion_wave=="Wave 2" & if_any(c(r2livere:r9livere), ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp12$hibpe, hklosa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 1 individuals with no high blood pressure/hypertension across 8 waves and 1 individuals with high blood pressure/hypertension
table(hklosa_all_sp12$diabe, hklosa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 1 individuals with no diabetes across 8 waves and 1 individuals with diabetes
table(hklosa_all_sp12$cancre, hklosa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no cancer across 8 waves and 0 individuals with cancer
table(hklosa_all_sp12$lunge, hklosa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no respiratory diseases across 0 waves and 213 individuals with respiratory diseases
table(hklosa_all_sp12$hearte, hklosa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no heart attack across 8 waves and 0 individuals with heart attack
table(hklosa_all_sp12$stroke, hklosa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no stroke across 8 waves and 0 individuals with stroke
table(hklosa_all_sp12$psyche, hklosa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no stroke across 8 waves and 0 individuals with stroke
table(hklosa_all_sp12$livere, hklosa_all_sp12$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no stroke across 8 waves and 0 individuals with stroke
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 5
hklosa_all_sp13 <- hklosa_all_sp12 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ hibpe,
  inclusion_wave!="Wave 5" ~ NA_real_, 
  inclusion_wave=="Wave 5" & if_any(r5hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ diabe,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ cancre,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ lunge,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hearte,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ stroke,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ psyche,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  livere=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ livere,
    inclusion_wave!="Wave 5" ~ NA_real_, 
    inclusion_wave=="Wave 5" & if_any(r5livere:r9livere, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp13$hibpe, hklosa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 413 individuals with no high blood pressure/hypertension across 5 waves and 117 individuals with high blood pressure/hypertension
table(hklosa_all_sp13$diabe, hklosa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 491 individuals with no diabetes across 5 waves and 39 individuals with diabetes
table(hklosa_all_sp13$cancre, hklosa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 507 individuals with no cancer across 5 waves and 23 individuals with cancer
table(hklosa_all_sp13$lunge, hklosa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 525 individuals with no respiratory diseases across 5 waves and 5 individuals with respiratory diseases
table(hklosa_all_sp13$hearte, hklosa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 518 individuals with no heart attack across 5 waves and 12 individuals with heart attack
table(hklosa_all_sp13$stroke, hklosa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 526 individuals with no stroke across 5 waves and 4 individuals with stroke
table(hklosa_all_sp13$psyche, hklosa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 523 individuals with no stroke across 5 waves and 7 individuals with stroke
table(hklosa_all_sp13$livere, hklosa_all_sp13$inclusion_wave, exclude=NULL) #For those included from wave 5, 523 individuals with no stroke across 5 waves and 7 individuals with stroke
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 6
hklosa_all_sp14 <- hklosa_all_sp13 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ hibpe,
  inclusion_wave!="Wave 6" ~ NA_real_, 
  inclusion_wave=="Wave 6" & if_any(r6hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ diabe,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ cancre,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ lunge,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ hearte,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ stroke,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ psyche,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  livere=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ livere,
    inclusion_wave!="Wave 6" ~ NA_real_, 
    inclusion_wave=="Wave 6" & if_any(r6livere:r9livere, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp14$hibpe, hklosa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 6, 2 individuals with no high blood pressure/hypertension across 4 waves and 6 individuals with high blood pressure/hypertension
table(hklosa_all_sp14$diabe, hklosa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 6, 6 individuals with no diabetes across 4 waves and 2 individuals with diabetes
table(hklosa_all_sp14$cancre, hklosa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no cancer across 4 waves and 0 individuals with cancer
table(hklosa_all_sp14$lunge, hklosa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no respiratory diseases across 0 waves and 5 individuals with respiratory diseases
table(hklosa_all_sp14$hearte, hklosa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 6, 7 individuals with no heart attack across 4 waves and 1 individuals with heart attack
table(hklosa_all_sp14$stroke, hklosa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no stroke across 4 waves and 0 individuals with stroke
table(hklosa_all_sp14$psyche, hklosa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no stroke across 4 waves and 0 individuals with stroke
table(hklosa_all_sp14$livere, hklosa_all_sp14$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no stroke across 4 waves and 0 individuals with stroke
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 8
hklosa_all_sp15 <- hklosa_all_sp14 %>% mutate(hibpe=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ hibpe,
  inclusion_wave!="Wave 8" ~ NA_real_, 
  inclusion_wave=="Wave 8" & if_any(r8hibpe:r9hibpe, ~ .x==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ diabe,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8diabe:r9diabe, ~ .x==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ cancre,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8cancre:r9cancre, ~ .x==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ lunge,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8lunge:r9lunge, ~ .x==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ hearte,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8hearte:r9hearte, ~ .x==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ stroke,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8stroke:r9stroke, ~ .x==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ psyche,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8psyche:r9psyche, ~ .x==1) ~ 1,
    TRUE ~ 0),
  livere=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ livere,
    inclusion_wave!="Wave 8" ~ NA_real_, 
    inclusion_wave=="Wave 8" & if_any(r8livere:r9livere, ~ .x==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp15$hibpe, hklosa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 50 individuals with no high blood pressure/hypertension across 2 waves and 16 individuals with high blood pressure/hypertension
table(hklosa_all_sp15$diabe, hklosa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 52 individuals with no diabetes across 2 waves and 14 individuals with diabetes
table(hklosa_all_sp15$cancre, hklosa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 62 individuals with no cancer across 2 waves and 4 individuals with cancer
table(hklosa_all_sp15$lunge, hklosa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 66 individuals with no respiratory diseases across 2 waves and 0 individuals with respiratory diseases
table(hklosa_all_sp15$hearte, hklosa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 65 individuals with no heart attack across 2 waves and 1 individuals with heart attack
table(hklosa_all_sp15$stroke, hklosa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 63 individuals with no stroke across 2 waves and 3 individuals with stroke
table(hklosa_all_sp15$psyche, hklosa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 64 individuals with no stroke across 2 waves and 2 individuals with stroke
table(hklosa_all_sp15$livere, hklosa_all_sp15$inclusion_wave, exclude=NULL) #For those included from wave 8, 64 individuals with no stroke across 2 waves and 2 individuals with stroke
######################################################
##Define doctor-diagnosed other chronic conditions among spousal pairs
###################################################### 
hklosa_all_sp16 <- hklosa_all_sp15 %>%
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
    slivere = ifelse(person_num == 1, livere[2], livere[1])) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(hklosa_all_sp16$shibpe, hklosa_all_sp16$inclusion_wave, exclude=NULL)
table(hklosa_all_sp16$sdiabe, hklosa_all_sp16$inclusion_wave, exclude=NULL)
table(hklosa_all_sp16$scancre, hklosa_all_sp16$inclusion_wave, exclude=NULL)
table(hklosa_all_sp16$slunge, hklosa_all_sp16$inclusion_wave, exclude=NULL)
table(hklosa_all_sp16$shearte, hklosa_all_sp16$inclusion_wave, exclude=NULL)
table(hklosa_all_sp16$sstroke, hklosa_all_sp16$inclusion_wave, exclude=NULL)
table(hklosa_all_sp16$spsyche, hklosa_all_sp16$inclusion_wave, exclude=NULL)
table(hklosa_all_sp16$slivere, hklosa_all_sp16$inclusion_wave, exclude=NULL)

##Compare the proportion of men with arthritis who had wives affected by other chronic conditions to those without
#High blood pressure/hypertension
male <- hklosa_all_sp16 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hibpe = male$shibpe
)
freq_table(freq) #58% versus 45%
chisq.test(male$arthritis, male$shibpe) #Significant

#High blood sugar/diabetes
male <- hklosa_all_sp16 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_diabe = male$sdiabe
)
freq_table(freq) #28% versus 21%
chisq.test(male$arthritis, male$sdiabe) #Significant

#Cancer
male <- hklosa_all_sp16 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_cancre = male$scancre
)
freq_table(freq) #11% versus 8%
chisq.test(male$arthritis, male$scancre) #Significant

#Lung diseases
male <- hklosa_all_sp16 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_lunge = male$slunge
)
freq_table(freq) #3% versus 2%
chisq.test(male$arthritis, male$slunge) #Non-significant

#Heart disease
male <- hklosa_all_sp16 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hearte = male$shearte
)
freq_table(freq) #13% versus 10%
chisq.test(male$arthritis, male$shearte) #Significant

#Stroke
male <- hklosa_all_sp16 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_stroke= male$sstroke
)
freq_table(freq) #7% versus %6
chisq.test(male$arthritis, male$sstroke) #Non-significant

#Mental related conditions
male <- hklosa_all_sp16 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_stroke= male$spsyche
)
freq_table(freq) #11% versus 5%
chisq.test(male$arthritis, male$spsyche) #Significant

#Liver disease
male <- hklosa_all_sp16 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_stroke= male$slivere
)
freq_table(freq) #4% versus 2%
chisq.test(male$arthritis, male$slivere) #Non-significant

##Compare the proportion of women with arthritis who had husbands affected by other chronic conditions to those without
#High blood pressure/hypertension
female <- hklosa_all_sp16 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hibpe = female$shibpe
)
freq_table(freq2) #55% versus 42%
chisq.test(female$arthritis, female$shibpe) #Significant

#High blood sugar/diabetes
female <- hklosa_all_sp16 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_diabe = female$sdiabe
)
freq_table(freq2) #29% versus 22%
chisq.test(female$arthritis, female$sdiabe) #Significant

#Cancer
female <- hklosa_all_sp16 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_cancre = female$scancre
)
freq_table(freq2) #15% versus 9%
chisq.test(female$arthritis, female$scancre) #Significant

#Respiratory diseases
female <- hklosa_all_sp16 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_lunge = female$slunge
)
freq_table(freq2) #9% versus 3%
chisq.test(female$arthritis, female$slunge) #Significant

#Heart disease
female <- hklosa_all_sp16 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hrtatte = female$shearte
)
freq_table(freq2) #14% versus 10%
chisq.test(female$arthritis, female$shearte) #Significant

#Stroke
female <- hklosa_all_sp16 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_stroke= female$sstroke
)
freq_table(freq2) #15% versus 10%
chisq.test(female$arthritis, female$sstroke) #Significant

#Mental related conditions
female <- hklosa_all_sp16 %>% filter(ragender==2)
freq <- ftable(
  Arthritis = female$arthritis, 
  Spousal_stroke= female$spsyche
)
freq_table(freq) #5% versus 4%
chisq.test(female$arthritis, female$spsyche) #Non-significant

#Liver disease
female <- hklosa_all_sp16 %>% filter(ragender==2)
freq <- ftable(
  Arthritis = female$arthritis, 
  Spousal_stroke= female$slivere
)
freq_table(freq) #5% versus 3%
chisq.test(female$arthritis, female$slivere) #Significant

######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions 
#Requiring individuals not exceeding the upper limit missing values of doctor-diagnosed variables only as operator "OR" not "AND" was used
#The number of NA became smaller when including medication variables to define other chronic conditions, which is unexpected and should use the other chronic condition variables defined by doctor-diagnosed only to refine rows with NA
##No additional cases added when including medication/treatment variables
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
hklosa_all_sp16 <- hklosa_all_sp16 %>% mutate(hibpe_dm=case_when(
  inclusion_wave!="Wave 1"~ NA_real_,
  inclusion_wave=="Wave 1" & (if_any(r1hibpe:r9hibpe, ~ .x==1) | if_any(r1rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1diabe:r9diabe, ~ .x==1) | if_any(r1rxdiab:r9rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1cancre:r9cancre, ~ .x==1) | if_any(r1rxcancr:r9rxcancr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1lunge:r9lunge, ~ .x==1) | if_any(r1rxlung:r9rxlung, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hearte_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1hearte:r9hearte, ~ .x==1) | if_any(r1rxheart:r9rxheart, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1stroke:r9stroke, ~ .x==1) | if_any(r1rxstrok:r9rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1psyche:r9psyche, ~ .x==1) | if_any(r1rxpsych:r9rxpsych, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  livere_dm=case_when(
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1livere:r9livere, ~ .x==1) | if_any(r1rxliver:r9rxliver, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  catracte_dm=case_when(
    catracte_exclude==1 ~ NA_real_,
    inclusion_wave!="Wave 1"~ NA_real_,
    inclusion_wave=="Wave 1" & (if_any(r1catrcte:r9catrcte, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp16$hibpe_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 2955 individuals with no high blood pressure/hypertension across 9 waves and 2861 individuals with high blood pressure/hypertension
table(hklosa_all_sp16$diabe_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 4401 individuals with no diabetes across 9 waves and 1415 individuals with diabetes
table(hklosa_all_sp16$cancre_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 5212 individuals with no cancer across 9 waves and 604 individuals with cancer
table(hklosa_all_sp16$lunge_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 5603 individuals with no lung diseases across 9 waves and 213 individuals with lung diseases
table(hklosa_all_sp16$hearte_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 5128 individuals with no heart disease across 9 waves and 688 individuals with heart disease
table(hklosa_all_sp16$stroke_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 5253 individuals with no stroke across 9 waves and 563 individuals with stroke
table(hklosa_all_sp16$psyche_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 5490 individuals with no mental-related conditions across 9 waves and 326 individuals with mental-related conditions
table(hklosa_all_sp16$livere_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 5630 individuals with no liver disease across 9 waves and 186 individuals with liver disease
table(hklosa_all_sp16$catracte_dm, hklosa_all_sp16$inclusion_wave, exclude=NULL) #For those included from wave 1, 4729 individuals with no cataract across 9 waves and 1085 individuals with cataract, 2 individuals with NA
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2
hklosa_all_sp17 <- hklosa_all_sp16 %>% mutate(hibpe_dm=case_when(
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
    inclusion_wave=="Wave 2" & (if_any(r2cancre:r9cancre, ~ .x==1) | if_any(r2rxcancr:r9rxcancr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave=="Wave 1" ~ lunge_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2lunge:r9lunge, ~ .x==1) | if_any(r2rxlung:r9rxlung, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hearte_dm=case_when(
    inclusion_wave=="Wave 1" ~ hearte_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2hearte:r9hearte, ~ .x==1) | if_any(r2rxheart:r9rxheart, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    inclusion_wave=="Wave 1" ~ stroke_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2stroke:r9stroke, ~ .x==1) | if_any(r2rxstrok:r9rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave=="Wave 1" ~ psyche_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2psyche:r9psyche, ~ .x==1) | if_any(r2rxpsych:r9rxpsych, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  livere_dm=case_when(
    inclusion_wave=="Wave 1" ~ livere_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2livere:r9livere, ~ .x==1) | if_any(r2rxliver:r9rxliver, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  catracte_dm=case_when(
    inclusion_wave=="Wave 1" ~ catracte_dm,
    inclusion_wave!="Wave 2"~ NA_real_,
    inclusion_wave=="Wave 2" & (if_any(r2catrcte:r9catrcte, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp17$hibpe_dm, hklosa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 2, 1 individuals with no high blood pressure/hypertension across 8 waves and 1 individuals with high blood pressure/hypertension
table(hklosa_all_sp17$diabe_dm, hklosa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 2, 1 individuals with no diabetes across 8 waves and 1 individuals with diabetes
table(hklosa_all_sp17$cancre_dm, hklosa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no cancer across 8 waves and 0 individuals with cancer
table(hklosa_all_sp17$lunge_dm, hklosa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no lung diseases across 8 waves and 0 individuals with lung diseases
table(hklosa_all_sp17$hearte_dm, hklosa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no heart disease across 8 waves and 0 individuals with heart disease
table(hklosa_all_sp17$stroke_dm, hklosa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no stroke across 8 waves and 0 individuals with stroke
table(hklosa_all_sp17$psyche_dm, hklosa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no mental-related conditions across 8 waves and 0 individuals with mental-related conditions
table(hklosa_all_sp17$catracte_dm, hklosa_all_sp17$inclusion_wave, exclude=NULL) #For those included from wave 2, 2 individuals with no cataract across 8 waves and 0 individuals with cataract
######################################################
#Inclusion in Wave 5
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 5
hklosa_all_sp18 <- hklosa_all_sp17 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2") ~ hibpe_dm,
  inclusion_wave!="Wave 5"~ NA_real_,
  inclusion_wave=="Wave 5" & (if_any(r5hibpe:r9hibpe, ~ .x==1) | if_any(r5rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ diabe_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5diabe:r9diabe, ~ .x==1) | if_any(r5rxdiab:r9rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ cancre_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5cancre:r9cancre, ~ .x==1) | if_any(r5rxcancr:r9rxcancr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ lunge_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5lunge:r9lunge, ~ .x==1) | if_any(r5rxlung:r9rxlung, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hearte_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ hearte_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5hearte:r9hearte, ~ .x==1) | if_any(r5rxheart:r9rxheart, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ stroke_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5stroke:r9stroke, ~ .x==1) | if_any(r5rxstrok:r9rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ psyche_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5psyche:r9psyche, ~ .x==1) | if_any(r5rxpsych:r9rxpsych, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  livere_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ livere_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5livere:r9livere, ~ .x==1) | if_any(r5rxliver:r9rxliver, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  catracte_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2") ~ catracte_dm,
    inclusion_wave!="Wave 5"~ NA_real_,
    inclusion_wave=="Wave 5" & (if_any(r5catrcte:r9catrcte, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp18$hibpe_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 413 individuals with no high blood pressure/hypertension across 5 waves and 117 individuals with high blood pressure/hypertension
table(hklosa_all_sp18$diabe_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 491 individuals with no diabetes across 5 waves and 39 individuals with diabetes
table(hklosa_all_sp18$cancre_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 507 individuals with no cancer across 5 waves and 23 individuals with cancer
table(hklosa_all_sp18$lunge_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 525 individuals with no respiratory diseases across 5 waves and 5 individuals with respiratory diseases
table(hklosa_all_sp18$hearte_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 518 individuals with no heart disease across 5 waves and 12 individuals with heart disease
table(hklosa_all_sp18$stroke_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 526 individuals with no stroke across 5 waves and 4 individuals with stroke
table(hklosa_all_sp18$psyche_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 523 individuals with no mental-related conditions across 5 waves and 7 individuals with mental-related conditions
table(hklosa_all_sp18$livere_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 523 individuals with no liver disease across 5 waves and 7 individuals with liver disease
table(hklosa_all_sp18$catracte_dm, hklosa_all_sp18$inclusion_wave, exclude=NULL) #For those included from wave 5, 517 individuals with no cataract across 5 waves and 13 individuals with cataract
######################################################
#Inclusion in Wave 6
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 6
hklosa_all_sp19 <- hklosa_all_sp18 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ hibpe_dm,
  inclusion_wave!="Wave 6"~ NA_real_,
  inclusion_wave=="Wave 6" & (if_any(r6hibpe:r9hibpe, ~ .x==1) | if_any(r6rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ diabe_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6diabe:r9diabe, ~ .x==1) | if_any(r6rxdiab:r9rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ cancre_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6cancre:r9cancre, ~ .x==1) | if_any(r6rxcancr:r9rxcancr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ lunge_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6lunge:r9lunge, ~ .x==1) | if_any(r6rxlung:r9rxlung, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hearte_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ hearte_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6hearte:r9hearte, ~ .x==1) | if_any(r6rxheart:r9rxheart, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ stroke_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6stroke:r9stroke, ~ .x==1) | if_any(r6rxstrok:r9rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ psyche_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6psyche:r9psyche, ~ .x==1) | if_any(r6rxpsych:r9rxpsych, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  livere_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ livere_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6livere:r9livere, ~ .x==1) | if_any(r6rxliver:r9rxliver, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  catracte_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5") ~ catracte_dm,
    inclusion_wave!="Wave 6"~ NA_real_,
    inclusion_wave=="Wave 6" & (if_any(r6catrcte:r9catrcte, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp19$hibpe_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 2 individuals with no high blood pressure/hypertension across 4 waves and 6 individuals with high blood pressure/hypertension
table(hklosa_all_sp19$diabe_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 6 individuals with no diabetes across 4 waves and 2 individuals with diabetes
table(hklosa_all_sp19$cancre_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no cancer across 4 waves and 0 individuals with cancer
table(hklosa_all_sp19$lunge_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no lung diseases across 0 waves and 5 individuals with lung diseases
table(hklosa_all_sp19$hearte_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 7 individuals with no heart disease across 4 waves and 1 individuals with heart disease
table(hklosa_all_sp19$stroke_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no stroke across 4 waves and 0 individuals with stroke
table(hklosa_all_sp19$psyche_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no mental related conditions across 4 waves and 0 individuals with mental related conditions
table(hklosa_all_sp19$livere_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 8 individuals with no liver disease across 4 waves and 0 individuals with liver disease
table(hklosa_all_sp19$catracte_dm, hklosa_all_sp19$inclusion_wave, exclude=NULL) #For those included from wave 6, 7 individuals with no cataract across 4 waves and 1 individuals with cataract
######################################################
#Inclusion in Wave 8
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 8
hklosa_all_sp20 <- hklosa_all_sp19 %>% mutate(hibpe_dm=case_when(
  inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ hibpe_dm,
  inclusion_wave!="Wave 8"~ NA_real_,
  inclusion_wave=="Wave 8" & (if_any(r8hibpe:r9hibpe, ~ .x==1) | if_any(r8rxhibp:r9rxhibp, ~ .x==1)) ~ 1, 
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ diabe_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8diabe:r9diabe, ~ .x==1) | if_any(r8rxdiab:r9rxdiab, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ cancre_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8cancre:r9cancre, ~ .x==1) | if_any(r8rxcancr:r9rxcancr, ~ .x==1)) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ lunge_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8lunge:r9lunge, ~ .x==1) | if_any(r8rxlung:r9rxlung, ~ .x==1)) ~ 1,   
    TRUE ~ 0),
  hearte_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ hearte_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8hearte:r9hearte, ~ .x==1) | if_any(r8rxheart:r9rxheart, ~ .x==1)) ~ 1,    
    TRUE ~ 0),  
  stroke_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ stroke_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8stroke:r9stroke, ~ .x==1) | if_any(r8rxstrok:r9rxstrok, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ psyche_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8psyche:r9psyche, ~ .x==1) | if_any(r8rxpsych:r9rxpsych, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  livere_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ livere_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8livere:r9livere, ~ .x==1) | if_any(r8rxliver:r9rxliver, ~ .x==1)) ~ 1,    
    TRUE ~ 0),
  catracte_dm=case_when(
    inclusion_wave %in% c("Wave 1", "Wave 2", "Wave 5", "Wave 6") ~ catracte_dm,
    inclusion_wave!="Wave 8"~ NA_real_,
    inclusion_wave=="Wave 8" & (if_any(r8catrcte:r9catrcte, ~ .x==1)) ~ 1,    
    TRUE ~ 0))
#Check the frequency of other chronic diseases
table(hklosa_all_sp20$hibpe_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 50 individuals with no high blood pressure/hypertension across 2 waves and 16 individuals with high blood pressure/hypertension
table(hklosa_all_sp20$diabe_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 52 individuals with no diabetes across 2 waves and 14 individuals with diabetes
table(hklosa_all_sp20$cancre_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 62 individuals with no cancer across 2 waves and 4 individuals with cancer
table(hklosa_all_sp20$lunge_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 66 individuals with no lung diseases across 2 waves and 0 individuals with lung diseases
table(hklosa_all_sp20$hearte_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 65 individuals with no heart disease across 2 waves and 1 individuals with heart disease
table(hklosa_all_sp20$stroke_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 63 individuals with no stroke across 2 waves and 3 individuals with stroke
table(hklosa_all_sp20$psyche_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 64 individuals with no mental related conditions across 2 waves and 2 individuals with mental related conditions
table(hklosa_all_sp20$livere_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 64 individuals with no liver disease across 2 waves and 2 individuals with liver disease
table(hklosa_all_sp20$catracte_dm, hklosa_all_sp20$inclusion_wave, exclude=NULL) #For those included from wave 8, 62 individuals with no cataract across 2 waves and 4 individuals with cataract
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions for the paired spouses
###################################################### 
hklosa_all_sp21 <- hklosa_all_sp20 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    shibpe_dm = ifelse(person_num == 1, hibpe_dm[2], hibpe_dm[1]),
    sdiabe_dm = ifelse(person_num == 1, diabe_dm[2], diabe_dm[1]),  
    scancre_dm = ifelse(person_num == 1, cancre_dm[2], cancre_dm[1]),
    slunge_dm = ifelse(person_num == 1, lunge_dm[2], lunge_dm[1]),
    shearte_dm = ifelse(person_num == 1, hearte_dm[2], hearte_dm[1]),
    sstroke_dm = ifelse(person_num == 1, stroke_dm[2], stroke_dm[1]),
    spsyche_dm = ifelse(person_num == 1, psyche_dm[2], psyche_dm[1]),
    slivere_dm = ifelse(person_num == 1, livere_dm[2], livere_dm[1]),
    scatracte_dm = ifelse(person_num == 1, catracte_dm[2], catracte_dm[1])) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(hklosa_all_sp21$shibpe_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)
table(hklosa_all_sp21$sdiabe_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)
table(hklosa_all_sp21$scancre_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)
table(hklosa_all_sp21$slunge_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)
table(hklosa_all_sp21$shearte_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)
table(hklosa_all_sp21$sstroke_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)
table(hklosa_all_sp21$spsyche_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)
table(hklosa_all_sp21$slivere_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)
table(hklosa_all_sp21$scatracte_dm, hklosa_all_sp21$inclusion_wave, exclude=NULL)

######################################################
##Define covariates at inclusion for respondents
#hwkcnt available only for wave 1 and for the following waves, it has changed to respondent level rwkcnt. Use data from wave 2 for wave 1
######################################################
#Define respondent covariates at inclusion for the three inclusion_wave groups
hklosa_all_sp22 <- hklosa_all_sp21 %>% mutate(
  rage=case_when(
    inclusion_wave=="Wave 1" ~ r1agey,
    inclusion_wave=="Wave 2" ~ r2agey,
    inclusion_wave=="Wave 5" ~ r5agey,
    inclusion_wave=="Wave 6" ~ r6agey,
    TRUE~ r8agey), 
  rrelig=case_when(
    inclusion_wave=="Wave 1" ~ r1relig_k,
    inclusion_wave=="Wave 2" ~ r2relig_k,
    inclusion_wave=="Wave 5" ~ r5relig_k,
    inclusion_wave=="Wave 6" ~ r6relig_k,
    TRUE~ r8relig_k),
  rrural=case_when(
    inclusion_wave=="Wave 1" ~ r1rural,
    inclusion_wave=="Wave 2" ~ r2rural,
    inclusion_wave=="Wave 5" ~ r5rural,
    inclusion_wave=="Wave 6" ~ r6rural,
    TRUE ~ r8rural),
  rshlt=case_when(
    inclusion_wave=="Wave 1" ~ r1shlt,
    inclusion_wave=="Wave 2" ~ r2shlt,
    inclusion_wave=="Wave 5" ~ r5shlt,
    inclusion_wave=="Wave 6" ~ r6shlt,
    TRUE~ r8shlt),
  radlfive=case_when(
    inclusion_wave=="Wave 1" ~ r1adlfiveb,
    inclusion_wave=="Wave 2" ~ r2adlfiveb,
    inclusion_wave=="Wave 5" ~ r5adlfiveb,
    inclusion_wave=="Wave 6" ~ r6adlfiveb,
    TRUE~ r8adlfiveb),
  riadlza=case_when(
    inclusion_wave=="Wave 1" ~ r1iadlzb,
    inclusion_wave=="Wave 2" ~ r2iadlzb,
    inclusion_wave=="Wave 5" ~ r5iadlzb,
    inclusion_wave=="Wave 6" ~ r6iadlzb,
    TRUE~ r8iadlzb),
  rpactvg=case_when(
    inclusion_wave=="Wave 1" ~ r1vigact, 
    inclusion_wave=="Wave 2" ~ r2vigact, 
    inclusion_wave=="Wave 5" ~ r5vigact, 
    inclusion_wave=="Wave 6" ~ r6vigact, 
    TRUE ~ r8vigact),
  rdrinkr=case_when(
    inclusion_wave=="Wave 1" ~ r1drinkn_k,
    inclusion_wave=="Wave 2" ~ r2drinkn_k,
    inclusion_wave=="Wave 5" ~ r5drinkn_k,
    inclusion_wave=="Wave 6" ~ r6drinkn_k,
    TRUE ~ r8drinkn_k),
  rsmokev=case_when(
    inclusion_wave=="Wave 1" ~ r1smokev,
    inclusion_wave=="Wave 2" ~ r2smokev,
    inclusion_wave=="Wave 5" ~ r5smokev,
    inclusion_wave=="Wave 6" ~ r6smokev,
    TRUE ~ r8smokev),
  ritearn=case_when(
    inclusion_wave=="Wave 1" ~ r1itearn,
    inclusion_wave=="Wave 2" ~ r2itearn,
    inclusion_wave=="Wave 5" ~ r5itearn,
    inclusion_wave=="Wave 6" ~ r6itearn,    
    TRUE ~ r8itearn),
  ritsemp=case_when(
    inclusion_wave=="Wave 1" ~ r1itsemp,
    inclusion_wave=="Wave 2" ~ r2itsemp,
    inclusion_wave=="Wave 5" ~ r5itsemp,
    inclusion_wave=="Wave 6" ~ r6itsemp,
    TRUE ~ r8itsemp),
  ritcap=case_when(
    inclusion_wave=="Wave 1" ~ r1icap,
    inclusion_wave=="Wave 2" ~ r2icap,
    inclusion_wave=="Wave 5" ~ r5icap,
    inclusion_wave=="Wave 6" ~ r6icap,
    TRUE ~ r8icap),
  ripripen=case_when(
    inclusion_wave=="Wave 1" ~ r1ipena,
    inclusion_wave=="Wave 2" ~ r2ipena,
    inclusion_wave=="Wave 5" ~ r5ipena,
    inclusion_wave=="Wave 6" ~ r6ipena,
    TRUE ~ r8ipena),
  ripubpen=case_when(
    inclusion_wave=="Wave 1" ~ r1ipubpen,
    inclusion_wave=="Wave 2" ~ r2ipubpen,
    inclusion_wave=="Wave 5" ~ r5ipubpen,
    inclusion_wave=="Wave 6" ~ r6ipubpen,
    TRUE ~ r8ipubpen),
  rigxfr=case_when(
    inclusion_wave=="Wave 1" ~ r1igxfr,
    inclusion_wave=="Wave 2" ~ r2igxfr,
    inclusion_wave=="Wave 5" ~ r5igxfr,
    inclusion_wave=="Wave 6" ~ r6igxfr,
    TRUE ~ r8igxfr),
  ritothr=case_when(
    inclusion_wave=="Wave 1" ~ r1itothr,
    inclusion_wave=="Wave 2" ~ r2itothr,
    inclusion_wave=="Wave 5" ~ r5itothr,
    inclusion_wave=="Wave 6" ~ r6itothr,
    TRUE ~ r8itothr),
  ritot=case_when(
    inclusion_wave=="Wave 1" ~ r1itot,
    inclusion_wave=="Wave 2" ~ r2itot,
    inclusion_wave=="Wave 5" ~ r5itot,
    inclusion_wave=="Wave 6" ~ r6itot,
    TRUE ~ r8itot),
  ritoth=case_when(
    inclusion_wave=="Wave 1" ~ r1itothhinc,
    inclusion_wave=="Wave 2" ~ r2itothhinc,
    inclusion_wave=="Wave 5" ~ r5itothhinc,
    inclusion_wave=="Wave 6" ~ r6itothhinc,
    TRUE ~ r8itothhinc),
  rkcnt=case_when(
    inclusion_wave=="Wave 1" ~ r2kcnt,
    inclusion_wave=="Wave 2" ~ r2kcnt,
    inclusion_wave=="Wave 5" ~ r5kcnt,
    inclusion_wave=="Wave 6" ~ r6kcnt,
    TRUE ~ r8kcnt),
  rsoc=case_when(
    inclusion_wave=="Wave 1" ~ r1socwk,
    inclusion_wave=="Wave 2" ~ r2socwk,
    inclusion_wave=="Wave 5" ~ r5socwk,
    inclusion_wave=="Wave 6" ~ r6socwk,
    TRUE ~ r8socwk),
  roccup=case_when(
    inclusion_wave=="Wave 1" ~ r1lbrf_k,
    inclusion_wave=="Wave 2" ~ r2lbrf_k,
    inclusion_wave=="Wave 5" ~ r5lbrf_k,
    inclusion_wave=="Wave 6" ~ r6lbrf_k,
    TRUE ~ r8lbrf_k),
  rbmin=case_when(
    inclusion_wave=="Wave 1" ~ r1bmi,
    inclusion_wave=="Wave 2" ~ r2bmi,
    inclusion_wave=="Wave 5" ~ r5bmi,
    inclusion_wave=="Wave 6" ~ r6bmi,
    TRUE ~ r8bmi),
  rbmicat=case_when(
    inclusion_wave=="Wave 1" ~ r1bmicat,
    inclusion_wave=="Wave 2" ~ r2bmicat,
    inclusion_wave=="Wave 5" ~ r5bmicat,
    inclusion_wave=="Wave 6" ~ r6bmicat,
    TRUE ~ r8bmicat))
######################################################
##Define covariates at inclusion for the paired spouses
######################################################
#Define spouses' covariates at inclusion for the three inclusion_wave groups
hklosa_all_sp22 <- hklosa_all_sp22 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sbyear = ifelse(person_num == 1, rabyear[2], rabyear[1]),
    sage = ifelse(person_num == 1, rage[2], rage[1]),
    sgender = ifelse(person_num == 1, ragender[2], ragender[1]),  
    seducl = ifelse(person_num == 1, raeducl[2], raeducl[1]),
    srelig = ifelse(person_num == 1, rrelig[2], rrelig[1]),
    sshlt = ifelse(person_num == 1, rshlt[2], rshlt[1]),
    sadlfive = ifelse(person_num == 1, radlfive[2], radlfive[1]),
    siadlza = ifelse(person_num == 1, riadlza[2], riadlza[1]),
    spactvg = ifelse(person_num == 1, rpactvg[2], rpactvg[1]),
    sdrinkr = ifelse(person_num == 1, rdrinkr[2], rdrinkr[1]),
    ssmokev = ifelse(person_num == 1, rsmokev[2], rsmokev[1]),
    sitearn = ifelse(person_num == 1, ritearn[2], ritearn[1]),
    sitsemp = ifelse(person_num == 1, ritsemp[2], ritsemp[1]),
    sitcap = ifelse(person_num == 1, ritcap[2], ritcap[1]),
    sipripen = ifelse(person_num == 1, ripripen[2], ripripen[1]),
    sipubpen = ifelse(person_num == 1, ripubpen[2], ripubpen[1]),
    sigxfr = ifelse(person_num == 1, rigxfr[2], rigxfr[1]),
    sitothr = ifelse(person_num == 1, ritothr[2], ritothr[1]),
    sitot = ifelse(person_num == 1, ritot[2], ritot[1]),
    sitoth = ifelse(person_num == 1, ritoth[2], ritoth[1]),
    skcnt = ifelse(person_num == 1, rkcnt[2], rkcnt[1]),
    ssoc = ifelse(person_num == 1, rsoc[2], rsoc[1]),
    smomeducl = ifelse(person_num == 1, ramomeducl[2], ramomeducl[1]),
    sdadeducl = ifelse(person_num == 1, radadeducl[2], radadeducl[1]),
    soccup = ifelse(person_num == 1, roccup[2], roccup[1]),
    sbmin = ifelse(person_num == 1, rbmin[2], rbmin[1]),
    sbmicat = ifelse(person_num == 1, rbmicat[2], rbmicat[1])) %>%
  ungroup()

#Define household income=ritearn+sitearn+ritcap+sitcap+ripripen+sipripen+ripubpen+sipubpen+rigxfr+sigxfr+ritothr+sitothr
hklosa_all_sp22 <- hklosa_all_sp22 %>% mutate(hincome=case_when(
  !is.na(ritearn) & !is.na(sitearn) & !is.na(ritcap) & !is.na(sitcap) & !is.na(ripripen) & !is.na(sipripen) & !is.na(ripubpen) & !is.na(sipubpen) & !is.na(rigxfr) & !is.na(sigxfr) & !is.na(ritothr) & !is.na(sitothr) ~ ritearn+sitearn+ritcap+sitcap+ripripen+sipripen+ripubpen+sipubpen+rigxfr+sigxfr+ritothr+sitothr,
  TRUE ~ NA),
  hincome2=case_when(
  !is.na(ritot) & !is.na(sitot) ~ ritot+sitot,
  TRUE ~ NA))

##Check potential misdefined variables
#Age at interview
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & rage != r1agey) | (inclusion_wave=="Wave 2" & rage != r2agey )| (inclusion_wave=="Wave 5" & rage != r5agey)) #None
#Self-reported health
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 6" & rshlt != r6shlt) | (inclusion_wave=="Wave 5" & rshlt != r5shlt)| (inclusion_wave=="Wave 8" & rshlt != r8shlt)) #None
#ADL summary
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & radlfive != r1adlfiveb) | (inclusion_wave=="Wave 2" & radlfive != r2adlfiveb) | (inclusion_wave=="Wave 5" & radlfive != r5adlfiveb)) #None
#IADL summary
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & riadlza != r1iadlzb) | (inclusion_wave=="Wave 2" & riadlza != r2iadlzb) | (inclusion_wave=="Wave 5" & riadlza != r5iadlzb)) #None
#Physical activity 
qc <- hklosa_all_sp22 %>% filter(inclusion_wave=="Wave 1" & rpactvg != r1vigact |inclusion_wave=="Wave 5" & rpactvg != r5vigact ) #All "r1vigact" should equal to 1
#Number of drinks/day when drinks
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & rdrinkr != r1drinkn_k) | (inclusion_wave=="Wave 2" & rdrinkr != r2drinkn_k)| (inclusion_wave=="Wave 5" & rdrinkr != r5drinkn_k)) #None
#Ever smoking
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & rsmokev != r1smokev) | (inclusion_wave=="Wave 2" & rsmokev != r2smokev)| (inclusion_wave=="Wave 5" & rsmokev != r5smokev)) #None
#Household's total income at an annual-level
qc <- hklosa_all_sp22 %>% filter(is.na(hincome)) %>% select(pid,householdID,inclusion_wave, ritearn, sitearn, ritcap, sitcap, ripripen, sipripen, ripubpen, sipubpen,rigxfr, sigxfr, ritothr, sitothr, ritot, sitot, hincome, hincome2) #66 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC
qc <- hklosa_all_sp22 %>% filter(hincome != hincome2) %>% mutate(difference=hincome-hincome2) %>%
  select(pid,householdID,inclusion_wave, ritearn, sitearn, ritcap, sitcap, ripripen, sipripen, ripubpen, sipubpen,rigxfr, sigxfr, ritothr, sitothr, ritot, sitot, hincome, hincome2, difference) #64 individuals with inconsistent hitot and hincome, but the difference was small
qc <- hklosa_all_sp22 %>% filter(is.na(hincome)) #66 missing values
qc <- hklosa_all_sp22 %>% filter(is.na(hincome2)) #66 missing values
#Use hincome2 in the analysis given there is no significant difference in value and missingness compared to hincome

#Weekly contact with children in person/by phone/email
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & rkcnt != r2kcnt) | (inclusion_wave=="Wave 2" & rkcnt != r2kcnt)| (inclusion_wave=="Wave 5" & rkcnt != r5kcnt)) #None
#Weekly social activity
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & rsoc != r1socwk) | (inclusion_wave=="Wave 2" & rsoc != r2socwk)| (inclusion_wave=="Wave 5" & rsoc != r5socwk)) #None
#Occupation
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & roccup != r1lbrf_k) | (inclusion_wave=="Wave 2" & roccup != r2lbrf_k) | (inclusion_wave=="Wave 6" & roccup != r6lbrf_k)) #None
#Self-reported BMI
qc <- hklosa_all_sp22 %>% filter((inclusion_wave=="Wave 1" & rbmin != r1bmi) | (inclusion_wave=="Wave 2" & rbmin != r2bmi)| (inclusion_wave=="Wave 5" & rbmin != r5bmi)) #None

#Save dataset hshare_all_sp25
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/KLoSA')
save(hklosa_all_sp22, file = "hklosa_all_sp22.rda")
######################################################

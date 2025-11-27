#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20250926
#UPDATED: 20251127
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE CHARLS DATA STRUCTURE, DATA PREPARATION
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#CHARLS waves 1 to 5, plus life history survey in 2014 and harmonized data of waves 1 to 4
  
#Logbook
######################################################  
#20251014 There were spousal pairs with a spouse participated from wave and the other spouse joined in later wave, when defining arthritis, use inww for wave participation
#20251015 According to missing proportion of wave 1 variables, define covariates (potential confounders)
#20251016 406 respondents in hcharls_all_sp reported no doctor-diagnosed arthritis but taking medication for arthritis, which could be used for treating other overlapping conditions.
#.        It could also be the case that the respondents were in process of being diagnosed with symptom-targeted medication, or has forgotten the diagnosis of arthritis
#20251016 There is a small number of inconsistent r1arthre and s1arthre, and r1rxarthr_c and s1rxarthr_c in hcharls_all_sp, mainly due to only one spouse participated in wave 1
#.        Use respondent variable to define outcomes
#20251017 Update research question to focus on familial aggregation where temporal relationship between respondent's arthritis and spousal arthritis does not matter
#20251017 Update inclusion criteria to request both spouses in a pair participated in at least one survey wave and request complete data on age, gender and doctor-diagnosed arthritis in at least wave
#20251020 The number of NA becames smaller when including medication variables to define other chronic conditions, which is unexpected and should use the other chronic condition variables defined by doctor-diagnosed only to refine rows with NA
#20251020 Education level 1 should be lower than upper secondary
#20251104 sarthritis_dm was wrongly deifned and it has been corrected in hcharls_all_sp11 on 20251104
#20251105 Add birth year
#20251121 Add occupation variable in ##6. Descriptive data of selected baseline and outcome variables in hcharls_all_sp11
######################################################

#Things to pay attention
###################################################### 
#250926, Count the number of spousal pair in each wave (1 to 5) in CHARLS in #1. Participation of individuals across waves (using harmonized data)
#251028 Add social activty variables RWSOCWK and SWSOCWK (added on 2025-11-17)
#20251105 birth year
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
#2. The number of eligible spousal pairs in each wave (using harmonized data)
#3. Arthritis status at inclusion among spouses
#4. Missingness of variables in hcharls_all_sp_arthre
#5. The number of individuals died during follow-up (hcharls_all_sp) 
#6. Descriptive data of selected baseline variables in hcharls_all_sp11
#7. Check strange values (hcharls_all_sp11)
#8. Missingness pattern (hcharls_all_sp11)
#9. Check multicollinearity (hcharls_all_sp11)
#10. Check if individuals had more than one marriage (hcharls_all_sp11)

#DATA PREPARATION
#1. List of included variables 
#2. Define outcomes and potential confounders to be adjusted in hcharls_all_sp
#3. Multiple imputation for covariates with missing proportion  5% < x < 40% (hcharls_all_sp11) 
#4. Post imputation modifications (hcharls_all_sp11 and hcharls_imp_all_long)
######################################################  
###################################################### 
#INSTALL AND LOAD LIBRARY, AND COUSTOM FUNCTION
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
#3. Arthritis status at inclusion among spouses
#4. Missingness of variables in hcharls_all_sp_arthre
#5. The number of individuals died during follow-up (hcharls_all_sp) 
#6. Descriptive data of selected baseline variables in hcharls_all_sp11
#7. Check strange values (hcharls_all_sp11)
#8. Missingness pattern (hcharls_all_sp11)
#9. Check multicollinearity (hcharls_all_sp11)
#10. Check if individuals had more than one marriage (hcharls_all_sp11)
######################################################
#1. Participation of individuals across waves (using harmonized data)
#1.1 Participation across wave at individual level (wave 1 to 5)
#1.2 The number of individuals and spousal pairs at each wave (wave 1 to 5)
######################################################
#1.1 Participation across wave at individual level (wave 1 to 5)
######################################################
#Load data
#Harmonized data_waves_1_to_4
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hw14 <- read_dta("H_CHARLS_D_Data.dta") %>% select(1,24:27)

#Wave 5_2020
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/CHARLS2020r')
w5 <- read_dta("Sample_Infor.dta") %>% select(1)

#Add participation indicator for wave 5_2020
hw15 <- hw14 %>% mutate(inw5=if_else(ID %in% w5$ID,1,0)) 

#Tabulate variable inw1 to inw4 to get distribution of participation across waves 1 to 4
hw15_inw_freq <- hw15 %>%   group_by(inw1, inw2, inw3, inw4, inw5) %>%
                            summarise(Frequency = n(), .groups = 'drop') %>%
                            arrange(desc(Frequency))

##Alluvial plot 
#Create a pattern identifier
hw15_inw_freq <- hw15_inw_freq %>%
  mutate(pattern_id = 1:n()) %>% relocate(pattern_id, Frequency)

#Remove labels for inw1 to inw4
hw15_inw_freq <- hw15_inw_freq  %>%
mutate(across(everything(), ~ {
  attr(., "labels") <- NULL
  attr(., "label") <- NULL
  class(.) <- setdiff(class(.), "labelled")
  return(.)
}))

#Transform data to long format for plotting
hw15_inw_freq_long <- hw15_inw_freq %>%
  pivot_longer(
    cols = starts_with("inw"),
    names_to = "Wave",
    values_to = "Participated"
  ) %>%
  mutate(Participated = factor(Participated, levels = c(1, 0), labels = c("Yes", "No")))

#Create the alluvial plot 
#Based on the data structure without information on drop out and death, the alluvial plot cannot clearly show the number of individuals included in each participation pattern across waves
#Graph to be updated after adding information on drop out and death 250825
ggplot(hw15_inw_freq_long,
       aes(x = Wave, 
           stratum = Participated, 
           alluvium = pattern_id,
           y = Frequency,
           fill = Participated,
           label = Participated)) +
  geom_flow(alpha = 0.7) +
  geom_stratum(alpha = 0.8) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(count)),
            size = 3, 
            color = "black",
            check_overlap = TRUE) +
  scale_fill_manual(values = c("Yes" = "#1f78b4", "No" = "#e31a1c")) +
  labs(title = "Participant Retention Patterns Across CHARLS Survey Waves",
       y = "Number of Participants",
       fill = "Participated") +
  theme_minimal() +
  theme(legend.position = "bottom")

##Cleveland plot
#Add pattern label
hw15_inw_freq <- hw15_inw_freq %>%
  unite("pattern_label", inw1:inw5, sep = "-", remove = FALSE)

#Create the cleveland plot
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Multimorbidity/Output/Graph')
tiff("hw15_inw_freq.tiff", width = 18, height = 12, units = "in", res = 300)
ggplot(hw15_inw_freq, aes(x = Frequency, y = reorder(pattern_label, pattern_label))) +
  geom_point(size = 3, color = "steelblue") +
  geom_segment(aes(xend = 0, yend = reorder(pattern_label, pattern_label)), color = "grey50") +
  geom_text(aes(label = paste0("n = ", Frequency)), 
            hjust = -0.2,
            size = 3.5,
            color = "black") +
  labs(title = "Frequency of Participation Patterns",
       x = "Number of Participants",
       y = "Pattern (Wave1-Wave2-Wave3-Wave4-Wave5)") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())
dev.off()

##Participation pattern that should be dropped
#Participation in one wave only: 1106 (wave1) + 373 (wave2) + 430 (wave3) + 158 (wave4) 

##The number of individuals added if including individuals with at least two participations
#Wave1: 16602
#Wave2: 3053
#Wave3: 3394
#Wave4: 470
######################################################
#1.2 The number of individuals and spousal pairs at each wave (wave 1 to 5)
######################################################
#Load data
#Harmonized data_waves_1_to_4
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hw14_is <- read_dta("H_CHARLS_D_Data.dta") %>% select(1,2,10:13,20:27, 32:35)

##Wave 1
#The total number of individual #17708
table(hw14_is$inw1) 
#QC: check the frequency of h1coupid
table <- as.data.frame(table(hw14_is$h1coupid)) #Freq_max=2
#The total number of spousal pairs #7455 
hw14_1_sp <- hw14_is %>% filter(inw1==1) %>% add_count(h1coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h1coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 2
#The total number of individual #18612
table(hw14_is$inw2) 
#QC: check the frequency of h2coupid
table <- as.data.frame(table(hw14_is$h2coupid)) #Freq_max=2
#The total number of spousal pairs #7790
hw14_2_sp <- hw14_is %>% filter(inw2==1) %>% add_count(h2coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h2coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 3
#The total number of individual #21097
table(hw14_is$inw3) 
#QC: check the frequency of h3coupid
table <- as.data.frame(table(hw14_is$h3coupid)) #Freq_max=2
#The total number of spousal pairs #8862
hw14_3_sp <- hw14_is %>% filter(inw3==1) %>% add_count(h3coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h3coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave 4
#The total number of individual #19816
table(hw14_is$inw4) 
#QC: check the frequency of h4coupid
table <- as.data.frame(table(hw14_is$h4coupid)) #Freq_max=2
#The total number of spousal pairs #8181
hw14_4_sp <- hw14_is %>% filter(inw4==1) %>% add_count(h4coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h4coupid) %>%       # Get unique categories
  summarise(unique_count = n()) 

##Wave5
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/CHARLS2020r')
demo <- read_dta("Demographic_Background.dta")
#The total number of individual #19395
length(table(demo$ID))
#QC: check the frequency of householdID
table <- as.data.frame(table(demo$householdID)) #Freq_max=2
#The total number of spousal pairs #7981
demo_5_sp <- demo %>% add_count(householdID) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(householdID) %>%       # Get unique categories
  summarise(unique_count = n()) 


######################################################
#2. The number of eligible spousal pairs in each wave (using harmonized data)
#Inclusion criteria: For each dataset, heterosexual spousal pairs in which both partners participated in at least two survey waves will be included. In cases where multiple spouses were recorded for an individual, only the first will be retained. Both spouses must have complete data on age, gender, and doctor-diagnosed arthritis.
#Exclusion criteria: To ensure the reliability of responses, spousal pairs in which either partner reported a doctor-diagnosed memory-related condition (e.g., dementia or Alzheimer’s disease) or was receiving treatment for such conditions will be excluded. Additionally, proxy interviews—often indicative of cognitive impairment—will also be excluded. 
######################################################
#Load data and select relevant variables
#Harmonized data_waves_1_to_4
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hcharls <- read_dta("H_CHARLS_D_Data.dta") %>% select(1:3,10:13, 18:40, 42:69, 82:85, 98:105, 114:189, 195:207, 224:262, 275:282,  515:530, 543:570, 603:826, 835:842, 859:866, 875:906, 915:922, 931:938, 947:970, 979:986, 995:1002, 1011:1018, 1027:1034, 1043:1050, 1059:1074, 1083:1090, 1099:1106, 1139:1146, 1151:1174, 1279:1302, 1495:1502, 1507:1516, 1551:1558, 1563:1570, 1591:1602, 1639:1642, 1761:1770, 1789:1812, 1873:1880, 1889:1904, 2021:2028, 2461:2471, 3222:3242, 3248:3252)
######################################################
##Wave 1 #13206 individuals and 6603 spousal pairs participating from wave 1
######################################################
#all spousal pairs
hcharls_w1 <- hcharls %>% filter(inw1==1 & !is.na(s1id) & s1id != "0") #14910 individuals and 7455 spousal pairs

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hcharls_w1$h1cpl) # all 1

#QC: age distribution, should be 45 or older
table(hcharls_w1$r1agey, exclude=NULL)
table(hcharls_w1$s1agey, exclude=NULL) #Five pairs with either of spouse younger than 22 years and 133 missing values on r1agey or s1agey

#Exclude spousal pairs with either of spouse younger than 22 years or with missing age values
hcharls_w1_2 <- hcharls_w1 %>% filter(r1agey >=22 & s1agey >=22) #14660 individuals and 7330 spousal pairs
hcharls_w1_2 %>% add_count(h1coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h1coupid) %>%       # Get unique categories
  summarise(unique_count = n())

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hcharls_w1_2 <- hcharls_w1_2 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre))))) 
table(hcharls_w1_2$missing_count_rarthre, exclude=NULL)
#Select 29 individual with all rwarthre missing
qc <- hcharls_w1_2 %>% filter(missing_count_rarthre==4) %>% select("ID", "householdID", "r1arthre", "r2arthre", "r3arthre", "r4arthre", "missing_count_rarthre") #29 individuals
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #two spousal pairs and 25 individuals with all rwarthre missing 

#Exclude spousal pairs with all rwarthre missing 
#hcharls_w1_3 <- hcharls_w1_2 %>% filter(!householdID %in% qc$householdID) #14606 individuals and 7303 spousal pairs
#table(as.data.frame(table(hcharls_w1_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: the number of individuals with doctor-diagnosed memory-related disease or receiving treatments for memory-related disease across four waves
qc <- hcharls_w1_2 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye)) | (r1rxmemry_c==1 & !is.na(r1rxmemry_c)) | (s1rxmemry_c==1 & !is.na(s1rxmemry_c)) |
                              (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye)) | (r2rxmemry_c==1 & !is.na(r2rxmemry_c)) | (s2rxmemry_c==1 & !is.na(s2rxmemry_c)) | 
                              (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye)) | (r3rxmemry_c==1 & !is.na(r3rxmemry_c)) | (s3rxmemry_c==1 & !is.na(s3rxmemry_c)) |
                              (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye)) | (r4rxmemry_c==1 & !is.na(r4rxmemry_c)) | (s4rxmemry_c==1 & !is.na(s4rxmemry_c))) %>% select("ID", "householdID","r1memrye", "s1memrye", "r1rxmemry_c", "s1rxmemry_c","r2memrye", "s2memrye", "r2rxmemry_c", "s2rxmemry_c", "r3memrye", "s3memrye", "r3rxmemry_c", "s3rxmemry_c", "r4memrye", "s4memrye", "r4rxmemry_c", "s4rxmemry_c") #516 individuals ans 258 spousal pairs
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #666 spousal pairs and 61 individuals with all at least one memory-related variable==1

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across four waves 
hcharls_w1_3 <- hcharls_w1_2 %>% filter(!householdID %in% qc$householdID)  #13206 individuals and 6603 spousal pairs
table(as.data.frame(table(hcharls_w1_3$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hcharls_w1_3$r1iwstat, exclude=NULL) #all 1
table(hcharls_w1_3$s1iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Create indicator of spousal participation across waves
hcharls_w1_3 <- hcharls_w1_3 %>% mutate(spousal_part_pattern = case_when(
  r1iwstat == 1 & (s1iwstat == 1 & !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 & !is.na(s4iwstat)) ~ "All waves",
  r1iwstat == 1 & (s1iwstat == 1 & !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & (r4iwstat != 1 | is.na(s4iwstat)) ~ "Waves 1-3 only",
  r1iwstat == 1 & (s1iwstat == 1 & !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & (r3iwstat != 1 | is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 & !is.na(s4iwstat)) ~ "Waves 1-2,4",
  r1iwstat == 1 & (s1iwstat == 1 & !is.na(s1iwstat)) & (r2iwstat != 1 | is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 & !is.na(s4iwstat)) ~ "Waves 1,3-4",
  r1iwstat == 1 & (s1iwstat == 1 & !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & (r3iwstat != 1 | is.na(s3iwstat)) & (r4iwstat != 1 | is.na(s4iwstat)) ~ "Waves 1-2 only",
  r1iwstat == 1 & (s1iwstat == 1 & !is.na(s1iwstat)) & (r2iwstat != 1 | is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & (r4iwstat != 1 | is.na(s4iwstat)) ~ "Waves 1,3 only",
  r1iwstat == 1 & (s1iwstat == 1 & !is.na(s1iwstat)) & (r2iwstat != 1 | is.na(s2iwstat)) & (r3iwstat != 1 | is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 & !is.na(s4iwstat)) ~ "Waves 1,4 only",
  TRUE ~ "Wave 1 only"
),
spousal_part_pattern = factor(spousal_part_pattern)
)

#Check frequency of spousal participation patterns
qc <- hcharls_w1_3 %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(qc$spousal_part_pattern, exclude=NULL)
#All waves: 8255 
#Waves 1-3 only: 1374
#Waves 1,3-4: 682
#Waves 1-2,4: 451
#Waves 1-2 only: 933
#Waves 1,3 only: 204
#Waves 1,4 only: 215
#Wave 1 only: 1092
  
#Check if misclassified
qc_spousal_part_pattern <- function(x){
  qc <- hcharls_w1_3 %>% filter(spousal_part_pattern==x)
  results <- list(
    r1 = table(qc$r1iwstat, exclude=NULL),
    r2 = table(qc$r2iwstat, exclude=NULL),
    r3 = table(qc$r3iwstat, exclude=NULL),
    r4 = table(qc$r4iwstat, exclude=NULL),
    s1 = table(qc$s1iwstat, exclude=NULL),
    s2 = table(qc$s2iwstat, exclude=NULL),
    s3 = table(qc$s3iwstat, exclude=NULL),
    s4 = table(qc$s4iwstat, exclude=NULL)
  )
  return(results)
}
qc_spousal_part_pattern("All waves")
qc_spousal_part_pattern("Waves 1-3 only")
qc_spousal_part_pattern("Waves 1,3-4")
qc_spousal_part_pattern("Waves 1-2,4")
qc_spousal_part_pattern("Waves 1-2 only")
qc_spousal_part_pattern("Waves 1,3 only")
qc_spousal_part_pattern("Waves 1,4 only")
qc_spousal_part_pattern("Wave 1 only")

#Check the number of spousal pairs among those with spousal_part_pattern=="Wave 1 only"
qc <- hcharls_w1_3 %>% filter(spousal_part_pattern =="Wave 1 only")
qc2 <- as.data.frame(table(qc$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3 <- hcharls_w1_3 %>% filter(householdID %in% qc2$Var1) %>% select("ID", "householdID", "h1coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3$spousal_part_pattern, exclude=NULL) #30 spousal pairs in which either spouse participated in wave 1 only and 531 spousal pairs in which both spouses participated in wave 1 only

#Select those with householdID count=1
#qc4 <- hcharls_w1_3 %>% filter(householdID %in% qc3$Var1) %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) #540 spousal pairs participated in wave 1 only and 30 spousal pairs in which rwistat and swistat did not align

#Exclude spousal pairs with spousal_part_pattern=="Wave 1 only"
#hcharls_w1_5 <- hcharls_w1_4 %>% filter(!(householdID %in% qc$householdID)) #12944 individuals and 6472 spousal pairs
#table(as.data.frame(table(hcharls_w1_5$h1coupid, exclude=NULL))$Freq, exclude=NULL) #All h1coupid counted twice
######################################################
##Wave 2 ##2870 individuals and 1435 spousal pairs participating from wave 2
######################################################
#All spousal pairs participating in wave 2
hcharls_w2 <- hcharls %>% filter(inw2==1 & !is.na(s2id) & s2id != "0") #15575 individuals and 7783 spousal pairs plus 9 respondents with no spouses responds to this wave

#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w2$householdID))$Freq, exclude=NULL) 33333

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hcharls_w2$h2cpl) # all 1

#Exclude individuals included in hcharls_w1_3
hcharls_w2_2 <- hcharls_w2 %>% filter(!householdID %in% hcharls_w1_3$householdID) #4553 individuals and 2275 spousal pairs and 3 individuals with no spouse responded
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w2_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 22 and above
table(hcharls_w2_2$r2agey, exclude=NULL)
table(hcharls_w2_2$s2agey, exclude=NULL) #Six pairs with either of spouse younger than 22 years and 91 missing values on r2agey and 94 missing values on s2agey
qc <- hcharls_w2_2 %>% filter(r2agey <=22 | s2agey <=22 | is.na(r2agey) | is.na(s2agey)) %>% select("ID", "householdID", "r2agey", "s2agey")
  
#Exclude spousal pairs with either of spouse younger than 22 years or with missing values on r2agey or s2agey
hcharls_w2_3 <- hcharls_w2_2 %>% filter(r2agey >=22 & s2agey >=22) #4366 individuals and 2183 spousal pairs
table(as.data.frame(table(hcharls_w2_3$householdID))$Freq, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hcharls_w2_3 <- hcharls_w2_3 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre))))) 
table(hcharls_w2_3$missing_count_rarthre, exclude=NULL)

#QC: Check the frequency of inw1
table(hcharls_w2_3$inw1, exclude=NULL) #1609 indiviudals with inw1==1, those were with their spouses not participated in wave 1

#QC: the number of individuals with doctor-diagnosed memory-related disease or receiving treatments for memory-related disease across four waves
qc <- hcharls_w2_3 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye)) | (r1rxmemry_c==1 & !is.na(r1rxmemry_c)) | (s1rxmemry_c==1 & !is.na(s1rxmemry_c)) |
                                (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye)) | (r2rxmemry_c==1 & !is.na(r2rxmemry_c)) | (s2rxmemry_c==1 & !is.na(s2rxmemry_c)) | 
                                (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye)) | (r3rxmemry_c==1 & !is.na(r3rxmemry_c)) | (s3rxmemry_c==1 & !is.na(s3rxmemry_c)) |
                                (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye)) | (r4rxmemry_c==1 & !is.na(r4rxmemry_c)) | (s4rxmemry_c==1 & !is.na(s4rxmemry_c))) %>% select("ID", "householdID","r1memrye", "s1memrye", "r1rxmemry_c", "s1rxmemry_c","r2memrye", "s2memrye", "r2rxmemry_c", "s2rxmemry_c", "r3memrye", "s3memrye", "r3rxmemry_c", "s3rxmemry_c", "r4memrye", "s4memrye", "r4rxmemry_c", "s4rxmemry_c") #516 individuals ans 258 spousal pairs
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #702 spousal pairs and 46 individuals with all at least one memory-related variable==1

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across four waves 
hcharls_w2_4 <- hcharls_w2_3 %>% filter(!householdID %in% qc$householdID)  #2870 individuals and 1435 spousal pairs
table(as.data.frame(table(hcharls_w2_4$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hcharls_w2_4$r2iwstat, exclude=NULL) #all 1
table(hcharls_w2_4$s2iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least one wave
#Create indicator of spousal participation across waves
hcharls_w2_4 <- hcharls_w2_4 %>% mutate(spousal_part_pattern = case_when(
  r1iwstat == 1 & (s1iwstat == 1 | !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 | !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 | !is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 | !is.na(s4iwstat)) ~ "All waves",
  r1iwstat == 1 & (s1iwstat == 1 | !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 | !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 | !is.na(s3iwstat)) & (r4iwstat != 1 | is.na(s4iwstat)) ~ "Waves 1-3",
  r1iwstat == 1 & (s1iwstat == 1 | !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 | !is.na(s2iwstat)) & (r3iwstat != 1 | is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 | !is.na(s4iwstat)) ~ "Waves 1-2,4",
  r1iwstat == 1 & (s1iwstat == 1 | !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 | !is.na(s2iwstat)) & (r3iwstat != 1 | is.na(s3iwstat)) & (r4iwstat != 1 | is.na(s4iwstat)) ~ "Waves 1-2",
  (r1iwstat != 1 | is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 & !is.na(s4iwstat)) ~ "Waves 2-4",
  (r1iwstat != 1 | is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & (r4iwstat != 1 | is.na(s4iwstat)) ~ "Waves 2 and 3",
  (r1iwstat != 1 | is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & (r3iwstat != 1 | is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 & !is.na(s4iwstat)) ~ "Waves 2 and 4",
  TRUE ~ "Wave 2 only"
),
spousal_part_pattern = factor(spousal_part_pattern)
)

#Check frequency of spousal participation patterns
qc <- hcharls_w2_4 %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
qc <- hcharls_w2_4 %>% filter(spousal_part_pattern=="Wave 2 only") %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(qc$spousal_part_pattern, exclude=NULL)
#Waves 2-4 only: 1833
#Waves 2 and 3: 405
#Waves 2 and 4: 212
#Wave 2 only: 420

#Check if misclassified
qc_spousal_part_pattern <- function(x){
  qc <- hcharls_w2_4 %>% filter(spousal_part_pattern==x)
  results <- list(
    r1 = table(qc$r1iwstat, exclude=NULL),
    r2 = table(qc$r2iwstat, exclude=NULL),
    r3 = table(qc$r3iwstat, exclude=NULL),
    r4 = table(qc$r4iwstat, exclude=NULL),
    s1 = table(qc$s1iwstat, exclude=NULL),
    s2 = table(qc$s2iwstat, exclude=NULL),
    s3 = table(qc$s3iwstat, exclude=NULL),
    s4 = table(qc$s4iwstat, exclude=NULL)
  )
  return(results)
}
qc_spousal_part_pattern("Waves 2-4")
qc_spousal_part_pattern("Waves 2 and 3")
qc_spousal_part_pattern("Waves 2 and 4")
qc_spousal_part_pattern("Wave 2 only")

##Some spousal pairs could have different spousal participation patterns due to death, loss of follow-up, or either spouse joined the survey later
##Such cases are fine as long as both spouses in a pairs participated in at least two waves
#Check the number of spousal pairs among different groups of spousal_part_pattern
"Waves 2-4"
qc_w2_2to4 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 2-4") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(as.data.frame(table(qc_w2_2to4$h2coupid, exclude=NULL))$Freq) # 11 rows with h2coupid==1
qc2_w2_2to4 <- as.data.frame(table(qc_w2_2to4$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_2to4 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_2to4$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_2to4$spousal_part_pattern, exclude=NULL) #4 spousal pairs in which either spouse participated in wave 2 only, rwiwstat and swiwstat did not align
#Select those with spousal_part_pattern =="Wave 2 only"
qc4_w2_2to4 <- qc3_w2_2to4 %>% filter(spousal_part_pattern =="Wave 2 only") %>% select("ID", "householdID", 16)

"Waves 2 and 3"
qc_w2_23 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 2 and 3") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(as.data.frame(table(qc_w2_23$h2coupid, exclude=NULL))$Freq) # 3 rows with h2coupid==1
qc2_w2_23 <- as.data.frame(table(qc_w2_23$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_23 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_23$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_23$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 2 and 4"
qc_w2_24 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 2 and 4") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(as.data.frame(table(qc_w2_24$h2coupid, exclude=NULL))$Freq) # 2 rows with h2coupid==1
qc2_w2_24 <- as.data.frame(table(qc_w2_24$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_24 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_24$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_24$spousal_part_pattern, exclude=NULL) #2 spousal pairs in which either spouse participated in wave 2 only, rwiwstat and swiwstat did not align
#Select those with spousal_part_pattern =="Wave 2 only"
qc4_w2_24 <- qc3_w2_24 %>% filter(spousal_part_pattern =="Wave 2 only") %>% select("ID", "householdID", 16)

"Wave 2 only"
qc_w2_2 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Wave 2 only") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(as.data.frame(table(qc_w2_2$h2coupid, exclude=NULL))$Freq) # 6 rows with h2coupid==1
qc2_w2_2 <- as.data.frame(table(qc_w2_2$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_2 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_2$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_2$spousal_part_pattern, exclude=NULL) #6 spousal pairs in which either spouse participated in wave 2 only, rwiwstat and swiwstat did not align

#Exclude spousal pairs with spousal_part_pattern =="Wave 2 only"
#hcharls_w2_5 <- hcharls_w2_4 %>% filter(!(householdID %in% qc_w2_2$householdID)) #13452 individuals and 6726 spousal pairs
#Check the number of spousal pairs
#table(as.data.frame(table(hcharls_w2_5$householdID))$Freq, exclude=NULL) 

#Check if all included spousal pairs are from sample refreshment
table(hcharls_w2_4$hacohort_c, exclude=NULL) #638 (319 spousal pairs) individuals from original sample (wave 1) and 2232 (1116 spousal pairs) individuals from sample refreshment
#Check if individuals with hacohort_c=1 are spousal pairs
qc <- hcharls_w2_4 %>% filter(hacohort_c==1) %>% select("ID", "householdID", "hacohort_c", "r1arthre","s1arthre", "r2arthre","s2arthre", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
#Check if individuals with hacohort_c=2 are spousal pairs
qc <- hcharls_w2_4 %>% filter(hacohort_c==2)
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
######################################################
##Wave 3 ##3330 individuals and 1665 spousal pairs participating from wave 3
######################################################
#All spousal pairs participating in wave 3
hcharls_w3 <- hcharls %>% filter(inw3==1 & !is.na(s3id) & s3id != "0") #17692 individuals and 8844 spousal pairs plus 4 respondents with no spouses responds to this wave
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3$householdID))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hcharls_w3$h3cpl) # all 1

#Exclude individuals included in hcharls_w1_3 and hcharls_w2_4
hcharls_w3_2 <- hcharls_w3 %>% filter(!(householdID %in% hcharls_w1_3$householdID | householdID %in% hcharls_w2_4$householdID)) #4928 individuals and 2463 spousal pairs and 2 individuals with no spouse responded
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3_2$householdID))$Freq, exclude=NULL)

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 22 and above
table(hcharls_w3_2$r3agey, exclude=NULL)
table(hcharls_w3_2$s3agey, exclude=NULL) #Six pairs with either of spouse younger than 22 years and 74 missing values on r1agey and 76 missing values on s1agey
qc <- hcharls_w3_2 %>% filter(r3agey <=22 | s3agey <=22 | is.na(r3agey) | is.na(s3agey)) %>% select("ID", "householdID", "r3agey", "s3agey") #162 individuals

#Exclude spousal pairs with either of spouse younger than 22 years or with missing values on r3agey or s3agey
hcharls_w3_3 <- hcharls_w3_2 %>% filter(r3agey >=22 & s3agey >=22) #4770 individuals and 2385 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3_3$householdID))$Freq, exclude=NULL) 
#Check age distribution
table(hcharls_w3_3$r3agey, exclude=NULL)
table(hcharls_w3_3$s3agey, exclude=NULL)

#QC: missing count of doctor-diagnosed arthritis (rwarthre)
hcharls_w3_3 <- hcharls_w3_3 %>% mutate(missing_count_rarthre = rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre))))) 
table(hcharls_w3_3$missing_count_rarthre, exclude=NULL)

#Exclude spousal pairs with missing values on doctor-diagnosed arthritis
#hcharls_w3_3 <- hcharls_w3_2 %>% filter(!is.na(r3arthre) & !is.na(s3arthre)) #13506 individuals and 6753 spousal pairs
#Check the number of spousal pairs
#table(as.data.frame(table(hcharls_w3_3$householdID))$Freq, exclude=NULL) 

#QC: the number of individuals with doctor-diagnosed memory-related disease or receiving treatments for memory-related disease across four waves
qc <- hcharls_w3_3 %>% filter((r1memrye==1 & !is.na(r1memrye)) | (s1memrye==1 & !is.na(s1memrye)) | (r1rxmemry_c==1 & !is.na(r1rxmemry_c)) | (s1rxmemry_c==1 & !is.na(s1rxmemry_c)) |
                                (r2memrye==1 & !is.na(r2memrye)) | (s2memrye==1 & !is.na(s2memrye)) | (r2rxmemry_c==1 & !is.na(r2rxmemry_c)) | (s2rxmemry_c==1 & !is.na(s2rxmemry_c)) | 
                                (r3memrye==1 & !is.na(r3memrye)) | (s3memrye==1 & !is.na(s3memrye)) | (r3rxmemry_c==1 & !is.na(r3rxmemry_c)) | (s3rxmemry_c==1 & !is.na(s3rxmemry_c)) |
                                (r4memrye==1 & !is.na(r4memrye)) | (s4memrye==1 & !is.na(s4memrye)) | (r4rxmemry_c==1 & !is.na(r4rxmemry_c)) | (s4rxmemry_c==1 & !is.na(s4rxmemry_c))) %>% select("ID", "householdID","r1memrye", "s1memrye", "r1rxmemry_c", "s1rxmemry_c","r2memrye", "s2memrye", "r2rxmemry_c", "s2rxmemry_c", "r3memrye", "s3memrye", "r3rxmemry_c", "s3rxmemry_c", "r4memrye", "s4memrye", "r4rxmemry_c", "s4rxmemry_c") 
table(as.data.frame(table(qc$householdID, exclude = NULL))$Freq, exclude=NULL) #1415 individuals and 695 spousal pairs and 25 individuals with all at least one memory-related variable==1

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease across four waves 
hcharls_w3_4 <- hcharls_w3_3 %>% filter(!householdID %in% qc$householdID)  #3330 individuals and 1665 spousal pairs
table(as.data.frame(table(hcharls_w3_4$householdID, exclude = NULL))$Freq, exclude=NULL)

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hcharls_w3_4$r3iwstat, exclude=NULL) #all 1
table(hcharls_w3_4$s3iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least two waves
#Create indicator of spousal participation across waves
hcharls_w3_4 <- hcharls_w3_4 %>% mutate(spousal_part_pattern = case_when(
  r1iwstat == 1 & (s1iwstat == 1 | !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 | !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 | !is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 | !is.na(s4iwstat)) ~ "All waves",
  r1iwstat == 1 & (s1iwstat == 1 | !is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 | !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 | !is.na(s3iwstat)) & ((r4iwstat != 1 | is.na(s4iwstat)) | (r4iwstat == 1 | is.na(s4iwstat))) ~ "Waves 1-3",
  r1iwstat == 1 & (s1iwstat == 1 | !is.na(s1iwstat)) & ((r2iwstat != 1 | is.na(s2iwstat)) | (r2iwstat == 1 | is.na(s2iwstat))) & r3iwstat == 1 & (s3iwstat == 1 | !is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 | !is.na(s4iwstat)) ~ "Waves 1,3-4",
  (r1iwstat != 1 | is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 & !is.na(s4iwstat)) ~ "Waves 2-4",
  (r1iwstat != 1 | is.na(s1iwstat)) & r2iwstat == 1 & (s2iwstat == 1 & !is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & (r4iwstat != 1 | is.na(s4iwstat)) ~ "Waves 2 and 3",
  (r1iwstat != 1 | is.na(s1iwstat)) & (r2iwstat != 1 | is.na(s2iwstat)) & r3iwstat == 1 & (s3iwstat == 1 & !is.na(s3iwstat)) & r4iwstat == 1 & (s4iwstat == 1 & !is.na(s4iwstat)) ~ "Waves 3 and 4",
  TRUE ~ "Wave 3 only"
),
spousal_part_pattern = factor(spousal_part_pattern)
)

#Check frequency of spousal participation patterns
qc <- hcharls_w3_4 %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
qc <- hcharls_w3_4 %>% filter(spousal_part_pattern=="Wave 3 only") %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(qc$spousal_part_pattern, exclude=NULL)
#All waves: 3
#Waves 1-3,4: 1
#Waves 3 and 4: 2633
#Wave 3 only: 693

#Check if misclassified
qc_spousal_part_pattern <- function(x){
  qc <- hcharls_w3_4 %>% filter(spousal_part_pattern==x)
  results <- list(
    r1 = table(qc$r1iwstat, exclude=NULL),
    r2 = table(qc$r2iwstat, exclude=NULL),
    r3 = table(qc$r3iwstat, exclude=NULL),
    r4 = table(qc$r4iwstat, exclude=NULL),
    s1 = table(qc$s1iwstat, exclude=NULL),
    s2 = table(qc$s2iwstat, exclude=NULL),
    s3 = table(qc$s3iwstat, exclude=NULL),
    s4 = table(qc$s4iwstat, exclude=NULL)
  )
  return(results)
}
qc_spousal_part_pattern("All waves")
qc_spousal_part_pattern("Waves 1,3-4")
qc_spousal_part_pattern("Waves 3 and 4")
qc_spousal_part_pattern("Wave 3 only")

##Some spousal pairs could have different spousal participation patterns due to death, loss of follow-up, or either spouse joined the survey later
##Such cases are fine as long as both spouses in a pairs participated in at least two waves
#Check the number of spousal pairs among different groups of spousal_part_pattern
"All waves"
qc_w3_aw <- hcharls_w3_4 %>% filter(spousal_part_pattern =="All waves") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(as.data.frame(table(qc_w3_aw$h3coupid, exclude=NULL))$Freq) # 12 rows with h3coupid==1
qc2_w3_aw <- as.data.frame(table(qc_w3_aw$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_aw <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_aw$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_aw$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

#"Waves 1,3-4"
qc_w3_134 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Waves 1,3-4") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(as.data.frame(table(qc_w3_134$h3coupid, exclude=NULL))$Freq) # 3 rows with h3coupid==1
qc2_w3_134 <- as.data.frame(table(qc_w3_134$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_134 <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_134$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_134$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 3 and 4"
qc_w3_34 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Waves 3 and 4") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(as.data.frame(table(qc_w3_34$h3coupid, exclude=NULL))$Freq) # 8 rows with h3coupid==1
qc2_w3_34 <- as.data.frame(table(qc_w3_34$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_34 <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_34$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_34$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Wave 3 only"
qc_w3_3 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Wave 3 only") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845)
table(as.data.frame(table(qc_w3_3$h3coupid, exclude=NULL))$Freq) # 3 rows with h3coupid==1, plus 451 spousal pairs
qc2_w3_3 <- as.data.frame(table(qc_w3_3$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_3 <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_3$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 845) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_3$spousal_part_pattern, exclude=NULL) #3 spousal pairs in which either spouse participated in wave 3 only

#Exclude spousal pairs with spousal_part_pattern =="Wave 3 only"
#hcharls_w3_5 <- hcharls_w3_3 %>% filter(!(householdID %in% qc_w3_3$householdID)) #12408 individuals and 6204 spousal pairs
#Check the number of spousal pairs
#table(as.data.frame(table(hcharls_w3_5$householdID))$Freq, exclude=NULL) 

#Check if all included spousal pairs are from sample refreshment
table(hcharls_w3_4$hacohort_c, exclude=NULL) #236 (118 spousal pairs) individuals from original sample (wave 1), 208 (104 spousal pairs) individuals from wave 2 sample refreshment, 2480 individuals (1240) from life history refreshment sample, and 406 (203 spousal pairs) individuals from wave 3 sample refreshment
#Check if individuals with hacohort_c=1 are spousal pairs
qc <- hcharls_w3_4 %>% filter(hacohort_c==1) %>% select("ID", "householdID", "hacohort_c", "r1arthre","s1arthre", "r2arthre","s2arthre", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
#Check if individuals with hacohort_c=2 are spousal pairs
qc <- hcharls_w3_4 %>% filter(hacohort_c==2)
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
#Check if individuals with hacohort_c=3 are spousal pairs
qc <- hcharls_w3_4 %>% filter(hacohort_c==3)
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
#Check if individuals with hacohort_c=4 are spousal pairs
qc <- hcharls_w3_4 %>% filter(hacohort_c==4)
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
######################################################
##All eligible spousal pairs, hcharls_all_sp, 15096 individuals and 7548 spousal pairs
######################################################
#Add inclusion wave indicator
hcharls_w1_3 <- hcharls_w1_3 %>% mutate(inclusion_wave="Wave 1")
hcharls_w2_4 <- hcharls_w2_4 %>% mutate(inclusion_wave="Wave 2")
hcharls_w3_4 <- hcharls_w3_4 %>% mutate(inclusion_wave="Wave 3")

##Dataset including all spousal pairs, combining hcharls_w1_3, hcharls_w2_4 and hcharls_w3_4)
hcharls_all_sp <- rbind(hcharls_w1_3, hcharls_w2_4, hcharls_w3_4) #19406 individuals and 9703 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_all_sp$householdID))$Freq, exclude=NULL) 
#Check frequency of inclusion_wave
table(hcharls_all_sp$inclusion_wave, exclude=NULL)
#Check if all were heterosexual spouse pairs
hetero <- hcharls_all_sp %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%
  summarise(hetero = n_distinct(ragender) == 2) %>% filter(hetero=="FALSE")
table(hetero$hetero, exclude=NULL) #16 spousal pairs were homosexual
qc <- hcharls_all_sp %>% filter(householdID %in% hetero$householdID) %>% select("ID", "householdID","ragender","s1gender", "s2gender", "s3gender", "s4gender", "inclusion_wave")  

#Exclude the homosexual spousal pair
hcharls_all_sp2 <- hcharls_all_sp %>% filter(!householdID %in% hetero$householdID) #19374 individuals and 9687 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_all_sp2$householdID))$Freq, exclude=NULL) 
#Select individuals with spousal_part_pattern=="Wave 1 only" or "Wave 2 only" or "Wave 3 only"
one_wave <- hcharls_all_sp2 %>% filter(spousal_part_pattern=="Wave 1 only" | spousal_part_pattern=="Wave 2 only" | spousal_part_pattern=="Wave 3 only") #2182 individuals and 1074 spousal pairs and 34 individuals with spouses participated in later waves
#Check the number of spousal pairs
table(as.data.frame(table(one_wave$householdID))$Freq, exclude=NULL) 

#Exclude individuals with spousal_part_pattern=="Wave 1 only" or "Wave 2 only" or "Wave 3 only"
hcharls_all_sp3 <- hcharls_all_sp2 %>% filter(!(householdID %in% one_wave$householdID)) #17158 individuals and 8579 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_all_sp3$householdID))$Freq, exclude=NULL) 

#Update variable missing_count_rarthre, also take spousal_part_pattern values into account
hcharls_all_sp3 <- hcharls_all_sp3 %>% mutate(missing_count_rarthre2=case_when(
                                              inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" ~ rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre)))),
                                              inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" ~ rowSums(is.na(across(c(r1arthre, r2arthre)))),
                                              inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" ~ rowSums(is.na(across(c(r1arthre, r2arthre, r4arthre)))),
                                              inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" ~ rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre)))),
                                              inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" ~ rowSums(is.na(across(c(r1arthre, r3arthre)))),
                                              inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4" ~ rowSums(is.na(across(c(r1arthre, r3arthre, r4arthre)))),
                                              inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" ~ rowSums(is.na(across(c(r1arthre, r4arthre)))),
                                              inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" ~ rowSums(is.na(across(c(r2arthre, r3arthre)))),
                                              inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" ~ rowSums(is.na(across(c(r2arthre, r4arthre)))),
                                              inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" ~ rowSums(is.na(across(c(r2arthre, r3arthre, r4arthre)))),
                                              inclusion_wave=="Wave 3" & spousal_part_pattern=="All waves" ~ rowSums(is.na(across(c(r1arthre, r2arthre, r3arthre, r4arthre)))),
                                              inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 1,3-4" ~ rowSums(is.na(across(c(r1arthre, r3arthre, r4arthre)))),
                                              inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" ~ rowSums(is.na(across(c(r3arthre, r4arthre))))
                                              ))

#Check missing frequency of missing_count_rarthre2 by inclusion_wave and spousal_part_pattern
table(hcharls_all_sp3$inclusion_wave, hcharls_all_sp3$spousal_part_pattern, hcharls_all_sp3$missing_count_rarthre2)
#For inclusion in wave 1, participation in all waves, 29 individuals with missing_count_rarthre>2
#For inclusion in wave 1, participation in waves 1 and 2 only, 18 individuals with missing_count_rarthre>0
#For inclusion in wave 1, participation in waves 1, 2 and 4 only, 2 individuals with missing_count_rarthre>1
#For inclusion in wave 1, participation in waves 1-3 only, 9 individuals with missing_count_rarthre>1
#For inclusion in wave 1, participation in waves 1 and 3 only, 9 individuals with missing_count_rarthre>0
#For inclusion in wave 1, participation in waves 1, 3 and 4 only, 3 individuals with missing_count_rarthre>1
#For inclusion in wave 1, participation in waves 1 and 4 only, 5 individuals with missing_count_rarthre>0

#For inclusion in wave 2, participation in waves 2 and 3 only, 84 individuals with missing_count_rarthre>0
#For inclusion in wave 2, participation in waves 2 and 4 only, 40 individuals with missing_count_rarthre>0
#For inclusion in wave 2, participation in waves 2-4 only, 266 individuals with missing_count_rarthre>1

#For inclusion in wave 2, participation in waves 3 and 4 only, 2246 individuals with missing_count_rarthre>0

#Select individuals with missing values on rwarthre exceed the upper limit
qc <- hcharls_all_sp3 %>% filter((inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & missing_count_rarthre2 > 2) |
                                 (inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & missing_count_rarthre2 > 0) |
                                 (inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & missing_count_rarthre2 > 1 ) |
                                 (inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & missing_count_rarthre2 > 1 ) |
                                 (inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & missing_count_rarthre2 > 0 ) |
                                 (inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4" & missing_count_rarthre2 > 1 ) |
                                 (inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & missing_count_rarthre2 > 0 ) |
                                 (inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & missing_count_rarthre2 > 0 ) |
                                 (inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & missing_count_rarthre2 > 0 ) |
                                 (inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & missing_count_rarthre2 > 1 ) |
                                 (inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & missing_count_rarthre2 > 0)) %>% select("ID", "householdID", "inclusion_wave", "spousal_part_pattern", "r1arthre", "r2arthre", "r3arthre", "r4arthre", "missing_count_rarthre2")
table(as.data.frame(table(qc$householdID, exclude=NULL))$Freq, exclude=NULL) #1137 spousal pairs and 437 indiviudals

#Exclude individuals with missing values on rwarthre exceed the upper limit
hcharls_all_sp4 <- hcharls_all_sp3 %>% filter(!(householdID %in% qc$householdID)) #14010 individuals and 7005 spousal pairs
table(as.data.frame(table(hcharls_all_sp4$householdID, exclude=NULL))$Freq, exclude=NULL) 

#Check the frequency of missing_count_rarthre2 by inclusion_wave and spousal_part_pattern
table(hcharls_all_sp4$inclusion_wave, hcharls_all_sp4$spousal_part_pattern, hcharls_all_sp4$missing_count_rarthre2, exclude=NULL) #Passed QC
######################################################
######################################################
#3. Arthritis status at inclusion among spouses
#3.1 Based on doctor-diagnosed data only
#3.2 Based on doctor-diagnosed data and taking medication for arthritis
##All eligible spousal pairs, excluding those with concordant arthritis status based on doctor diagnosis and medication: 12740 individuals and 6370 spousal pairs
######################################################
#3.1 Based on doctor-diagnosed data only
######################################################
##Wave 1 (all spousal pairs included in wave 1)
#The number of spousal pairs with both spouses with doctor-diagnosed arthritis at inclusion
con_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 1" & r1arthre==1 & s1arthre==1) #1558 individuals and 779 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_all_sp4 %>% filter((inclusion_wave=="Wave 1" & r1arthre==0 & s1arthre==1) | (inclusion_wave=="Wave 1" & r1arthre==1 & s1arthre==0)) #4332 individuals and 2166 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 1" & r1arthre==0 & s1arthre==0) #6052 individuals and 3026 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL) 

##Wave 2 (all spousal pairs included in wave 2)
#The number of spousal pairs with both spouses with doctor-diagnosed arthritis at inclusion
con_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 2" & r2arthre==1 & s2arthre==1) #178 individuals and 89 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_all_sp4 %>% filter((inclusion_wave=="Wave 2" & r2arthre==0 & s2arthre==1) | (inclusion_wave=="Wave 2" & r2arthre==1 & s2arthre==0)) #550 individuals and 275 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 2" & r2arthre==0 & s2arthre==0) #1094 individuals and 547 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL)

##Wave 3 (all spousal pairs included in wave 3)
#The number of spousal pairs with both spouses with doctor-diagnosed arthritis at inclusion
con_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 3" & r3arthre==1 & s3arthre==1) #16 individuals and 8 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_all_sp4 %>% filter((inclusion_wave=="Wave 3" & r3arthre==0 & s3arthre==1) | (inclusion_wave=="Wave 3" & r3arthre==1 & s3arthre==0)) #48 individuals and 24 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 3" & r3arthre==0 & s3arthre==0) #172 individuals and 86 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL)

##Dataset including a new variable arthritis_doc_con as indicator of consistency rwarthre and swarthre at inclusion
hcharls_all_sp4 <- hcharls_all_sp4 %>% mutate(rsarthritis_d_con=case_when(
                                              inclusion_wave=="Wave 1" & r1arthre==1 & s1arthre==1 ~ "Con at inclusion",
                                              (inclusion_wave=="Wave 1" & r1arthre==0 & s1arthre==1) | (inclusion_wave=="Wave 1" & r1arthre==1 & s1arthre==0) ~ "Dis at inclusion",
                                              inclusion_wave=="Wave 1" & r1arthre==0 & s1arthre==0 ~ "Both no arthrthis at inclusion",
                                              inclusion_wave=="Wave 2" & r2arthre==1 & s2arthre==1 ~ "Con at inclusion",
                                              (inclusion_wave=="Wave 2" & r2arthre==0 & s2arthre==1) | (inclusion_wave=="Wave 2" & r2arthre==1 & s2arthre==0) ~ "Dis at inclusion",
                                              inclusion_wave=="Wave 2" & r2arthre==0 & s2arthre==0 ~ "Both no arthrthis at inclusion",
                                              inclusion_wave=="Wave 3" & r3arthre==1 & s3arthre==1 ~ "Con at inclusion",
                                              (inclusion_wave=="Wave 3" & r3arthre==0 & s3arthre==1) | (inclusion_wave=="Wave 3" & r3arthre==1 & s3arthre==0) ~ "Dis at inclusion",
                                              inclusion_wave=="Wave 3" & r3arthre==0 & s3arthre==0 ~ "Both no arthrthis at inclusion"))
#Check the number of spousal pairs with rsarthritis_dc_con by inclusion_wave
table(hcharls_all_sp4$inclusion_wave, hcharls_all_sp4$rsarthritis_d_con, exclude=NULL) #10 individuals with missing values
qc <- hcharls_all_sp4 %>% filter(is.na(rsarthritis_d_con)) %>% select("ID", "householdID", "inclusion_wave","inw1", "inw2", "inw3", "inw4","r1arthre" , "r2arthre", "r3arthre", "r4arthre", "s1arthre" , "s2arthre", "s3arthre", "s4arthre" )
# 2 spousal pairs included from wave 1, one discordant on arthritis in wave 3 and one both with no arthritis in wave 3
# 3 spousal pairs included from wave 2, two discordant on arthritis in wave 3 and one concordant in wave 3
######################################################
#3.2 Based on doctor-diagnosed data and taking medication for arthritis
######################################################
#Creating a new variable indicating arthritis based on doctor diagnosis and medication
hcharls_all_sp4 <- hcharls_all_sp4 %>% mutate(r_arthritis_dm=case_when(
                                              (inclusion_wave=="Wave 1" & (r1arthre==1 | r1rxarthr_c==1))|(inclusion_wave=="Wave 1" & (r1arthre==1 | is.na(r1rxarthr_c))) ~ 1,
                                              (inclusion_wave=="Wave 2" & (r2arthre==1 | r2rxarthr_c==1))|(inclusion_wave=="Wave 2" & (r2arthre==1 | is.na(r2rxarthr_c))) ~ 1,
                                              (inclusion_wave=="Wave 3" & (r3arthre==1 | r3rxarthr_c==1))|(inclusion_wave=="Wave 3" & (r3arthre==1 | is.na(r3rxarthr_c))) ~ 1,
                                              TRUE ~ 0)) %>%
                                       mutate(s_arthritis_dm=case_when(
                                              (inclusion_wave=="Wave 1" & (s1arthre==1 | s1rxarthr_c==1))|(inclusion_wave=="Wave 1" & (s1arthre==1 | is.na(s1rxarthr_c))) ~ 1,
                                              (inclusion_wave=="Wave 2" & (s2arthre==1 | s2rxarthr_c==1))|(inclusion_wave=="Wave 2" & (s2arthre==1 | is.na(s2rxarthr_c))) ~ 1,
                                              (inclusion_wave=="Wave 3" & (s3arthre==1 | s3rxarthr_c==1))|(inclusion_wave=="Wave 3" & (s3arthre==1 | is.na(s3rxarthr_c))) ~ 1,
                                              TRUE ~ 0)) 
table(hcharls_all_sp4$inclusion_wave, hcharls_all_sp4$r_arthritis_dm, exclude=NULL)
table(hcharls_all_sp4$inclusion_wave, hcharls_all_sp4$s_arthritis_dm, exclude=NULL)

#Qc: any missing values in r1rxarthr_c and s1rxarthr_c
table(hcharls_all_sp4$inclusion_wave, hcharls_all_sp4$r1rxarthr_c, exclude=NULL) #22 missing values 
table(hcharls_all_sp4$inclusion_wave, hcharls_all_sp4$s1rxarthr_c, exclude=NULL) #22 missing values 

##Wave 1 (all spousal pairs included in wave 1)
#The number of spousal pairs with both spouses with doctor-diagnosed/medication-indicated arthritis at inclusion
con_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 1" & r_arthritis_dm==1 & s_arthritis_dm==1) #1902 individuals and 951 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_all_sp4 %>% filter((inclusion_wave=="Wave 1" & r_arthritis_dm==0 & s_arthritis_dm==1) | (inclusion_wave=="Wave 1" & r_arthritis_dm==1 & s_arthritis_dm==0)) #4402 individuals and 2201 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 1" & r_arthritis_dm==0 & s_arthritis_dm==0) #5642 individuals and 2821 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL) 

##Wave 2 (all spousal pairs included in wave 2)
#The number of spousal pairs with both spouses with doctor-diagnosed arthritis at inclusion
con_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 2" & r_arthritis_dm==1 & s_arthritis_dm==1) #208 individuals and 204 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_all_sp4 %>% filter((inclusion_wave=="Wave 2" & r_arthritis_dm==0 & s_arthritis_dm==1) | (inclusion_wave=="Wave 2" & r_arthritis_dm==1 & s_arthritis_dm==0)) #566 individuals and 283 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 2" & r_arthritis_dm==0 & s_arthritis_dm==0) #1054 individuals and 527 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL)

##Wave 3 (all spousal pairs included in wave 3)
#The number of spousal pairs with both spouses with doctor-diagnosed arthritis at inclusion
con_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 3" & r_arthritis_dm==1 & s_arthritis_dm==1) #18 individuals and 9 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_all_sp4 %>% filter((inclusion_wave=="Wave 3" & r_arthritis_dm==0 & s_arthritis_dm==1) | (inclusion_wave=="Wave 3" & r_arthritis_dm==1 & s_arthritis_dm==0)) #50 individuals and 25 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_all_sp4 %>% filter(inclusion_wave=="Wave 3" & r_arthritis_dm==0 & s_arthritis_dm==0) #168 individuals and 84 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL)

##Dataset including a new variable arthritis_dm_con as indicator of consistency rwarthre and rwrxarthr_c, and swarthre and swrxarthr_c at inclusion
hcharls_all_sp4 <- hcharls_all_sp4 %>% mutate(rsarthritis_dm_con=case_when(
  inclusion_wave=="Wave 1" & r_arthritis_dm==1 & s_arthritis_dm==1 ~ "Con at inclusion",
  (inclusion_wave=="Wave 1" & r_arthritis_dm==0 & s_arthritis_dm==1) | (inclusion_wave=="Wave 1" & r_arthritis_dm==1 & s_arthritis_dm==0) ~ "Dis at inclusion",
  inclusion_wave=="Wave 1" & r_arthritis_dm==0 & s_arthritis_dm==0 ~ "Both no arthrthis at inclusion",
  inclusion_wave=="Wave 2" & r_arthritis_dm==1 & s_arthritis_dm==1 ~ "Con at inclusion",
  (inclusion_wave=="Wave 2" & r_arthritis_dm==0 & s_arthritis_dm==1) | (inclusion_wave=="Wave 2" & r_arthritis_dm==1 & s_arthritis_dm==0) ~ "Dis at inclusion",
  inclusion_wave=="Wave 2" & r_arthritis_dm==0 & s_arthritis_dm==0 ~ "Both no arthrthis at inclusion",
  inclusion_wave=="Wave 3" & r_arthritis_dm==1 & s_arthritis_dm==1 ~ "Con at inclusion",
  (inclusion_wave=="Wave 3" & r_arthritis_dm==0 & s_arthritis_dm==1) | (inclusion_wave=="Wave 3" & r_arthritis_dm==1 & s_arthritis_dm==0) ~ "Dis at inclusion",
  inclusion_wave=="Wave 3" & r_arthritis_dm==0 & s_arthritis_dm==0 ~ "Both no arthrthis at inclusion"))
#Check the frequency of r_arthrthis_dm and s_arthritis_dm by inclusion_wave
table(hcharls_all_sp4$inclusion_wave, hcharls_all_sp4$rsarthritis_dm_con, exclude=NULL) #10 individuals with missing values
######################################################
######################################################
#4. Missingness of variables in hcharls_all_sp_arthre
######################################################
##Wave 1
missing_analysis <- function(data) {
  data.frame(
    Variable = names(data),
    Type = sapply(data, function(x) class(x)[1]),
    N_Complete = colSums(!is.na(data)),
    N_Missing = colSums(is.na(data)),
    Proportion_Missing = round(colSums(is.na(data)) / nrow(data), 3)
  ) |> 
    arrange(desc(Proportion_Missing))
}

missing_table_in_w1 <- missing_analysis(hcharls_all_sp4[hcharls_all_sp4$inclusion_wave == "Wave 1", ])
#r1mbmicat/s1mbmicat missing proportion was about 20%, in line with the fact that the 78.9% of completeness of physical examination in wave 1, information based on Chinese_users__guide_20130407_
#missingness for BMI unlikely happened at random given those without physical examination in wave 1 likely had physical disability or worse health condition, younger men (work outside) and older women

##Wave 2
missing_table_in_w2 <- missing_analysis(hcharls_all_sp4[hcharls_all_sp4$inclusion_wave == "Wave 2", ])

##Wave 3
missing_table_in_w3 <- missing_analysis(hcharls_all_sp4[hcharls_all_sp4$inclusion_wave == "Wave 3", ])

#Save missing tables
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/CHARLS")
write_xlsx(missing_table_in_w1, path = "Charls_missing_table_in_w1_updated.xlsx", col_names=T, format_headers=T)
write_xlsx(missing_table_in_w2, path = "Charls_missing_table_in_w2_updated.xlsx", col_names=T, format_headers=T)
write_xlsx(missing_table_in_w3, path = "Charls_missing_table_in_w3_updated.xlsx", col_names=T, format_headers=T)

######################################################
#5. The number of individuals died during follow-up (hcharls_all_sp)
#radyear in dataset hcharls_all_sp_arthre is completely missing, indicating no one died during follow-up
#Double check using harmonized end of life dataset
######################################################
#Load harmonized end of life dataset
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hcharls_eol <- read_dta("H_CHARLS_EOL_a.dta")

#Check if any individuals in hcharls_all_sp also in hcharls_eol as well
qc <- hcharls_all_sp %>% filter(householdID %in% hcharls_eol$householdID) #None

#Check frequency of rwiwstat, values 5 and 6 indicating death, and value 9 indicating 'don't know alive or died'
table(hcharls_all_sp_arthre$r1iwstat, exclude=NULL) #No death indicators
table(hcharls_all_sp_arthre$r2iwstat, exclude=NULL) #No death indicators
table(hcharls_all_sp_arthre$r3iwstat, exclude=NULL) #236 individuals died in wave 3 and 319 individuals with unknown death status
table(hcharls_all_sp_arthre$r4iwstat, exclude=NULL) #455 individuals died in wave 4, 236 individual died in wave 3, and 1077 individuals with unknown death status
######################################################
#6. Descriptive data of selected baseline and outcome variables in hcharls_all_sp11
######################################################
#Load data
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
load("hcharls_all_sp11.rda")

#Select baseline and outcome variables and convert variables with have_labelled to factor variables
descriptive <- hcharls_all_sp11 %>%
  select(rage, ragender, hrural, rmcurln, raeducl, rmbmin, rmbmicat, rpact, rdrinkr, rsmokev, roccup, hincome, hkcnt, ramomeducl, radadeducl, rclstress, rclstress2, rshlta, radlfive, rmobilsev, arthritis, arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, dyslipe, dyslipe_dm, livere, livere_dm, kidneye, kidneye_dm, digeste, digeste_dm, asthmae) %>%
  mutate(across(where(haven::is.labelled), as.numeric))
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
      hrural ~ "Household living area",
      rmcurln ~ "Length of current marriage",
      raeducl ~ "Highest attained education level",
      rmbmin ~ "Measured BMI_cont",
      rmbmicat ~ "Measured BMI_cat",
      rpact ~ "Physcial activity",
      rdrinkr ~ "Number of drinks per day",
      rsmokev ~ "Ever smoking",
      roccup ~ "Occupation",
      hincome ~ "Household income",
      hkcnt ~ "Weekly contact with children",
      ramomeducl ~ "Maternal education",
      radadeducl ~ "Paternal education",
      rclstress ~ "Childhood/lifetime stressful events (four vars)",
      rclstress2 ~ "Childhood/lifetime stressful events (lifethe/chdeath)",
      rshlta ~ "Self-reported health, alternative scale",
      radlfive ~ "ADL summary, five",
      rmobilsev ~ "Mobility summary, seven",
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
      dyslipe ~ "Doctor-diagnosed dyslipidemia",
      dyslipe_dm ~ "Doctor-diagnosed/medication-indicated dyslipidemia",
      livere ~ "Doctor-diagnosed liver disease",
      livere_dm ~ "Doctor-diagnosed/medication-indicated liver disease",
      kidneye ~ "Doctor-diagnosed kidney disease",
      kidneye_dm ~ "Doctor-diagnosed/medication-indicated kidney disease",
      digeste ~ "Doctor-diagnosed digestive disease",
      digeste_dm ~ "Doctor-diagnosed/medication-indicated digestive disease",
      asthmae ~ "Doctor-diagnosed asthmae")
  ) %>%
  # Additional formatting
  add_p() %>%  # Add p-values for group comparisons
  add_overall() %>%  # Add overall column
bold_labels() %>%
  italicize_levels()

#Save descriptive table
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/CHARLS")
write_xlsx(descriptive_base_var_by_gender[["table_body"]], path = "descriptive_base_out_var_by_gender.xlsx", col_names=T, format_headers=T)
######################################################
#7. Check strange values (hcharls_all_sp11)
######################################################
#Load dataset hcharls_all_sp11 and select relevant variables
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
load("hcharls_all_sp11.rda") 

#Select relevant variable
hcharls_strange <- hcharls_all_sp11 %>% select(ID, rage, rabyear, sbyear, ragender, hrural, rmcurln, raeducl, rmbmin, rmbmicat, rpact, rdrinkr, rsmokev, roccup, roccup3, hincome, hkcnt, ramomeducl, radadeducl, rclstress, rclstress2, rshlta, radlfive, rmobilsev, arthritis, arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, dyslipe, dyslipe_dm, livere, livere_dm, kidneye, kidneye_dm, digeste, digeste_dm, asthmae) %>%
                                  mutate(across(where(haven::is.labelled), as.numeric))

#Function for checking frequency of each continuous variable
freq <- function(data, var){
  freq_table <- as.data.frame(table(data[[var]], exclude=NULL))
  return(freq_table)
}
freq_table <- freq(hcharls_all_sp11, "rage")
freq_table <- freq(hcharls_all_sp11, "rabyear")
freq_table <- freq(hcharls_all_sp11, "rmcurln")
freq_table <- freq(hcharls_all_sp11, "rmbmin") #1 individual with BMI < 10 and 8 individuals with BMI > 100, those values are biologically implausible 
freq_table <- freq(hcharls_all_sp11, "hincome") #with negative and zero values
freq_table <- freq(hcharls_all_sp11, "rdrinkr")

#Add an indicator for strange BMI values that would be excluded from analysis
hcharls_all_sp11 <- hcharls_all_sp11 %>% mutate(BMI_exclude=ifelse(rmbmin<10 | rmbmin>100,1,0))
table(hcharls_all_sp11$BMI_exclude, exclude = NULL) #9 observations with biologically implausible values

#Save dataset hcharls_all_sp11
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
save(hcharls_all_sp11, file = "hcharls_all_sp11.rda")
######################################################
#8. Missingness pattern (hcharls_all_sp11)
######################################################
#Load dataset hcharls_all_sp11 and select relevant variables
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
load("hcharls_all_sp11.rda")  

hcharls_miss <- hcharls_all_sp11 %>% select(ID, rage, rabyear, sbyear, ragender, hrural, rmcurln, raeducl, rmbmin, rmbmicat, rpact, rdrinkr, rsmokev, roccup, roccup3, hincome, hkcnt, ramomeducl, radadeducl, rclstress, rclstress2, rshlta, radlfive, rmobilsev, arthritis, arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, dyslipe, dyslipe_dm, livere, livere_dm, kidneye, kidneye_dm, digeste, digeste_dm, asthmae)

#Visual Missingness Map
vis_miss(hcharls_miss)
#Individuals with physical activity missing, also likley have BMI and childhood/lifetime stressful event missing 

#Visualize missing patterns
hcharls_mp <- md.pattern(hcharls_miss)
hcharls_mp_pair <- md.pairs(hcharls_miss) #pairwise missing pattern
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Graphs')
tiff("hcharls_missing_patterns.tiff", width = 12, height = 16, units = "in", res = 300)
gg_miss_upset(hcharls_miss)
dev.off()

#Check missing distribution between rmbmin and hincome
marginplot(hcharls_miss[, c("rmbmin", "hincome")], col=mdc(1:2), cex=1.2, cex.lab=1.2, cex.numbers=1.3, pch=19)
#Similar distribution between observed and missing, likely missing at random

# Show percent of missing values per variable
gg_miss_var(hcharls_miss, show_pct = TRUE)

# Get a summary table for each variable
miss_var_summary(hcharls_miss)
######################################################
#9. Check multicollinearity (hcharls_all_sp11)
######################################################
#Identify highly correlated variables
numeric_vars <- hcharls_miss[sapply(hcharls_miss, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

#Find variables with correlation > 0.9
high_cor <- which(abs(cor_matrix) > 0.9 & upper.tri(cor_matrix), arr.ind = TRUE)
print(high_cor)
#rage and rabyear; r1mcurln and r2mcurln are highly correlated, with cor > 0.9

#Save cor_matrix
cor_df <- as.data.frame(cor_matrix)
cor_df$Variable <- rownames(cor_matrix)
cor_df <- cor_df[, c("Variable", setdiff(names(cor_df), "Variable"))]
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/CHARLS')
write.csv(cor_df, "charls_cor_matrix.csv", row.names = FALSE)
######################################################
#10. Check if individuals had more than one marriage (hcharls_all_sp11)
######################################################
#Load data
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
load("hcharls_all_sp11.rda")

#Identify individuals with more than one spouses
qc <- as.data.frame(table(hcharls_all_sp11$ID)) %>% filter(Freq>1) #No individuals had more than one marriage
######################################################
#DATA PREPARATION
#1. List of included variables 
#2. Define outcomes and potential confounders to be adjusted in hcharls_all_sp
#3. Multiple imputation for covariates with missing proportion  5% < x < 40% (hcharls_all_sp) 
#4. Post imputation modifications (hcharls_all_sp11 and hcharls_imp_all_long)
###################################################### 
#1. List of included variables
######################################################  
#Load data and select relevant variables
#Harmonized data_waves_1_to_4
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hcharls <- read_dta("H_CHARLS_D_Data.dta") %>% select(1:3,10:13, 18:40, 42:69, 82:85, 98:105, 114:189, 195:207, 224:262, 275:282,  515:530, 543:570, 603:826, 835:842, 859:866, 875:906, 915:922, 931:938, 947:970, 979:986, 995:1002, 1011:1018, 1027:1034, 1043:1050, 1059:1074, 1083:1090, 1099:1106, 1139:1146, 1151:1174, 1279:1302, 1495:1502, 1507:1516, 1551:1558, 1563:1570, 1591:1602, 1639:1642, 1761:1770, 1789:1812, 1873:1880, 1889:1904, 2021:2028, 2461:2471, 3222:3234, 3248:3252)

# Check if labels exist as attributes
var_info <- sapply(hcharls, function(x) {
  label <- attr(x, "label")
  ifelse(is.null(label), "", label)
})

# Extract value labels from attributes
val_info <- sapply(hcharls, function(x) {
  labels <- attr(x, "labels")
  if(!is.null(labels)) {
    # Format as "1=Label1; 2=Label2; ..."
    paste(paste(labels, "=", names(labels)), collapse = "; ")
  } else {
    ""
  }
})

#Create a detailed table
hcharls_variable_table <- data.frame(
  Variable = names(hcharls),
  Label = sapply(var_info, function(x) ifelse(is.null(x), "", x)),
  Type = sapply(hcharls, function(x) paste(class(x), collapse = ", ")),
  N_Missing = colSums(is.na(hcharls)),
  Unique_Values = sapply(hcharls, function(x) length(unique(na.omit(x)))),
  stringsAsFactors = FALSE
)

#Add value labels as a separate column
hcharls_variable_table$Value_Labels <- sapply(names(hcharls), function(var) {
  labels <- val_info[[var]]
  if(!is.null(labels)) {
    paste(paste0(names(labels), " (", labels, ")"), collapse = "; ")
  } else {
    ""
  }
})

#Save as excel file
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Basic information')
write.xlsx(hcharls_variable_table, file = "hcharls_selected_variable_table_252009.xlsx", colNames=T, format_headers=T)
######################################################  
#2. Define outcomes and potential confounder to be adjusted in hcharls_all_sp
######################################################  
##Add back R1CHDEATHE and S1CHDEATHE to hcharls_all_sp
#Save dataset hcharls_all_sp
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
save(hcharls_all_sp4, file = "hcharls_all_sp4.rda")
######################################################  
##Define doctor-diagnosed arthritis (outcome, cases/controls)
######################################################  
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 1 
hcharls_all_sp4 <- hcharls_all_sp4 %>% mutate(arthritis=case_when(
                                                      inclusion_wave!="Wave 1"~ NA, 
                                                      inclusion_wave=="Wave 1" & (r1arthre==1 |r2arthre==1|r3arthre==1|r4arthre==1) ~ 1,
                                                      TRUE ~ 0))
#Check the frequency of arthritis
table(hcharls_all_sp4$arthritis, hcharls_all_sp4$inclusion_wave, exclude=NULL) #For those included from wave 1, 6138 individuals with no arthritis across four waves and 5808 individuals with arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hcharls_all_sp4 %>% filter(arthritis==0) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hcharls_all_sp4 %>% filter(arthritis==1) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) #All had 1 
######################################################  
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 2
hcharls_all_sp5 <- hcharls_all_sp4 %>% mutate(arthritis=case_when(
  inclusion_wave=="Wave 1" ~ arthritis,
  inclusion_wave=="Wave 3"~ NA, 
  inclusion_wave=="Wave 2" & (r2arthre==1|r3arthre==1|r4arthre==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hcharls_all_sp5$arthritis, hcharls_all_sp5$inclusion_wave, exclude=NULL) #For those inclued from wave 2, 1100 individuals with no arthritis 728 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hcharls_all_sp5 %>% filter(arthritis==0) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hcharls_all_sp5 %>% filter(arthritis==1) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) #All had 1 
###################################################### 
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 2
hcharls_all_sp6 <- hcharls_all_sp5 %>% mutate(arthritis=case_when(
  inclusion_wave=="Wave 1" ~ arthritis,
  inclusion_wave=="Wave 2" ~ arthritis,
  inclusion_wave=="Wave 3" & (r3arthre==1|r4arthre==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hcharls_all_sp6$arthritis, hcharls_all_sp6$inclusion_wave, exclude=NULL) #For those included from wave 3, 170 individuals with no arthritis and 66 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hcharls_all_sp6 %>% filter(arthritis==0) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hcharls_all_sp6 %>% filter(arthritis==1) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) #All had 1 
###################################################### 
##Define doctor-diagnosed arthritis (exposure)
###################################################### 
hcharls_all_sp6 <- hcharls_all_sp6 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sarthritis = ifelse(person_num == 1, arthritis[2], arthritis[1])
  ) %>%
  ungroup()

#Check the frequency of sarthritis
table(hcharls_all_sp6$sarthritis, exclude=NULL)

#Compare the proportion of men with arthritis who had wives affected by arthritis to those without
male <- hcharls_all_sp6 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_arthritis = male$sarthritis
)
freq_table(freq) #65% versus 43%
chisq.test(male$arthritis, male$sarthritis) #Significant

#Compare the proportion of women with arthritis who had husbands affected by arthritis to those without
female <- hcharls_all_sp6 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_arthritis = female$sarthritis
)
freq_table(freq2) #53% versus 31%
chisq.test(female$arthritis, female$sarthritis) #Significant
######################################################  
##Define doctor-diagnosed/medication-indicated arthritis (outcome, cases/controls)
#No individuals with missing values of rwrxarthr_c exceed the upper limit
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included from wave 1 
hcharls_all_sp6 <- hcharls_all_sp6 %>% mutate(arthritis_dm=case_when(
  inclusion_wave!="Wave 1"~ NA, 
  inclusion_wave=="Wave 1" & (r1arthre==1 |r2arthre==1|r3arthre==1|r4arthre==1|r1rxarthr_c==1|r2rxarthr_c==1|r3rxarthr_c==1|r4rxarthr_c==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis_dm
table(hcharls_all_sp6$arthritis_dm, hcharls_all_sp6$inclusion_wave, exclude=NULL) #For those included from wave 1, 6113 individuals with no doctor-diagnosed/medication-indicated arthritis across four waves and 5833 individuals with doctor-diagnosed/medication-indicated arthritis 
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hcharls_all_sp6 %>% filter(arthritis_dm==0) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre", "r1rxarthr_c", "r2rxarthr_c", "r3rxarthr_c", "r4rxarthr_c")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r1rxarthr_c, exclude=NULL) 
table(qc$r2rxarthr_c, exclude=NULL) 
table(qc$r3rxarthr_c, exclude=NULL)
table(qc$r4rxarthr_c, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hcharls_all_sp6 %>% filter(arthritis_dm==1) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre", "r1rxarthr_c", "r2rxarthr_c", "r3rxarthr_c", "r4rxarthr_c")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r1rxarthr_c, exclude=NULL) 
table(qc$r2rxarthr_c, exclude=NULL) 
table(qc$r3rxarthr_c, exclude=NULL) 
table(qc$r4rxarthr_c, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed/medication-indicated arthritis (case) among spousal pairs included in wave 2
hcharls_all_sp7 <- hcharls_all_sp6 %>% mutate(arthritis_dm=case_when(
  inclusion_wave=="Wave 1" ~ arthritis_dm,
  inclusion_wave=="Wave 3"~ NA, 
  inclusion_wave=="Wave 2" & (r2arthre==1|r3arthre==1|r4arthre==1|r2rxarthr_c==1|r3rxarthr_c==1|r4rxarthr_c==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hcharls_all_sp7$arthritis_dm, hcharls_all_sp6$inclusion_wave, exclude=NULL) #For those included from wave 2, 1100 individuals with no arthritis 728 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hcharls_all_sp7 %>% filter(arthritis_dm==0) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre", "r1rxarthr_c", "r2rxarthr_c", "r3rxarthr_c", "r4rxarthr_c")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r1rxarthr_c, exclude=NULL) 
table(qc$r2rxarthr_c, exclude=NULL) 
table(qc$r3rxarthr_c, exclude=NULL) 
table(qc$r4rxarthr_c, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hcharls_all_sp7 %>% filter(arthritis_dm==1) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre", "r1rxarthr_c", "r2rxarthr_c", "r3rxarthr_c", "r4rxarthr_c")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL) 
table(qc$r1rxarthr_c, exclude=NULL) 
table(qc$r2rxarthr_c, exclude=NULL) 
table(qc$r3rxarthr_c, exclude=NULL) 
table(qc$r4rxarthr_c, exclude=NULL) #All had 1 
######################################################
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed arthritis (case) among spousal pairs included in wave 2
hcharls_all_sp8 <- hcharls_all_sp7 %>% mutate(arthritis_dm=case_when(
  inclusion_wave=="Wave 1" ~ arthritis_dm,
  inclusion_wave=="Wave 2" ~ arthritis_dm,
  inclusion_wave=="Wave 3" & (r3arthre==1|r4arthre==1|r3rxarthr_c==1|r4rxarthr_c==1) ~ 1,
  TRUE ~ 0))
#Check the frequency of arthritis
table(hcharls_all_sp8$arthritis_dm, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 3, 170 individuals with no arthritis and 66 individuals with arthritis
#Check the frequencies of arthritis variables among those individuals with no arthritis across four waves
qc <- hcharls_all_sp8 %>% filter(arthritis_dm==0) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre", "r1rxarthr_c", "r2rxarthr_c", "r3rxarthr_c", "r4rxarthr_c")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL)
table(qc$r1rxarthr_c, exclude=NULL) 
table(qc$r2rxarthr_c, exclude=NULL) 
table(qc$r3rxarthr_c, exclude=NULL) 
table(qc$r4rxarthr_c, exclude=NULL) #All 0 or NA
#Check the frequencies of arthritis variables among those individuals with arthritis
qc <- hcharls_all_sp8 %>% filter(arthritis_dm==1) %>% select("ID", "householdID", "arthritis", "r1arthre", "r2arthre" , "r3arthre", "r4arthre", "r1rxarthr_c", "r2rxarthr_c", "r3rxarthr_c", "r4rxarthr_c")
table(qc$r1arthre, exclude=NULL)
table(qc$r2arthre, exclude=NULL)
table(qc$r3arthre, exclude=NULL)
table(qc$r4arthre, exclude=NULL)
table(qc$r1rxarthr_c, exclude=NULL) 
table(qc$r2rxarthr_c, exclude=NULL) 
table(qc$r3rxarthr_c, exclude=NULL) 
table(qc$r4rxarthr_c, exclude=NULL) #All had 1 
###################################################### 
##Define doctor-diagnosed/medication-indicated arthritis (exposure)
###################################################### 
hcharls_all_sp8 <- hcharls_all_sp8 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sarthritis_dm = ifelse(person_num == 1, arthritis_dm[2], arthritis_dm[1])
  ) %>%
  ungroup()

#Check the frequency of sarthritis
table(hcharls_all_sp8$sarthritis_dm, exclude=NULL)

#Compare the proportion of men with arthritis who had wives affected by arthritis to those without
male <- hcharls_all_sp8 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis_dm, 
  Spousal_arthritis = male$sarthritis_dm
)
freq_table(freq) #65% versus 43%
chisq.test(male$arthritis_dm, male$sarthritis_dm) #Significant

#Compare the proportion of women with arthritis who had husbands affected by arthritis to those without
female <- hcharls_all_sp8 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis_dm, 
  Spousal_arthritis = female$sarthritis_dm
)
freq_table(freq2) #53% versus 31%
chisq.test(female$arthritis_dm, female$sarthritis_dm) #Significant
######################################################  
######################################################  
##Define doctor-diagnosed other chronic conditions
######################################################  
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 1 
hcharls_all_sp8 <- hcharls_all_sp8 %>% mutate(hibpe=case_when(
  inclusion_wave!="Wave 1"~ NA,
  inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1hibpe, r2hibpe, r3hibpe, r4hibpe)))) > 2 ~ NA,
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1hibpe, r2hibpe)))) > 0 ~ NA,
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1hibpe, r2hibpe, r4hibpe)))) > 1 ~ NA,  
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1hibpe, r2hibpe, r3hibpe)))) > 1 ~ NA,  
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1hibpe, r3hibpe)))) > 0 ~ NA,  
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1hibpe, r3hibpe, r4hibpe)))) > 1 ~ NA,  
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1hibpe, r4hibpe)))) > 0 ~ NA,  
  inclusion_wave=="Wave 1" & (r1hibpe==1|r2hibpe==1|r3hibpe==1|r4hibpe==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1diabe, r2diabe, r3diabe, r4diabe)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1diabe, r2diabe)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1diabe, r2diabe, r4diabe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1diabe, r2diabe, r3diabe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1diabe, r3diabe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1diabe, r3diabe, r4diabe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1diabe, r4diabe)))) > 0 ~ NA, 
    inclusion_wave=="Wave 1" & (r1diabe==1 |r2diabe==1|r3diabe==1|r4diabe==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1cancre, r2cancre, r3cancre, r4cancre)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1cancre, r2cancre)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1cancre, r2cancre, r4cancre)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1cancre, r2cancre, r3cancre)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1cancre, r3cancre)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1cancre, r3cancre, r4cancre)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1cancre, r4cancre)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & (r1cancre==1 |r2cancre==1|r3cancre==1|r4cancre==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1lunge, r2lunge, r3lunge, r4lunge)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1lunge, r2lunge)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1lunge, r2lunge, r4lunge)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1lunge, r2lunge, r3lunge)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1lunge, r3lunge)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1lunge, r3lunge, r4lunge)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1lunge, r4lunge)))) > 0 ~ NA, 
    inclusion_wave=="Wave 1" & (r1lunge==1 |r2lunge==1|r3lunge==1|r4lunge==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1hearte, r2hearte, r3hearte, r4hearte)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1hearte, r2hearte)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1hearte, r2hearte, r4hearte)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1hearte, r2hearte, r3hearte)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1hearte, r3hearte)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1hearte, r3hearte, r4hearte)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1hearte, r4hearte)))) > 0 ~ NA, 
    inclusion_wave=="Wave 1" & (r1hearte==1 |r2hearte==1|r3hearte==1|r4hearte==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1stroke, r2stroke, r3stroke, r4stroke)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1stroke, r2stroke)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1stroke, r2stroke, r4stroke)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1stroke, r2stroke, r3stroke)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1stroke, r3stroke)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1stroke, r3stroke, r4stroke)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1stroke, r4stroke)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & (r1stroke==1 |r2stroke==1|r3stroke==1|r4stroke==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1psyche, r2psyche, r3psyche, r4psyche)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1psyche, r2psyche)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1psyche, r2psyche, r4psyche)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1psyche, r2psyche, r3psyche)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1psyche, r3psyche)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1psyche, r3psyche, r4psyche)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1psyche, r4psyche)))) > 0 ~ NA, 
    inclusion_wave=="Wave 1" & (r1psyche==1 |r2psyche==1|r3psyche==1|r4psyche==1) ~ 1,
    TRUE ~ 0),
  dyslipe=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1dyslipe, r2dyslipe, r3dyslipe, r4dyslipe)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1dyslipe, r2dyslipe)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1dyslipe, r2dyslipe, r4dyslipe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1dyslipe, r2dyslipe, r3dyslipe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1dyslipe, r3dyslipe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1dyslipe, r3dyslipe, r4dyslipe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1dyslipe, r4dyslipe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & (r1dyslipe==1 |r2dyslipe==1|r3dyslipe==1|r4dyslipe==1) ~ 1,
    TRUE ~ 0),
  livere=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1livere, r2livere, r3livere, r4livere)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1livere, r2livere)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1livere, r2livere, r4livere)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1livere, r2livere, r3livere)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1livere, r3livere)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1livere, r3livere, r4livere)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1livere, r4livere)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & (r1livere==1 |r2livere==1|r3livere==1|r4livere==1) ~ 1,
    TRUE ~ 0),
  kidneye=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1kidneye, r2kidneye, r3kidneye, r4kidneye)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1kidneye, r2kidneye)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1kidneye, r2kidneye, r4kidneye)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1kidneye, r2kidneye, r3kidneye)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1kidneye, r3kidneye)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1kidneye, r3kidneye, r4kidneye)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1kidneye, r4kidneye)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & (r1kidneye==1 |r2kidneye==1|r3kidneye==1|r4kidneye==1) ~ 1,
    TRUE ~ 0),
  digeste=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1digeste, r2digeste, r3digeste, r4digeste)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1digeste, r2digeste)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1digeste, r2digeste, r4digeste)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1digeste, r2digeste, r3digeste)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1digeste, r3digeste)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1digeste, r3digeste, r4digeste)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1digeste, r4digeste)))) > 0 ~ NA, 
    inclusion_wave=="Wave 1" & (r1digeste==1 |r2digeste==1|r3digeste==1|r4digeste==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave!="Wave 1"~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1asthmae, r2asthmae, r3asthmae, r4asthmae)))) > 2 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1asthmae, r2asthmae)))) > 0 ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1asthmae, r2asthmae, r4asthmae)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1asthmae, r2asthmae, r3asthmae)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1asthmae, r3asthmae)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1asthmae, r3asthmae, r4asthmae)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1asthmae, r4asthmae)))) > 0 ~ NA, 
    inclusion_wave=="Wave 1" & (r1asthmae==1 |r2asthmae==1|r3asthmae==1|r4asthmae==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hcharls_all_sp8$hibpe, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 6986 individuals with no high blood pressure/hypertension across four waves and 4904 individuals with high blood pressure/hypertension, 56 individuals with NA
table(hcharls_all_sp8$diabe, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 10166 individuals with no diabetes across four waves and 1678 individuals with diabetes, 102 individuals with NA
table(hcharls_all_sp8$cancre, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 11534 individuals with no cancer across four waves and 365 individuals with cancer, 47 individuals with NA
table(hcharls_all_sp8$lunge, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 9649 individuals with no lung diseases across four waves and 2255 individuals with lung diseases, 42 individuals with NA
table(hcharls_all_sp8$hearte, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 9342 individuals with no heart diseases across four waves and 2550 individuals with heart diseases, 47 individuals with NA
table(hcharls_all_sp8$stroke, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 11058 individuals with no stroke across four waves and 861 individuals with stroke, 27 individuals with NA
table(hcharls_all_sp8$psyche, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 11528 individuals with no psychiatric conditions across four waves and 376 individuals with psychiatric conditions, 42 individuals with NA
table(hcharls_all_sp8$dyslipe, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 8830 individuals with no dyslipidemia across four waves and 2903 individuals with dyslipidemia, 213 individuals with NA
table(hcharls_all_sp8$livere, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 10809 individuals with no liver diseases across four waves and 1061 individuals with liver diseases, 76 individuals with NA
table(hcharls_all_sp8$kidneye, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 10391 individuals with no kidney diseases across four waves and 1497 individuals with kidney diseases, 58 individuals with NA
table(hcharls_all_sp8$digeste, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 7590 individuals with no digestive diseases across four waves and 4326 individuals with digestive diseases, 30 individuals with NA
table(hcharls_all_sp8$asthmae, hcharls_all_sp8$inclusion_wave, exclude=NULL) #For those included from wave 1, 11059 individuals with no asthma across four waves and 847 individuals with asthma, 40 individuals with NA

######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 2
hcharls_all_sp9 <- hcharls_all_sp8 %>% mutate(hibpe=case_when(
  inclusion_wave=="Wave 1"  ~ hibpe,
  inclusion_wave=="Wave 3"~ NA,
  inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2hibpe, r3hibpe)))) > 0 ~ NA,  
  inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2hibpe, r4hibpe)))) > 0 ~ NA,  
  inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2hibpe, r3hibpe, r4hibpe)))) > 1 ~ NA, 
  inclusion_wave=="Wave 2" & (r2hibpe==1|r3hibpe==1|r4hibpe==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave=="Wave 1"  ~ diabe,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2diabe, r3diabe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2diabe, r4diabe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2diabe, r3diabe, r4diabe)))) > 1 ~ NA, 
    inclusion_wave=="Wave 2" & (r2diabe==1|r3diabe==1|r4diabe==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave=="Wave 1"  ~ cancre,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2cancre, r3cancre)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2cancre, r4cancre)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2cancre, r3cancre, r4cancre)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2cancre==1|r3cancre==1|r4cancre==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave=="Wave 1" ~ lunge,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2lunge, r3lunge)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2lunge, r4lunge)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2lunge, r3lunge, r4lunge)))) > 1 ~ NA,  
    inclusion_wave=="Wave 2" & (r2lunge==1|r3lunge==1|r4lunge==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave=="Wave 1"  ~ hearte,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2hearte, r3hearte)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2hearte, r4hearte)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2hearte, r3hearte, r4hearte)))) > 1 ~ NA, 
    inclusion_wave=="Wave 2" & (r2hearte==1|r3hearte==1|r4hearte==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave=="Wave 1"  ~ stroke,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2stroke, r3stroke)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2stroke, r4stroke)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2stroke, r3stroke, r4stroke)))) > 1 ~ NA,  
    inclusion_wave=="Wave 2" & (r2stroke==1|r3stroke==1|r4stroke==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave=="Wave 1"  ~ psyche,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2psyche, r3psyche)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2psyche, r4psyche)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2psyche, r3psyche, r4psyche)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2psyche==1|r3psyche==1|r4psyche==1) ~ 1,
    TRUE ~ 0),
  dyslipe=case_when(
    inclusion_wave=="Wave 1" ~ dyslipe,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2dyslipe, r3dyslipe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2dyslipe, r4dyslipe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2dyslipe, r3dyslipe, r4dyslipe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 2" & (r2dyslipe==1|r3dyslipe==1|r4dyslipe==1) ~ 1,
    TRUE ~ 0),
  livere=case_when(
    inclusion_wave=="Wave 1"  ~ livere,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2livere, r3livere)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2livere, r4livere)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2livere, r3livere, r4livere)))) > 1 ~ NA,  
    inclusion_wave=="Wave 2" & (r2livere==1|r3livere==1|r4livere==1) ~ 1,
    TRUE ~ 0),
  kidneye=case_when(
    inclusion_wave=="Wave 1"  ~ kidneye,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2kidneye, r3kidneye)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2kidneye, r4kidneye)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2kidneye, r3kidneye, r4kidneye)))) > 1 ~ NA, 
    inclusion_wave=="Wave 2" & (r2kidneye==1|r3kidneye==1|r4kidneye==1) ~ 1,
    TRUE ~ 0),
  digeste=case_when(
    inclusion_wave=="Wave 1"  ~ digeste,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2digeste, r3digeste)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2digeste, r4digeste)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2digeste, r3digeste, r4digeste)))) > 1 ~ NA, 
    inclusion_wave=="Wave 2" & (r2digeste==1|r3digeste==1|r4digeste==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave=="Wave 1" ~ asthmae,
    inclusion_wave=="Wave 3"~ NA,
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2asthmae, r3asthmae)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2asthmae, r4asthmae)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2asthmae, r3asthmae, r4asthmae)))) > 1 ~ NA,  
    inclusion_wave=="Wave 2" & (r2asthmae==1|r3asthmae==1|r4asthmae==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hcharls_all_sp9$hibpe, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1213 individuals with no high blood pressure/hypertension across four waves and 594 individuals with high blood pressure/hypertension, 21 individuals with NA
table(hcharls_all_sp9$diabe, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1609 individuals with no diabetes across four waves and 197 individuals with diabetes, 22 individuals with NA
table(hcharls_all_sp9$cancre, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1740 individuals with no cancer across four waves and 64 individuals with cancer, 24 individuals with NA
table(hcharls_all_sp9$lunge, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1534 individuals with no lung diseases across four waves and 272 individuals with lung diseases, 22 individuals with NA
table(hcharls_all_sp9$hearte, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1498 individuals with no heart diseases across four waves and 310 individuals with heart diseases, 20 individuals with NA
table(hcharls_all_sp9$stroke, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1738 individuals with no stroke across four waves and 74 individuals with stroke, 16 individuals with NA
table(hcharls_all_sp9$psyche, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1774 individuals with no psychiatric conditions across four waves and 35 individuals with psychiatric conditions, 19 individuals with NA
table(hcharls_all_sp9$dyslipe, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1373 individuals with no dyslipidemia across four waves and 399 individuals with dyslipidemia, 56 individuals with NA
table(hcharls_all_sp9$livere, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1671 individuals with no liver diseases across four waves and 138 individuals with liver diseases, 19 individuals with NA
table(hcharls_all_sp9$kidneye, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1622 individuals with no kidney diseases across four waves and 187 individuals with kidney diseases, 19 individuals with NA
table(hcharls_all_sp9$digeste, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1213 individuals with no digestive diseases across four waves and 600 individuals with digestive diseases, 15 individuals with NA
table(hcharls_all_sp9$asthmae, hcharls_all_sp9$inclusion_wave, exclude=NULL) #For those included from wave 2, 1717 individuals with no asthma across four waves and 95 individuals with asthma, 16 individuals with NA
######################################################
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed other chronic conditions among spousal pairs included in wave 3
hcharls_all_sp10 <- hcharls_all_sp9 %>% mutate(hibpe=case_when(
  inclusion_wave=="Wave 1"  ~ hibpe,
  inclusion_wave=="Wave 2"  ~ hibpe,
  inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3hibpe, r4hibpe)))) > 0 ~ NA, 
  inclusion_wave=="Wave 3" & (r3hibpe==1|r4hibpe==1) ~ 1,
  TRUE ~ 0),
  diabe=case_when(
    inclusion_wave=="Wave 1"  ~ diabe,
    inclusion_wave=="Wave 2"  ~ diabe,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3diabe, r4diabe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 3" & (r3diabe==1|r4diabe==1) ~ 1,
    TRUE ~ 0),
  cancre=case_when(
    inclusion_wave=="Wave 1"  ~ cancre,
    inclusion_wave=="Wave 2"  ~ cancre,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3cancre, r4cancre)))) > 0 ~ NA,
    inclusion_wave=="Wave 3" & (r3cancre==1|r4cancre==1) ~ 1,
    TRUE ~ 0),
  lunge=case_when(
    inclusion_wave=="Wave 1"  ~ lunge,
    inclusion_wave=="Wave 2"  ~ lunge,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3lunge, r4lunge)))) > 0 ~ NA,  
    inclusion_wave=="Wave 3" & (r3lunge==1|r4lunge==1) ~ 1,
    TRUE ~ 0),
  hearte=case_when(
    inclusion_wave=="Wave 1"  ~ hearte,
    inclusion_wave=="Wave 2"  ~ hearte,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3hearte, r4hearte)))) > 0 ~ NA,  
    inclusion_wave=="Wave 3" & (r3hearte==1|r4hearte==1) ~ 1,
    TRUE ~ 0),
  stroke=case_when(
    inclusion_wave=="Wave 1"  ~ stroke,
    inclusion_wave=="Wave 2"  ~ stroke,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3stroke, r4stroke)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3stroke==1|r4stroke==1) ~ 1,
    TRUE ~ 0),
  psyche=case_when(
    inclusion_wave=="Wave 1"  ~ psyche,
    inclusion_wave=="Wave 2"  ~ psyche,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3psyche, r4psyche)))) > 0 ~ NA,  
    inclusion_wave=="Wave 3" & (r3psyche==1|r4psyche==1) ~ 1,
    TRUE ~ 0),
  dyslipe=case_when(
    inclusion_wave=="Wave 1"  ~ dyslipe,
    inclusion_wave=="Wave 2"  ~ dyslipe,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3dyslipe, r4dyslipe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 3" & (r3dyslipe==1|r4dyslipe==1) ~ 1,
    TRUE ~ 0),
  livere=case_when(
    inclusion_wave=="Wave 1"  ~ livere,
    inclusion_wave=="Wave 2"  ~ livere,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3livere, r4livere)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3livere==1|r4livere==1) ~ 1,
    TRUE ~ 0),
  kidneye=case_when(
    inclusion_wave=="Wave 1"  ~ kidneye,
    inclusion_wave=="Wave 2"  ~ kidneye,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3kidneye, r4kidneye)))) > 0 ~ NA,
    inclusion_wave=="Wave 3" & (r3kidneye==1|r4kidneye==1) ~ 1,
    TRUE ~ 0),
  digeste=case_when(
    inclusion_wave=="Wave 1"  ~ digeste,
    inclusion_wave=="Wave 2"  ~ digeste,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3digeste, r4digeste)))) > 0 ~ NA,  
    inclusion_wave=="Wave 3" & (r3digeste==1|r4digeste==1) ~ 1,
    TRUE ~ 0),
  asthmae=case_when(
    inclusion_wave=="Wave 1"  ~ asthmae,
    inclusion_wave=="Wave 2"  ~ asthmae,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3asthmae, r4asthmae)))) > 0 ~ NA,  
    inclusion_wave=="Wave 3" & (r3asthmae==1|r4asthmae==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of arthritis
table(hcharls_all_sp10$hibpe, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 149 individuals with no high blood pressure/hypertension across four waves and 86 individuals with high blood pressure/hypertension, 1 individuals with NA
table(hcharls_all_sp10$diabe, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 200 individuals with no diabetes across four waves and 35 individuals with diabetes, 1 individuals with NA
table(hcharls_all_sp10$cancre, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 226 individuals with no cancer across four waves and 7 individuals with cancer, 3 individuals with NA
table(hcharls_all_sp10$lunge, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 208 individuals with no lung diseases across four waves and 25 individuals with lung diseases, 3 individuals with NA
table(hcharls_all_sp10$hearte, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 184 individuals with no heart diseases across four waves and 50 individuals with heart diseases, 2 individuals with NA
table(hcharls_all_sp10$stroke, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 223 individuals with no stroke across four waves and 13 individuals with stroke, 0 individuals with NA
table(hcharls_all_sp10$psyche, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 227 individuals with no psychiatric conditions across four waves and 9 individuals with psychiatric conditions, 0 individuals with NA
table(hcharls_all_sp10$dyslipe, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 170 individuals with no dyslipidemia across four waves and 61 individuals with dyslipidemia, 5 individuals with NA
table(hcharls_all_sp10$livere, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 216 individuals with no liver diseases across four waves and 18 individuals with liver diseases, 2 individuals with NA
table(hcharls_all_sp10$kidneye, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 208 individuals with no kidney diseases across four waves and 27 individuals with kidney diseases, 1 individuals with NA
table(hcharls_all_sp10$digeste, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 174 individuals with no digestive diseases across four waves and 60 individuals with digestive diseases, 2 individuals with NA
table(hcharls_all_sp10$asthmae, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 3, 224 individuals with no asthma across four waves and 12 individuals with asthma, 0 individuals with NA
######################################################
##Define doctor-diagnosed other chronic conditions among spousal pairs
###################################################### 
hcharls_all_sp10 <- hcharls_all_sp10 %>%
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
    sdyslipe = ifelse(person_num == 1, dyslipe[2], dyslipe[1]),
    slivere = ifelse(person_num == 1, livere[2], livere[1]),
    skidneye = ifelse(person_num == 1, kidneye[2], kidneye[1]),
    sdigeste = ifelse(person_num == 1, digeste[2], digeste[1]),
    sasthmae = ifelse(person_num == 1, asthmae[2], asthmae[1])
    ) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(hcharls_all_sp10$shibpe, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sdiabe, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$scancre, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$slunge, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$shearte, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sstroke, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sdyslipe, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$slivere, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$skidneye, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sdigeste, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sasthmae, hcharls_all_sp10$inclusion_wave, exclude=NULL)

##Compare the proportion of men with arthritis who had wives affected by other chronic conditions to those without
#High blood pressure/hypertension
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hibpe = male$shibpe
)
freq_table(freq) #42% versus 39%
chisq.test(male$arthritis, male$shibpe) #Significant

#High blood sugar/diabetes
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_diabe = male$sdiabe
)
freq_table(freq) #16% versus 15%
chisq.test(male$arthritis, male$sdiabe) #Non-significant

#Cancer
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_cancre = male$scancre
)
freq_table(freq) #5% versus 3%
chisq.test(male$arthritis, male$scancre) #Significant

#Lung diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_lunge = male$slunge
)
freq_table(freq) #18% versus 13%
chisq.test(male$arthritis, male$slunge) #Significant

#Heart diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hearte = male$shearte
)
freq_table(freq) #27% versus 22%
chisq.test(male$arthritis, male$shearte) #Significant

#Stroke
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_stroke= male$sstroke
)
freq_table(freq) #7% versus 6%
chisq.test(male$arthritis, male$sstroke) #Non-significant

#Psychiatric conditions
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_psyche= male$spsyche
)
freq_table(freq) #5% versus 3%
chisq.test(male$arthritis, male$spsyche) #Significant

#Dyslipidemia
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_dyslipe= male$sdyslipe
)
freq_table(freq) #27% versus 26%
chisq.test(male$arthritis, male$sdyslipe) #Non-significant

#Liver diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_livere= male$slivere
)
freq_table(freq) #10% versus 6%
chisq.test(male$arthritis, male$slivere) #Significant

#Kidney diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_kidneye= male$skidneye
)
freq_table(freq) #14% versus 9%
chisq.test(male$arthritis, male$skidneye) #Significant

#Digestive diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_digeste= male$sdigeste
)
freq_table(freq) #46% versus 34%
chisq.test(male$arthritis, male$sdigeste) #Significant

#Asthma
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_asthmae= male$sasthmae
)
freq_table(freq) #7% versus 5%
chisq.test(male$arthritis, male$sasthmae) #Significant

##Compare the proportion of women with arthritis who had husbands affected by other chronic conditions to those without
#High blood pressure/hypertension
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hibpe = female$shibpe
)
freq_table(freq2) #41% versus 39%
chisq.test(female$arthritis, female$shibpe) #Non-Significant

#High blood sugar/diabetes
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_diabe = female$sdiabe
)
freq_table(freq2) #13% versus 12%
chisq.test(female$arthritis, female$sdiabe) #Non-significant

#Cancer
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_cancre = female$scancre
)
freq_table(freq2) #2% versus 2%
chisq.test(female$arthritis, female$scancre) #Non-significant

#Lung diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_lunge = female$slunge
)
freq_table(freq2) #26% versus 17%
chisq.test(female$arthritis, female$slunge) #Significant

#Heart diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hearte = female$shearte
)
freq_table(freq2) #19% versus 16%
chisq.test(female$arthritis, female$shearte) #Significant

#Stroke
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_stroke= female$sstroke
)
freq_table(freq2) #7% versus 7%
chisq.test(female$arthritis, female$sstroke) #Non-significant

#Psychiatric conditions
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_psyche= female$spsyche
)
freq_table(freq2) #3% versus 2%
chisq.test(female$arthritis, female$spsyche) #Significant

#Dyslipidemia
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_dyslipe= female$sdyslipe
)
freq_table(freq2) #22% versus 24%
chisq.test(female$arthritis, female$sdyslipe) #Non-significant

#Liver diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_livere= female$slivere
)
freq_table(freq2) #11% versus 8%
chisq.test(female$arthritis, female$slivere) #Significant

#Kidney diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_kidneye= female$skidneye
)
freq_table(freq2) #16% versus 11%
chisq.test(female$arthritis, female$skidneye) #Significant

#Digestive diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_digeste= female$sdigeste
)
freq_table(freq2) #36% versus 28%
chisq.test(female$arthritis, female$sdigeste) #Significant

#Asthma
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_asthmae= female$sasthmae
)
freq_table(freq2) #10% versus 6%
chisq.test(female$arthritis, female$sasthmae) #Significant
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions 
#Requiring individuals not exceeding the upper limit missing values of doctor-diagnosed variables only as operator "OR" not "AND" was used
#The number of NA becames smaller when including medication variables to define other chronic conditions, which is unexpected and should use the other chronic condition variables defined by doctor-diagnosed only to refine rows with NA
###################################################### 
#Inclusion in Wave 1
###################################################### 
#Define doctor-diagnosed/medication-indicated other chronic diseases among spousal pairs included from wave 1 
hcharls_all_sp10 <- hcharls_all_sp10 %>% mutate(hibpe_dm=case_when(
  inclusion_wave!="Wave 1"~ NA, 
  inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1hibpe, r2hibpe, r3hibpe, r4hibpe)))) > 2 ~ NA,
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1hibpe, r2hibpe)))) > 0  ~ NA,
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1hibpe, r2hibpe, r4hibpe)))) > 1 ~ NA,  
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1hibpe, r2hibpe, r3hibpe)))) > 1 ~ NA,  
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1hibpe, r3hibpe)))) > 0 ~ NA,  
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1hibpe, r3hibpe, r4hibpe)))) > 1 ~ NA,  
  inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1hibpe, r4hibpe)))) > 0  ~ NA, 
  inclusion_wave=="Wave 1" & (r1hibpe==1 |r2hibpe==1|r3hibpe==1|r4hibpe==1|r1rxhibp_c==1|r2rxhibp_c==1|r3rxhibp_c==1|r4rxhibp_c==1) ~ 1,
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1diabe, r2diabe, r3diabe, r4diabe)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1diabe, r2diabe)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1diabe, r2diabe, r4diabe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1diabe, r2diabe, r3diabe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1diabe, r3diabe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1diabe, r3diabe, r4diabe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1diabe, r4diabe)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & (r1diabe==1 |r2diabe==1|r3diabe==1|r4diabe==1|r1rxdiab_c==1|r2rxdiab_c==1|r3rxdiab_c==1|r4rxdiab_c==1) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1cancre, r2cancre, r3cancre, r4cancre)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1cancre, r2cancre)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1cancre, r2cancre, r4cancre)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1cancre, r2cancre, r3cancre)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1cancre, r3cancre)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1cancre, r3cancre, r4cancre)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1cancre, r4cancre)))) > 0  ~ NA, 
    inclusion_wave=="Wave 1" & (r1cancre==1 |r2cancre==1|r3cancre==1|r4cancre==1|r1cncrmeds_c==1|r2cncrmeds_c==1|r3cncrmeds_c==1|r4cncrmeds_c==1| r1cncrchem==1| r2cncrchem==1| r3cncrchem==1| r4cncrchem==1| r1cncrsurg==1| r2cncrsurg==1| r3cncrsurg==1| r4cncrsurg==1| r1cncrradn==1| r2cncrradn==1| r3cncrradn==1| r4cncrradn==1) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1lunge, r2lunge, r3lunge, r4lunge)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1lunge, r2lunge)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1lunge, r2lunge, r4lunge)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1lunge, r2lunge, r3lunge)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1lunge, r3lunge)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1lunge, r3lunge, r4lunge)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1lunge, r4lunge)))) > 0  ~ NA,  
    inclusion_wave=="Wave 1" & (r1lunge==1 |r2lunge==1|r3lunge==1|r4lunge==1|r1rxlung_c==1|r2rxlung_c==1|r3rxlung_c==1|r4rxlung_c==1) ~ 1,
    TRUE ~ 0),
  hearte_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1hearte, r2hearte, r3hearte, r4hearte)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1hearte, r2hearte)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1hearte, r2hearte, r4hearte)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1hearte, r2hearte, r3hearte)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1hearte, r3hearte)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1hearte, r3hearte, r4hearte)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1hearte, r4hearte)))) > 0  ~ NA, 
    inclusion_wave=="Wave 1" & (r1hearte==1 |r2hearte==1|r3hearte==1|r4hearte==1|r1rxheart_c==1|r2rxheart_c==1|r3rxheart_c==1|r4rxheart_c==1) ~ 1,
    TRUE ~ 0),
  stroke_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1stroke, r2stroke, r3stroke, r4stroke)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1stroke, r2stroke)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1stroke, r2stroke, r4stroke)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1stroke, r2stroke, r3stroke)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1stroke, r3stroke)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1stroke, r3stroke, r4stroke)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1stroke, r4stroke)))) > 0  ~ NA,  
    inclusion_wave=="Wave 1" & (r1stroke==1 |r2stroke==1|r3stroke==1|r4stroke==1|r1rxstrok_c==1|r2rxstrok_c==1|r3rxstrok_c==1|r4rxstrok_c==1) ~ 1,
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1psyche, r2psyche, r3psyche, r4psyche)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1psyche, r2psyche)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1psyche, r2psyche, r4psyche)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1psyche, r2psyche, r3psyche)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1psyche, r3psyche)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1psyche, r3psyche, r4psyche)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1psyche, r4psyche)))) > 0  ~ NA,  
    inclusion_wave=="Wave 1" & (r1psyche==1 |r2psyche==1|r3psyche==1|r4psyche==1|r1rxpsych==1|r2rxpsych==1|r3rxpsych==1|r4rxpsych==1) ~ 1,
    TRUE ~ 0),
  dyslipe_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1dyslipe, r2dyslipe, r3dyslipe, r4dyslipe)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1dyslipe, r2dyslipe)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1dyslipe, r2dyslipe, r4dyslipe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1dyslipe, r2dyslipe, r3dyslipe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1dyslipe, r3dyslipe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1dyslipe, r3dyslipe, r4dyslipe)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1dyslipe, r4dyslipe)))) > 0  ~ NA, 
    inclusion_wave=="Wave 1" & (r1dyslipe==1 |r2dyslipe==1|r3dyslipe==1|r4dyslipe==1|r1rxdyslip_c==1|r2rxdyslip_c==1|r3rxdyslip_c==1|r4rxdyslip_c==1) ~ 1,
    TRUE ~ 0),
  livere_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1livere, r2livere, r3livere, r4livere)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1livere, r2livere)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1livere, r2livere, r4livere)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1livere, r2livere, r3livere)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1livere, r3livere)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1livere, r3livere, r4livere)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1livere, r4livere)))) > 0  ~ NA,  
    inclusion_wave=="Wave 1" & (r1livere==1 |r2livere==1|r3livere==1|r4livere==1|r1rxliver_c==1|r2rxliver_c==1|r3rxliver_c==1|r4rxliver_c==1) ~ 1,
    TRUE ~ 0),
  kidneye_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1kidneye, r2kidneye, r3kidneye, r4kidneye)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1kidneye, r2kidneye)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1kidneye, r2kidneye, r4kidneye)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1kidneye, r2kidneye, r3kidneye)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1kidneye, r3kidneye)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1kidneye, r3kidneye, r4kidneye)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1kidneye, r4kidneye)))) > 0  ~ NA,  
    inclusion_wave=="Wave 1" & (r1kidneye==1 |r2kidneye==1|r3kidneye==1|r4kidneye==1|r1rxkidney_c==1|r2rxkidney_c==1|r3rxkidney_c==1|r4rxkidney_c==1) ~ 1,
    TRUE ~ 0),
  digeste_dm=case_when(
    inclusion_wave!="Wave 1"~ NA, 
    inclusion_wave=="Wave 1" & spousal_part_pattern=="All waves" & rowSums(is.na(across(c(r1digeste, r2digeste, r3digeste, r4digeste)))) > 2  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2 only" & rowSums(is.na(across(c(r1digeste, r2digeste)))) > 0  ~ NA,
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-2,4" & rowSums(is.na(across(c(r1digeste, r2digeste, r4digeste)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1-3 only" & rowSums(is.na(across(c(r1digeste, r2digeste, r3digeste)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3 only" & rowSums(is.na(across(c(r1digeste, r3digeste)))) > 0 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,3-4 only" & rowSums(is.na(across(c(r1digeste, r3digeste, r4digeste)))) > 1 ~ NA,  
    inclusion_wave=="Wave 1" & spousal_part_pattern=="Waves 1,4 only" & rowSums(is.na(across(c(r1digeste, r4digeste)))) > 0  ~ NA,  
    inclusion_wave=="Wave 1" & (r1digeste==1 |r2digeste==1|r3digeste==1|r4digeste==1|r1rxdigest_c==1|r2rxdigest_c==1|r3rxdigest_c==1|r4rxdigest_c==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of doctor-diagnosed/medication-indicated other chronic conditions
table(hcharls_all_sp10$hibpe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 6920 individuals with no high blood pressure/hypertension across four waves and 4970 individuals with high blood pressure/hypertension, 56 individuals with NA
table(hcharls_all_sp10$diabe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 10158 individuals with no diabetes across four waves and 1686 individuals with diabetes, 102 individuals with NA
table(hcharls_all_sp10$cancre_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 11530 individuals with no cancer across four waves and 369 individuals with cancer, 47 individuals with NA
table(hcharls_all_sp10$lunge_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 9579 individuals with no lung diseases across four waves and 2325 individuals with lung diseases, 42 individuals with NA
table(hcharls_all_sp10$hearte_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 9318 individuals with no heart diseases across four waves and 2574 individuals with heart diseases, 54 individuals with NA
table(hcharls_all_sp10$stroke_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 11052 individuals with no stroke across four waves and 867 individuals with stroke, 27 individuals with NA
table(hcharls_all_sp10$psyche_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 11511 individuals with no psychiatric conditions across four waves and 393 individuals with psychiatric conditions, 42 individuals with NA
table(hcharls_all_sp10$dyslipe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 8822 individuals with no dyslipidemia across four waves and 2911 individuals with dyslipidemia, 213 individuals with NA
table(hcharls_all_sp10$livere_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 10802 individuals with no liver diseases across four waves and 1068 individuals with liver diseases, 76 individuals with NA
table(hcharls_all_sp10$kidneye_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 10372 individuals with no kidney diseases across four waves and 1516 individuals with kidney diseases, 58 individuals with NA
table(hcharls_all_sp10$digeste_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 7571 individuals with no digestive diseases across four waves and 4345 individuals with digestive diseases, 30 individuals with NA
######################################################
#Inclusion in Wave 2
###################################################### 
#Define doctor-diagnosed/medication-indicated other chronic diseases among spousal pairs included from wave 1 
hcharls_all_sp10 <- hcharls_all_sp10 %>% mutate(hibpe_dm=case_when(
  inclusion_wave=="Wave 1"~ hibpe_dm, 
  inclusion_wave=="Wave 3"~ NA, 
  inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2hibpe, r3hibpe)))) > 0 ~ NA,  
  inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2hibpe, r4hibpe)))) > 0 ~ NA,  
  inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2hibpe, r3hibpe, r4hibpe)))) > 1 ~ NA, 
  inclusion_wave=="Wave 2" & (r2hibpe==1|r3hibpe==1|r4hibpe==1|r2rxhibp_c==1|r3rxhibp_c==1|r4rxhibp_c==1) ~ 1,
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave=="Wave 1"~ diabe_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2diabe, r3diabe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2diabe, r4diabe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2diabe, r3diabe, r4diabe)))) > 1 ~ NA, 
    inclusion_wave=="Wave 2" & (r2diabe==1|r3diabe==1|r4diabe==1|r2rxdiab_c==1|r3rxdiab_c==1|r4rxdiab_c==1) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave=="Wave 1"~ cancre_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2cancre, r3cancre)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2cancre, r4cancre)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2cancre, r3cancre, r4cancre)))) > 1 ~ NA, 
    inclusion_wave=="Wave 2" & (r2cancre==1|r3cancre==1|r4cancre==1|r2cncrmeds_c==1|r3cncrmeds_c==1|r4cncrmeds_c==1|r2cncrchem==1| r3cncrchem==1| r4cncrchem==1|r2cncrsurg==1| r3cncrsurg==1| r4cncrsurg==1|r2cncrradn==1| r3cncrradn==1| r4cncrradn==1) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave=="Wave 1"~ lunge_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2lunge, r3lunge)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2lunge, r4lunge)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2lunge, r3lunge, r4lunge)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2lunge==1|r3lunge==1|r4lunge==1|r2rxlung_c==1|r3rxlung_c==1|r4rxlung_c==1) ~ 1,
    TRUE ~ 0),
  hearte_dm=case_when(
    inclusion_wave=="Wave 1"~ hearte_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2hearte, r3hearte)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2hearte, r4hearte)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2hearte, r3hearte, r4hearte)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2hearte==1|r3hearte==1|r4hearte==1|r2rxheart_c==1|r3rxheart_c==1|r4rxheart_c==1) ~ 1,
    TRUE ~ 0),
  stroke_dm=case_when(
    inclusion_wave=="Wave 1"~ stroke_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2stroke, r3stroke)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2stroke, r4stroke)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2stroke, r3stroke, r4stroke)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2stroke==1|r3stroke==1|r4stroke==1|r2rxstrok_c==1|r3rxstrok_c==1|r4rxstrok_c==1) ~ 1,
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave=="Wave 1"~ psyche_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2psyche, r3psyche)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2psyche, r4psyche)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2psyche, r3psyche, r4psyche)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2psyche==1|r3psyche==1|r4psyche==1|r2rxpsych==1|r3rxpsych==1|r4rxpsych==1) ~ 1,
    TRUE ~ 0),
  dyslipe_dm=case_when(
    inclusion_wave=="Wave 1"~ dyslipe_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2dyslipe, r3dyslipe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2dyslipe, r4dyslipe)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2dyslipe, r3dyslipe, r4dyslipe)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2dyslipe==1|r3dyslipe==1|r4dyslipe==1|r2rxdyslip_c==1|r3rxdyslip_c==1|r4rxdyslip_c==1) ~ 1,
    TRUE ~ 0),
  livere_dm=case_when(
    inclusion_wave=="Wave 1"~ livere_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2livere, r3livere)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2livere, r4livere)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2livere, r3livere, r4livere)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2livere==1|r3livere==1|r4livere==1|r2rxliver_c==1|r3rxliver_c==1|r4rxliver_c==1) ~ 1,
    TRUE ~ 0),
  kidneye_dm=case_when(
    inclusion_wave=="Wave 1"~ kidneye_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2kidneye, r3kidneye)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2kidneye, r4kidneye)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2kidneye, r3kidneye, r4kidneye)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2kidneye==1|r3kidneye==1|r4kidneye==1|r2rxkidney_c==1|r3rxkidney_c==1|r4rxkidney_c==1) ~ 1,
    TRUE ~ 0),
  digeste_dm=case_when(
    inclusion_wave=="Wave 1"~ digeste_dm, 
    inclusion_wave=="Wave 3"~ NA, 
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 3" & rowSums(is.na(across(c(r2digeste, r3digeste)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2 and 4" & rowSums(is.na(across(c(r2digeste, r4digeste)))) > 0 ~ NA,  
    inclusion_wave=="Wave 2" & spousal_part_pattern=="Waves 2-4" & rowSums(is.na(across(c(r2digeste, r3digeste, r4digeste)))) > 1 ~ NA,
    inclusion_wave=="Wave 2" & (r2digeste==1|r3digeste==1|r4digeste==1|r2rxdigest_c==1|r3rxdigest_c==1|r4rxdigest_c==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of doctor-diagnosed/medication-indicated other chronic conditions
table(hcharls_all_sp10$hibpe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1208 individuals with no high blood pressure/hypertension across four waves and 599 individuals with high blood pressure/hypertension, 21 individuals with NA
table(hcharls_all_sp10$diabe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1609 individuals with no diabetes across four waves and 197 individuals with diabetes, 22 individuals with NA
table(hcharls_all_sp10$cancre_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1740 individuals with no cancer across four waves and 64 individuals with cancer, 24 individuals with NA
table(hcharls_all_sp10$lunge_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1525 individuals with no lung diseases across four waves and 281 individuals with lung diseases, 22 individuals with NA
table(hcharls_all_sp10$hearte_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1498 individuals with no heart diseases across four waves and 310 individuals with heart diseases, 20 individuals with NA
table(hcharls_all_sp10$stroke_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1738 individuals with no stroke across four waves and 74 individuals with stroke, 16 individuals with NA
table(hcharls_all_sp10$psyche_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1773 individuals with no psychiatric conditions across four waves and 36 individuals with psychiatric conditions, 19 individuals with NA
table(hcharls_all_sp10$dyslipe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1373 individuals with no dyslipidemia across four waves and 399 individuals with dyslipidemia, 56 individuals with NA
table(hcharls_all_sp10$livere_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1671 individuals with no liver diseases across four waves and 138 individuals with liver diseases, 19 individuals with NA
table(hcharls_all_sp10$kidneye_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1622 individuals with no kidney diseases across four waves and 187 individuals with kidney diseases, 19 individuals with NA
table(hcharls_all_sp10$digeste_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 1213 individuals with no digestive diseases across four waves and 600 individuals with digestive diseases, 15 individuals with NA
######################################################
#Inclusion in Wave 3
###################################################### 
#Define doctor-diagnosed/medication-indicated other chronic diseases among spousal pairs included from wave 1 
hcharls_all_sp10 <- hcharls_all_sp10 %>% mutate(hibpe_dm=case_when(
  inclusion_wave=="Wave 1"  ~ hibpe_dm,
  inclusion_wave=="Wave 2"  ~ hibpe_dm,
  inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3hibpe, r4hibpe)))) > 0 ~ NA, 
  inclusion_wave=="Wave 3" & (r3hibpe==1|r4hibpe==1|r3rxhibp_c==1|r4rxhibp_c==1) ~ 1,
  TRUE ~ 0),
  diabe_dm=case_when(
    inclusion_wave=="Wave 1"  ~ diabe_dm,
    inclusion_wave=="Wave 2"  ~ diabe_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3diabe, r4diabe)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3diabe==1|r4diabe==1|r3rxdiab_c==1|r4rxdiab_c==1) ~ 1,
    TRUE ~ 0),
  cancre_dm=case_when(
    inclusion_wave=="Wave 1"  ~ cancre_dm,
    inclusion_wave=="Wave 2"  ~ cancre_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3cancre, r4cancre)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3cancre==1|r4cancre==1|r3cncrmeds_c==1|r4cncrmeds_c==1|r3cncrchem==1| r4cncrchem==1|r3cncrsurg==1| r4cncrsurg==1|r3cncrradn==1| r4cncrradn==1) ~ 1,
    TRUE ~ 0),
  lunge_dm=case_when(
    inclusion_wave=="Wave 1"  ~ lunge_dm,
    inclusion_wave=="Wave 2"  ~ lunge_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3lunge, r4lunge)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3lunge==1|r4lunge==1|r3rxlung_c==1|r4rxlung_c==1) ~ 1,
    TRUE ~ 0),
  hearte_dm=case_when(
    inclusion_wave=="Wave 1"  ~ hearte_dm,
    inclusion_wave=="Wave 2"  ~ hearte_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3hearte, r4hearte)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3hearte==1|r4hearte==1|r3rxheart_c==1|r4rxheart_c==1) ~ 1,
    TRUE ~ 0),
  stroke_dm=case_when(
    inclusion_wave=="Wave 1"  ~ stroke_dm,
    inclusion_wave=="Wave 2"  ~ stroke_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3stroke, r4stroke)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3stroke==1|r4stroke==1|r3rxstrok_c==1|r4rxstrok_c==1) ~ 1,
    TRUE ~ 0),
  psyche_dm=case_when(
    inclusion_wave=="Wave 1"  ~ psyche_dm,
    inclusion_wave=="Wave 2"  ~ psyche_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3psyche, r4psyche)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3psyche==1|r4psyche==1|r3rxpsych==1|r4rxpsych==1) ~ 1,
    TRUE ~ 0),
  dyslipe_dm=case_when(
    inclusion_wave=="Wave 1"  ~ dyslipe_dm,
    inclusion_wave=="Wave 2"  ~ dyslipe_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3dyslipe, r4dyslipe)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3dyslipe==1|r4dyslipe==1|r3rxdyslip_c==1|r4rxdyslip_c==1) ~ 1,
    TRUE ~ 0),
  livere_dm=case_when(
    inclusion_wave=="Wave 1"  ~ livere_dm,
    inclusion_wave=="Wave 2"  ~ livere_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3livere, r4livere)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3livere==1|r4livere==1|r3rxliver_c==1|r4rxliver_c==1) ~ 1,
    TRUE ~ 0),
  kidneye_dm=case_when(
    inclusion_wave=="Wave 1"  ~ kidneye_dm,
    inclusion_wave=="Wave 2"  ~ kidneye_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3kidneye, r4kidneye)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3kidneye==1|r4kidneye==1|r3rxkidney_c==1|r4rxkidney_c==1) ~ 1,
    TRUE ~ 0),
  digeste_dm=case_when(
    inclusion_wave=="Wave 1"  ~ digeste_dm,
    inclusion_wave=="Wave 2"  ~ digeste_dm,
    inclusion_wave=="Wave 3" & spousal_part_pattern=="Waves 3 and 4" & rowSums(is.na(across(c(r3digeste, r4digeste)))) > 0 ~ NA, 
    inclusion_wave=="Wave 3" & (r3digeste==1|r4digeste==1|r3rxdigest_c==1|r4rxdigest_c==1) ~ 1,
    TRUE ~ 0))
#Check the frequency of doctor-diagnosed/medication-indicated other chronic conditions
table(hcharls_all_sp10$hibpe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 148 individuals with no high blood pressure/hypertension across four waves and 87 individuals with high blood pressure/hypertension, 1 individuals with NA
table(hcharls_all_sp10$diabe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 200 individuals with no diabetes across four waves and 35 individuals with diabetes, 1 individuals with NA
table(hcharls_all_sp10$cancre_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 226 individuals with no cancer across four waves and 7 individuals with cancer, 3 individuals with NA
table(hcharls_all_sp10$lunge_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 207 individuals with no lung diseases across four waves and 26 individuals with lung diseases, 3 individuals with NA
table(hcharls_all_sp10$hearte_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 184 individuals with no heart diseases across four waves and 50 individuals with heart diseases, 2 individuals with NA
table(hcharls_all_sp10$stroke_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 223 individuals with no stroke across four waves and 13 individuals with stroke, 0 individuals with NA
table(hcharls_all_sp10$psyche_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 227 individuals with no psychiatric conditions across four waves and 9 individuals with psychiatric conditions, 0 individuals with NA
table(hcharls_all_sp10$dyslipe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 170 individuals with no dyslipidemia across four waves and 61 individuals with dyslipidemia, 5 individuals with NA
table(hcharls_all_sp10$livere_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 216 individuals with no liver diseases across four waves and 18 individuals with liver diseases, 2 individuals with NA
table(hcharls_all_sp10$kidneye_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 208 individuals with no kidney diseases across four waves and 27 individuals with kidney diseases, 1 individuals with NA
table(hcharls_all_sp10$digeste_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL) #For those included from wave 1, 174 individuals with no digestive diseases across four waves and 60 individuals with digestive diseases, 2 individuals with NA
######################################################
##Define doctor-diagnosed/medication-indicated other chronic conditions for the paired spouses
###################################################### 
hcharls_all_sp10 <- hcharls_all_sp10 %>%
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
    sdyslipe_dm = ifelse(person_num == 1, dyslipe_dm[2], dyslipe_dm[1]),
    slivere_dm = ifelse(person_num == 1, livere_dm[2], livere_dm[1]),
    skidneye_dm = ifelse(person_num == 1, kidneye_dm[2], kidneye_dm[1]),
    sdigeste_dm = ifelse(person_num == 1, digeste_dm[2], digeste_dm[1])
  ) %>%
  ungroup()

#Check the frequency of other chronic conditions in spouses
table(hcharls_all_sp10$shibpe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sdiabe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$scancre_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$slunge_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$shearte_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sstroke_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sdyslipe_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$slivere_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$skidneye_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)
table(hcharls_all_sp10$sdigeste_dm, hcharls_all_sp10$inclusion_wave, exclude=NULL)

##Compare the proportion of men with arthritis who had wives affected by other chronic conditions to those without
#High blood pressure/hypertension
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hibpe = male$shibpe_dm
)
freq_table(freq) #42% versus 39%
chisq.test(male$arthritis, male$shibpe_dm) #Significant

#High blood sugar/diabetes
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_diabe = male$sdiabe_dm
)
freq_table(freq) #16% versus 15%
chisq.test(male$arthritis, male$sdiabe_dm) #Non-significant

#Cancer
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_cancre = male$scancre_dm
)
freq_table(freq) #5% versus 3%
chisq.test(male$arthritis, male$scancre_dm) #Significant

#Lung diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_lunge = male$slunge_dm
)
freq_table(freq) #19% versus 16%
chisq.test(male$arthritis, male$slunge_dm) #Significant

#Heart diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_hearte = male$shearte_dm
)
freq_table(freq) #27% versus 22%
chisq.test(male$arthritis, male$shearte_dm) #Significant

#Stroke
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_stroke= male$sstroke_dm
)
freq_table(freq) #7% versus 6%
chisq.test(male$arthritis, male$sstroke_dm) #Non-significant

#Psychiatric conditions
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_psyche= male$spsyche_dm
)
freq_table(freq) #5% versus 3%
chisq.test(male$arthritis, male$spsyche_dm) #Significant

#Dyslipidemia
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_dyslipe= male$sdyslipe_dm
)
freq_table(freq) #27% versus 26%
chisq.test(male$arthritis, male$sdyslipe_dm) #Non-significant

#Liver diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_livere= male$slivere_dm
)
freq_table(freq) #10% versus 6%
chisq.test(male$arthritis, male$slivere_dm) #Significant

#Kidney diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_kidneye= male$skidneye_dm
)
freq_table(freq) #14% versus 9%
chisq.test(male$arthritis, male$skidneye_dm) #Significant

#Digestive diseases
male <- hcharls_all_sp10 %>% filter(ragender==1)
freq <- ftable(
  Arthritis = male$arthritis, 
  Spousal_digeste= male$sdigeste_dm
)
freq_table(freq) #46% versus 34%
chisq.test(male$arthritis, male$sdigeste_dm) #Significant

##Compare the proportion of women with arthritis who had husbands affected by other chronic conditions to those without
#High blood pressure/hypertension
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hibpe = female$shibpe_dm
)
freq_table(freq2) #42% versus 40%
chisq.test(female$arthritis, female$shibpe_dm) #Non-Significant

#High blood sugar/diabetes
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_diabe = female$sdiabe_dm
)
freq_table(freq2) #13% versus 12%
chisq.test(female$arthritis, female$sdiabe_dm) #Non-significant

#Cancer
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_cancre = female$scancre_dm
)
freq_table(freq2) #2% versus 2%
chisq.test(female$arthritis, female$scancre_dm) #Non-significant

#Lung diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_lunge = female$slunge_dm
)
freq_table(freq2) #26% versus 17%
chisq.test(female$arthritis, female$slunge_dm) #Significant

#Heart diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_hearte = female$shearte_dm
)
freq_table(freq2) #19% versus 17%
chisq.test(female$arthritis, female$shearte_dm) #Significant

#Stroke
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_stroke= female$sstroke_dm
)
freq_table(freq2) #7% versus 7%
chisq.test(female$arthritis, female$sstroke_dm) #Non-significant

#Psychiatric conditions
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_psyche= female$spsyche_dm
)
freq_table(freq2) #3% versus 2%
chisq.test(female$arthritis, female$spsyche_dm) #Significant

#Dyslipidemia
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_dyslipe= female$sdyslipe_dm
)
freq_table(freq2) #22% versus 24%
chisq.test(female$arthritis, female$sdyslipe_dm) #Non-significant

#Liver diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_livere= female$slivere_dm
)
freq_table(freq2) #11% versus 8%
chisq.test(female$arthritis, female$slivere_dm) #Significant

#Kidney diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_kidneye= female$skidneye_dm
)
freq_table(freq2) #16% versus 11%
chisq.test(female$arthritis, female$skidneye_dm) #Significant

#Digestive diseases
female <- hcharls_all_sp10 %>% filter(ragender==2)
freq2 <- ftable(
  Arthritis = female$arthritis, 
  Spousal_digeste= female$sdigeste_dm
)
freq_table(freq2) #36% versus 28%
chisq.test(female$arthritis, female$sdigeste_dm) #Significant
######################################################
##Define covariates at inclusion for respondents
#Education level 1 should be lower than upper secondary
#Length of current marriage only available for waves 1 and 2
######################################################
#Load occupation data and add occupation variables
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
hcharls_occup <- read_dta("hcharls_occup.dta")

#Define respondent covariates at inclusion for the three inclusion_wave groups
hcharls_all_sp11 <- hcharls_all_sp10 %>% mutate(
  rage=case_when(
  inclusion_wave=="Wave 1" ~ r1agey,
  inclusion_wave=="Wave 2" ~ r2agey,
  TRUE~ r3agey), 
  rmcurln=case_when(
    inclusion_wave=="Wave 1" ~ r1mcurln,
    inclusion_wave=="Wave 2" ~ r2mcurln,
    TRUE~ NA),
  hrural=case_when(
    inclusion_wave=="Wave 1" ~ h1rural,
    inclusion_wave=="Wave 2" ~ h2rural,
    TRUE~ h3rural),
  rrural=case_when(
    inclusion_wave=="Wave 1" ~ r1rural2,
    inclusion_wave=="Wave 2" ~ r2rural2,
    TRUE~ r3rural2),
  rshlta=case_when(
    inclusion_wave=="Wave 1" ~ r1shlta,
    inclusion_wave=="Wave 2" ~ r2shlta,
    TRUE~ r3shlta),
  radlfive=case_when(
    inclusion_wave=="Wave 1" ~ r1adlfive,
    inclusion_wave=="Wave 2" ~ r2adlfive,
    TRUE~ r3adlfive),
  rmobilsev=case_when(
    inclusion_wave=="Wave 1" ~ r1mobilsev,
    inclusion_wave=="Wave 2" ~ r2mobilsev,
    TRUE~ r3mobilsev),
  rpact=case_when(
    inclusion_wave=="Wave 1" & r1vgact_c==1 ~ 3, #vigorous
    inclusion_wave=="Wave 1" & r1mdact_c==1 ~ 2, #moderate
    inclusion_wave=="Wave 1" & r1ltact_c==1 ~ 1, #light
    inclusion_wave=="Wave 1" & r1ltact_c==0 & !is.na(r1ltact_c) & r1mdact_c==0 & !is.na(r1mdact_c) & r1vgact_c==0 & !is.na(r1vgact_c) ~ 0,
    inclusion_wave=="Wave 1" & r1ltact_c==0 & !is.na(r1ltact_c) & is.na(r1mdact_c) & is.na(r1vgact_c) ~ 0,
    inclusion_wave=="Wave 1" & is.na(r1ltact_c) & r1mdact_c==0 & !is.na(r1mdact_c) & is.na(r1vgact_c) ~ 0,
    inclusion_wave=="Wave 1" & is.na(r1ltact_c) & is.na(r1mdact_c) & r1vgact_c==0 & !is.na(r1vgact_c) ~ 0,    
    inclusion_wave=="Wave 1" & r1ltact_c==0 & !is.na(r1ltact_c) & r1mdact_c==0 & !is.na(r1mdact_c) & is.na(r1vgact_c) ~ 0,
    inclusion_wave=="Wave 1" & r1ltact_c==0 & !is.na(r1ltact_c) & is.na(r1mdact_c) & r1vgact_c==0 & !is.na(r1vgact_c) ~ 0,
    inclusion_wave=="Wave 1" & is.na(r1ltact_c) & r1mdact_c==0 & !is.na(r1mdact_c) & r1vgact_c==0 & !is.na(r1vgact_c) ~ 0,
    inclusion_wave=="Wave 2" & r2vgact_c==1 ~ 3, #vigorous
    inclusion_wave=="Wave 2" & r2mdact_c==1 ~ 2, #moderate
    inclusion_wave=="Wave 2" & r2ltact_c==1 ~ 1, #light
    inclusion_wave=="Wave 2" & r2ltact_c==0 & !is.na(r2ltact_c) & r2mdact_c==0 & !is.na(r2mdact_c) & r2vgact_c==0 & !is.na(r2vgact_c) ~ 0,
    inclusion_wave=="Wave 2" & r2ltact_c==0 & !is.na(r2ltact_c) & is.na(r2mdact_c) & is.na(r2vgact_c) ~ 0,
    inclusion_wave=="Wave 2" & is.na(r2ltact_c) & r2mdact_c==0 & !is.na(r2mdact_c) & is.na(r2vgact_c) ~ 0,
    inclusion_wave=="Wave 2" & is.na(r2ltact_c) & is.na(r2mdact_c) & r2vgact_c==0 & !is.na(r2vgact_c) ~ 0,    
    inclusion_wave=="Wave 2" & r2ltact_c==0 & !is.na(r2ltact_c) & r2mdact_c==0 & !is.na(r2mdact_c) & is.na(r2vgact_c) ~ 0,
    inclusion_wave=="Wave 2" & r2ltact_c==0 & !is.na(r2ltact_c) & is.na(r2mdact_c) & r2vgact_c==0 & !is.na(r2vgact_c) ~ 0,
    inclusion_wave=="Wave 2" & is.na(r2ltact_c) & r2mdact_c==0 & !is.na(r2mdact_c) & r2vgact_c==0 & !is.na(r2vgact_c) ~ 0,
    inclusion_wave=="Wave 3" & r3vgact_c==1 ~ 3, #vigorous
    inclusion_wave=="Wave 3" & r3mdact_c==1 ~ 2, #moderate
    inclusion_wave=="Wave 3" & r3ltact_c==1 ~ 1, #light
    inclusion_wave=="Wave 3" & r3ltact_c==0 & !is.na(r3ltact_c) & r3mdact_c==0 & !is.na(r3mdact_c) & r3vgact_c==0 & !is.na(r3vgact_c) ~ 0,
    inclusion_wave=="Wave 3" & r3ltact_c==0 & !is.na(r3ltact_c) & is.na(r3mdact_c) & is.na(r3vgact_c) ~ 0,
    inclusion_wave=="Wave 3" & is.na(r3ltact_c) & r3mdact_c==0 & !is.na(r3mdact_c) & is.na(r3vgact_c) ~ 0,
    inclusion_wave=="Wave 3" & is.na(r3ltact_c) & is.na(r3mdact_c) & r3vgact_c==0 & !is.na(r3vgact_c) ~ 0,    
    inclusion_wave=="Wave 3" & r3ltact_c==0 & !is.na(r3ltact_c) & r3mdact_c==0 & !is.na(r3mdact_c) & is.na(r3vgact_c) ~ 0,
    inclusion_wave=="Wave 3" & r3ltact_c==0 & !is.na(r3ltact_c) & is.na(r3mdact_c) & r3vgact_c==0 & !is.na(r3vgact_c) ~ 0,
    inclusion_wave=="Wave 3" & is.na(r3ltact_c) & r3mdact_c==0 & !is.na(r3mdact_c) & r3vgact_c==0 & !is.na(r3vgact_c) ~ 0,
    TRUE ~ NA),
  rdrinkr=case_when(
    inclusion_wave=="Wave 1" ~ r1drinkr_c,
    inclusion_wave=="Wave 2" ~ r2drinkr_c,
    TRUE ~ r3drinkr_c),
  rsmokev=case_when(
    inclusion_wave=="Wave 1" ~ r1smokev,
    inclusion_wave=="Wave 2" ~ r2smokev,
    TRUE ~ r3smokev),
  ritearn=case_when(
    inclusion_wave=="Wave 1" ~ r1itearn,
    inclusion_wave=="Wave 2" ~ r2itearn,
    TRUE ~ r3itearn),
  ritsemp=case_when(
    inclusion_wave=="Wave 1" ~ r1itsemp,
    inclusion_wave=="Wave 2" ~ r2itsemp,
    TRUE ~ r3itsemp),
  hitsemp=case_when(
    inclusion_wave=="Wave 1" ~ hh1itsemp,
    inclusion_wave=="Wave 2" ~ hh2itsemp,
    TRUE ~ hh3itsemp),
  ripen=case_when(
    inclusion_wave=="Wave 1" ~ r1ipen,
    inclusion_wave=="Wave 2" ~ r2ipen,
    TRUE ~ r3ipen),
  rigxfr=case_when(
    inclusion_wave=="Wave 1" ~ r1igxfr,
    inclusion_wave=="Wave 2" ~ r2igxfr,
    TRUE ~ r3igxfr),
  riothr=case_when(
    inclusion_wave=="Wave 1" ~ r1iothr,
    inclusion_wave=="Wave 2" ~ r2iothr,
    TRUE ~ r3iothr),
  hiothhh=case_when(
    inclusion_wave=="Wave 1" ~ hh1iothhh,
    inclusion_wave=="Wave 2" ~ hh2iothhh,
    TRUE ~ hh3iothhh),
  hkcnt=case_when(
    inclusion_wave=="Wave 1" ~ h1kcnt,
    inclusion_wave=="Wave 2" ~ h2kcnt,
    TRUE ~ h3kcnt),
  rsocwk=case_when(
    inclusion_wave=="Wave 1" ~ r1socwk,
    inclusion_wave=="Wave 2" ~ r2socwk,
    TRUE ~ r3socwk),
  roccup=case_when(
    inclusion_wave=="Wave 1" ~ r1lbrf_c,
    inclusion_wave=="Wave 2" ~ r2lbrf_c,
    TRUE ~ r3lbrf_c),
  rmbmin=case_when(
    inclusion_wave=="Wave 1" ~ r1mbmi,
    inclusion_wave=="Wave 2" ~ r2mbmi,
    TRUE ~ r3mbmi),
  rmbmicat=case_when(
    inclusion_wave=="Wave 1" ~ r1mbmicata,
    inclusion_wave=="Wave 2" ~ r2mbmicata,
    TRUE ~ r3mbmicata),
  rclstress=case_when(
    inclusion_wave=="Wave 1" & (rapadrug==1|r1lifethe==1|ramischlth==1|r1chdeathe==1) ~ 1,
    inclusion_wave=="Wave 1" & (rapadrug==0 & !is.na(rapadrug) & r1lifethe==0 & !is.na(r1lifethe) & ramischlth==0 & !is.na(ramischlth) & r1chdeathe==0 & !is.na(r1chdeathe)) ~ 0,
    inclusion_wave=="Wave 2" & (rapadrug==1|r2lifethe==1|ramischlth==1|r2chdeathe==1) ~ 1,
    inclusion_wave=="Wave 2" & (rapadrug==0 & !is.na(rapadrug) & r2lifethe==0 & !is.na(r2lifethe) & ramischlth==0 & !is.na(ramischlth) & r2chdeathe==0 & !is.na(r2chdeathe)) ~ 0,
    inclusion_wave=="Wave 3" & (rapadrug==1|r3lifethe==1|ramischlth==1|r3chdeathe==1) ~ 1,
    inclusion_wave=="Wave 3" & (rapadrug==0 & !is.na(rapadrug) & r3lifethe==0 & !is.na(r3lifethe) & ramischlth==0 & !is.na(ramischlth) & r3chdeathe==0 & !is.na(r3chdeathe)) ~ 0,
    TRUE ~ NA),
  rclstress2=case_when(
    inclusion_wave=="Wave 1" & (r1lifethe==1|r1chdeathe==1) ~ 1,
    inclusion_wave=="Wave 1" & (r1lifethe==0 & !is.na(r1lifethe) & r1chdeathe==0 & !is.na(r1chdeathe)) ~ 0,
    inclusion_wave=="Wave 2" & (r2lifethe==1|r2chdeathe==1) ~ 1,
    inclusion_wave=="Wave 2" & (r2lifethe==0 & !is.na(r2lifethe) & r2chdeathe==0 & !is.na(r2chdeathe)) ~ 0,
    inclusion_wave=="Wave 3" & (r3lifethe==1|r3chdeathe==1) ~ 1,
    inclusion_wave=="Wave 3" & (r3lifethe==0 & !is.na(r3lifethe) & r3chdeathe==0 & !is.na(r3chdeathe)) ~ 0,
    TRUE ~ NA))

#Check for inconsistencies in household variables
inconsistent_households <- hcharls_all_sp11 %>%
  group_by(householdID) %>%
  summarise(
    across(c(hrural, hkcnt, hitsemp, hiothhh), 
           ~ length(unique(.)) > 1,  # Returns TRUE if more than one unique value
           .names = "{.col}_inconsistent"
    )
  ) %>%
  filter(if_any(ends_with("_inconsistent"), ~ .))  # Keep only households with any inconsistency
#All household variables are consistent within spousal pairs

#Add occupation variables
hcharls_all_sp11 <- hcharls_all_sp11 %>% left_join(.,hcharls_occup, by="ID")

hcharls_all_sp11 <- hcharls_all_sp11 %>% mutate(roccup2=case_when(
  inclusion_wave=="Wave 1" ~ r1jgovtemp,
  inclusion_wave=="Wave 2" ~ r2jgovtemp,
  TRUE ~ r3jgovtemp))

#Tabulate roccup and roccup2
table(hcharls_all_sp11$roccup, hcharls_all_sp11$roccup2, exclude=NULL)

#Define occupation variable incorporating information from roccup2 (employed by government)
hcharls_all_sp11 <- hcharls_all_sp11 %>% mutate(roccup3=case_when(
  roccup==3 & roccup2==1 ~ 9,
  TRUE ~ roccup
))

#Add height and weight variables
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hcharls <- read_dta("H_CHARLS_D_Data.dta") %>% select(ID,r1mheight, r2mheight, r3mheight,r3mheight, r1mweight, r2mweight, r3mweight)
hcharls_all_sp11 <- hcharls_all_sp11 %>% left_join(.,hcharls,by="ID")

#Define height and weight
hcharls_all_sp11 <- hcharls_all_sp11 %>% mutate(
  rmheight=case_when(
    inclusion_wave=="Wave 1" ~ r1mheight,
    inclusion_wave=="Wave 2" ~ r2mheight,
    TRUE~ r3mheight),
  rmweight=case_when(
    inclusion_wave=="Wave 1" ~ r1mweight,
    inclusion_wave=="Wave 2" ~ r2mweight,
    TRUE~ r3mweight))

#Create variable of any chronic conditions
hcharls_all_sp11 <- hcharls_all_sp11 %>% mutate(
  rchrondis = case_when(
    # Check if any variable = 1 (no NA in this row for the 1s check)
    inclusion_wave=="Wave 1" & if_any(c(r1hibpe, r1diabe, r1cancre, r1lunge, r1hearte, r1stroke, r1psyche, r1dyslipe, r1livere, r1kidneye, r1digeste, r1asthmae), ~ .x == 1) ~ 1,
    # Check if all variables = 0 (no NA in this row for the 0s check)
    inclusion_wave=="Wave 1" & if_all(c(r1hibpe, r1diabe, r1cancre, r1lunge, r1hearte, r1stroke, r1psyche, r1dyslipe, r1livere, r1kidneye, r1digeste, r1asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 2" & if_any(c(r2hibpe, r2diabe, r2cancre, r2lunge, r2hearte, r2stroke, r2psyche, r2dyslipe, r2livere, r2kidneye, r2digeste, r2asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 2" & if_all(c(r2hibpe, r2diabe, r2cancre, r2lunge, r2hearte, r2stroke, r2psyche, r2dyslipe, r2livere, r2kidneye, r2digeste, r2asthmae),  ~ .x == 0) ~ 0,
    inclusion_wave=="Wave 3" & if_any(c(r3hibpe, r3diabe, r3cancre, r3lunge, r3hearte, r3stroke, r3psyche, r3dyslipe, r3livere, r3kidneye, r3digeste, r3asthmae), ~ .x == 1) ~ 1,
    inclusion_wave=="Wave 3" & if_all(c(r3hibpe, r3diabe, r3cancre, r3lunge, r3hearte, r3stroke, r3psyche, r3dyslipe, r3livere, r3kidneye, r3digeste, r3asthmae),  ~ .x == 0) ~ 0,
    # Otherwise NA
    TRUE ~ NA_real_))

#Check frequency of any_chron_dis
table(hcharls_all_sp11$rchrondis, exclude=NULL) #223 NAs
qc <- hcharls_all_sp11 %>% filter(is.na(rchrondis)) %>% select(rchrondis, r1hibpe, r1diabe, r1cancre, r1lunge, r1hearte, r1stroke, r1psyche, r1dyslipe, r1livere, r1kidneye, r1digeste, r1asthmae,r2hibpe, r2diabe, r2cancre, r2lunge, r2hearte, r2stroke, r2psyche, r2dyslipe, r2livere, r2kidneye, r2digeste, r2asthmae, r3hibpe, r3diabe, r3cancre, r3lunge, r3hearte, r3stroke, r3psyche, r3dyslipe, r3livere, r3kidneye, r3digeste, r3asthmae )

#Check frequency of roccup3
table(hcharls_all_sp11$roccup3, exclude=NULL)
table(hcharls_all_sp11$roccup, exclude=NULL)
######################################################
##Define covariates at inclusion for the paired spouses
#Education level 1 should be lower than upper secondary
#Length of current marriage only available for waves 1 and 2
######################################################
#Define spouses' covariates at inclusion for the three inclusion_wave groups
hcharls_all_sp11 <- hcharls_all_sp11 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    sbyear = ifelse(person_num == 1, rabyear[2], rabyear[1]),
    sage = ifelse(person_num == 1, rage[2], rage[1]),
    sgender = ifelse(person_num == 1, ragender[2], ragender[1]),  
    seducl = ifelse(person_num == 1, raeducl[2], raeducl[1]),
    smcurln = ifelse(person_num == 1, rmcurln[2], rmcurln[1]),
    srural = ifelse(person_num == 1,  rrural[2],  rrural[1]),
    sshlta = ifelse(person_num == 1, rshlta[2], rshlta[1]),
    sadlfive = ifelse(person_num == 1, radlfive[2], radlfive[1]),
    smobilsev = ifelse(person_num == 1, rmobilsev[2], rmobilsev[1]),
    spact = ifelse(person_num == 1, rpact[2], rpact[1]),
    sdrinkr = ifelse(person_num == 1, rdrinkr[2], rdrinkr[1]),
    ssmokev = ifelse(person_num == 1, rsmokev[2], rsmokev[1]),
    sitearn = ifelse(person_num == 1, ritearn[2], ritearn[1]),
    sipen = ifelse(person_num == 1, ripen[2], ripen[1]),
    sigxfr = ifelse(person_num == 1, rigxfr[2], rigxfr[1]),
    siothr = ifelse(person_num == 1, riothr[2], riothr[1]),
    smomeducl = ifelse(person_num == 1, ramomeducl[2], ramomeducl[1]),
    sdadeducl = ifelse(person_num == 1, radadeducl[2], radadeducl[1]),
    ssocwk = ifelse(person_num == 1, rsocwk[2], rsocwk[1]),
    soccup = ifelse(person_num == 1, roccup[2], roccup[1]),
    soccup3 = ifelse(person_num == 1, roccup3[2], roccup3[1]),
    smbmin = ifelse(person_num == 1, rmbmin[2], rmbmin[1]),
    smbmicat = ifelse(person_num == 1, rmbmicat[2], rmbmicat[1]),
    sclstress = ifelse(person_num == 1, rclstress[2], rclstress[1]),
    sclstress2 = ifelse(person_num == 1, rclstress2[2], rclstress2[1])) %>%
  ungroup()

#Define household income=ritearn+sitearn+hitsemp+ripen+sipen+rigxfr+sigxfr+riothr+siothr+hiothhh
hcharls_all_sp11 <- hcharls_all_sp11 %>% mutate(hincome=case_when(
  !is.na(ritearn) & !is.na(sitearn) & !is.na(hitsemp) & !is.na(ripen) & !is.na(sipen) & !is.na(rigxfr) & !is.na(sigxfr) & !is.na(riothr) & !is.na(siothr) & !is.na(hiothhh) ~ ritearn+sitearn+hitsemp+ripen+sipen+rigxfr+sigxfr+riothr+siothr+hiothhh,
  TRUE ~ NA))

#Define weight and height variables for paired spouses
hcharls_all_sp11 <- hcharls_all_sp11 %>%
  group_by(householdID) %>%
  filter(n() == 2) %>%  # Only keep complete pairs
  mutate(
    person_num = row_number(),
    smheight = ifelse(person_num == 1, rmheight[2], rmheight[1]),
    smweight = ifelse(person_num == 1, rmweight[2], rmweight[1]),
    schrondis = ifelse(person_num == 1, rchrondis[2], rchrondis[1])) %>%
  ungroup()

##Check potential misdefined variables
#Age at interview
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & rage != r1agey) | (inclusion_wave=="Wave 2" & rage != r2agey )| (inclusion_wave=="Wave 3" & rage != r3agey)) #None
#Length of current marriage
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & rmcurln != r1mcurln) | (inclusion_wave=="Wave 2" & rmcurln != r2mcurln)) #None
#Living area
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & hrural != h1rural) | (inclusion_wave=="Wave 2" & hrural != h2rural)| (inclusion_wave=="Wave 3" & hrural != h3rural)) #None
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & rrural != r1rural2) | (inclusion_wave=="Wave 2" & rrural != r2rural2)| (inclusion_wave=="Wave 3" & rrural != r3rural2)) #None
#Self-reported health
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & rshlta != r1shlta) | (inclusion_wave=="Wave 2" & rshlta != r2shlta)| (inclusion_wave=="Wave 3" & rshlta != r3shlta)) #None
#ADL summary
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & radlfive != r1adlfive) | (inclusion_wave=="Wave 2" & radlfive != r2adlfive) | (inclusion_wave=="Wave 3" & radlfive != r3adlfive)) #None
#Mobility summary
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & rmobilsev != r1mobilsev) | (inclusion_wave=="Wave 2" & rmobilsev != r2mobilsev) | (inclusion_wave=="Wave 3" & rmobilsev != r3mobilsev)) #None
#Physical activity 
qc <- hcharls_all_sp11 %>% select("inclusion_wave", "r1ltact_c", "r1mdact_c", "r1vgact_c", "r2ltact_c", "r2mdact_c", "r2vgact_c","r3ltact_c", "r3mdact_c", "r3vgact_c", "rpact")
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 1" & rpact==3) #All "r1vgact_c" should equal to 1
table(qc$r1vgact_c,exclude=NULL) #Passed QC
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 2" & rpact==3) #All "r1vgact_c" should equal to 1
table(qc$r2vgact_c,exclude=NULL) #Passed QC
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 3" & rpact==3) #All "r1vgact_c" should equal to 1
table(qc$r3vgact_c,exclude=NULL) #Passed QC
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 1" & is.na(rpact)) %>% select("inclusion_wave", "r1ltact_c", "r1mdact_c", "r1vgact_c", "r2ltact_c", "r2mdact_c", "r2vgact_c","r3ltact_c", "r3mdact_c", "r3vgact_c", "rpact")#All wave 1 physical activity variables should be NA. #Passed QC
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 2" & is.na(rpact)) %>% select("inclusion_wave", "r1ltact_c", "r1mdact_c", "r1vgact_c", "r2ltact_c", "r2mdact_c", "r2vgact_c","r3ltact_c", "r3mdact_c", "r3vgact_c", "rpact")#All wave 2 physical activity variables should be NA. #Passed QC
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 3" & is.na(rpact)) %>% select("inclusion_wave", "r1ltact_c", "r1mdact_c", "r1vgact_c", "r2ltact_c", "r2mdact_c", "r2vgact_c","r3ltact_c", "r3mdact_c", "r3vgact_c", "rpact")#All wave 3 physical activity variables should be NA. #Passed QC
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 1" & rpact==0) %>% select("inclusion_wave", "r1ltact_c", "r1mdact_c", "r1vgact_c", "r2ltact_c", "r2mdact_c", "r2vgact_c","r3ltact_c", "r3mdact_c", "r3vgact_c", "rpact")#All wave 1 physical activity variables should be 0. #Passed QC
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 2" & rpact==0) %>% select("inclusion_wave", "r1ltact_c", "r1mdact_c", "r1vgact_c", "r2ltact_c", "r2mdact_c", "r2vgact_c","r3ltact_c", "r3mdact_c", "r3vgact_c", "rpact")#All wave 2 physical activity variables should be 0. #Passed QC
qc <- hcharls_all_sp11 %>% filter(inclusion_wave=="Wave 3" & rpact==0) %>% select("inclusion_wave", "r1ltact_c", "r1mdact_c", "r1vgact_c", "r2ltact_c", "r2mdact_c", "r2vgact_c","r3ltact_c", "r3mdact_c", "r3vgact_c", "rpact")#All wave 3 physical activity variables should be 0. #Passed QC
#The highest frequency of drinking behavior during the last year that the respondent reports for any one of the three types of alcohol.
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & rdrinkr != r1drinkr_c) | (inclusion_wave=="Wave 2" & rdrinkr != r2drinkr_c)| (inclusion_wave=="Wave 3" & rdrinkr != r3drinkr_c)) #None
#Ever smoking
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & rsmokev != r1smokev) | (inclusion_wave=="Wave 2" & rsmokev != r2smokev)| (inclusion_wave=="Wave 3" & rsmokev != r3smokev)) #None
#Household income in the past year (before or after tax)
qc <- hcharls_all_sp11 %>% select("ID","householdID","inclusion_wave", "ritearn", "sitearn", "hitsemp", "ripen", "sipen","rigxfr", "sigxfr", "riothr", "siothr","hiothhh", "hincome")
qc <- hcharls_all_sp11 %>% filter(is.na(hincome)) %>% select("ID","householdID","inclusion_wave", "ritearn", "sitearn", "hitsemp", "ripen", "sipen","rigxfr", "sigxfr", "riothr", "siothr","hiothhh") #2084 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC
#Weekly contact with children in person/by phone/email
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & hkcnt != h1kcnt) | (inclusion_wave=="Wave 2" & hkcnt != h2kcnt)| (inclusion_wave=="Wave 3" & hkcnt != h3kcnt)) #None
#Occupation
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & roccup != r1lbrf_c) | (inclusion_wave=="Wave 2" & roccup != r2lbrf_c)| (inclusion_wave=="Wave 3" & roccup != r3lbrf_c)) #None
#Measured BMI
qc <- hcharls_all_sp11 %>% filter((inclusion_wave=="Wave 1" & rmbmin != r1mbmi) | (inclusion_wave=="Wave 2" & rmbmin != r2mbmi)| (inclusion_wave=="Wave 3" & rmbmin != r3mbmi)) #None
#Childhood/lifetime stressful events including guardians had an alcohol and/or drug issue, ever experienced serious traffic accident/injury, ever experienced death of own child, or ever missed a month or more of school because of a health condition before age 16
qc <- hcharls_all_sp11 %>% select("ID","householdID","inclusion_wave", "rapadrug", "r1lifethe", "ramischlth", "r1chdeathe", "rclstress")
qc <- hcharls_all_sp11 %>% filter(is.na(rclstress)) %>% select("ID","householdID","inclusion_wave", "rapadrug", "r1lifethe", "ramischlth", "r1chdeathe", "rclstress") #3896 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC
#Childhood/lifetime stressful events including ever experienced serious traffic accident/injury or ever experienced death of own child (exclusion of other two variables due to high proportion of missingness)
qc <- hcharls_all_sp11 %>% select("ID","householdID","inclusion_wave", "rapadrug", "r1lifethe", "ramischlth", "r1chdeathe", "rclstress2")
qc <- hcharls_all_sp11 %>% filter(is.na(rclstress2)) %>% select("ID","householdID","inclusion_wave", "rapadrug", "r1lifethe", "ramischlth", "r1chdeathe", "rclstress2") #819 missing values
qc$missing_count <- rowSums(is.na(qc))
table(qc$missing_count, exclude=NULL) #The lowest count should be 1. Passed QC

#Save dataset hcharls_all_sp11
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
save(hcharls_all_sp11, file = "hcharls_all_sp11.rda")
######################################################
#3. Multiple imputation for covariates with missing proportion  5% < x < 40% (hcharls_all_sp) 
#Based on #9., remove rabyear and r2mcurln from imputation 
###################################################### 
#Load data
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
load("hcharls_all_sp11.rda") 

#Create new disease variables for imputation
hcharls_all_sp11 <- hcharls_all_sp11 %>% mutate(
  hibpe2=case_when(
    inclusion_wave=="Wave 1" ~ r1hibpe,
    inclusion_wave=="Wave 2" ~ r2hibpe,
    TRUE ~ r3hibpe),
  diabe2=case_when(
    inclusion_wave=="Wave 1" ~ r1diabe,
    inclusion_wave=="Wave 2" ~ r2diabe,
    TRUE ~ r3diabe),
  cancre2=case_when(
    inclusion_wave=="Wave 1" ~ r1cancre,
    inclusion_wave=="Wave 2" ~ r2cancre,
    TRUE ~ r3cancre),
  hearte2=case_when(
    inclusion_wave=="Wave 1" ~ r1hearte,
    inclusion_wave=="Wave 2" ~ r2hearte,
    TRUE ~ r3hearte),
  stroke2=case_when(
    inclusion_wave=="Wave 1" ~ r1stroke,
    inclusion_wave=="Wave 2" ~ r2stroke,
    TRUE ~ r3stroke),
  lunge2=case_when(
    inclusion_wave=="Wave 1" ~ r1lunge,
    inclusion_wave=="Wave 2" ~ r2lunge,
    TRUE ~ r3lunge),
  psyche2=case_when(
    inclusion_wave=="Wave 1" ~ r1psyche,
    inclusion_wave=="Wave 2" ~ r2psyche,
    TRUE ~ r3psyche),
  dyslipe2=case_when(
    inclusion_wave=="Wave 1" ~ r1dyslipe,
    inclusion_wave=="Wave 2" ~ r2dyslipe,
    TRUE ~ r3dyslipe),
  livere2=case_when(
    inclusion_wave=="Wave 1" ~ r1livere,
    inclusion_wave=="Wave 2" ~ r2livere,
    TRUE ~ r3livere),
  digeste2=case_when(
    inclusion_wave=="Wave 1" ~ r1digeste,
    inclusion_wave=="Wave 2" ~ r2digeste,
    TRUE ~ r3digeste),
  asthmae2=case_when(
    inclusion_wave=="Wave 1" ~ r1asthmae,
    inclusion_wave=="Wave 2" ~ r2asthmae,
    TRUE ~ r3asthmae),
  kidneye2=case_when(
    inclusion_wave=="Wave 1" ~ r1kidneye,
    inclusion_wave=="Wave 2" ~ r2kidneye,
    TRUE ~ r3kidneye),
  arthre2=case_when(
    inclusion_wave=="Wave 1" ~ r1arthre,
    inclusion_wave=="Wave 2" ~ r2arthre,
    TRUE ~ r3arthre))

#Select relevant variables
hcharls_miss <- hcharls_all_sp11 %>% dplyr::select(ID, rage, rabyear, ragender, ramomeducl, radadeducl, rclstress2, hrural, rmcurln, raeducl, rpact, rdrinkr, rsmokev, roccup3, rmheight,rmweight,rmbmin, hincome, hkcnt, hibpe2, diabe2, cancre2, lunge2, hearte2, stroke2, psyche2, dyslipe2, livere2, kidneye2, digeste2, asthmae2, arthre2, rshlta, radlfive, rmobilsev, rsocwk, arthritis, arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, dyslipe, dyslipe_dm, livere, livere_dm, kidneye, kidneye_dm, digeste, digeste_dm, asthmae) %>% 
  mutate(across(c(rage, rabyear, rmcurln, rmheight, rmweight, rmbmin, rdrinkr, hincome), as.numeric)) %>%
  mutate(across(c(ragender, hrural, raeducl, rpact, rsmokev, roccup3, hkcnt,ramomeducl, radadeducl, rclstress2, rshlta, radlfive, rmobilsev,hibpe2, diabe2, cancre2, lunge2, hearte2, stroke2, psyche2, dyslipe2, livere2, kidneye2, digeste2, asthmae2, arthre2, rsocwk, arthritis, arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, dyslipe, dyslipe_dm, livere, livere_dm, kidneye, kidneye_dm, digeste, digeste_dm, asthmae), as.factor))

#hcharls_miss <- hcharls_all_sp11 %>% dplyr::select(ID, rage, rabyear, ragender, ramomeducl, radadeducl, r1lifethe, r2lifethe, r3lifethe, r1chdeathe, r2chdeathe, r3chdeathe, hrural, r1mcurln, r2mcurln, raeducl, r1vgact_c, r2vgact_c, r3vgact_c, r1mdact_c, r2mdact_c, r3mdact_c, r1ltact_c, r2ltact_c, r3ltact_c, r1drinkr_c, r2drinkr_c, r3drinkr_c, r1smokev, r2smokev, r3smokev, r1lbrf_c, r2lbrf_c, r3lbrf_c, r1mheight, r2mheight, r3mheight,r3mheight, r1mweight, r2mweight, r3mweight, r1mbmi, r2mbmi, r3mbmi, r1itearn, r2itearn, r3itearn, hh1itsemp, hh2itsemp, hh3itsemp, r1ipen, r2ipen, r3ipen, r1igxfr, r2igxfr, r3igxfr, r1iothr, r2iothr, r3iothr, hh1iothhh, hh2iothhh, hh3iothhh, h1kcnt, h2kcnt, h3kcnt, r1hibpe, r2hibpe, r3hibpe, r1diabe, r2diabe, r3diabe, r1cancre, r2cancre, r3cancre, r1lunge, r2lunge, r3lunge, r1hearte, r2hearte, r3hearte, r1stroke, r2stroke, r3stroke, r1psyche, r2psyche, r3psyche, r1dyslipe, r2dyslipe, r3dyslipe, r1livere, r2livere, r3livere, r1kidneye, r2kidneye, r3kidneye, r1digeste, r2digeste, r3digeste, r1asthmae, r2asthmae, r3asthmae,r1arthre, r2arthre, r3arthre, r1shlta, r2shlta, r3shlta, r1adlfive, r2adlfive, r3adlfive, r1mobilsev,r2mobilsev, r3mobilsev, r1socwk, r2socwk, r3socwk, arthritis, arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, dyslipe, dyslipe_dm, livere, livere_dm, kidneye, kidneye_dm, digeste, digeste_dm, asthmae) %>% 
#  mutate(across(c(rage, rabyear, r1mcurln, r2mcurln,r1mheight, r2mheight, r3mheight,r3mheight, r1mweight, r2mweight, r3mweight, r1mbmi, r2mbmi, r3mbmi, r1drinkr_c, r2drinkr_c,r3drinkr_c, r1itearn, r2itearn, r3itearn, hh1itsemp, hh2itsemp, hh3itsemp, r1ipen, r2ipen, r3ipen, r1igxfr, r2igxfr, r3igxfr, r1iothr, r2iothr, r3iothr, hh1iothhh, hh2iothhh, hh3iothhh), as.numeric)) %>%
#  mutate(across(c(ragender, hrural, raeducl, r1vgact_c, r2vgact_c, r3vgact_c, r1mdact_c, r2mdact_c, r3mdact_c, r1ltact_c, r2ltact_c, r3ltact_c, r1smokev, r2smokev, r3smokev, r1lbrf_c, r2lbrf_c, r3lbrf_c, h1kcnt, h2kcnt, h3kcnt, ramomeducl, radadeducl, r1lifethe, r2lifethe, r3lifethe, r1chdeathe, r2chdeathe, r3chdeathe, r1shlta, r2shlta, r3shlta, r1adlfive, r2adlfive, r3adlfive, r1mobilsev,r2mobilsev, r3mobilsev, r1hibpe, r2hibpe, r3hibpe, r1diabe, r2diabe, r3diabe, r1cancre, r2cancre, r3cancre, r1lunge, r2lunge, r3lunge, r1hearte, r2hearte, r3hearte, r1stroke, r2stroke, r3stroke, r1psyche, r2psyche, r3psyche, r1dyslipe, r2dyslipe, r3dyslipe, r1livere, r2livere, r3livere, r1kidneye, r2kidneye, r3kidneye, r1digeste, r2digeste, r3digeste, r1asthmae, r2asthmae, r3asthmae,r1arthre, r2arthre, r3arthre, r1socwk, r2socwk, r3socwk, arthritis, arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, dyslipe, dyslipe_dm, livere, livere_dm, kidneye, kidneye_dm, digeste, digeste_dm, asthmae), as.factor))

#Define ordered variables
hcharls_miss$raeducl <- ordered(hcharls_miss$raeducl, levels = c("1", "2", "3"))
hcharls_miss$ramomeducl <- ordered(hcharls_miss$ramomeducl, levels = c("1", "2", "3"))
hcharls_miss$radadeducl <- ordered(hcharls_miss$radadeducl, levels = c("1", "2", "3"))
hcharls_miss$rshlta <- ordered(hcharls_miss$rshlta, levels = c("1", "2", "3","4","5"))
hcharls_miss$radlfive <- ordered(hcharls_miss$radlfive, levels = c("0", "1", "2","3","4", "5"))
hcharls_miss$rmobilsev <- ordered(hcharls_miss$rmobilsev, levels = c("0", "1", "2","3","4", "5", "6","7"))
#hcharls_miss$r2shlta <- ordered(hcharls_miss$r2shlta, levels = c("1", "2", "3","4","5"))
#hcharls_miss$r2adlfive <- ordered(hcharls_miss$r2adlfive, levels = c("0", "1", "2","3","4", "5"))
#hcharls_miss$r2mobilsev <- ordered(hcharls_miss$r2mobilsev, levels = c("0", "1", "2","3","4", "5", "6","7"))
#hcharls_miss$r3shlta <- ordered(hcharls_miss$r3shlta, levels = c("1", "2", "3","4","5"))
#hcharls_miss$r3adlfive <- ordered(hcharls_miss$r3adlfive, levels = c("0", "1", "2","3","4", "5"))
#hcharls_miss$r3mobilsev <- ordered(hcharls_miss$r3mobilsev, levels = c("0", "1", "2","3","4", "5", "6","7"))

#Check how many individuals with missing rmbmin with information on rpact
qc <- hcharls_all_sp11 %>% filter(is.na(rmbmin) & !is.na(rpact)) %>% select(ID, rmbmin, rpact) #942/3089 ~ 30% of individuals with mising BMI who had information on rpact

#Create the mids object containing the defacult setting
ini <- mice(hcharls_miss, max=0, print=FALSE)

#Extract meth data and revise accordingly
meth <- ini$meth #modelling methods
##Modelling
#Numeric variables: Predictive mean matching 
#Factor with 2 levels: Logistic regression 
#Factor with > 2 levels: Multinomial logit model
#Ordinal variables: Ordered logit model
#Change several variables simultaneously
#meth_var_to_change <- c("r1vgact_c", "r2vgact_c", "r3vgact_c", "r1mdact_c", "r2mdact_c", "r3mdact_c", "r1ltact_c", "r2ltact_c", "r3ltact_c", "arthritis", "arthritis_dm","hibpe","hibpe_dm","diabe", "diabe_dm", "cancre", "cancre_dm", "lunge", "lunge_dm", "hearte", "hearte_dm", "stroke", "stroke_dm", "psyche", "psyche_dm", "dyslipe", "dyslipe_dm", "livere", "livere_dm", "kidneye", "kidneye_dm", "digeste","digeste_dm", "asthmae")
meth_var_to_change <- c("rpact","arthritis", "arthritis_dm","hibpe","hibpe_dm","diabe", "diabe_dm", "cancre", "cancre_dm", "lunge", "lunge_dm", "hearte", "hearte_dm", "stroke", "stroke_dm", "psyche", "psyche_dm", "dyslipe", "dyslipe_dm", "livere", "livere_dm", "kidneye", "kidneye_dm", "digeste","digeste_dm", "asthmae")
meth[meth_var_to_change] <- ""
#Compute BMI using rmheight and rmweight
meth["rmbmin"] <- "~I(pmin(pmax(rmweight/((rmheight)^2),12),60))"

#Extract prediction matrix
pred <- ini$pred #prediction matrix
quickpred <- quickpred(hcharls_miss) #take absolute correlation with the target or with the response indicator of at least 0.1 
##Update prediction matrix: wave-specific prediction 
#Include rage and ragender in all prediction sets
quickpred[,c("rage","ragender")] <- 1
#Remove ID, rage, and ragender from imputation
quickpred[c("ID","rage","rabyear","ragender"),c("ID","rage","rabyear","ragender")] <- 0
#Remove newly defined disease variables from predictor sets and from prediction
quickpred[,c("rabyear","rpact", "arthritis", "arthritis_dm","hibpe","hibpe_dm","diabe", "diabe_dm", "cancre", "cancre_dm", "lunge", "lunge_dm", "hearte", "hearte_dm", "stroke", "stroke_dm", "psyche", "psyche_dm", "dyslipe", "dyslipe_dm", "livere", "livere_dm", "kidneye", "kidneye_dm", "digeste","digeste_dm", "asthmae")] <- 0
quickpred[c("rabyear","rpact","arthritis", "arthritis_dm","hibpe","hibpe_dm","diabe", "diabe_dm", "cancre", "cancre_dm", "lunge", "lunge_dm", "hearte", "hearte_dm", "stroke", "stroke_dm", "psyche", "psyche_dm", "dyslipe", "dyslipe_dm", "livere", "livere_dm", "kidneye", "kidneye_dm", "digeste","digeste_dm", "asthmae"),] <- 0

#Save quickpred
quickpred_df <- as.data.frame(quickpred)
quickpred_df$Variable <- rownames(quickpred)
quickpred_df <- quickpred_df[, c("Variable", setdiff(names(quickpred_df), "Variable"))]
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/CHARLS')
write.csv(quickpred_df, "charls_imp_quickpred.csv", row.names = FALSE)

#Multiple imputation
hcharls_imp <- mice(hcharls_miss, m=5, meth=meth, pred=quickpred, include=c("rage","ragender"), maxit=20, print=FALSE, seed=12345)

##Diagnostics
#Whether imputed values are plausible 
qc <- as.data.frame(table(hcharls_imp[["imp"]][["ramomeducl"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["radadeducl"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rclstress2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmcurln"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmcurln"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmcurln"]][["3"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmcurln"]][["4"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmcurln"]][["5"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rdrinkr"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rsmokev"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["roccup3"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmheight"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmheight"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmheight"]][["3"]]), exclude=NULL) #1 value with height <1
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmheight"]][["4"]]), exclude=NULL) #2 value with height <1
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmheight"]][["5"]]), exclude=NULL) #2 value with height <1
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmweight"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmweight"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmweight"]][["3"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmweight"]][["4"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmweight"]][["5"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmbmin"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmbmin"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmbmin"]][["3"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmbmin"]][["4"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rmbmin"]][["5"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["hincome"]][["1"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["hincome"]][["2"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["hincome"]][["3"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["hincome"]][["4"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["hincome"]][["5"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["hkcnt"]]), exclude=NULL) #Pass QC
qc <- as.data.frame(table(hcharls_imp[["imp"]][["rsocwk"]]), exclude=NULL) #Pass QC

#Check distributions of original and imputed data
xyplot(hcharls_imp, rmweight ~ rmheight | .imp, pch = 20, cex = 1.4)
xyplot(hcharls_imp, rmheight ~ rmbmin | .imp, pch = 20, cex = 1.4)
xyplot(hcharls_imp, rmbmin ~ rmcurln | .imp, pch = 20, cex = 1.4)
xyplot(hcharls_imp, rmcurln ~ hincome | .imp, pch = 20, cex = 1.4)
xyplot(hcharls_imp, rdrinkr ~ rmbmin | .imp, pch = 20)
xyplot(hcharls_imp, rdrinkr ~ rsocwk | .imp, pch = 20)

#Extracts imputed data (five imputations)
hcharls_imp_all_long <- complete(hcharls_imp,"long", include = TRUE)
###################################################### 
#4. Post imputation modifications (hcharls_all_sp11 and hcharls_imp_all_long)
###################################################### 
#Load data
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
load("hcharls_all_sp11.rda") 
hcharls_imp_all_long <- readRDS("hcharls_imp_all_long.rds")

#Extract rchondis from hcharls_all_sp11 and add to the five imputed datasets
var <- hcharls_all_sp11 %>% select(ID, householdID, rchrondis)
hcharls_imp_all_long <- hcharls_imp_all_long %>% left_join(., var, by = "ID")

#Create categorical BMI
hcharls_imp_all_long <- hcharls_imp_all_long %>% mutate(
    rmbmicat = case_when(
      rmbmin <= 18.5 ~ 1,
      rmbmin > 18.5 & rmbmin < 23 ~ 2,
      rmbmin >= 23 & rmbmin < 25 ~ 3,
      rmbmin >= 25 ~ 4))

#Define spousal variables
hcharls_imp_all_long <- hcharls_imp_all_long %>%
  group_by(.imp, householdID) %>%
  filter(n() == 2) %>%
  arrange(.imp, householdID, ID) %>% 
  mutate(
    person_num = row_number(),
    sbyear = if_else(person_num == 1, lead(rabyear), lag(rabyear)),
    sage = if_else(person_num == 1, lead(rage), lag(rage)),
    sgender = if_else(person_num == 1, lead(ragender), lag(ragender)),  
    seducl = if_else(person_num == 1, lead(raeducl), lag(raeducl)),
    smcurln = if_else(person_num == 1, lead(rmcurln), lag(rmcurln)),
    sshlta = if_else(person_num == 1, lead(rshlta), lag(rshlta)),
    sadlfive = if_else(person_num == 1, lead(radlfive), lag(radlfive)),
    smobilsev = if_else(person_num == 1, lead(rmobilsev), lag(rmobilsev)),
    spact = if_else(person_num == 1, lead(rpact), lag(rpact)),
    sdrinkr = if_else(person_num == 1, lead(rdrinkr), lag(rdrinkr)),
    ssmokev = if_else(person_num == 1, lead(rsmokev), lag(rsmokev)),
    smomeducl = if_else(person_num == 1, lead(ramomeducl), lag(ramomeducl)),
    sdadeducl = if_else(person_num == 1, lead(radadeducl), lag(radadeducl)),
    ssocwk = if_else(person_num == 1, lead(rsocwk), lag(rsocwk)),
    soccup3 = if_else(person_num == 1, lead(roccup3), lag(roccup3)),
    smbmin = if_else(person_num == 1, lead(rmbmin), lag(rmbmin)),
    smbmicat = if_else(person_num == 1, lead(rmbmicat), lag(rmbmicat)),
    sclstress2 = if_else(person_num == 1, lead(rclstress2), lag(rclstress2)),
    schrondis = if_else(person_num == 1, lead(rchrondis), lag(rchrondis)),
    sarthritis = if_else(person_num == 1, lead(arthritis), lag(arthritis)),
    shibpe = if_else(person_num == 1, lead(hibpe), lag(hibpe)),
    sdiabe = if_else(person_num == 1, lead(diabe), lag(diabe)),  
    scancre = if_else(person_num == 1, lead(cancre), lag(cancre)),
    slunge = if_else(person_num == 1, lead(lunge), lag(lunge)),
    shearte = if_else(person_num == 1, lead(hearte), lag(hearte)),
    sstroke = if_else(person_num == 1, lead(stroke), lag(stroke)),
    spsyche = if_else(person_num == 1, lead(psyche), lag(psyche)),
    sdyslipe = if_else(person_num == 1, lead(dyslipe), lag(dyslipe)),
    slivere = if_else(person_num == 1, lead(livere), lag(livere)),
    skidneye = if_else(person_num == 1, lead(kidneye), lag(kidneye)),
    sdigeste = if_else(person_num == 1, lead(digeste), lag(digeste)),
    sasthmae = if_else(person_num == 1, lead(asthmae), lag(asthmae)),
    sarthritis_dm = if_else(person_num == 1, lead(arthritis_dm), lag(arthritis_dm)),
    shibpe_dm = if_else(person_num == 1, lead(hibpe_dm), lag(hibpe_dm)),
    sdiabe_dm = if_else(person_num == 1, lead(diabe_dm), lag(diabe_dm)),  
    scancre_dm = if_else(person_num == 1, lead(cancre_dm), lag(cancre_dm)),
    slunge_dm = if_else(person_num == 1, lead(lunge_dm), lag(lunge_dm)),
    shearte_dm = if_else(person_num == 1, lead(hearte_dm), lag(hearte_dm)),
    sstroke_dm = if_else(person_num == 1, lead(stroke_dm), lag(stroke_dm)),
    spsyche_dm = if_else(person_num == 1, lead(psyche_dm), lag(psyche_dm)),
    sdyslipe_dm = if_else(person_num == 1, lead(dyslipe_dm), lag(dyslipe_dm)),
    slivere_dm = if_else(person_num == 1, lead(livere_dm), lag(livere_dm)),
    skidneye_dm = if_else(person_num == 1, lead(kidneye_dm), lag(kidneye_dm)),
    sdigeste_dm = if_else(person_num == 1, lead(digeste_dm), lag(digeste_dm)))%>%
  ungroup()

#hcharls_imp_final <- as.mids(bind_rows(hcharls_imp_all_long))

#Save dataset hcharls_imp_all_long
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
saveRDS(hcharls_imp_all_long, file = "hcharls_imp_all_long.rds")
###################################################### 
###################################################### 


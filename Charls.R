#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20250926
#UPDATED: 20251010
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE CHARLS DATA STRUCTURE, DATA PREPARATION, PERFORM STATISTICAL ANALYSES 
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#CHARLS waves 1 to 5, plus life history survey in 2014 and harmonized data of waves 1 to 4
  
#Logbook
######################################################  
#20251014 There were spousal pairs with a spouse participated from wave and the other spouse joined in later wave, when defining arthritis, use inww for wave participation
######################################################

#Things to pay attention
###################################################### 
#250926, Count the number of spousal pair in each wave (1 to 5) in CHARLS in #1. Participation of individuals across waves (using harmonized data)
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
#2. The number of eligible spousal pairs in each wave (using harmonized data)

#DATA PREPARATION
#1. List of included variables 
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
               kableExtra
) 
###################################################### 

######################################################
#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
#2. The number of eligible spousal pairs in each wave (using harmonized data)
#3. Doctor-diagnosed arthritis status at inclusion among spouses
#4. Missingness of variables in hcharls_all_sp_arthre
#5. The number of individuals died during follow-up (hcharls_all_sp) 
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
#Inclusion criteria: For each dataset, heterosexual spousal pairs in which both partners participated in at least two survey waves will be included. In cases where multiple spouses were recorded for an individual, only the first will be retained. Both spouses must have complete data on doctor-diagnosed arthritis and other chronic diseases.
#Exclusion criteria: To ensure the reliability of responses, spousal pairs in which either partner reported a doctor-diagnosed memory-related condition (e.g., dementia or Alzheimer’s disease) or was receiving treatment for such conditions will be excluded. Additionally, proxy interviews—often indicative of cognitive impairment—will also be excluded. 
######################################################
#Load data and select relevant variables
#Harmonized data_waves_1_to_4
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Data/CHARLS/Waves/Harmonized CHARLS_waves_1_to_4')
hcharls <- read_dta("H_CHARLS_D_Data.dta") %>% select(1:3,10:13, 18:40, 42:69, 82:85, 98:105, 114:189, 195:207, 224:262, 275:282,  515:530, 543:570, 603:826, 835:842, 859:866, 875:906, 915:922, 931:938, 947:970, 979:986, 995:1002, 1011:1018, 1027:1034, 1043:1050, 1059:1074, 1083:1090, 1099:1106, 1139:1146, 1151:1174, 1279:1302, 1495:1502, 1507:1516, 1551:1558, 1563:1570, 1591:1602, 1639:1642, 1761:1770, 1789:1812, 1873:1880, 1889:1904, 2021:2028, 2461:2471, 3222:3234, 3248:3252)
######################################################
##Wave 1 #12848 individuals and 6424 eligible spousal pairs participating from wave 1
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

#QC: missingness of doctor-diagnosed arthritis
qc <- hcharls_w1_2 %>% filter(is.na(r1arthre) | is.na(s1arthre)) #156 individuals

#Exclude spousal pairs with missing values on doctor-diagnosed arthritis
hcharls_w1_3 <- hcharls_w1_2 %>% filter(!is.na(r1arthre) & !is.na(s1arthre)) #14504 individuals and 7252 spousal pairs
hcharls_w1_3 %>% add_count(h1coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h1coupid) %>%       # Get unique categories
  summarise(unique_count = n())

#QC: the number of individuals with doctor-diagnosed memory-related disease or receiving treatments for memory-related disease in wave 1
qc <- hcharls_w1_3 %>% filter(r1memrye==1 | s1memrye==1 | r1rxmemry_c==1 | s1rxmemry_c==1) %>% select("ID", "householdID","r1memrye", "s1memrye", "r1rxmemry_c", "s1rxmemry_c") #516 individuals ans 258 spousal pairs

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease in wave 1 
hcharls_w1_4 <- hcharls_w1_3 %>% filter(r1memrye !=1 |is.na(r1memrye), 
                                        s1memrye !=1 |is.na(s1memrye), 
                                        r1rxmemry_c !=1 | is.na(r1rxmemry_c),
                                        s1rxmemry_c !=1 | is.na(s1rxmemry_c))  #13988 individuals and 6994 spousal pairs
hcharls_w1_4 %>% add_count(h1coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h1coupid) %>%       # Get unique categories
  summarise(unique_count = n())

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hcharls_w1_4$r1iwstat, exclude=NULL) #all 1
table(hcharls_w1_4$s1iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least two waves
#Create indicator of spousal participation across waves
hcharls_w1_4 <- hcharls_w1_4 %>% mutate(spousal_part_pattern = case_when(
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
qc <- hcharls_w1_4 %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(qc$spousal_part_pattern, exclude=NULL)
#All waves: 8804 
#Waves 1-3 only: 1474
#Waves 1,3-4: 708
#Waves 1-2,4: 482
#Waves 1-2 only: 987
#Waves 1,3 only: 206
#Waves 1,4 only: 217
#Wave 1 only: 1110 
  
#Check if misclassified
qc_spousal_part_pattern <- function(x){
  qc <- hcharls_w1_4 %>% filter(spousal_part_pattern==x)
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
qc <- hcharls_w1_4 %>% filter(spousal_part_pattern =="Wave 1 only")
qc2 <- as.data.frame(table(qc$h1coupid, exclude=NULL))
qc3 <- as.data.frame(table(qc$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_2 <- hcharls_w1_4 %>% filter(householdID %in% qc3$Var1) %>% select("ID", "householdID", "h1coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_2$spousal_part_pattern, exclude=NULL) #6 spousal pairs in which either spouse participated in wave 2 only, rwiwstat and swiwstat did not align

#Select those with householdID count=1
qc4 <- hcharls_w1_4 %>% filter(householdID %in% qc3$Var1) %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) #540 spousal pairs participated in wave 1 only and 30 spousal pairs in which rwistat and swistat did not align

#Exclude spousal pairs with spousal_part_pattern=="Wave 1 only"
hcharls_w1_5 <- hcharls_w1_4 %>% filter(!(householdID %in% qc$householdID)) #12848 individuals and 6424 spousal pairs
hcharls_w1_5 %>% add_count(h1coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h1coupid) %>%       # Get unique categories
  summarise(unique_count = n())
table(as.data.frame(table(hcharls_w1_5$h1coupid, exclude=NULL))$Freq, exclude=NULL) #All h1coupid counted twice
######################################################
##Wave 2 ##1976 individuals and 988 spousal pairs, 152 of which are spousal pairs with only one spouse participated in wave 1 and 912 refreshed spousal pairs added in wave 2
######################################################
#All spousal pairs participating in wave 2
hcharls_w2 <- hcharls %>% filter(inw2==1 & !is.na(s2id) & s2id != "0") #15575 individuals and 7783 spousal pairs plus 9 respondents with no spouses responds to this wave

#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w2$householdID))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hcharls_w2$h2cpl) # all 1

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 22 and above
table(hcharls_w2$r2agey, exclude=NULL)
table(hcharls_w2$s2agey, exclude=NULL) #Six pairs with either of spouse younger than 22 years and 91 missing values on r1agey and 100 missing values on s1agey
qc <- hcharls_w2 %>% filter(r2agey <=22 | s2agey <=22 | is.na(r2agey) | is.na(s2agey)) %>% select("ID", "householdID", "r2agey", "s2agey")
  
#Exclude spousal pairs with either of spouse younger than 22 years or with missing values on r2agey or s2agey
hcharls_w2_2 <- hcharls_w2 %>% filter(r2agey >=22 & s2agey >=22) #15382 individuals and 7691 spousal pairs
hcharls_w2_2 %>% add_count(h2coupid) %>%      # Add count column
  filter(n == 2) %>%           # Filter for count=2
  distinct(h2coupid) %>%       # Get unique categories
  summarise(unique_count = n())

#QC: missingness of doctor-diagnosed arthritis
qc <- hcharls_w2_2 %>% filter(is.na(r2arthre) | is.na(s2arthre)) %>% select("ID", "householdID", "r2arthre", "s2arthre") #156 individuals

#Exclude spousal pairs with missing values on doctor-diagnosed arthritis
hcharls_w2_3 <- hcharls_w2_2 %>% filter(!is.na(r2arthre) & !is.na(s2arthre)) #14388 individuals and 7194 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w2_3$householdID))$Freq, exclude=NULL) 

#QC: the number of individuals with doctor-diagnosed memory-related disease or receiving treatments for memory-related disease in wave 1
qc <- hcharls_w2_3 %>% filter(r2memrye==1 | s2memrye==1 | r2rxmemry_c==1 | s2rxmemry_c==1) %>% select("ID", "householdID","r2memrye", "s2memrye", "r2rxmemry_c", "s2rxmemry_c") #620 individuals ans 310 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease in wave 2
hcharls_w2_4 <- hcharls_w2_3 %>% filter(r2memrye !=1 |is.na(r2memrye), 
                                        s2memrye !=1 |is.na(s2memrye), 
                                        r2rxmemry_c !=1 | is.na(r2rxmemry_c),
                                        s2rxmemry_c !=1 | is.na(s2rxmemry_c))  #13768 individuals and 6884 spousal pairs plus 10 respondents with no spouses responded this wave
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w2_4$householdID))$Freq, exclude=NULL) 

#QC: Interview status of respondents and their spouses, should be alive and response (code=1)
table(hcharls_w2_4$r2iwstat, exclude=NULL) #all 1
table(hcharls_w2_4$s2iwstat, exclude=NULL) #all 1

#QC: Spousal pairs participated in at least two waves
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
qc <- hcharls_w2_4 %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
qc <- hcharls_w2_4 %>% filter(spousal_part_pattern=="Wave 2 only") %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(qc$spousal_part_pattern, exclude=NULL)
#All waves: 8681 
#Waves 1-3 only: 1434
#Waves 2-4 only: 1487
#Waves 1-2,4: 466
#Waves 1-2: 941
#Waves 2 and 3: 289
#Waves 2 and 4: 160
#Wave 2 only: 310 

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
qc_spousal_part_pattern("All waves")
qc_spousal_part_pattern("Waves 1-3")
qc_spousal_part_pattern("Waves 1-2,4")
qc_spousal_part_pattern("Waves 1-2")
qc_spousal_part_pattern("Waves 2-4")
qc_spousal_part_pattern("Waves 2 and 3")
qc_spousal_part_pattern("Waves 2 and 4")
qc_spousal_part_pattern("Wave 2 only")

##Some spousal pairs could have different spousal participation patterns due to death, loss of follow-up, or either spouse joined the survey later
##Such cases are fine as long as both spouses in a pairs participated in at least two waves
#Check the number of spousal pairs among different groups of spousal_part_pattern
"All waves"
qc_w2_aw <- hcharls_w2_4 %>% filter(spousal_part_pattern =="All waves") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w2_aw$h2coupid, exclude=NULL))$Freq) # 19 rows with h2coupid==1
qc2_w2_aw <- as.data.frame(table(qc_w2_aw$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_aw <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_aw$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_aw$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 1-3"
qc_w2_1to3 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 1-3") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w2_1to3$h2coupid, exclude=NULL))$Freq) # 12 rows with h2coupid==1
qc2_w2_1to3 <- as.data.frame(table(qc_w2_1to3$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_1to3 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_1to3$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_1to3$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

#"Waves 1-2,4"
qc_w2_124 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 1-2,4") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w2_124$h2coupid, exclude=NULL))$Freq) # 6 rows with h2coupid==1
qc2_w2_124 <- as.data.frame(table(qc_w2_124$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_124 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_124$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_124$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 1-2"
qc_w2_12 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 1-2") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w2_12$h2coupid, exclude=NULL))$Freq) # 17 rows with h2coupid==1
qc2_w2_12 <- as.data.frame(table(qc_w2_12$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_12 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_12$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_12$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 2-4"
qc_w2_2to4 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 2-4") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w2_2to4$h2coupid, exclude=NULL))$Freq) # 11 rows with h2coupid==1
qc2_w2_2to4 <- as.data.frame(table(qc_w2_2to4$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_2to4 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_2to4$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_2to4$spousal_part_pattern, exclude=NULL) #4 spousal pairs in which either spouse participated in wave 2 only, rwiwstat and swiwstat did not align
#Select those with spousal_part_pattern =="Wave 2 only"
qc4_w2_2to4 <- qc3_w2_2to4 %>% filter(spousal_part_pattern =="Wave 2 only") %>% select("ID", "householdID", 16)

"Waves 2 and 3"
qc_w2_23 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 2 and 3") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w2_23$h2coupid, exclude=NULL))$Freq) # 3 rows with h2coupid==1
qc2_w2_23 <- as.data.frame(table(qc_w2_23$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_23 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_23$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_23$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 2 and 4"
qc_w2_24 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Waves 2 and 4") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w2_24$h2coupid, exclude=NULL))$Freq) # 2 rows with h2coupid==1
qc2_w2_24 <- as.data.frame(table(qc_w2_24$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_24 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_24$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_24$spousal_part_pattern, exclude=NULL) #2 spousal pairs in which either spouse participated in wave 2 only, rwiwstat and swiwstat did not align
#Select those with spousal_part_pattern =="Wave 2 only"
qc4_w2_24 <- qc3_w2_24 %>% filter(spousal_part_pattern =="Wave 2 only") %>% select("ID", "householdID", 16)

"Wave 2 only"
qc_w2_2 <- hcharls_w2_4 %>% filter(spousal_part_pattern =="Wave 2 only") %>% select("ID", "householdID", "h2coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w2_2$h2coupid, exclude=NULL))$Freq) # 6 rows with h2coupid==1
qc2_w2_2 <- as.data.frame(table(qc_w2_2$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w2_2 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w2_2$Var1) %>% select("ID", "householdID", "h2coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w2_2$spousal_part_pattern, exclude=NULL) #6 spousal pairs in which either spouse participated in wave 2 only, rwiwstat and swiwstat did not align

#Exclude spousal pairs with spousal_part_pattern =="Wave 2 only"
hcharls_w2_5 <- hcharls_w2_4 %>% filter(!(householdID %in% qc_w2_2$householdID)) #13452 individuals and 6726 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w2_5$householdID))$Freq, exclude=NULL) 

#Exclude spousal pairs already include in hcharls_w1_5
hcharls_w2_6 <- hcharls_w2_5 %>% filter(!(householdID %in% hcharls_w1_5$householdID)) #1976 individuals and 988 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w2_6$householdID))$Freq, exclude=NULL) 
#Check if all included spousal pairs are from sample refreshment
table(hcharls_w2_6$hacohort_c, exclude=NULL) #152 (76 spousal pairs) individuals from original sample (wave 1) and 1824 (912 spousal pairs) individuals from sample refreshment
#Check if individuals with hacohort_c=1 are spousal pairs
qc <- hcharls_w2_6 %>% filter(hacohort_c==1) %>% select("ID", "householdID", "hacohort_c", "r1arthre","s1arthre", "r2arthre","s2arthre", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
#Check if individuals with hacohort_c=2 are spousal pairs
qc <- hcharls_w2_6 %>% filter(hacohort_c==2)
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
######################################################
##Wave 3 ##274 individuals and 137 spousal pairs, 23 of which are spousal pairi with only one spouse participated in wave 1, 7 spousal pairs with only one spouse participated in wave 2 and 214 refreshed spousal pairs added in wave 3
######################################################
#All spousal pairs participating in wave 3
hcharls_w3 <- hcharls %>% filter(inw3==1 & !is.na(s3id) & s3id != "0") #17692 individuals and 8844 spousal pairs plus 4 respondents with no spouses responds to this wave
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3$householdID))$Freq, exclude=NULL) 

#QC: frequency of HWCPL (Whether coupled household), should all be 1
table(hcharls_w3$h3cpl) # all 1

#QC: age distribution, respondents' age should be 45 or older, spouses' age should be 22 and above
table(hcharls_w3$r3agey, exclude=NULL)
table(hcharls_w3$s3agey, exclude=NULL) #Six pairs with either of spouse younger than 22 years and 74 missing values on r1agey and 78 missing values on s1agey
qc <- hcharls_w3 %>% filter(r3agey <=22 | s3agey <=22 | is.na(r3agey) | is.na(s3agey)) %>% select("ID", "householdID", "r3agey", "s3agey") #162 individuals

#Exclude spousal pairs with either of spouse younger than 22 years or with missing values on r3agey or s3agey
hcharls_w3_2 <- hcharls_w3 %>% filter(r3agey >=22 & s3agey >=22) #17532 individuals and 8766 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3_2$householdID))$Freq, exclude=NULL) 
#Check age distribution
table(hcharls_w3_2$r3agey, exclude=NULL)
table(hcharls_w3_2$s3agey, exclude=NULL)

#QC: missingness of doctor-diagnosed arthritis
qc <- hcharls_w3_2 %>% filter(is.na(r3arthre) | is.na(s3arthre)) %>% select("ID", "householdID", "r3arthre", "s3arthre") #4026 individuals

#Exclude spousal pairs with missing values on doctor-diagnosed arthritis
hcharls_w3_3 <- hcharls_w3_2 %>% filter(!is.na(r3arthre) & !is.na(s3arthre)) #13506 individuals and 6753 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3_3$householdID))$Freq, exclude=NULL) 

#QC: the number of individuals with doctor-diagnosed memory-related disease or receiving treatments for memory-related disease in wave 3
qc <- hcharls_w3_3 %>% filter(r3memrye==1 | s3memrye==1 | r3rxmemry_c==1 | s3rxmemry_c==1) %>% select("ID", "householdID","r3memrye", "s3memrye", "r3rxmemry_c", "s3rxmemry_c") #766 individuals ans 383 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) 

#Exclude spousal pairs in which either spouse with doctor-diagnosed memory-related disease in wave 3
hcharls_w3_4 <- hcharls_w3_3 %>% filter(r3memrye !=1 |is.na(r3memrye), 
                                        s3memrye !=1 |is.na(s3memrye), 
                                        r3rxmemry_c !=1 | is.na(r3rxmemry_c),
                                        s3rxmemry_c !=1 | is.na(s3rxmemry_c))  #12740 individuals and 6370 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3_4$householdID))$Freq, exclude=NULL) 

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
qc <- hcharls_w3_4 %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
qc <- hcharls_w3_4 %>% filter(spousal_part_pattern=="Wave 3 only") %>% select("ID", "householdID", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(qc$spousal_part_pattern, exclude=NULL)
#All waves: 8372
#Waves 1-3 only: 1360
#Waves 1-3,4: 693
#Waves 2-4 only: 1446
#Waves 2 and 3 only: 289
#Waves 3 and 4: 250
#Wave 3 only: 330 

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
qc_spousal_part_pattern("Waves 1-3")
qc_spousal_part_pattern("Waves 1,3-4")
qc_spousal_part_pattern("Waves 2-4")
qc_spousal_part_pattern("Waves 2 and 3")
qc_spousal_part_pattern("Waves 3 and 4")
qc_spousal_part_pattern("Wave 3 only")

##Some spousal pairs could have different spousal participation patterns due to death, loss of follow-up, or either spouse joined the survey later
##Such cases are fine as long as both spouses in a pairs participated in at least two waves
#Check the number of spousal pairs among different groups of spousal_part_pattern
"All waves"
qc_w3_aw <- hcharls_w3_4 %>% filter(spousal_part_pattern =="All waves") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w3_aw$h3coupid, exclude=NULL))$Freq) # 12 rows with h3coupid==1
qc2_w3_aw <- as.data.frame(table(qc_w3_aw$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_aw <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_aw$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_aw$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 1-3"
qc_w3_1to3 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Waves 1-3") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w3_1to3$h3coupid, exclude=NULL))$Freq) # 8 rows with h3coupid==1
qc2_w3_1to3 <- as.data.frame(table(qc_w3_1to3$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_1to3 <- hcharls_w2_4 %>% filter(householdID %in% qc2_w3_1to3$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_1to3$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

#"Waves 1,3-4"
qc_w3_134 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Waves 1,3-4") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w3_134$h3coupid, exclude=NULL))$Freq) # 3 rows with h3coupid==1
qc2_w3_134 <- as.data.frame(table(qc_w3_134$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_134 <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_134$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_134$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 2-4"
qc_w3_2to4 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Waves 2-4") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w3_2to4$h3coupid, exclude=NULL))$Freq) # 10 rows with h3coupid==1
qc2_w3_2to4 <- as.data.frame(table(qc_w3_2to4$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_2to4 <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_2to4$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_2to4$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 2 and 3"
qc_w3_23 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Waves 2 and 3") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w3_23$h3coupid, exclude=NULL))$Freq) # 3 rows with h3coupid==1
qc2_w3_23 <- as.data.frame(table(qc_w3_23$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_23 <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_23$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_23$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Waves 3 and 4"
qc_w3_34 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Waves 3 and 4") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w3_34$h3coupid, exclude=NULL))$Freq) # 8 rows with h3coupid==1
qc2_w3_34 <- as.data.frame(table(qc_w3_34$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_34 <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_34$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_34$spousal_part_pattern, exclude=NULL) #No spousal pairs in which either spouse participated in wave 2 only

"Wave 3 only"
qc_w3_3 <- hcharls_w3_4 %>% filter(spousal_part_pattern =="Wave 3 only") %>% select("ID", "householdID", "h3coupid", "r1iwstat", "r2iwstat", "r3iwstat","r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836)
table(as.data.frame(table(qc_w3_3$h3coupid, exclude=NULL))$Freq) # 2 rows with h3coupid==1
qc2_w3_3 <- as.data.frame(table(qc_w3_3$householdID, exclude=NULL)) %>% filter(Freq==1)
#Select those with householdID count=1
qc3_w3_3 <- hcharls_w3_4 %>% filter(householdID %in% qc2_w3_3$Var1) %>% select("ID", "householdID", "h3coupid", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
#Frequency of spousal_part_pattern among those spousal pairs with discordant spousal_part_pattern
table(qc3_w3_3$spousal_part_pattern, exclude=NULL) #2 spousal pairs in which either spouse participated in wave 3 only

#Exclude spousal pairs with spousal_part_pattern =="Wave 3 only"
hcharls_w3_5 <- hcharls_w3_4 %>% filter(!(householdID %in% qc_w3_3$householdID)) #12408 individuals and 6204 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3_5$householdID))$Freq, exclude=NULL) 

#Exclude spousal pairs already include in hcharls_w1_5 and hcharls_w2_
hcharls_w3_6 <- hcharls_w3_5 %>% filter(!(householdID %in% hcharls_w1_5$householdID | householdID %in% hcharls_w2_6$householdID)) #274 individuals and 137 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_w3_6$householdID))$Freq, exclude=NULL) 
#Check if all included spousal pairs are from sample refreshment
table(hcharls_w3_6$hacohort_c, exclude=NULL) #46 (23 spousal pairs) individuals from original sample (wave 1), 14 (7 spousal pairs) individuals from wave 2 sample refreshment, and 214 (107 spousal pairs) individuals from wave 3 sample refreshment
#Check if individuals with hacohort_c=1 are spousal pairs
qc <- hcharls_w3_6 %>% filter(hacohort_c==1) %>% select("ID", "householdID", "hacohort_c", "r1arthre","s1arthre", "r2arthre","s2arthre", "inw1", "r1iwstat", "inw2", "r2iwstat", "inw3", "r3iwstat", "inw4", "r4iwstat", "s1iwstat", "s2iwstat", "s3iwstat","s4iwstat", 836) 
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
#Check if individuals with hacohort_c=2 are spousal pairs
qc <- hcharls_w3_6 %>% filter(hacohort_c==2)
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
#Check if individuals with hacohort_c=4 are spousal pairs
qc <- hcharls_w3_6 %>% filter(hacohort_c==4)
table(as.data.frame(table(qc$householdID))$Freq, exclude=NULL) #All spousal pairs
######################################################
##All eligible spousal pairs, hcharls_all_sp, 15098 individuals and 7549 spousal pairs
######################################################
#Add inclusion wave indicator
hcharls_w1_5 <- hcharls_w1_5 %>% mutate(inclusion_wave="Wave 1")
hcharls_w2_6 <- hcharls_w2_6 %>% mutate(inclusion_wave="Wave 2")
hcharls_w3_6 <- hcharls_w3_6 %>% mutate(inclusion_wave="Wave 3")

##Dataset including all eligible spousal pairs, combining hcharls_w1_5, hcharls_w2_6 and hcharls_w3_6)
hcharls_all_sp <- rbind(hcharls_w1_5, hcharls_w2_6, hcharls_w3_6)
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_all_sp$householdID))$Freq, exclude=NULL) 
#Check frequency of inclusion_wave
table(hcharls_all_sp$inclusion_wave, exclude=NULL)
######################################################
######################################################
#3. Doctor-diagnosed arthritis status at inclusion among spouses
##Wave 1, exclude 1718 individuals and 859 spousal pairs
##Wave 2, exclude 206 individuals and 103 spousal pairs
##Wave 3, exclude 26 individuals and 13 spousal pairs
##All eligible spousal pairs, excluding those with concordant arthritis status: hcharls_all_sp_arthre, 13148 individuals and 6574 spousal pairs
######################################################
##Wave 1 (all spousal pairs included in wave 1)
#The number of spousal pairs with both spouses with doctor-diagnosed arthritis at inclusion
con_arthre_sp <- hcharls_w1_5 %>% filter(r1arthre==1 & s1arthre==1) #1718 individuals and 859 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_w1_5 %>% filter((r1arthre==0 & s1arthre==1) | (r1arthre==1 & s1arthre==0)) #4712 individuals and 2356 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_w1_5 %>% filter(r1arthre==0 & s1arthre==0) #6418 individuals and 3209 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL) 

##Wave 2 (all spousal pairs included in wave 2)
#The number of spousal pairs with both spouses with doctor-diagnosed arthritis at inclusion
con_arthre_sp <- hcharls_w2_6 %>% filter(r2arthre==1 & s2arthre==1) #206 individuals and 103 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_w2_6 %>% filter((r2arthre==0 & s2arthre==1) | (r2arthre==1 & s2arthre==0)) #616 individuals and 308 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_w2_6 %>% filter(r2arthre==0 & s2arthre==0) #1154 individuals and 577 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL)

##Wave 3 (all spousal pairs included in wave 3)
#The number of spousal pairs with both spouses with doctor-diagnosed arthritis at inclusion
con_arthre_sp <- hcharls_w3_6 %>% filter(r3arthre==1 & s3arthre==1) #26 individuals and 13 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(con_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs with disconcordant arthritis status at inclusion
dis_arthre_sp <- hcharls_w3_6 %>% filter((r3arthre==0 & s3arthre==1) | (r3arthre==1 & s3arthre==0)) #66 individuals and 33 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(dis_arthre_sp$householdID))$Freq, exclude=NULL) 

#The number of spousal pairs in which both spouses had no arthritis diagnosis at inclusion
no_arthre_sp <- hcharls_w3_6 %>% filter(r3arthre==0 & s3arthre==0) #182 individuals and 91 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(no_arthre_sp$householdID))$Freq, exclude=NULL)

##Dataset including all eligible spousal pairs with disconcordant arthritis status or both with no arthritis diagnosis at inclusion
hcharls_w1_6 <- hcharls_w1_5 %>% filter(r1arthre!=1 | s1arthre!=1)
hcharls_w2_7 <- hcharls_w2_6 %>% filter(r2arthre!=1 | s2arthre!=1)
hcharls_w3_7 <- hcharls_w3_6 %>% filter(r3arthre!=1 | s3arthre!=1)
hcharls_all_sp_arthre <- rbind(hcharls_w1_6,hcharls_w2_7,hcharls_w3_7) #13148 individuals and 6574 spousal pairs
#Check the number of spousal pairs
table(as.data.frame(table(hcharls_all_sp_arthre$householdID))$Freq, exclude=NULL)
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

missing_table_in_w1 <- missing_analysis(hcharls_w1_6)
#r1mbmicat/s1mbmicat missing proportion was about 20%, in line with the fact that the 78.9% of completeness of physical examination in wave 1, information based on Chinese_users__guide_20130407_
#missingness for BMI unlikely happened at random given those without physical examination in wave 1 likely had physical disability or worse health condition, younger men (work outside) and older women


##Wave 2
missing_table_in_w2 <- missing_analysis(hcharls_w2_7)

##Wave 3
missing_table_in_w3 <- missing_analysis(hcharls_w3_7)

#Save missing tables
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Output/Descriptive data/CHARLS")
write_xlsx(missing_table_in_w1, path = "Charls_missing_table_in_w1.xlsx", col_names=T, format_headers=T)
write_xlsx(missing_table_in_w2, path = "Charls_missing_table_in_w2.xlsx", col_names=T, format_headers=T)
write_xlsx(missing_table_in_w3, path = "Charls_missing_table_in_w3.xlsx", col_names=T, format_headers=T)

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
######################################################
#DATA PREPARATION
#1. List of included variables 
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
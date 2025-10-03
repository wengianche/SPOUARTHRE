#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20250926
#UPDATED: 20251002
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE CHARLS DATA STRUCTURE, DATA PREPARATION, PERFORM STATISTICAL ANALYSES 
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#CHARLS waves 1 to 5, plus life history survey in 2014 and harmonized data of waves 1 to 4
  
#Logbook
######################################################  
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


#DATA PREPARATION
#1. Adding variables from wave 5
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
               gmodels
) 
###################################################### 

######################################################
#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)
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

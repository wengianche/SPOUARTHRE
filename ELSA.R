#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20250929
#UPDATED: 20251010
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE ELSA 2002-2019 wave (1-9) DATA STRUCTURE, DATA PREPARATION, PERFORM STATISTICAL ANALYSES 
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#Harmonized ELSA waves 1 to 9
  
#Logbook
######################################################  
######################################################

#Things to pay attention
###################################################### 
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)


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
               openxlsx,
               gmodels,
               readr
) 
###################################################### 

######################################################
#DATA EXPLORATION
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
######################################################
#DATA PREPARATION
#1. List of included variables  
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
###################################################### 


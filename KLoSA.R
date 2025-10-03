#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20251002
#UPDATED: 20251002
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
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DATA EXPLORATION
#1. Participation of individuals across waves (using harmonized data)


#DATA PREPARATION
#1. 
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


#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20251002
#UPDATED: 20251009
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
######################################################
#DATA PREPARATION
#1. List of included variables  
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

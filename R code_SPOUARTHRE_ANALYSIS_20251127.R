#Project - SPOUSAL AGGREGATION OF ARTHRITIS AND COAGGREGATION OF OTHER CHRONIC COMORBIDITIES 
#BASED ON ANALYSIS PLAN_VERSION X CREATED BY PI WENG IAN CHE
#CREATED: 20251127
#UPDATED: 20251127
#ANALYST: WENG IAN CHE
#PURPOSE OF THIS SYNTAX: EXPLORE PERFORM STATISTICAL ANALYSES 
#R VERSION: version 4.4.3 (2025-02-28)
----------------------------------------------------------------------
#DATA SOURCE:
#CHARLS: hcharls_all_sp11 and hcharls_imp_all_long
#ELSA: helsa_all_sp27 (with original missing codes), helsa_all_sp28 (refine missing codes for occupation) and helsa_imp_all_long
#SHARE: hshare_all_26 and hshare_imp_all_long
#MHAS: hmhas_all_sp23 and hmhas_imp_all_long
#KLoSA: hklosa_all_sp22 
  
  
#Logbook
######################################################  
######################################################

#Things to pay attention
###################################################### 
######################################################

#Outline
######################################################
#INSTALL AND LOAD LIBRARY

#DESCRIPTIVE ANALYSES
#1. Household and individual characteristics at inclusion, as well as spousal arthritis/rheumatism and other chronic conditions by respondent gender and presence of arthritis in respondents

#STATISTICAL ANALYSES
#1. Spousal aggregation of arthritis/rheumatism: Crude models
#2. Spousal aggregation of arthritis/rheumatism: Adjusted models
#3. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Crude models
#4. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Adjusted models (Causal direction from other chronic disease to arthritis)
#5. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Adjusted models (Causal direction from arthritis to other chronic disease)

#ADDITIONAL ANALYSES
#1. Exclude Proxy interview: Crude models
#2. Exclude Proxy interview: Adjusted models
#3. Requiring >=2 self-reported doctor diagnosed arthritis across participating waves: Crude models
#4. Requiring >=2 self-reported doctor diagnosed arthritis across participating waves: Adjusted models
#5. Requiring >=2 self-reported doctor diagnosed other chronic diseases across participating waves: Crude models
#6. Requiring >=2 self-reported doctor diagnosed other chronic diseases across participating waves: Adjusted models
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
#DESCRIPTIVE ANALYSES
#1. Household and individual characteristics at inclusion, as well as spousal arthritis/rheumatism and other chronic conditions by respondent gender and presence of arthritis in respondents
###################################################### 
#1. Household and individual characteristics at inclusion, as well as spousal arthritis/rheumatism and other chronic conditions by respondent gender and presence of arthritis in respondents
###################################################### 
#CHARLS
###################################################### 
#Load data
setwd("/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/CHARLS")
load("hcharls_all_sp11.rda")

#Create baseline and outcome variables
hcharls_t1 <- sample_data %>%
  select(rage, rabyear, ragender, hrural, rmcurln, raeducl, rmbmin, rmbmicat, rpact, rdrinkr, rsmokev, roccup2_2, hincome, hkcnt, ramomeducl, radadeducl, rclstress2, rshlta, radlfive, rmobilsev, arthritis, arthritis_dm, hibpe, hibpe_dm, diabe, diabe_dm, cancre, cancre_dm, lunge, lunge_dm, hearte, hearte_dm, stroke, stroke_dm, psyche, psyche_dm, dyslipe, dyslipe_dm, livere, livere_dm, kidneye, kidneye_dm, digeste, digeste_dm, asthmae) %>%
  tbl_summary(
    by = gender,
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test", #edit needed 
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  modify_caption("**Table 1. Baseline Characteristics, spousal arthritis and other chronic diseases by Gender**") %>%
  bold_labels() %>%
  italicize_levels()
###################################################### 
#ELSA
###################################################### 
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/ELSA')
load("helsa_all_sp28.rda")

###################################################### 
#SHARE
###################################################### 
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/SHARE')
load("hshare_all_sp26.rda")
###################################################### 
#MHAS
###################################################### 
#Load data
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/MHAS')
load("hmhas_all_sp23.rda")
###################################################### 
#KLoSA
###################################################### 
#Load data
setwd('/Users/wengianche/Documents/UM MACAO FELLOW/Projects/Spousal aggregation of arthritis/Manuscript 1/Data/KLoSA')
load("hklosa_all_sp22.rda")
###################################################### 
###################################################### 
#STATISTICAL ANALYSES
#1. Spousal aggregation of arthritis/rheumatism: Crude models
#2. Spousal aggregation of arthritis/rheumatism: Adjusted models
#3. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Crude models
#4. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Adjusted models (Causal direction from other chronic disease to arthritis)
#5. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Adjusted models (Causal direction from arthritis to other chronic disease)
###################################################### 
#1. Spousal aggregation of arthritis/rheumatism: Crude models
###################################################### 
###################################################### 
#2. Spousal aggregation of arthritis/rheumatism: Adjusted models
###################################################### 
###################################################### 
#3. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Crude models
###################################################### 
###################################################### 
#4. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Adjusted models (Causal direction from other chronic disease to arthritis)
###################################################### 
###################################################### 
#5. Spousal coaggregation of other chronic diseases in individuals with arthritis/rheumatism: Adjusted models (Causal direction from arthritis to other chronic disease)
###################################################### 
###################################################### 
###################################################### 
#ADDITIONAL ANALYSES
#1. Exclude Proxy interview: Crude models
#2. Exclude Proxy interview: Adjusted models
#3. Requiring >=2 self-reported doctor diagnosed arthritis across participating waves: Crude models
#4. Requiring >=2 self-reported doctor diagnosed arthritis across participating waves: Adjusted models
#5. Requiring >=2 self-reported doctor diagnosed other chronic diseases across participating waves: Crude models
#6. Requiring >=2 self-reported doctor diagnosed other chronic diseases across participating waves: Adjusted models
###################################################### 
#1. Exclude Proxy interview: Crude models
###################################################### 
###################################################### 
#2. Exclude Proxy interview: Adjusted models
###################################################### 
###################################################### 
#3. Requiring >=2 self-reported doctor diagnosed arthritis across participating waves: Crude models
###################################################### 
###################################################### 
#4. Requiring >=2 self-reported doctor diagnosed arthritis across participating waves: Adjusted models
###################################################### 
###################################################### 
#5. Requiring >=2 self-reported doctor diagnosed other chronic diseases across participating waves: Crude models
###################################################### 
###################################################### 
#6. Requiring >=2 self-reported doctor diagnosed other chronic diseases across participating waves: Adjusted models
###################################################### 
###################################################### 

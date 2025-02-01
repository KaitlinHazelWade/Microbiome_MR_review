#########################################################################################
## Project: Systematic review of MR studies with the gut microbiome as an exposure
## Script: Cleaning the data before analyses and synthesis of evidence
## Created: 03/11/2023
## By: Charlie Hatcher and adapted by Kaitlin Wade
#########################################################################################
rm(list=ls())

################
## Setting up ##
################

## Install packages
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("dplyr")
#install.packages("data.table")
#install.packages("stringr")
#install.packages("openxlsx")
#install.packages("meta")
#install.packages("ggstance")
#install.packages("grid")

## Load packages
library(readxl)
library(ggplot2)
library(cowplot)
library(dplyr)
library(data.table)
library(stringr)
library(openxlsx)
library(meta)
library(ggstance)
library(grid)

## Set the working directory 
setwd("C:/Users/kw8985/OneDrive - University of Bristol/Migrated O Drive/Post-Doc/Work/Reviews and editorials/MR Microbiome Sys Review/")

#####################
## Reading in data ##
#####################

## Read in data for Kaitlin's data extraction and change column names
data_kaitlin <- read_excel("./Backup/DataExtraction/final_data_extraction_310125.xlsx", sheet="Kaitlin")
data_kaitlin <- subset(data_kaitlin, select = -c(96:98))
data_kaitlin <- data_kaitlin[-c(1,2), ]
colnames(data_kaitlin) <- c("ID", "abstract", "preprint", "published", "title", "year", "journal", "doi", "first_author", "corresponding_name", "corresponding_email", "corresponding_address",
"hypothesis", "rationale", "study_design", "outcome_formatted", "outcome", "outcome_group", "outcome_def", "outcome_study", "outcome_age", "outcome_sex", "outcome_og_gwas_case",
"outcome_og_gwas_control", "outcome_study_case", "outcome_study_control", "outcome_n_diff", "outcome_health", "outcome_pop", "exposure_group", "exposure", "exposure_study", "exposure_age", 
"exposure_sex", "exposure_study_n", "exposure_og_n", "exposure_n_diff", "exposure_health", "exposure_pop", "exposure_gwas_covs", "exposure_impute", "exposure_indep", "pleiotropy", "pval_threshold",
"palindrome", "proxy", "hwe", "single_instrument", "snp_number", "GRS", "gwas_snp_number", "snp_number_diff", "power", "power_method", "main_method", "sensitivity_1", "sensitivity_2", "sensitivity_3", 
"sensitivity_4", "sensitivity_5", "analysis_notes", "instrument_strength", "software", "program", "replication_indep", "replication", "multiple_testing", "exposure_unit", "outcome_unit", "mrestimate", 
"mrbeta", "mrse", "mrci", "mrlci", "mruci", "mrp", "mradjustment", "mrpadj", "limitations", "limit_pleiotropy", "limit_pop", "limit_power", "other_gwasinfo", "other_else", "other_bias", "dup_exposure", 
"dup_outcome", "dup_mrbeta", "dup_mrse", "dup_mrci", "dup_mrlci", "dup_mruci", "dup_mrp", "dup_mrpadj", "dup_f")
data_kaitlin$initial_extractor <- "Kaitlin"
data <- data_kaitlin[rowSums(is.na(data_kaitlin)) != ncol(data_kaitlin), ] ## 28270 rows and 96 rows
rm(data)

## Read in data for Charlie's data extraction and change column names
data_charlie <- read_excel("./Backup/DataExtraction/final_data_extraction_310125.xlsx", sheet="Charlies")
colnames(data_charlie) <- c("ID", "abstract", "preprint", "published", "title", "year", "journal", "doi", "first_author", "corresponding_name", "corresponding_email", "corresponding_address",
"hypothesis", "rationale", "study_design", "outcome_formatted", "outcome", "outcome_group", "outcome_def", "outcome_study", "outcome_age", "outcome_sex", "outcome_study_case", "outcome_study_control",
"outcome_og_gwas_case", "outcome_og_gwas_control", "outcome_n_diff", "outcome_health", "outcome_pop", "exposure_group", "exposure", "exposure_study", "exposure_age", "exposure_sex", "exposure_study_n",
"exposure_og_n", "exposure_n_diff", "exposure_health", "exposure_pop", "exposure_gwas_covs", "exposure_impute", "exposure_indep", "pleiotropy", "pval_threshold", "palindrome", "proxy", "hwe", 
"single_instrument", "snp_number", "GRS", "gwas_snp_number", "snp_number_diff", "power", "power_method", "main_method", "sensitivity_1", "sensitivity_2", "sensitivity_3", "sensitivity_4", "sensitivity_5", 
"analysis_notes", "instrument_strength", "software", "program", "replication_indep", "replication", "multiple_testing", "exposure_unit", "outcome_unit", "mrestimate", "mrbeta", "mrse", "mrci", "mrlci", 
"mruci", "mrp", "mradjustment", "mrpadj", "limitations", "limit_pleiotropy", "limit_pop", "limit_power", "other_gwasinfo", "other_else", "other_bias", "dup_exposure", "dup_outcome", "dup_mrbeta", "dup_mrse", 
"dup_mrci", "dup_mrlci", "dup_mruci", "dup_mrp", "dup_mrpadj", "dup_f")
data_charlie$initial_extractor <- "Charlie"
data <- data_charlie[rowSums(is.na(data_charlie)) != ncol(data_charlie), ] ## 20340 and 96 rows

## Combine the data
data <- rbind(data_kaitlin, data_charlie)
dim(data) # should be 48610 rows and 96 columns

## Replace the inital extractor variable to be "Both" when we extracted together
ids <- c(0557846834, 0289377291, 1519899723, 0601575515, 0122541860, 3836325935, 3278202937, 1977124126)
for(i in ids){
  data$initial_extractor[which(data$ID == i)] <- "Both"
}
table(data$initial_extractor)

###############################
## Cleaning up exposure data ##
###############################

## Look at exposures
table(data$exposure_group)

## Summarise exposures by exposure group, ID and exposure
summ_exposure_group <- data %>%
  group_by(ID, exposure_group, exposure, exposure_study) %>%
  summarise() 
head(summ_exposure_group)
table(summ_exposure_group$exposure_group)

## Renaming all exposures to be gut microbiome for completeness (meta-analyses are going to be prioritized based on outcome)
data$exposure_group[which(data$exposure_group == "Microbial traits (see notes)" & data$ID == "NA")] <- "Gut microbiome"
data$exposure_group[which(data$exposure_group == "Microbial abundance" & data$ID == "Other sources")] <- "Gut microbiome"
data$exposure_group[which(data$exposure_group == "Metagenomic pathway" & data$ID == "Other sources")] <- "Gut microbiome pathway"
data$exposure_group[which(data$exposure_group == "Gut microbiome abundance" & data$ID == 4290593439)] <- "Gut microbiome"

## Given discussions with Charlie that we may have missed some data from papers with metabolites, we're removing metabolites from analysis
data <- data[-which(data$exposure_group == "Gut microbiota dependent metabolites" & data$ID == 2526390572),] # removes 4 entries; 48606 (whole paper)
data <- data[-which(data$exposure_group == '"Microbiome dependent metabolites"' & data$ID == 1108156353),] # removes 4 entries; 48602 (not whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-related metabolites" & data$ID == 3191171275),] # removes 5 entries; 48597 (not whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-dependent metabolites" & data$ID == 1795779689),] # removes 40 entries; 48557 (not whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-dependent metabolites" & data$ID == 2044689642),] # removes 24 entries; 48533 (not whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-dependent metabolites" & data$ID == 3184652665),] # removes 8 entries; 48525 (not whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-dependent metabolites" & data$ID == 3549710879),] # removes 3 entries; 48522 (not whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-dependent metabolites" & data$ID == 4286528530),] # removes 144 entries; 48378 (not whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-dependent metabolites" & data$ID == 505573255),] # removes 24 entries; 48354 (not whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-dependent metabolites" & data$ID == 637274083),] # removes 100 entries; 48254 (whole paper)
data <- data[-which(data$exposure_group == "Gut microbiome-related metabolites" & data$ID == "Other sources"),] # removes 171 entries; 48083 (not whole paper)

## Check that there are still only 66 studies - 48084 observations
length(unique(data$ID)) ## 66 - woooooohooooo!!

#######################################################################################
## Cleaning up mistakes in outcome data and harmonizing outcome groups and subgroups ##
#######################################################################################

## Look at outcomes
table(data$outcome_group)

## Edit outcome groups that were incorrectly named or incorrectly assigned 
data$outcome_group[which(data$outcome_group == "Cardiovascualr")] <- "Cardiovascular"
data$outcome_group[which(data$outcome_group == "Esophagus")] <- "Oesophagus"
data$outcome_group[which(data$outcome_group == "Heart")] <- "Cardiovascular"
data$outcome_group[which(data$outcome_group == "Eye")] <- "Eyes"
data$outcome_group[which(data$outcome_group == "Gum disease")] <- "Mouth"
data$outcome_group[which(data$outcome_group == "Drug usage")] <- "Drugs"
data$outcome_group[which(data$outcome_group == "Drug reaction")] <- "Drugs"

## Present all outcomes, formatted outcomes (i.e., subcategories) and outcome groups (i.e., broad categories)
## Autoimmune group ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
head(summ_outcome_group)
summ_outcome_group[summ_outcome_group$outcome_group == "Autoimmunity",]

# Change
data$outcome_formatted[which(data$outcome_formatted == "Autoimmune immune disease")] <- "Autoimmune disease"
data$outcome[which(data$outcome == "RA")] <- "Rheumatoid arthritis"
data$outcome_formatted[which(data$outcome == "Asthma")] <- "Allergy"
data$outcome_group[which(data$outcome == "Asthma")] <- "Immune"
data$outcome_formatted[which(data$outcome == "Eczema")] <- "Allergy"
data$outcome_group[which(data$outcome == "Eczema")] <- "Immune"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Autoimmunity",]
table(data$outcome_group)

## Behaviour ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Behaviour",]

# Change
data$outcome[which(data$outcome == "beer/cider glasses per month")] <- "Beer/cider glasses per month"
data$outcome[which(data$outcome == "champagne/white wine glasses per month")] <- "Champagne/white wine glasses per month"
data$outcome[which(data$outcome == "current + former drinkers vs. never")] <- "Current and former drinkers vs. never"
data$outcome[which(data$outcome == "current drinkers vs. never")] <- "Current drinkers vs. never"
data$outcome[which(data$outcome == "current drinkerswith meals: yes, it varies, no")] <- "Current drinkers with meals: yes, it varies, no"
data$outcome[which(data$outcome == "current drinkers with meals: yes + it varies vs. no")] <- "Current drinkers with meals: yes / it varies vs. no"
data$outcome[which(data$outcome == "current drinkers with meals: yes vs. no")] <- "Current drinkers with meals: yes vs. no"
data$outcome[which(data$outcome == "fortwine glasses per month")] <- "Fortwine glasses per month"
data$outcome[which(data$outcome == "other alcohol glasses per month")] <- "Other alcohol glasses per month"
data$outcome[which(data$outcome == "overall alcohol intake")] <- "Alcohol intake"
data$outcome[which(data$outcome == "red wine glasses per month")] <- "Red wine glasses per month"
data$outcome[which(data$outcome == "spirits measures per month")] <- "Spirits measures per month"
data$outcome[which(data$outcome == "total drinks of alcohol per month")] <- "Total drinks of alcohol per month"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Behaviour",]
table(data$outcome_group)

## Blood - want to change this to be in the Cardiovascular outcome_group ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Blood",]

# Change
data$outcome[which(data$outcome == "Lymphocytec count")] <- "Lymphocyte count"
data$outcome[which(data$outcome == "Monocyte_count")] <- "Monocyte count"
data$outcome_formatted[which(data$outcome == "Basophil count")] <- "Blood cell count"
data$outcome_formatted[which(data$outcome == "Eosinophil count")] <- "Blood cell count"
data$outcome_formatted[which(data$outcome == "Lymphocyte count")] <- "Blood cell count"
data$outcome_formatted[which(data$outcome == "Monocyte count")] <- "Blood cell count"
data$outcome_formatted[which(data$outcome == "Platelet count")] <- "Blood clotting"
data$outcome_formatted[which(data$outcome == "Mean corpuscular hemoglobin")] <- "Red blood cell"
data$outcome_formatted[which(data$outcome == "Red blood cell count")] <- "Blood cell count"
data$outcome_formatted[which(data$outcome == "White blood cell count")] <- "Blood cell count"
data$outcome_formatted[which(data$outcome == "Fibrinogen")] <- "Plasma protein"
data$outcome_group[which(data$outcome == "Fibrinogen")] <- "Protein"
data$outcome_group[which(data$outcome_group == "Blood")] <- "Cardiovascular"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Blood",]
table(data$outcome_group)

## Bone ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Bone",]
summ_outcome_group[summ_outcome_group$outcome_group == "Skeletal",]
summ_outcome_group[summ_outcome_group$outcome_group == "Musculoskeletal disease",]

# Change
data$outcome[which(data$outcome == "Any osteoarthritis")] <- "Osteoarthritis"
data$outcome[which(data$outcome == "Knee/Hip osteoarthritis")] <- "Knee/hip osteoarthritis"
data$outcome_group[which(data$outcome == "Gout")] <- "Bone"
data$outcome_formatted[which(data$outcome == "Gout")] <- "Bone disease"
data$outcome_group[which(data$outcome == "Paget's disease")] <- "Bone"
data$outcome_formatted[which(data$outcome == "Paget's disease")] <- "Bone disease"
data$outcome_group[which(data$outcome == "Chronic widespread pain")] <- "Pain"
data$outcome_group[which(data$outcome == "Multiple site chronic pain")] <- "Pain"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Bone",]
table(data$outcome_group)

## Brain ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Brain",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Brain"])
summ_outcome_group[summ_outcome_group$outcome_formatted == "Brain disease",]
summ_outcome_group[summ_outcome_group$outcome_formatted == "Epilepsy",]
summ_outcome_group[summ_outcome_group$outcome_formatted == "Mental health",]
summ_outcome_group[summ_outcome_group$outcome_formatted == "Mental health disorder",]
summ_outcome_group[summ_outcome_group$outcome_formatted == "Mental health disorders",]
summ_outcome_group[summ_outcome_group$outcome_formatted == "Personality",]
summ_outcome_group[summ_outcome_group$outcome_formatted == "Neurodegenerative disease",]
summ_outcome_group[summ_outcome_group$outcome_formatted == "Neurodegenerative diseases",]

# Change
data$outcome[which(data$outcome == "Cerebral_aneurysm")] <- "Cerebral aneurysm"
data$outcome_group[which(data$outcome == "Cerebral aneurysm")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Cerebral aneurysm")] <- "Brain aneurysm"
data$outcome_formatted[which(data$outcome == "Autism")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Schizophrenia")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Extroversion")] <- "Personality trait"
data$outcome_formatted[which(data$outcome == "Alzheimer's disease")] <- "Neurodegenerative diseases"
data$outcome_formatted[which(data$outcome == "ADHD")] <- "Mental health disorders"
data$outcome[which(data$outcome == "ADHD")] <- "Attention deficit hyperactivity disorder"
data$outcome_formatted[which(data$outcome == "Able to confide")] <- "Personality trait"
data$outcome_group[which(data$outcome == "Age of smoking initiation")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Age of smoking initiation")] <- "Smoking"
data$outcome_group[which(data$outcome == "Age of stopping smoking")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Age of stopping smoking")] <- "Smoking"
data$outcome[which(data$outcome == "Age of stopping smoking")] <- "Age stopped smoking"
data$outcome_group[which(data$outcome == "Age started smoking in current smokers")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Age started smoking in current smokers")] <- "Smoking"
data$outcome_group[which(data$outcome == "Age started smoking in former smokers")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Age started smoking in former smokers")] <- "Smoking"
data$outcome_group[which(data$outcome == "Age stopped smoking")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Age stopped smoking")] <- "Smoking"
data$outcome_group[which(data$outcome == "Alcohol")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Alcohol")] <- "Alcohol"
data$outcome[which(data$outcome == "Alcohol consumed")] <- "Alcohol consumption"
data$outcome_group[which(data$outcome == "Alcohol consumption")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Alcohol consumption")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Alcohol dependence")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Alcohol dependence")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Alcohol drinker status: Never")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Alcohol drinker status: Never")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Alcohol drinker status: Previous")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Alcohol drinker status: Previous")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Alcohol intake frequency.")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Alcohol intake frequency.")] <- "Alcohol"
data$outcome[which(data$outcome == "Alcohol intake frequency.")] <- "Alcohol intake frequency"
data$outcome_group[which(data$outcome == "Alcohol intake versus 10 years previously")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Alcohol intake versus 10 years previously")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Alcohol usually taken with meals")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Alcohol usually taken with meals")] <- "Alcohol"
data$outcome_formatted[which(data$outcome == "Amygdala volume")] <- "Brain health"
data$outcome_formatted[which(data$outcome == "Amyotrophic lateral sclerosis")] <- "Mental health disorders"
data$outcome[which(data$outcome == "Anorexia Nervosa")] <- "Anorexia nervosa"
data$outcome_formatted[which(data$outcome == "Anorexia nervosa")] <- "Mental health disorders"
data$outcome[which(data$outcome == "Attempted fluid intelligence (FI) test.")] <- "Attempted fluid intelligence test"
data$outcome[which(data$outcome == "Attention deficit/ hyperactivity disorder (ADHD)")] <- "Attention deficit hyperactivity disorder"
data$outcome_formatted[which(data$outcome == "Attention deficit hyperactivity disorder")] <- "Mental health disorders"
data$outcome[which(data$outcome == "Autism")] <- "Autism spectrum disorder"
data$outcome_formatted[which(data$outcome == "Autism Spectrum Disorder")] <- "Mental health disorders"
data$outcome[which(data$outcome == "Autism Spectrum Disorder")] <- "Autism spectrum disorder"
data$outcome_group[which(data$outcome == "Average monthly intake of other alcoholic drinks")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Average monthly intake of other alcoholic drinks")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Average weekly beer plus cider intake")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Average weekly beer plus cider intake")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Average weekly champagne plus white wine intake")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Average weekly champagne plus white wine intake")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Average weekly fortified wine intake")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Average weekly fortified wine intake")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Average weekly intake of other alcoholic drinks")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Average weekly intake of other alcoholic drinks")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Average weekly red wine intake")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Average weekly red wine intake")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Average weekly spirits intake")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Average weekly spirits intake")] <- "Alcohol"
data$outcome_formatted[which(data$outcome == "Bulimia nervosa")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Caudate volume")] <- "Brain health"
data$outcome_formatted[which(data$outcome == "Chronotype")] <- "Sleep"
data$outcome_group[which(data$outcome == "Chronotype")] <- "Behaviour"
data$outcome_group[which(data$outcome == "Cigarettes smoked per day")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Cigarettes smoked per day")] <- "Smoking"
data$outcome_group[which(data$outcome == "Current tobacco smoking")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Current tobacco smoking")] <- "Smoking"
data$outcome_formatted[which(data$outcome == "Daytime dozing / sleeping (narcolepsy)")] <- "Sleep disorders"
data$outcome_formatted[which(data$outcome == "Depressive symptoms")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Destinations on discharge from hospital (recoded): High security psychiatric hospital: NHS")] <- "Mental health hospitalization"
data$outcome_formatted[which(data$outcome == "Destinations on discharge from hospital (recoded): High security psychiatric hospital: Other")] <- "Mental health hospitalization"
data$outcome_formatted[which(data$outcome == "Destinations on discharge from hospital (recoded): Transfer to other NHS provider: Psychiatry, learning disabilities")] <- "Mental health hospitalization"
data$outcome_formatted[grep("Diagnoses - main ICD10: E", data$outcome)] <- "Mental health disorders"
data$outcome_formatted[grep("Diagnoses - main ICD10: F", data$outcome)] <- "Mental health disorders"
data$outcome_formatted[grep("Diagnoses - main ICD10: G12", data$outcome)] <- "Neurodegenerative diseases"
data$outcome_formatted[grep("Diagnoses - main ICD10: G31", data$outcome)] <- "Neurodegenerative diseases"
data$outcome_formatted[grep("Diagnoses - main ICD10: G40", data$outcome)] <- "Epilepsy"
data$outcome_formatted[grep("Diagnoses - main ICD10: G47", data$outcome)] <- "Sleep disorders"
data$outcome_group[grep("Diagnoses - main ICD10: G5", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - main ICD10: G5", data$outcome)] <- "Neuropathy"
data$outcome_group[grep("Diagnoses - main ICD10: G6", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - main ICD10: G6", data$outcome)] <- "Neuropathy"
data$outcome_group[grep("Diagnoses - main ICD10: G9", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - main ICD10: G9", data$outcome)] <- "Neuropathy"
data$outcome_group[grep("Diagnoses - main ICD10: H8", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - main ICD10: H8", data$outcome)] <- "Neuronitis"
data$outcome_group[grep("Diagnoses - main ICD10: I42", data$outcome)] <- "Cardiovascular"
data$outcome_formatted[grep("Diagnoses - main ICD10: I42", data$outcome)] <- "Alcoholic cardiomyopathy"
data$outcome_group[grep("Diagnoses - main ICD10: K29.2", data$outcome)] <- "Gastrointestinal tract"
data$outcome_formatted[grep("Diagnoses - main ICD10: K29.2", data$outcome)] <- "Alcoholic gastritis"
data$outcome_group[grep("Diagnoses - main ICD10: K86.0", data$outcome)] <- "Pancreas"
data$outcome_formatted[grep("Diagnoses - main ICD10: K86.0", data$outcome)] <- "Alcoholic pancreatitis"
data$outcome_group[grep("Diagnoses - main ICD10: K70", data$outcome)] <- "Liver"
data$outcome_formatted[grep("Diagnoses - main ICD10: K70", data$outcome)] <- "Liver disease"
data$outcome_group[grep("Diagnoses - main ICD10: M", data$outcome)] <- "Pain"
data$outcome_formatted[grep("Diagnoses - main ICD10: M", data$outcome)] <- "Algoneurodystrophy"
data$outcome_group[grep("Diagnoses - main ICD10: N", data$outcome)] <- "Bladder"
data$outcome_formatted[grep("Diagnoses - main ICD10: N", data$outcome)] <- "Bladder dysfunction"
data$outcome_formatted[grep("Diagnoses - main ICD10: Q", data$outcome)] <- "Brain disease"
data$outcome_group[grep("Diagnoses - main ICD10: T42", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - main ICD10: T42", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - main ICD10: T43.0", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - main ICD10: T43.0", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - main ICD10: T43.2", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - main ICD10: T43.2", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - main ICD10: T43.3", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - main ICD10: T43.3", data$outcome)] <- "Antipsychotics"
data$outcome_group[grep("Diagnoses - main ICD10: T43.4", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - main ICD10: T43.4", data$outcome)] <- "Antipsychotics"
data$outcome_group[grep("Diagnoses - main ICD10: T43.5", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - main ICD10: T43.5", data$outcome)] <- "Antipsychotics"
data$outcome_group[grep("Diagnoses - main ICD10: T51", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - main ICD10: T51", data$outcome)] <- "Alcohol"
data$outcome_group[grep("Diagnoses - main ICD10: T78", data$outcome)] <- "Inflammation"
data$outcome_formatted[grep("Diagnoses - main ICD10: T78", data$outcome)] <- "Angioneurotic oedema"
data$outcome_group[grep("Diagnoses - main ICD10: T87", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - main ICD10: T87", data$outcome)] <- "Neuroma"
data$outcome_formatted[grep("Diagnoses - main ICD10: Z", data$outcome)] <- "Mental health hospitalization"
data$outcome_formatted[grep("Diagnoses - main ICD9: 3000", data$outcome)] <- "Mental health disorders"
data$outcome_group[grep("Diagnoses - main ICD9: 3050", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - main ICD9: 3050", data$outcome)] <- "Alcohol"
data$outcome_formatted[grep("Diagnoses - main ICD9: 3451", data$outcome)] <- "Epilepsy"
data$outcome_formatted[grep("Diagnoses - main ICD9: 3459", data$outcome)] <- "Epilepsy"
data$outcome_group[grep("Diagnoses - main ICD9: 5353", data$outcome)] <- "Gastrointestinal tract"
data$outcome_formatted[grep("Diagnoses - main ICD9: 5353", data$outcome)] <- "Alcoholic gastritis"
data$outcome_formatted[grep("Diagnoses - main ICD9: 7805", data$outcome)] <- "Sleep disorders"
data$outcome_group[grep("Diagnoses - main ICD9: 9690", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - main ICD9: 9690", data$outcome)] <- "Antidepressants"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: E", data$outcome)] <- "Mental health disorders"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: F", data$outcome)] <- "Mental health disorders"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: G12", data$outcome)] <- "Neurodegenerative diseases"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: G31", data$outcome)] <- "Neurodegenerative diseases"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: G40", data$outcome)] <- "Epilepsy"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: G47", data$outcome)] <- "Sleep disorders"
data$outcome_group[grep("Diagnoses - secondary ICD10: G5", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: G5", data$outcome)] <- "Neuropathy"
data$outcome_group[grep("Diagnoses - secondary ICD10: G6", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: G6", data$outcome)] <- "Neuropathy"
data$outcome_group[grep("Diagnoses - secondary ICD10: G9", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: G9", data$outcome)] <- "Neuropathy"
data$outcome_group[grep("Diagnoses - secondary ICD10: H8", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: H8", data$outcome)] <- "Neuronitis"
data$outcome_group[grep("Diagnoses - secondary ICD10: I42", data$outcome)] <- "Cardiovascular"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: I42", data$outcome)] <- "Alcoholic cardiomyopathy"
data$outcome_group[grep("Diagnoses - secondary ICD10: K29.2", data$outcome)] <- "Gastrointestinal tract"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: K29.2", data$outcome)] <- "Alcoholic gastritis"
data$outcome_group[grep("Diagnoses - secondary ICD10: K86.0", data$outcome)] <- "Pancreas"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: K86.0", data$outcome)] <- "Alcoholic pancreatitis"
data$outcome_group[grep("Diagnoses - secondary ICD10: K70", data$outcome)] <- "Liver"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: K70", data$outcome)] <- "Liver disease"
data$outcome_group[grep("Diagnoses - secondary ICD10: M", data$outcome)] <- "Pain"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: M", data$outcome)] <- "Algoneurodystrophy"
data$outcome_group[grep("Diagnoses - secondary ICD10: N", data$outcome)] <- "Bladder"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: N", data$outcome)] <- "Bladder dysfunction"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Q", data$outcome)] <- "Brain disease"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: R45", data$outcome)] <- "Mood and feelings"
data$outcome_group[grep("Diagnoses - secondary ICD10: R78.0", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: R78.0", data$outcome)] <- "Alcohol"
data$outcome_group[grep("Diagnoses - secondary ICD10: T42", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T42", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - secondary ICD10: T43.0", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T43.0", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - secondary ICD10: T43.2", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T43.2", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - secondary ICD10: T43.3", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T43.3", data$outcome)] <- "Antipsychotics"
data$outcome_group[grep("Diagnoses - secondary ICD10: T43.4", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T43.4", data$outcome)] <- "Antipsychotics"
data$outcome_group[grep("Diagnoses - secondary ICD10: T43.5", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T43.5", data$outcome)] <- "Antipsychotics"
data$outcome_group[grep("Diagnoses - secondary ICD10: T51", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T51", data$outcome)] <- "Alcohol"
data$outcome_group[grep("Diagnoses - secondary ICD10: T78", data$outcome)] <- "Inflammation"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T78", data$outcome)] <- "Angioneurotic oedema"
data$outcome_group[grep("Diagnoses - secondary ICD10: T87", data$outcome)] <- "Nervous system"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: T87", data$outcome)] <- "Neuroma"
data$outcome_group[grep("Diagnoses - secondary ICD10: Y49.0", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Y49.0", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - secondary ICD10: Y49.2", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Y49.2", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - secondary ICD10: Y49.5", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Y49.5", data$outcome)] <- "Antipsychotics"
data$outcome_group[grep("Diagnoses - secondary ICD10: Y88", data$outcome)] <- "Medical procedure"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Y88", data$outcome)] <- "Abnormal reaction"
data$outcome_group[grep("Diagnoses - secondary ICD10: Y90", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Y90", data$outcome)] <- "Alcohol"
data$outcome_group[grep("Diagnoses - secondary ICD10: Y91", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Y91", data$outcome)] <- "Alcohol"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Z", data$outcome)] <- "Mental health hospitalization"
data$outcome_group[grep("Diagnoses - secondary ICD10: Z72", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Z72", data$outcome)] <- "Alcohol"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: Z82", data$outcome)] <- "Epilepsy"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 3000", data$outcome)] <- "Mental health disorders"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 3039", data$outcome)] <- "Mental health disorders"
data$outcome_group[grep("Diagnoses - secondary ICD9: 3050", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 3050", data$outcome)] <- "Alcohol"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 3119", data$outcome)] <- "Mental health disorders"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 3451", data$outcome)] <- "Epilepsy"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 3459", data$outcome)] <- "Epilepsy"
data$outcome_group[grep("Diagnoses - secondary ICD9: 5353", data$outcome)] <- "Gastrointestinal tract"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 5353", data$outcome)] <- "Alcohol gastritis"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 7805", data$outcome)] <- "Sleep disorders"
data$outcome_group[grep("Diagnoses - secondary ICD9: 9690", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 9690", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("Diagnoses - secondary ICD9: 9800", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: 9800", data$outcome)] <- "Alcohol"
data$outcome_group[grep("Diagnoses - secondary ICD9: E87", data$outcome)] <- "Medical procedure"
data$outcome_formatted[grep("Diagnoses - secondary ICD9: E87", data$outcome)] <- "Abnormal reaction"
data$outcome_group[which(data$outcome == "Difficulty not smoking for 1 day")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Difficulty not smoking for 1 day")] <- "Smoking"
data$outcome_group[which(data$outcome == "Difficulty not smoking for 1 day")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Difficulty not smoking for 1 day")] <- "Smoking"
data$outcome_formatted[which(data$outcome == "Ever depressed for a whole week")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Ever highly irritable/argumentative for 2 days")] <- "Mood and feelings"
data$outcome_group[which(data$outcome == "Ever smoked")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Ever smoked")] <- "Smoking"
data$outcome_group[which(data$outcome == "Ever stopped smoking for 6+ months")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Ever stopped smoking for 6+ months")] <- "Smoking"
data$outcome_group[which(data$outcome == "Ever tried to stop smoking")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Ever tried to stop smoking")] <- "Smoking"
data$outcome_formatted[which(data$outcome == "Ever unenthusiastic/disinterested for a whole week")] <- "Mood and feelings"
data$outcome_group[which(data$outcome == "Ever vs never smoked")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Ever vs never smoked")] <- "Smoking"
data$outcome_group[which(data$outcome == "Exposure to tobacco smoke at home")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Exposure to tobacco smoke at home")] <- "Smoking"
data$outcome_group[which(data$outcome == "Exposure to tobacco smoke outside home")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Exposure to tobacco smoke outside home")] <- "Smoking"
data$outcome_group[grep("External causes: Y49.0", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("External causes: Y49.0", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("External causes: Y49.2", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("External causes: Y49.2", data$outcome)] <- "Antidepressants"
data$outcome_group[grep("External causes: Y49.5", data$outcome)] <- "Drugs"
data$outcome_formatted[grep("External causes: Y49.5", data$outcome)] <- "Antipsychotics"
data$outcome_group[grep("External causes: Y88.3 ", data$outcome)] <- "Medical procedure"
data$outcome_formatted[grep("External causes: Y88.3 ", data$outcome)] <- "Abnormal reaction"
data$outcome_group[grep("External causes: Y91.9", data$outcome)] <- "Behaviour"
data$outcome_formatted[grep("External causes: Y91.9", data$outcome)] <- "Alcohol"
data$outcome_formatted[which(data$outcome == "Fed-up feelings")] <- "Mood and feelings"
data$outcome_group[which(data$outcome == "Former alcohol drinker")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Former alcohol drinker")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Former vs current smoker")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Former vs current smoker")] <- "Smoking"
data$outcome_formatted[which(data$outcome == "Frequency of depressed mood in last 2 weeks")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Frequency of tenseness / restlessness in last 2 weeks")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Frequency of tiredness / lethargy in last 2 weeks")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Frequency of unenthusiasm / disinterest in last 2 weeks")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Frequency of tenseness / restlessness in last 2 weeks")] <- "Mood and feelings"
data$outcome_group[which(data$outcome == "Getting up in morning")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Getting up in morning")] <- "Sleep"
data$outcome_formatted[which(data$outcome == "Guilty feelings")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Handedness (chirality/laterality): Left-handed")] <- "Personality trait"
data$outcome_formatted[which(data$outcome == "Handedness (chirality/laterality): Use both right and left hands equally")] <- "Personality trait"
data$outcome_formatted[which(data$outcome == "Happiness")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Hippocampus volume")] <- "Brain health"
data$outcome_formatted[which(data$outcome == "Illnesses of father: Severe depression")] <- "Family history of mental health disorders"
data$outcome_formatted[which(data$outcome == "Illnesses of mother: Severe depression")] <- "Family history of mental health disorders"
data$outcome_formatted[which(data$outcome == "Illnesses of siblings: Severe depression")] <- "Family history of mental health disorders"
data$outcome_formatted[which(data$outcome == "Internalizing problems")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Intracranial volume")] <- "Brain health"
data$outcome_formatted[which(data$outcome == "Irritability")] <- "Mood and feelings"
data$outcome_group[which(data$outcome == "Lifetime number of same-sex sexual partners")] <- "Sexual and reproductive health"
data$outcome_formatted[which(data$outcome == "Lifetime number of same-sex sexual partners")] <- "Sexuality"
data$outcome_group[which(data$outcome == "Lifetime number of sexual partners")] <- "Sexual and reproductive health"
data$outcome_formatted[which(data$outcome == "Lifetime number of sexual partners")] <- "Sexuality"
data$outcome[which(data$outcome == "Light smokers  at least 100 smokes in lifetime")] <- "Light smokers, at least 100 smokes in lifetime"
data$outcome_formatted[which(data$outcome == "Light smokers, at least 100 smokes in lifetime")] <- "Smoking"
data$outcome_group[which(data$outcome == "Light smokers, at least 100 smokes in lifetime")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Likelihood of resuming smoking")] <- "Smoking"
data$outcome_group[which(data$outcome == "Likelihood of resuming smoking")] <- "Behaviour"
data$outcome[which(data$outcome == "Loneliness  isolation")] <- "Loneliness and isolation"
data$outcome[which(data$outcome == "Loneliness, isolation")] <- "Loneliness and isolation"
data$outcome_formatted[which(data$outcome == "Loneliness isolation")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Loneliness, isolation")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Longest period of depression")] <- "Mental health disorders"
data$outcome_formatted[grep("Main speciality of consultant \\(recoded\\)", data$outcome)] <- "Mental health hospitalization"
data$outcome[which(data$outcome == "Major Depressive Disorder")] <- "Major depressive disorder"
data$outcome_formatted[which(data$outcome == "Major depressive disorder")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Manic/hyper symptoms: I needed less sleep than usual")] <- "Sleep disorders"
data$outcome_formatted[which(data$outcome == "Maternal smoking around birth")] <- "Parental smoking"
data$outcome_group[which(data$outcome == "Maternal smoking around birth")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Miserableness")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Mood swings")] <- "Mood and feelings"
data$outcome_group[which(data$outcome == "Morning/evening person (chronotype)")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Morning/evening person (chronotype)")] <- "Sleep"
data$outcome_group[which(data$outcome == "Nap during day")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Nap during day")] <- "Sleep"
data$outcome_formatted[which(data$outcome == "Neo-neuroticism")] <- "Personality trait"
data$outcome_formatted[which(data$outcome == "Nervous feelings")] <- "Mood and feelings"
data$outcome_group[which(data$outcome == "Neuroblastoma")] <- "Cancer"
data$outcome_formatted[which(data$outcome == "Neuroblastoma")] <- "Neuroblastoma"
data$outcome[which(data$outcome == "Neuroticism (NEU)")] <- "Neuroticism"
data$outcome_formatted[which(data$outcome == "Neuroticism")] <- "Personality trait"
data$outcome_formatted[which(data$outcome == "Neuroticism score")] <- "Personality trait"
data$outcome[which(data$outcome == "Non-cancer illness code  self-reported: anxiety/panic attacks")] <- "Non-cancer illness code, self-reported: anxiety/panic attacks"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: anxiety/panic attacks")] <- "Mental health disorders"
data$outcome[which(data$outcome == "Non-cancer illness code  self-reported: depression")] <- "Non-cancer illness code, self-reported: depression"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: depression")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code  self-reported: sleep apnoea")] <- "Sleep disorders"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: sleep apnoea")] <- "Sleep disorders"
data$outcome_group[which(data$outcome == "Non-cancer illness code, self-reported: alcohol dependency")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: alcohol dependency")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Non-cancer illness code, self-reported: alcoholic liver disease / alcoholic cirrhosis")] <- "Liver"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: alcoholic liver disease / alcoholic cirrhosis")] <- "Alcoholic liver disease"
data$outcome_group[which(data$outcome == "Non-cancer illness code, self-reported: benign neuroma")] <- "Nervous system"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: benign neuroma")] <- "Neuroma"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: chronic/degenerative neurological problem")] <- "Neurodegenerative diseases"
data$outcome_group[which(data$outcome == "Non-cancer illness code, self-reported: diabetic neuropathy/ulcers")] <- "Nervous system"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: diabetic neuropathy/ulcers")] <- "Neuropathy"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: epilepsy")] <- "Epilepsy"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: headaches (not migraine)")] <- "Brain disease"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: insomnia")] <- "Sleep disorders"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: migraine")] <- "Brain disease"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: motor neurone disease")] <- "Neurodegenerative diseases"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: neurological injury/trauma")] <- "Brain disease"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: obsessive compulsive disorder (ocd)")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: other neurological problem")] <- "Brain disease"
data$outcome_group[which(data$outcome == "Non-cancer illness code, self-reported: peripheral neuropathy")] <- "Nervous system"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: peripheral neuropathy")] <- "Neuropathy"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: post-natal depression")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: psychological/psychiatric problem")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Non-cancer illness code, self-reported: schizophrenia")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Nucleus accumbens volume")] <- "Brain health"
data$outcome_group[which(data$outcome == "Number of cigarettes currently smoked daily (current cigarette smokers)")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Number of cigarettes currently smoked daily (current cigarette smokers)")] <- "Smoking"
data$outcome_group[which(data$outcome == "Number of cigarettes previously smoked daily")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Number of cigarettes previously smoked daily")] <- "Smoking"
data$outcome_formatted[which(data$outcome == "Number of depression episodes")] <- "Mental health disorders"
data$outcome_group[which(data$outcome == "Number of unsuccessful stop-smoking attempts")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Number of unsuccessful stop-smoking attempts")] <- "Smoking"
data$outcome[which(data$outcome == "Obsessive Compulsive Disorder")] <- "Obsessive compulsive disorder"
data$outcome_group[grep("Operative procedures", data$outcome)] <- "Medical procedure"
data$outcome_formatted[grep("Operative procedures", data$outcome)] <- "Operative procedure"
data$outcome_group[which(data$outcome == "Other alcohol intake")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Other alcohol intake")] <- "Alcohol"
data$outcome_group[which(data$outcome == "Other non-alcoholic drinks")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Other non-alcoholic drinks")] <- "Alcohol"
data$outcome_formatted[which(data$outcome == "PGC cross-disorder traits")] <- "Mental health disorders"
data$outcome[which(data$outcome == "Pack years adult smoking as proportion of life span exposed to smoking PREVIEW ONLY")] <- "Pack years adult smoking as proportion of life span exposed to smoking"
data$outcome_group[which(data$outcome == "Pack years adult smoking as proportion of life span exposed to smoking")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Pack years adult smoking as proportion of life span exposed to smoking")] <- "Smoking"
data$outcome[which(data$outcome == "Pack years of smoking PREVIEW ONLY")] <- "Pack years of smoking"
data$outcome_group[which(data$outcome == "Pack years of smoking")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Pack years of smoking")] <- "Smoking"
data$outcome_formatted[which(data$outcome == "Pallidum volume")] <- "Brain health"
data$outcome_formatted[which(data$outcome == "Parkinson's disease")] <- "Mental health disorders"
data$outcome_group[which(data$outcome == "Past tobacco smoking")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Past tobacco smoking")] <- "Smoking"
data$outcome_formatted[which(data$outcome == "Putamen volume")] <- "Brain health"
data$outcome_group[grep("glasses/contact lenses", data$outcome)] <- "Eyes"
data$outcome_formatted[grep("glasses/contact lenses", data$outcome)] <- "Eye health"
data$outcome[which(data$outcome == "Reason for glasses/contact lenses: For short-sightedness  i.e. only or mainly for distance viewing such as driving  cinema etc (called 'myopia')")] <- "Reason for glasses/contact lenses: For short-sightedness, i.e. only or mainly for distance viewing such as driving, cinema etc (called 'myopia')"
data$outcome_formatted[which(data$outcome == "Reason for glasses/contact lenses: For short-sightedness, i.e. only or mainly for distance viewing such as driving, cinema etc (called 'myopia')")]  <- "Eye health"
data$outcome_group[which(data$outcome == "Reason for glasses/contact lenses: For short-sightedness, i.e. only or mainly for distance viewing such as driving, cinema etc (called 'myopia')")] <- "Eyes"
data$outcome_group[which(data$outcome == "Reason for reducing amount of alcohol drunk: Health precaution")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Reason for reducing amount of alcohol drunk: Health precaution")] <- "Alcohol"
data$outcome_formatted[which(data$outcome == "Risk taking")] <- "Personality trait"
data$outcome_formatted[which(data$outcome == "Seen a psychiatrist for nerves, anxiety, tension or depression")] <- "Mental health disorders"
data$outcome[which(data$outcome == "Seen a psychiatrist for nerves  anxiety  tension or depression")] <- "Seen a psychiatrist for nerves, anxiety, tension or depression"
data$outcome_formatted[which(data$outcome == "Seen doctor (GP) for nerves, anxiety, tension or depression")] <- "Mental health disorders"
data$outcome[which(data$outcome == "Seen doctor (GP) for nerves  anxiety  tension or depression")] <- "Seen doctor (GP) for nerves, anxiety, tension or depression"
data$outcome_formatted[which(data$outcome == "Sensitivity / hurt feelings")] <- "Mood and feelings"
data$outcome_group[which(data$outcome == "Sleep duration")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Sleep duration")] <- "Sleep"
data$outcome_formatted[which(data$outcome == "Sleeplessness / insomnia")] <- "Sleep disorders"
data$outcome_group[which(data$outcome == "Smoked cigarette or pipe within last hour")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Smoked cigarette or pipe within last hour")] <- "Smoking"
data$outcome_group[which(data$outcome == "Smoking compared to 10 years previous")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Smoking compared to 10 years previous")] <- "Smoking"
data$outcome_group[which(data$outcome == "Smoking status: Current")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Smoking status: Current")] <- "Smoking"
data$outcome_group[which(data$outcome == "Smoking status: Previous")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Smoking status: Previous")] <- "Smoking"
data$outcome_group[which(data$outcome == "Smoking/smokers in household")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Smoking/smokers in household")] <- "Smoking"
data$outcome_group[which(data$outcome == "Snoring")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Snoring")] <- "Sleep"
data$outcome_formatted[which(data$outcome == "Sources of admission to hospital (recoded): High security psychiatric hospital: NHS")] <- "Mental health hospitalization"
data$outcome_formatted[which(data$outcome == "Sources of admission to hospital (recoded): Transfer to other NHS provider: psychiatry, learning disabilities")] <- "Mental health hospitalization"
data$outcome_formatted[which(data$outcome == "Suffer from 'nerves'")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Suicide attempt")] <- "Mental health disorders"
data$outcome_formatted[which(data$outcome == "Tense / 'highly strung'")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Thalamus volume")] <- "Brain health"
data$outcome_group[which(data$outcome == "Time spent driving")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Time spent driving")] <- "Travel"
data$outcome_group[which(data$outcome == "Tobacco smoking: Ex-smoker")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Tobacco smoking: Ex-smoker")] <- "Smoking"
data$outcome_formatted[grep("Treatment speciality of consultant", data$outcome)] <- "Mental health hospitalization"
data$outcome_group[grep("Treatment/medication code", data$outcome)] <- "Drugs"
data$outcome_formatted[which(data$outcome == "Treatment/medication code: neurontin 100mg capsule")] <- "Anticonvulsant"
data$outcome_formatted[which(data$outcome == "Treatment/medication code: polyvinyl alcohol 1% eye drops")] <- "Eye medication"
data$outcome_group[which(data$outcome == "Type of tobacco previously smoked: Cigars or pipes")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Type of tobacco previously smoked: Cigars or pipes")] <- "Smoking"
data$outcome_group[which(data$outcome == "Wants to stop smoking")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Wants to stop smoking")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why reduced smoking: Doctor's advice")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why reduced smoking: Doctor's advice")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why reduced smoking: Financial reasons")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why reduced smoking: Financial reasons")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why reduced smoking: Health precaution")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why reduced smoking: Health precaution")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why reduced smoking: Illness or ill health")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why reduced smoking: Illness or ill health")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why reduced smoking: None of the above")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why reduced smoking: None of the above")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why stopped smoking: Doctor's advice")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why stopped smoking: Doctor's advice")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why stopped smoking: Financial reasons")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why stopped smoking: Financial reasons")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why stopped smoking: Health precaution")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why stopped smoking: Health precaution")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why stopped smoking: Illness or ill health")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why stopped smoking: Illness or ill health")] <- "Smoking"
data$outcome_group[which(data$outcome == "Why stopped smoking: None of the above")] <- "Behaviour"
data$outcome_formatted[which(data$outcome == "Why stopped smoking: None of the above")] <- "Smoking"
data$outcome_formatted[which(data$outcome == "Worrier / anxious feelings")] <- "Mood and feelings"
data$outcome_formatted[which(data$outcome == "Worry too long after embarrassment")] <- "Mood and feelings"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Brain",]
table(data$outcome_group)

## Cancer ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Cancer",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Cancer"])

# Change
data$outcome[which(data$outcome == "Esophageal_cancer")] <- "Oesophageal cancer"
data$outcome_formatted[which(data$outcome == "Esophageal cancer")] <- "Oesophageal cancer"
data$outcome[which(data$outcome == "Gastric_cancer")] <- "Gastric cancer"
data$outcome_formatted[which(data$outcome == "Gastric cancer")] <- "Gastric cancer"
data$outcome[which(data$outcome == "hepatocellular_carcinoma")] <- "Hepatocellular carcinoma"
data$outcome_formatted[which(data$outcome == "Hepatocellular carcinoma")] <- "Liver cancer"
data$outcome[which(data$outcome == "Cervical_cancer")] <- "Cervical cancer"
data$outcome_formatted[which(data$outcome == "Cervical cancer")] <- "Cervical cancer"
data$outcome[which(data$outcome == "CRC")] <- "Colorectal cancer"
data$outcome_formatted[which(data$outcome == "CRC")] <- "Colorectal cancer"
data$outcome[grep("Colon cancer", data$outcome)] <- "Colon cancer"
data$outcome[grep("Distal colon cancer", data$outcome)] <- "Distal colon cancer"
data$outcome[grep("Proximal colon cancer", data$outcome)] <- "Proximal colon cancer"
data$outcome[grep("Rectal cancer", data$outcome)] <- "Rectal cancer"
data$outcome[which(data$outcome == "hepatocellular carcinoma")] <- "Hepatocellular carcinoma"
data$outcome_formatted[which(data$outcome == "Neuroblastoma")] <- "Neural cancer"
data$outcome[which(data$outcome == "Pancreatic_cancer")] <- "Pancreatic cancer"
data$outcome[which(data$outcome == "Prostate_cancer")] <- "Prostate cancer"
data$outcome_formatted[which(data$outcome == "Lung adenocarcinoma")] <- "Lung cancer"
data$outcome_formatted[which(data$outcome == "Lung cancer")] <- "Lung cancer"
data$outcome_formatted[which(data$outcome == "Squamous cell lung cancer")] <- "Lung cancer"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Cancer",]
table(data$outcome_group)

## Cardiovascular ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Cardiovascular",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Cardiovascular"])

# Change
data$outcome_formatted[which(data$outcome_formatted == "Heart")] <- "Heart health"
data$outcome_formatted[which(data$outcome_formatted == "Heart disease")] <- "Cardiovascular disease"
data$outcome_formatted[which(data$outcome == "Diagnoses - secondary ICD10: I42.6 Alcoholic cardiomyopathy")] <- "Cardiovascular disease"
data$outcome_formatted[which(data$outcome == "Activated partial thromboplastin time")] <- "Blood clotting"
data$outcome[which(data$outcome == "DBP")] <- "Diastolic blood pressure"
data$outcome_formatted[which(data$outcome == "Blood pressure")] <- "Blood pressure"
data$outcome_formatted[which(data$outcome == "Hypertension")] <- "Hypertension"
data$outcome[which(data$outcome == "SBP")] <- "Systolic blood pressure"
data$outcome_formatted[which(data$outcome == "Cerebral aneurysm")] <- "Aneurysm"
data$outcome[which(data$outcome == "Peripheral_artery_disease")] <- "Peripheral artery disease"
data$outcome_formatted[which(data$outcome == "Erythrocyte count")] <- "Blood cell count"
data$outcome_formatted[which(data$outcome == "Reticulocyte count")] <- "Blood cell count"
data$outcome[which(data$outcome == "E/A_ratio")] <- "E/A ratio"
data$outcome[which(data$outcome == "Left_ventricular_internal_dimension_diastole")] <- "Left ventricular internal dimension diastole"
data$outcome[which(data$outcome == "Posterior_wall_thickness")] <- "Posterior wall thickness"
data$outcome[which(data$outcome == "AF")] <- "Atrial fibrillation"
data$outcome[which(data$outcome == "Ischemic Stroke")] <- "Ischemic stroke"
data$outcome_formatted[which(data$outcome == "Fractional shortening")] <- "Heart health"
data$outcome_formatted[which(data$outcome == "Pulse pressure")] <- "Blood pressure"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Cardiovascular",]
table(data$outcome_group)

## Drugs ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Drugs",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Drugs"])

# Change
data$outcome_formatted[which(data$outcome_formatted == "Drug opioid")] <- "Opioids"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Drugs",]
table(data$outcome_group)

## Eyes ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Eyes",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Eyes"])

# Change
data$outcome_formatted[which(data$outcome_formatted == "Eye disorder")] <- "Eye disease"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Eyes",]
table(data$outcome_group)

## Gastrointestinal tract ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Gastrointestinal tract",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Gastrointestinal tract"])

# No change

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Gastrointestinal tract",]
table(data$outcome_group)

## Genome ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Genome",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Genome"])

# Change
data$outcome_group[which(data$outcome_group == "Genome")] <- "Longevity"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Genome",]
table(data$outcome_group)

## Immune system ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Immune",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Immune"])

# Change
data$outcome_group[which(data$outcome_group == "Immune")] <- "Immune system"
data$outcome_formatted[which(data$outcome_formatted == "Alergy")] <- "Allergy"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Immune system",]
table(data$outcome_group)

## Inflammation ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Inflammation",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Inflammation"])

# Change
data$outcome_group[which(data$outcome == "IgA nephropathy")] <- "Kidney"
data$outcome_group[which(data$outcome == "Systemic lupus erythematosus")] <- "Autoimmunity"
data$outcome_formatted[which(data$outcome == "Systemic lupus erythematosus")] <- "Autoimmune disease"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Inflammation",]
table(data$outcome_group)

## Kidney ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Kidney",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Kidney"])

# Change
data$outcome_formatted[which(data$outcome_formatted == "Kidney Biomarkers")] <- "Kidney biomarkers"
data$outcome[which(data$outcome == "eGFR")] <- "Estimated glomerular filtration rate"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Kidney",]
table(data$outcome_group)

## Liver ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Liver",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Liver"])

# Change
data$outcome[which(data$outcome == "Chronic_hepatitis")] <- "Chronic hepatitis"
data$outcome_formatted[grep("Diagnoses - main ICD10: K70", data$outcome)] <- "Alcoholic liver disease"
data$outcome_formatted[grep("Diagnoses - secondary ICD10: K70", data$outcome)] <- "Alcoholic liver disease"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Liver",]
table(data$outcome_group)

## Longevity ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Longevity",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Longevity"])

# Change
data$outcome_formatted[which(data$outcome == "Parental extreme longevity (95 years and older)")] <- "Parental longevity"
data$outcome_formatted[which(data$outcome == "Parental lifespan")] <- "Parental longevity"
data$outcome_formatted[which(data$outcome == "Parental longevity (both parents in top 10%)")] <- "Parental longevity"
data$outcome_formatted[which(data$outcome == "Parental longevity (combined parental age at death)")] <- "Parental longevity"
data$outcome_formatted[which(data$outcome == "Parental longevity (combined parental attained age, Martingale residuals)")] <- "Parental longevity"
data$outcome_formatted[which(data$outcome == "Parental longevity (father's age at death)")] <- "Parental longevity"
data$outcome_formatted[which(data$outcome == "Parental longevity (father's attained age)")] <- "Parental longevity"
data$outcome_formatted[which(data$outcome == "Parental longevity (mother's age at death)")] <- "Parental longevity"
data$outcome_formatted[which(data$outcome == "Parental longevity (mother's attained age)")] <- "Parental longevity"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Longevity",]
table(data$outcome_group)

## Lung ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Lung",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Lung"])

# Change
data$outcome_formatted[which(data$outcome == "COPD")] <- "Lung disease"
data$outcome_group[which(data$outcome == "Lung cancer")] <- "Cancer"
data$outcome[which(data$outcome == "COPD")] <- "Chronic obstructive pulmonary disease"
data$outcome_formatted[which(data$outcome == "Chronic obstructive pulmonary disease")] <- "Lung disease"
data$outcome_group[which(data$outcome == "Lung adenocarcinoma")] <- "Cancer"
data$outcome_group[which(data$outcome == "Lung cancer")] <- "Cancer"
data$outcome[which(data$outcome == "Pulmonary_tuberculosis")] <- "Pulmonary tuberculosis"
data$outcome_group[which(data$outcome == "Squamous cell lung cancer")] <- "Cancer"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Lung",]
table(data$outcome_group)

## Medical procedures ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Medical procedure",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Medical procedure"])

# No change

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Medical procedure",]
table(data$outcome_group)

## Metabolic health ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Metabolic health",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Metabolic health"])

# Change
data$outcome[which(data$outcome == "BMI")] <- "Body mass index"
data$outcome_formatted[which(data$outcome == "Body mass index")] <- "Adiposity measure"
data$outcome[which(data$outcome == "Body fat %")] <- "Body fat percentage"
data$outcome_formatted[which(data$outcome == "Body fat percentage")] <- "Adiposity measure"
data$outcome_formatted[which(data$outcome == "Childhood obesity")] <- "Obesity"
data$outcome[which(data$outcome == "Diabetes - presumably Type 2 diabetes")] <- "Diabetes"
data$outcome_formatted[which(data$outcome == "Diabetes")] <- "Diabetes"
data$outcome_group[which(data$outcome == "Diabetic retinopathy")] <- "Eyes"
data$outcome_formatted[which(data$outcome == "Diabetic retinopathy")] <- "Eye disease"
data$outcome_formatted[which(data$outcome == "Extreme body mass index")] <- "Adiposity measure"
data$outcome_formatted[which(data$outcome == "Extreme waist-to-hip ratio")] <- "Adiposity measure"
data$outcome[which(data$outcome == "Extreme waist-to-hip ratio")] <- "Extreme waist-hip ratio"
data$outcome_formatted[which(data$outcome == "Grip strength")] <- "Strength"
data$outcome_formatted[which(data$outcome == "Hip circumference")] <- "Adiposity measure"
data$outcome_formatted[which(data$outcome == "Obesity")] <- "Obesity"
data$outcome_formatted[which(data$outcome == "Obesity class 1")] <- "Obesity"
data$outcome_formatted[which(data$outcome == "Obesity class 2")] <- "Obesity"
data$outcome_formatted[which(data$outcome == "Obesity class 3")] <- "Obesity"
data$outcome_formatted[which(data$outcome == "Overweight")] <- "Overweight"
data$outcome[which(data$outcome == "SAT (possibly subcutaneous adipose tissue?)")] <- "SAT"
data$outcome_formatted[which(data$outcome == "SAT")] <- "Adiposity measure"
data$outcome[which(data$outcome == "T2DM")] <- "Type 2 diabetes"
data$outcome_formatted[which(data$outcome == "Type 2 diabetes")] <- "Diabetes"
data$outcome_formatted[which(data$outcome == "Trunk fat mass (TFM)")] <- "Adiposity measure"
data$outcome[which(data$outcome == "VAT (possibly visceral adipose tissue?)")] <- "VAT"
data$outcome_formatted[which(data$outcome == "VAT")] <- "Adiposity measure"
data$outcome[which(data$outcome == "WHR")] <- "Waist-hip ratio"
data$outcome_formatted[which(data$outcome == "Waist-hip ratio")] <- "Adiposity measure"
data$outcome[which(data$outcome == "WHRadjBMI")] <- "Waist-hip ratio adjusted for BMI"
data$outcome_formatted[which(data$outcome == "Waist-hip ratio adjusted for BMI")] <- "Adiposity measure"
data$outcome_formatted[which(data$outcome == "Waist circumference")] <- "Adiposity measure"
data$outcome[which(data$outcome == "Waist hip ratio")] <- "Waist-hip ratio"
data$outcome[which(data$outcome == "Waist-to-hip ratio")] <- "Waist-hip ratio"
data$outcome_formatted[which(data$outcome == "Waist-hip ratio")] <- "Adiposity measure"
data$outcome_formatted[which(data$outcome == "Appendicular lean mass")] <- "Adiposity measure"
data$outcome[which(data$outcome == '"metabolic traits"')] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Blood sugar")] <- "Metabolites"
data$outcome[which(data$outcome == "HDL")] <- "HDL cholesterol"
data$outcome[which(data$outcome == "HDL-C")] <- "HDL cholesterol"
data$outcome[which(data$outcome == "HbA1C1")] <- "HbA1c"
data$outcome[which(data$outcome == "Hemoglobin A1c")] <- "HbA1c"
data$outcome[which(data$outcome == "High-density-lipoprotein_cholesterol")] <- "HDL cholesterol"
data$outcome[which(data$outcome == "LDL")] <- "LDL cholesterol"
data$outcome[which(data$outcome == "LDL-C")] <- "LDL cholesterol"
data$outcome[which(data$outcome == "Low-density-lipoprotein_cholesterol")] <- "LDL cholesterol"
data$outcome[which(data$outcome == "TC")] <- "Total cholesterol"
data$outcome[which(data$outcome == "TG")] <- "Triglycerides"
data$outcome[which(data$outcome == "Triglyceride")] <- "Triglycerides"
data$outcome[which(data$outcome == "triglyceride")] <- "Triglycerides"
data$outcome[which(data$outcome == "selenium")] <- "Selenium"
data$outcome[which(data$outcome == "serum uric acid")] <- "Uric acid"
data$outcome[which(data$outcome == "progesterone")] <- "Progesterone"
data$outcome_formatted[which(data$outcome_formatted == "Metabolic traits")] <- "Metabolic health"
data$outcome[which(data$outcome == "Total_protein")] <- "Total protein"
data$outcome[which(data$outcome == "Zinc_sulfate_turbidity_test")] <- "Zinc sulfate turbidity test"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Metabolic health",]
table(data$outcome_group)

## Medical procedures ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Medical procedure",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Medical procedure"])

# No change

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Medical procedure",]
table(data$outcome_group)

## Mouth ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Mouth",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Mouth"])

# Change
data$outcome[which(data$outcome == "Periodontal_disease")] <- "Periodontal disease"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Mouth",]
table(data$outcome_group)

## Those helpfully labelled as "NA" ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "NA",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "NA"])
table(data$outcome[which(data$ID == "914635920")])

# Change
# In the MR section of the paper, authors only report abbrevation definitions for the AD, AF, ALS, CAD, CKD, CRC, eGFR, PCa, PD, RA and T2DM
# They also say in their results that they looked at creatinine but this doesn't seem to be abbreviated anywhere but presume it's sCr
# They also never define BMI but presume it's body mass index
# In the other sections of the paper, they also define DBP, FBS, HbA1c, HDL-C, LDL-C, SBP, TC and TG so have defined those
# Searched in BioBank Japan PheWeb (https://pheweb.jp/) for the others and was only able to define a few of them
# The rest I have left as "NA" for both outcome_group and outcome_formatted:
# Specifically, I couldn't conclusively define APTT, BS, CK, Cl, EA, EF, Fbg, FS, IVS, K, LDH, LVDd, LVDs, LVM, LVMI, Na, PT, PW, RWT and ZTT on PheWeb (even though some of them are possibly identifiable)

data$outcome[which(data$outcome == "AG")] <- "Albumin/globulin ratio" # Based on PheWeb
data$outcome_group[which(data$outcome == "Albumin/globulin ratio")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Albumin/globulin ratio")] <- "Metabolites"
data$outcome[which(data$outcome == "Alb")] <- "Albumin" # Based on PheWeb 
data$outcome_group[which(data$outcome == "Albumin")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Albumin")] <- "Metabolites"
data$outcome[which(data$outcome == "ALP")] <- "Alkaline phosphatase" # Based on PheWeb
data$outcome_group[which(data$outcome == "Alkaline phosphatase")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Alkaline phosphatase")] <- "Metabolites"
data$outcome[which(data$outcome == "ALT")] <- "Alanine aminotransferase" # Based on PheWeb
data$outcome_group[which(data$outcome == "Alanine aminotransferase")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Alanine aminotransferase")] <- "Metabolites"
data$outcome[which(data$outcome == "AST")] <- "Aspartate transaminase" # Based on PheWeb
data$outcome_group[which(data$outcome == "Aspartate transaminase")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Aspartate transaminase")] <- "Metabolites"
data$outcome[which(data$outcome == "Baso")] <- "Basophil count" # Based on PheWeb
data$outcome_group[which(data$outcome == "Basophil count")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Basophil count")] <- "Blood cell count"
data$outcome[which(data$outcome == "BUN")] <- "Blood urea nitrogen" # Based on PheWeb
data$outcome_group[which(data$outcome == "Blood urea nitrogen")] <- "Kidney"
data$outcome_formatted[which(data$outcome == "Blood urea nitrogen")] <- "Kidney biomarkers"
data$outcome[which(data$outcome == "Ca")] <- "Calcium" # Based on PheWeb
data$outcome_group[which(data$outcome == "Calcium")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Calcium")] <- "Metabolites"
data$outcome[which(data$outcome == "CRP")] <- "C-reactive protein" # Based on PheWeb
data$outcome_group[which(data$outcome == "C-reactive protein")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "C-reactive protein")] <- "Metabolites"
data$outcome[which(data$outcome == "Eosino")] <- "Eosinophil count" # Based on PheWeb
data$outcome_group[which(data$outcome == "Eosinophil count")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Eosinophil count")] <- "Blood cell count"
data$outcome[which(data$outcome == "GGT")] <- "Gamma-glutamyl transpeptidase" # Based on PheWeb
data$outcome_group[which(data$outcome == "Gamma-glutamyl transpeptidase")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Gamma-glutamyl transpeptidase")] <- "Metabolites"
data$outcome[which(data$outcome == "Hb")] <- "Hemaglobin" # Based on PheWeb
data$outcome_group[which(data$outcome == "Hemaglobin")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Hemaglobin")] <- "Red blood cell"
data$outcome[which(data$outcome == "Ht")] <- "Hematocrit" # Based on PheWeb
data$outcome_group[which(data$outcome == "Hematocrit")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Hematocrit")] <- "Red blood cell"
data$outcome[which(data$outcome == "Lym")] <- "Lymphocyte count" # Based on PheWeb
data$outcome_group[which(data$outcome == "Lymphocyte count")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Lymphocyte count")] <- "Blood cell count"
data$outcome[which(data$outcome == "MAP")] <- "Mean arterial pressure" # Based on PheWeb
data$outcome_group[which(data$outcome == "Mean arterial pressure")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Mean arterial pressure")] <- "Blood pressure"
data$outcome[which(data$outcome == "MCH")] <- "Mean corpuscular hemoglobin" # Based on PheWeb
data$outcome_group[which(data$outcome == "Mean corpuscular hemoglobin")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Mean corpuscular hemoglobin")] <- "Red blood cell"
data$outcome[which(data$outcome == "MCHC")] <- "Mean corpuscular hemoglobin concentration" # Based on PheWeb
data$outcome_group[which(data$outcome == "Mean corpuscular hemoglobin concentration")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Mean corpuscular hemoglobin concentration")] <- "Red blood cell"
data$outcome[which(data$outcome == "MCV")] <- "Mean corpuscular volume" # Based on PheWeb
data$outcome_group[which(data$outcome == "Mean corpuscular volume")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Mean corpuscular volume")] <- "Red blood cell"
data$outcome[which(data$outcome == "NAP")] <- "Non-albumin protein" # Based on PheWeb
data$outcome_group[which(data$outcome == "Non-albumin protein")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Non-albumin protein")] <- "Metabolites"
data$outcome[which(data$outcome == "Neutro")] <- "Neutrophil count" # Based on PheWeb
data$outcome_group[which(data$outcome == "Neutrophil count")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Neutrophil count")] <- "Blood cell count"
data$outcome[which(data$outcome == "PP")] <- "Pulse pressure" # Based on PheWeb
data$outcome_group[which(data$outcome == "Pulse pressure")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Pulse pressure")] <- "Blood pressure"
data$outcome[which(data$outcome == "Plt")] <- "Platelet count" # Based on PheWeb
data$outcome_group[which(data$outcome == "Platelet count")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Platelet count")] <- "Blood clotting"
data$outcome[which(data$outcome == "RBC")] <- "Red blood cell count" # Based on PheWeb
data$outcome_group[which(data$outcome == "Red blood cell count")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Red blood cell count")] <- "Blood cell count"
data$outcome[which(data$outcome == "TBil")] <- "Total bilirubin" # Based on PheWeb
data$outcome_group[which(data$outcome == "Total bilirubin")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "Total bilirubin")] <- "Red blood cell"
data$outcome[which(data$outcome == "TP")] <- "Total protein" # Based on PheWeb
data$outcome_group[which(data$outcome == "Total protein")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Total protein")] <- "Metabolites"
data$outcome[which(data$outcome == "UA")] <- "Uric acid" # Based on PheWeb
data$outcome_group[which(data$outcome == "Uric acid")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Uric acid")] <- "Metabolites"
data$outcome[which(data$outcome == "WBC")] <- "White blood cell count" # Based on PheWeb
data$outcome_group[which(data$outcome == "White blood cell count")] <- "Cardiovascular"
data$outcome_formatted[which(data$outcome == "White blood cell count")] <- "Blood cell count"
data$outcome[which(data$outcome == "sCr")] <- "Serum creatinine" # Based on PheWeb
data$outcome_group[which(data$outcome == "Serum creatinine")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome == "Serum creatinine")] <- "Metabolites"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "NA",]
table(data$outcome_group)

## Nervous system ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Nervous system",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Nervous system"])

# Change
data$outcome_formatted[which(data$outcome == "Hirschsprung's disease")] <- "Hirschsprung's disease"
data$outcome_group[which(data$outcome == "Hirschsprung's disease")] <- "Gastrointestinal tract"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Nervous system",]
table(data$outcome_group)

## Nutrition ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Nutrition",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Nutrition"])

# Change
data$outcome[which(data$outcome == "Bread type Brown")] <- "Bread type: Brown"
data$outcome[which(data$outcome == "Bread type White")] <- "Bread type: White"
data$outcome[which(data$outcome == "Bread type Wholemeal or wholegrain")] <- "Bread type: Wholemeal or wholegrain"
data$outcome[which(data$outcome == "Cereal type Biscuit cereal")] <- "Cereal type: Biscuit cereal"
data$outcome[which(data$outcome == "Cereal type Bran cereal")] <- "Cereal type: Bran cereal"
data$outcome[which(data$outcome == "Cereal type Muesli")] <- "Cereal type: Muesli"
data$outcome[which(data$outcome == "Cereal type Oat cereal")] <- "Cereal type: Oat cereal"
data$outcome[which(data$outcome == "Coffee type Ground coffee")] <- "Coffee type: Ground coffee"
data$outcome[which(data$outcome == "Coffee type Instant coffee")] <- "Coffee type: Instant coffee"
data$outcome[which(data$outcome == "Coffee consumed")] <- "Coffee intake"
data$outcome[which(data$outcome == "Coffee type: Ground coffee (include espresso, filter etc)")] <- "Coffee type: Ground coffee"
data$outcome[which(data$outcome == "Milk type used Full cream")] <- "Milk type: Full cream"
data$outcome[which(data$outcome == "Milk type used Semi-skimmed")] <- "Milk type: Semi-skimmed"
data$outcome[which(data$outcome == "Milk type used Skimmed")] <- "Milk type: Skimmed"
data$outcome[which(data$outcome == "Milk type used Soya")] <- "Milk type: Soy"
data$outcome[which(data$outcome == "Never eat eggs dairy wheat sugar Eggs or foods containing eggs")] <- "Never eat eggs or foods containing eggs"
data$outcome[which(data$outcome == "Never eat eggs dairy wheat sugar I eat all of the above")] <- "Eat eggs dairy wheat and sugar"
data$outcome[which(data$outcome == "Never eat eggs dairy wheat sugar Sugar or foods drinks containing sugar")] <- "Never eat sugar or foods / drinks containing sugar"
data$outcome[which(data$outcome == "Never eat eggs dairy wheat sugar Wheat products")] <- "Never eat wheat or foods containing wheat"
data$outcome[which(data$outcome == "Non-butter spread type details Flora Pro-Active or Benecol")] <- "Non-butter spread type: Flora Pro-Active or Benecol"
data$outcome[which(data$outcome == "Non-butter spread type details Olive oil based spread")] <- "Non-butter spread type: Olive oil based spread"
data$outcome[which(data$outcome == "Non-butter spread type details Other low or reduced fat spread")] <- "Non-butter spread type: Other low or reduced fat spread"
data$outcome[which(data$outcome == "Non-butter spread type details Polyunsaturated sunflower oil based spread")] <- "Non-butter spread type: Polyunsaturated sunflower oil based spread"
data$outcome[which(data$outcome == "Non-butter spread type details Soft margarine")] <- "Non-butter spread type: Soft margarine"
data$outcome[which(data$outcome == "Salad  raw vegetable intake")] <- "Salad and/or raw vegetable intake"
data$outcome[which(data$outcome == "Spread type Butter spreadable butter")] <- "Spread type: Butter / spreadable butter"
data$outcome[which(data$outcome == "all spreads vs. never")] <- "All spreads vs. never"
data$outcome[which(data$outcome == "any milk vs. never")] <- "Any milk vs. never"
data$outcome[which(data$outcome == "any oil based spread vs. never")] <- "Any oil based spread vs. never"
data$outcome[which(data$outcome == "biscuit cereal vs. any other")] <- "Cereal type: Biscuit cereal"
data$outcome[which(data$outcome == "bowls of cereal per week")] <- "Cereal intake per week"
data$outcome[which(data$outcome == "bran cereal vs. any other")] <- "Cereal type: Bran cereal"
data$outcome[which(data$outcome == "brown vs. any other")] <- "Bread type: Brown"
data$outcome[which(data$outcome == "butter + margarine spreads vs. never")] <- "Butter and margarine spreads vs. never"
data$outcome[which(data$outcome == "butter and butter-like spreads vs. oil-based spreads")] <- "Butter and butter-like spreads vs. oil-based spreads"
data$outcome[which(data$outcome == "butter and margarine spreads vs. oil-based spreads")] <- "Butter and margarine spreads vs. oil-based spreads"
data$outcome[which(data$outcome == "butter spreads vs. any other")] <- "Butter spreads"
data$outcome[which(data$outcome == "butter spreads vs. never")] <- "Butter spreads vs. never"
data$outcome[which(data$outcome == "cornflakes/frosties vs. any other")] <- "Cereal type: Cornflakes/frosties"
data$outcome[which(data$outcome == "cups of coffee per day")] <- "Coffee intake"
data$outcome[which(data$outcome == "cups of tea per day")] <- "Tea intake"
data$outcome[which(data$outcome == "dairy-based milk vs. never")] <- "Dairy-based milk vs. never"
data$outcome[which(data$outcome == "decaffeinated coffee vs. any other")] <- "Coffee type: Decaffeinated coffee"
data$outcome[which(data$outcome == "Coffee type Decaffeinated coffee")] <- "Coffee type: Decaffeinated coffee"
data$outcome[which(data$outcome == "flora + benecol spreads vs. any other")] <- "Non-butter spread type: Flora Pro-Active or Benecol"
data$outcome[which(data$outcome == "flora + benecol spreads vs. never")] <- "Flora Pro-Active or Benecol vs. never"
data$outcome[which(data$outcome == "full cream milk vs. any other")] <- "Milk type: Full cream"
data$outcome[which(data$outcome == "full cream milk vs. never")] <- "Full cream milk vs. never"
data$outcome[which(data$outcome == "glasses of water per day")] <- "Water intake"
data$outcome[which(data$outcome == "ground coffee vs. any other")] <- "Coffee type: Ground coffee"
data$outcome[which(data$outcome == "ground+instant coffee vs. other+decaff")] <- "Ground / instant coffee vs. other / decaf"
data$outcome[which(data$outcome == "low fat spread vs. any other")] <- "Non-butter spread type: Other low or reduced fat spread"
data$outcome[which(data$outcome == "low fat spread vs. never")] <- "Low fat spread vs. never"
data$outcome[which(data$outcome == "muesli vs. any other")] <- "Cereal type: Muesli"
data$outcome[which(data$outcome == "never eat dairy vs. no EDWS restrictions")] <- "Never eat dairy or foods containing dairy"
data$outcome[which(data$outcome == "never eat dairy vs. no dairy restrictions")] <- "Never eat dairy vs. no dairy restrictions"
data$outcome[which(data$outcome == "never eat eggs vs. no  EDWS restrictions")] <- "Never eat eggs or foods containing eggs"
data$outcome[which(data$outcome == "never eat eggs vs. no eggs restrictions")] <- "Never eat eggs vs. no eggs restrictions"
data$outcome[which(data$outcome == "never eat sugar vs. no EDWS restrictions")] <- "Never eat sugar or foods / drinks containing sugar"
data$outcome[which(data$outcome == "never eat sugar vs. no sugar restrictions")] <- "Never eat sugar vs. no sugar restrictions"
data$outcome[which(data$outcome == "never eat wheat vs. no  EDWS restrictions")] <- "Never eat wheat or foods containing wheat"
data$outcome[which(data$outcome == "never eat wheat vs. no wheat restrictions")] <- "Never eat wheat vs. no wheat restrictions"
data$outcome[which(data$outcome == "oat cereal vs. any other")] <- "Cereal type: Oat cereal"
data$outcome[which(data$outcome == "olive oil spread vs. any other")] <- "Non-butter spread type: Olive oil based spread"
data$outcome[which(data$outcome == "olive oil spread vs. never")] <- "Olive oil spread vs. never"
data$outcome[which(data$outcome == "overall beef intake")] <- "Beef intake"
data$outcome[which(data$outcome == "overall cheese intake")] <- "Cheese intake"
data$outcome[which(data$outcome == "overall lamb/mutton intake")] <- "Lamb / mutton intake"
data$outcome[which(data$outcome == "overall non-oily fish intake")] <- "Non-oily fish intake"
data$outcome[which(data$outcome == "overall oily fish intake")] <- "Oily fish intake"
data$outcome[which(data$outcome == "overall pork intake")] <- "Pork intake"
data$outcome[which(data$outcome == "overall poultry intake")] <- "Poultry intake"
data$outcome[which(data$outcome == "overall processed meat intake")] <- "Processed meat intake"
data$outcome[which(data$outcome == "pieces of dried fruit per day")] <- "Dried fruit intake"
data$outcome[which(data$outcome == "pieces of fresh fruit per day")] <- "Fresh fruit intake"
data$outcome[which(data$outcome == "semi-skimmed milk vs. any other")] <- "Milk type: Semi-skimmed"
data$outcome[which(data$outcome == "semi-skimmed milk vs. never")] <- "Semi-skimmed milk vs. never"
data$outcome[which(data$outcome == "skimmed milk vs. any other")] <- "Milk type: Skimmed"
data$outcome[which(data$outcome == "skimmed milk vs. never")] <- "Skimmed milk vs. never"
data$outcome[which(data$outcome == "skimmed, semi-skimmed, full cream (QT)")] <- "Skimmed, semi-skimmed, full cream (QT)"
data$outcome[which(data$outcome == "slices of bread per week")] <- "Slices of bread per week"
data$outcome[which(data$outcome == "soy milk vs. any other")] <- "Milk type: Soy"
data$outcome[which(data$outcome == "soy milk vs. never")] <- "Soy milk vs. never"
data$outcome[which(data$outcome == "tablespoons of cooked vegetables per day")] <- "Cooked vegetable intake"
data$outcome[which(data$outcome == "tablespoons of raw vegetables per day")] <- "Salad and/or raw vegetable intake"
data$outcome[which(data$outcome == "tub margarine spreads vs. any other")] <- "Non-butter spread type: Soft margarine"
data$outcome[which(data$outcome == "tub margarine spreads vs. never")] <- "Soft margarine spreads vs. never"
data$outcome[which(data$outcome == "white vs. any other")] <- "Bread type: White"
data$outcome[which(data$outcome == "white vs. wholemeal/wholegrain + brown")] <- "White bread vs. wholemeal / wholegrain / brown bread"
data$outcome[which(data$outcome == "wholemeal/whole grain vs. white + brown")] <- "Wholemeal / wholegrain vs. white / brown bread"
data$outcome[which(data$outcome == "wholemeal/wholegrain vs. any other")] <- "Bread type: Wholemeal or wholegrain"
data$outcome[which(data$outcome == "Lamb mutton intake")] <- "Lamb / mutton intake"
data$outcome[which(data$outcome == "other milk vs. any other")] <- "Milk type: Other milk"
data$outcome[which(data$outcome == "other milk vs. never")] <- "Other milk vs. never"
data$outcome[which(data$outcome == "other oil-based spread vs. any other")] <- "Non-butter spread type: Other oil-based spread"
data$outcome[which(data$outcome == "other oil-based spread vs. never")] <- "Other oil-based spread vs. never"
data$outcome[which(data$outcome == "temperature of hot drinks")] <- "Hot drink temperature"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Nutrition",]
table(data$outcome_group)

## Oesophagus ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Oesophagus",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Oesophagus"])

# Change

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Oesophagus",]
table(data$outcome_group)

## Pain ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Pain",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Pain"])

# No changes

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Pain",]
table(data$outcome_group)

## Pancreas ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Pancreas",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Pancreas"])

# No changes

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Pancreas",]
table(data$outcome_group)

## Pregnancy related disorders - putting under the umbrella of sexual and reproductive health ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Pregnancy related disorders",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Pregnancy related disorders"])

# Change
data$outcome_group[which(data$outcome_group == "Pregnancy related disorders")] <- "Sexual and reproductive health"
data$outcome_formatted[which(data$outcome_formatted == "Pre-eclampsia")] <- "Pregnancy-related disorders"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Pregnancy related disorders",] # shouldn't be any now as have changed under umbrella of sexual health
table(data$outcome_group)

## Protein - incorporate the proteins group into the metabolic health group ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Protein",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Protein"])

# Change
data$outcome_group[which(data$outcome_formatted == "Plasma protein")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome_formatted == "Plasma protein")] <- "Metabolites"
data$outcome_group[which(data$outcome_formatted == "Protein")] <- "Metabolic health"
data$outcome_formatted[which(data$outcome_formatted == "Protein")] <- "Metabolites"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Protein",] # shouldn't be any now as have changed under the umbrella of metabolites
table(data$outcome_group)

## Reproductive health - now putting under the umbrella of sexual and reproductive health ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Reproductive health",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Reproductive health"])

# Change
data$outcome_group[which(data$outcome_group == "Reproductive health")] <- "Sexual and reproductive health"
data$outcome_formatted[which(data$outcome_formatted == "Pregnancy-related disorders")] <- "Pregnancy outcomes"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Reproductive health",] # shouldn't be any now as have changed under the umbrella of sexual health
table(data$outcome_group)

## Respiratory disorders ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Respiratory disorders",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Respiratory disorders"])

# Change
data$outcome_formatted[which(data$outcome_formatted == "ARDS")] <- "Acute respiratory distress syndrome"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Respiratory disorders",]
table(data$outcome_group)

## Sexual and reproductive health ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Sexual and reproductive health",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Sexual and reproductive health"])

# Change
data$outcome_formatted[which(data$outcome == "Endometriosis")] <- "Uterus"

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Sexual and reproductive health",] 
table(data$outcome_group)

## Skin ##
# Set up
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Skin",]
table(summ_outcome_group$outcome_formatted[summ_outcome_group$outcome_group == "Skin"])

# No changes

# Check
summ_outcome_group <- data %>%
  group_by(outcome_group, outcome_formatted, outcome) %>%
  summarise() 
summ_outcome_group[summ_outcome_group$outcome_group == "Skin",]
table(data$outcome_group)

############################################################
## Creating a table to summarise all of the outcome names ##
############################################################

## Tabulated the exposure studies
table(data$outcome_study)

## Checking which have been done
done_outcome <- as.vector(unique(data$ID[!is.na(data$outcome)]))
length(done_outcome)

## Create a dataframe to check that all exposures have been formatted accordingly 
complete_outcome <- data[!is.na(data$outcome), c("ID","outcome","outcome_formatted")] ## should be 48083 rows
unique_complete_outcome <- unique(complete_outcome$outcome) 
data_unique_complete_outcome <- as.data.frame(unique_complete_outcome) 
write.xlsx(data_unique_complete_outcome, "./Data analysis/unique_outcomes.xlsx", colNames = FALSE, rowNames = FALSE, overwrite = TRUE) 

##############################################################
## Harmonizing the exposure names across and within studies ##
##############################################################
## For the purposes of knowing whether or not studies are meta-analysable, I want the simplest exposure name in the "exposure_formatted" column
## More information about the exposure (e.g., whether it's from a hurdle binary model or RNT model or just other information that was originally in the "exposure" column) can be in the "exposure_info" column

## First remove the large or redundant individual files
rm(summ_outcome_group)
rm(summ_exposure_group)
rm(data_charlie)
rm(data_kaitlin)

## Figuring out how to harmonize exposures - might be best to harmonize by microbiome data being used then across studies
colnames(data)
head(data[,c("ID","exposure","exposure_study")])
table(data$exposure_study)
data$exposure_formatted <- NA
data$exposure_info <- NA

## Bonder 2016 ##
table(data$ID[data$exposure_study == "Bonder 2016"]) # 3 studies (3032725182, 4290593439, 601575515)
data[data$exposure_study == "Bonder 2016",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Bonder 2016"])

# First study (ID = 3032725182) does not have interpretable units according to data extraction so won't be meta-analyseable either way
data$exposure_unit[data$ID == "3032725182"]
unique(data$exposure_study[data$ID == "3032725182"])
# Note that this study also uses Goodrich 2016 so will be dealing with the rest of the studies that use that one separately later
length(data$exposure[data$ID == "3032725182"])
length(unique(data$exposure[data$ID == "3032725182"]))
table(unique(data$exposure[data$ID == "3032725182"])) ## I quite like how this is presented (e.g., bacteria name (classification level) like "Bacteroidales (order)", so going to harmonize all studies like this)
# But want to change the taxonomical classifications to be capitalised
data$exposure_formatted[which(data$ID == "3032725182")] <- data$exposure[which(data$ID == "3032725182")]
data$exposure_formatted[which((data$ID == "3032725182") & grepl("\\(family\\)", data$exposure))] <- gsub("\\(family\\)", "\\(Family\\)", data$exposure[which((data$ID == "3032725182") & grepl("\\(family\\)", data$exposure))])
data$exposure_formatted[which((data$ID == "3032725182") & grepl("\\(order\\)", data$exposure))] <- gsub("\\(order\\)", "\\(Order\\)", data$exposure[which((data$ID == "3032725182") & grepl("\\(order\\)", data$exposure))])
data[which((data$ID == "3032725182")), c("ID","exposure","exposure_formatted")]
table(unique(data$exposure_formatted[data$ID == "3032725182"]))
table(unique(data$exposure_info[data$ID == "3032725182"]))
length(data$exposure_formatted[data$ID == "3032725182"])
length(unique(data$exposure_formatted[data$ID == "3032725182"]))
sum(is.na(data$exposure_formatted[data$ID == "3032725182"]))
sum(is.na(data$exposure_info[data$ID == "3032725182"]))
data[which((data$ID == "3032725182")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 4290593439) had some exposures that are relating to taxa, where they have written out the full taxonomical clade
# It also does not have interpretable units according to data extraction so won't be meta-analyseable either way but want it to be simpler
data$exposure_unit[data$ID == "4290593439"]
unique(data$exposure_study[data$ID == "4290593439"])
# Note that this study also uses several other exposure_study sources so will be dealing with the studies that use those separately later
length(data$exposure[data$ID == "4290593439"])
length(unique(data$exposure[data$ID == "4290593439"]))
table(unique(data$exposure[data$ID == "4290593439"]))
# Want to change the taxonomical classifications to be capitalised
data$exposure_formatted[which(data$ID == "4290593439")] <- data$exposure[which(data$ID == "4290593439")]
data$exposure_formatted[which((data$ID == "4290593439") & grepl("\\(family\\)", data$exposure))] <- gsub("\\(family\\)", "\\(Family\\)", data$exposure[which((data$ID == "4290593439") & grepl("\\(family\\)", data$exposure))])
data$exposure_formatted[which((data$ID == "4290593439") & grepl("\\(genus\\)", data$exposure_formatted))] <- gsub("\\(genus\\)", "\\(Genus\\)", data$exposure_formatted[which((data$ID == "4290593439") & grepl("\\(genus\\)", data$exposure_formatted))])
data$exposure_formatted[which((data$ID == "4290593439") & grepl("\\(species\\)", data$exposure_formatted))] <- gsub("\\(species\\)", "\\(Species\\)", data$exposure_formatted[which((data$ID == "4290593439") & grepl("\\(species\\)", data$exposure_formatted))])
data[which((data$ID == "4290593439")), c("ID","exposure","exposure_formatted")]
table(unique(data$exposure_formatted[data$ID == "4290593439"]))
# Now have to simplify the longer names
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Bifidobacteriaceae (Family) Bifidobacterium (Genus) NA (Species)"))] <- "Bifidobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Bifidobacteriaceae (Family) Bifidobacterium (Genus) NA (Species)"))] <- "Bifidobacterium (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) Butyricicoccus (Genus) Butyricicoccus pullicaecorum (Species)"))] <- "Clostridiaceae (Family) Butyricicoccus (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) Butyricicoccus (Genus) Butyricicoccus pullicaecorum (Species)"))] <- "Butyricicoccus pullicaecorum (Species)"
data$exposure_info[which((data$ID == "4290593439") & grepl("Clostridiaceae \\(Family\\) Clostridium \\(Genus\\)", data$exposure_formatted))] <- "Clostridiaceae (Family) Clostridium (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium celerecrescens (Species)"))] <- "Clostridium celerecrescens (Species)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium cellulolyticum (Species)"))] <- "Clostridium cellulolyticum (Species)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium IV (Species)"))] <- "Clostridium IV (Species)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium propionicum (Species)"))] <- "Clostridium propionicum (Species)"
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) NA (Species)"))] <- "Clostridiaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) NA (Species)"))] <- "Clostridium (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) NA (Genus) NA (Species)"))] <- "Clostridiaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiaceae (Family) NA (Genus) NA (Species)"))] <- "Clostridiaceae (Unclassified Genus in Family)"
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Atopobium (Genus) NA (Species)"))] <- "Coriobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Atopobium (Genus) NA (Species)"))] <- "Atopobium (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) Eggerthella sinensis (Species)"))] <- "Coriobacteriaceae (Family) Eggerthella (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) Eggerthella sinensis (Species)"))] <- "Eggerthella sinensis (Species)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) lenta (Species)"))] <- "Coriobacteriaceae (Family) Eggerthella (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) lenta (Species)"))] <- "Eggerthella lenta (Species)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) NA (Species)"))] <- "Coriobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) NA (Species)"))] <- "Eggerthella (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Gordonibacter  (Genus) Gordonibacter pamelaeae (Species)"))] <- "Coriobacteriaceae (Family) Gordonibacter (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Gordonibacter  (Genus) Gordonibacter pamelaeae (Species)"))] <- "Gordonibacter pamelaeae (Species)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Gordonibacter (Genus) NA (Species)"))] <- "Coriobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Coriobacteriaceae (Family) Gordonibacter (Genus) NA (Species)"))] <- "Gordonibacter (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Enterobacteriaceae (Family) EscherichiaShigella (Genus) NA (Species)"))] <- "Enterobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Enterobacteriaceae (Family) EscherichiaShigella (Genus) NA (Species)"))] <- "Escherichia Shigella (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Enterobacteriaceae (Family) Klebsiella  (Genus) Klebsiella variicola (Species)"))] <- "Enterobacteriaceae (Family) Klebsiella (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Enterobacteriaceae (Family) Klebsiella  (Genus) Klebsiella variicola (Species)"))] <- "Klebsiella variicola (Species)" 
data$exposure_info[which((data$ID == "4290593439") & grepl("Lachnospiraceae \\(Family\\) Blautia  \\(Genus\\)", data$exposure_formatted))] <- "Lachnospiraceae (Family) Blautia (Genus)"
data$exposure_info[which((data$ID == "4290593439") & grepl("Lachnospiraceae \\(Family\\) Blautia \\(Genus\\)", data$exposure_formatted))] <- "Lachnospiraceae (Family) Blautia (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Blautia  (Genus) Blautia stercoris (Species)"))] <- "Blautia stercoris (Species)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Blautia coccoides (Species)"))] <- "Blautia coccoides (Species)" 
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Blautiagenus (Species)"))] <- "Blautiagenus (Species)" 
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Ruminococcus obeum (Species)"))] <- "Ruminococcus obeum (Species)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Blautia  (Genus)"))] <- "Lachnospiraceae (Family) Coprococcus (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Coprococcus (Genus) Coprococcus comes (Species)"))] <- "Coprococcus comes (Species)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Coprococcus (Genus) NA (Species)"))] <- "Lachnospiraceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Coprococcus (Genus) NA (Species)"))] <- "Coprococcus (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Dorea (Genus) NA (Species)"))] <- "Lachnospiraceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Dorea (Genus) NA (Species)"))] <- "Dorea (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Lachnospira (Genus) NA (Species)"))] <- "Lachnospiraceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Lachnospira (Genus) NA (Species)"))] <- "Lachnospira (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Lachnospiraceae noname (Genus) Lachnospiraceae bacterium 1 1 57FAA (Species)"))] <- "Lachnospiraceae (Family) Lachnospiraceae noname (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Lachnospiraceae noname (Genus) Lachnospiraceae bacterium 1 1 57FAA (Species)"))] <- "Lachnospiraceae bacterium 1_1_57FAA (Species)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Murimonas (Genus) Murimonas intestini (Species)"))] <- "Lachnospiraceae (Family) Murimonas (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Murimonas (Genus) Murimonas intestini (Species)"))] <- "Murimonas intestini (Species)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Roseburia (Genus) NA (Species)"))] <- "Lachnospiraceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Lachnospiraceae (Family) Roseburia (Genus) NA (Species)"))] <- "Roseburia (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Methanobacteriaceae (Family) Methanobrevibacter (Genus) NA (Species)"))] <- "Methanobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Methanobacteriaceae (Family) Methanobrevibacter (Genus) NA (Species)"))] <- "Methanobrevibacter (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Methanobacteriaceae (Family) NA (Genus) NA (Species)"))] <- "Methanobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Methanobacteriaceae (Family) NA (Genus) NA (Species)"))] <- "Methanobacteriaceae (Unclassified Genus in Family)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Ruminococcaceae (Family) Anaerofilum (Genus) NA (Species)"))] <- "Ruminococcaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Ruminococcaceae (Family) Anaerofilum (Genus) NA (Species)"))] <- "Anaerofilum (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Ruminococcaceae (Family) Oscillospira (Genus) NA (Species)"))] <- "Ruminococcaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Ruminococcaceae (Family) Oscillospira (Genus) NA (Species)"))] <- "Oscillospira (Unclassified Species in Genus)" 
data$exposure_info[which((data$ID == "4290593439") & grepl("Ruminococcaceae \\(Family\\) Ruminococcus \\(Genus\\)", data$exposure_formatted))] <- "Ruminococcaceae (Family) Ruminococcus (Genus)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Ruminococcaceae (Family) Ruminococcus (Genus) Ruminococcus flavefaciens (Species)"))] <- "Ruminococcus flavefaciens (Species)" 
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Ruminococcaceae (Family) Ruminococcus (Genus) Ruminococcus gnavus (Species)"))] <- "Ruminococcus gnavus (Species)" 
data$exposure_info[which((data$ID == "4290593439") & (data$exposure_formatted == "Synergistaceae (Family) NA (Genus) NA (Species)"))] <- "Synergistaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Synergistaceae (Family) NA (Genus) NA (Species)"))] <- "Synergistaceae (Unclassified Genus in Family)" 
data$exposure_formatted[which((data$ID == "4290593439") & (data$exposure_formatted == "Clostridiumxylanovorans (Species)"))] <- "Clostridium xylanovorans (Species)"
table(unique(data$exposure_formatted[data$ID == "4290593439"]))
table(unique(data$exposure_info[data$ID == "4290593439"]))
length(data$exposure_formatted[data$ID == "4290593439"])
length(unique(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_info[data$ID == "4290593439"]))
data[which((data$ID == "4290593439")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Third study (ID = 601575515) uses SNPs as the exposure rather than the taxa 
# Doesn't matter either way because none of this will be meta-analyseable considering it's not clear what the units are for this analysis
data$exposure_unit[data$ID == "601575515"]
unique(data$exposure_study[data$ID == "601575515"])
length(data$exposure[data$ID == "601575515"])
length(unique(data$exposure[data$ID == "601575515"])) 
table(unique(data$exposure[data$ID == "601575515"]))
# But want to change the taxonomical classifications to be capitalised
data$exposure_formatted[which(data$ID == "601575515")] <- data$exposure[which(data$ID == "601575515")]
data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(p\\)", data$exposure))] <- gsub("\\(p\\)", "\\(Phylum\\)", data$exposure[which((data$ID == "601575515") & grepl("\\(p\\)", data$exposure))])
data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(c\\)", data$exposure_formatted))] <- gsub("\\(c\\)", "\\(Class\\)", data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(c\\)", data$exposure_formatted))])
data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(o\\)", data$exposure_formatted))] <- gsub("\\(o\\)", "\\(Order\\)", data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(o\\)", data$exposure_formatted))])
data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(f\\)", data$exposure_formatted))] <- gsub("\\(f\\)", "\\(Family\\)", data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(f\\)", data$exposure_formatted))])
data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(g\\)", data$exposure_formatted))] <- gsub("\\(g\\)", "\\(Genus\\)", data$exposure_formatted[which((data$ID == "601575515") & grepl("\\(g\\)", data$exposure_formatted))])
# Noticed a spelling mistake
data$exposure_formatted[which((data$ID == "601575515") & grepl("assoicated", data$exposure_formatted))] <- gsub("assoicated", "associated", data$exposure_formatted[which((data$ID == "601575515") & grepl("assoicated", data$exposure_formatted))])
data[which((data$ID == "601575515")), c("ID","exposure_formatted")]
## Think I'd like to make the SNP name the exposure_formatted and then all other information in the exposure_info column
data$exposure_info[which(data$ID == "601575515")] <- sub("^rs[0-9]+ - ", "", data$exposure_formatted[which(data$ID == "601575515")])
data$exposure_formatted[which(data$ID == "601575515")] <- sub(" - .*", "", data$exposure_formatted[which(data$ID == "601575515")])
table(unique(data$exposure_formatted[data$ID == "601575515"]))
length(data$exposure_formatted[data$ID == "601575515"])
length(unique(data$exposure_formatted[data$ID == "601575515"]))
sum(is.na(data$exposure_formatted[data$ID == "601575515"]))
sum(is.na(data$exposure_info[data$ID == "601575515"]))
data[which((data$ID == "601575515")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Bonder 2016 and Wang 2016 ##
table(data$ID[data$exposure_study == "Bonder 2016 and Wang 2016"]) # 1 study (950921645)
length(data$exposure[data$exposure_study == "Bonder 2016 and Wang 2016"])
length(unique(data$exposure[data$exposure_study == "Bonder 2016 and Wang 2016"]))
data[data$exposure_study == "Bonder 2016 and Wang 2016",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Bonder 2016 and Wang 2016"])

# The only study (ID = 950921645) uses SNPs from multiple studies and describes the exposure as the whole microbiome
# Not meta-analyseable anyway considering it's not clear what the units are for this analysis
data$exposure_unit[data$ID == "950921645"]
unique(data$exposure_study[data$ID == "950921645"])
table(unique(data$exposure[data$ID == "950921645"])) 
length(data$exposure[data$ID == "950921645"])
length(unique(data$exposure[data$ID == "950921645"]))
# Just want to change this to be "Gut microbiome" and then "bacterial taxa" in the exposure_formatted and "exposure_info", respectively
data$exposure_formatted[which(data$ID == "950921645")] <- "Gut microbiome"
data$exposure_info[which(data$ID == "950921645")] <- "Bacterial taxa"
unique(data$exposure_formatted[which(data$ID == "950921645")])
unique(data$exposure_info[which(data$ID == "950921645")])
length(data$exposure_formatted[data$ID == "950921645"])
length(unique(data$exposure_formatted[data$ID == "950921645"]))
sum(is.na(data$exposure_formatted[data$ID == "950921645"]))
sum(is.na(data$exposure_info[data$ID == "950921645"]))
data[which((data$ID == "950921645")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Combination of Bonder and Wang (notice that this is the same combination of studies as the one above so going to format to be the same) ##
table(data$ID[data$exposure_study == "Combination of Bonder and Wang"]) # 1 study (3836325935)
data$exposure_study[which(data$ID == "3836325935")] <- "Bonder 2016 and Wang 2016"
unique(data$exposure_study[data$ID == "3836325935"])
data$exposure_age[which(data$ID == "3836325935")] <- "18-84"
data$exposure_sex[which(data$ID == "3836325935")] <- "Both"
data$exposure_og_n[which(data$ID == "3836325935")] <- "1514 from Bonder and 1812 from Wang"
data$exposure_n_diff[which(data$ID == "3836325935")] <- "N given in paper is the total N from both Bonder and Wang combined"
data$exposure_health[which(data$ID == "3836325935")] <- "Healthy"
data$exposure_pop[which(data$ID == "3836325935")] <- "European (Netherlands) - Bonder and Europeans - Wang"
table(unique(data$exposure[data$ID == "3836325935"]))
# No need to change 
data$exposure_formatted[which(data$ID == "3836325935")] <- data$exposure[which(data$ID == "3836325935")]
unique(data$exposure_formatted[which(data$ID == "3836325935")])
length(data$exposure_formatted[data$ID == "3836325935"])
length(unique(data$exposure_formatted[data$ID == "3836325935"]))
sum(is.na(data$exposure_formatted[data$ID == "3836325935"]))
sum(is.na(data$exposure_info[data$ID == "3836325935"]))
data[which((data$ID == "3836325935")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Mixture of Bonder and Wang ##
# Notice that this is the same combination of studies as the one above so going to format to be the same
table(data$ID[data$exposure_study == "Mixture of Bonder and Wang"]) # 1 study (1308760143)
unique(data$exposure_study[data$ID == "1308760143"])
# Note that the study (ID = 1308760143) has a mixture of source microbiome data so will be dealing with the studies that use those later but, for now, will make the mixture of bonder and wang the same as above
data$exposure_age[which(data$exposure_study == "Mixture of Bonder and Wang")] <- "18-84"
data$exposure_sex[which(data$exposure_study == "Mixture of Bonder and Wang")] <- "Both"
data$exposure_og_n[which(data$exposure_study == "Mixture of Bonder and Wang")] <- "1514 from Bonder and 1812 from Wang"
data$exposure_n_diff[which(data$exposure_study == "Mixture of Bonder and Wang")] <- "Ns given in paper are from Bonder and Wang separately"
data$exposure_health[which(data$exposure_study == "Mixture of Bonder and Wang")] <- "Healthy"
data$exposure_pop[which(data$exposure_study == "Mixture of Bonder and Wang")] <- "European (Netherlands) - Bonder and Europeans - Wang"
table(unique(data$exposure[data$exposure_study == "Mixture of Bonder and Wang"]))
# Just want to add the taxonomical unit
data$exposure_formatted[which(data$exposure_study == "Mixture of Bonder and Wang")] <- data$exposure[which(data$exposure_study == "Mixture of Bonder and Wang")]
data$exposure_formatted[which(data$exposure_study == "Mixture of Bonder and Wang")] <- "Blautia (Genus)"
unique(data$exposure_formatted[which(data$exposure_study == "Mixture of Bonder and Wang")])
length(data$exposure_formatted[data$exposure_study == "Mixture of Bonder and Wang"])
length(unique(data$exposure_formatted[data$exposure_study == "Mixture of Bonder and Wang"]))
sum(is.na(data$exposure_formatted[data$exposure_study == "Mixture of Bonder and Wang"]))
sum(is.na(data$exposure_info[data$exposure_study == "Mixture of Bonder and Wang"]))
data$exposure_study[which(data$exposure_study == "Mixture of Bonder and Wang")] <- "Bonder 2016 and Wang 2016"
data[which((data$exposure_study == "Bonder 2016 and Wang 2016")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Current study ##
table(data$ID[data$exposure_study == "Current study"]) # 7 studies (2136589463, 2179267930, 2300517094, 3132308026, 560890194, 568199176, 914635920)
data[data$exposure_study == "Current study",c("ID","exposure","exposure_study", "exposure_unit")]
length(data$exposure[data$exposure_study == "Current study"])
length(unique(data$exposure[data$exposure_study == "Current study"]))
unique(data$exposure[data$exposure_study == "Current study"])
unique(data$exposure_unit[data$exposure_study == "Current study"])
unique(data$exposure_study[which(data$ID %in% c("2136589463", "2179267930", "2300517094", "3132308026", "560890194", "568199176", "914635920"))])
unique(data[which(data$ID %in% c("2136589463", "2179267930", "2300517094", "3132308026", "560890194", "568199176", "914635920")), c("ID", "exposure_study")])
# Note that one of these seven (568199176) has used a Sanna 2019 as well so will have to deal with the other studies that have used that later
# Need to harmonize
data$exposure_formatted[which(data$exposure_study == "Current study")] <- data$exposure[which(data$exposure_study == "Current study")]
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure == "Clostridiumsp CAG.226"))] <- gsub("Clostridiumsp CAG.226", "Clostridium CAG.226 (Species)", data$exposure[which((data$exposure_study == "Current study") & (data$exposure == "Clostridiumsp CAG.226"))])
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure == "Coprococcussp CAG.131"))] <- gsub("Coprococcussp CAG.131", "Coprococcus CAG.131 (Species)", data$exposure[which((data$exposure_study == "Current study") & (data$exposure == "Coprococcussp CAG.131"))])
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure == "M00307 pathway (SFCA butyrate production)"))] <- gsub("M00307 pathway \\(SFCA butyrate production\\)", "M00307 \\(Pathway\\)", data$exposure[which((data$exposure_study == "Current study") & (data$exposure == "M00307 pathway (SFCA butyrate production)"))])
data$exposure_info[which((data$exposure_study == "Current study") & (data$exposure == "M00307 pathway (SFCA butyrate production)"))] <- "SFCA butyrate production"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure == "COBALSYN-PWY: adenosylcobalamin salvage from cobinamide I"))] <- gsub("COBALSYN-PWY: adenosylcobalamin salvage from cobinamide I", "COBALSYN-PWY \\(Pathway\\)", data$exposure[which((data$exposure_study == "Current study") & (data$exposure == "COBALSYN-PWY: adenosylcobalamin salvage from cobinamide I"))])
data$exposure_info[which((data$exposure_study == "Current study") & (data$exposure == "COBALSYN-PWY: adenosylcobalamin salvage from cobinamide I"))] <- "Adenosylcobalamin salvage from cobinamide I"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure == "GLYCOGENSYNTH-PWY: glycogen biosynthesis I (from ADP-D-Glucose)"))] <- gsub("GLYCOGENSYNTH-PWY: glycogen biosynthesis I \\(from ADP-D-Glucose\\)", "GLYCOGENSYNTH-PWY \\(Pathway\\)", data$exposure[which((data$exposure_study == "Current study") & (data$exposure == "GLYCOGENSYNTH-PWY: glycogen biosynthesis I (from ADP-D-Glucose)"))])
data$exposure_info[which((data$exposure_study == "Current study") & (data$exposure == "GLYCOGENSYNTH-PWY: glycogen biosynthesis I (from ADP-D-Glucose)"))] <- "Glycogen biosynthesis I (from ADP-D-Glucose)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure == "k__Bacteria|p__Firmicutes|c__Clostridia|o__Clostridiales|f__Eubacteriaceae|g__Eubacterium|s__Eubacterium_rectale"))] <- gsub("k__Bacteria\\|p__Firmicutes\\|c__Clostridia\\|o__Clostridiales\\|f__Eubacteriaceae\\|g__Eubacterium\\|s__Eubacterium_rectale", "Eubacterium rectale (Species)", data$exposure[which((data$exposure_study == "Current study") & (data$exposure == "k__Bacteria|p__Firmicutes|c__Clostridia|o__Clostridiales|f__Eubacteriaceae|g__Eubacterium|s__Eubacterium_rectale"))])
data$exposure_info[which((data$exposure_study == "Current study") & (data$exposure == "k__Bacteria|p__Firmicutes|c__Clostridia|o__Clostridiales|f__Eubacteriaceae|g__Eubacterium|s__Eubacterium_rectale"))] <- "Bacteria (Kingdom) Firmicutes (Phylum) Clostridia (Class) Clostridiales (Order) Eubacteriaceae (Family) Eubacterium (Genus)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure == "k__Bacteria|p__Firmicutes|c__Negativicutes|o__Selenomonadales|f__Veillonellaceae|g__Veillonella|s__Veillonella_parvula"))] <- gsub("k__Bacteria\\|p__Firmicutes\\|c__Negativicutes\\|o__Selenomonadales\\|f__Veillonellaceae\\|g__Veillonella\\|s__Veillonella_parvula", "Veillonella parvula (Species)", data$exposure[which((data$exposure_study == "Current study") & (data$exposure == "k__Bacteria|p__Firmicutes|c__Negativicutes|o__Selenomonadales|f__Veillonellaceae|g__Veillonella|s__Veillonella_parvula"))])
data$exposure_info[which((data$exposure_study == "Current study") & (data$exposure == "k__Bacteria|p__Firmicutes|c__Negativicutes|o__Selenomonadales|f__Veillonellaceae|g__Veillonella|s__Veillonella_parvula"))] <- "Bacteria (Kingdom) Firmicutes (Phylum) Negativicutes (Class) Selenomonadales (Order) Veillonellaceae (Family) Veillonella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (grepl(glob2rx("genus *"), data$exposure)))] <- sub("^genus\\s(\\w+)$", "\\1 (Genus)", data$exposure[which((data$exposure_study == "Current study") & (grepl(glob2rx("genus *"), data$exposure)))])
data$exposure_info[which(data$exposure_study == "Current study" & grepl(glob2rx("MF*:*"), data$exposure_formatted))] <- sub("^MF\\d+:","", data$exposure_formatted[which(data$exposure_study == "Current study" & grepl(glob2rx("MF*:*"), data$exposure_formatted))])
data$exposure_formatted[which(data$exposure_study == "Current study" & grepl(glob2rx("MF*:*"), data$exposure_formatted))] <- sub(":.*", " (Pathway)", data$exposure_formatted[which(data$exposure_study == "Current study" & grepl(glob2rx("MF*:*"), data$exposure_formatted))])
data$exposure_formatted[which((data$exposure_study == "Current study") & (grepl(glob2rx("order *"), data$exposure)))] <- sub("^order\\s(\\w+)$", "\\1 (Order)", data$exposure[which((data$exposure_study == "Current study") & (grepl(glob2rx("order *"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "Current study") & (grepl(glob2rx("family *"), data$exposure)))] <- sub("^family\\s(\\w+)$", "\\1 (Family)", data$exposure[which((data$exposure_study == "Current study") & (grepl(glob2rx("family *"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "Current study") & (grepl(glob2rx("species *"), data$exposure)))] <- sub("species (\\w+) (\\w+)", "\\1 \\2 (Species)", data$exposure[which((data$exposure_study == "Current study") & (grepl(glob2rx("species *"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "species Faecalibacterium_prausnitzii"))] <- "Faecalibacterium prausnitzii (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "species unclassified_Lachnospiraceae_bacterium_9_1_43BFAA"))] <- "Lachnospiraceae bacterium 9_1_43BFAA (Unclassified Species in Family)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "p_Proteobacteria"))] <- "Proteobacteria (Phylum)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "species Achromobacter_piechaudii"))] <- "Achromobacter piechaudii (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "species Escherichia_coli"))] <- "Escherichia coli (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "species Pseudoflavonifractor_capillosus"))] <- "Pseudoflavonifractor capillosus (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "species Salmonella_enterica"))] <- "Salmonella enterica (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "p__TM7(Saccharibacteria)"))] <- "Saccharibacteria TM7 (Phylum)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "_Clostridiaceae (Family)"))] <- "Clostridiaceae (Family)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "_Comamonadaceae (Family)"))] <- "Comamonadaceae (Family)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "_Klebsiella (Genus)"))] <- "Klebsiella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "_Desulfovibrio d168 (Species)"))] <- "Desulfovibrio d168 (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Clostridiales Family XIII"))] <- "Clostridiales XIII (Family)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Christensenellaceae R-7 group"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Acinetobacter radioresistens"))] <- "Acinetobacter radioresistens (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Bacteroides fragilis"))] <- "Bacteroides fragilis (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Bacteroides stercoris"))] <- "Bacteroides stercoris (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Bacteroides uniformis"))] <- "Bacteroides uniformis (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Ruminococcaceae UCG-002"))] <- "UCG002 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Ruminococcaceae UCG-008"))] <- "UCG008 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Bacteroides eggerthii"))] <- "Bacteroides eggerthii (Species)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Sutterella"))] <- "Sutterella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Prevotella 2"))] <- "Prevotella2 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Current study") & (data$exposure_formatted == "Ruminococcus 2"))] <- "Ruminococcus2 (Genus)"
unique(data$exposure_formatted[data$exposure_study == "Current study"])
unique(data$exposure[data$exposure_study == "Current study"])
length(data$exposure_formatted[data$exposure_study == "Current study"])
length(unique(data$exposure_formatted[data$exposure_study == "Current study"]))
sum(is.na(data$exposure_formatted[data$exposure_study == "Current study"]))
sum(is.na(data$exposure_info[data$exposure_study == "Current study"]))
data[which((data$exposure_study == "Current study")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Freidin 2021 ##
table(data$ID[data$exposure_study == "Freidin 2021"]) # 1 study (1519899723)
data[data$exposure_study == "Freidin 2021",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Freidin 2021"])

# First study (ID = 1519899723) 
data$exposure_unit[data$ID == "1519899723"]
unique(data$exposure_study[data$ID == "1519899723"])
length(data$exposure[data$ID == "1519899723"])
length(unique(data$exposure[data$ID == "1519899723"]))
table(unique(data$exposure[data$ID == "1519899723"])) 
# Adding taxonomical classification
data$exposure_formatted[which(data$ID == "1519899723")] <- data$exposure[which(data$ID == "1519899723")]
data$exposure_formatted[which(data$ID == "1519899723")] <- "Coprococcus comes (Species)"
data$exposure_formatted[which(data$ID == "1519899723")]
length(data$exposure_formatted[data$ID == "1519899723"])
length(unique(data$exposure_formatted[data$ID == "1519899723"]))
sum(is.na(data$exposure_formatted[data$ID == "1519899723"]))
sum(is.na(data$exposure_info[data$ID == "1519899723"]))
data[which((data$ID == "1519899723")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Goodrich 2016 ##
table(data$ID[data$exposure_study == "Goodrich 2016"]) # 8 studies (1308760143 2093521025 2383481240 2934159401 3032725182 3272561227 4251029218 4290593439)
data[data$exposure_study == "Goodrich 2016",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Goodrich 2016"])

# First study (ID = 1308760143) 
data$exposure_unit[data$ID == "1308760143"]
unique(data$exposure_study[data$ID == "1308760143"])
# Note that this first study used a mixture of Bonder and Wang (amongst other exposure study sources), which has already been handled 
# Therefore, we'll be handling all phenotypes of this study and will have to deal with the other studies that have used these other exposure study sources later
length(data$exposure[data$ID == "1308760143"])
length(unique(data$exposure[data$ID == "1308760143"]))
table(unique(data$exposure[data$ID == "1308760143"])) 
# Adding taxonomical classifications
data$exposure_formatted[which(data$ID == "1308760143")] <- data$exposure[which(data$ID == "1308760143")]
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Acidaminococcus"))] <- "Acidaminococcus (Genus)"
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Aggregatibacter"))] <- "Aggregatibacter (Genus)"
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Anaerostipes"))] <- "Anaerostipes (Genus)"
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Bifidobacterium"))] <- "Bifidobacterium (Genus)"
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Blautia"))] <- "Blautia (Genus)"
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Desulfovibrio"))] <- "Desulfovibrio (Genus)"
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Dorea"))] <- "Dorea (Genus)"
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Faecalibacterium"))] <- "Faecalibacterium (Genus)" 
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Lachnospira"))] <- "Lachnospira (Genus)"
data$exposure_formatted[which((data$ID == "1308760143") & (data$exposure_formatted == "Lactobacillus"))] <- "Lactobacillus (Genus)"
table(unique(data$exposure_formatted[data$ID == "1308760143"]))
length(data$exposure_formatted[data$ID == "1308760143"])
length(unique(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_info[data$ID == "1308760143"]))
data[which((data$ID == "1308760143")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 2093521025) 
data$exposure_unit[data$ID == "2093521025"]
unique(data$exposure_study[data$ID == "2093521025"])
length(data$exposure[data$ID == "2093521025"])
length(unique(data$exposure[data$ID == "2093521025"]))
table(unique(data$exposure[data$ID == "2093521025"])) 
# Any changes
data$exposure_formatted[which((data$ID == "2093521025") & (grepl(glob2rx("Family *"), data$exposure)))] <- sub("^Family\\s(\\w+)$", "\\1 (Family)", data$exposure[which((data$ID == "2093521025") & (grepl(glob2rx("Family *"), data$exposure)))])
data$exposure_formatted[which((data$ID == "2093521025") & (grepl(glob2rx("Genus *"), data$exposure)))] <- sub("^Genus\\s(\\w+)$", "\\1 (Genus)", data$exposure[which((data$ID == "2093521025") & (grepl(glob2rx("Genus *"), data$exposure)))])
data$exposure_formatted[which((data$ID == "2093521025") & (grepl(glob2rx("Species *"), data$exposure)))] <- sub("^Species\\s(\\w+)$", "\\1 (Species)", data$exposure[which((data$ID == "2093521025") & (grepl(glob2rx("Species *"), data$exposure)))])
table(unique(data$exposure_formatted[data$ID == "2093521025"]))
table(unique(data$exposure_info[data$ID == "2093521025"]))
length(data$exposure_formatted[data$ID == "2093521025"])
length(unique(data$exposure_formatted[data$ID == "2093521025"]))
sum(is.na(data$exposure_formatted[data$ID == "2093521025"]))
sum(is.na(data$exposure_info[data$ID == "2093521025"]))
data[which((data$ID == "2093521025")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Third study (ID = 2383481240) 
data$exposure_unit[data$ID == "2383481240"]
unique(data$exposure_study[data$ID == "2383481240"])
# Note that this study uses Kurilshikov 2021 as well so will have to deal with the studies that use this study later
length(data$exposure[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"])
length(unique(data$exposure[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"]))
table(unique(data$exposure[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"])) 
# Add species to the end of each bacterial
data$exposure_formatted[which(data$ID == "2383481240" & data$exposure_study == "Goodrich 2016")] <- data$exposure[which(data$ID == "2383481240" & data$exposure_study == "Goodrich 2016")]
data$exposure_formatted[which(data$ID == "2383481240" & data$exposure_study == "Goodrich 2016")] <- paste(data$exposure[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"], "(Species)", sep = " ")
table(unique(data$exposure_formatted[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"]))
table(unique(data$exposure_info[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"]))
length(data$exposure_formatted[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"])
length(unique(data$exposure_formatted[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"]))
sum(is.na(data$exposure_formatted[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"]))
sum(is.na(data$exposure_info[data$ID == "2383481240" & data$exposure_study == "Goodrich 2016"]))
data[which((data$ID == "2383481240")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Fourth study (ID = 2934159401) 
data$exposure_unit[data$ID == "2934159401"]
unique(data$exposure_study[data$ID == "2934159401"])
# Note that this study uses Kurilshikov 2021 as well so will have to deal with the studies that use this study later
length(data$exposure[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"])
length(unique(data$exposure[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"]))
table(unique(data$exposure[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"])) 
# Add taxonomical classification
data$exposure_formatted[which(data$ID == "2934159401" & data$exposure_study == "Goodrich 2016")] <- data$exposure[which(data$ID == "2934159401" & data$exposure_study == "Goodrich 2016")]
# Note there was a spelling mistake in one
data$exposure_formatted[which((data$ID == "2934159401") & (data$exposure_formatted == "Erysipelotrichi"))] <- "Erysipelotrichia (Class)"
genus_list <- c("Akkermansia","Anaerostipes","Bacteroides","Bifidobacterium","Blautia","Coprococcus","Dorea","Eggerthella","Faecalibacterium","Haemophilus","Holdemania","Roseburia","Ruminococcus","Streptococcus","Veillonella")
for(i in genus_list){
  data$exposure_formatted[which((data$ID == "2934159401") & (data$exposure_formatted == i))] <- paste(i, "(Genus)", sep = " ")
}
rm(genus_list, i)
phylum_list <- c("Actinobacteria","Bacteroidetes","Firmicutes","Proteobacteria","Verrucomicrobia")
for(i in phylum_list){
  data$exposure_formatted[which((data$ID == "2934159401") & (data$exposure_formatted == i))] <- paste(i, "(Phylum)", sep = " ")
}
rm(phylum_list, i)
family_list <- c("Bacteroidaceae","Barnesiellaceae","Bifidobacteriaceae","Clostridiaceae","Coriobacteriaceae","Enterobacteriaceae","Erysipelotrichaceae","Lachnospiraceae", "Mogibacteriaceae","Pasteurellaceae","Rikenellaceae","Ruminococcaceae","Streptococcaceae","Veillonellaceae","Verrucomicrobiaceae")
for(i in family_list){
  data$exposure_formatted[which((data$ID == "2934159401") & (data$exposure_formatted == i))] <- paste(i, "(Family)", sep = " ")
}
rm(family_list, i)
order_list <- c("Bacteroidales","Bifidobacteriales","Clostridiales","Coriobacteriales","Enterobacteriales","Erysipelotrichales","Lactobacillales","Pasteurellales","Verrucomicrobiales")
for(i in order_list){
  data$exposure_formatted[which((data$ID == "2934159401") & (data$exposure_formatted == i))] <- paste(i, "(Order)", sep = " ")
}
rm(order_list, i)
class_list <- c("Bacilli","Bacteroidia","Clostridia","Coriobacteriia","Gammaproteobacteria","Verrucomicrobiae")
for(i in class_list){
  data$exposure_formatted[which((data$ID == "2934159401") & (data$exposure_formatted == i))] <- paste(i, "(Class)", sep = " ")
}
rm(class_list, i)
table(unique(data$exposure_formatted[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"]))
table(unique(data$exposure_info[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"]))
length(data$exposure_formatted[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"])
length(unique(data$exposure_formatted[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"]))
sum(is.na(data$exposure_formatted[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"]))
sum(is.na(data$exposure_info[data$ID == "2934159401" & data$exposure_study == "Goodrich 2016"]))
data[which((data$ID == "2934159401" & data$exposure_study == "Goodrich 2016")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Fifth study (ID = 3032725182)
data$exposure_unit[data$ID == "3032725182"]
unique(data$exposure_study[data$ID == "3032725182"])
# Note that, as this study also uses Bonder 2016, this has already been dealt with before
length(data$exposure[data$ID == "3032725182"])
length(unique(data$exposure[data$ID == "3032725182"]))
table(unique(data$exposure[data$ID == "3032725182"])) 
table(unique(data$exposure_formatted[data$ID == "3032725182"]))
table(unique(data$exposure_info[data$ID == "3032725182"]))
length(data$exposure_formatted[data$ID == "3032725182"])
length(unique(data$exposure_formatted[data$ID == "3032725182"]))
sum(is.na(data$exposure_formatted[data$ID == "3032725182"]))
sum(is.na(data$exposure_info[data$ID == "3032725182"]))
data[which((data$ID == "3032725182")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Sixth study (ID = 3272561227) 
data$exposure_unit[data$ID == "3272561227"]
unique(data$exposure_study[data$ID == "3272561227"])
length(data$exposure[data$ID == "3272561227"])
length(unique(data$exposure[data$ID == "3272561227"]))
table(unique(data$exposure[data$ID == "3272561227"])) 
# Any changes
data$exposure_formatted[which(data$ID == "3272561227")] <- data$exposure[which(data$ID == "3272561227")]
data$exposure_formatted[which((data$ID == "3272561227") & grepl("\\(family\\)", data$exposure))] <- gsub("\\(family\\)", "\\(Family\\)", data$exposure[which((data$ID == "3272561227") & grepl("\\(family\\)", data$exposure))])
data$exposure_formatted[which((data$ID == "3272561227") & grepl("\\(genus\\)", data$exposure))] <- gsub("\\(genus\\)", "\\(Genus\\)", data$exposure[which((data$ID == "3272561227") & grepl("\\(genus\\)", data$exposure))])
table(unique(data$exposure_formatted[data$ID == "3272561227"]))
table(unique(data$exposure_info[data$ID == "3272561227"]))
length(data$exposure_formatted[data$ID == "3272561227"])
length(unique(data$exposure_formatted[data$ID == "3272561227"]))
sum(is.na(data$exposure_formatted[data$ID == "3272561227"]))
sum(is.na(data$exposure_info[data$ID == "3272561227"]))
data[which((data$ID == "3272561227")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Seventh study (ID = 4251029218) 
data$exposure_unit[data$ID == "4251029218"]
unique(data$exposure_study[data$ID == "4251029218"])
## Note that this also uses Wang 2016, so will have to handle the other studies that use this data source later 
length(data$exposure[data$ID == "4251029218"])
length(unique(data$exposure[data$ID == "4251029218"]))
table(unique(data$exposure[data$ID == "4251029218"])) 
# Any changes
data$exposure_formatted[which(data$ID == "4251029218")] <- data$exposure[which(data$ID == "4251029218")]
data$exposure_formatted[which((data$ID == "4251029218") & grepl("\\(family\\)", data$exposure))] <- gsub("\\(family\\)", "\\(Family\\)", data$exposure[which((data$ID == "4251029218") & grepl("\\(family\\)", data$exposure))])
data$exposure_formatted[which((data$ID == "4251029218") & grepl("\\(genus\\)", data$exposure))] <- gsub("\\(genus\\)", "\\(Genus\\)", data$exposure[which((data$ID == "4251029218") & grepl("\\(genus\\)", data$exposure))])
table(unique(data$exposure_formatted[data$ID == "4251029218"]))
table(unique(data$exposure_info[data$ID == "4251029218"]))
length(data$exposure_formatted[data$ID == "4251029218"])
length(unique(data$exposure_formatted[data$ID == "4251029218"]))
sum(is.na(data$exposure_formatted[data$ID == "4251029218"]))
sum(is.na(data$exposure_info[data$ID == "4251029218"]))
data[which((data$ID == "4251029218")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Eighth study (ID = 4290593439) 
data$exposure_unit[data$ID == "4290593439"]
unique(data$exposure_study[data$ID == "4290593439"])
# Note that this study also uses several other exposure_study sources so will be dealing with the studies that use those separately later
# But, otherwise, this study has already been handled
length(data$exposure[data$ID == "4290593439"])
length(unique(data$exposure[data$ID == "4290593439"]))
table(unique(data$exposure[data$ID == "4290593439"])) 
table(unique(data$exposure_formatted[data$ID == "4290593439"]))
table(unique(data$exposure_info[data$ID == "4290593439"]))
length(data$exposure_formatted[data$ID == "4290593439"])
length(unique(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_info[data$ID == "4290593439"]))
data[which((data$ID == "4290593439")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Goodrich 2016 and Turpin 2016 ##
table(data$ID[data$exposure_study == "Goodrich 2016 and Turpin 2016"]) # 1 study (2238214110)
data[data$exposure_study == "Goodrich 2016 and Turpin 2016",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Goodrich 2016 and Turpin 2016"])

# First study (ID = 2238214110) 
data$exposure_unit[data$ID == "2238214110"]
unique(data$exposure_study[data$ID == "2238214110"])
length(data$exposure[data$ID == "2238214110"])
length(unique(data$exposure[data$ID == "2238214110"]))
table(unique(data$exposure[data$ID == "2238214110"])) 
# Change to "Gut microbiome" for consistency with others (doesn't matter anyway because it won't be meta-analysable given that "gut microbiota" is heterogeneous)
data$exposure_formatted[which(data$ID == "2238214110")] <- "Gut microbiome"
table(unique(data$exposure_formatted[data$ID == "2238214110"]))
table(unique(data$exposure_info[data$ID == "2238214110"]))
length(data$exposure_formatted[data$ID == "2238214110"])
length(unique(data$exposure_formatted[data$ID == "2238214110"]))
sum(is.na(data$exposure_formatted[data$ID == "2238214110"]))
sum(is.na(data$exposure_info[data$ID == "2238214110"]))
data[which((data$ID == "2238214110")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Goodrich 2016; Wang 2016; Turpin 2016; Davenport 2015 and Bonder 2016 ##
table(data$ID[data$exposure_study == "Goodrich 2016; Wang 2016; Turpin 2016; Davenport 2015 and Bonder 2016"]) # 1 study (3113131015)
data[data$exposure_study == "Goodrich 2016; Wang 2016; Turpin 2016; Davenport 2015 and Bonder 2016",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Goodrich 2016; Wang 2016; Turpin 2016; Davenport 2015 and Bonder 2016"])

# First study (ID = 3113131015) 
data$exposure_unit[data$ID == "3113131015"]
unique(data$exposure_study[data$ID == "3113131015"])
length(data$exposure[data$ID == "3113131015"])
length(unique(data$exposure[data$ID == "3113131015"]))
table(unique(data$exposure[data$ID == "3113131015"])) 
# Adding taxonomical classification
data$exposure_formatted[which(data$ID == "3113131015")] <- data$exposure[which(data$ID == "3113131015")]
# Note that there was a spelling mistake in one
data$exposure_formatted[which((data$ID == "3113131015") & (data$exposure_formatted == "Aanaerostipes"))] <- "Anaerostipes (Genus)"
genus_list <- c("Acidaminococcus","Acinetobacter","Aggregatibacter","Atopobium","Bacteroides","Bifidobacterium","Coprococcus","Desulfovibrio","Dorea","Eggerthella","Eubacterium","Faecalibacterium","Lachnospira","Lactobacillus","Leuconostoc","Megamonas","Mogibacterium","Oscillospira","Pseudobutyruvubrio","Roseburia","Slackia","Weissella")
for(i in genus_list){
  data$exposure_formatted[which((data$ID == "3113131015") & (data$exposure_formatted == i))] <- paste(i, "(Genus)", sep = " ")
}
rm(genus_list, i)
table(unique(data$exposure_formatted[data$ID == "3113131015"]))
table(unique(data$exposure_info[data$ID == "3113131015"]))
length(data$exposure_formatted[data$ID == "3113131015"])
length(unique(data$exposure_formatted[data$ID == "3113131015"]))
sum(is.na(data$exposure_formatted[data$ID == "3113131015"]))
sum(is.na(data$exposure_info[data$ID == "3113131015"]))
data[which((data$ID == "3113131015")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Hughes 2020 ##
table(data$ID[data$exposure_study == "Hughes 2020"]) # 5 studies (0635532754  122541860 2380923650  2738712179  2918940101)
data[data$exposure_study == "Hughes 2020",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Hughes 2020"])

# First study (ID = 0635532754) 
data$exposure_unit[data$ID == "0635532754"]
unique(data$exposure_study[data$ID == "0635532754"])
length(data$exposure[data$ID == "0635532754"])
length(unique(data$exposure[data$ID == "0635532754"]))
table(unique(data$exposure[data$ID == "0635532754"])) 
# Add taxonomical classifications
data$exposure_formatted[which(data$ID == "0635532754")] <- data$exposure[which(data$ID == "0635532754")]
data$exposure_formatted[which(data$ID == "0635532754" & data$exposure_formatted == "Dialister")] <- "Dialister (Genus)"
data$exposure_formatted[which(data$ID == "0635532754" & data$exposure_formatted == "Lactobacillales")] <- "Lactobacillales (Order)"
data$exposure_formatted[which(data$ID == "0635532754" & data$exposure_formatted == "Streptococcaceae")] <- "Streptococcaceae (Family)"
data$exposure_formatted[which(data$ID == "0635532754" & data$exposure_formatted == "Streptococcus")] <- "Streptococcus (Genus)"
table(unique(data$exposure_formatted[data$ID == "0635532754"]))
table(unique(data$exposure_info[data$ID == "0635532754"]))
length(data$exposure_formatted[data$ID == "0635532754"])
length(unique(data$exposure_formatted[data$ID == "0635532754"]))
sum(is.na(data$exposure_formatted[data$ID == "0635532754"]))
sum(is.na(data$exposure_info[data$ID == "0635532754"]))
data[which((data$ID == "0635532754")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 122541860)
data$exposure_unit[data$ID == "122541860"]
unique(data$exposure_study[data$ID == "122541860"])
length(data$exposure[data$ID == "122541860"])
length(unique(data$exposure[data$ID == "122541860"]))
table(unique(data$exposure[data$ID == "122541860"])) 
# Need to capitalise the taxonomical classifications and move the "abundance" and "presence" information to the exposure_info variable, whilst also making sure that the unclassified phenotypes are in the same format
data$exposure_formatted[which(data$ID == "122541860")] <- data$exposure[which(data$ID == "122541860")]
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *ceae presence"), data$exposure)))] <- sub("unclassified \\(genus\\) (\\w+) presence", "\\1 (Unclassified Genus in Family)", data$exposure[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *ceae presence"), data$exposure)))])
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *ceae abundance"), data$exposure)))] <- sub("unclassified \\(genus\\) (\\w+) abundance", "\\1 (Unclassified Genus in Family)", data$exposure[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *ceae abundance"), data$exposure)))])
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *cutes presence"), data$exposure)))] <- sub("unclassified \\(genus\\) (\\w+) presence", "\\1 (Unclassified Genus in Phylum)", data$exposure[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *cutes presence"), data$exposure)))])
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *cutes abundance"), data$exposure)))] <- sub("unclassified \\(genus\\) (\\w+) abundance", "\\1 (Unclassified Genus in Phylum)", data$exposure[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *cutes abundance"), data$exposure)))])
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *dales presence"), data$exposure)))] <- sub("unclassified \\(genus\\) (\\w+) presence", "\\1 (Unclassified Genus in Order)", data$exposure[which((data$ID == "122541860") & (grepl(glob2rx("unclassified *dales presence"), data$exposure)))])
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("* (\\genus\\) presence"), data$exposure)))] <- sub("\\(genus\\) presence", "\\(Genus\\)", data$exposure[which((data$ID == "122541860") & (grepl(glob2rx("* \\(genus\\) presence"), data$exposure)))])
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("* (\\genus\\) abundance"), data$exposure)))] <- sub("\\(genus\\) abundance", "\\(Genus\\)", data$exposure[which((data$ID == "122541860") & (grepl(glob2rx("* \\(genus\\) abundance"), data$exposure)))])
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("Sutterellaceae * presence"), data$exposure)))] <- "Sutterellaceae (Family)"
data$exposure_formatted[which((data$ID == "122541860") & (grepl(glob2rx("* (\\class\\) abundance"), data$exposure)))] <- sub("\\(class\\) abundance", "\\(Class\\)", data$exposure[which((data$ID == "122541860") & (grepl(glob2rx("* \\(class\\) abundance"), data$exposure)))])
data$exposure_info[which((data$ID == "122541860") & grepl("abundance", data$exposure))] <- "Abundance (RNT)"
data$exposure_info[which((data$ID == "122541860") & grepl("presence", data$exposure))] <- "Presence vs. absence (HB)"
table(unique(data$exposure_formatted[data$ID == "122541860"]))
table(unique(data$exposure_info[data$ID == "122541860"]))
length(data$exposure_formatted[data$ID == "122541860"])
length(unique(data$exposure_formatted[data$ID == "122541860"]))
sum(is.na(data$exposure_formatted[data$ID == "122541860"]))
sum(is.na(data$exposure_info[data$ID == "122541860"]))
data[which((data$ID == "122541860")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Third study (ID = 2380923650) 
data$exposure_unit[data$ID == "2380923650"]
unique(data$exposure_study[data$ID == "2380923650"])
length(data$exposure[data$ID == "2380923650"])
length(unique(data$exposure[data$ID == "2380923650"]))
table(unique(data$exposure[data$ID == "2380923650"])) 
# All are in the same format so just need to shift to be consistent with all other studies and they only seem to have used the RNT variables
data$exposure_formatted[which(data$ID == "2380923650")] <- data$exposure[which(data$ID == "2380923650")]
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_C_*_RNT"), data$exposure)))] <- sub("G_unclassified_C_(\\w+)_RNT", "\\1 (Unclassified Genus in Class)", data$exposure[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_C_*_RNT"), data$exposure)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_F_*_RNT"), data$exposure)))] <- sub("G_unclassified_F_(\\w+)_RNT", "\\1 (Unclassified Genus in Family)", data$exposure[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_F_*_RNT"), data$exposure)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_O_*_RNT"), data$exposure)))] <- sub("G_unclassified_O_(\\w+)_RNT", "\\1 (Unclassified Genus in Order)", data$exposure[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_O_*_RNT"), data$exposure)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_P_*_RNT"), data$exposure)))] <- sub("G_unclassified_P_(\\w+)_RNT", "\\1 (Unclassified Genus in Phylum)", data$exposure[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_P_*_RNT"), data$exposure)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_K_*_RNT"), data$exposure)))] <- sub("G_unclassified_K_(\\w+)_RNT", "\\1 (Unclassified Genus in Kingdom)", data$exposure[which((data$ID == "2380923650") & (grepl(glob2rx("G_unclassified_K_*_RNT"), data$exposure)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("C_*_RNT"), data$exposure_formatted)))] <- gsub("C_(.*?)_RNT$", "\\1 (Class)", data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("C_*_RNT"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("F_*_RNT"), data$exposure_formatted)))] <- gsub("F_(.*?)_RNT$", "\\1 (Family)", data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("F_*_RNT"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("O_*_RNT"), data$exposure_formatted)))] <- gsub("O_(.*?)_RNT$", "\\1 (Order)", data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("O_*_RNT"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("P_*_RNT"), data$exposure_formatted)))] <- gsub("P_(.*?)_RNT$", "\\1 (Phylum)", data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("P_*_RNT"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("G_*_RNT"), data$exposure_formatted)))] <- gsub("G_(.*?)_RNT$", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("G_*_RNT"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("Div_*_RNT"), data$exposure_formatted)))] <- gsub("Div_(.*?)_RNT$", "\\1 (Diversity)", data$exposure_formatted[which((data$ID == "2380923650") & (grepl(glob2rx("Div_*_RNT"), data$exposure_formatted)))])
data$exposure_info[which((data$ID == "2380923650") & (grepl(glob2rx("*_RNT"), data$exposure)))] <- "Abundance (RNT)"
data$exposure_formatted[which((data$ID == "2380923650") & (data$exposure_formatted == "Clostridium_IV (Genus)"))] <- "Clostridium IV (Genus)"
data$exposure_formatted[which((data$ID == "2380923650") & (data$exposure_formatted == "Clostridium_sensu_stricto (Genus)"))] <- "Clostridium sensu stricto (Genus)"
data$exposure_formatted[which((data$ID == "2380923650") & (data$exposure_formatted == "Clostridium_XlVa (Genus)"))] <- "Clostridium XlVa (Genus)"
data$exposure_formatted[which((data$ID == "2380923650") & (data$exposure_formatted == "Clostridium_XVIII (Genus)"))] <- "Clostridium XVIII (Genus)"
data$exposure_formatted[which((data$ID == "2380923650") & (data$exposure_formatted == "Escherichia_Shigella (Genus)"))] <- "Escherichia Shigella (Genus)"
table(unique(data$exposure_formatted[data$ID == "2380923650"]))
table(unique(data$exposure_info[data$ID == "2380923650"]))
length(data$exposure_formatted[data$ID == "2380923650"])
length(unique(data$exposure_formatted[data$ID == "2380923650"]))
sum(is.na(data$exposure_formatted[data$ID == "2380923650"]))
sum(is.na(data$exposure_info[data$ID == "2380923650"]))
data[which((data$ID == "2380923650")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Fourth study (ID = 2738712179) 
data$exposure_unit[data$ID == "2738712179"]
unique(data$exposure_study[data$ID == "2738712179"])
length(data$exposure[data$ID == "2738712179"])
length(unique(data$exposure[data$ID == "2738712179"]))
table(unique(data$exposure[data$ID == "2738712179"])) 
# Reformat to be the same as others using Hughes data
data$exposure_formatted[which(data$ID == "2738712179")] <- data$exposure[which(data$ID == "2738712179")]
data$exposure_formatted[which((data$ID == "2738712179") & (grepl(glob2rx("G_*_HB"), data$exposure_formatted)))] <- gsub("G_(.*?)_HB$", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "2738712179") & (grepl(glob2rx("G_*_HB"), data$exposure_formatted)))])
data$exposure_info[which((data$ID == "2738712179"))] <- "Presence vs. absence (HB)"
table(unique(data$exposure_formatted[data$ID == "2738712179"]))
table(unique(data$exposure_info[data$ID == "2738712179"]))
length(data$exposure_formatted[data$ID == "2738712179"])
length(unique(data$exposure_formatted[data$ID == "2738712179"]))
sum(is.na(data$exposure_formatted[data$ID == "2738712179"]))
sum(is.na(data$exposure_info[data$ID == "2738712179"]))
data[which((data$ID == "2738712179")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Fifth study (ID = 2918940101) 
data$exposure_unit[data$ID == "2918940101"]
unique(data$exposure_study[data$ID == "2918940101"])
length(data$exposure[data$ID == "2918940101"])
length(unique(data$exposure[data$ID == "2918940101"]))
table(unique(data$exposure[data$ID == "2918940101"])) 
# Reformat to be the same as others using Hughes data
data$exposure_formatted[which(data$ID == "2918940101")] <- data$exposure[which(data$ID == "2918940101")]
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, F. * \\(AB\\)"), data$exposure_formatted)))] <- sub("G. unclassified, F. (\\w+) \\(AB\\)", "\\1 (Unclassified Genus in Family)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, F. * \\(AB\\)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, F. * \\(P/A\\)"), data$exposure_formatted)))] <- sub("G. unclassified, F. (\\w+) \\(P/A\\)", "\\1 (Unclassified Genus in Family)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, F. * \\(P/A\\)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, O. * \\(P/A\\)"), data$exposure_formatted)))] <- sub("G. unclassified, O. (\\w+) \\(P/A\\)", "\\1 (Unclassified Genus in Order)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, O. * \\(P/A\\)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, P. * \\(AB\\)"), data$exposure_formatted)))] <- sub("G. unclassified, P. (\\w+) \\(AB\\)", "\\1 (Unclassified Genus in Phylum)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, P. * \\(AB\\)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, P. * \\(P/A\\)"), data$exposure_formatted)))] <- sub("G. unclassified, P. (\\w+) \\(P/A\\)", "\\1 (Unclassified Genus in Phylum)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. unclassified, P. * \\(P/A\\)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("C. * \\(AB\\)"), data$exposure_formatted)))] <- sub("C. (\\w+) \\(AB\\)", "\\1 (Class)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("C. * \\(AB\\)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("F. * \\(P/A\\)"), data$exposure_formatted)))] <- sub("F. (\\w+) \\(P/A\\)", "\\1 (Family)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("F. * \\(P/A\\)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. * \\(AB\\)"), data$exposure_formatted)))] <- sub("G. (\\w+) \\(AB\\)", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. * \\(AB\\)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. * \\(P/A\\)"), data$exposure_formatted)))] <- sub("G. (\\w+) \\(P/A\\)", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "2918940101") & (grepl(glob2rx("G. * \\(P/A\\)"), data$exposure_formatted)))])
data$exposure_info[which((data$ID == "2918940101") & grepl("\\(AB\\)", data$exposure))] <- "Abundance"
data$exposure_info[which((data$ID == "2918940101") & grepl("\\(P/A\\)", data$exposure))] <- "Presence vs. absence"
table(unique(data$exposure_formatted[data$ID == "2918940101"]))
table(unique(data$exposure_info[data$ID == "2918940101"]))
length(data$exposure_formatted[data$ID == "2918940101"])
length(unique(data$exposure_formatted[data$ID == "2918940101"]))
sum(is.na(data$exposure_formatted[data$ID == "2918940101"]))
sum(is.na(data$exposure_info[data$ID == "2918940101"]))
data[which((data$ID == "2918940101")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Kurilshikov 2021 ##
table(data$ID[data$exposure_study == "Kurilshikov 2021"]) # 23 studies (which is too many, so I just did for the usage of this exposure data) 
# 1977124126 2662850683 776143774 2419487002 707846668 1599546107 4286528530 1286056190 2547037187 1795779689 3191171275 1055369512 23119582 3286259646 1324207092 4199071353 3760378084 2383481240 3678116995 2634842805 2934159401 4163561095 880716487
data[data$exposure_study == "Kurilshikov 2021",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Kurilshikov 2021"])
# as there are 21 studies, I'm going to do this together given similarities across reporting, then deal with unique values individually
length(data$exposure[data$exposure_study == "Kurilshikov 2021"])
length(unique(data$exposure[data$exposure_study == "Kurilshikov 2021"]))
table(unique(data$exposure[data$exposure_study == "Kurilshikov 2021"])) 
# changes
table(unique(data$exposure[which(grepl(".csv",data$exposure))]))
data$exposure[which(grepl(".csv",data$exposure))] <- sub("(\\w+).csv", "\\1", data$exposure[which(grepl(".csv",data$exposure))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021"))] <- data$exposure[which((data$exposure_study == "Kurilshikov 2021"))]
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "\"Total\" microbiome"))] <- "Gut microbiome"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "\"Total microbiome\""))] <- "Gut microbiome"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("phylum.*.id.*"), data$exposure)))] <- sub("phylum\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("phylum.*.id.*"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("phylum.*.id.*"), data$exposure)))] <- sub("phylum\\.(\\w+)\\.id.*", "\\1 (Phylum)", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("phylum.*.id.*"), data$exposure)))])
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("class.*.id.*"), data$exposure)))] <- sub("class\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("class.*.id.*"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("class.*.id.*"), data$exposure)))] <- sub("class\\.(\\w+)\\.id.*", "\\1 (Class)", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("class.*.id.*"), data$exposure)))])
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("family.*.id.*"), data$exposure)))] <- sub("family\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("family.*.id.*"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("family.*.id.*"), data$exposure)))] <- sub("family\\.(\\w+)\\.id.*", "\\1 (Family)", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("family.*.id.*"), data$exposure)))])
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("order.*.id.*"), data$exposure)))] <- sub("order\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("order.*.id.*"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("order.*.id.*"), data$exposure)))] <- sub("order\\.(\\w+)\\.id.*", "\\1 (Order)", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("order.*.id.*"), data$exposure)))])
unique(data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("genus..*group.id.*"), data$exposure_formatted)))])
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Clostridiuminnocuumgroup.id.14397")] <- "ID = 14397"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Clostridiuminnocuumgroup.id.14397")] <- "Clostridium innocuum Group (Species in Clostridium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumbrachygroup.id.11296")] <- "ID = 11296"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumbrachygroup.id.11296")] <- "Eubacterium brachy Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumcoprostanoligenesgroup.id.11375")] <- "ID = 11375"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumcoprostanoligenesgroup.id.11375")] <- "Eubacterium coprostanoligenes Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumeligensgroup.id.14372")] <- "ID = 14372"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumeligensgroup.id.14372")] <- "Eubacterium eligens Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumfissicatenagroup.id.14373")] <- "ID = 14373"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumfissicatenagroup.id.14373")] <- "Eubacterium fissicatena Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumhalliigroup.id.11338")] <- "ID = 11338"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumhalliigroup.id.11338")] <- "Eubacterium hallii Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumnodatumgroup.id.11297")] <- "ID = 11297"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumnodatumgroup.id.11297")] <- "Eubacterium nodatum Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumoxidoreducensgroup.id.11339")] <- "ID = 11339"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumoxidoreducensgroup.id.11339")] <- "Eubacterium oxidoreducens Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumrectalegroup.id.14374")] <- "ID = 14374"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumrectalegroup.id.14374")] <- "Eubacterium rectale Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumruminantiumgroup.id.11340")] <- "ID = 11340"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumruminantiumgroup.id.11340")] <- "Eubacterium ruminantium Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumventriosumgroup.id.11341")] <- "ID = 11341"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumventriosumgroup.id.11341")] <- "Eubacterium ventriosum Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumxylanophilumgroup.id.14375")] <- "ID = 14375"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Eubacteriumxylanophilumgroup.id.14375")] <- "Eubacterium xylanophilum Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Ruminococcusgauvreauiigroup.id.11342")] <- "ID = 11342"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Ruminococcusgauvreauiigroup.id.11342")] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Ruminococcusgnavusgroup.id.14376")] <- "ID = 14376"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Ruminococcusgnavusgroup.id.14376")] <- "Ruminococcus gnavus Group (Species in Ruminococcus Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Ruminococcustorquesgroup.id.14377")] <- "ID = 14377"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus..Ruminococcustorquesgroup.id.14377")] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("genus.*.id.*"), data$exposure_formatted)))] <- sub("genus\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("genus.*.id.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("genus.*.id.*"), data$exposure_formatted)))] <- sub("genus\\.(\\w+)\\.id.*", "\\1 (Genus)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("genus.*.id.*"), data$exposure_formatted)))])
unique(data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*.id.*"), data$exposure_formatted)))])
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "family.BacteroidalesS24.7group.id.11173")] <- "ID = 11173"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "family.BacteroidalesS24.7group.id.11173"))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus.ChristensenellaceaeR.7group.id.11283")] <- "ID = 11283"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus.ChristensenellaceaeR.7group.id.11283"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus.Escherichia.Shigella.id.3504")] <- "ID = 3504"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus.Escherichia.Shigella.id.3504"))] <- "Escherichia Shigella (Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus.Eubacteriumventriosum.group.id.11341")] <- "ID = 11341"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus.Eubacteriumventriosum.group.id.11341")] <- "Eubacterium ventriosum Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus.Ruminococcusgauvreauii.group.id.11342")] <- "ID = 11342"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "genus.Ruminococcusgauvreauii.group.id.11342")] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "BacteroidalesS24.7group.id.11173"))] <- "ID = 11173"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "BacteroidalesS24.7group.id.11173"))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "ClostridialesvadinBB60group.id.11286"))] <- "ID = 11286"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ClostridialesvadinBB60group.id.11286"))] <- "Clostridiales vadin BB60 Group (Family in Clostridiales Order)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "familyXI.id.1936"))] <- "ID = 1936"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "familyXI.id.1936"))] <- "XI (Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "familyXIII.id.1957"))] <- "ID = 1957"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "familyXIII.id.1957"))] <- "XIII (Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Clostridiuminnocuumgroup.id.14397")] <- "ID = 14397"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Clostridiuminnocuumgroup.id.14397")] <- "Clostridium innocuum Group (Species in Clostridium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumbrachygroup.id.11296")] <- "ID = 11296"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumbrachygroup.id.11296")] <- "Eubacterium brachy Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumcoprostanoligenesgroup.id.11375")] <- "ID = 11375"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumcoprostanoligenesgroup.id.11375")] <- "Eubacterium coprostanoligenes Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumeligensgroup.id.14372")] <- "ID = 14372"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumeligensgroup.id.14372")] <- "Eubacterium eligens Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumfissicatenagroup.id.14373")] <- "ID = 14373"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumfissicatenagroup.id.14373")] <- "Eubacterium fissicatena Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumhalliigroup.id.11338")] <- "ID = 11338"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumhalliigroup.id.11338")] <- "Eubacterium hallii Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumnodatumgroup.id.11297")] <- "ID = 11297"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumnodatumgroup.id.11297")] <- "Eubacterium nodatum Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumoxidoreducensgroup.id.11339")] <- "ID = 11339"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumoxidoreducensgroup.id.11339")] <- "Eubacterium oxidoreducens Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumrectalegroup.id.14374")] <- "ID = 14374"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumrectalegroup.id.14374")] <- "Eubacterium rectale Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumruminantiumgroup.id.11340")] <- "ID = 11340"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumruminantiumgroup.id.11340")] <- "Eubacterium ruminantium Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumventriosumgroup.id.11341")] <- "ID = 11341"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumventriosumgroup.id.11341")] <- "Eubacterium ventriosum Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumxylanophilumgroup.id.14375")] <- "ID = 14375"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Eubacteriumxylanophilumgroup.id.14375")] <- "Eubacterium xylanophilum Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcusgauvreauiigroup.id.11342")] <- "ID = 11342"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcusgauvreauiigroup.id.11342")] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcusgnavusgroup.id.14376")] <- "ID = 14376"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcusgnavusgroup.id.14376")] <- "Ruminococcus gnavus Group (Species in Ruminococcus Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcustorquesgroup.id.14377")] <- "ID = 14377"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcustorquesgroup.id.14377")] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "CandidatusSoleaferrea.id.11350"))] <- "ID = 11350"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "CandidatusSoleaferrea.id.11350"))] <- "Candidatus Soleaferrea (Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "ChristensenellaceaeR.7group.id.11283")] <- "ID = 11283"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ChristensenellaceaeR.7group.id.11283"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "Clostridiumsensustricto1.id.1873"))] <- "ID = 1873"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiumsensustricto1.id.1873"))] <- "Clostridium sensu stricto1 (Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "DefluviitaleaceaeUCG011.id.11287"))] <- "ID = 11287"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "DefluviitaleaceaeUCG011.id.11287"))] <- "UCG011 Group (Genus in Defluviitaleaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "ErysipelotrichaceaeUCG003.id.11384"))] <- "ID = 11384" 
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ErysipelotrichaceaeUCG003.id.11384"))] <- "UCG003 Group (Genus in Erysipelotrichaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Escherichia.Shigella.id.3504")] <- "ID = 3504"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Escherichia.Shigella.id.3504"))] <- "Escherichia Shigella (Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "FamilyXIIIAD3011group.id.11293"))] <- "ID = 11293"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIAD3011group.id.11293"))] <- "XIII AD3011 Group (Genus in Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "FamilyXIIIUCG001.id.11294"))] <- "ID = 11294"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIUCG001.id.11294"))] <- "XII UCG001 Group (Genus in Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "LachnospiraceaeFCS020group.id.11314"))] <- "ID = 11314"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeFCS020group.id.11314"))] <- "FCS020 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "LachnospiraceaeNC2004group.id.11316"))] <- "ID = 11316"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeNC2004group.id.11316"))] <- "NC2004 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "LachnospiraceaeND3007group.id.11317"))] <- "ID = 11317"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeND3007group.id.11317"))] <- "ND3007 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "LachnospiraceaeNK4A136group.id.11319"))] <- "ID = 11319"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeNK4A136group.id.11319"))] <- "NK4A136 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "LachnospiraceaeUCG001.id.11321"))] <- "ID = 11321"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeUCG001.id.11321"))] <- "UCG001 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "LachnospiraceaeUCG004.id.11324"))] <- "ID = 11324"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeUCG004.id.11324"))] <- "UCG004 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "LachnospiraceaeUCG008.id.11328"))] <- "ID = 11328"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeUCG008.id.11328"))] <- "UCG008 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "LachnospiraceaeUCG010.id.11330"))] <- "ID = 11330"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeUCG010.id.11330"))] <- "UCG010 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RikenellaceaeRC9gutgroup.id.11191"))] <- "ID = 11191"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RikenellaceaeRC9gutgroup.id.11191"))] <- "RC9 Gut Group (Genus in Rikenellaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeNK4A214group.id.11358"))] <- "ID = 11358"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeNK4A214group.id.11358"))] <- "NK4A214 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG002.id.11360"))] <- "ID = 11360"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG002.id.11360"))] <- "UCG002 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG003.id.11361"))] <- "ID = 11361"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG003.id.11361"))] <- "UCG003 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG004.id.11362"))] <- "ID = 11362"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG004.id.11362"))] <- "UCG004 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG005.id.11363"))] <- "ID = 11363"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG005.id.11363"))] <- "UCG005 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG009.id.11366"))] <- "ID = 11366"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG009.id.11366"))] <- "UCG009 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG010.id.11367"))] <- "ID = 11367"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG010.id.11367"))] <- "UCG010 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG011.id.11368"))] <- "ID = 11368"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG011.id.11368"))] <- "UCG011 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG013.id.11370"))] <- "ID = 11370"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG013.id.11370"))] <- "UCG013 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "RuminococcaceaeUCG014.id.11371"))] <- "ID = 11371"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeUCG014.id.11371"))] <- "UCG014 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*.id.*"), data$exposure_formatted)))] <- sub("(\\w+).id.(\\w+)", "ID = \\2", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*.id.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*.id.*"), data$exposure_formatted)))] <- sub("(\\w+).id.*", "\\1", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*.id.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Phylum_*"), data$exposure_formatted)))] <- sub("Phylum_(\\w+)", "\\1 \\(Phylum\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Phylum_*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Class_*"), data$exposure_formatted)))] <- sub("Class_(\\w+)", "\\1 \\(Class\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Class_*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Order_*"), data$exposure_formatted)))] <- sub("Order_(\\w+)", "\\1 \\(Order\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Order_*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Family_*"), data$exposure_formatted)))] <- sub("Family_(\\w+)", "\\1 \\(Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Family_*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Genus_*"), data$exposure_formatted)))] <- sub("Genus_(\\w+)", "\\1 \\(Genus\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Genus_*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("phylum.*"), data$exposure_formatted)))] <- sub("phylum.(\\w+)", "\\1 \\(Phylum\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("phylum.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("class.*"), data$exposure_formatted)))] <- sub("class.(\\w+)", "\\1 \\(Class\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("class.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("order.*"), data$exposure_formatted)))] <- sub("order.(\\w+)", "\\1 \\(Order\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("order.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("family.*"), data$exposure_formatted)))] <- sub("family.(\\w+)", "\\1 \\(Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("family.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("genus.*"), data$exposure_formatted)))] <- sub("genus.(\\w+)", "\\1 \\(Genus\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("genus.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("phylum*"), data$exposure_formatted)))] <- sub("phylum(\\w+)", "\\1 \\(Phylum\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("phylum*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Genus  *"), data$exposure_formatted)))] <- sub("Genus  (\\w+)", "\\1 \\(Genus\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Genus  *"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Genus *"), data$exposure_formatted)))] <- sub("Genus (\\w+)", "\\1 \\(Genus\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Genus *"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Order *"), data$exposure_formatted)))] <- sub("Order (\\w+)", "\\1 \\(Order\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Order *"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Class *"), data$exposure_formatted)))] <- sub("Class (\\w+)", "\\1 \\(Class\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Class *"), data$exposure_formatted)))])
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "family.FamilyXIII.id.1957"))] <- "ID = 1957"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "family.FamilyXIII.id.1957"))] <- "XIII (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "family.FamilyXIII"))] <- "XIII (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "CandidatusSoleaferrea (Genus)"))] <- "Candidatus Soleaferrea (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiumsensustricto1 (Genus)"))] <- "Clostridium sensu stricto1 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "DefluviitaleaceaeUCG011 (Genus)"))] <- "UCG011 Group (Genus in Defluviitaleaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ErysipelotrichaceaeUCG003 (Genus)"))] <- "UCG003 Group (Genus in Erysipelotrichaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIAD3011group (Genus)"))] <- "XIII AD3011 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIUCG001 (Genus)"))] <- "XII UCG001 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Lachnospiraceae*group (Genus)"), data$exposure_formatted)))] <- sub("Lachnospiraceae(\\w+)group \\(Genus\\)", "\\1 Group \\(Genus in Lachnospiraceae Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Lachnospiraceae*group (Genus)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("LachnospiraceaeUCG* (Genus)"), data$exposure_formatted)))] <- sub("Lachnospiraceae(\\w+) \\(Genus\\)", "\\1 Group \\(Genus in Lachnospiraceae Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("LachnospiraceaeUCG* (Genus)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RikenellaceaeRC9gutgroup (Genus)"))] <- "RC9 Gut Group (Genus in Rikenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeNK4A214group (Genus)"))] <- "NK4A214 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("RuminococcaceaeUCG* (Genus)"), data$exposure_formatted)))] <- sub("Ruminococcaceae(\\w+) \\(Genus\\)", "\\1 Group \\(Genus in Ruminococcaceae Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("RuminococcaceaeUCG* (Genus)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcustorquesgroup (Genus)"))] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "BacteroidalesS24.7 (Family)"))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ClostridialesvadinBB60 (Family)"))] <- "Clostridiales vadin BB60 Group (Family in Clostridiales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ClostridialesvadinBB60group (Family)"))] <- "Clostridiales vadin BB60 Group (Family in Clostridiales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXI (Family)" ))] <- "XI (Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "FamilyXI (ID: 1936) (Family)" ))] <- "ID = 1936"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXI (ID: 1936) (Family)" ))] <- "XI (Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "FamilyXIII (ID: 1957) (Family)"))] <- "ID = 1957"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIII (ID: 1957) (Family)"))] <- "XIII (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Rikenellaceae.id (Family)" ))] <- "Rikenellaceae (Family)" 
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Unknown family \\(ID: *\\) \\(Family\\)"), data$exposure)))] <- sub("Unknown family \\(ID: (\\w+)\\) \\(Family\\)", "ID = \\1", data$exposure[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Unknown family \\(ID: *\\) \\(Family\\)"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Unknown family \\(ID: *\\) \\(Family\\)"), data$exposure)))] <- "Unknown Family"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Clostridiuminnocuum (Genus)")] <- "Clostridium innocuum Group (Species in Clostridium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Eubacterium* (Genus)"), data$exposure_formatted)))] <- sub("Eubacterium(\\w+) \\(Genus\\)", "Eubacterium \\1 Group \\(Species in Eubacterium Genus\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Eubacterium* (Genus)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcusgauvreauii (Genus)")] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcusgnavus (Genus)")] <- "Ruminococcus gnavus Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcustorques (Genus)")] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "ChristensenellaceaeR.7 (Genus)")] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Escherichia.Shigella (Genus)")] <- "Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIAD3011 (ID: 11293) (Genus)"))] <- "XIII AD3011 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIUCG001 (ID: 11294) (Genus)"))] <- "XII UCG001 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeFCS020group (Genus)"))] <- "FCS020 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeFCS020 (Genus)"))] <- "FCS020 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("LachnospiraceaeN*group (Genus)"), data$exposure_formatted)))] <- sub("Lachnospiraceae(\\w+)group \\(Genus\\)", "\\1 Group \\(Genus in Lachnospiraceae Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("LachnospiraceaeN*group (Genus)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("LachnospiraceaeN* (Genus)"), data$exposure_formatted)))] <- sub("Lachnospiraceae(\\w+) \\(Genus\\)", "\\1 Group \\(Genus in Lachnospiraceae Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("LachnospiraceaeN* (Genus)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RikenellaceaeRC9 (Genus)"))] <- "RC9 Gut Group (Genus in Rikenellaceae Family)" 
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeNK4A214 (Genus)"))] <- "NK4A214 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Unknown genus (ID: *) (Genus)"), data$exposure_formatted)))] <- sub("Unknown genus \\(ID: (\\w+)\\) \\(Genus\\)", "ID = \\1", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Unknown genus (ID: *) (Genus)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Unknown genus (ID: *) (Genus)"), data$exposure_formatted)))] <- "Unknown Genus"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*(class)"), data$exposure_formatted)))] <- sub("class", "Class", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*(class)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*(family)"), data$exposure_formatted)))] <- sub("family", "Family", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*(family)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*(order)"), data$exposure_formatted)))] <- sub("order", "Order", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*(order)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*(genus)"), data$exposure_formatted)))] <- sub("genus", "Genus", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("*(genus)"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("unknownfamily (Family)"), data$exposure_formatted)))] <- "Unknown Family"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("unknowngenus (Genus)"), data$exposure_formatted)))] <- "Unknown Genus"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("RuminococcaceaeUCG013"), data$exposure_formatted)))] <- "UCG013 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Eubacteriumcoprostanoligenes"), data$exposure_formatted)))] <- "Eubacterium coprostanoligenes Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("BacteroidalesS247group"), data$exposure_formatted)))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "CandidatusSoleaferrea"))] <- "Candidatus Soleaferrea (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ChristensenellaceaeR7group"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiuminnocuumgroup"))] <- "Clostridium innocuum Group (Species in Clostridium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiuminnocuum"))] <- "Clostridium innocuum Group (Species in Clostridium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiumsensustricto1"))] <- "Clostridium sensu stricto1 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "DefluviitaleaceaeUCG011"))] <- "UCG011 Group (Genus in Defluviitaleaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ErysipelotrichaceaeUCG003"))] <- "UCG003 Group (Genus in Erysipelotrichaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ErysipelotrichaceaeUCG003"))] <- "Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Eubacterium*group"), data$exposure_formatted)))] <- sub("Eubacterium(\\w+)group", "Eubacterium \\1 Group \\(Species in Eubacterium Genus\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("Eubacterium*group"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXI"))] <- "XI (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIII"))] <- "XIII (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIAD3011group"))] <- "XIII AD3011 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIUCG001"))] <- "XII UCG001 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("LachnospiraceaeUC*"), data$exposure_formatted)))] <- sub("Lachnospiraceae(\\w+)", "\\1 Group \\(Genus in Lachnospiraceae Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("LachnospiraceaeUC*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RikenellaceaeRC9gutgroup"))] <- "RC9 Gut Group (Genus in Rikenellaceae Family)" 
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeNK4A214group"))] <- "NK4A214 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("RuminococcaceaeUCG*"), data$exposure_formatted)))] <- sub("Ruminococcaceae(\\w+)", "\\1 Group \\(Genus in Ruminococcaceae Family\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (grepl(glob2rx("RuminococcaceaeUCG*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcusgauvreauiigroup")] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcusgnavusgroup")] <- "Ruminococcus gnavus Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcustorquesgroup")] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "unknowngenus")] <- "Unknown Genus"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "BacteroidalesS24 (Family).7group"))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Escherichia (Genus).Shigella"))] <- "Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ChristensenellaceaeR (Genus)"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (Genus) NC2004group"))] <- "NC2004 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Erysipelotrichaceae (Genus) UCG003"))] <- "UCG003 Group (Genus in Erysipelotrichaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcus torques")] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ChristensenellaceaeR (Genus)-7"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Species Clostridium innocuum")] <- "Clostridium innocuum Group (Species in Clostridium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Ruminococcaceae (Genus) UCG-010")] <- "UCG010 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Species Ruminococcus torques")] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & data$exposure == "Species Eubacterium hallii")] <- "Eubacterium hallii Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Bacteroidales S24-7"))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiaceae_1"))] <- "Clostridiaceae1"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "BacteroidalesS24.7"))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiaceae_1" ))] <- "Clostridiaceae1"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ClostridialesvadinBB60"))] <- "Clostridiales vadin BB60 Group (Family in Clostridiales Order)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXI (ID: 1936)"))] <- "ID = 1936"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXI (ID: 1936)"))] <- "XI (Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted =="FamilyXIII (ID: 1957)"))] <- "ID = 1937"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIII (ID: 1957)"))] <- "XIII (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Ruminococcustorquesgroup"))] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Ruminococcusgnavusgroup"))] <- "Ruminococcus gnavus Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Ruminococcusgauvreauiigroup"))] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumxylanophilumgroup"))] <- "Eubacterium xylanophilum Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumventriosumgroup"))] <- "Eubacterium ventriosum Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumruminantiumgroup"))] <- "Eubacterium ruminantium Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumrectalegroup"))] <- "Eubacterium rectale Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumoxidoreducensgroup"))] <- "Eubacterium oxidoreducens Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumnodatumgroup"))] <- "Eubacterium nodatum Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumhalliigroup"))] <- "Eubacterium hallii Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumfissicatenagroup"))] <- "Eubacterium fissicatena Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumeligensgroup"))] <- "Eubacterium eligens Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumcoprostanoligenesgroup"))] <- "Eubacterium coprostanoligenes Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Eubacteriumbrachygroup"))] <- "Eubacterium brachy Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "genus..Clostridiuminnocuumgroup"))] <- "Clostridium innocuum Group (Species in Clostridium Genus)"
eubacterium_list <- c("Eubacteriumbrachy","Eubacteriumeligens","Eubacteriumfissicatena","Eubacteriumhallii","Eubacteriumnodatum","Eubacteriumoxidoreducens","Eubacteriumrectale","Eubacteriumruminantium","Eubacteriumventriosum","Eubacteriumxylanophilum")
for(i in eubacterium_list){
  data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == i))] <- sub("Eubacterium(\\w+)", "Eubacterium \\1 Group \\(Species in Eubacterium Genus\\)", data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == i))]) 
}
rm(eubacterium_list, i)
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ClostridialesvadinBB60group"))] <- "Clostridiales vadin BB60 Group (Family in Clostridiales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "EscherichiaShigella"))] <- "Escherichia Shigella (Genus)"
genus_list <- c("Bifidobacterium","Ruminococcus","Allisonella","Romboutsia","Tyzzerella3","Oxalobacter","Ruminococcus1","Intestinibacter","Streptococcus","Enterorhabdus","Erysipelatoclostridium","Actinomyces","Adlercreutzia","Akkermansia","Alistipes","Alloprevotella","Anaerofilum","Anaerostipes","Anaerotruncus","Bacteroides","Barnesiella","Bilophila","Blautia","Butyricicoccus","Butyricimonas","Butyrivibrio","Catenibacterium","Collinsella","Coprobacter","Coprococcus1","Coprococcus2","Coprococcus3","Desulfovibrio","Dialister","Dorea","Eggerthella","Eisenbergiella","Faecalibacterium","Flavonifractor","Fusicatenibacter","Gordonibacter","Haemophilus","Holdemanella","Holdemania","Howardella","Hungatella","Intestinimonas","Lachnoclostridium","Lachnospira","Lactobacillus","Lactococcus","Marvinbryantia","Methanobrevibacter","Odoribacter","Olsenella","Oscillibacter","Oscillospira","Parabacteroides","Paraprevotella","Parasutterella","Peptococcus","Phascolarctobacterium","Prevotella7","Prevotella9","Roseburia","Ruminiclostridium5","Ruminiclostridium6","Ruminiclostridium9","Ruminococcus2","Sellimonas","Senegalimassilia","Slackia","Subdoligranulum","Sutterella","Terrisporobacter","Turicibacter","Veillonella","Victivallis","Campylobacter","Candida","Shigella","Eubacterium","Ruminiclostridium","Prevotella")
for(i in genus_list){
  data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == i))] <- paste(i, "(Genus)", sep = " ")
}
rm(genus_list, i)
phylum_list <- c("Actinobacteria","Melainabacteria","Bacteroidetes","Cyanobacteria","Euryarchaeota","Firmicutes","Lentisphaerae","Proteobacteria","Tenericutes","Verrucomicrobia")
for(i in phylum_list){
  data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == i))] <- paste(i, "(Phylum)", sep = " ")
}
rm(phylum_list, i)
family_list <- c("Acidaminococcaceae","Actinomycetaceae","Alcaligenaceae","Bacteroidaceae","Bifidobacteriaceae","Christensenellaceae","Clostridiaceae1","Coriobacteriaceae","Defluviitaleaceae","Desulfovibrionaceae","Enterobacteriaceae","Erysipelotrichaceae","Lachnospiraceae","Lactobacillaceae","Methanobacteriaceae","Oxalobacteraceae","Pasteurellaceae","Peptococcaceae","Peptostreptococcaceae","Porphyromonadaceae","Prevotellaceae","Rhodospirillaceae","Rikenellaceae","Ruminococcaceae","Streptococcaceae","Veillonellaceae","Verrucomicrobiaceae","Victivallaceae")
for(i in family_list){
  data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == i))] <- paste(i, "(Family)", sep = " ")
}
rm(family_list, i)
order_list <- c("Actinomycetales","Bacillales","Bacteroidales","Bifidobacteriales","Burkholderiales","Clostridiales","Coriobacteriales","Desulfovibrionales","Enterobacteriales","Erysipelotrichales","Gastranaerophilales","Lactobacillales","Methanobacteriales","MollicutesRF9","NB1n","Pasteurellales","Rhodospirillales","Selenomonadales","Verrucomicrobiales","Victivallales")
for(i in order_list){
  data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == i))] <- paste(i, "(Order)", sep = " ")
}
rm(order_list, i)
class_list <- c("Alphaproteobacteria","Bacilli","Bacteroidia","Betaproteobacteria","Clostridia","Coriobacteriia","Deltaproteobacteria","Erysipelotrichia","Gammaproteobacteria","Lentisphaeria","Methanobacteria","Mollicutes","Negativicutes","Verrucomicrobiae")
for(i in class_list){
  data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == i))] <- paste(i, "(Class)", sep = " ")
}
rm(class_list, i)
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ClostridialesvadinBB60group"))] <- "Clostridiales vadin BB60 Group (Family in Clostridiales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "EscherichiaShigella"))] <- "Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeFCS020group"))] <- "FCS020 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeNC2004group"))] <- "NC2004 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeND3007group"))] <- "ND3007 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeNK4A136group"))] <- "NK4A136 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcustorques (Genus)"))] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Family Actinomycetaceae"))] <- "Actinomycetaceae (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Family Clostridiaceae1"))] <- "Clostridiaceae1 (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Family Prevotellaceae"))] <- "Prevotellaceae (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Phylum Actinobacteria"))] <- "Actinobacteria (Phylum)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (Genus) UCG-010"))] <- "UCG010 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Family Methanobacteriaceae"))] <- "Methanobacteriaceae (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Family Christensenellaceae"))] <- "Christensenellaceae (Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiales vadin BB60"))] <- "Clostridiales vadin BB60 Group (Family in Clostridiales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcusgauvreauii"))] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcusgnavus"))] <- "Ruminococcus gnavus Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcustorques"))] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ChristensenellaceaeR.7"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Escherichia.Shigella"))] <- "Escherichia Shigella (Genus)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "FamilyXIIIAD3011 (ID: 11293)"))] <- "ID = 11293"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIAD3011 (ID: 11293)"))] <- "XIII AD3011 Group (Genus in Family)"
data$exposure_info[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure == "FamilyXIIIUCG001 (ID: 11294)"))] <- "ID = 11294"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "FamilyXIIIUCG001 (ID: 11294)"))] <- "XII UCG001 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeFCS020"))] <- "FCS020 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeNC2004"))] <- "NC2004 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeND3007"))] <- "ND3007 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "LachnospiraceaeNK4A136"))] <- "NK4A136 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RikenellaceaeRC9"))] <- "RC9 Gut Group (Genus in Rikenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "RuminococcaceaeNK4A214"))] <- "NK4A214 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Gut microbiome (as a whole)"))] <- "Gut microbiome"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "unknownfamily"))] <- "Unknown Family"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "BacteroidalesS24_7group"))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ChristensenellaceaeR_7group"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Escherichia_Shigella"))] <- "Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Candidatus Soleaferrea"))] <- "Candidatus Soleaferrea (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Christensenellaceae (R7 group)"))] <- "R7 Group (Genus in Christensenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridium (innocuum group)"))] <- "Clostridium innocuum Group (Species in Clostridium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Clostridiumsensustricto 1"))] <- "Clostridium sensu stricto1 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Coprococcus 1"))] <- "Coprococcus1 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Coprococcus 2"))] <- "Coprococcus2 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Coprococcus 3"))] <- "Coprococcus3 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Defluviitaleaceae (UCG011)"))] <- "UCG011 Group (Genus in Defluviitaleaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Erysipelotrichaceae (UCG003)"))] <- "UCG003 Group (Genus in Erysipelotrichaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Escherichia Shigella"))] <- "Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (brachy group)"))] <- "Eubacterium brachy Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (coprostanoligenes group)"))] <- "Eubacterium coprostanoligenes Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (eligens group)"))] <- "Eubacterium eligens Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (fissicatena group)"))] <- "Eubacterium fissicatena Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (hallii group)"))] <- "Eubacterium hallii Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (nodatum group)"))] <- "Eubacterium nodatum Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (oxidoreducens group)"))] <- "Eubacterium oxidoreducens Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (rectale group)"))] <- "Eubacterium rectale Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (ruminantium group)"))] <- "Eubacterium ruminantium Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (ventriosum group)"))] <- "Eubacterium ventriosum Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium (xylanophilum group)"))] <- "Eubacterium xylanophilum Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Family XIII (AD3011 group)"))] <- "XIII AD3011 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Family XIII (UCG001)"))] <- "XII UCG001 Group (Genus in Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (FCS020 group)"))] <- "FCS020 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (NC2004 group)"))] <- "NC2004 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (ND3007 group)"))] <- "ND3007 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (NK4A136 group)"))] <- "NK4A136 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (UCG001)"))] <- "UCG001 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (UCG004)"))] <- "UCG004 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (UCG008)"))] <- "UCG008 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Lachnospiraceae (UCG010)"))] <- "UCG010 Group (Genus in Lachnospiraceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Prevotella 7"))] <- "Prevotella7 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Prevotella 9"))] <- "Prevotella9 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Rikenellaceae (RC9 gut group)"))] <- "RC9 Gut Group (Genus in Rikenellaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminiclostridium 5"))] <- "Ruminiclostridium5 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminiclostridium 6"))] <- "Ruminiclostridium6 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminiclostridium 9"))] <- "Ruminiclostridium9 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (NK4A214 group)"))] <- "NK4A214 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG002)"))] <- "UCG002 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG003)"))] <- "UCG003 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG004)"))] <- "UCG004 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG005)"))] <- "UCG005 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG009)"))] <- "UCG009 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG010)"))] <- "UCG010 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG011)"))] <- "UCG011 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG013)"))] <- "UCG013 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae (UCG014)"))] <- "UCG014 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcus (gauvreauii group)"))] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcus (gnavus group)"))] <- "Ruminococcus gnavus Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcus (torques group)"))] <- "Ruminococcus torques Group (Species in Ruminococcus Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcus 1"))] <- "Ruminococcus1 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcus 2"))] <- "Ruminococcus2 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Tyzzerella 3"))] <- "Tyzzerella3 (Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae_UCG003"))] <- "UCG003 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium ruminantium group"))] <- "Eubacterium ruminantium Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Eubacterium ventriosum group"))] <- "Eubacterium ventriosum Group (Species in Eubacterium Genus)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "Ruminococcaceae UCG002"))] <- "UCG002 Group (Genus in Ruminococcaceae Family)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "BacteroidalesS24.7group"))] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_formatted[which((data$exposure_study == "Kurilshikov 2021") & (data$exposure_formatted == "ChristensenellaceaeR.7group"))] <- "R7 Group (Genus in Christensenellaceae Family)"
table(unique(data$exposure_formatted[data$exposure_study == "Kurilshikov 2021"]))
table(unique(data$exposure_info[data$exposure_study == "Kurilshikov 2021"]))
length(data$exposure_formatted[data$exposure_study == "Kurilshikov 2021"])
length(unique(data$exposure_formatted[data$exposure_study == "Kurilshikov 2021"]))
sum(is.na(data$exposure_formatted[data$exposure_study == "Kurilshikov 2021"]))
sum(is.na(data$exposure_info[data$exposure_study == "Kurilshikov 2021"]))
data[which((data$exposure_study == "Kurilshikov 2021")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Lopera-Maya 2022 ##
table(data$ID[data$exposure_study == "Lopera-Maya 2022"]) # 1 study (1828507192)
data[data$exposure_study == "Lopera-Maya 2022",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Lopera-Maya 2022"])

# First study (ID = 1828507192) 
data$exposure_unit[data$ID == "1828507192"]
unique(data$exposure_study[data$ID == "1828507192"])
length(data$exposure[data$ID == "1828507192"])
length(unique(data$exposure[data$ID == "1828507192"]))
table(unique(data$exposure[data$ID == "1828507192"])) 
# Any changes
data$exposure_formatted[which(data$ID == "1828507192")] <- data$exposure[which(data$ID == "1828507192")]
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "ASPASN.PWY..superpathway.of.L.aspartate.and.L.asparagine.biosynthesis"))] <- "ASPASN-PWY (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "ASPASN.PWY..superpathway.of.L.aspartate.and.L.asparagine.biosynthesis"))] <- "Superpathway of L-aspartate and L-asparagine biosynthesis"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.5088..L.glutamate.degradation.VIII..to.propanoate."))] <- "PWY-5088 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.5088..L.glutamate.degradation.VIII..to.propanoate."))] <- "L-glutamate degradation VIII to propanoate"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.6612..superpathway.of.tetrahydrofolate.biosynthesis"))] <- "PWY-6612 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.6612..superpathway.of.tetrahydrofolate.biosynthesis"))] <- "Superpathway of tetrahydrofolate biosynthesis"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.7234..inosine.5..phosphate.biosynthesis.III"))] <- "PWY-7234 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.7234..inosine.5..phosphate.biosynthesis.III"))] <- "Inosine-5-phosphate biosynthesis III"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.6151..S.adenosyl.L.methionine.cycle.I"))] <- "PWY-6151 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.6151..S.adenosyl.L.methionine.cycle.I"))] <- "S-adenosyl-L-methionine cycle I"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.841..superpathway.of.purine.nucleotides.de.novo.biosynthesis.I"))] <- "PWY-841 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.841..superpathway.of.purine.nucleotides.de.novo.biosynthesis.I"))] <- "Superpathway of purine nucleotides den novo biosynthesis I"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PYRIDNUCSYN.PWY..NAD.biosynthesis.I..from.aspartate."))] <- "PYRIDNUCSYN-PWY (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PYRIDNUCSYN.PWY..NAD.biosynthesis.I..from.aspartate."))] <- "NAD biosynthesis I from aspartate"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "GLYCOCAT.PWY..glycogen.degradation.I..bacterial."))] <- "GLYCOCAT-PWY (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "GLYCOCAT.PWY..glycogen.degradation.I..bacterial."))] <- "Glycogen degradation I (bacterial)"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "HISDEG.PWY..L.histidine.degradation.I"))] <- "HISDEG-PWY (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "HISDEG.PWY..L.histidine.degradation.I"))] <- "L-histidine degradation I"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "HISTSYN.PWY..L.histidine.biosynthesis"))] <- "HISTSYN-PWY (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "HISTSYN.PWY..L.histidine.biosynthesis"))] <- "L-histidine biosynthesis"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "LACTOSECAT.PWY..lactose.and.galactose.degradation.I"))] <- "LACTOSECAT-PWY (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "LACTOSECAT.PWY..lactose.and.galactose.degradation.I"))] <- "Lactose and galactose degradation I"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.6147..6.hydroxymethyl.dihydropterin.diphosphate.biosynthesis.I"))] <- "PWY-6147 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.6147..6.hydroxymethyl.dihydropterin.diphosphate.biosynthesis.I"))] <- "6-hydroxymethyl-dihydropterin-diphosphate biosynthesis I"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.6353..purine.nucleotides.degradation.II..aerobic."))] <- "PWY-6353 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.6353..purine.nucleotides.degradation.II..aerobic."))] <- "Purine nucleotides degradation II (aerobic)"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY_REDCITCYC..TCA.cycle.VIII..helicobacter."))] <- "PWY-REDCITCYC (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY_REDCITCYC..TCA.cycle.VIII..helicobacter."))] <- "TCA cycle VIII (helicobacter)"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "TRPSYN.PWY..L.tryptophan.biosynthesis"))] <- "TRPSYN-PWY (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "TRPSYN.PWY..L.tryptophan.biosynthesis"))] <- "L-tryptophan biosynthesis"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.6708..ubiquinol.8.biosynthesis..prokaryotic."))] <- "PWY-6708 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.6708..ubiquinol.8.biosynthesis..prokaryotic."))] <- "Ubiquinol-8 biosynthesis (prokaryotic)"
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure == "PWY.7209..superpathway.of.pyrimidine.ribonucleosides.degradation"))] <- "PWY-7209 (Pathway)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure == "PWY.7209..superpathway.of.pyrimidine.ribonucleosides.degradation"))] <- "Superpathway of pyrimidine ribonucleosides degradation"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure_formatted == "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Rikenellaceae.g__Alistipes.s__Alistipes_sp_AP11"))] <-  sub("^k__(.*)\\.p__(.*)\\.c__(.*)\\.o__(.*)\\.f__(.*)\\.g__(.*)\\.s__(.*)", "\\1 (Kingdom) \\2 (Phylum) \\3 (Class) \\4 (Order) \\5 (Family) \\6 (Genus)", data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure_formatted == "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Rikenellaceae.g__Alistipes.s__Alistipes_sp_AP11"))])
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure_formatted == "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Rikenellaceae.g__Alistipes.s__Alistipes_sp_AP11"))] <- "Alistipes AP11 (Species)"
data$exposure_info[which((data$ID == "1828507192") & (data$exposure_formatted == "k__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacteriales.f__Enterobacteriaceae.g__Escherichia.s__Escherichia_unclassified"))] <-  sub("^k__(.*)\\.p__(.*)\\.c__(.*)\\.o__(.*)\\.f__(.*)\\.g__(.*)\\.s__(.*)", "\\1 (Kingdom) \\2 (Phylum) \\3 (Class) \\4 (Order) \\5 (Family) \\6 (Genus)", data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure_formatted == "k__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacteriales.f__Enterobacteriaceae.g__Escherichia.s__Escherichia_unclassified"))])
data$exposure_formatted[which((data$ID == "1828507192") & (data$exposure_formatted == "k__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacteriales.f__Enterobacteriaceae.g__Escherichia.s__Escherichia_unclassified"))] <- "Escherichia (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "1828507192") & (grepl("s__", data$exposure_formatted)))] <-  sub("^k__(.*)\\.p__(.*)\\.c__(.*)\\.o__(.*)\\.f__(.*)\\.g__(.*)\\.s__(.*)", "\\1 (Kingdom) \\2 (Phylum) \\3 (Class) \\4 (Order) \\5 (Family) \\6 (Genus)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("s__", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1828507192") & (grepl("s__", data$exposure_formatted)))] <- sub(".*s__(\\w+)_(\\w+)", "\\1 \\2 (Species)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("s__", data$exposure_formatted)))])
data$exposure_info[which((data$ID == "1828507192") & (grepl("g__", data$exposure_formatted)))] <-  sub("^k__(.*)\\.p__(.*)\\.c__(.*)\\.o__(.*)\\.f__(.*)\\.g__(.*)", "\\1 (Kingdom) \\2 (Phylum) \\3 (Class) \\4 (Order) \\5 (Family)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("g__", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1828507192") & (grepl("g__", data$exposure_formatted)))] <- sub(".*g__(\\w+)", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("g__", data$exposure_formatted)))])
data$exposure_info[which((data$ID == "1828507192") & (grepl("f__", data$exposure_formatted)))] <-  sub("^k__(.*)\\.p__(.*)\\.c__(.*)\\.o__(.*)\\.f__(.*)", "\\1 (Kingdom) \\2 (Phylum) \\3 (Class) \\4 (Order)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("f__", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1828507192") & (grepl("f__", data$exposure_formatted)))] <- sub(".*f__(\\w+)", "\\1 (Family)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("f__", data$exposure_formatted)))])
data$exposure_info[which((data$ID == "1828507192") & (grepl("o__", data$exposure_formatted)))] <-  sub("^k__(.*)\\.p__(.*)\\.c__(.*)\\.o__(.*)", "\\1 (Kingdom) \\2 (Phylum) \\3 (Class)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("o__", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1828507192") & (grepl("o__", data$exposure_formatted)))] <- sub(".*o__(\\w+)", "\\1 (Order)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("o__", data$exposure_formatted)))])
data$exposure_info[which((data$ID == "1828507192") & (grepl("c__", data$exposure_formatted)))] <-  sub("^k__(.*)\\.p__(.*)\\.c__(.*)", "\\1 (Kingdom) \\2 (Phylum)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("c__", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1828507192") & (grepl("c__", data$exposure_formatted)))] <- sub(".*c__(\\w+)", "\\1 (Class)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("c__", data$exposure_formatted)))])
data$exposure_info[which((data$ID == "1828507192") & (grepl("p__", data$exposure_formatted)))] <-  sub("^k__(.*)\\.p__(.*)", "\\1 (Kingdom)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("p__", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1828507192") & (grepl("p__", data$exposure_formatted)))] <- sub(".*p__(\\w+)", "\\1 (Phylum)", data$exposure_formatted[which((data$ID == "1828507192") & (grepl("p__", data$exposure_formatted)))])
table(unique(data$exposure_formatted[data$ID == "1828507192"]))
table(unique(data$exposure_info[data$ID == "1828507192"]))
length(data$exposure_formatted[data$ID == "1828507192"])
length(unique(data$exposure_formatted[data$ID == "1828507192"]))
sum(is.na(data$exposure_formatted[data$ID == "1828507192"]))
sum(is.na(data$exposure_info[data$ID == "1828507192"]))
data[which((data$ID == "1828507192")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Mixture of five studies (Kurilshikov, Ruhlemann for microbial traits and Sanna, Rhee, Kettunen for metabolites) ##
table(data$ID[data$exposure_study == "Mixture of five studies (Kurilshikov, Ruhlemann for microbial traits and Sanna, Rhee, Kettunen for metabolites)"]) # 1 study (Other sources)
data[data$exposure_study == "Mixture of five studies (Kurilshikov, Ruhlemann for microbial traits and Sanna, Rhee, Kettunen for metabolites)",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Mixture of five studies (Kurilshikov, Ruhlemann for microbial traits and Sanna, Rhee, Kettunen for metabolites)"])

# First study (ID = Other sources) 
data$exposure_unit[data$ID == "Other sources"]
unique(data$exposure_study[data$ID == "Other sources"])
length(data$exposure[data$ID == "Other sources"])
length(unique(data$exposure[data$ID == "Other sources"]))
table(unique(data$exposure[data$ID == "Other sources"])) 
# Any changes
data$exposure_formatted[which(data$ID == "Other sources")] <- data$exposure[which(data$ID == "Other sources")]
data$exposure_formatted[which((data$ID == "Other sources") & (grepl("G-", data$exposure_formatted)))] <- sub("G-(\\w+).*", "\\1 (Unclassified Genus in Family)", data$exposure_formatted[which((data$ID == "Other sources") & (grepl("G-", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl("OTU", data$exposure_formatted)))] <- sub("(\\w+) \\(genus-(\\w+)\\)", "\\1 (in \\2 Genus)", data$exposure_formatted[which((data$ID == "Other sources") & (grepl("OTU", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl("OTU", data$exposure_formatted)))] <- sub("prevotella", "Prevotella", data$exposure_formatted[which((data$ID == "Other sources") & (grepl("OTU", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl("OTU", data$exposure_formatted)))] <- sub("ruminococcus", "Ruminococcus", data$exposure_formatted[which((data$ID == "Other sources") & (grepl("OTU", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "Other sources") & (data$exposure_formatted == "TestASV-18 (genus-prevotella)"))] <- "TestASV-18 (in Prevotella Genus)"
data$exposure_formatted[which((data$ID == "Other sources") & (data$exposure_formatted == "TestASV-38 (genus-ruminococcus)"))] <- "TestASV-38 (in Ruminococcus Genus)"
data$exposure_info[which((data$ID == "Other sources") & (grepl(glob2rx("class.*.id.*"), data$exposure)))] <- sub("class\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure[which((data$ID == "Other sources") & (grepl(glob2rx("class.*.id.*"), data$exposure)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("class.*.id.*"), data$exposure)))] <- sub("class\\.(\\w+)\\.id.*", "\\1 (Class)", data$exposure[which((data$ID == "Other sources") & (grepl(glob2rx("class.*.id.*"), data$exposure)))])
data$exposure_info[which((data$ID == "Other sources") & (grepl(glob2rx("family.unknownfamily.id.*"), data$exposure)))] <- sub("family.unknownfamily.id.(\\w+)", "ID = \\1", data$exposure[which((data$ID == "Other sources") & (grepl(glob2rx("family.unknownfamily.id.*"), data$exposure)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("family.unknownfamily.id.*"), data$exposure)))] <- "Unknown Family"
data$exposure_info[which((data$ID == "Other sources") & (grepl(glob2rx("family*.id.*"), data$exposure_formatted)))] <- sub("family\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("family*.id.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("family*.id.*"), data$exposure_formatted)))] <- sub("family\\.(\\w+)\\.id.*", "\\1 (Family)", data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("family*.id.*"), data$exposure_formatted)))])
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "family.BacteroidalesS24.7group.id.11173")] <- "ID = 11173"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "family.BacteroidalesS24.7group.id.11173")] <- "S24_7 Group (Family in Bacteroidales Order)"
data$exposure_info[which((data$ID == "Other sources") & (grepl(glob2rx("genus.unknowngenus.id.*"), data$exposure)))] <- sub("genus.unknowngenus.id.(\\w+)", "ID = \\1", data$exposure[which((data$ID == "Other sources") & (grepl(glob2rx("genus.unknowngenus.id.*"), data$exposure)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("genus.unknowngenus.id.*"), data$exposure)))] <- "Unknown Genus"
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "genus..Eubacteriumventriosumgroup.id.11341")] <- "ID = 11341"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "genus..Eubacteriumventriosumgroup.id.11341")] <- "Eubacterium ventriosum Group (Species in Eubacterium Genus)"
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "genus..Ruminococcusgauvreauiigroup.id.11342")] <- "ID = 11342"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "genus..Ruminococcusgauvreauiigroup.id.11342")] <- "Ruminococcus gauvreauii Group (Species in Ruminococcus Genus)"
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "genus.FamilyXIIIAD3011group.id.11293")] <- "ID = 11293"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "genus.FamilyXIIIAD3011group.id.11293")] <- "XIII AD3011 Group (Genus in Family)"
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "genus.LachnospiraceaeUCG008.id.11328")] <- "ID = 11328"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "genus.LachnospiraceaeUCG008.id.11328")] <- "UCG008 Group (Genus in Lachnospiraceae Family)"
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "genus.RuminococcaceaeNK4A214group.id.11358")] <- "ID = 11358"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "genus.RuminococcaceaeNK4A214group.id.11358")] <- "NK4A214 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "genus.RuminococcaceaeUCG002.id.11360")] <- "ID = 11360"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "genus.RuminococcaceaeUCG002.id.11360")] <- "UCG002 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "genus.RuminococcaceaeUCG003.id.11361")] <- "ID = 11361"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "genus.RuminococcaceaeUCG003.id.11361")] <- "UCG003 Group (Genus in Ruminococcaceae Family)"
data$exposure_info[which((data$ID == "Other sources") & data$exposure_formatted == "genus.RuminococcaceaeUCG005.id.11363")] <- "ID = 11363"
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "genus.RuminococcaceaeUCG005.id.11363")] <- "UCG005 Group (Genus in Ruminococcaceae Family)" 
data$exposure_info[which((data$ID == "Other sources") & (grepl(glob2rx("genus*.id.*"), data$exposure_formatted)))] <- sub("genus\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("genus*.id.*"), data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("genus*.id.*"), data$exposure_formatted)))] <- sub("genus\\.(\\w+)\\.id.*", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("genus*.id.*"), data$exposure_formatted)))])
data$exposure_info[which((data$ID == "Other sources") & (grepl(glob2rx("order*.id.*"), data$exposure)))] <- sub("order\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure[which((data$ID == "Other sources") & (grepl(glob2rx("order*.id.*"), data$exposure)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("order*.id.*"), data$exposure)))] <- sub("order\\.(\\w+)\\.id.*", "\\1 (Order)", data$exposure[which((data$ID == "Other sources") & (grepl(glob2rx("order*.id.*"), data$exposure)))])
data$exposure_info[which((data$ID == "Other sources") & (grepl(glob2rx("phylum*.id.*"), data$exposure)))] <- sub("phylum\\.(\\w+).id.(\\w+)", "ID = \\2", data$exposure[which((data$ID == "Other sources") & (grepl(glob2rx("phylum*.id.*"), data$exposure)))])
data$exposure_formatted[which((data$ID == "Other sources") & (grepl(glob2rx("phylum*.id.*"), data$exposure)))] <- sub("phylum\\.(\\w+)\\.id.*", "\\1 (Phylum)", data$exposure[which((data$ID == "Other sources") & (grepl(glob2rx("phylum*.id.*"), data$exposure)))])
data$exposure_formatted[which((data$ID == "Other sources") & data$exposure_formatted == "Pathway PWY-5022")] <- "PWY-5022 (Pathway)"
table(unique(data$exposure_formatted[data$ID == "Other sources"]))
table(unique(data$exposure_info[data$ID == "Other sources"]))
length(data$exposure_formatted[data$ID == "Other sources"])
length(unique(data$exposure_formatted[data$ID == "Other sources"]))
sum(is.na(data$exposure_formatted[data$ID == "Other sources"]))
sum(is.na(data$exposure_info[data$ID == "Other sources"]))
data[which((data$ID == "Other sources")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Mixture of Goodrich and Davenport ##
table(data$ID[data$exposure_study == "Mixture of Goodrich and Davenport"]) # 1 study (1308760143)
data[data$exposure_study == "Mixture of Goodrich and Davenport",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Mixture of Goodrich and Davenport"])

# First study (ID = 1308760143) 
data$exposure_unit[data$ID == "1308760143"]
unique(data$exposure_study[data$ID == "1308760143"])
length(data$exposure[data$ID == "1308760143"])
length(unique(data$exposure[data$ID == "1308760143"]))
table(unique(data$exposure[data$ID == "1308760143"])) 
# Note that this study has already been handled
table(unique(data$exposure_formatted[data$ID == "1308760143"])) 
table(unique(data$exposure_info[data$ID == "1308760143"]))
length(data$exposure_formatted[data$ID == "1308760143"])
length(unique(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_info[data$ID == "1308760143"]))
data[which((data$ID == "1308760143")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Mixture of Goodrich, Bonder and Davenport ##
table(data$ID[data$exposure_study == "Mixture of Goodrich, Bonder and Davenport"]) # 1 study (1308760143 - same as above)
data[data$exposure_study == "Mixture of Goodrich, Bonder and Davenport",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Mixture of Goodrich, Bonder and Davenport"])

# First study (ID = 1308760143) 
data$exposure_unit[data$ID == "1308760143"]
unique(data$exposure_study[data$ID == "1308760143"])
length(data$exposure[data$ID == "1308760143"])
length(unique(data$exposure[data$ID == "1308760143"]))
table(unique(data$exposure[data$ID == "1308760143"])) 
# Note that this study has already been handled
table(unique(data$exposure_formatted[data$ID == "1308760143"]))
table(unique(data$exposure_info[data$ID == "1308760143"]))
length(data$exposure_formatted[data$ID == "1308760143"])
length(unique(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_info[data$ID == "1308760143"]))
data[which((data$ID == "1308760143")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Mixture of Wang, Goodrich and Turpin ##
table(data$ID[data$exposure_study == "Mixture of Wang, Goodrich and Turpin"]) # 1 study (1308760143 - same as above)
data[data$exposure_study == "Mixture of Wang, Goodrich and Turpin",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Mixture of Wang, Goodrich and Turpin"])

# First study (ID = 1308760143) 
data$exposure_unit[data$ID == "1308760143"]
unique(data$exposure_study[data$ID == "1308760143"])
length(data$exposure[data$ID == "1308760143"])
length(unique(data$exposure[data$ID == "1308760143"]))
table(unique(data$exposure[data$ID == "1308760143"])) 
# Note that this study has already been handled
table(unique(data$exposure_formatted[data$ID == "1308760143"]))
table(unique(data$exposure_info[data$ID == "1308760143"]))
length(data$exposure_formatted[data$ID == "1308760143"])
length(unique(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_info[data$ID == "1308760143"]))
data[which((data$ID == "1308760143")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## NA ##
table(data$ID[data$exposure_study == "NA"]) # 5 studies (261727485, 289377291 557846834, 878349061, 978492202)
data[data$exposure_study == "NA",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "NA"])

# First study (ID = 261727485) 
data$exposure_unit[data$ID == "261727485"]
unique(data$exposure_study[data$ID == "261727485"])
length(data$exposure[data$ID == "261727485"])
length(unique(data$exposure[data$ID == "261727485"]))
table(unique(data$exposure[data$ID == "261727485"])) 
# Abstract where the bacteria was not provided
data$exposure_formatted[which(data$ID == "261727485")] <- "Unknown Genus"
data$exposure_info[which(data$ID == "261727485")] <- "Abundance"
table(unique(data$exposure_formatted[data$ID == "261727485"]))
table(unique(data$exposure_info[data$ID == "261727485"]))
length(data$exposure_formatted[data$ID == "261727485"])
length(unique(data$exposure_formatted[data$ID == "261727485"]))
sum(is.na(data$exposure_formatted[data$ID == "261727485"]))
sum(is.na(data$exposure_info[data$ID == "261727485"]))
data[which((data$ID == "261727485")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 289377291) 
data$exposure_unit[data$ID == "289377291"]
unique(data$exposure_study[data$ID == "289377291"])
length(data$exposure[data$ID == "289377291"])
length(unique(data$exposure[data$ID == "289377291"]))
table(unique(data$exposure[data$ID == "289377291"])) 
# Reformat to be the same as others using similar data
data$exposure_formatted[which(data$ID == "289377291")] <- data$exposure[which(data$ID == "289377291")]
data$exposure_info[which((data$ID == "289377291") & grepl("abundance", data$exposure))] <- "Abundance"
data$exposure_info[which((data$ID == "289377291") & grepl("presence", data$exposure))] <- "Presence vs. absence"
data$exposure_formatted[which((data$ID == "289377291") & (data$exposure_formatted == "Butyricicoccus (abundance)"))] <- "Butyricicoccus (Genus)"
data$exposure_formatted[which((data$ID == "289377291") & (data$exposure_formatted == "Firmicutes (abundance)"))] <- "Firmicutes (Phylum)"
data$exposure_formatted[which((data$ID == "289377291") & (data$exposure_formatted == "Firmicutes (presence/absence)"))] <- "Firmicutes (Phylum)"
table(unique(data$exposure_formatted[data$ID == "289377291"]))
table(unique(data$exposure_info[data$ID == "289377291"]))
length(data$exposure_formatted[data$ID == "289377291"])
length(unique(data$exposure_formatted[data$ID == "289377291"]))
sum(is.na(data$exposure_formatted[data$ID == "289377291"]))
sum(is.na(data$exposure_info[data$ID == "289377291"]))
data[which((data$ID == "289377291")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Third study (ID = 557846834) 
data$exposure_unit[data$ID == "557846834"]
unique(data$exposure_study[data$ID == "557846834"])
length(data$exposure[data$ID == "557846834"])
length(unique(data$exposure[data$ID == "557846834"]))
table(unique(data$exposure[data$ID == "557846834"])) 
# Any changes
data$exposure_formatted[which(data$ID == "557846834")] <- data$exposure[which(data$ID == "557846834")]
data$exposure_info[which(data$ID == "557846834" & grepl("Functional units", data$exposure_formatted))] <- sub(".*- (\\w+)", "\\1", data$exposure_formatted[which(data$ID == "557846834" & grepl("Functional units", data$exposure_formatted))])
data$exposure_formatted[which(data$ID == "557846834" & grepl("Functional units", data$exposure_formatted))] <- "Functional units"
data$exposure_info[which(data$ID == "557846834" & grepl("Microbial taxa", data$exposure_formatted))] <- sub(".*- (\\w+)", "\\1", data$exposure_formatted[which(data$ID == "557846834" & grepl("Microbial taxa", data$exposure_formatted))])
data$exposure_info[which(data$ID == "557846834" & grepl("unit", data$exposure_info))] <- sub("unit", "Unit", data$exposure_info[which(data$ID == "557846834" & grepl("unit", data$exposure_info))])
data$exposure_formatted[which(data$ID == "557846834" & grepl("Microbial taxa", data$exposure_formatted))] <- "Gut microbiome"
table(unique(data$exposure_formatted[data$ID == "557846834"]))
table(unique(data$exposure_info[data$ID == "557846834"]))
length(data$exposure_formatted[data$ID == "557846834"])
length(unique(data$exposure_formatted[data$ID == "557846834"]))
sum(is.na(data$exposure_formatted[data$ID == "557846834"]))
sum(is.na(data$exposure_info[data$ID == "557846834"]))
data[which((data$ID == "557846834")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Fourth study (ID = 878349061) 
data$exposure_unit[data$ID == "878349061"]
unique(data$exposure_study[data$ID == "878349061"])
length(data$exposure[data$ID == "878349061"])
length(unique(data$exposure[data$ID == "878349061"]))
table(unique(data$exposure[data$ID == "878349061"])) 
# No changes
data$exposure_formatted[which(data$ID == "878349061")] <- data$exposure[which(data$ID == "878349061")]
table(unique(data$exposure_formatted[data$ID == "878349061"]))
table(unique(data$exposure_info[data$ID == "878349061"]))
length(data$exposure_formatted[data$ID == "878349061"])
length(unique(data$exposure_formatted[data$ID == "878349061"]))
sum(is.na(data$exposure_formatted[data$ID == "878349061"]))
sum(is.na(data$exposure_info[data$ID == "878349061"]))
data[which((data$ID == "878349061")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Fifth study (ID = 978492202) 
data$exposure_unit[data$ID == "978492202"]
unique(data$exposure_study[data$ID == "978492202"])
length(data$exposure[data$ID == "978492202"])
length(unique(data$exposure[data$ID == "978492202"]))
table(unique(data$exposure[data$ID == "978492202"])) 
# Any changes
data$exposure_formatted[which(data$ID == "978492202")] <- data$exposure[which(data$ID == "978492202")]
data$exposure_formatted[which(data$ID == "978492202")] <- sub("(\\w+) (\\w+)", "\\2 \\(\\1\\)", data$exposure_formatted[which(data$ID == "978492202")])
table(unique(data$exposure_formatted[data$ID == "978492202"]))
table(unique(data$exposure_info[data$ID == "978492202"]))
length(data$exposure_formatted[data$ID == "978492202"])
length(unique(data$exposure_formatted[data$ID == "978492202"]))
sum(is.na(data$exposure_formatted[data$ID == "978492202"]))
sum(is.na(data$exposure_info[data$ID == "978492202"]))
data[which((data$ID == "978492202")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Not reported ##
table(data$ID[data$exposure_study == "Not reported"]) # 2 studies (1915685263, 4090935045)
data[data$exposure_study == "Not reported",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Not reported"])

# First study (ID = 1915685263) 
data$exposure_unit[data$ID == "1915685263"]
unique(data$exposure_study[data$ID == "1915685263"])
length(data$exposure[data$ID == "1915685263"])
length(unique(data$exposure[data$ID == "1915685263"]))
table(unique(data$exposure[data$ID == "1915685263"])) 
# Any changes
data$exposure_formatted[which(data$ID == "1915685263")] <- "Gut microbiome"
table(unique(data$exposure_formatted[data$ID == "1915685263"]))
table(unique(data$exposure_info[data$ID == "1915685263"]))
length(data$exposure_formatted[data$ID == "1915685263"])
length(unique(data$exposure_formatted[data$ID == "1915685263"]))
sum(is.na(data$exposure_formatted[data$ID == "1915685263"]))
sum(is.na(data$exposure_info[data$ID == "1915685263"]))
data[which((data$ID == "1915685263")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 4090935045) 
data$exposure_unit[data$ID == "4090935045"]
unique(data$exposure_study[data$ID == "4090935045"])
length(data$exposure[data$ID == "4090935045"])
length(unique(data$exposure[data$ID == "4090935045"]))
table(unique(data$exposure[data$ID == "4090935045"])) 
# No need to change, already in the right format
data$exposure_formatted[which(data$ID == "4090935045")] <- data$exposure[which(data$ID == "4090935045")]
table(unique(data$exposure_formatted[data$ID == "4090935045"]))
table(unique(data$exposure_info[data$ID == "4090935045"]))
length(data$exposure_formatted[data$ID == "4090935045"])
length(unique(data$exposure_formatted[data$ID == "4090935045"]))
sum(is.na(data$exposure_formatted[data$ID == "4090935045"]))
sum(is.na(data$exposure_info[data$ID == "4090935045"]))
data[which((data$ID == "4090935045")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Qin 2022 ##
table(data$ID[data$exposure_study == "Qin 2022"]) # 2 studies (3148374251, 718621312)
data[data$exposure_study == "Qin 2022",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Qin 2022"])

# First study (ID = 3148374251) 
data$exposure_unit[data$ID == "3148374251"]
unique(data$exposure_study[data$ID == "3148374251"])
length(data$exposure[data$ID == "3148374251"])
length(unique(data$exposure[data$ID == "3148374251"]))
table(unique(data$exposure[data$ID == "3148374251"])) 
# No changes
data$exposure_formatted[which(data$ID == "3148374251")] <- data$exposure[which(data$ID == "3148374251")]
table(unique(data$exposure_formatted[data$ID == "3148374251"]))
table(unique(data$exposure_info[data$ID == "3148374251"]))
length(data$exposure_formatted[data$ID == "3148374251"])
length(unique(data$exposure_formatted[data$ID == "3148374251"]))
sum(is.na(data$exposure_formatted[data$ID == "3148374251"]))
sum(is.na(data$exposure_info[data$ID == "3148374251"]))
data[which((data$ID == "3148374251")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 718621312) 
data$exposure_unit[data$ID == "718621312"]
unique(data$exposure_study[data$ID == "718621312"])
length(data$exposure[data$ID == "718621312"])
length(unique(data$exposure[data$ID == "718621312"]))
table(unique(data$exposure[data$ID == "718621312"])) 
# Any changes
data$exposure_formatted[which(data$ID == "718621312")] <- data$exposure[which(data$ID == "718621312")]
data$exposure_formatted[which((data$ID == "718621312") & grepl("__", data$exposure_formatted))] <- sub(" ", "_", data$exposure_formatted[which((data$ID == "718621312") & grepl("__", data$exposure_formatted))])
data$exposure_formatted[which((data$ID == "718621312") & grepl("s__CAG", data$exposure_formatted))] <- sub("s__(\\w+)-(\\w+)_(\\w+)", "\\1-\\2 \\3 (Species)", data$exposure_formatted[which((data$ID == "718621312") & grepl("s__CAG", data$exposure_formatted))])
data$exposure_formatted[which((data$ID == "718621312") & grepl("s__", data$exposure_formatted))] <- sub("s__(\\w+)_(\\w+)", "\\1 \\2 (Species)", data$exposure_formatted[which((data$ID == "718621312") & grepl("s__", data$exposure_formatted))])
# Found a spelling mistake in one 
data$exposure_formatted[which((data$ID == "718621312") & grepl("g__CAG", data$exposure_formatted))] <- sub("g__(\\w+)-(\\w+)", "\\1-\\2 (Genus)", data$exposure_formatted[which((data$ID == "718621312") & grepl("g__CAG", data$exposure_formatted))])
data$exposure_formatted[which((data$ID == "718621312") & grepl("g__GCA-900066495", data$exposure_formatted))] <- "CAG-900066495 (Genus)"
data$exposure_formatted[which((data$ID == "718621312") & grepl("g__", data$exposure_formatted))] <- sub("g__(\\w+)", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "718621312") & grepl("g__", data$exposure_formatted))])
data$exposure_formatted[which((data$ID == "718621312") & (data$exposure_formatted == "Eubacterium_I_ramulus A (Species)"))] <- "Eubacterium_I ramulus_A (Species)"
table(unique(data$exposure_formatted[data$ID == "718621312"]))
table(unique(data$exposure_info[data$ID == "718621312"]))
length(data$exposure_formatted[data$ID == "718621312"])
length(unique(data$exposure_formatted[data$ID == "718621312"]))
sum(is.na(data$exposure_formatted[data$ID == "718621312"]))
sum(is.na(data$exposure_info[data$ID == "718621312"]))
data[which((data$ID == "718621312")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Rühlemann 2021 ##
table(data$ID[data$exposure_study == "Rühlemann 2021"]) # 2 studies (3278202937, 95001313)
data[data$exposure_study == "Rühlemann 2021",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Rühlemann 2021"])

# First study (ID = 3278202937) 
data$exposure_unit[data$ID == "3278202937"]
unique(data$exposure_study[data$ID == "3278202937"])
length(data$exposure[data$ID == "3278202937"])
length(unique(data$exposure[data$ID == "3278202937"]))
table(unique(data$exposure[data$ID == "3278202937"])) 
# Any changes
data$exposure_formatted[which(data$ID == "3278202937")] <- data$exposure[which(data$ID == "3278202937")]
data$exposure_formatted[which((data$ID == "3278202937") & (grepl("G_", data$exposure_formatted)))] <- sub("G_(\\w+)", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "3278202937") & (grepl("G_", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3278202937") & (data$exposure_formatted == "Clostridium_XlVa (Genus)"))] <- "Clostridium XlVa (Genus)"
data$exposure_formatted[which((data$ID == "3278202937") & (grepl("C_", data$exposure_formatted)))] <- sub("C_(\\w+)", "\\1 (Class)", data$exposure_formatted[which((data$ID == "3278202937") & (grepl("C_", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3278202937") & (grepl("F_", data$exposure_formatted)))] <- sub("F_(\\w+)", "\\1 (Family)", data$exposure_formatted[which((data$ID == "3278202937") & (grepl("F_", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3278202937") & (grepl("O_", data$exposure_formatted)))] <- sub("O_(\\w+)", "\\1 (Order)", data$exposure_formatted[which((data$ID == "3278202937") & (grepl("O_", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3278202937") & (grepl("P_", data$exposure_formatted)))] <- sub("P_(\\w+)", "\\1 (Phylum)", data$exposure_formatted[which((data$ID == "3278202937") & (grepl("P_", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3278202937") & (grepl("OTU", data$exposure_formatted)))] <- sub("(\\w+)_(\\w+)", "\\1-\\2", data$exposure_formatted[which((data$ID == "3278202937") & (grepl("OTU", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3278202937") & (grepl("TestASV", data$exposure_formatted)))] <- sub("(\\w+)_(\\w+)", "\\1-\\2", data$exposure_formatted[which((data$ID == "3278202937") & (grepl("TestASV", data$exposure_formatted)))])
table(unique(data$exposure_formatted[data$ID == "3278202937"]))
table(unique(data$exposure_info[data$ID == "3278202937"]))
length(data$exposure_formatted[data$ID == "3278202937"])
length(unique(data$exposure_formatted[data$ID == "3278202937"]))
sum(is.na(data$exposure_formatted[data$ID == "3278202937"]))
sum(is.na(data$exposure_info[data$ID == "3278202937"]))
data[which((data$ID == "3278202937")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 95001313) 
data$exposure_unit[data$ID == "95001313"]
unique(data$exposure_study[data$ID == "95001313"])
length(data$exposure[data$ID == "95001313"])
length(unique(data$exposure[data$ID == "95001313"]))
table(unique(data$exposure[data$ID == "95001313"])) 
# Any changes
data$exposure_formatted[which(data$ID == "95001313")] <- data$exposure[which(data$ID == "95001313")]
# The row with "G_Porphyromonadaceae (without outliers)" as the exposure is actually a sensitivity analysis so I would like to remove this but want to check that the duplicate exposure is correct
data$dup_exposure[which((data$ID == "95001313") & (data$exposure == "G_Porphyromonadaceae (without outliers)"))]  ## it is so will remove
if (any(data$ID == "95001313" & data$exposure == "G_Porphyromonadaceae (without outliers)")) {
  data <- data[!(data$ID == "95001313" & data$exposure == "G_Porphyromonadaceae (without outliers)"), ]
}
data$exposure_formatted[which((data$ID == "95001313") & (grepl("G_", data$exposure_formatted)))] <- sub("G_(\\w+)", "\\1 (Genus)", data$exposure_formatted[which((data$ID == "95001313") & (grepl("G_", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "95001313") & (grepl("F_", data$exposure_formatted)))] <- sub("F_(\\w+)", "\\1 (Family)", data$exposure_formatted[which((data$ID == "95001313") & (grepl("F_", data$exposure_formatted)))])
table(unique(data$exposure_formatted[data$ID == "95001313"]))
table(unique(data$exposure_info[data$ID == "95001313"]))
length(data$exposure_formatted[data$ID == "95001313"])
length(unique(data$exposure_formatted[data$ID == "95001313"]))
sum(is.na(data$exposure_formatted[data$ID == "95001313"]))
sum(is.na(data$exposure_info[data$ID == "95001313"]))
data[which((data$ID == "95001313")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Sanna 2019 ##
table(data$ID[data$exposure_study == "Sanna 2019"]) # 3 studies (2044689642, 3184652665, 568199176)
data[data$exposure_study == "Sanna 2019",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Sanna 2019"])

# There are only 3 exposures so will deal with these together
data$exposure_unit[data$exposure_study == "Sanna 2019"]
unique(data$exposure_study[data$exposure_study == "Sanna 2019"])
length(data$exposure[data$exposure_study == "Sanna 2019"])
length(unique(data$exposure[data$exposure_study == "Sanna 2019"]))
table(unique(data$exposure[data$exposure_study == "Sanna 2019"])) 
# Any changes
data$exposure_formatted[which(data$exposure_study == "Sanna 2019")] <- data$exposure[which(data$exposure_study == "Sanna 2019")]
data$exposure_info[which((data$exposure_study == "Sanna 2019") & (data$exposure_formatted == "M00307 pathway (SFCA butyrate production)"))] <-  "SFCA butyrate production"
data$exposure_formatted[which((data$exposure_study == "Sanna 2019") & (data$exposure_formatted == "M00307 pathway (SFCA butyrate production)"))] <- "M00307 (Pathway)"
data$exposure_formatted[which((data$exposure_study == "Sanna 2019") & (data$exposure_formatted == "PWY-5022"))] <- "PWY-5022 (Pathway)"
data$exposure_info[which((data$exposure_study == "Sanna 2019") & (data$exposure_formatted == "PWY-5022 (4-aminobutanoate degradation V pathway)"))] <-"4-aminobutanoate degradation V pathway"
data$exposure_formatted[which((data$exposure_study == "Sanna 2019") & (data$exposure_formatted == "PWY-5022 (4-aminobutanoate degradation V pathway)"))] <- "PWY-5022 (Pathway)"
table(unique(data$exposure_formatted[data$exposure_study == "Sanna 2019"]))
table(unique(data$exposure_info[data$exposure_study == "Sanna 2019"]))
length(data$exposure_formatted[data$exposure_study == "Sanna 2019"])
length(unique(data$exposure_formatted[data$exposure_study == "Sanna 2019"]))
sum(is.na(data$exposure_formatted[data$exposure_study == "Sanna 2019"]))
sum(is.na(data$exposure_info[data$exposure_study == "Sanna 2019"]))
data[which((data$exposure_study == "Sanna 2019")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Scepanovic 2019 ##
table(data$ID[data$exposure_study == "Scepanovic 2019"]) # 1 studies (4290593439)
data[data$exposure_study == "Scepanovic 2019",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Scepanovic 2019"])

# First study (ID = 4290593439) 
data$exposure_unit[data$ID == "4290593439"]
unique(data$exposure_study[data$ID == "4290593439"])
length(data$exposure[data$ID == "4290593439"])
length(unique(data$exposure[data$ID == "4290593439"]))
table(unique(data$exposure[data$ID == "4290593439"])) 
# Any changes
data$exposure_formatted[which(data$ID == "4290593439")] <- data$exposure[which(data$ID == "4290593439")]
data$exposure_formatted[which((data$ID == "4290593439") & (grepl("species", data$exposure_formatted)))] <- sub("species", "Species", data$exposure_formatted[which((data$ID == "4290593439") & (grepl("species", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "4290593439") & (grepl("genus", data$exposure_formatted)))] <- sub("genus", "Genus", data$exposure_formatted[which((data$ID == "4290593439") & (grepl("genus", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "4290593439") & (grepl("family", data$exposure_formatted)))] <- sub("family", "Family", data$exposure_formatted[which((data$ID == "4290593439") & (grepl("family", data$exposure_formatted)))])
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Bifidobacteriaceae (Family) Bifidobacterium (Genus) NA (Species)"))] <- "Bifidobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Bifidobacteriaceae (Family) Bifidobacterium (Genus) NA (Species)"))] <- "Bifidobacterium (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) NA (Species)"))] <- "Clostridiaceae (Family) Clostridium (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) NA (Species)"))] <- "Clostridium (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) NA (Genus) NA (Species)"))] <- "Clostridiaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) NA (Genus) NA (Species)"))] <- "Clostridiaceae (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Atopobium (Genus) NA (Species)"))] <- "Coriobacteriaceae (Family) Atopobium (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Atopobium (Genus) NA (Species)"))] <- "Atopobium (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) NA (Species)"))] <- "Coriobacteriaceae (Family) Eggerthella (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) NA (Species)"))] <- "Eggerthella (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Gordonibacter (Genus) NA (Species)"))] <- "Coriobacteriaceae (Family) Gordonibacter (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Gordonibacter (Genus) NA (Species)"))] <- "Gordonibacter (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Enterobacteriaceae (Family) EscherichiaShigella (Genus) NA (Species)"))] <- "Enterobacteriaceae (Family) Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Enterobacteriaceae (Family) EscherichiaShigella (Genus) NA (Species)"))] <- "Escherichia Shigella (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Coprococcus (Genus) NA (Species)"))] <- "Lachnospiraceae (Family) Coprococcus (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Coprococcus (Genus) NA (Species)"))] <- "Coprococcus (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Dorea (Genus) NA (Species)"))] <- "Lachnospiraceae (Family) Dorea (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Dorea (Genus) NA (Species)"))] <- "Dorea (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Lachnospira (Genus) NA (Species)"))] <- "Lachnospiraceae (Family) Lachnospira (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Lachnospira (Genus) NA (Species)"))] <- "Lachnospira (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Roseburia (Genus) NA (Species)"))] <- "Lachnospiraceae (Family) Roseburia (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Roseburia (Genus) NA (Species)"))] <- "Roseburia (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Methanobacteriaceae (Family) Methanobrevibacter (Genus) NA (Species)"))] <- "Methanobacteriaceae (Family) Methanobrevibacter (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Methanobacteriaceae (Family) Methanobrevibacter (Genus) NA (Species)"))] <- "Methanobrevibacter (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Methanobacteriaceae (Family) NA (Genus) NA (Species)"))] <- "Methanobacteriaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Methanobacteriaceae (Family) NA (Genus) NA (Species)"))] <- "Methanobacteriaceae (Unclassified Genus in Family)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Ruminococcaceae (Family) Anaerofilum (Genus) NA (Species)"))] <- "Ruminococcaceae (Family) Anaerofilum (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Ruminococcaceae (Family) Anaerofilum (Genus) NA (Species)"))] <- "Anaerofilum (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Ruminococcaceae (Family) Oscillospira (Genus) NA (Species)"))] <- "Ruminococcaceae (Family) Oscillospira (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Ruminococcaceae (Family) Oscillospira (Genus) NA (Species)"))] <- "Oscillospira (Unclassified Species in Genus)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Synergistaceae (Family) NA (Genus) NA (Species)"))] <- "Synergistaceae (Family)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Synergistaceae (Family) NA (Genus) NA (Species)"))] <- "Synergistaceae (Unclassified Genus in Family)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Butyricicoccus (Genus) Butyricicoccus pullicaecorum (Species)"))] <- "Clostridiaceae (Family) Butyricicoccus (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Butyricicoccus (Genus) Butyricicoccus pullicaecorum (Species)"))] <- "Butyricicoccus pullicaecorum (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium celerecrescens (Species)"))] <- "Clostridiaceae (Family) Clostridium (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium celerecrescens (Species)"))] <- "Clostridium celerecrescens (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium cellulolyticum (Species)"))] <- "Clostridiaceae (Family) Clostridium (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium cellulolyticum (Species)"))] <- "Clostridium cellulolyticum (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium IV (Species)"))] <- "Clostridiaceae (Family) Clostridium (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium IV (Species)"))] <- "Clostridium IV (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium propionicum (Species)"))] <- "Clostridiaceae (Family) Clostridium (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Clostridiaceae (Family) Clostridium (Genus) Clostridium propionicum (Species)"))] <- "Clostridium propionicum (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) Eggerthella sinensis (Species)"))] <- "Coriobacteriaceae (Family) Eggerthella (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) Eggerthella sinensis (Species)"))] <- "Eggerthella sinensis (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) lenta (Species)"))] <- "Coriobacteriaceae (Family) Eggerthella (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Eggerthella (Genus) lenta (Species)"))] <- "Eggerthella lenta (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Gordonibacter  (Genus) Gordonibacter pamelaeae (Species)"))] <- "Coriobacteriaceae (Family) Gordonibacter (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Coriobacteriaceae (Family) Gordonibacter  (Genus) Gordonibacter pamelaeae (Species)"))] <- "Gordonibacter pamelaeae (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Enterobacteriaceae (Family) Klebsiella  (Genus) Klebsiella variicola (Species)"))] <- "Enterobacteriaceae (Family) Klebsiella (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Enterobacteriaceae (Family) Klebsiella  (Genus) Klebsiella variicola (Species)"))] <- "Klebsiella variicola (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Blautia  (Genus) Blautia stercoris (Species)"))] <- "Lachnospiraceae (Family) Blautia (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Blautia  (Genus) Blautia stercoris (Species)"))] <- "Blautia stercoris (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Blautia coccoides (Species)"))] <- "Lachnospiraceae (Family) Blautia (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Blautia coccoides (Species)"))] <- "Blautia coccoides (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Blautiagenus (Species)"))] <- "Lachnospiraceae (Family) Blautia (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Blautiagenus (Species)"))] <- "Blautiagenus (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Ruminococcus obeum (Species)"))] <- "Lachnospiraceae (Family) Blautia (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Blautia (Genus) Ruminococcus obeum (Species)"))] <- "Ruminococcus obeum (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Coprococcus (Genus) Coprococcus comes (Species)"))] <- "Lachnospiraceae (Family) Coprococcus (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Coprococcus (Genus) Coprococcus comes (Species)"))] <- "Coprococcus comes (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Lachnospiraceae noname (Genus) Lachnospiraceae bacterium 1 1 57FAA (Species)"))] <- "Lachnospiraceae (Family) Lachnospiraceae noname (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Lachnospiraceae noname (Genus) Lachnospiraceae bacterium 1 1 57FAA (Species)"))] <- "Lachnospiraceae bacterium 1_1_57FAA (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Murimonas (Genus) Murimonas intestini (Species)"))] <- "Lachnospiraceae (Family) Murimonas (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Lachnospiraceae (Family) Murimonas (Genus) Murimonas intestini (Species)"))] <- "Murimonas intestini (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Ruminococcaceae (Family) Ruminococcus (Genus) Ruminococcus flavefaciens (Species)"))] <- "Ruminococcaceae (Family) Ruminococcus (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Ruminococcaceae (Family) Ruminococcus (Genus) Ruminococcus flavefaciens (Species)"))] <- "Ruminococcus flavefaciens (Species)"
data$exposure_info[which((data$ID == "4290593439" & data$exposure_formatted == "Ruminococcaceae (Family) Ruminococcus (Genus) Ruminococcus gnavus (Species)"))] <- "Ruminococcaceae (Family) Ruminococcus (Genus)"
data$exposure_formatted[which((data$ID == "4290593439" & data$exposure_formatted == "Ruminococcaceae (Family) Ruminococcus (Genus) Ruminococcus gnavus (Species)"))] <- "Ruminococcus gnavus (Species)"
table(unique(data$exposure_formatted[data$ID == "4290593439"]))
table(unique(data$exposure_info[data$ID == "4290593439"]))
length(data$exposure_formatted[data$ID == "4290593439"])
length(unique(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_info[data$ID == "4290593439"]))
data[which((data$ID == "4290593439")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Turpin 2016 ##
table(data$ID[data$exposure_study == "Turpin 2016"]) # 2 studies (1308760143, 4290593439)
data[data$exposure_study == "Turpin 2016",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Turpin 2016"])

# First study (ID = 1308760143) 
data$exposure_unit[data$ID == "1308760143"]
unique(data$exposure_study[data$ID == "1308760143"])
length(data$exposure[data$ID == "1308760143"])
length(unique(data$exposure[data$ID == "1308760143"]))
table(unique(data$exposure[data$ID == "1308760143"])) 
# Already been handled
table(unique(data$exposure_formatted[data$ID == "1308760143"]))
table(unique(data$exposure_info[data$ID == "1308760143"]))
length(data$exposure_formatted[data$ID == "1308760143"])
length(unique(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_formatted[data$ID == "1308760143"]))
sum(is.na(data$exposure_info[data$ID == "1308760143"]))
data[which((data$ID == "1308760143")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 4290593439) 
data$exposure_unit[data$ID == "4290593439"]
unique(data$exposure_study[data$ID == "4290593439"])
length(data$exposure[data$ID == "4290593439"])
length(unique(data$exposure[data$ID == "4290593439"]))
table(unique(data$exposure[data$ID == "4290593439"])) 
# Already been handled
table(unique(data$exposure_formatted[data$ID == "4290593439"]))
table(unique(data$exposure_info[data$ID == "4290593439"]))
length(data$exposure_formatted[data$ID == "4290593439"])
length(unique(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_info[data$ID == "4290593439"]))
data[which((data$ID == "4290593439")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Unclear (mixture of all, see notes) ##
table(data$ID[data$exposure_study == "Unclear (mixture of all, see notes)"]) # 1 study (4290593439)
data[data$exposure_study == "Unclear (mixture of all, see notes)",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Unclear (mixture of all, see notes)"])

# First study (ID = 4290593439) 
data$exposure_unit[data$ID == "4290593439"]
unique(data$exposure_study[data$ID == "4290593439"])
length(data$exposure[data$ID == "4290593439"])
length(unique(data$exposure[data$ID == "4290593439"]))
table(unique(data$exposure[data$ID == "4290593439"])) 
# Also already been handled
table(unique(data$exposure_formatted[data$ID == "4290593439"]))
table(unique(data$exposure_info[data$ID == "4290593439"]))
length(data$exposure_formatted[data$ID == "4290593439"])
length(unique(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_info[data$ID == "4290593439"]))
data[which((data$ID == "4290593439")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## Wang 2016 ## 
table(data$ID[data$exposure_study == "Wang 2016"]) # 5 studies (1108156353, 3549710879, 4251029218, 4290593439, 505573255)
data[data$exposure_study == "Wang 2016",c("ID","exposure","exposure_study", "exposure_unit")]
unique(data$exposure[data$exposure_study == "Wang 2016"])

# First study (ID = 1108156353) 
data$exposure_unit[data$ID == "1108156353"]
unique(data$exposure_study[data$ID == "1108156353"])
length(data$exposure[data$ID == "1108156353"])
length(unique(data$exposure[data$ID == "1108156353"]))
table(unique(data$exposure[data$ID == "1108156353"])) 
# Any changes
data$exposure_formatted[which(data$ID == "1108156353")] <- data$exposure[which(data$ID == "1108156353")]
data$exposure_formatted[which((data$ID == "1108156353" & grepl(" class", data$exposure_formatted)))] <- sub(" class", " (Class)", data$exposure_formatted[which((data$ID == "1108156353" & grepl(" class", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1108156353" & grepl(" genus", data$exposure_formatted)))] <- sub(" genus", " (Genus)", data$exposure_formatted[which((data$ID == "1108156353" & grepl(" genus", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1108156353" & grepl(" order", data$exposure_formatted)))] <- sub(" order", " (Order)", data$exposure_formatted[which((data$ID == "1108156353" & grepl(" order", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1108156353" & grepl(" family", data$exposure_formatted)))] <- sub(" family", " (Family)", data$exposure_formatted[which((data$ID == "1108156353" & grepl(" family", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1108156353" & grepl(" phylum", data$exposure_formatted)))] <- sub(" phylum", " (Phylum)", data$exposure_formatted[which((data$ID == "1108156353" & grepl(" phylum", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "EscherichiaShigella"))] <- "Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "OTU10032 unclassified Enterobacteriaceae"))] <- "OTU10032 (Unclassified OTU in Enterobacteriaceae Family)"
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "OTU10032 unclassified Enterobacteriaceae Species-level OTU"))] <- "OTU10032 (Unclassified Species in Enterobacteriaceae Family)"
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "OTU15355 Dialister Species level OTU"))] <- "OTU15355 (Species in Dialister Genus)"
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "OTU13305 Fecalibacterium Species-level OTU"))] <- "OTU13305 (Species in Fecalibacterium Genus)"
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "Unclassified Acidaminococcaceae"))] <- "Acidaminococcaceae (Unclassified Genus in Family)"
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "Unclassified Erysipelotrichaceae"))] <- "Erysipelotrichaceae (Unclassified Genus in Family)"
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "Unclassified Marinilabiliaceae"))] <- "Marinilabiliaceae (Unclassified Genus in Family)"
# Spelling mistake in one of these
data$exposure_formatted[which((data$ID == "1108156353" & data$exposure_formatted == "Unclassified Prophyromonadaceae"))] <- "Porphyromonadaceae (Unclassified Genus in Family)"
table(unique(data$exposure_formatted[data$ID == "1108156353"]))
table(unique(data$exposure_info[data$ID == "1108156353"]))
length(data$exposure_formatted[data$ID == "1108156353"])
length(unique(data$exposure_formatted[data$ID == "1108156353"]))
sum(is.na(data$exposure_formatted[data$ID == "1108156353"]))
sum(is.na(data$exposure_info[data$ID == "1108156353"]))
data[which((data$ID == "1108156353")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Second study (ID = 3549710879) 
data$exposure_unit[data$ID == "3549710879"]
unique(data$exposure_study[data$ID == "3549710879"])
length(data$exposure[data$ID == "3549710879"])
length(unique(data$exposure[data$ID == "3549710879"]))
table(unique(data$exposure[data$ID == "3549710879"])) 
# Any changes
data$exposure_formatted[which(data$ID == "3549710879")] <- data$exposure[which(data$ID == "3549710879")]
data$exposure_formatted[which((data$ID == "3549710879" & grepl(" class", data$exposure_formatted)))] <- sub(" class", " (Class)", data$exposure_formatted[which((data$ID == "3549710879" & grepl(" class", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3549710879" & grepl(" order", data$exposure_formatted)))] <- sub(" order", " (Order)", data$exposure_formatted[which((data$ID == "3549710879" & grepl(" order", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3549710879" & grepl(" family", data$exposure_formatted)))] <- sub(" family", " (Family)", data$exposure_formatted[which((data$ID == "3549710879" & grepl(" family", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "3549710879" & grepl(" phylum", data$exposure_formatted)))] <- sub(" phylum", " (Phylum)", data$exposure_formatted[which((data$ID == "3549710879" & grepl(" phylum", data$exposure_formatted)))])
# Spelling mistakes in a few of these
data$exposure_formatted[which((data$ID == "3549710879" & data$exposure_formatted == "OTU10032 unclassifed Enterobacteriaceae"))] <- "OTU10032 (Unclassified OTU in Enterobacteriaceae Family)"
data$exposure_formatted[which((data$ID == "3549710879" & data$exposure_formatted == "Unclassifed Erysipelotrichaceae"))] <- "Enterobacteriaceae (Unclassified Genus in Family)"
table(unique(data$exposure_formatted[data$ID == "3549710879"]))
table(unique(data$exposure_info[data$ID == "3549710879"]))
length(data$exposure_formatted[data$ID == "3549710879"])
length(unique(data$exposure_formatted[data$ID == "3549710879"]))
sum(is.na(data$exposure_formatted[data$ID == "3549710879"]))
sum(is.na(data$exposure_info[data$ID == "3549710879"]))
data[which((data$ID == "3549710879")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Third study (ID = 4251029218) 
data$exposure_unit[data$ID == "4251029218"]
unique(data$exposure_study[data$ID == "4251029218"])
length(data$exposure[data$ID == "4251029218"])
length(unique(data$exposure[data$ID == "4251029218"]))
table(unique(data$exposure[data$ID == "4251029218"])) 
# Any changes
data$exposure_formatted[which(data$ID == "4251029218")] <- data$exposure[which(data$ID == "4251029218")]
data$exposure_formatted[which((data$ID == "4251029218" & grepl("genus", data$exposure_formatted)))] <- sub("genus", "Genus", data$exposure_formatted[which((data$ID == "4251029218" & grepl("genus", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "4251029218" & grepl("family", data$exposure_formatted)))] <- sub("family", "Family", data$exposure_formatted[which((data$ID == "4251029218" & grepl("family", data$exposure_formatted)))])
table(unique(data$exposure_formatted[data$ID == "4251029218"]))
table(unique(data$exposure_info[data$ID == "4251029218"]))
length(data$exposure_formatted[data$ID == "4251029218"])
length(unique(data$exposure_formatted[data$ID == "4251029218"]))
sum(is.na(data$exposure_formatted[data$ID == "4251029218"]))
sum(is.na(data$exposure_info[data$ID == "4251029218"]))
data[which((data$ID == "4251029218")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Fourth study (ID = 4290593439) 
data$exposure_unit[data$ID == "4290593439"]
unique(data$exposure_study[data$ID == "4290593439"])
length(data$exposure[data$ID == "4290593439"])
length(unique(data$exposure[data$ID == "4290593439"]))
table(unique(data$exposure[data$ID == "4290593439"])) 
# Already handled
table(unique(data$exposure_formatted[data$ID == "4290593439"]))
table(unique(data$exposure_info[data$ID == "4290593439"]))
length(data$exposure_formatted[data$ID == "4290593439"])
length(unique(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_formatted[data$ID == "4290593439"]))
sum(is.na(data$exposure_info[data$ID == "4290593439"]))
data[which((data$ID == "4290593439")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

# Fifth study (ID = 505573255) 
data$exposure_unit[data$ID == "505573255"]
unique(data$exposure_study[data$ID == "505573255"])
length(data$exposure[data$ID == "505573255"])
length(unique(data$exposure[data$ID == "505573255"]))
table(unique(data$exposure[data$ID == "505573255"])) 
# Any changes
data$exposure_formatted[which(data$ID == "505573255")] <- data$exposure[which(data$ID == "505573255")]
data$exposure_formatted[which((data$ID == "505573255" & grepl(" class", data$exposure_formatted)))] <- sub(" class", " (Class)", data$exposure_formatted[which((data$ID == "505573255" & grepl(" class", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "505573255" & grepl(" genus", data$exposure_formatted)))] <- sub(" genus", " (Genus)", data$exposure_formatted[which((data$ID == "505573255" & grepl(" genus", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "505573255" & grepl(" order", data$exposure_formatted)))] <- sub(" order", " (Order)", data$exposure_formatted[which((data$ID == "505573255" & grepl(" order", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "505573255" & grepl(" family", data$exposure_formatted)))] <- sub(" family", " (Family)", data$exposure_formatted[which((data$ID == "505573255" & grepl(" family", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "505573255" & grepl(" phylum", data$exposure_formatted)))] <- sub(" phylum", " (Phylum)", data$exposure_formatted[which((data$ID == "505573255" & grepl(" phylum", data$exposure_formatted)))])
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "EscherichiaShigella"))] <- "Escherichia Shigella (Genus)"
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "Blautia"))] <- "Blautia (Genus)"
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "OTU15355 Dialister Species-level OTU"))] <- "OTU15355 (Species in Dialister Genus)"
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "OTU13305 Fecalibacterium Species-level OTU"))] <- "OTU13305 (Species in Fecalibacterium Genus)"
# A few spelling mistakes in these
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "OTU10032 unclassifed Enterobacteriaceae"))] <- "OTU10032 (Unclassified OTU in Enterobacteriaceae Family)"
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "OTU10032 unclassifed Enterobacteriaceae Species-level OTU"))] <- "OTU10032 (Unclassified Species in Enterobacteriaceae Family)"
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "Unclassifed Acidaminococcaceae"))] <- "Acidaminococcaceae (Unclassified Genus in Family)"
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "Unclassifed Erysiphelotrichaceae"))] <- "Erysipelotrichaceae (Unclassified Genus in Family)"
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "Unclassifed Marinilabiliaceae"))] <- "Marinilabiliaceae (Unclassified Genus in Family)"
data$exposure_formatted[which((data$ID == "505573255" & data$exposure_formatted == "Unclassifed Porphyromonadaceae"))] <- "Porphyromonadaceae (Unclassified Genus in Family)"
table(unique(data$exposure_formatted[data$ID == "505573255"]))
table(unique(data$exposure_info[data$ID == "505573255"]))
length(data$exposure_formatted[data$ID == "505573255"])
length(unique(data$exposure_formatted[data$ID == "505573255"]))
sum(is.na(data$exposure_formatted[data$ID == "505573255"]))
sum(is.na(data$exposure_info[data$ID == "505573255"]))
data[which((data$ID == "505573255")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]

## There is one where the exposure study was listed as the "Flemish Gut Flora Project"
table(data$ID[data$exposure_study == "\"Flemish Gut Flora Project\""]) # 1 study ("NA")
data[data$exposure_study == "\"Flemish Gut Flora Project\"",c("ID","exposure","exposure_study", "exposure_unit")]
length(data$exposure[data$exposure_study == "\"Flemish Gut Flora Project\""])
length(unique(data$exposure[data$exposure_study == "\"Flemish Gut Flora Project\""]))
unique(data$exposure[data$exposure_study == "\"Flemish Gut Flora Project\""])
unique(data$exposure_unit[data$exposure_study == "\"Flemish Gut Flora Project\""])
unique(data$exposure_study[which(data$ID %in% c("NA"))])
unique(data[which(data$ID %in% c("NA")), c("ID", "exposure_study")])
# Need to harmonize (note that there is an error here with one of the exposures, which should specify the model)
data$exposure[which(data$exposure_study == "\"Flemish Gut Flora Project\"" & data$exposure == "F_Streptococcaceae")] <- "F_Streptococcaceae_HB"
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("C_*_HB"), data$exposure)))] <- gsub("^C_(.*?)_HB$", "\\1 (Class)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("C_*_HB"), data$exposure)))])
data$exposure_info[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("C_*_HB"), data$exposure)))] <- "Presence vs. absence (HB)"
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("F_*_HB"), data$exposure)))] <- gsub("^F_(.*?)_HB$", "\\1 (Family)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("F_*_HB"), data$exposure)))])
data$exposure_info[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("F_*_HB"), data$exposure)))] <- "Presence vs. absence (HB)"
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (data$exposure == "F_Streptococcaceae"))] <- gsub("F_Streptococcaceae", "Streptococcaceae (Family)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (data$exposure == "F_Streptococcaceae"))])
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_*_HB"), data$exposure)))] <- gsub("^G_(.*?)_HB$", "\\1 (Genus)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_*_HB"), data$exposure)))])
data$exposure_info[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_*_HB"), data$exposure)))] <- "Presence vs. absence (HB)"
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_unclassified_C*_HB"), data$exposure)))] <- gsub("^G_unclassified_C_(.*?)_HB$", "\\1 (Unclassified Genus in Class)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_unclassified_C_*_HB"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_unclassified_F*_HB"), data$exposure)))] <- gsub("^G_unclassified_F_(.*?)_HB$", "\\1 (Unclassified Genus in Family)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_unclassified_F_*_HB"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_unclassified_K*_HB"), data$exposure)))] <- gsub("^G_unclassified_K_(.*?)_HB$", "\\1 (Unclassified Genus in Kingdom)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_unclassified_K_*_HB"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_unclassified_P*_HB"), data$exposure)))] <- gsub("^G_unclassified_P_(.*?)_HB$", "\\1 (Unclassified Genus in Phylum)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("G_unclassified_P_*_HB"), data$exposure)))])
data$exposure_formatted[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("O_*_HB"), data$exposure)))] <- gsub("^O_(.*?)_HB$", "\\1 (Order)", data$exposure[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("O_*_HB"), data$exposure)))])
data$exposure_info[which((data$exposure_study == "\"Flemish Gut Flora Project\"") & (grepl(glob2rx("O_*_HB"), data$exposure)))] <- "Presence vs. absence (HB)"
unique(data$exposure_formatted[data$exposure_study == "\"Flemish Gut Flora Project\""])
unique(data$exposure[data$exposure_study == "\"Flemish Gut Flora Project\""])
length(data$exposure_formatted[data$exposure_study == "\"Flemish Gut Flora Project\""])
length(unique(data$exposure_formatted[data$exposure_study == "\"Flemish Gut Flora Project\""]))
sum(is.na(data$exposure_formatted[data$exposure_study == "\"Flemish Gut Flora Project\""]))
sum(is.na(data$exposure_info[data$exposure_study == "\"Flemish Gut Flora Project\""]))
data[which((data$exposure_study == "\"Flemish Gut Flora Project\"")), c("ID","exposure","exposure_formatted","exposure_unit","exposure_info")]
# And harmonize with the other exposure studies
data$exposure_study[which(data$exposure_study == "\"Flemish Gut Flora Project\"")] <- "Not reported"

#############################################################
## Creating a table to summarise all of the exposure names ##
#############################################################

## Tabulated the exposure studies
table(data$exposure_study)

## Checking which have been done
done_exposure <- as.vector(unique(data$ID[!is.na(data$exposure_formatted)]))
length(done_exposure)

## Create a dataframe to check that all exposures have been formatted accordingly 
complete_exposure <- data[!is.na(data$exposure_formatted), c("ID","exposure","exposure_formatted","exposure_info")] ## should be 48082 rows
unique_complete_exposure <- unique(complete_exposure$exposure_formatted) 
table(unique_complete_exposure)
unique_complete_exposure[-(grep(")", unique_complete_exposure))] ## Should only be those called "Gut microbiome", "Functional units", all the OTUs, TESTASVs, SNPs and those called "Unknown Family" or "Unknown Genus
data_unique_complete_exposure <- as.data.frame(unique_complete_exposure) 
write.xlsx(data_unique_complete_exposure, "./Data analysis/unique_exposures.xlsx", colNames = FALSE, rowNames = FALSE, overwrite = TRUE) 
rm(ids, done_exposure, done_outcome, complete_exposure, complete_outcome, unique_complete_exposure, unique_complete_outcome, data_unique_complete_exposure, data_unique_complete_outcome)

#####################################################################
## Checking duplicate extracted outcome data matches original data ##
#####################################################################

## Check that the duplicate outcomes match (i.e., original outcomes and duplicate outcomes and, if so, take new formatted ones)
colnames(data)
outcomes <- data[,c("ID","outcome","dup_outcome","outcome_formatted","outcome_group" )]
outcomes$comparison <- ifelse(outcomes$outcome == outcomes$dup_outcome, "Same", "Different")
table(outcomes$comparison)
different_outcomes <- unique(outcomes[which(outcomes$comparison == "Different"),]) ## 27679 different, 20403 same

## Having looked at the "different_outcomes" manually, there are a few things that need fixing
## Note that most of the outcomes are the same but with minor spelling differences
## Below are some of the ones I have changes manually due to spelling differences
table(data$outcome[(which(data$ID == "880716487") & grepl("¬†", data$outcome))])
data$outcome[which(data$outcome == "¬†Chemokine (C-X-C motif) ligand 1")] <- "Chemokine (C-X-C motif) ligand 1"
data$outcome[which(data$outcome == "¬†Galectin-3")] <- "Galectin-3"
data$outcome[which(data$outcome == "Carbohydrate¬†antigen 125")] <- "Carbohydrate antigen 125"
data$outcome[which(data$outcome == "Cathepsin L1¬†")] <- "Cathepsin L1"
data$outcome[which(data$outcome == "Cluster of differentiation¬†40")] <- "Cluster of differentiation 40"
data$outcome[which(data$outcome == "Cystatin-B¬†")] <- "Cystatin-B"
data$outcome[which(data$outcome == "Galanin¬†")] <- "Galanin"
data$outcome[which(data$outcome == "Heparin-binding EGF-like growth factor¬†")] <- "Heparin-binding EGF-like growth factor"
data$outcome[which(data$outcome == "Integrin beta-1-binding protein¬†1")] <- "Integrin beta-1-binding protein 1"
data$outcome[which(data$outcome == "Interleukin¬†6¬†receptor, alpha")] <- "Interleukin 6 receptor, alpha"
data$outcome[which(data$outcome == "NF-kappa-B essential modulator¬†")] <- "NF-kappa-B essential modulator"
data$outcome[which(data$outcome == "Prolactin¬†")] <- "Prolactin"
data$outcome[which(data$outcome == "Vascular endothelial growth factor¬†A")] <- "Vascular endothelial growth factor A"
data$outcome[which(data$outcome == "Vascular endothelial growth factor¬†D")] <- "Vascular endothelial growth factor D"
data$outcome[which(data$outcome == "Vascular endothelial growth factor¬†D")] <- "Vascular endothelial growth factor D"
data$outcome[which(data$outcome == "Vascular endothelial growth factor¬†D")] <- "Vascular endothelial growth factor D"
data$outcome[which(data$outcome == "Vascular endothelial growth factor¬†D")] <- "Vascular endothelial growth factor D"
outcomes <- data[,c("ID","outcome","dup_outcome","outcome_formatted","outcome_group" )]
outcomes$comparison <- ifelse(outcomes$outcome == outcomes$dup_outcome, "Same", "Different")
table(outcomes$comparison)
different_outcomes <- unique(outcomes[which(outcomes$comparison == "Different"),]) 

## First ID which seems to be weird is 880716487 so I'm going to remove that from the data to check why
data_wo_8807 <- data[-which(data$ID == "880716487"),]
data_8807 <- data[which(data$ID == "880716487"),]

# Then I'm going to separate out the information that was originally extracted and extracted in duplicate
data_8807_initial <- data_8807[,c(1:85,96,97,98)]
data_8807_initial <- data_8807_initial[order(data_8807_initial$outcome, data_8807_initial$exposure),]
data_8807_dup <- data_8807[,c(1,86:95)]

# Then order based on the exposure and outcome columns
data_8807_dup <- data_8807_dup[order(data_8807_dup$dup_outcome, data_8807_dup$dup_exposure),]

# Then remerge and check whether that's worked
data_8807_dup$ID <- NULL
data_8807_remerged <- cbind(data_8807_initial, data_8807_dup)
outcomes_8807 <- data_8807_remerged[,c("ID","outcome","dup_outcome","outcome_formatted","outcome_group")]
outcomes_8807$comparison <- ifelse(outcomes_8807$outcome == outcomes_8807$dup_outcome, "Same", "Different")
table(outcomes_8807$comparison)
different_outcomes_8807 <- outcomes_8807[which(outcomes_8807$comparison == "Different"),]

# Now they are the same but only look different because of the spelling differences but want to check whether the exposures lined up
data_8807_check <- data_8807_remerged[, c("ID", "exposure", "dup_exposure", "outcome", "dup_outcome", "mrbeta","mrp","dup_mrbeta","dup_mrp")]
data_8807_check$mrbeta <- as.numeric(data_8807_check$mrbeta)
data_8807_check$dup_mrbeta <- as.numeric(data_8807_check$dup_mrbeta)
data_8807_check$mrp <- as.numeric(data_8807_check$mrp)
data_8807_check$dup_mrp <- as.numeric(data_8807_check$dup_mrp)
cor(data_8807_check$mrbeta, data_8807_check$dup_mrbeta, use = "pairwise.complete.obs") ## HOORAY!
cor(data_8807_check$mrp, data_8807_check$dup_mrp, use = "pairwise.complete.obs") ## HOORAY!

# Note that they still seem to appear different in the code so I'll make the dup_outcome match the outcome
data_8807_remerged$dup_outcome <- data_8807_remerged$outcome

# Now we rejoin the original dataframe (without the 880716487 ID) with this new formatted one
rm(data_8807, data_8807_initial, data_8807_dup, outcomes_8807, different_outcomes_8807, data_8807_check)
colnames(data_wo_8807)
colnames(data_8807_remerged)
data2 <- rbind(data_wo_8807, data_8807_remerged)
rm(data_wo_8807, data_8807_remerged)

## Now we recalculate those that are different between initial and duplicate information
rm(outcomes)
colnames(data2)
outcomes <- data2[,c("ID","outcome","dup_outcome","outcome_formatted","outcome_group")]
outcomes$comparison <- ifelse(outcomes$outcome == outcomes$dup_outcome, "Same", "Different")
table(outcomes$comparison)
different_outcomes <- outcomes[which(outcomes$comparison == "Different"),] ## 10714 different, 37268 same
unique_different_outcomes <- unique(different_outcomes)

## Next one where there are dissimilarities are with ID 4163561095
## Where initial outcome is "Depressive symptoms" but duplicate outcome is "Major depressive disorder"
data2[(which(data2$ID == "4163561095" & data2$outcome == "Depressive symptoms")), c("ID", "exposure", "dup_exposure", "outcome", "dup_outcome", "mrbeta","mrp","dup_mrbeta","dup_mrp")]
# Having looked at the paper again, the original outcome is correct so leaving as it is for now because I'll be creating a "final outcome" column based on the original outcome column anyway

## Other minor changes
data2$outcome[which(data2$outcome == "gama-glutamyl transferase")] <- "Gamma-glutamyl transferase"
data2$outcome[which(data2$outcome == "triacylglyceride")] <- "Triacylglyceride"

## EVERYTHING ELSE IS THE SAME OUTCOME, JUST SPELLED DIFFERENTLY SO CAN USE THE "outcome" COLUMN AS THE FINAL VERSION
rm(data, outcomes, different_outcomes, same_outcomes_8807, unique_different_outcomes)
data <- data2
rm(data2)

## Create a dataframe to check that all exposures have been formatted accordingly 
complete_outcome <- data[!is.na(data$outcome), c("ID","outcome","outcome_formatted")] ## should be 48083 rows
unique_complete_outcome <- unique(complete_outcome$outcome) 
data_unique_complete_outcome <- as.data.frame(unique_complete_outcome) 
write.xlsx(data_unique_complete_outcome, "./Data analysis/unique_outcomes.xlsx", colNames = FALSE, rowNames = FALSE, overwrite = TRUE) 
rm(complete_outcome, unique_complete_outcome, data_unique_complete_outcome)

######################################################################
## Checking duplicate extracted exposure data matches original data ##
######################################################################

## Check that the duplicate exposures match (i.e., original exposures and duplicate exposures and, if so, take new formatted ones)
colnames(data)
exposures <- data[,c("ID","exposure","dup_exposure","exposure_formatted","exposure_info")]
exposures$comparison <- ifelse(exposures$exposure == exposures$dup_exposure, "Same", "Different")
table(exposures$comparison)
different_exposures <- unique(exposures[which(exposures$comparison == "Different"),]) ## 28812 different, 19270 same
rm(different_exposures)

## There are some differences when it comes to naming some features (e.g., as either a Phylum or Class) so need to check these in IDs:
## 2383481240, 880716487, 2934159401, 3760378084, 4251029218
## First 2383481240
check_238 <- unique(data[which(data$ID == "2383481240"), c("ID","exposure","exposure_formatted","exposure_info","dup_exposure","mrbeta","mrp","dup_mrbeta","dup_mrp")])

# I've checked the paper and the duplicate outcome was write so have changed the "exposure_formatted"
data$exposure_formatted[(which(data$exposure_formatted == "Actinobacteria (Phylum)" & data$dup_exposure == "Actinobacteria (class)" & data$mrbeta == "0.99195997999999996"))] <- "Actinobacteria (Class)"
data$exposure_formatted[(which(data$exposure_formatted == "Actinobacteria (Phylum)" & data$dup_exposure == "Actinobacteria (class)" & data$mrbeta == "0.94964990999999999"))] <- "Actinobacteria (Class)"
data$exposure_formatted[(which(data$exposure_formatted == "Melainabacteria (Phylum)" & data$dup_exposure == "Melainabacteria (class)" & data$mrbeta == "0.94939921000000005"))] <- "Melainabacteria (Class)"

## Second 880716487
check_880 <- unique(data[which(data$ID == "880716487"), c("ID","exposure","exposure_formatted","exposure_info","dup_exposure","mrbeta","mrp","dup_mrbeta","dup_mrp")])

# The duplicated exposure is correct in that "Actinobacteria.id.419" refers to the class not the phylum
data[which(data$exposure == "Actinobacteria.id.419" & data$ID == "880716487"), c("ID","exposure","exposure_formatted","exposure_info","dup_exposure")]
data$exposure_formatted[which(data$exposure == "Actinobacteria.id.419" & data$ID == "880716487")] <- "Actinobacteria (Class)"
data$exposure_formatted[(which(data$exposure_formatted == "Melainabacteria (Phylum)" & data$dup_exposure == "Melainabacteria.id.1589 (class)" & data$ID == "880716487"))] <- "Melainabacteria (Class)"

## Third 2934159401
check_293 <- unique(data[which(data$ID == "2934159401"), c("ID","exposure","exposure_formatted","exposure_info","dup_exposure","mrbeta","mrp","dup_mrbeta","dup_mrp")])
data$exposure_formatted[(which(data$exposure_formatted == "Actinobacteria (Phylum)" & data$dup_exposure == "Actinobacteria" & data$mrbeta == "1.04282568" & data$ID == "2934159401"))] <- "Actinobacteria (Class)"
data$exposure_formatted[(which(data$exposure_formatted == "Actinobacteria (Phylum)" & data$dup_exposure == "Actinobacteria" & data$mrbeta == "0.39326664" & data$ID == "2934159401"))] <- "Actinobacteria (Class)"
data$exposure_formatted[(which(data$exposure_formatted == "Actinobacteria (Phylum)" & data$dup_exposure == "Actinobacteria class" & data$mrbeta == "1.3302065000000001" & data$ID == "2934159401"))] <- "Actinobacteria (Class)"

## Fourth 3760378084
check_376 <- unique(data[which(data$ID == "3760378084"), c("ID","exposure","exposure_formatted","exposure_info","dup_exposure","mrbeta","mrp","dup_mrbeta","dup_mrp")])
data$exposure_formatted[(which(data$exposure_formatted == "Melainabacteria (Phylum)" & data$dup_exposure == "Melainabacteria (Class)"))] <- "Melainabacteria (Class)"

## Need to check the full information provided in IDs 560890194 and 3278202937, as the duplicate exposure suggests a model and the original exposure does not
check_560 <- unique(data[which(data$ID == "560890194"), c("ID","exposure","exposure_formatted","exposure_info","outcome","dup_exposure","dup_outcome","mrbeta","mrp","dup_mrbeta","dup_mrp")])
check_327 <- unique(data[which(data$ID == "3278202937"), c("ID","exposure","exposure_formatted","exposure_info","outcome","dup_exposure","dup_outcome","mrbeta","mrp","dup_mrbeta","dup_mrp")])

# Add "Abundance (linear)" and "Presence vs. absence (logistic)" where appropriate in ID 560890194
data$exposure_info[(which(data$ID == "560890194" & grepl("abundance", data$dup_exposure)))] <- "Abundance (linear)"
data$exposure_info[(which(data$ID == "560890194" & grepl("logistic", data$dup_exposure)))] <- "Presence vs. absence (logistic)"

# Add "Abundance" and "Presence vs. absence" where appropriate in ID 3278202937
data$exposure_info[(which(data$ID == "3278202937" & grepl("Abundance", data$dup_exposure)))] <- "Abundance"
data$exposure_info[(which(data$ID == "3278202937" & grepl("Presence", data$dup_exposure)))] <- "Presence vs. absence"

## Fifth 4251029218
check_425 <- unique(data[which(data$ID == "4251029218"), c("ID","exposure","exposure_formatted","exposure_info","dup_exposure","mrbeta","mrp","dup_mrbeta","dup_mrp")])

# Having checked the original paper, the original outcome (genus) was correct
data$dup_exposure[(which(data$dup_exposure == "Akkermansia (family)" & data$exposure_formatted == "Akkermansia (Genus)"))] <- "Akkermansia (Genus)"

# Remove the checking datasets
rm(check_238, check_880, check_293, check_376, check_560, check_327, check_425)

## Some of the formatting of bacterial names in the exposure_formatted column for ID 2179267930 has gone a little strange (with "species" included too)
data$exposure_formatted[which(data$exposure_formatted == "Bacteroidespecies intestinalis (Species)")] <- "Bacteroides intestinalis (Species)"
data$exposure_formatted[which(data$exposure_formatted == "Methylobacilluspecies flagellatus (Species)")] <- "Methylobacillus flagellatus (Species)"
data$exposure_formatted[which(data$exposure_formatted == "Mobiluncuspecies curtisii (Species)")] <- "Mobiluncus curtisii (Species)"
data$exposure_formatted[which(data$exposure_formatted == "Pseudomonaspecies stutzeri (Species)")] <- "Pseudomonas stutzeri (Species)"
data$exposure_formatted[which(data$exposure_formatted == "Streptococcuspecies parasanguinis (Species)")] <- "Streptococcus parasanguinis (Species)"

## Some little spelling mistakes
data$exposure_info[which(data$exposure_info == "succinate_conversion_to propionate")] <- "Conversion from succinate to propionate"

## Recheck that the duplicate exposures match (i.e., original exposures and duplicate exposures and, if so, take new formatted ones)
colnames(data)
exposures <- data[,c("ID","exposure","dup_exposure","exposure_formatted","exposure_info")]
exposures$comparison <- ifelse(exposures$exposure == exposures$dup_exposure, "Same", "Different")
table(exposures$comparison)
different_exposures <- unique(exposures[which(exposures$comparison == "Different"),]) ## manually scrolled through this to see where there were differences

## EVERYTHING ELSE IS THE SAME EXPOSURE, JUST SPELLED DIFFERENTLY SO CAN USE THE "exposure_formmatted" COLUMN AS THE FINAL VERSION
rm(different_exposures, exposures)

#####################################################################
## Checking duplicate extracted summary data matches original data ##
#####################################################################

## Most important is checking that the duplicate betas, p-values, etc. match across initial and duplicate extractions
colnames(data)
data_test <- data[, c("ID","exposure_formatted","exposure_info","dup_exposure","outcome","dup_outcome","mrbeta","mrse","mrlci","mruci","mrp","mrpadj","dup_mrbeta","dup_mrse","dup_mrlci","dup_mruci","dup_mrp","dup_mrpadj")]
data_test$mrbeta <- as.numeric(data_test$mrbeta)
data_test$dup_mrbeta <- as.numeric(data_test$dup_mrbeta)
data_test$mrse <- as.numeric(data_test$mrse)
data_test$dup_mrse <- as.numeric(data_test$dup_mrse)
data_test$mrlci <- as.numeric(data_test$mrlci)
data_test$dup_mrlci <- as.numeric(data_test$dup_mrlci)
data_test$mruci <- as.numeric(data_test$mruci)
data_test$dup_mruci <- as.numeric(data_test$dup_mruci)
data_test$mrp <- as.numeric(data_test$mrp)
data_test$dup_mrp <- as.numeric(data_test$dup_mrp)
data_test$mrpadj <- as.numeric(data_test$mrpadj)
data_test$dup_mrpadj <- as.numeric(data_test$dup_mrpadj)
cor(data_test$mrbeta, data_test$dup_mrbeta, use = "pairwise.complete.obs") ## 1 - woohoo!
cor(data_test$mrse, data_test$dup_mrse, use = "pairwise.complete.obs") ## 1 - woohoo!
cor(data_test$mrlci, data_test$dup_mrlci, use = "pairwise.complete.obs") ## 1 - woohoo!
cor(data_test$mruci, data_test$dup_mruci, use = "pairwise.complete.obs") ## 1 - woohoo!
cor(data_test$mrp, data_test$dup_mrp, use = "pairwise.complete.obs") ## 1 - woohoo!
cor(data_test$mrpadj, data_test$dup_mrpadj, use = "pairwise.complete.obs") ## 1 - woohoo!

## Generate a column in the test dataset that shows the numerical difference in beta, se, lci, uci, p columns
data_test$beta_diff <- NA
data_test$se_diff <- NA
data_test$lci_diff <- NA
data_test$uci_diff <- NA
data_test$p_diff <- NA
data_test$padj_diff <- NA

## Want to see if there are any large differences that could indicate errors
## Note that I did this first time and there were some instances where there was at least one error 
## I have manually checked those an, cross-referencing the original papers, and finalised the spreadsheet
## The rest of the code just checks that these changes were made correctly and the only differences are due to negligible rounding differences
data_test$beta_diff <- data_test$mrbeta-data_test$dup_mrbeta
beta_diff <- data_test[which(abs(data_test$beta_diff)>0.0001),]
summary(beta_diff$beta_diff)

data_test$se_diff <- data_test$mrse-data_test$dup_mrse
se_diff <- data_test[which(abs(data_test$se_diff)>0.0001),]
summary(se_diff$se_diff)

data_test$lci_diff <- data_test$mrlci-data_test$dup_mrlci
lci_diff <- data_test[which(abs(data_test$lci_diff)>0.0001),]
summary(lci_diff$lci_diff)

data_test$uci_diff <- data_test$mruci-data_test$dup_mruci
uci_diff <- data_test[which(abs(data_test$uci_diff)>0.0001),]
summary(uci_diff$uci_diff)

data_test$p_diff <- data_test$mrp-data_test$dup_mrp
p_diff <- data_test[which(abs(data_test$p_diff)>0.0001),]
summary(p_diff$p_diff)

## Remove those individual datasets and restrict the test dataset to those that have differences that I've gone back to manually check
rm(beta_diff, se_diff, lci_diff, uci_diff, p_diff)
data_diff <- data_test %>%
  filter(abs(beta_diff > 0.0001) | abs(se_diff > 0.0001) | abs(lci_diff > 0.0001) | abs(uci_diff > 0.0001) | abs(p_diff > 0.0001))

## Therefore, there are now no instances where there were differences in the data that was extracted originally and in duplicate
table(unique(data_diff$ID)) 
rm(data_diff, data_test)

## Lastly, replace NA's in the adjusted p-values extracted in duplicate with those that are in the originally extracted data where there are missing data
new_data <- data %>% 
  mutate(dup_mrpadj = coalesce(dup_mrpadj, mrpadj))

## Now, remove original data and replace
rm(data)
data <- new_data
rm(new_data)

################################################################################################
## Checking that the F-statistics match across extractors and taking most comprehensive value ##
################################################################################################

## In a similar way as above, seeing whether the extracted F-statistics match across extractors
colnames(data)
data_test <- data[, c("ID","exposure_formatted","exposure_info","dup_exposure","outcome","dup_outcome","instrument_strength","dup_f")]
data_test$instrument_strength <- as.numeric(data_test$instrument_strength)
data_test$dup_f <- as.numeric(data_test$dup_f)
cor(data_test$instrument_strength, data_test$dup_f, use = "pairwise.complete.obs") # 1 - woohoo!

## Generate a column in the test dataset that shows the numerical difference in beta, se, lci, uci, p columns
data_test$f_diff <- NA

## Want to see if there are any large differences that could indicate errors
## Note that I did this first time and there were several rows where there was at least one error
## I have manually checked those an, cross-referencing the original paper, and finalised the spreadsheet
## Therefore, there are now no instances where there were differences in the data that was extracted originally and in duplicate
## The rest of the code just checks that these changes were made correctly and the only differences are due to negligible rounding differences
data_test$f_diff <- data_test$instrument_strength-data_test$dup_f
f_diff <- data_test[which(abs(data_test$f_diff)>0.0001),]
summary(f_diff$f_diff)
table(unique(f_diff$ID)) 
rm(f_diff, data_test)
 
## Lastly, replace NA's in the F-statistics extracted in duplicate with those that are in the originally extracted data where there are missing data
data_test <- data[, c("ID","exposure_formatted","exposure_info","dup_exposure","outcome","dup_outcome","instrument_strength","dup_f")]
new_data <- data %>% 
  mutate(dup_f = coalesce(dup_f, instrument_strength))
new_data <- data %>% 
  mutate(instrument_strength = coalesce(instrument_strength, dup_f))
new_data$instrument_strength <- as.numeric(new_data$instrument_strength)
new_data$dup_f <- as.numeric(new_data$dup_f)
cor(new_data$instrument_strength, new_data$dup_f, use = "pairwise.complete.obs") ## 1 so all good

## Now, remove original data and replace
rm(data, data_test)
data <- new_data
rm(new_data)

##########################################################################
## Checking all columns have information in them that is not unexpected ##
##########################################################################

## Tabulate all column entries to check there are no mistakes and to remove any unnecessary information
colnames(data)

## First section (information about the journal and publication)
table(unique(data$ID))
table(unique(data$abstract))
table(unique(data$preprint))
table(unique(data$published))
table(unique(data$title))
table(unique(data$year))
table(unique(data$journal))
table(unique(data$doi))
table(unique(data$first_author))
table(unique(data$corresponding_name))
table(unique(data$corresponding_email))
table(unique(data$corresponding_address))

## Generate a dataframe that outputs an object with the information that should be mostly the same across most columns across an individual study to check the wording
## Would this be the same as taking the first entry per ID? would need to check that, per ID, specific column entries were the same and therefore I could just check one of each
## I did this a couple of times manually to get one unique row per study (the only reasons for non-unique rows per study for these columns where they should have been unique were copying / pasting / spelling errors between rows)
## There were initially 74 unique rows considering these columns, where there should have been 66, but then actually two studies have both one- and two-sample MR results so there should be 68 rows)
info_cols_unique <- data[, c("ID", "hypothesis", "rationale", "study_design", "software","limitations", "limit_pleiotropy", "limit_pop", "limit_power")]
info_data_unique <- unique(info_cols_unique) ## Now writing this out so I can look in excel because it's easier with long entries per cell
duplicates <- info_data_unique[duplicated(info_data_unique$ID), ] 
write.xlsx(info_data_unique, "./Data analysis/DuplicatedentriesinRows.xlsx", asTable = FALSE, overwrite = TRUE, keepNA = TRUE) 
rm(duplicates, info_cols_unique, info_data_unique)

## Only one I want to harmonize is the study_design
data$study_design[which(data$study_design == "One-sample individual-level")] <- "One-sample"
data$study_design[which(data$study_design == "Two-sample summary-level")] <- "Two-sample"
table(data$study_design)

## Also noticed that there is an "NA" and "Not reported" for exposure_study so changing all to "Not reported"
data$exposure_study[which(data$exposure_study == "NA")] <- "Not reported"

## These would be different per line and just need to check they are ok - assume all entries are fine unless explicitly changed (mainly just getting rid of sassy comments again rather than changing anything for consistency because it would take too long and doesn't matter - e.g., doesn't matter if I've said "Not described" or "NA" because both indicate the same thing)
table(unique(data$outcome_def))
table(unique(data$outcome_study))
table(unique(data$outcome_age))
table(unique(data$outcome_sex))
table(unique(data$outcome_og_gwas_case))
table(unique(data$outcome_og_gwas_control)) 
table(unique(data$outcome_study_case))
table(unique(data$outcome_study_control)) 
table(unique(data$outcome_n_diff)) 
table(unique(data$outcome_health)) ## Noticed that there were lots of differences in how "case-control" studies, "healthy" were presented
data$outcome_health[which(data$outcome_health == "case-control")] <- "Case-control"
data$outcome_health[which(data$outcome_health == "case/control")] <- "Case-control"
data$outcome_health[which(data$outcome_health == "Case/control")] <- "Case-control"
data$outcome_health[which(data$outcome_health == "Case-contol")] <- "Case-control"
data$outcome_health[which(data$outcome_health == "case/contol")] <- "Case-control"
data$outcome_health[which(data$outcome_health == "Case/contol")] <- "Case-control"
data$outcome_health[which(data$outcome_health == "disease")] <- "Case-control"
data$outcome_health[which(data$outcome_health == "healthy")] <- "Healthy"
data$outcome_health[which(data$outcome_health == "Healthy (though aged 55-70)")] <- "Healthy"
data$outcome_health[which(data$outcome_health == "healthy participants")] <- "Healthy"
data$outcome_health[which(data$outcome_health == "Healthy participants")] <- "Healthy"
data$outcome_health[which(data$outcome_health == "Healthy participants - 402 early postmenopausal Chinese women were randomly recruited from Guangzhou City, China. The inclusion criteria included aged 40 years or older, being in early postmenopausal stage, and have lived in GuangZhou City for at least 3 months")] <- "Healthy postmenopausal women"
data$outcome_health[which(data$outcome_health == "healthy subjects")] <- "Healthy"
data$outcome_health[which(data$outcome_health == "Healthy subjects")] <- "Healthy"
data$outcome_health[which(data$outcome_health == "healthy subjects (though they write this as hypertension and present odds ratios)")] <- "Not clear - may be Healthy or Case-control"
data$outcome_health[which(data$outcome_health == "High-cardiovascular risk")] <- "Those with high-cardiovascular risk"
data$outcome_health[which(data$outcome_health == "Patients of at least one of 47 diseases were recruited into Biobank")] <- "Those with at least 47 diseases"
data$outcome_health[which(data$outcome_health == "Not clear - may be continuous or case-control")] <- "Not clear - may be Healthy or Case-control"
table(unique(data$outcome_health))
table(unique(data$outcome_pop)) ## There was one misspelled word
data$outcome_pop[which(data$outcome_pop == "Chinese (GuangZhou City)")] <- "Chinese (Guangzhou City)"
data$outcome_pop[which(data$outcome_pop == "Chinese (GuangZhou City)")] <- "Chinese (Guangzhou City)"
table(unique(data$outcome_pop)) 
table(unique(data$exposure_age))
data$exposure_age[which(data$exposure_age == "age 6-35")] <- "6-35"
data$exposure_age[which(data$exposure_age == "8 to 84")] <- "8-84" 
table(unique(data$exposure_age))
table(unique(data$exposure_sex)) ## lots of different ways of saying the same thing so changing
data$exposure_sex[which(data$exposure_sex == "both")] <- "Both"
data$exposure_sex[which(data$exposure_sex == "both (Twins UK 83% women)")] <- "Both"
data$exposure_sex[which(data$exposure_sex == "Females")] <- "Female"
table(unique(data$exposure_sex)) 
table(unique(data$exposure_study_n))
table(unique(data$exposure_og_n)) 
table(unique(data$exposure_n_diff)) 
data$exposure_n_diff[which(data$exposure_n_diff == "Not clear where this number has come from")] <- "Not clear"
data$exposure_n_diff[which(data$exposure_n_diff == "zero-truncation")] <- "Used the zero-truncated data"
table(unique(data$exposure_health)) 
data$exposure_health[which(data$exposure_health == "Healthy subject")] <- "Healthy"
data$exposure_health[which(data$exposure_health == "healthy subjects")] <- "Healthy"
data$exposure_health[which(data$exposure_health == 'healthy subjects (for all) but those with "normoglycemic" individuals for sanna et al.')] <- 'Healthy (for all other than Sanna et al., which includes "normoglycemic" individuals)'
table(unique(data$exposure_pop)) 
data$exposure_pop[which(data$exposure_pop == "European individuals")] <- "European"
data$exposure_pop[which(data$exposure_pop == "European samples only")] <- "European"
table(unique(data$exposure_gwas_covs)) 
data$exposure_gwas_covs[which(data$exposure_gwas_covs == "Adjusting for age, gender, shipment date, collection method and frst 3 PCs")] <- "Age, gender, shipment date, collection method and first 3 PCs"
data$exposure_gwas_covs[which(data$exposure_gwas_covs == "Adjustment for age, principal genetic components, technical covariates, sex and 5 (monoethnic cohorts) or 10 (multiethnic cohorts) principal components (PCs)")] <- "Age, principal genetic components, technical covariates, sex and 5 (monoethnic cohorts) or 10 (multiethnic cohorts) principal components (PCs)"
data$exposure_gwas_covs[which(data$exposure_gwas_covs == "Not mentioned with regards to the exposure data but sex, age, first 10 prinicpal components and genotyping batch with regards to the outcome data")] <- "Not mentioned with regards to the exposure data but sex, age, first 10 principal components and genotyping batch with regards to the outcome data"
table(unique(data$exposure_gwas_covs)) 
table(unique(data$exposure_impute)) 
data$exposure_impute[which(data$exposure_impute == "Not discussed - say that they used Human OmniExpress\r\narray for genotyping")] <- "Not discussed - say that they used Human OmniExpress array for genotyping"
table(unique(data$exposure_indep)) 
data$exposure_indep[which(data$exposure_indep == "Not mentioned and their figures and results suggest that they did not do this at all (I've never seen such biased and over estimated betas)")] <- "Not mentioned and their figures and results suggest that they did not do this" 
data$exposure_indep[which(data$exposure_indep == "r2 > 0.001 and clump window\r\n< 10,000 kb")] <- "r2 > 0.001 and clump window< 10,000 kb"
data$exposure_indep[which(data$exposure_indep == "r2 of 0.01 to identify independent\r\ninstruments")] <- "r2 of 0.01 to identify independent instruments"
table(unique(data$exposure_indep)) 
table(unique(data$pleiotropy))
table(unique(data$pval_threshold)) ## Lots of different ways of saying the same thing
data$pval_threshold[which(data$pval_threshold == "1.0000000000000001E-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "1x10-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "1x10e-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P < 1 × 10−5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P < 1.0 × 10−5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P < 1x10-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P <1x10-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "p< 1 × 10−5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P<1×10-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P<1x10-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P<1x10e-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "Same as in one-sample (P < 1 × 10−5)")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "Selected IVs at P<1×10-5")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P value threshold of 1×10−5 was used for selection of genetic predictors associated with microbial features")] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "p < 5.0 x10-5")] <- "5x10-05"
data$pval_threshold[which(data$pval_threshold == "P < 1x10-6")] <- "1x10-06"
data$pval_threshold[which(data$pval_threshold == "P <1 × 10−6")] <- "1x10-06"
data$pval_threshold[which(data$pval_threshold == "p< 1 × 10−6")] <- "1x10-06"
data$pval_threshold[which(data$pval_threshold == "5x10-6")] <- "5x10-06"
data$pval_threshold[which(data$pval_threshold == "p< 1 × 10−8")] <- "1x10-08"
data$pval_threshold[which(data$pval_threshold == "5x10-8")] <- "5x10-08"
data$pval_threshold[which(data$pval_threshold == "ACTUALLY USED 5E-8!!!")] <- "5x10-08"
data$pval_threshold[which(data$pval_threshold == "Genomewide significance level (P < 5 × 10−8)")] <- "5x10-08"
data$pval_threshold[which(data$pval_threshold == "P < 5 x 10-8")] <- "5x10-08"
data$pval_threshold[which(data$pval_threshold == "P < 5x10-8")] <- "5x10-08"
data$pval_threshold[which(data$pval_threshold == "P<5e-8")] <- "5x10-08"
data$pval_threshold[which(data$pval_threshold == "P < 5x10-8 (shown in Table S5) or 1 e-05")] <- "5x10-08 or 1x10-05"
data$pval_threshold[which(data$pval_threshold == "p< 1 × 10−9")] <- "1x10-09"
data$pval_threshold[which(data$pval_threshold == "p <10-5")] <- "P<10-05"
data$pval_threshold[which(data$pval_threshold == '"Loose" cut-off of p-value <1e-5')] <- "1x10-05"
data$pval_threshold[which(data$pval_threshold == "P<10-5")] <- "P<10-05"
data$pval_threshold[which(data$pval_threshold == "P<10−5")] <- "P<10-05"
table(unique(data$ID[which(data$pval_threshold == "cccccccccc")]))
data$pval_threshold[which(data$pval_threshold == "cccccccccc")] <- "5x10-05"
data$pval_threshold[which(data$pval_threshold == "Selected IVSs at 1e-05 (as part of their polygenic risk score analysis)")] <- "1x10-05"
data$pval_threshold[grep("where 1e-06 was used but it is not clear whether", data$pval_threshold)] <- "1x10-06"
data$pval_threshold[which(data$pval_threshold == "5e-08 from different studies")] <- "5x10-08"
data$pval_threshold[grep("Not discussed but says", data$pval_threshold)] <- "Not discussed"
data$pval_threshold[grep("analyses only on the microbial traits for which", data$pval_threshold)] <- "5x10-08"
table(unique(data$pval_threshold))
table(unique(data$proxy)) 
data$proxy[which(data$proxy == "Not needed for one-sample")] <- "Not discussed" # not sure that "Not needed for one-sample" is appropriate so changed
table(unique(data$hwe)) 
data$hwe[which(data$hwe == "In the GWAS section, authors mention that there is o deviation from Hardy-Weinberg equilibrium")] <- "In the GWAS section, authors mention that there is no deviation from Hardy-Weinberg equilibrium"
table(unique(data$single_instrument)) ## there seems to be instances where a single instrument is indicated but there is a "Not discussed"
unique(data$ID[which(data$single_instrument == "Not discussed")]) ## this is a study where they only provided the SNP number in Table 1 for a select number of results - note also that the Actinobacteria (Phylum) - IL-8 result they present in table 1 seems to not match with their results in the supplement (used here) and I think they actually meant IL-18 when they said IL-8
unique(data$ID[which(data$single_instrument == "NA")]) ## Lots here and, looking back, just seems these are where the SNP number wasn't presented at all
colnames(data)
test <- data[which(data$ID=="880716487" & (data$outcome == "Interleukin-8" | data$outcome == "Interleukin-18") & data$exposure_formatted == "Actinobacteria (Phylum)"), c("ID","exposure_formatted","outcome","mrbeta","mrp","dup_exposure","dup_outcome","dup_mrbeta","dup_mrp")]
data$single_instrument[which(data$single_instrument == "Not discussed")] <- "Not clear" ## Because of the discrepancy above, I'm going to write that it's not clear instead
rm(test)
table(unique(data$snp_number)) ## Seems as though snp_number was used as what would be "multiple_instrument" as an equivalent (in two-sample) because there isn't a 1 as a numerical value
unique(data$ID[which(data$snp_number == "Not discussed")]) ## Same one as before so going to change to not clear
data$snp_number[which(data$snp_number == "Not discussed")] <- "Not clear" 
unique(data$ID[which(data$snp_number == "NA")]) ## Lots here and, looking back, just seems these are where the SNP number wasn't presented at all
table(unique(data$ID[which(data$single_instrument == "NA" & data$GRS == "NA")])) ## Both are only ever missing when authors haven't presented SNPs
table(unique(data$ID[which(data$single_instrument == 1 & data$GRS != "NA")])) ## Checking that there isn't incompatible information provided - all good
table(unique(data$ID[which(data$single_instrument == 1 & data$snp_number != 1)])) ## Checking that there isn't incompatible information provided - few here
data$snp_number[which(data$single_instrument !="NA")] <- data$single_instrument[which(data$single_instrument !="NA")]
table(data$snp_number)
table(data$single_instrument)
data <- subset(data, select = -c(single_instrument))
table(unique(data$GRS)) 
table(unique(data$ID[which(data$GRS == "Not discussed")])) ## Same one as before so going to change to not clear
data$GRS[which(data$GRS == "Not discussed")] <- "Not clear" 
data$ID[which(data$GRS == 9)] ## This is a one-sample MR but there are no other entries in the "GRS" column so checking whether there are other one-sample MR analyses and how those SNP numbers have been reported
data$study_design[which(data$GRS == 9)] ## Given that this is the only one, I'll put this in the "snp_number" column and only have one column for this information
data$snp_number[which(data$GRS !="NA")] <- data$GRS[which(data$GRS !="NA")]
table(data$snp_number)
table(data$GRS)
data <- subset(data, select = -c(GRS))
table(unique(data$gwas_snp_number))
data$gwas_snp_number[which(data$gwas_snp_number == "138 (possibly)")] <- "Bonder (9 taxonomy and 33 microbial pathways; Wang (54 with abundance and 42 with beta-diversity)"
data$gwas_snp_number[which(data$gwas_snp_number == "Mixture of studies" & data$exposure_study == "Bonder 2016 and Wang 2016")] <- "Bonder (9 taxonomy and 33 pathways); Wang (54 with abundance and 42 with beta-diversity)"
data$gwas_snp_number[which(data$gwas_snp_number == "Mixture of studies" & data$exposure_study == "Mixture of Goodrich and Davenport")] <- "Goodrich (1 beta-diversity); Davenport (15 in winter, 14 in summer and 8 combined)"
data$gwas_snp_number[which(data$gwas_snp_number == "Mixture of studies" & data$exposure_study == "Mixture of Goodrich, Bonder and Davenport")] <- "Goodrich (1 beta-diversity); Bonder (9 taxonomy and 33 pathways); Davenport (15 in winter, 14 in summer and 8 combined)"
data$gwas_snp_number[which(data$gwas_snp_number == "Mixture of studies" & data$exposure_study == "Mixture of Wang, Goodrich and Turpin")] <- "Wang (54 with abundance and 42 with beta-diversity); Goodrich (1 beta-diversity); Turpin (58 with abundance)"
data$gwas_snp_number[which(data$gwas_snp_number == "Kurilshikov: 31 loci (1431 SNPs), Ruhlemann: 38 loci (44 genome-wide significant SNPs), Sanna: 3, Rhee: 31 discrete loci associated with 64 metabolites, Kettunen: 62 loci were significantly associated with at least one metabolic measure.")] <- "Kurilshikov: 31 loci (1431 SNPs), Ruhlemann: 38 loci (44 genome-wide significant SNPs)"
table(unique(data$gwas_snp_number))
table(data$ID[which(data$gwas_snp_number == "NA")]) ## Where no information was provided
table(unique(data$snp_number_diff)) 
data$snp_number_diff[which(data$snp_number_diff == "Not clear what they did")] <- "Not clear"
data$snp_number_diff[which(data$snp_number_diff == "Not explicitly mentioned")] <- "Not mentioned"
data$snp_number_diff[which(data$snp_number_diff == "Not reported how many SNPs were use")] <- "Not mentioned"
table(unique(data$snp_number_diff))
table(unique(data$power))
data$power[which(data$power == 'No - they conducted a "post-hoc" power calculation suggesting greater than 80% power for strong associations but less than 80% power for weaker associations, unsurprisingly')] <- 'No - they conducted a "post-hoc" power calculation suggesting greater than 80% power for strong associations but less than 80% power for weaker associations'
table(unique(data$power_method)) ## Could be harmonized more
data$power_method[which(data$power_method == "Calculated using the online tool https://shiny.cnsgenom\r\nics.com/mRnd/")] <- "https://shiny.cnsgenomics.com/mRnd/"
data$power_method[which(data$power_method == "mRnd (http://cnsgenomics.com/shiny/mRnd/)")] <- "https://shiny.cnsgenomics.com/mRnd/"
data$power_method[which(data$power_method == "mRnD website")] <- "https://shiny.cnsgenomics.com/mRnd/"
data$power_method[which(data$power_method == "Power calculations for MR were conducted based on the website: mRnd (http://cnsgenomics.com/shiny/mRnd/).")] <- "https://shiny.cnsgenomics.com/mRnd/"
data$power_method[which(data$power_method == "The website (http://cnsgenomics.com/shiny/mRnd/) was used to calculate the power")] <- "https://shiny.cnsgenomics.com/mRnd/"
data$power_method[which(data$power_method == "Used http://cnsgenomics.com/shiny/mRnd/")] <- "https://shiny.cnsgenomics.com/mRnd/"
data$power_method[which(data$power_method == "Used online tool from Stephen Burgess https://sb452.shinyapps.io/power/")] <- "https://sb452.shinyapps.io/power/"
data$power_method[which(data$power_method == "We assessed the power of our MR analyses using the online calculator mRnd (https://shiny.cnsgenomics.com/mRnd/)")] <- "https://shiny.cnsgenomics.com/mRnd/"
table(unique(data$power_method))
table(unique(data$main_method)) ## Could be harmonized more
data$main_method[which(data$main_method == "Inverse-variance weighted")] <- "Inverse variance weighted"
data$main_method[which(data$main_method == "Inverse-variance weighted random effects")] <- "Inverse variance weighted (random effects)"
data$main_method[which(data$main_method == "Inverse-variants weighted with multiplicative random effects")] <- "Inverse variance weighted (multiplicative random effects)"
data$main_method[which(data$main_method == "Inverse Variance Weighted (Fixed effects)")] <- "Inverse variance weighted (fixed effects)"
data$main_method[which(data$main_method == "IVW")] <- "Inverse variance weighted"
data$main_method[which(data$main_method == "IVW with multiplicative random effects")] <- "Inverse variance weighted (multiplicative random effects)"
data$main_method[which(data$main_method == "Two-stage least square regression (TSLS)")] <- "Two-stage least squares"
data$main_method[which(data$main_method == "Wald ratio (see notes)")] <- "Wald ratio"
table(unique(data$main_method))
list_of_sens <- lapply(c("sensitivity_1", "sensitivity_2", "sensitivity_3", "sensitivity_4", "sensitivity_5"), function(col) {
  data.frame(
    value = data[[col]],
    column = col
  )
})
combined_sens <- do.call(rbind, list_of_sens)
unique_sens <- combined_sens %>%
  group_by(value) %>%
  summarise(columns = paste(unique(column), collapse = ","), .groups = 'drop')
table(unique(unique_sens$value))
rm(list_of_sens, combined_sens, unique_sens)
columns_to_update <- c("sensitivity_1", "sensitivity_2", "sensitivity_3", "sensitivity_4", "sensitivity_5")
data[columns_to_update] <- apply(data[columns_to_update], 2, function(x) gsub("Cochrane's Q", "Cochran's Q", x))
data[columns_to_update] <- apply(data[columns_to_update], 2, function(x) gsub("Cochrans Q", "Cochran's Q", x))
data[columns_to_update] <- apply(data[columns_to_update], 2, function(x) gsub("MR Egger", "MR-Egger", x))
data[columns_to_update] <- apply(data[columns_to_update], 2, function(x) gsub("MR Egger (bootstrap)", "MR-Egger (bootstrap)", x))
data[columns_to_update] <- apply(data[columns_to_update], 2, function(x) gsub("MR Raps", "MR-RAPS", x))
data[columns_to_update] <- apply(data[columns_to_update], 2, function(x) gsub("MR TRYX", "MR-TRYX", x))
data[columns_to_update] <- apply(data[columns_to_update], 2, function(x) gsub("Penalised IVW", "Penalised inverse variance weighted", x))
data[columns_to_update] <- apply(data[columns_to_update], 2, function(x) gsub("Robust IVW", "Robust inverse variance weighted", x))
rm(columns_to_update)
table(unique(data$sensitivity_1))
table(unique(data$sensitivity_2))
table(unique(data$sensitivity_3))
table(unique(data$sensitivity_4))
table(unique(data$sensitivity_5))
table(unique(data$analysis_notes)) ## Need to consolidate some things here
data$analysis_notes[grep("As the SNPs were from one study, they were able", data$analysis_notes)] <- NA
data$analysis_notes[grep("Authors do not present the number of SNPs they found in the GWAS", data$analysis_notes)] <- NA
data$analysis_notes[grep("Betas and OR presented so have presented the Beta and SE here", data$analysis_notes)] <- NA
data$analysis_notes[grep("Really unclear what they did but think they undertook GWA", data$analysis_notes)] <- NA
data$analysis_notes[which(data$analysis_notes == "Very well presented")] <- NA
table(unique(data$analysis_notes))
table(unique(data$program)) ## Need to consolidate some things here
data$program[which(data$program == "MendelianRandomization package and MR-PRESSO")] <- "MendelianRandomization and MR-PRESSO packages"
data$program[which(data$program == "Mendelian Randomization and MR-PRESSO packages")] <- "MendelianRandomization and MR-PRESSO packages"
data$program[which(data$program == "MendelianRandomization R package")] <- "MendelianRandomization package"
data$program[which(data$program == "ivreg' command in AER package")] <- "ivreg command in AER package"
data$program[which(data$program == '"MR-Base"')] <- "MR-Base"
data$program[which(data$program == "MR-Base platform")] <- "MR-Base"
data$program[which(data$program == "TwoSampleMR and MRPRESSO")] <- "TwoSampleMR and MR-PRESSO"
data$program[which(data$program == '“Mendelian Randomization” and “MR-PRESSO” packages')] <- 'MendelianRandomization and MR-PRESSO packages'
data$program[which(data$program == 'TwoSampleMR (version 0.5.4) and “MRPRESSO” package')] <- 'TwoSampleMR (version 0.5.4) and MR-PRESSO package'
data$program[which(data$program == '"two-sample MR and MRPRESSO" packages')] <- 'TwoSampleMR (though authors say "two-sample MR") and MR-PRESSO packages'
data$program[which(data$program == "AER")] <- "AER package"
data$program[which(data$program == "TwoSampleMR package (V 0.5.6), mr. raps (V 0.2), Radial MR (V 1.0) and MR-PRESSO (V 1.0)")] <- "TwoSampleMR package (version 0.5.6), MR-RAPS (version 0.2), Radial MR (version 1.0) and MR-PRESSO (version 1.0)"
data$program[which(data$program == "TwoSampleMR v0.5.5")] <- "TwoSampleMR version 0.5.5"
data$program[which(data$program == "TwoSampleMR version 0.5.6.")] <- "TwoSampleMR (version 0.5.6)"
data$program[which(data$program == "TwosampleMR (version 0.56)")] <- "TwoSampleMR (version 0.5.6)"
data$program[which(data$program == "TwoSampleMR package")] <- "TwoSampleMR"
data$program[which(data$program == "TwosampleMR (version 0.5.6), MR-PRESSO (version 1.0), MRcML, and qvalue R packages.")] <- "TwoSampleMR (version 0.5.6), MR-PRESSO (version 1.0), MRcML, and qvalue packages"
data$program[which(data$program == "TwoSampleMR package (v.0.4.25)")] <- "TwoSampleMR (version 0.4.25)"
data$program[which(data$program == "TwoSampleMR version 4.02")] <- "TwoSampleMR version 4.02 (though not sure what version they used given the current version doesn't go beyond version 0.6.6)"
table(unique(data$program))
table(unique(data$replication_indep)) ## I think we should get rid of the "replication_indep" column because that's one thing that I want to look at in the review (i.e., did another study do the same thing)
data <- subset(data, select = -c(replication_indep))
table(unique(data$replication))
data$replication[which(data$replication == "This is the replication")] <- "This was the replication"
data$replication[which(data$replication == 'Yes - they use two studies to "replicate" their findings but note that these studies have measured the microbiome in different ways and they use the same outcome data so this is not a replication')] <- 'No - they use two studies to "replicate" their findings but note that these studies have measured the microbiome in different ways and they use the same outcome data so this is not a replication'
data$replication[which(data$replication == 'They used a sub-sample of the original sample as a "follow-up" and used this to validate the original result')] <- 'No - They used a sub-sample of the original sample as a "follow-up" and used this to validate the original result'
data$replication[which(data$replication == "To confirm the robustness of our results, we further performed the following sensitivity analyses: 1) for microbiota features with at least 3 SNPs, the weighted median method (30) was performed to test the causal effects from microbiota feature to TFM; and 2) in the UKB individual-level genetic data, we derived a weighted genetic risk score (GRS) as a secondary IV for the identified bacterial taxa and then examined the association of GRS with the outcome TFM.")] <- "No - they looked at the association between the GRS of the significant microbial traits and the outcome directly"
table(unique(data$multiple_testing)) 
table(unique(data$mradjustment)) 
data$mradjustment[which(data$mradjustment == "Bonferroni correction")] <- "Bonferroni"
data$mradjustment[which(data$mradjustment == "BH")] <- "Benjamini-Hochberg"
data$mradjustment[which(data$mradjustment == "Benjamin-Hochberg procedure (FDR)")] <- "Benjamini-Hochberg"
data$mradjustment[which(data$mradjustment == "multiple test corrected significance (10−4<P")] <- '"multiple test corrected significance (10−4<P)"'
table(unique(data$mradjustment)) 
table(unique(data$exposure_unit))
data$exposure_unit[which(data$exposure_unit == '"relative abundance" but not clear what this unit actually is')] <- "Relative abundance (but not clear what unit is)"
data$exposure_unit[which(data$exposure_unit == "1-SD of the log-transformed abundance")] <- "Abundance (SD of log-transformed values)"
data$exposure_unit[which(data$exposure_unit == "Abundance")] <- "Abundance (but not clear what unit is)"
data$exposure_unit[which(data$exposure_unit == 'An approximate doubling of the genetic liability to presence (vs. absence) of each binary microbial trait (those denoted with “P/A”)')] <- "Presence (approximate doubling of the genetic liability to presence vs. absence)"
data$exposure_unit[which(data$exposure_unit == "Authors say relative abundance but no unit of measurement")] <- "Relative abundance (but not clear what unit is)"
data$exposure_unit[which(data$exposure_unit == "log(OR) for presence")] <- "Presence (log(OR))"
data$exposure_unit[which(data$exposure_unit == "Never mentioned but possibly SD or log SD")] <- "Not clear (possibly SD or SD of log values)"
data$exposure_unit[which(data$exposure_unit == "Normalised bacterial counts but not clear")] <- 'Not clear (authors say "normalised bacterial counts")'
data$exposure_unit[which(data$exposure_unit == "Not clear - likely SD change but not mentioned")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "Not clear - they do say that the GWAS summary statistics are standardised (mean 0 and variance 1), suggesting it would be SD but it is not clear that this is correct")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "Not clear at all")] <- "Not clear"
data$exposure_unit[which(data$exposure_unit == "Not clear but Bonder GWAS units is meta-analysis z-score")] <- "Not clear (possibly Z-score)"
data$exposure_unit[which(data$exposure_unit == "Not mentioned but likely SD")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "Not presented nor are the units interpretable")] <- "Not clear"
data$exposure_unit[which(data$exposure_unit == "Per relative abundance (Box-Cox–transformed)")] <- "Relative abundance (Box-Cox transformed)"
data$exposure_unit[which(data$exposure_unit == "Per relative abundance (log10)")] <- "Relative abundance (log10 transformed)"
data$exposure_unit[which(data$exposure_unit == "Possibly SD but not clear - pathway abundances were recorded as reads per kilobase of transript per million reads mapped and then inverse-rank transformed")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "Possibly SD but not specified for the exposures")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "Presence")] <- "Presence (but not clear what unit is)"
data$exposure_unit[which(data$exposure_unit == "Presence vs. absence (doubling of genetic liability)")] <- "Presence (approximate doubling of the genetic liability to presence vs. absence)"
data$exposure_unit[which(data$exposure_unit == "Relative abundance")] <- "Relative abundance (but not clear what unit is)"
data$exposure_unit[which(data$exposure_unit == "SD")] <- "Abundance (SD)"
data$exposure_unit[which(data$exposure_unit == "SD - though not clear (rank-based inverse normal transformation)")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "SD (abundance)")] <- "Abundance (SD)"
data$exposure_unit[grep("I think", data$exposure_unit)] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "SD but not clear because they only say that effect estimates represent betas for continuous exposures")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "SD but not mentioned")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "SD but really not clear")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "SD change")] <- "Abundance (SD)"
data$exposure_unit[which(data$exposure_unit == "SD change for rank normalised AB microbial traits")] <- "Abundance (SD)"
data$exposure_unit[which(data$exposure_unit == "SD in relative abundance")] <- "Relative abundance (SD)"
data$exposure_unit[which(data$exposure_unit == "Unclear")] <- "Not clear"
data$exposure_unit[which(data$exposure_unit == 'Unclear (presented as "decrease in bacterial taxa")')] <- 'Not clear (authors say "decrease in bacterial taxa")'
data$exposure_unit[which(data$exposure_unit == "Unclear but likely SD (however, they have clumped data from multiple studies and multiple SNPs across studies without normalisation)")] <- "Not clear (possibly SD)"
data$exposure_unit[which(data$exposure_unit == "unit decrease")] <- 'Not clear (authors say "unit decrease")'
data$exposure_unit[which(data$exposure_unit == "Unit decrease but not clear what this is given the use of two studies")] <- 'Not clear (authors say "unit decrease")'
data$exposure_unit[which(data$exposure_unit == "unit increase")] <- 'Not clear (authors say "unit increase")'
data$exposure_unit[which(data$exposure_unit == "Z-score decrease")] <- 'Not clear (authors say "Z-score decrease")'
data$exposure_unit[which(data$exposure_unit == "Z-score increase")] <- 'Not clear (authors say "Z-score increase")'
table(unique(data$exposure_unit))
table(unique(data$outcome_unit))
data$outcome_unit[which(data$outcome_unit =='"natural log transformed" 25(OH)D')] <- 'Not clear (authors say "natural log transformed" 25(OH)D)'
data$outcome_unit[which(data$outcome_unit =='"outcome units" ')] <- 'Not clear (authors say "outcome units")'
data$outcome_unit[which(data$outcome_unit =="%")] <- "Percentage"
data$outcome_unit[which(data$outcome_unit =="Authors only say that telomere length was expressed as a ratio (T/S) of telomere repeat length (T) to copy the number of a single copy gene (S) so it's not clear")] <- "Not clear (possibly a ratio (T/S) of telomere repeat length (T) to copy the number of a single copy gene (S))"
data$outcome_unit[which(data$outcome_unit =="case-control")] <- "OR"
data$outcome_unit[which(data$outcome_unit =="Case-control")] <- "OR"
data$outcome_unit[which(data$outcome_unit =="Case/contro l")] <- "OR"
data$outcome_unit[which(data$outcome_unit =="case/control")] <- "OR"
data$outcome_unit[which(data$outcome_unit =="Case/control")] <- "OR"
data$outcome_unit[which(data$outcome_unit =="log-transformed (but not sure what original units are)")] <- "Not clear (log-transformed units)"
data$outcome_unit[which(data$outcome_unit =="Log odds")] <- "log odds"
data$outcome_unit[which(data$outcome_unit =="Never mentioned - continous variable but units not mentioned")] <- "Not clear"
data$outcome_unit[which(data$outcome_unit =="Not clear - results presented as ORs but this is a continuous variable (usually mmHG but could be SD)")] <- "Not clear (results presented as ORs but this is a continuous variable)"
data$outcome_unit[which(data$outcome_unit =="Not clear - though they say that the outcomes are on a standardised scale (mean 0 and variance 1), which wouldn't make sense for the binary outcomes")] <- "Not clear (possibly SD but this is a binary variable)"
data$outcome_unit[which(data$outcome_unit =="Not clear and Information not provided in the Supplement")] <- "Not clear"
data$outcome_unit[which(data$outcome_unit =='Not clear but Supplementary Table S1 shows transformation (and therefore maybe unit?) is (assuming the same thing as "E/A" in their supplement) rank-based inverse normal transformation')] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =='Not clear but Supplementary Table S1 shows transformation (and therefore maybe unit?) is (assuming this is the same as "A/G" written in their supplement) rank-based inverse normal transformation')] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="Not clear but Supplementary Table S1 shows transformation (and therefore maybe unit?) is Common log transformation + Z-score")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="Not clear but Supplementary Table S1 shows transformation (and therefore maybe unit?) is Rank-based inverse normal transformation")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="Not clear but Supplementary Table S1 shows transformation (and therefore maybe unit?) is Z-score")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="Not mentioned but likely % given phenotype")] <- "Not clear (possibly %)"
data$outcome_unit[which(data$outcome_unit =="kilograms per square meter")] <- "kg/m^2"
data$outcome_unit[which(data$outcome_unit =="Not mentioned but likely no unit given ratio")] <- "Not clear (possibly ratio)"
data$outcome_unit[which(data$outcome_unit =="Not mentioned but likely kg/m^2 from UK Biobank showcase")] <- "Not clear (possibly kg/m^2)"
data$outcome_unit[which(data$outcome_unit =="Not mentioned but likely mmHg given UK Biobank showcase")] <- "Not clear (possibly mmHg)"
data$outcome_unit[which(data$outcome_unit =="Not mentioned but likely mmol/L from UK Biobank showcase")] <- "Not clear (possibly (mmol/L)"
data$outcome_unit[which(data$outcome_unit =="Not mentioned but likely mmol/mol from the UK Biobank Showcase")] <- "Not clear (possibly mmol/mol)"
data$outcome_unit[which(data$outcome_unit =="Not presented though from the GWAS, this is likely mmol/L")] <- "Not clear (possibly mmol/L)"
data$outcome_unit[which(data$outcome_unit =="Not stated but this is likely OR")] <- "Not clear (possibly OR)"
data$outcome_unit[which(data$outcome_unit =="Not stated but this is likely SD")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="Not stated, but from above, this is likely log odds")] <- "Not clear (possibly log odds)"
data$outcome_unit[which(data$outcome_unit =="ot stated, but from above, this is likely SD")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="OR - case control")] <- "OR"
data$outcome_unit[which(data$outcome_unit =="original")] <- 'Not clear (authors say "original")'
data$outcome_unit[which(data$outcome_unit =="Possibly kg but never explicitly stated in terms of how to interpret the MR estimates")] <- "Not clear (possibly kg)"
data$outcome_unit[which(data$outcome_unit =="Possibly SD but not clear - each metabolite was inverse-rank transformed")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="Says SD in the table online but it's a binary phenotype so should be Log odds")] <- "Not clear (possibly SD but this is a binary variable)"
data$outcome_unit[which(data$outcome_unit =="Says SD in the table online but should be log odds ratio as binary")] <- "Not clear (possibly SD but this is a binary variable)"
data$outcome_unit[which(data$outcome_unit =="SD - in the methods but units never described in results")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="SD - though not clear (rank-based inverse normal transformation)")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="SD (kg/m^2)")] <- "SD"
data$outcome_unit[which(data$outcome_unit =="SD but not clear")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="SD units")] <- "SD"
data$outcome_unit[which(data$outcome_unit =="Unclear")] <- "Not clear"
data$outcome_unit[which(data$outcome_unit =="Not stated, but from above, this is likely SD")] <- "Not clear (possibly SD)"
data$outcome_unit[which(data$outcome_unit =="log OR")] <- "log(OR)"
table(unique(data$outcome_unit))

# Also want to check that any outcome unit that is quoted as being an odds ratio is actually an odds ratio and not a log odds ratio (i.e., if there are negative numbers, it is likely a log oods ratio)
data$mrbeta <- as.numeric(data$mrbeta)
summary(data$mrbeta[which(data$outcome_unit == "OR")]) ## some negative numbers indicating that it's likely there are log odds ratios but there might also be positive numbers that are log odds (important for meta-analyses)
table(unique(data$ID[which(data$outcome_unit == "OR" & data$mrbeta <0)])) 
length(unique(data$ID[which(data$outcome_unit == "OR" & data$mrbeta <0)])) ## loads of IDs that have negative betas so will look individually (might be that these are just all log odds ratios)

# Let's go one by one
unit_1055 <- data[which(data$ID == 1055369512), c("ID", "exposure_formatted","outcome","exposure_unit","outcome_unit","mrbeta","mrse","mrlci","mruci","mrp")] ## looks like the results specifically for multiple site chronic pain are actually log odds ratios
summary(data$mrbeta[which(data$ID == 1055369512 & data$outcome == "Multiple site chronic pain" & data$outcome_unit == "OR")])
data$outcome_unit[which(data$ID == 1055369512 & data$outcome == "Multiple site chronic pain" & data$outcome_unit == "OR")] <- "log(OR)"
table(unique(data$outcome_unit[which(data$ID == 1055369512 & data$outcome == "Multiple site chronic pain")]))
rm(unit_1055)

# I've checked these and they look like they all should be labelled as log odds ratios because of the magnitude and/or direction of the effect estimate (i.e., negative) despite being a binary outcome with information about the SE and p-values provided
unit_var <- c(122541860, 1286056190, 1324207092, 1519899723, 1795779689, 1915685263, 23119582, 2547037187, 2738712179, 3272561227, 3278202937, 4163561095, 4251029218, 718621312, 776143774, 95001313, 950921645)
for(i in unit_var){
  unit <- data[which(data$ID == i), c("ID", "exposure_formatted","outcome","exposure_unit","outcome_unit","mrbeta","mrse","mrlci","mruci","mrp")] 
  summary(data$mrbeta[which(data$ID == i & data$outcome_unit == "OR")])
  data$outcome_unit[which(data$ID == i & data$outcome_unit == "OR")] <- "log(OR)"
  table(unique(data$outcome_unit[which(data$ID == i)]))
  rm(unit)
}
rm(i, unit_var)
unit_os<- data[which(data$ID == "Other sources"), c("ID", "exposure_formatted","outcome","exposure_unit","outcome_unit","mrbeta","mrse","mrlci","mruci","mrp")] ## looks like these are all log odds ratios as well
summary(data$mrbeta[which(data$ID == "Other sources" & data$outcome_unit == "OR")])
data$outcome_unit[which(data$ID == "Other sources" & data$outcome_unit == "OR")] <- "log(OR)"
table(unique(data$outcome_unit[which(data$ID == "Other sources")]))
rm(unit_os)
unit_8807<- data[which(data$ID == 880716487), c("ID", "exposure_formatted","outcome","exposure_unit","outcome_unit","mrbeta","mrse","mrlci","mruci","mrp")] ## looks like these are all log odds ratios
summary(data$mrbeta[which(data$ID == 880716487 & data$outcome_unit == "OR")]) ## as there are no SEs (but there are negative betas), it is unclear whether these are odds ratios or log odds ratios just based on the beta and p-values
data$outcome_unit[which(data$ID == 880716487 & data$outcome_unit == "OR")] <- "Not clear (possibly log odds)"
table(unique(data$outcome_unit[which(data$ID == 880716487)]))
rm(unit_8807)

# Back to the other columns
data$other_gwasinfo[which(data$other_gwasinfo == "This is the GWAS of the exposure (for metabolites) and then they didn't present a huge amount of information about MiBioGen (for microbiome) so had to go to the GWAS of the microbiome and of the cardiovascular outcome")] <- "They didn't present a huge amount of information about MiBioGen (for microbiome) so had to go to the GWAS of the microbiome and of the cardiovascular outcome"
table(unique(data$other_gwasinfo))
table(unique(data$other_else))
data$other_else[which(data$other_else == "No")] <- "NA"
data$other_else[which(data$other_else == "They looked at metabolites as well as exposures;")] <- "They looked at metabolites as well as exposures"

## Removing the other_bias column but want to output another spreadsheet to help with writing and justification of the risk of bias which captures the unique rows for each study
bias_data <- data[, c("ID", "year", "journal", "doi", "first_author", "other_bias")]
bias_data_unique <- unique(bias_data)
write.xlsx(bias_data_unique, "./Data analysis/Bias_Identified_from_Extraction.xlsx", asTable = FALSE, overwrite = TRUE, keepNA = TRUE) ## I looked at this file and made sure that all entries were correct and that there were no sassy comments...!
rm(bias_data, bias_data_unique)
data <- subset(data, select = -c(other_bias))

## Check that all columns have been checked
colnames(data) 

# Publishing info: ID, abstract, preprint, published, title, year, journal, doi, first_author, corresponding_name, corresponding_email, corresponding_address
# Motivation and design: hypothesis, rationale, study_design
# Outcome info: outcome_formatted, outcome, outcome_group, outcome_def, outcome_study, outcome_age, outcome_sex, outcome_og_gwas_case, ,outcome_og_gwas_control, outcome_study_case, outcome_study_control, outcome_n_diff, outcome_health, outcome_pop
# Exposure info: exposure_group, exposure, exposure_study, exposure_age, exposure_sex, exposure_study_n, exposure_og_n, exposure_n_diff, exposure_health, exposure_pop, exposure_gwas_covs, exposure_impute, exposure_indep
# Instrumentation: pleiotropy, pval_threshold, proxy, hwe, snp_number, single_instrument (removed), GRS (removed), gwas_snp_number, snp_number_diff, instrument_strength
# Power: power, power_method
# Analysis methods:"main_method", sensitivity_1, sensitivity_2, sensitivity_3, sensitivity_4, sensitivity_5, analysis_notes
# Software: program, software
# Replication and multiple testing: replication_indep, replication, multiple_testing, mradjustment
# Units: exposure_unit, outcome_unit
# Limitations: limitations, limit_pleiotropy, limit_pop, limit_power
# Other stuff: other_gwasinfo, other_else, other_bias, initial_extractor

## Now outputting the results into a cleaner spreadsheet 
write.xlsx(data, "./Data analysis/Cleaned_Data_Extracted.xlsx", asTable = TRUE, overwrite = TRUE, keepNA = TRUE)
dim(data) ## 48083
rm(data)

## Now move onto the "data_synthesis.R" script
############################################################################ END ############################################################################

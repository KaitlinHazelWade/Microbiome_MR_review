#########################################################################################
## Project: Systematic review of MR studies with the gut microbiome as an exposure
## Script: Creating summaries and plots required for the paper
## Created: 20/09/2023
## By: Kaitlin Wade
#########################################################################################
rm(list=ls())

################
## Setting up ##
################

#### Install packages
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
#install.packages("tidyverse")
#install.packages("RColorBrewer")
#install.packages("patchwork")

#### Load packages
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
library(tidyverse)
library(RColorBrewer)
library(patchwork)

#### Set the working directory (directory has been removed for publication)
setwd("...")

#### Reading in appropriate data after cleaning and analyses
data <- read.xlsx("./Data analysis/Cleaned_Data_Extracted_For_Analyses.xlsx") 
colnames(data)

############################################################ SUMMARISING AND PLOTTING DATA FOR PAPER ############################################################

##############
## ABSTRACT ##
##############
#### Summarising the number of studies and estimates
length(unique(data$ID))
dim(data) ## 48082

#### Summarising the number of exposures and outcomes
length(unique(data$exposure_formatted)) ## 612
length(unique(data$outcome)) ## 905

#### Summarising the number of outcome types
table(data$outcome_group) ## These add up to 47594
length(data$outcome_group[which(data$outcome_group != "NA")]) 
length(data$ID[data$outcome_group=="NA"]) ## An additional 100 which are listed as "NA"

#### Nothing in the introduction

#############
## METHODS ##
#############
#### Types of information extracted
colnames(data)

#### Extra columns that were added
table(unique(data$exposure))
table(unique(data$exposure_formatted))
table(unique(data$exposure_group))

table(unique(data$outcome))
table(unique(data$outcome_formatted))
table(unique(data$outcome_group))

#############
## RESULTS ##
#############
#### Generate a table to put in the paper with information on ID, author, title, year, journal, study design, exposures, exposure studies, outcomes, outcome groups, outcome studies
colnames(data)
table <- data[,c("ID","first_author","title","journal","doi","year","study_design","exposure_formatted","e_unit","study1","outcome","outcome_group","study2","o_unit")]
head(table)

# Summarise those group sizes
group_sizes <- table %>%
  count(ID)
summary_stats <- group_sizes %>%
  summarise(
    min_rows = min(n),
    max_rows = max(n),
    mean_rows = mean(n),
    median_rows = median(n)
  )
print(summary_stats)

data_summarized <- table %>%
  group_by(ID, first_author, title, journal, doi, year) %>%
  summarize(
    study_design = paste(unique(na.omit(study_design)), collapse = ", "),
    exposure = paste(unique(na.omit(exposure_formatted)), collapse = ", "),
    study1 = paste(unique(na.omit(study1)), collapse = ", "),
    outcome = paste(unique(na.omit(outcome)), collapse = ", "),
    outcome_group = paste(unique(na.omit(outcome_group)), collapse = ", "),
    study2 = paste(unique(na.omit(study2)), collapse = ", "),
    .groups = "drop"
  )
write.xlsx(data_summarized, "./Paper/Summary_Table.xlsx", sheetName = "Full Summary", overwrite = TRUE, colNames = TRUE, rowNames = FALSE, asTable = TRUE)
rm(data_summarized)

data_brief_summ <- table %>%
  group_by(ID, first_author, year) %>%
  summarize(
    study_design = paste(unique(na.omit(study_design)), collapse = ", "),
    study1 = paste(unique(na.omit(study1)), collapse = ", "),
    outcome_group = paste(unique(na.omit(outcome_group)), collapse = ", "),
    study2 = paste(unique(na.omit(study2)), collapse = ", "),
    .groups = "drop"
  )
write.xlsx(data_brief_summ, "./Paper/Brief_Summary_Table.xlsx", sheetName = "Brief Summary", overwrite = TRUE, colNames = TRUE, rowNames = FALSE, asTable = TRUE)
rm(data_brief_summ, table)

## Number published vs. conference abstracts / pre-prints (remembering that one pre-print was published)
data$published[which(data$ID == 1234567891)] <- "Yes"
data$abstract[which(data$ID == 1234567891)] <- "No"
table(unique(data$published))
table(unique(data$abstract))
table(unique(data$preprint))
length(unique(data$ID[which(data$published== "Yes")]))
length(unique(data$ID[which(data$published== "No")]))
length(unique(data$ID[which(data$abstract== "Yes")]))
length(unique(data$ID[which(data$abstract== "No")]))
length(unique(data$ID[which(data$abstract== "(Duplicate conference abstract)")]))
table(unique(data$ID[which(data$abstract== "Yes")]))
table(unique(data$ID[which(data$abstract== "(Duplicate conference abstract)")]))
length(unique(data$ID[which(data$preprint== "Yes")]))
length(unique(data$ID[which(data$preprint== "No")]))
length(unique(data$ID[which(data$preprint== "(Duplicate pre-print)")]))
table(unique(data$ID[which(data$preprint== "Yes")]))
table(unique(data$ID[which(data$preprint== "(Duplicate pre-print)")]))

#### Year of publication
IDs <- unique(data$ID)
for(i in IDs){
  print(paste0(i, ": ", unique(data$year[which(data$ID == i)]), " ", unique(data$abstract[which(data$ID == i)]), " ", unique(data$preprint[which(data$ID == i)]), " ", unique(data$published[which(data$ID == i)])))
}

data$year <- as.numeric(data$year)
summary(data$year)
a <- length(unique(data$ID[which(data$year == 2018)]))
b <- length(unique(data$ID[which(data$year == 2019)]))
c <- length(unique(data$ID[which(data$year == 2020)]))
d <- length(unique(data$ID[which(data$year == 2021)]))
e <- length(unique(data$ID[which(data$year == 2022)]))
f <- length(unique(data$ID[which(data$year == 2023)]))
print(paste0("2018: ", (a/66)*100))
print(paste0("2019: ", (b/66)*100))
print(paste0("2020: ", (c/66)*100))
print(paste0("2021: ", (d/66)*100))
print(paste0("2022: ", (e/66)*100))
print(paste0("2023: ", (f/66)*100))
rm(a,b,c,d,e,f)

#### Country
## Need to harmonize the corresponding address
table(unique(data$corresponding_address)) ## Looking at these, they include the following countries and need to summarise this information
table(unique(data$ID[which(grepl("Deming Department of Medicine, Tulane University School of Medicine, Tulane University, New Orleans", data$corresponding_address))])) ## This includes both China and USA so went back to the original paper and the very last author is in the USA so changing
data$corresponding_address[which(grepl("Deming Department of Medicine, Tulane University School of Medicine, Tulane University, New Orleans", data$corresponding_address))] <- "Tulane Center of Biomedical Informatics and Genomics, Deming Department of Medicine, Tulane University School of Medicine, Tulane University, New Orleans, LA 70112, USA"
table(unique(data$corresponding_address[which(grepl("Harvard T.H. Chan School of Public Health, Boston, Massachusetts.", data$corresponding_address))])) ## This includes Boston but didn't mention the country
data$corresponding_address[which(grepl("Harvard T.H. Chan School of Public Health, Boston, Massachusetts.", data$corresponding_address))] <- "Department of Epidemiology, Harvard T.H. Chan School of Public Health, Boston, Massachusetts, USA"
table(unique(data$corresponding_address[which(data$corresponding_address == "Department of Genetics, University of Groningen, University Medical Center Groningen, Groningen, the Netherland")])) ## This has spelled the Netherlands incorrectly 
data$corresponding_address[which(data$corresponding_address == "Department of Genetics, University of Groningen, University Medical Center Groningen, Groningen, the Netherland")] <- "Department of Genetics, University of Groningen, University Medical Center Groningen, Groningen, the Netherlands"
table(unique(data$corresponding_address[which(grepl("London", data$corresponding_address))])) ## This includes London but not the country
data$corresponding_address[which(grepl("London", data$corresponding_address))] <- "Department of Twin Research\r\nand Genetic Epidemiology, King’s College London, St Thomas’\r\nHospital, London SE1 7EH, UK"
table(unique(data$corresponding_address[which(grepl("Environmental Health, Harvard T.H. Chan School of Public\r\nHealth, Boston, MA, United States", data$corresponding_address))])) ## This includes "United States", but want to harmonize to USA
data$corresponding_address[which(grepl("Environmental Health, Harvard T.H. Chan School of Public\r\nHealth, Boston, MA, United States", data$corresponding_address))] <- "Environmental Health, Harvard T.H. Chan School of Public\r\nHealth, Boston, MA, USA"
table(unique(data$ID[which(grepl("UK, 3\r\nDepartment of Microbiology and Immunology, Rega Instituut, KU Leuven–University of Leuven, Leuven, Belgium", data$corresponding_address))])) ## This includes both UK and Belgium so want to see who the last author is
data$corresponding_address[which(grepl("UK, 3\r\nDepartment of Microbiology and Immunology, Rega Instituut, KU Leuven–University of Leuven, Leuven, Belgium", data$corresponding_address))] <- "Department of Microbiology and Immunology, Rega Instituut, KU Leuven–University of Leuven, Leuven, Belgium"
table(unique(data$ID[which(grepl("Department of Paediatric Rheumatology, Bristol Royal Hospital For Children", data$corresponding_address))])) ## This includes several places in Bristol but not the country
data$corresponding_address[which(grepl("Department of Paediatric Rheumatology, Bristol Royal Hospital For Children", data$corresponding_address))] <- "MRC Integrative Epidemiology Unit, University of Bristol, UK"
table(unique(data$corresponding_address[which(grepl("Population Health Sciences, Bristol Medical School, University of Bristol, BS8 2BN", data$corresponding_address))])) ## This includes Bristol but not the country
data$corresponding_address[which(grepl("Population Health Sciences, Bristol Medical School, University of Bristol, BS8 2BN", data$corresponding_address))] <- "Population Health Sciences, Bristol Medical School, University of Bristol, BS8 2BN, UK"
table(unique(data$ID[which(grepl("Central South University, 172 Tongzipo Road, Yuelu District, Changsha 410013, Hunan Province, P.R. China", data$corresponding_address))])) ## This includes both China and USA so went back to the original paper and the very last author is in the USA so changing
data$corresponding_address[which(grepl("Central South University, 172 Tongzipo Road, Yuelu District, Changsha 410013, Hunan Province, P.R. China", data$corresponding_address))] <- "Tulane Center of Bioinformatics and Genomics, Department of Biostatistics and Data Science, Tulane University School of Public Health and Tropical Medicine, New Orleans, LA 70112, USA"
table(unique(data$corresponding_address[which(grepl("Yale University, Neurology, New Haven, United States", data$corresponding_address))])) ## This includes "United States" but want to harmonize to USA
data$corresponding_address[which(grepl("Yale University, Neurology, New Haven, United States", data$corresponding_address))] <- "Yale University, Neurology, New Haven, USA"
table(unique(data$corresponding_address[which(grepl("Cardiologie et de Pneumologie de Québec", data$corresponding_address))])) ## This includes "Québec" but want to harmonize to Canada
data$corresponding_address[which(grepl("Cardiologie et de Pneumologie de Québec", data$corresponding_address))] <- "Centre de Recherche de L’Institut Universitaire de Cardiologie et de Pneumologie de Québec, Y-3106, Pavillon Marguerite D’Youville, 2725, Canada"

## The following Ids did not have a clear correspondence address: 23119582, 261727485, 557846834; though some of these were obtained from reviewing the paper again and, for abstracts, these were the first author affiliations
data$corresponding_address[which(data$ID == 23119582)] <- "Molecular Genetics, London, UK"
data$corresponding_address[which(data$ID == 261727485)] <- "University of Bristol, Bristol, UK"
data$corresponding_address[which(data$ID == 557846834)] <- "Guangzhou Institute of Respiratory Diseases, State Key Laboratory of Respiratory Diseases, The First Affiliated Hospital of Guangzhou Medical University, Guangzhou Medical University, Guangzhou, China"

table(unique(data$corresponding_address)) ## Looking at these, they include the following countries and need to summarise this information
countries <- c("China", "Australia", "Canada", "Netherlands", "USA", "Spain", "UK", "Japan", "Germany", "Korea", "Belgium") ## These are the countries of the corresponding addresses
data$corresponding_country <- NA
for(i in countries){
  data$corresponding_country[which(grepl(i, data$corresponding_address))] <- i
}
table(unique(data$corresponding_country))

row_one <- data %>%
  group_by(ID) %>%
  filter(row_number()==1)
table(row_one$corresponding_country)
for(i in countries){
  print(paste0(i, ": ", length(row_one$corresponding_country[which(row_one$corresponding_country %in% i)]), ": ", ((length(row_one$corresponding_country[which(row_one$corresponding_country %in% i)]))/66)*100))
}
rm(countries,i)

check <- row_one[,c("ID", "corresponding_address", "corresponding_country")] 
rm(check)

#### Journal of publication
table(row_one$journal)
journals <- unique(row_one$journal)
for(i in journals){
  print(paste0(i, ": ", length(row_one$journal[which(row_one$journal %in% i)]), ": ", ((length(row_one$journal[which(row_one$journal %in% i)]))/66)*100))
}
rm(journals, IDs, i, row_one)

#### Types of outcome
table(data$outcome_group)
outcomes <- unique(data$outcome_group)
for(i in outcomes){
  print(paste0(i, ": ", length(data$outcome_group[which(data$outcome_group %in% i)]), ": ", (length(data$outcome_group[which(data$outcome_group %in% i)])/length(data$ID))*100))
}
rm(outcomes)

#### Types of exposure
table(unique(data$exposure_group))
length(data$exposure_formatted[which(data$exposure_group == "Gut microbiome")]) ## 48029
(length(data$exposure_formatted[which(data$exposure_group == "Gut microbiome")]) / length(data$ID))*100
length(data$exposure_formatted[which(data$exposure_group == "Gut microbiome pathway")]) ## 53
(length(data$exposure_formatted[which(data$exposure_group == "Gut microbiome pathway")]) / length(data$ID))*100
table(unique(data$exposure_formatted[which(grepl("unc.", data$exposure_formatted))]))

## Numbers of taxonomical units and exposure measures
table(unique(data$exposure_formatted))
species <- length(data$exposure_formatted[which(grepl("\\(Species\\)", data$exposure_formatted))]) # 618
genus <- length(data$exposure_formatted[which(grepl("\\(Genus\\)", data$exposure_formatted))]) # 16988
family <- length(data$exposure_formatted[which(grepl("\\(Family\\)", data$exposure_formatted))]) # 6007
order <- length(data$exposure_formatted[which(grepl("\\(Order\\)", data$exposure_formatted))]) # 3651
class <- length(data$exposure_formatted[which(grepl("\\(Class\\)", data$exposure_formatted))]) # 2833
phylum <- length(data$exposure_formatted[which(grepl("\\(Phylum\\)", data$exposure_formatted))]) # 1748
kingdom <- length(data$exposure_formatted[which(grepl("\\(Kingdom\\)", data$exposure_formatted))]) # 0
unknown_family <- length(data$exposure_formatted[which(grepl("Unknown Family", data$exposure_formatted))]) # 80
unknown_genus <- length(data$exposure_formatted[which(grepl("Unknown Genus", data$exposure_formatted))]) # 167
unc_species_genus <- length(data$exposure_formatted[which(grepl("\\(unc. Species in Genus\\)", data$exposure_formatted))]) # 111
unc_species_fam <- length(data$exposure_formatted[which(grepl("\\(unc. Species in Family\\)", data$exposure_formatted))]) # 10
unc_genus_fam <- length(data$exposure_formatted[which(grepl("\\(unc. Genus in Family\\)", data$exposure_formatted))]) # 668
unc_genus_order <- length(data$exposure_formatted[which(grepl("\\(unc. Genus in Order\\)", data$exposure_formatted))]) # 175
unc_genus_class <- length(data$exposure_formatted[which(grepl("\\(unc. Genus in Class\\)", data$exposure_formatted))]) # 84
unc_genus_phylum <- length(data$exposure_formatted[which(grepl("\\(unc. Genus in Phylum\\)", data$exposure_formatted))]) # 275
unc_genus_king <- length(data$exposure_formatted[which(grepl("\\(unc. Genus in Kingdom\\)", data$exposure_formatted))]) # 84
pathway <- length(data$exposure_formatted[which(grepl("\\(Pathway\\)", data$exposure_formatted))]) # 1349
functional <- length(data$exposure_formatted[which(grepl("Functional units", data$exposure_formatted))]) # 2
microbiome <- length(data$exposure_formatted[which(grepl("Gut microbiome", data$exposure_formatted))]) # 537
OTU <- length(data$exposure_formatted[which(grepl("OTU", data$exposure_formatted))]) # 5321
TESTASV <- length(data$exposure_formatted[which(grepl("TestASV", data$exposure_formatted))]) # 1951
diversity <- length(data$exposure_formatted[which(grepl("Diversity", data$exposure_formatted))]) # 249
group_species <- length(data$exposure_formatted[which(grepl("Group \\(Species in", data$exposure_formatted))]) # 1848
group_genus <- length(data$exposure_formatted[which(grepl("Group \\(Genus in", data$exposure_formatted))]) # 3062
group_family <- length(data$exposure_formatted[which(grepl("Group \\(Family in", data$exposure_formatted))]) # 255
a <- length(data$exposure_formatted[which(grepl("rs10093096", data$exposure_formatted))]) # 1
b <- length(data$exposure_formatted[which(grepl("rs11545016", data$exposure_formatted))]) # 1
c <- length(data$exposure_formatted[which(grepl("rs11867190", data$exposure_formatted))]) # 1
d <- length(data$exposure_formatted[which(grepl("rs12913063", data$exposure_formatted))]) # 1
e <- length(data$exposure_formatted[which(grepl("rs131659", data$exposure_formatted))]) # 1
f <- length(data$exposure_formatted[which(grepl("rs4396302", data$exposure_formatted))]) # 1
g <- length(data$exposure_formatted[which(grepl("rs6848139", data$exposure_formatted))]) # 1
h <- length(data$exposure_formatted[which(grepl("rs7585642", data$exposure_formatted))]) # 1
i <- length(data$exposure_formatted[which(grepl("rs7594065", data$exposure_formatted))]) # 1
sum(species, genus, family, order, class, phylum, kingdom, unknown_family, unknown_genus, unc_species_genus, unc_species_fam, unc_genus_fam, unc_genus_order, unc_genus_class, unc_genus_phylum, unc_genus_king, pathway, functional, microbiome, OTU, TESTASV, diversity, group_species, group_genus, group_family,a,b,c,d,e,f,g,h,i)

## Species with groups in species
species_all <- sum(species, group_species, unc_species_genus, unc_species_fam)

## Genus with groups in genus
genus_all <- sum(genus, group_genus, unknown_genus, unc_genus_fam, unc_genus_class, unc_genus_order, unc_genus_phylum, unc_genus_king)

## Family with groups in family
family_all <- sum(family, group_family, unknown_family)

## Unclear (i.e., the "functional units" and "gut microbiome")
unclear <- sum(functional, microbiome)

## Unclassified
unclassified <- sum(unc_species_genus, unc_species_fam, unc_genus_fam, unc_genus_order, unc_genus_class, unc_genus_phylum, unc_genus_king)
unclassified

## Unknown
unknown <- sum(unknown_family, unknown_genus)
unknown

## SNPs
snps <- sum(a,b,c,d,e,f,g,h,i)
rm(species, genus, unknown_genus, unc_genus_fam, unc_genus_class, unc_genus_order, unc_species_genus, unc_species_fam, unc_genus_phylum, unc_genus_king, family, kingdom, a,b,c,d,e,f,g,h,i,group_species,group_genus,group_family, unknown_family, functional, microbiome, unknown, unclassified)

## Count them all
sum(species_all, genus_all, family_all, order, class, phylum, pathway, unclear, OTU, TESTASV, diversity, snps)
exposures <- list(species_all = species_all, genus_all = genus_all, family_all = family_all, order = order, class = class, phylum = phylum, pathway = pathway, unclear = unclear, OTU = OTU, TESTASV = TESTASV, diversity = diversity, snps = snps)
for(i in names(exposures)){
  print(paste0(i, ": ", exposures[[i]], ": ", (exposures[[i]]/length(data$ID))*100))
}
rm(i, exposures)
rm(species_all, genus_all, family_all, order, class, phylum, pathway, unclear, OTU, TESTASV, diversity, snps)

#### Types of analysis
table(unique(data$study_design))
length(data$study_design[which(data$study_design == "One-sample")]) ## 36
length(data$study_design[which(data$study_design == "One-sample")])/length(data$ID)*100
length(data$study_design[which(data$study_design == "Two-sample")]) ## 48024
length(data$study_design[which(data$study_design == "Two-sample")])/length(data$ID)*100
table(unique(data$ID[which(data$study_design == '"One-sample" but list two-sample methods')])) ## Need to check this is the case before reporting it!
table(unique(data$first_author[which(data$study_design == '"One-sample" but list two-sample methods')])) ## Need to check this is the case before reporting it!
table(unique(data$year[which(data$study_design == '"One-sample" but list two-sample methods')])) ## Need to check this is the case before reporting it!

#### Studies used for the exposure GWAS
table(unique(data$study1))
exp_studies <- unique(data$study1)

## Percentage of estimates that used different GWASs
for(i in exp_studies){
  print(paste0(i, ": ", length(data$study1[which(data$study1 %in% i)]), ": ", (length(data$study1[which(data$study1 %in% i)])/length(data$ID))*100))
}

## Number of records that presented their own GWAS analyses
table(unique(data$study1[which(grepl("Current study", data$study1))]))
current <- data[which(grepl("Current study", data$study1)), c("ID","exposure_formatted","outcome","study1","study2","exposure_study_n","replication")]
current_studies <- unique(data$study1[which(grepl("Current study", data$study1))])
for(i in current_studies){
  print(paste0(i, " - MR (N):", unique(data$exposure_study_n[which(data$study1 %in% i)]), " - GWAS (N):", unique(data$exposure_og_n[which(data$study1 %in% i)])))
}

## More information about the records that conducted the mGWAS in the "current study" and which records used that same data for the outcome information (i.e., also labelled "Current study" in the extraction)
current <- data[which(grepl("Current study", data$study1)),c("ID","first_author","year","exposure_formatted","outcome","study1","exposure_study_n","study2","outcome_og_gwas_case","outcome_og_gwas_control","outcome_study_case","outcome_study_control","outcome_n_diff","replication")] 
dim(current) ## 14551 estimates
length(unique(current$ID[which(grepl("Current study", current$study1))])) ## 14 studies
table(unique(current$ID)) ## 122541860 1519899723 1828507192 2044689642 2136589463 2179267930 2300517094 2662850683 3132308026 3278202937  560890194  568199176  718621312  914635920 
rm(current_studies)

# Checking 122541860 = NO - this used different outcome GWASs 
table(unique(current$study1[which(current$ID == 122541860)]))
table(unique(current$study2[which(current$ID == 122541860)]))
table(unique(current$exposure_study_n[which(current$ID == 122541860)]))
table(unique(current$outcome_study_case[which(current$ID == 122541860)]))
table(unique(current$outcome_study_control[which(current$ID == 122541860)]))

# Checking 1519899723 = YES - the GWAS within which the outcome was sourced was also a substantial contributor to the GWAS meta-analysis for the exposure (they performed the GWAS of the exposure then meta-analysed with other data)
table(unique(current$study1[which(current$ID == 1519899723)]))
table(unique(current$study2[which(current$ID == 1519899723)]))
table(unique(current$exposure_study_n[which(current$ID == 1519899723)]))
table(unique(current$outcome_study_case[which(current$ID == 1519899723)]))
table(unique(current$outcome_study_control[which(current$ID == 1519899723)]))

# Checking 1828507192 = NO - this used different outcome GWASs 
table(unique(current$study1[which(current$ID == 1828507192)]))
table(unique(current$study2[which(current$ID == 1828507192)]))
table(unique(current$exposure_study_n[which(current$ID == 1828507192)]))
table(unique(current$outcome_study_case[which(current$ID == 1828507192)]))
table(unique(current$outcome_study_control[which(current$ID == 1828507192)]))

# Checking 2044689642 = NO - this used different outcome GWASs 
table(unique(current$study1[which(current$ID == 2044689642)]))
table(unique(current$study2[which(current$ID == 2044689642)]))
table(unique(current$exposure_study_n[which(current$ID == 2044689642)]))
table(unique(current$outcome_study_case[which(current$ID == 2044689642)]))
table(unique(current$outcome_study_control[which(current$ID == 2044689642)]))

# Checking 2136589463 = YES - this one did use the same data for the exposure and outcome data
table(unique(current$study1[which(current$ID == 2136589463)]))
table(unique(current$study2[which(current$ID == 2136589463)]))
table(unique(current$exposure_study_n[which(current$ID == 2136589463)]))
table(unique(current$outcome_study_case[which(current$ID == 2136589463)]))
table(unique(current$outcome_study_control[which(current$ID == 2136589463)]))

# Checking 2179267930 = YES - this one did use the same data for the exposure and outcome in the one-sample and two-sample MR analyses across discovery and replication (i.e., they did a gwas of the exposure and outcome in two subsamples of the population (one discovery, one replication) and then conducted MR analyses using the same data) BUT they also did a replication with a different outcome (although only 4 exposure-outcome pairs were present)
table(unique(current$study1[which(current$ID == 2179267930)]))
table(unique(current$study2[which(current$ID == 2179267930)]))
table(unique(current$exposure_study_n[which(current$ID == 2179267930)]))
table(unique(current$outcome_study_case[which(current$ID == 2179267930)]))
table(unique(current$outcome_study_control[which(current$ID == 2179267930)]))

# Checking 2300517094 = YES - this one did use the same data for the exposure and outcome data
table(unique(current$study1[which(current$ID == 2300517094)]))
table(unique(current$study2[which(current$ID == 2300517094)]))
table(unique(current$exposure_study_n[which(current$ID == 2300517094)]))
table(unique(current$outcome_study_case[which(current$ID == 2300517094)]))
table(unique(current$outcome_study_control[which(current$ID == 2300517094)]))

# Checking 2662850683 = NO - though unclear what the exact GWASs were considering the GWAS Ns weren't presented
table(unique(current$study1[which(current$ID == 2662850683)]))
table(unique(current$study2[which(current$ID == 2662850683)]))
table(unique(current$exposure_study_n[which(current$ID == 2662850683)]))
table(unique(current$outcome_study_case[which(current$ID == 2662850683)]))
table(unique(current$outcome_study_control[which(current$ID == 2662850683)]))

# Checking 3132308026 = YES - this one did use the same data for the exposure and outcome data 
table(unique(current$study1[which(current$ID == 3132308026)]))
table(unique(current$study2[which(current$ID == 3132308026)]))
table(unique(current$exposure_study_n[which(current$ID == 3132308026)]))
table(unique(current$outcome_study_case[which(current$ID == 3132308026)]))
table(unique(current$outcome_study_control[which(current$ID == 3132308026)]))

# Checking 3278202937 = NO - though unclear what the exact GWASs were
table(unique(current$study1[which(current$ID == 3278202937)]))
table(unique(current$study2[which(current$ID == 3278202937)]))
table(unique(current$exposure_study_n[which(current$ID == 3278202937)]))
table(unique(current$outcome_study_case[which(current$ID == 3278202937)]))
table(unique(current$outcome_study_control[which(current$ID == 3278202937)]))

# Checking 560890194 = NO - this one used a different outcome dataset but it wasn't clear what the outcome numbers were
table(unique(current$study1[which(current$ID == 560890194)]))
table(unique(current$study2[which(current$ID == 560890194)]))
table(unique(current$exposure_study_n[which(current$ID == 560890194)]))
table(unique(current$outcome_study_case[which(current$ID == 560890194)]))
table(unique(current$outcome_study_control[which(current$ID == 560890194)]))

# Checking 568199176 = YES - this one did use the same data for the exposure and outcome data 
table(unique(current$study1[which(current$ID == 568199176)]))
table(unique(current$study2[which(current$ID == 568199176)]))
table(unique(current$exposure_study_n[which(current$ID == 568199176)]))
table(unique(current$outcome_study_case[which(current$ID == 568199176)]))
table(unique(current$outcome_study_control[which(current$ID == 568199176)]))

# Checking 718621312 = N0 - this one used different outcome datasets
table(unique(current$study1[which(current$ID == 718621312)]))
table(unique(current$study2[which(current$ID == 718621312)]))
table(unique(current$exposure_study_n[which(current$ID == 718621312)]))
table(unique(current$outcome_study_case[which(current$ID == 718621312)]))
table(unique(current$outcome_study_control[which(current$ID == 718621312)]))

# Checking 914635920 = NO - this one used a different outcome dataset
table(unique(current$study1[which(current$ID == 914635920)]))
table(unique(current$study2[which(current$ID == 914635920)]))
table(unique(current$exposure_study_n[which(current$ID == 914635920)]))
table(unique(current$outcome_study_case[which(current$ID == 914635920)]))
table(unique(current$outcome_study_control[which(current$ID == 914635920)]))

# Make a numerical column that I can use to compare the exposure and outcome data (NAs introduced means that the data wasn't clear)
current$exposure_study_n <- as.numeric(current$exposure_study_n)
current$outcome_study_case <- as.numeric(current$outcome_study_case)
current$same_data <- current$exposure_study_n - current$outcome_study_case
current_studies <- unique(current$ID)
for(i in current_studies){
  print(paste0(i, ": ", unique(current$exposure_study_n[which(current$ID %in% i)]), ": ",unique(current$outcome_study_case[which(current$ID %in% i)]), ": ", unique(current$same_data[which(current$ID %in% i)])))
}
rm(current, current_studies, i)

## Number of records using multiple GWASs within a record
N_study1 <- data %>%
  group_by(ID) %>%
  mutate(E = length(unique(study1)))
N_study1
summary(N_study1$E)
table(unique(N_study1$E))
IDs <- unique(N_study1$ID)
for(i in IDs){
  print(paste0(i, ": ", unique(N_study1$E[which(N_study1$ID == i)]), ": ", unique(N_study1$study1[which(N_study1$ID == i)])))
}
a <- length(unique(N_study1$ID[which(N_study1$E == 1)]))
b <- length(unique(N_study1$ID[which(N_study1$E == 2)])) 
c <- length(unique(N_study1$ID[which(N_study1$E == 3)]))
d <- length(unique(N_study1$ID[which(N_study1$E == 6)]))
e <- length(unique(N_study1$ID[which(N_study1$study1 %in% c("Mixture of mGWASs", "Unclear mixture of mGWASs", "Not reported") & N_study1$E==1)]))
a <- a-e
a+b+c+d+e
total_multiple_studies <- a+(b*2)+(c*3)+(d*6)+e
for(i in c(a,b,c,d,e)){
  print(paste0(i, ": ", (i/66)*100))
}
f <- length(unique(N_study1$ID[which(N_study1$study1 %in% c("Mixture of mGWASs", "Unclear mixture of mGWASs") & N_study1$E==1)]))
b+c+d+f
rm(f)

## Percentage of records using different GWASs 
for(i in exp_studies){
  print(paste0(i, ": ", length(unique(data$ID[which(data$study1 %in% i)])), ": ", (length(unique(data$ID[which(data$study1 %in% i)]))/total_multiple_studies)*100)) ## totals 78 so there must be some records that use multiple mGWASs
}
rm(a,b,c,d,e,total_multiple_studies, exp_studies, i, IDs)

## More information about the studies that used multiple mGWASs
multiple_study1 <- unique(N_study1$ID[which(N_study1$E >1)])
mixture_study1 <- unique(N_study1$ID[which(N_study1$study1 == "Mixture of mGWASs")])
unclear_study1 <- unique(N_study1$ID[which(N_study1$study1 == "Unclear mixture of mGWASs")])
multiple_mixture_study1 <- unique(c(multiple_study1, mixture_study1, unclear_study1))
rm(multiple_study1, mixture_study1, unclear_study1, N_study1)
multiple_mixture_study1 ## 4290593439 3032725182 568199176 1308760143 4251029218 2383481240 2934159401 3836325935 Other sources 950921645 2238214110 3113131015
for(i in multiple_mixture_study1){
  print(paste0(i, " by ", unique(data$first_author[which(data$ID == i)]), ": ", unique(data$study1[which(data$ID == i)])))
}

## Now going one by one (with reference to paper) to understand what they did 
# Firstly, the ones that used over 2 different studies
table(unique(data$study1[which(data$ID == 4290593439)])) ## includes an "unclear mixture of mGWASs"
table(unique(data$first_author[which(data$ID == 4290593439)])) ## by Groot et al. used five mGWASs and then did an analysis combining four of these
length(data$ID[which(data$ID == 4290593439)]) ## 123 
length(data$ID[which(data$ID == 4290593439) & data$study1 == "Unclear mixture of mGWASs"]) ## 28 
table(unique(data$study1[which(data$ID == 1308760143)])) ## includes a mixture
table(unique(data$first_author[which(data$ID == 1308760143)])) ## by Yang et al. used a mixture of three different mGWAS
length(data$ID[which(data$ID == 1308760143)]) ## 22
length(data$ID[which(data$ID == 1308760143 & data$study1 == "Mixture of mGWASs")]) ## 9
table(unique(data$study1[which(data$ID == 3113131015)])) ## only a mixture but same as previous paper
table(unique(data$first_author[which(data$ID == 3113131015)])) ## by Zhou et al - used five (like Yang et al.) different mGWASs
length(data$ID[which(data$ID == 3113131015)]) ## 23
length(data$ID[which(data$ID == 3113131015 & data$snp_number >1)]) ## 8
table(unique(data$study1[which(data$ID == "Other sources")])) ## includes only a mixture
table(unique(data$first_author[which(data$ID == "Other sources")])) ## by Gagnon et al - used three different mGWASs (two for MTs, one for pathway)
length(data$ID[which(data$ID == "Other sources")]) ## 1027

# Then the ones that seem to either have used a "Mixture" 
table(unique(data$study1[which(data$ID == 3836325935)])) ## only includes estimates with a mixture of mGWASs
table(unique(data$first_author[which(data$ID == 3836325935)])) ## by Inamo et al - used two different mGWASs
length(data$ID[which(data$ID == 3836325935)]) ## 2
table(unique(data$study1[which(data$ID == 950921645)])) ## only includes estimates with a mixture of mGWASs
table(unique(data$first_author[which(data$ID == 950921645)])) ## by Lee et al - used two different mGWASs
length(data$ID[which(data$ID == 950921645)]) ## 1 
table(unique(data$study1[which(data$ID == 2238214110)])) ## by Qi et al - used two different mGWASs
table(unique(data$first_author[which(data$ID == 2238214110)])) ## by Qi et al - used two different mGWASs
length(data$ID[which(data$ID == 2238214110)]) ## 515

# Then the ones that seem to either have used multiple mGWASs but published the estimates separately
table(unique(data$study1[which(data$ID == 3032725182)])) ## publishes estimates separately for two mGWASs
table(unique(data$first_author[which(data$ID == 3032725182)])) ## by Ni et al - used two different mGWASs
length(data$ID[which(data$ID == 3032725182)]) ## 12
table(unique(data$study1[which(data$ID == 4251029218)])) ## publishes estimates separately for two mGWASs
table(unique(data$first_author[which(data$ID == 4251029218)])) ## by Ni et al - used two different mGWASs
length(data$ID[which(data$ID == 4251029218)]) ## 7
table(unique(data$study1[which(data$ID == 568199176)])) ## publishes estimates separately for two mGWASs
table(unique(data$first_author[which(data$ID == 568199176)])) ## by Lv et al - used two different mGWASs (including one in which they conducted)
length(data$ID[which(data$ID == 568199176)]) ## 2 
table(unique(data$study1[which(data$ID == 2383481240)])) ## publishes estimates separately for two mGWASs
table(unique(data$first_author[which(data$ID == 2383481240)])) ## by Liu et al - used two different mGWASs
length(data$ID[which(data$ID == 2383481240)]) ## 239 
table(unique(data$study1[which(data$ID == 2934159401)])) ## publishes estimates separately for two mGWASs
table(unique(data$first_author[which(data$ID == 2934159401)])) ## by Liu et al - used two different mGWASs
length(data$ID[which(data$ID == 2934159401)]) ## 445 
rm(i, multiple_mixture_study1)

#### Meta-analysis within records are in the "data_synthesis.R" document

#### Meta-analysis between records 
## Everything relevant to this is in the "data_synthesis.R" document but have got summary here that should match up with final numbers of estimates meta-analysed
colnames(data)
rep_data <- data %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()

## Firstly, need the number of estimates had the same exposure-outcome pairs
rep_data_dups <- rep_data[which(rep_data$matches >1),] ## 5768 rows of analyses that were replicated
rep_data_dups_complete_phenos <- rep_data_dups[(which((rep_data_dups$exposure_formatted != "NA" & rep_data_dups$outcome != "NA"))),] ## No exposures and outcomes listed as "NA"
length(unique(rep_data_dups_complete_phenos$exposure_formatted))
length(unique(rep_data_dups_complete_phenos$outcome))

## Of those, number of estimates that had clear units (i.e., they could be compared across records) and were the same across records
# Exposure units
table(unique(rep_data_dups_complete_phenos$e_unit))
length(unique(rep_data_dups_complete_phenos$e_unit))
length(which(is.na(rep_data_dups_complete_phenos$e_unit))) ## 5475 
(length(which(is.na(rep_data_dups_complete_phenos$e_unit)))/length(rep_data_dups_complete_phenos$e_unit))*100 ## 94.92% 

# Outcome units
table(unique(rep_data_dups_complete_phenos$o_unit))
length(unique(rep_data_dups_complete_phenos$o_unit))
length(which(is.na(rep_data_dups_complete_phenos$o_unit))) ## 558
(length(which(is.na(rep_data_dups_complete_phenos$o_unit)))/length(rep_data_dups_complete_phenos$o_unit))*100 ## 9.67%

# Units for both combined
length(which((is.na(rep_data_dups_complete_phenos$o_unit) & is.na(rep_data_dups_complete_phenos$e_unit)))) ## 359 estimates have neither clear exposure or outcome units
(length(which((is.na(rep_data_dups_complete_phenos$o_unit) & is.na(rep_data_dups_complete_phenos$e_unit))))/length(rep_data_dups_complete_phenos$ID))*100 ## 6.36%
length(which((is.na(rep_data_dups_complete_phenos$o_unit) | is.na(rep_data_dups_complete_phenos$e_unit)))) ## 5666 estimates have neither clear exposure or outcome units
(length(which((is.na(rep_data_dups_complete_phenos$o_unit) | is.na(rep_data_dups_complete_phenos$e_unit))))/length(rep_data_dups_complete_phenos$ID))*100 ## 98.23%

# Create a dataset with only those estimates that can be meta-analysed
rep_data_dups_complete_units <- rep_data_dups_complete_phenos[-which((is.na(rep_data_dups_complete_phenos$o_unit) | is.na(rep_data_dups_complete_phenos$e_unit))),]

## Of those, number of estimates that had clear exposure and outcome data (i.e., could see whether they were using different outcome data for meta-analysis)
# Exposure data
table(unique(rep_data_dups_complete_units$study1)) 
length(unique(rep_data_dups_complete_units$study1)) ## 5

# Outcome data
table(unique(rep_data_dups_complete_units$study2)) 
length(unique(rep_data_dups_complete_units$study2)) ## 14

## Of those, number of estimates had explicitly different outcome data so could be meta-analysed
# Create a dataset that selects those that have different outcome studies across estimates
rep_across_studies <- rep_data_dups_complete_units %>%
  group_by(exp_out) %>%
  filter(n_distinct(study2) > 1,
         n_distinct(ID) > 1) %>%
  filter(n() > 1) %>%
  ungroup() ## 9 where they seem to replicate over studies (i.e., same exposure, same outcome, different outcome study, different ID)
rep_across_studies_check <- rep_across_studies[,c("ID","exposure_formatted","outcome","study1","study2","e_unit","o_unit")]
(9/5768)*100
rm(rep_data, rep_data_dups_complete_phenos, rep_data_dups_complete_units, rep_across_studies, rep_across_studies_check)

## Then meta-analyses results are in the appropriate folders from the "data_synthesis.R" script

#### Quality assessment
quality <- read.xlsx("./Quality Assessment/QA_results_complete_publication_300125.xlsx",sheet = "RoB FINAL", colNames = TRUE)
quality <- quality[-c(67,68,69),] 
colnames(quality)
quality$moderate <- rowSums(quality[,c(6:24)] == 2)
quality$high <- rowSums(quality[,c(6:24)] == 1)

## Summary statistics for low quality
summary(quality$Count.if.any.high.risk.of.bias)
sd(quality$Count.if.any.high.risk.of.bias)

## Summary statistics for moderate quality
summary(quality$moderate)
sd(quality$moderate)

## Summary statistics for high quality
summary(quality$high)
sd(quality$high)

## Number of items scoring as low quality most often
low_quality_list <- list()
for(i in colnames(quality)){
  print(paste0(colnames(quality[which(colnames(quality) %in% c(i))]), ": ", length(quality[,i][which(quality[,i] == 3)])))
  a <- length(quality[,i][which(quality[,i] == 3)])
  low_quality_list[[i]] <- assign(i,a)
}
all_low_quality <- as.data.frame(do.call(rbind, low_quality_list))
colnames(all_low_quality) <- c("low_quality_score")
all_low_quality
summary(all_low_quality[,1]) ## maximum is 51 so let's see which are above 25
for(i in colnames(quality)){
  print(paste0(colnames(quality[which((colnames(quality) %in% c(i)) & (length(quality[,i][which(quality[,i] == 3)]) >= 25))]), ": ", length(quality[,i][which((quality[,i] == 3) & (length(quality[,i][which(quality[,i] == 3)]) >= 25))])))
}

# Those that were over 25 are Replication, Reporting, Sensitivity analyses, IV.selection (1), bias due to selection of participants, data availability and exposure info, then iv selection (2)
colnames(quality)
low_quality <- c("Replication","Reporting","Sensitivity.analyses","IV.selection.(1)","Bias.due.to.selection.of.participants","Data.availability.and.reproducibility","Exposure.information","IV.selection.(2)")
for(i in low_quality){
  print(paste0(i, ": ", length(quality[,i][which(quality[,i] == 3)]), ": ", (length(quality[,i][which(quality[,i] == 3)])/length(quality$ID))*100))
}
rm(a,i, low_quality,low_quality_list)

## Number of items scoring as moderate quality most often
moderate_quality_list <- list()
for(i in colnames(quality)){
  print(paste0(colnames(quality[which(colnames(quality) %in% c(i))]), ": ", length(quality[,i][which(quality[,i] == 2)])))
  b <- length(quality[,i][which(quality[,i] == 2)])
  moderate_quality_list[[i]] <- assign(i,b)
}
all_moderate_quality <- as.data.frame(do.call(rbind, moderate_quality_list))
colnames(all_moderate_quality) <- c("moderate_quality_score")
all_moderate_quality
summary(all_moderate_quality[,1]) ## maximum is 60 so let's see which are above 30
for(i in colnames(quality)){
  print(paste0(colnames(quality[which((colnames(quality) %in% c(i)) & (length(quality[,i][which(quality[,i] == 2)]) >= 30))]), ": ", length(quality[,i][which((quality[,i] == 2) & (length(quality[,i][which(quality[,i] == 2)]) >= 30))])))
}

# Ones that are over 30 are harmonization, SNP information, other confounding or collider bias, genetic confounding, direct effects, overfitting, reverse causation, main analysis, outcome information, weak instrument bias, IV selection (2), data availability and reproducibility, exposure information, participant selection
colnames(quality)
moderate_quality <- c("Harmonization./.preparation","SNP.information","Other.Confounding./.Collider.bias","Genetic.confounding","Additional.direct.effects.between.IV.and.outcome.(exclusion.restriction.assumption)","Overestimation.via.sample.overlap.or.overfitting","Reverse.causation","Main.analyses","Outcome.information","Weak.instrument.bias","IV.selection.(2)","Data.availability.and.reproducibility","Exposure.information","Bias.due.to.selection.of.participants")
for(i in moderate_quality){
  print(paste0(i, ": ", length(quality[,i][which(quality[,i] == 2)]), ": ", (length(quality[,i][which(quality[,i] == 2)])/length(quality$ID))*100))
}
rm(b,i,moderate_quality, moderate_quality_list)

## Number of items scoring as high quality most often
high_quality_list <- list()
for(i in colnames(quality)){
  print(paste0(colnames(quality[which(colnames(quality) %in% c(i))]), ": ", length(quality[,i][which(quality[,i] == 1)])))
  c <- length(quality[,i][which(quality[,i] == 1)])
  high_quality_list[[i]] <- assign(i,c)
}
all_high_quality <- as.data.frame(do.call(rbind, high_quality_list))
colnames(all_high_quality) <- c("high_quality_score")
all_high_quality
summary(all_high_quality[,1]) ## maximum is 25 so let's see which are above 12
for(i in colnames(quality)){
  print(paste0(colnames(quality[which((colnames(quality) %in% c(i)) & (length(quality[,i][which(quality[,i] == 1)]) >= 12))]), ": ", length(quality[,i][which((quality[,i] == 1) & (length(quality[,i][which(quality[,i] == 1)]) >= 12))])))
}

# Ones that are over 12 were weak instrument bias, data and IV selection (1), exclusion restriction and reverse causation
colnames(quality)
high_quality <- c("Weak.instrument.bias","Data","IV.selection.(1)","Additional.direct.effects.between.IV.and.outcome.(exclusion.restriction.assumption)","Reverse.causation")
for(i in high_quality){
  print(paste0(i, ": ", length(quality[,i][which(quality[,i] == 1)]), ": ", (length(quality[,i][which(quality[,i] == 1)])/length(quality$ID))*100))
}
rm(c,i, high_quality, high_quality_list)

## Records that had the most low-quality
rm("Additional.direct.effects.between.IV.and.outcome.(exclusion.restriction.assumption)","Author","Bias.due.to.selection.of.participants","Count.if.any.high.risk.of.bias","Data","Data.availability.and.reproducibility","DOI","Exposure.information","Genetic.confounding","Harmonization./.preparation","high","IV.selection.(1)","IV.selection.(2)","Main.analyses","moderate","ID","Other.Confounding./.Collider.bias","Outcome.information","Overestimation.via.sample.overlap.or.overfitting","Replication","Reporting","Reverse.causation","Sensitivity.analyses","SNP.information","Title","Weak.instrument.bias","Year")
# those with 9/19
length(quality$ID)
summary(quality$Count.if.any.high.risk.of.bias)
length(quality$ID[which(quality$Count.if.any.high.risk.of.bias >=9)])
quality$ID[which(quality$Count.if.any.high.risk.of.bias >=9)]
paste0(quality$ID[which(quality$Count.if.any.high.risk.of.bias >=9)], ": ", quality$Author[which(quality$Count.if.any.high.risk.of.bias >=9)], ": ", quality$Count.if.any.high.risk.of.bias[which(quality$Count.if.any.high.risk.of.bias >=9)], ": ", ((quality$Count.if.any.high.risk.of.bias[which(quality$Count.if.any.high.risk.of.bias >=9)])/19)*100)

# those with 8/19
length(quality$ID)
summary(quality$Count.if.any.high.risk.of.bias)
length(quality$ID[which(quality$Count.if.any.high.risk.of.bias >=8)])
quality$ID[which(quality$Count.if.any.high.risk.of.bias >=8)]
paste0(quality$ID[which(quality$Count.if.any.high.risk.of.bias >=8)], ": ", quality$Author[which(quality$Count.if.any.high.risk.of.bias >=8)], ": ", quality$Title[which(quality$Count.if.any.high.risk.of.bias >=8)], ": ", quality$Count.if.any.high.risk.of.bias[which(quality$Count.if.any.high.risk.of.bias >=8)], ": ", ((quality$Count.if.any.high.risk.of.bias[which(quality$Count.if.any.high.risk.of.bias >=8)])/19)*100)

## Records that didn't have any high quality
paste0(quality$ID[which(quality$high == 0)], ": ", quality$Author[which(quality$high == 0)])
paste0(quality$Title[which(quality$high == 0)], ": ", quality$Author[which(quality$high == 0)])
rm(all_high_quality, all_low_quality, all_moderate_quality)

## P-value thresholds for studies
# Summarise the number of records that use a lenient p-value threshold
table(quality$`IV.selection.(1)`[which(quality$`IV.selection.(1)` ==3)])

## Same analyses undertaken
same_analysis <- rep_data_dups %>%
  filter(e_s1_o_s2 %in% e_s1_o_s2[duplicated(e_s1_o_s2)]) ## 4085 analyses that apparently have been done in the same datasets
same_analysis$bin_rep <- NULL

#############
## FIGURES ##
#############
## Figure 3 (visualisation of outcome types)
data_summary <- data.frame(table(data$outcome_group, useNA = "ifany"))
head(data_summary)
colnames(data_summary) <- c("outcome_group", "count")

# Replace NA with "Unknown Outcome"
data_summary$outcome_group <- as.character(data_summary$outcome_group)
data_summary$outcome_group[is.na(data_summary$outcome_group)] <- "Unknown Outcome"

# Generate plot
color_palette <- colorRampPalette(brewer.pal(12, "Paired"))(length(unique(data_summary$outcome_group)))
ggplot(data_summary, aes(x = reorder(outcome_group, -count), y = count, fill = outcome_group)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Outcome Group", y = "Total Count") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5, color = "black") +
  scale_fill_manual(values = color_palette) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
ggsave("./Data analysis/Figure3_Outcome_Types.png", height = 7, width = 12)
rm(data_summary)

## Figure 4 (visualisation of types of exposures)
# Lets make a split by microbiome taxa group
data_summary <- data.frame(table(data$exposure_formatted), taxa_group = NA)
colnames(data_summary) <- c("exposure_formatted", "count", "taxa_group")

# We can start with easy labeling - exact matches for taxa classifcations
data_summary <- data_summary %>%
  mutate(taxa_group = ifelse(grepl("\\(Genus\\)$", exposure_formatted), "Genus", taxa_group),
         taxa_group = ifelse(grepl("\\(Family\\)$", exposure_formatted), "Family", taxa_group),
         taxa_group = ifelse(grepl("\\(Order\\)$", exposure_formatted), "Order", taxa_group),
         taxa_group = ifelse(grepl("\\(Phylum\\)$", exposure_formatted), "Phylum", taxa_group),
         taxa_group = ifelse(grepl("\\(Class\\)$", exposure_formatted), "Class", taxa_group),
         taxa_group = ifelse(grepl("(Pathway)", exposure_formatted), "Pathway", taxa_group),
         taxa_group = ifelse(grepl("(Species)", exposure_formatted), "Species", taxa_group),
         taxa_group = ifelse(grepl("(Diversity)", exposure_formatted), "Diversity", taxa_group),
         taxa_group = ifelse(grepl("ASV", exposure_formatted), "Amplicon Sequence Variants (ASV)", taxa_group),
         taxa_group = ifelse(grepl("OTU", exposure_formatted), "Operational Taxonomic Units (OTU)", taxa_group),
         taxa_group = ifelse(exposure_formatted == "Unknown Family", "Family", taxa_group),
         taxa_group = ifelse(exposure_formatted == "Unknown Genus", "Genus", taxa_group))

# Now we can look at the more complex strings, we will take the classification level before the word "in"
data_summary <- data_summary %>%
  # Only modify rows where "in" is present and taxa_group is NA
  mutate(
    taxa_group = ifelse(
      str_detect(exposure_formatted, " in ") & is.na(taxa_group),
      # Extract the taxonomic group before "in"
      str_extract(exposure_formatted, "\\b\\w+\\b(?= in)"),
      # Otherwise, keep the original taxa_group
      taxa_group
    )
  )

# Functional units or Gut Microbiome measurements are unclear
data_summary <- data_summary %>%
  mutate(
    taxa_group = ifelse(
      grepl("Functional units", exposure_formatted),
      "Unclear",
      taxa_group
    ),
    taxa_group = ifelse(
      grepl("Gut microbiome", exposure_formatted),
      "Unclear",
      taxa_group
    )
  )

# Lets finally annotated the rs level exposures
data_summary <- data_summary %>%
  mutate(
    taxa_group = ifelse(
      is.na(taxa_group),
      "Genetic variants associated with\n gut microbial traits",
      taxa_group
    )
  )

# Now we want to get the counts of these new categories
count4plot <- data_summary %>%
  group_by(taxa_group) %>%
  summarise(count = sum(count))

# Make the plot
color_palette <- colorRampPalette(brewer.pal(12, "Paired"))(length(unique(count4plot$taxa_group)))
ggplot(count4plot, aes(x = reorder(taxa_group, -count), y = count, fill = taxa_group)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Exposure Group", y = "Total Count") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5, color = "black") +
  scale_fill_manual(values = color_palette) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
ggsave("./Data analysis/Figure4_Exposure_Types.png", height = 7, width = 12)
rm(data_summary)

## Figure 6
qa_summary1 <- quality %>%
  mutate(paper = paste0(Author, "_", Year),  # Concatenate Author and Year
         lowROB = rowSums(select(., 6:24) == 1),  # Count occurrences of 1 in columns 6-24
         mediumROB = rowSums(select(., 6:24) == 2),  # Count occurrences of 2 in columns 6-24
         highROB = rowSums(select(., 6:24) == 3)  # Count occurrences of 3 in columns 6-24
  )  %>%
  select(paper, ID, lowROB, mediumROB, highROB) 

# Some repeats with diff publication so add DOI in
qa_summary1$paper <- ifelse(qa_summary1$paper %in% qa_summary1$paper[duplicated(qa_summary1$paper)], paste0(qa_summary1$paper, "_", qa_summary1$ID) ,qa_summary1$paper)

#Make long data format 
qa_long <- qa_summary1 %>%
  pivot_longer(cols = ends_with("ROB"),  # Select columns starting with "ROB_count"
               names_to = "ROB_type",  # Create a new column for ROB_type (low, medium, high)
               values_to = "count")

# Make ROB_type a factor from low to high
qa_long$ROB_type <- factor(qa_long$ROB_type, levels = c("highROB", "mediumROB", "lowROB"))

# Make the plot
ggplot(qa_long, aes(x = paper, y = count, fill = ROB_type)) +
  geom_bar(stat = "identity", show.legend = TRUE, color = "black", size = 0.25) +  
  labs(x = "Study", y = "Quality Assessment Count",
       fill = "Quality Assessment") +
  scale_fill_brewer(palette = "YlOrRd", direction = -1, labels = c("lowROB" = "High Quality", "mediumROB" = "Medium Quality", "highROB" = "Low Quality")) +  
  coord_flip() +  
  theme_minimal(base_size = 15) +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12),  
    axis.title = element_text(size = 14),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic"),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  
    plot.margin = margin(1, 1, 1, 1, "cm")  
  )
ggsave("./Data analysis/Quality_Assessment_Within_Studies.png", height = 11, width = 12)

## Figure 5
qa_summary2 <- data.frame(ROB_metric = colnames(quality)[6:24],
                          lowROB = as.vector(colSums(quality[, 6:24] == 1)),
                          mediumROB = as.vector(colSums(quality[, 6:24] == 2)),
                          highROB = as.vector(colSums(quality[, 6:24] == 3)))

# Change ROB metric for figure
table(unique(qa_summary2$ROB_metric))
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Additional.direct.effects.between.IV.and.outcome.(exclusion.restriction.assumption)")] <- "Exclusion restriction assumption"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Bias.due.to.selection.of.participants")] <- "Participant selection"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Data.availability.and.reproducibility")] <- "Data availability / reproducibility"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Data")] <- "Data clarity"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Exposure.information")] <- "Exposure information"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Genetic confounding")] <- "Genetic confounding"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Harmonization./.preparation")] <- "Harmonization / preparation"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "IV.selection.(1)")] <- "IV selection (p-value threshold)"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "IV.selection.(2)")] <- "Other IV selection criteria"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Main.analyses")] <- "Main analyses"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Other.Confounding./.Collider.bias")] <- "Collider bias"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Outcome.information")] <- "Outcome information"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Overestimation.via.sample.overlap.or.overfitting")] <- "Sample overlap / overfitting"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Reverse.causation")] <- "Reverse causation"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Sensitivity.analyses")] <- "Sensitivity analyses"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "SNP.information")] <- "SNP information"
qa_summary2$ROB_metric[which(qa_summary2$ROB_metric == "Weak.instrument.bias")] <- "Weak instrument bias"

# Make long data format 
qa_long2 <- qa_summary2 %>%
  pivot_longer(cols = ends_with("ROB"),  # Select columns starting with "ROB_count"
               names_to = "ROB_type",  # Create a new column for ROB_type (low, medium, high)
               values_to = "count")

# Make ROB_type a factor from low to high
qa_long2$ROB_type <- factor(qa_long2$ROB_type, levels = c("highROB", "mediumROB", "lowROB"))

# Make the figure
ggplot(qa_long2, aes(x = ROB_metric, y = count, fill = ROB_type)) +
  geom_bar(stat = "identity", show.legend = TRUE, color = "black", size = 0.25) +  
  labs(x = "Quality Assessment Category", y = "Number of records",
       fill = "Quality Assessment") +
  scale_fill_brewer(palette = "YlOrRd", direction = -1, labels = c("lowROB" = "High Quality", "mediumROB" = "Medium Quality", "highROB" = "Low Quality")) +  
  coord_flip() +  
  theme_minimal(base_size = 15) +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12),  
    axis.title = element_text(size = 14),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic"),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  
    plot.margin = margin(1, 1, 1, 1, "cm")  
  )
ggsave("./Data analysis/Quality_Assessment_Across_Studies.png", height = 10, width = 12)

############################################################ END ############################################################

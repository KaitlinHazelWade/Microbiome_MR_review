#########################################################################################
## Project: Systematic review of MR studies with the gut microbiome as an exposure
## Script: Synthesis of evidence
## Created: 20/09/2023
## By: Kaitlin Wade
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

## Set the working directory (working directory removed for publication)
setwd("..")

############################################################ CREATING DATA FOR ANALYSES ############################################################

#######################################################################
## Cleaning dataset for meta-analyses and creating helpful variables ##
#######################################################################
data <- read.xlsx("./Data analysis/Cleaned_Data_Extracted.xlsx") ## 48082

## Some of the names of exposures are too long to use in the meta-analysis so changing for the purposes of within/between-study replication
table(unique(data$exposure_formatted))
data$exposure_formatted[grepl("Unclassified", data$exposure_formatted)] <- gsub("Unclassified", "unc.", data$exposure_formatted[grepl("Unclassified", data$exposure_formatted)])
table(unique(data$exposure_formatted))

## The fact that one of the IDs is "NA" is going to get annoying so changing this (this is also the folder within which the record is now stored)
data$ID[which(data$first_author == "C.A.F. Rivier")] <- "1234567891"
length(unique(data$ID))

## Some of the author's names need to be more specific for the summary table in the end (so might as well change here)
data$first_author[which(data$first_author == "Hughes")] <- "David A. Hughes"
data$first_author[which(data$first_author == "Clarke")] <- "S. L. Clarke"
data$first_author[which(data$first_author == "Chen" & data$ID == "557846834")] <- "Yilin Chen"
data$first_author[which(data$first_author == "P Tejera")] <- "P. Tejera"
data$first_author[which(data$first_author == "Freidin")] <- "Maxim B. Freidin"
data$first_author[which(data$first_author == "Ning" & data$ID == "1977124126")] <- "Jing Ning"
data$first_author[which(data$first_author == "Rühlemann")] <- "Malte Christoph Rühlemann"
data$first_author[which(data$first_author == "Li Pengsheng")] <- "Pengsheng Li"
data$first_author[which(data$first_author == "Inamo")] <- "Jun Inamo"
data$first_author[which(data$first_author == "Radjabzadeh")] <- "Djawad Radjabzadeh"
data$first_author[which(data$first_author == "Hilde E Groot")] <- "Hilde E. Groot"
data$first_author[which(data$ID == "3113131015")] <- "Huaqiang Zhou"

## Need to make a column that displays the exposure, exposure study, outcome and outcome study (and then I can check the units) to know whether these are comparable
## Therefore, first, let's make a column that is a simplification of the exposure study
table(unique(data$exposure_study)) 
data$study1 <- data$exposure_study
data$study1[which(data$study1 == "Bonder 2016 and Wang 2016")] <- "Mixture of mGWASs"
data$study1[which(data$study1 == "Goodrich 2016 and Turpin 2016")] <- "Mixture of mGWASs"
data$study1[which(data$study1 == "Goodrich 2016; Wang 2016; Turpin 2016; Davenport 2015 and Bonder 2016")] <- "Mixture of mGWASs"
data$study1[which(data$study1 == "Mixture of five studies (Kurilshikov, Ruhlemann for microbial traits and Sanna, Rhee, Kettunen for metabolites)")] <- "Mixture of mGWASs"
data$study1[which(data$study1 == "Mixture of Goodrich and Davenport")] <- "Mixture of mGWASs"
data$study1[which(data$study1 == "Mixture of Goodrich, Bonder and Davenport")] <- "Mixture of mGWASs"
data$study1[which(data$study1 == "Mixture of Wang, Goodrich and Turpin")] <- "Mixture of mGWASs"
data$study1[which(data$study1 == "Unclear (mixture of all, see notes)")] <- "Unclear mixture of mGWASs"
table(unique(data$study1))

# I also want to specify what is meant by "Current study" (will be checking these in turn)
unique(data[which(data$exposure_study == "Current study"), c("ID","first_author","year")])

# Now clarifying the others (that I've checked did perform their own GWAS and then undertake the MR using that data as at least study1)
data$study1[which(data$exposure_study == "Current study" & data$first_author == "Hui-Min Liu")] <- "Liu 2021 (Current study)"
data$study1[which(data$exposure_study == "Current study" & data$first_author == "Wan-Qiang Lv")] <- "Lv 2021 (Current study)"
data$study1[which(data$exposure_study == "Current study" & data$first_author == "Lianmin Chen")] <- "Chen 2022 (Current study)"
data$study1[which(data$exposure_study == "Current study" & data$first_author == "Xiaomin Liu")] <- "Liu 2022 (Current study)"
data$study1[which(data$exposure_study == "Current study" & data$first_author == "Fengzhe Xu")] <- "Xu 2020 (Current study)"
data$study1[which(data$exposure_study == "Current study" & data$first_author == "Wen-Di Shen")] <- "Shen 2022 (Current study)"
data$study1[which(data$exposure_study == "Current study" & data$first_author == "Ulrika Boulund")] <- "Boulund 2022 (Current study)"

# And add information about the studies that did do their own GWAS even though we didn't mention that in the original data extraction
studies <- unique(data$study1)
for(i in studies){
  print(paste0(i, ": ", unique(data$first_author[which(data$study1 == i)])))
}
data$study1[which(data$exposure_study == "Sanna 2019" & data$first_author == "Serena Sanna")] <- "Sanna 2019 (Current study)"
data$study1[which(data$exposure_study == "Hughes 2020" & data$first_author == "David A. Hughes")] <- "Hughes 2020 (Current study)"
data$study1[which(data$exposure_study == "Rühlemann 2021" & data$first_author == "Malte Christoph Rühlemann")] <- "Rühlemann 2021 (Current study)"
data$study1[which(data$exposure_study == "Kurilshikov 2021" & data$first_author == "Alexander Kurilshikov")] <- "Kurilshikov 2021 (Current study)"
data$study1[which(data$exposure_study == "Lopera-Maya 2022" & data$first_author == "Esteban A. Lopera-Maya")] <- "Lopera-Maya 2022 (Current study)"
data$study1[which(data$exposure_study == "Freidin 2021" & data$first_author == "Maxim B. Freidin")] <- "Freidin 2021 (Current study)"
data$study1[which(data$exposure_study == "Qin 2022" & data$first_author == "Youwen Qin")] <- "Qin 2022 (Current study)"
rm(studies)

# Check - I also checked whether we had missed any and there were no other MR studies that performed their own GWAS in the paper before undertaking MR analyses
table(unique(data$study1[which(data$exposure_study == "Current study")]))
table(unique(data$study1))

## Similarly, let's make a column that is a simplification of the outcome data
table(unique(data$outcome_study))
data$study2 <- data$outcome_study
data$study2[which(data$study2 == '"MEGASTROKE"')] <- "Unknown GWAS"
data$study2[which(data$study2 == '"Recent JIA GWAS"')] <- "Unknown GWAS"

# Most of the outcome names follow the same pattern (i.e., "outcome; study"), so I just need to remove the study for most of them
data$study2[grepl(glob2rx("*; *"), data$outcome_study)] <- sub(".*;\\s*", "", data$outcome_study[grepl(glob2rx("*; *"), data$outcome_study)])
table(unique(data$study2))

# Now need to deal with the ones that didn't follow that pattern
data$study2[which(data$study2 == "(PGC) Stahl 2019")] <- "Stahl 2019"
table(unique(data$outcome_study[which(data$study2 == "2018")]))
data$study2[which(data$study2 == "2018")] <- "IOCDF-GC+OCGAS 2018"
data$study2[which(data$study2 == "Andreu-Sánchez (metabolites)")] <- "Andreu-Sánchez 2022"

# Data extracted from Rivier et al. stated that the data used was "current study" but actually they used data from the "MEGASTROKE" but unclear which GWAS
data$study2[which(data$study2 == "Current study" & data$first_author == "C.A.F. Rivier")] <- "Unknown GWAS"

# Now specifying which have "Current study" as study2 (e.g., where they performed their own GWAS or the data for the GWAS is only available in the current study)
data$study2[which(data$study2 == "Current study" & data$first_author == "Charlie Hatcher")] <- paste0("Hatcher ", data$year[which(data$study2 == "Current study" & data$first_author == "Charlie Hatcher")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Eva M. Asensio")] <- paste0("Asensio ", data$year[which(data$study2 == "Current study" & data$first_author == "Eva M. Asensio")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Maxim B. Freidin")] <- paste0("Freidin ", data$year[which(data$study2 == "Current study" & data$first_author == "Maxim B. Freidin")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Hilde E. Groot")] <- paste0("Groot ", data$year[which(data$study2 == "Current study" & data$first_author == "Hilde E. Groot")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Hui-Min Liu")] <- paste0("Liu ", data$year[which(data$study2 == "Current study" & data$first_author == "Hui-Min Liu")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Lianmin Chen")] <- paste0("Chen ", data$year[which(data$study2 == "Current study" & data$first_author == "Lianmin Chen")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Qian Xu")] <- paste0("Xu ", data$year[which(data$study2 == "Current study" & data$first_author == "Qian Xu")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Wan-Qiang Lv")] <- paste0("Lv ", data$year[which(data$study2 == "Current study" & data$first_author == "Wan-Qiang Lv")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Wen-Di Shen")] <- paste0("Shen ", data$year[which(data$study2 == "Current study" & data$first_author == "Wen-Di Shen")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Xiaomin Liu")] <- paste0("Liu ", data$year[which(data$study2 == "Current study" & data$first_author == "Xiaomin Liu")], " (Current study)")
data$study2[which(data$study2 == "Current study" & data$first_author == "Qian Yang")] <- "Yang 2018 (Current study)" ## This is where they meta-analysed CARDIoGRAMplusC4D (Nikpay 2015) with CARDIoGRAMplusC4D Metabochip (Deloukas 2013) adjusting for overlap but these results are not presented
data$study2[which(data$study2 == "Not clear - reference Locke et al (which doesn't mention GWAS of waist-hip ratio) and numbers look closer to Shungin et al.")] <- "Unknown GWAS"
data$study2[which(data$study2 == "Not discussed")] <- "Unknown GWAS"
data$study2[which(data$study2 == "Pan UK Biobank (https://pan.ukbb.broadinstitute.org/downloads/index.html)")] <- "Pan UK Biobank GWAS"
data$study2[which(data$study2 == "Release 5 2020")] <- "FinnGen Release 5 (2020)"
data$study2[which(data$study2 == "Release 7 2022")] <- "FinnGen Release 7 (2022)"
data$study2[grep("Study outcomes are from the Japanese Biobank study", data$study2)] <- "Japanese Biobank"
data$study2[which(data$study2 == "Yufei Wang 2014")] <- "Wang 2014"

# There are a few where the outcome GWAS is "NA"
data$study2[which(data$ID == "261727485")] <- "Unknown GWAS"
data$study2[which(data$ID == "878349061")] <- "Unknown GWAS"
data$study2[which(data$ID == "978492202")] <- "Unknown GWAS"
table(unique(data$study2))

## There are also loads of entries where the outcome study is just listed as "MR-Base" or "IEU OpenGWAS" so, for meta-analysing, I need to decide whether or not it is clear what data was actually used
## To make that decision, I'm going to go through the papers again and see whether or not authors referenced (or provided the PubMedID) and sample size of the GWAS (and this matched the original paper) - if so, I will add the GWAS information as the outcome study
## If the authors provide the MR-Base ID as well as the sample size of the GWAS (i.e., they don't reference or make it clear what the original GWAS was), then that should be enough to list the GWAS information as the outcome study if the sample sizes match
## However, if the authors only provide the MR-Base ID and do not provide a reference / PubMedID of the original paper and the sample size of the GWAS (or they provide this but it doesn't match with the original), then I'll list this as unclear
table(unique(data$ID[which(data$study2 == "MR-Base / IEUOpenGWAS")])) # 8 studies: 1795779689, 1828507192, 2238214110, 2662850683, 3278202937, 557846834, 718621312, Other sources
colnames(data)

## Let's go one by one to solve what dataset they used
# First one: 1795779689
# Outcome = forms of epilepsy and authors list the MR-Base ID and sample sizes of these outcomes (noting that the sample sizes always match the information on MR-Base)
# The IDs include: ieu-b-8 to ieu-b-17, noting that the consortium is referenced - "ILAE" (International League Against Epilepsy) and all of these come from the PubMed ID 30531953
# This is a consortium and everything matches so I can change this
table(unique(data$outcome[which(data$ID == 1795779689)]))
table(unique(data$outcome_def[which(data$ID == 1795779689)]))
table(unique(data$outcome_n_diff[which(data$ID == 1795779689)]))
data$study2[which(data$study2 == "MR-Base / IEUOpenGWAS" & data$ID == 1795779689)] <- "ILAE 2018"
table(unique(data$study2))

# Second one: 1828507192
# Outcome = lots of outcomes including nutrition, anthropometry and adiposity, diseases including Alzheimer's and celiac disease = random
# Some of the IDs seem to be from UK Biobank and some from the IEU OpenGWAS
# Sample sizes between what is presented in the paper and the original GWAS seem to match for all but HDL cholesterol and Triglycerides (where no study size was mentioned)
# Going back through the paper, there are missing values for the sample sizes for both the GWASs of those phenotypes, which are both from the Neale Lab GWAS of UK Biobank (even though there are other phenotypes from that data source) and the IEU OpenGWAS also doesn't have a sample size for these
# For the phenotypes where authors do state the MR-Base ID, the PubMed ID (where possible) and the sample sizes, I'll look up the exact GWAS and put that as the outcome study information if it all matches
# For HDL and Trigs, I'll put UK Biobank
table(unique(data$outcome[which(data$ID == 1828507192)]))
table(unique(data$outcome_def[which(data$ID == 1828507192)]))
table(unique(data$outcome_n_diff[which(data$ID == 1828507192)]))
table(unique(data$outcome[which(data$ID == 1828507192 & data$outcome_n_diff %in% c("No participant number listed","No participant number mentioned"))]))
table(unique(data$outcome_def[which((data$ID == 1828507192) & grepl("ukb", data$outcome_def))])) 
length(unique(data$outcome_def[which((data$ID == 1828507192) & grepl("ukb", data$outcome_def))])) # 59 of 78 outcomes from UK Biobank (either IEU or Neale's lab edition)
data$study2[which((data$ID == 1828507192) & grepl("ukb", data$outcome_def))] <- "UK Biobank"
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ebi-a-GCST002222")] <- "Willer 2013" ## This was LDL cholesterol from Willer 2013
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ebi-a-GCST006908")] <- "Malik 2018" ## This was ischemic stroke from Malik 2018 though note the sample size from Malik and the IEU OpenGWAS are not the same
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-1024")] <- "Sawcer 2011" ## This was multiple sclerosis from Sawcer 2011
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-1054")] <- "Kottgen 2013" ## This was gout from Kottgen 2013
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-26")] <- "Morris 2012" # This was type 2 diabetes from Morris 2012
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-276")] <- "Dubois 2010" # This was celiac disease from Dubois 2010
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-297")] <- "Lambert 2013" # This was Alzheimer's disease from Lambert 2013
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-30")] <- "Liu 2015" # This was Crohn's disease from Liu 2015
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-31")] <- "Liu 2015" # This was IBD from Liu 2015
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-32")] <- "Liu 2015" # This was ulcerative colitis from Liu 2015
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-44")] <- "Moffatt 2010" # This was asthma from Moffatt 2010
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-45")] <- "Boraska 2014" # This was anorexia nervosa from Boraska 2014
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-73")] <- "Shungin 2015" # This was waist-to-hip ratio from Shungin 2015 though note that the sample size from Shungin and the IEU OpenGWAS are not the same
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-8")] <- "Schunkert 2011" # This is coronary heart disease from Schunkert 2011
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-812")] <- "Simón-Sánchez 2009" # This is Parkinson's disease from Simón-Sánchez 2009
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-815")] <- "Hom 2009" # This is systemic lupus erythematosus from Hom 2009
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-832")] <- "Okada 2014" # This is rheumatoid arthritis from Okada 2014
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-835")] <- "Locke 2015" # This is body mass index from Locke 2015
data$study2[which(data$ID == 1828507192 & data$outcome_def == "MR-Base ID = ieu-a-996")] <- "Paternoster 2015" # This is eczema from Paternoster 2015 but note that the sample size in the MR paper, GWAS and IEU OpenGWAS are different (likely due to removal of 23andMe) and that both the MR paper and IEU OpenGWAS say that the date was 2014 but the GWAS is 2015
data$study2[which(data$study2 == "MR-Base / IEUOpenGWAS" & data$ID == 1828507192 & data$outcome_n_diff %in% c("No participant number listed","No participant number mentioned") & data$outcome %in% c("HDL cholesterol", "Triglycerides"))] <- "UK Biobank"
table(unique(data$study2))

# Third one: 2238214110
# Lots of "brain-related" outcomes
# Mostly from UK Biobank (with "UKB-a/b" as the ID) but some with just numbers as IDs so I will assume that they are the MR-Base IDs (but will check sample sizes)
# Note that the authors only present (in Supplementary Table 1) the case numbers for what appears to be their primary outcomes (from UK Biobank) but they do not present the sample sizes for any other phenotypes (therefore, I can't compare the IDs to see whether they match across the GWAS used and the GWAS paper)
# Therefore, for those with "UKB" in the name, I'll write "UK Biobank" but I'll write "Unclear" for the others
table(unique(data$outcome[which(data$ID == 2238214110)]))
table(unique(data$outcome_def[which(data$ID == 2238214110)])) 
table(unique(data$outcome_n_diff[which(data$ID == 2238214110)]))
table(unique(data$outcome_def[which((data$ID == 2238214110) & grepl("UKB", data$outcome_def))]))
length(data$outcome_def[which((data$ID == 2238214110) & grepl("UKB", data$outcome_def))]) ## 459 out of 515 outcomes were from UK Biobank
data$study2[which((data$ID == 2238214110) & grepl("UKB", data$outcome_def))] <- "UK Biobank"
data$study2[which(data$ID == 2238214110 & data$study2 == "MR-Base / IEUOpenGWAS")] <- "Unknown GWAS"
table(unique(data$study2[which(data$ID == 2238214110)]))
table(unique(data$study2))

# Fourth one: 2662850683
# Lots of outcomes that have been previously associated with the microbiome
# Paper says that outcome data were from existing GWASs or summary statistics from the Neale Lab UK Biobank but all have their own IDs
# In all cases, the numbers of cases/controls are not provided so it's difficult to know exactly what data has been used because I cannot compare the sample sizes from the original GWAS with those presented in the paper
table(unique(data$outcome[which(data$ID == 2662850683)]))
table(unique(data$outcome_def[which(data$ID == 2662850683)])) ## Note that one of the entries (with MR-Base ID 297) says "see column CE", which is the bias column which reiterates the above point about the sample sizes not being provided
table(unique(data$outcome_n_diff[which(data$ID == 2662850683)])) ## Again, stating that there were no sample sizes provided for any outcome
data$study2[which(data$ID == 2662850683)] <- "Unknown GWAS"
table(unique(data$study2))

# Fifth one: 3278202937
# Lots of outcomes, mainly diseases
# Similar to the previous paper, the authors have provided the IDs from the MR-Base catalog but they have not provided the sample sizes for these, therefore it is impossible to confirm which data were used
# In all cases, the number of cases/controls were not provided therefore we'll have to say that the GWAS is unclear
table(unique(data$outcome[which(data$ID == 3278202937)]))
table(unique(data$outcome_def[which(data$ID == 3278202937)]))
table(unique(data$outcome_n_diff[which(data$ID == 3278202937)]))
data$study2[which(data$ID == 3278202937)] <- "Unknown GWAS"
table(unique(data$study2))

# Sixth one: 557846834
# Only one outcome = pulmonary hypertension
# This was an abstract, where the outcome data source was specified in the table (UKB-b-439) but this does not exist in the current IEU OpenGWAS and the sample size (463010) seems to be a summary of the whole UK Biobank 
# It might be ukb-b-12493 but this is not the same as what is presented and the abstract does not present the cases and controls number so this is unclear
table(unique(data$outcome[which(data$ID == 557846834)]))
table(unique(data$outcome_def[which(data$ID == 557846834)]))
table(unique(data$outcome_study[which(data$ID == 557846834)]))
table(unique(data$outcome_n_diff[which(data$ID == 557846834)]))
data$study2[which(data$ID == 557846834)] <- "Unknown GWAS"
table(unique(data$study2))

# Seventh one: 718621312
# Lots of binary disease outcomes again with only MR-Base IDs (or one instance where the MR-Base ID was not mentioned)
# In most cases, the sample sizes match up with what the original GWAS and IEU Open GWAS and enough information was provided in the supplement to add information to the outcome study
# Note that the authors have a supplementary table (Table 4) where they present IDs for other studies but these are not included in the results shown of the gut microbiome on outcomes
table(unique(data$outcome[which(data$ID == 718621312)]))
table(unique(data$outcome_def[which(data$ID == 718621312)]))
table(unique(data$outcome_n_diff[which(data$ID == 718621312)]))
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1054")] <- "Kottgen 2013" ## This was gout from Kottgen 2013
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1086")] <- "van Rheenen 2016" ## Note that this ID does not exist but the PubMedID and the sample size they quote matches with the ID 1085 therefore likely a mistake
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1102")] <- "Pattaro 2016" ## This is chronic kidney disease from Pattaro 2016
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1126")] <- "Michailidou 2017" ## This is breast cancer from Michailidou 2017
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1169")] <- "Zeggini 2012" ## This is hip osteoarthritis from the arcOGEN consortium (but there is another MR study that specified the first author name) - note that the numbers here seem only relating to one particular set of discovery samples
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1171")] <- "Zeggini 2012" ## This is hip and knee osteoarthritis from the arcOGEN consortium (but there is another MR study that specified the first author name) - note that the numbers here seem only relating to one particular set of discovery samples
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1185")] <- "Unknown GWAS" ## This is autism spectrum disorder from MR-Base, but no PubMed ID is provided - looking into the 2017 release, which this seems to be from, there is a paper by Grove et al. 2019 with similar numbers (though, one number off - cases from original paper are 18381 but IEU OpenGWAS says 18382 - and publication year was 2017, not 2019) so this is not clear enough
data$outcome_n_diff[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1185")] <- "Sample size numbers and publication year do not exactly match the most likely publication from which this data has been sourced"
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1186")] <- "Duncan 2017" ## This is anorexia nervosa from Duncan 2017
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-1188")] <- "Wray 2018" ## This is major depressive disorder from Wray 2018 but note that the sample size in the MR paper, GWAS and IEU OpenGWAS are different (likely due to removal of 23andMe)
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-22")] <- "Ripke 2014" ## This is schizophrenia from Ripke 2014 - note that there is already a Ripke 2013 and this is indeed a different, earlier study
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-297")] <- "Lambert 2013" ## This is Alzheimer's disease from Lambert 2013 but note that this is the discovery and not the replication
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-965")] <- "Wang 2015" ## This is lung adenocarcinoma from Wang 2015
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-966")] <- "Wang 2015" ## This is lung cancer from Wang 2015
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-967")] <- "Wang 2015" ## This is squamous cell adenocarcinoma of the lung from Wang 2015
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-975")] <- "Albagha 2011" ## This is paget's disease from Albagha 2011
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID = ieu-a-996")] <- "Paternoster 2015" # This is eczema from Paternoster 2015 but note that the sample size in the MR paper, GWAS and IEU OpenGWAS are different (likely due to removal of 23andMe) and that both the MR paper and IEU OpenGWAS say that the date was 2014 but the GWAS is 2015
data$study2[which(data$ID == 718621312 & data$outcome_def == "MR-Base ID not mentioned")] <- "Unknown GWAS"
table(unique(data$study2))

# Eighth one: Other sources
# Lots of outcomes including brain, cardiometabolic and lifespan-related traits - some of which are already in the right format (last name and year of the GWAS)
# However, there are a small number that are from MR-Base / IEUOpenGWAS (body mass index and osteoporosis) and there doesn't seem to be any differences in the numbers presented
# There also doesn't seem to be information in the "outcome_def" column about what GWAS they have been sourced from so have gone back through the paper 
# That information is provided and both studies have been sourced from UK Biobank but this information was provided in the data extraction, despite the paper referencing this
table(unique(data$first_author[which(data$ID == "Other sources")]))
table(unique(data$title[which(data$ID == "Other sources")]))
table(unique(data$outcome[which(data$ID == "Other sources")]))
table(unique(data$outcome_study[which(data$ID == "Other sources")]))
table(unique(data$outcome[which(data$ID == "Other sources" & data$outcome_study == "MR-Base / IEUOpenGWAS")]))
table(unique(data$outcome_n_diff[which(data$ID == "Other sources" & data$outcome_study == "MR-Base / IEUOpenGWAS")]))
table(unique(data$outcome_def[which(data$ID == "Other sources" & data$outcome_study == "MR-Base / IEUOpenGWAS")]))
data$outcome_def[which(data$ID == "Other sources" & data$outcome == "Body mass index")] <- "MR-Base ID = ukb-b-19953"
data$outcome_og_gwas_case[which(data$ID == "Other sources" & data$outcome == "Body mass index")] <- 461460
data$outcome_sex[which(data$ID == "Other sources" & data$outcome == "Body mass index")] <- "Both"
data$outcome_def[which(data$ID == "Other sources" & data$outcome == "Osteoporosis")] <- "MR-Base ID = ukb-b-12141"
data$outcome_og_gwas_case[which(data$ID == "Other sources" & data$outcome == "Osteoporosis")] <- 7547
data$outcome_og_gwas_control[which(data$ID == "Other sources" & data$outcome == "Osteoporosis")] <- 455386
data$outcome_sex[which(data$ID == "Other sources" & data$outcome == "Osteoporosis")] <- "Both"
data$study2[which(data$ID == "Other sources" & data$outcome == "Body mass index")] <- "UK Biobank"
data$study2[which(data$ID == "Other sources" & data$outcome == "Osteoporosis")] <- "UK Biobank"
table(unique(data$study2))

## For the sake of meta-analysing, we also need to make sure the exposure units are shortened to include this info in the analysis (if I haven't changed them, they've stayed the same)
table(unique(data$exposure_unit))
data$e_unit <- data$exposure_unit
data$e_unit[data$exposure_unit == '"genetically predicted abundance"'] <- "NA"
data$e_unit[data$exposure_unit == "Abundance (but not clear what unit is)"] <- "NA"
data$e_unit[data$exposure_unit == "Abundance (SD of log-transformed values)"] <- "abundance (SD)"
data$e_unit[data$exposure_unit == "Abundance (SD)"] <- "abundance (SD)"
data$e_unit[data$exposure_unit == "Not clear"] <- "NA"
data$e_unit[data$exposure_unit == 'Not clear (authors say "decrease in bacterial taxa")'] <- "NA"
data$e_unit[data$exposure_unit == 'Not clear (authors say "normalised bacterial counts")'] <- "NA"
data$e_unit[data$exposure_unit == 'Not clear (authors say "unit decrease")'] <- "NA"
data$e_unit[data$exposure_unit == 'Not clear (authors say "unit increase")'] <- "NA"
data$e_unit[data$exposure_unit == 'Not clear (authors say "Z-score decrease")'] <- "NA"
data$e_unit[data$exposure_unit == 'Not clear (authors say "Z-score increase")'] <- "NA"
data$e_unit[data$exposure_unit == "Not clear (possibly SD or SD of log values)"] <- "NA"
data$e_unit[data$exposure_unit == "Not clear (possibly SD)"] <- "NA"
data$e_unit[data$exposure_unit == "Not clear (possibly Z-score)"] <- "NA"
data$e_unit[data$exposure_unit == "Not described"] <- "NA"
data$e_unit[data$exposure_unit == "Not discussed"] <- "NA"
data$e_unit[data$exposure_unit == "Not mentioned"] <- "NA"
data$e_unit[data$exposure_unit == "Per allele (thus not presented)"] <- "per allele"
data$e_unit[data$exposure_unit == "Presence (approximate doubling of the genetic liability to presence vs. absence)"] <- "presence"
data$e_unit[data$exposure_unit == "Presence (but not clear what unit is)"] <- "NA"
data$e_unit[data$exposure_unit == "Presence (log(OR))"] <- "presence"
data$e_unit[data$exposure_unit == "Relative abundance (Box-Cox transformed)"] <- "abundance (Box-Cox)"
data$e_unit[data$exposure_unit == "Relative abundance (but not clear what unit is)"] <- "NA"
data$e_unit[data$exposure_unit == "Relative abundance (log10 transformed)"] <- "abundance (log10)"
data$e_unit[data$exposure_unit == "Relative abundance (SD)"] <- "abundance (SD)"
table(unique(data$e_unit))

## For the sake of meta-analysing, we also need to make sure the outcome units are shortened to include this info in the analysis (if I haven't changed them, they've stayed the same)
table(unique(data$outcome_unit))
data$o_unit <- data$outcome_unit
data$o_unit[which(data$outcome_unit == '"outcome units"')] <- "NA"
data$o_unit[which(data$outcome_unit == "log odds")] <- "log(OR)"
data$o_unit[which(data$outcome_unit == "mg/dL per allele")] <- "mg/dL"
data$e_unit[which(data$outcome_unit == "mg/dL per allele")] <- "per allele"
data$o_unit[which(data$outcome_unit == "mU×min/L")] <- "mU*min/L"
data$o_unit[which(data$outcome_unit == "Not clear")] <- "NA"
data$o_unit[which(data$outcome_unit == 'Not clear (authors say "natural log transformed" 25(OH)D)')] <- "NA"
data$o_unit[which(data$outcome_unit == 'Not clear (authors say "original")')] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (log-transformed units)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly %)")] <- "NA"
data$o_unit[which(data$outcome_unit == "Not clear (possibly (mmol/L)")] <- "NA"
data$o_unit[which(data$outcome_unit == "Not clear (possibly a ratio (T/S) of telomere repeat length (T) to copy the number of a single copy gene (S))")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly kg)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly kg/m^2)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly log odds)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly mmHg)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly mmol/L)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly mmol/mol)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly OR)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly ratio)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly SD but this is a binary variable)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (possibly SD)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not clear (results presented as ORs but this is a continuous variable)")] <- "NA" 
data$o_unit[which(data$outcome_unit == "Not discussed")] <- "NA" 
table(unique(data$o_unit))

## Now just clarifying where the study design was unclear
data$study_design[which(data$ID == "878349061")] <- "Unknown"

## And missing DOIs:
data$doi[which(data$ID == "23119582")] <- "Not provided"

## Now let's have a look at the cases where the authors say they replicated
table(unique(data$replication)) 
data$bin_rep <- 0
data$bin_rep[which(data$replication == "Included an additional 199 participants with both genetic  data and gut microbiome data as a replication cohort, which belonged to the control arm of a case-control study of hip fracture in Guangdong Province, China")] <- 1
data$bin_rep[which(data$replication == "Japan Biobank was used as a replication to findings in this study")] <- 1
data$bin_rep[which(data$replication == "They performed one- and two-sample MR analyses (though not replication)")] <- 1
data$bin_rep[which(data$replication == "They used the UKB as a replication set and then also used a different mGWAS for exposure data for the gut microbiome")] <- 1
data$bin_rep[which(data$replication == "This was the replication")] <- 1
data$bin_rep[which(data$replication == "Yes - they say they replicated but the outcome data that they used included essentially the same data (Dubois and Trynka)")] <- 1
data$bin_rep[which(data$replication == "Yes - they undertook the same analysis within UK Biobank (results presented below the discovery)")] <- 1
data$bin_rep[which(data$replication == "Yes - this is the discovery; Present GCTA-GSMR estimates for discovery  and replication cohort, but not clear how each of these cohorts were used in the 2SMR setting")] <- 1
data$bin_rep[which(data$replication == "Yes - this is the discovery; Split into 1539 individuals in discovery and 1006 in replication")] <- 1
data$bin_rep[which(data$replication == "Yes - this is the replication; Present GCTA-GSMR estimates for discovery  and replication cohort, but not clear how each of these cohorts were used in the 2SMR setting")] <- 1
data$bin_rep[which(data$replication == "Yes - this is the replication; Split into 1539 individuals in discovery and 1006 in replication")] <- 1
data$bin_rep[which(data$replication == "Yes (discovery presented here)")] <- 1
data$bin_rep[which(data$replication == "Yes (see below) - they use TwinsUK (Goodrich 2016) as their discovery and Lifelines deep (Bonder) as their replication")] <- 1
table(data$bin_rep) ## 1138 estimates that are apparently replicated within studies
table(unique(data$ID[which(data$bin_rep == 1)])) ## 9 studies that apparently replicated internally but I'm unsure about a few of those so need to check them all and write a comparison

## Also need to make sure that the beta, se, p, lci and uci columns are all numeric for meta-analysis
data$mrbeta <- as.numeric(data$mrbeta)
data$dup_mrbeta <- as.numeric(data$dup_mrbeta)
data$mrse <- as.numeric(data$mrse)
data$dup_mrse <- as.numeric(data$dup_mrse)
data$mrlci <- as.numeric(data$mrlci)
data$dup_mrlci <- as.numeric(data$dup_mrlci)
data$mruci <- as.numeric(data$mruci)
data$dup_mruci <- as.numeric(data$dup_mruci)
data$mrp <- as.numeric(data$mrp)
data$dup_mrp <- as.numeric(data$dup_mrp)

## Need to see whether there are any exposure-outcome relationships that have been replicated
## We can also see whether there were any other internally replicated analyses (i.e., where ID, exposure and outcome are the same but outcome study is different)
## To do that, let's create a variable that includes all this information
## At minimum, I need a variable where the exposure unit is there and I can explore whether estimates can be combined based on outcome units
data$e_s1_o_s2 <- NA
data$e_s1_o_s2 <- paste0(data$e_unit, "_", data$exposure_formatted, "_", data$study1, "_", data$outcome, "_", data$study2)
data[c(1:10), c("e_unit", "exposure_formatted", "exposure_study", "study1", "outcome", "outcome_study", "study2", "e_s1_o_s2")]
colnames(data)

## Need to see whether there are any exposure-outcome relationships that have been replicated (where the exposure unit information is also informative)
data$exp_out <- paste0(data$e_unit, "_", data$exposure_formatted,"_",data$outcome)
rep_data <- data %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_data_dups <- rep_data[which(rep_data$matches >1),] ## 5768 rows of analyses that were replicated

## Let's see how many exposures, outcomes and combinations of the two there are in this general dataset
combo <- as.vector(unique(rep_data_dups$exp_out)) ## 2700 combinations 
exposures <- as.vector(unique(rep_data_dups$exposure_formatted)) ## 284 different exposures
outcome_types <- as.vector(unique(rep_data_dups$outcome_group)) ## 16 different outcome groups
outcomes <- as.vector(unique(rep_data_dups$outcome)) ## 193 different outcomes
rm(combo, exposures, outcomes, outcome_types)

#####################################################
## Creating dataset for within-study meta-analyses ##
#####################################################
## Check that there are no others that have been internally replicated (i.e., ones where we mistakenly said that there wasn't an internal replication but there was)
## i.e., where the exposures, outcomes and ID is the same but the outcome study is different
internal_rep_check <- rep_data_dups %>%
  group_by(exp_out) %>%
  filter(n_distinct(study2) > 1,
         n_distinct(ID) == 1) %>%
  filter(n() > 1) %>%
  ungroup() ## 506 where they seem to replicate but in the same study (i.e., same exposure (and unit), same outcome, different outcome study, same ID)
table(unique(internal_rep_check$ID)) ## 7 unique IDs, where 4 were in the list of those above (where bin_rep==1) but 5 of those above don't appear here, with 3 new ones
## Those identified IDs include 1308760143, 1324207092, 2044689642, 2179267930, 2238214110, 2934159401, 707846668 

## Already appeared (i.e., bin_rep==1): 2044689642, 2179267930, 2934159401, 3032725182, 3272561227, 568199176, 601575515, 707846668, 914635920 
table(data$bin_rep) ## 1142 estimates that are replicated within studies
table(unique(data$ID[which(data$bin_rep == 1)])) ## 9 studies

## For some reason this code is not picking up 3032725182, 3272561227, 568199176, 601575515, 914635920 (despite replication internally)
## It did pick up these: 2044689642, 2179267930, 2934159401, 707846668
## Those that don't appear (i.e., where bin_rep!=1 and there are extra internal replications) are: 1308760143, 1324207092, 2238214110 (so need to add these in my internal rep analyses)
## Check that the internal_rep_check object captures the self-proclaimed internally replicated analyses (it does)
table(unique(internal_rep_check$bin_rep[internal_rep_check$ID == 2044689642]))
table(unique(internal_rep_check$bin_rep[internal_rep_check$ID == 2934159401]))
table(unique(internal_rep_check$bin_rep[internal_rep_check$ID == 707846668]))

## Need to change bin_rep==1 for those three IDs and then check them later in my internally replicated analyses
## And create a dataset of "internally replicated" signals
data$bin_rep[which(data$ID == 1308760143)] <- 1
data$bin_rep[which(data$ID == 1324207092)] <- 1
data$bin_rep[which(data$ID == 2238214110)] <- 1
internal_rep <- data[which(data$bin_rep == 1),] 
table(unique(internal_rep$ID)) ## 12 studies that apparently replicated internally but I'm unsure about a few of those so need to check them all and write a comparison
dim(internal_rep) ## 1735 now with the addition of these new studies

## Now let's take these study by study to see whether they are indeed replications and then store these in a "meta_analysis" object
## To do this, in each study, I'll also be creating a column that includes information on the exposure and outcome
# First one = 2044689642 (came up in "internal_rep_check")
table(unique(internal_rep$ID))
rep_2044 <- internal_rep[which(internal_rep$ID == "2044689642"),] 
dim(rep_2044) ## 24 of the 1735
table(rep_2044$exposure_formatted) ## this is with one exposure so just need to figure out which outcomes are duplicated (i.e., replicated estimates exist)
table(rep_2044$outcome) ## Lots of outcomes but only some are duplicated
table(rep_2044$study_design) ## Same study design
rep_2044 <- rep_2044 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_2044_dups <- rep_2044[which(rep_2044$matches >1),]
dim(rep_2044_dups) ## 4 of the 24 were replicated
head(rep_2044_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")])
table(data$replication[which(data$ID == "2044689642")]) ## They did use two different outcome studies so this is a replication but only a few were actually replicated
data$replication[which(data$ID == "2044689642" & data$exp_out %in% rep_2044_dups$exp_out)] <- "Yes"
data$replication[which(data$ID == "2044689642" & data$replication != "Yes")] <- "No"
rm(rep_2044)

# Second one = 2179267930 (came up in "internal_rep_check")
table(unique(internal_rep$ID)) 
rep_2179 <- internal_rep[which(internal_rep$ID == "2179267930"),] 
dim(rep_2179) ## 269 of the 1735
table(rep_2179$exposure_formatted) ## Lots of exposures but only some are duplicated
table(rep_2179$outcome) ## Lots of outcomes but only some are duplicated
table(rep_2179$study_design) ## Different study designs
rep_2179 <- rep_2179 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_2179_dups <- rep_2179[which(rep_2179$matches >1),]
dim(rep_2179_dups) ## Actually, only 74 of the 269 are those results that are replicated
head(rep_2179_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")])
head(rep_2179_dups[,c("exposure_formatted","outcome","replication","study_design")])

table(rep_2179_dups$replication)  ## They did use two different outcome studies and two different study designs so this is a replication but, whilst they say that they used Japan Biobank as a replication, there are only 4 exposure-outcome associations in Japan Biobank that were also in the original study
rep_2179_dups$replication[which(rep_2179_dups$ID == "2179267930" & rep_2179_dups$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Split", rep_2179_dups$replication))] <- "Discovery"
rep_2179_dups$replication[which(rep_2179_dups$ID == "2179267930" & rep_2179_dups$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Split", rep_2179_dups$replication))] <- "Replication"
rep_2179_dups$replication[which(rep_2179_dups$ID == "2179267930" & rep_2179_dups$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Present GCTA-GSMR", rep_2179_dups$replication))] <- "Discovery"
rep_2179_dups$replication[which(rep_2179_dups$ID == "2179267930" & rep_2179_dups$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Present GCTA-GSMR", rep_2179_dups$replication))] <- "Replication"
rep_2179_dups$replication[which(rep_2179_dups$ID == "2179267930" & rep_2179_dups$exp_out %in% rep_2179_dups$exp_out & grepl("Japan Biobank was used as a replication to findings in this study", rep_2179_dups$replication))] <- "Replication"
data$exposure_study_n[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Split", data$replication))] <- 1539
data$outcome_og_gwas_case[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Split", data$replication))] <- 1539
data$outcome_study_case[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Split", data$replication))] <- 1539
data$replication[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Split", data$replication))] <- "Discovery"
data$exposure_study_n[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Split", data$replication))] <- 1006 ## Looked back at paper and it's either 1004 (in text) or 1006 (in tables) but as 1006 is said most often, have added this
data$outcome_og_gwas_case[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Split", data$replication))] <- 1006 ## Looked back at paper and it's either 1004 (in text) or 1006 (in tables) but as 1006 is said most often, have added this
data$outcome_study_case[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Split", data$replication))] <- 1006 ## Looked back at paper and it's either 1004 (in text) or 1006 (in tables) but as 1006 is said most often, have added this
data$replication[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Split", data$replication))] <- "Replication"
data$exposure_study_n[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Present GCTA-GSMR", data$replication))] <- 1539
data$outcome_og_gwas_case[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Present GCTA-GSMR", data$replication))] <- 1539
data$outcome_study_case[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Present GCTA-GSMR", data$replication))] <- 1539
data$replication[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the discovery; Present GCTA-GSMR", data$replication))] <- "Discovery"
data$exposure_study_n[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Present GCTA-GSMR", data$replication))] <- 1006
data$outcome_og_gwas_case[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Present GCTA-GSMR", data$replication))] <- 1006
data$outcome_study_case[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Present GCTA-GSMR", data$replication))] <- 1006
data$replication[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Yes - this is the replication; Present GCTA-GSMR", data$replication))] <- "Replication"
data$exposure_study_n[which(data$ID == "2179267930" & grepl("Japan Biobank was used as a replication to findings in this study", data$replication))] <- 1539
data$replication[which(data$ID == "2179267930" & data$exp_out %in% rep_2179_dups$exp_out & grepl("Japan Biobank was used as a replication to findings in this study", data$replication))] <- "Replication"
data$replication[which(data$ID == "2179267930" & data$replication %notin% c("Discovery", "Replication"))] <- "No"
rm(rep_2179)

# Third one = 2238214110 (which didn't come up in the original "internal_rep_check" because the outcome_study was all MR-Base originally)
table(unique(internal_rep$ID)) 
rep_2238 <- internal_rep[which(internal_rep$ID == "2238214110"),] 
dim(rep_2238) ## 515 of the 1735
table(rep_2238$exposure_formatted) ## One "gut microbiome" exposure
table(rep_2238$outcome) ## Lots of outcomes but only some are duplicated
table(rep_2238$study_design) ## Same study design
rep_2238 <- rep_2238 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_2238_dups <- rep_2238[which(rep_2238$matches >1),]
dim(rep_2238_dups) ## 166 of those 515 are replicated
head(rep_2238_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")])
table(rep_2238_dups$replication) ## THEY USED UK BIOBANK (AND NOT INDEPENDENT SAMPLES) OR AN UNKNOWN GWAS; THEREFORE THIS IS NOT A REPLICATION (need to therefore change the replication column both here and in the main dataset)
data$replication[which(data$ID == "2238214110" & data$exp_out %in% rep_2238_dups$exp_out)] <- "No, they did not use a different or clear outcome data sources"
data$replication[which(data$ID == "2238214110" & data$replication != "No, they did not use a different or clear outcome data sources")] <- "No"
rm(rep_2238, rep_2238_dups) ## So none of these should be included in the dataset that indicates the replicated results within studies

# Fourth one = 2934159401 (came up in "internal_rep_check")
table(unique(internal_rep$ID)) 
rep_2934 <- internal_rep[internal_rep$ID == "2934159401",] 
dim(rep_2934) ## 445 of the 1735
table(rep_2934$exposure_formatted) ## Lots of exposures but only some are duplicated
table(rep_2934$outcome) ## This is with one outcome so just need to figure out which outcomes are duplicated (i.e., replicated estimates exist)
table(rep_2934$study_design) ## Same study design
rep_2934 <- rep_2934 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_2934_dups <- rep_2934[rep_2934$matches >1,]
dim(rep_2934_dups) ## 441 of the 445 are results that are replicated; but note that the exposure unit is not clear
head(rep_2934_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")]) 
data$replication[which(data$ID == "2934159401" & data$exp_out %in% rep_2934_dups$exp_out)] <- "Yes, they used the UK Biobank as a replication set and then also used a different mGWAS for exposure data for the gut microbiome"
data$replication[which(data$ID == "2934159401" & data$replication != "Yes, they used the UK Biobank as a replication set and then also used a different mGWAS for exposure data for the gut microbiome")] <- "No"
rm(rep_2934)

# Fifth one = 3032725182 (not sure why this didn't come up in the "internal_rep_check")
table(unique(internal_rep$ID)) 
rep_3032 <- internal_rep[internal_rep$ID == "3032725182",] 
dim(rep_3032) ## 12 of the 1735
table(rep_3032$exposure_formatted) ## this is with a few exposures but only a few are replicated 
table(rep_3032$outcome) ## One outcome
table(rep_3032$study_design) ## Same study design
rep_3032 <- rep_3032 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_3032_dups <- rep_3032[rep_3032$matches >1,]
dim(rep_3032_dups) ## Only 4 seem to be replicated
head(rep_3032_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")]) 
table(rep_3032_dups$replication)  ## THEY USED TWO EXPOSURE DATASETS BUT NOT TWO OUTCOME DATASETS THEREFORE THIS IS NOT A REPLICATION (need to therefore change the replication column both here and in the main dataset)
data$replication[which(data$ID == "3032725182" & data$exp_out %in% rep_3032_dups$exp_out)] <- "No, whilst authors do say that they replicated, they did not use a different outcome data sources"
data$replication[which(data$ID == "3032725182" & data$replication != "No, whilst authors do say that they replicated, they did not use a different outcome data sources")] <- "No"
rm(rep_3032, rep_3032_dups) ## So none of these should be included in the dataset that indicates the replicated results within studies

# Sixth one = 3272561227 (didn't come up in the original "internal_rep_check" because the outcome_study is all "Not discussed")
table(unique(internal_rep$ID)) 
rep_3272 <- internal_rep[internal_rep$ID == "3272561227",] 
dim(rep_3272) ## 6 of the 1735
table(rep_3272$exposure_formatted) ## A few exposures, all of which seem to be duplicated
table(rep_3272$outcome) ## One outcome
table(rep_3272$study_design) ## Same study design
rep_3272 <- rep_3272 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_3272_dups <- rep_3272[rep_3272$matches >1,]
dim(rep_3272_dups) ## All 6 of the 6 are those results that are replicated; also note that the exposure unit is not clear
head(rep_3272_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")]) 
table(rep_3272_dups$replication) ## They say that this is a replication but the outcome study isn't mentioned (at least the exact GWAS wasn't cited), but they do appear to be different but don't know whether the samples are independent
data$replication[which(data$ID == "3272561227")] <- "No, whilst authors do say that they replicated, it is not clear whether they used different outcome data sources"
rm(rep_3272, rep_3272_dups)

# Seventh one = 568199176 (again, not sure why this didn't come up in the "internal_rep_check" because it's clearly same exposure, outcome, ID but different outcome study)
table(unique(internal_rep$ID)) 
rep_5681 <- internal_rep[internal_rep$ID == "568199176",] 
dim(rep_5681) ## 2 of the 1735
table(rep_5681$exposure_formatted) ## One exposure
table(rep_5681$outcome) ## One outcome
table(rep_5681$study_design) ## Different study designs
rep_5681 <- rep_5681 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_5681_dups <- rep_5681[rep_5681$matches >1,]
dim(rep_5681_dups)
head(rep_5681[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication")]) ## Note that they do apparently do a replication but (i) the exposure unit in the one-sample is not clear and (ii) this is across different ancestries
data$replication[which(data$ID == "568199176" & data$exp_out %in% rep_5681$exp_out)] <- "No, they performed one- and two-sample MR analysis but units are unclear and different ancestries" 
rm(rep_5681, rep_5681_dups) ## So none of these should be included in the dataset that indicates the replicated results within studies

# Eighth one = 601575515 (not sure why this didn't come up in the "internal_rep_check")
table(unique(internal_rep$ID)) 
rep_6015 <- internal_rep[internal_rep$ID == "601575515",] 
dim(rep_6015) ## 9 of the 1735
table(rep_6015$exposure_formatted) ## These are SNPs, not microbial exposures
table(rep_6015$outcome) ## Lots of outcomes but only some are duplicated
table(rep_6015$study_design) ## Same study design
rep_6015 <- rep_6015 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_6015_dups <- rep_6015[rep_6015$matches >1,]
dim(rep_6015_dups)
head(rep_6015[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")]) 
table(rep_6015$replication) ## THEY DO SAY THAT THE REPLICATED BUT THIS WAS NOT EXTRACTED BECAUSE THEY USED ESSENTIALLY THE SAME DATASETS AS THE OUTCOME THEREFORE THIS IS NOT A REPLICATION (need to therefore change the replication column both here and in the main dataset)
data$replication[which(data$ID == "601575515" & data$exp_out %in% rep_6015$exp_out)] <- "No, whilst authors do say that they replicated, they did used non-independent outcome data sources"
rm(rep_6015, rep_6015_dups) ## So none of these should be included in the dataset that indicates the replicated results within studies

# Ninth one = 707846668 (came up in "internal_rep_check")
table(unique(internal_rep$ID)) 
rep_7078 <- internal_rep[internal_rep$ID == "707846668",] 
dim(rep_7078) ## 65 of the 1735
table(rep_7078$exposure_formatted) ## Lots of exposures, some of which are duplicated
table(rep_7078$outcome) ## Lots of outcomes but only some are duplicated
table(rep_7078$study_design) ## Same study design
rep_7078 <- rep_7078 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_7078_dups <- rep_7078[rep_7078$matches >1,]
dim(rep_7078_dups) ## Only 18 of the 65 are those results that are replicated
head(rep_7078_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")]) ## Note that the units are consistently inconsistent
table(rep_7078_dups$replication) ## However, the dubois and trynka (for celiac) and julia and bentham (for lupus) are non-independent so need to remove those
data$replication[which(data$ID == "707846668" & data$exp_out %in% rep_7078_dups$exp_out)] <- "Yes"
data$replication[which(data$ID == "707846668" & data$replication != "Yes")] <- "No"
data$replication[which(data$ID == "707846668" & data$outcome == "Celiac disease")] <- "No, whilst authors do say that they replicated, they did used non-independent outcome data sources"
data$replication[which(data$ID == "707846668" & data$outcome == "Systemic lupus erythematosus")] <- "No, whilst authors do say that they replicated, they did used non-independent outcome data sources"
rep_7078_dups <- rep_7078_dups[-which(rep_7078_dups$outcome %in% c("Celiac disease", "Systemic lupus erythematosus")),] ## now 12 estimates
rm(rep_7078)

# Tenth one = 914635920 (not sure why this didn't come up in the "internal_rep_check")
table(unique(internal_rep$ID)) 
rep_9146 <- internal_rep[internal_rep$ID == "914635920",] 
dim(rep_9146) ## 310 of the 1735
table(rep_9146$exposure_formatted) ## Lots of exposures and all seem duplicated to the same amount
table(rep_9146$outcome) ## Lots of outcomes but only some are duplicated
table(rep_9146$study_design) ## Same study design
rep_9146 <- rep_9146 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_9146_dups <- rep_9146[rep_9146$matches >1,]
dim(rep_9146_dups)
head(rep_9146[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")])
table(rep_9146$replication) ## Whilst this was originally classified as having a replication element, this was only for the GWAS; therefore, it is not a replication (need to therefore change the replication column both here and in the main dataset)
data$replication[which(data$ID == "914635920")] <- "No"
rm(rep_9146, rep_9146_dups) ## So none of these should be included in the dataset that indicates the replicated results within studies

# Eleventh one = 1308760143 (came up in "internal_rep_check")
table(unique(internal_rep$ID)) 
rep_1308 <- internal_rep[internal_rep$ID == "1308760143",] 
dim(rep_1308) ## 22 of the 1735
table(rep_1308$exposure_formatted) ## Lots of exposures
table(rep_1308$outcome) ## Lots of outcomes but only some are duplicated
table(rep_1308$study_design) ## Same study design
rep_1308 <- rep_1308 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_1308_dups <- rep_1308[rep_1308$matches >1,]
dim(rep_1308_dups) ## 4 of the original 23 are replicated; though, not that the exposure unit is per allele rather than an MR effect estimate
head(rep_1308_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")])
table(rep_1308_dups$replication) ## One outcome with two exposures seem to be replicated even though they weren't specified in the data but the data are not independent so this shouldn't be here either
data$replication[which(data$ID == "1308760143")] <- "No"
rm(rep_1308, rep_1308_dups) 

# Twelfth one = 1324207092 (came up in "internal_rep_check")
table(unique(internal_rep$ID)) 
rep_1324 <- internal_rep[internal_rep$ID == "1324207092",] 
dim(rep_1324) ## 56 of the 1735
table(rep_1324$exposure_formatted) ## Lots of exposures and all seem duplicated to the same amount
table(rep_1324$outcome) ## One outcome
table(rep_1324$study_design) ## Same study design but note that these are of different ancestry
rep_1324 <- rep_1324 %>%
  group_by(exp_out) %>%
  mutate(matches = n()) %>%
  ungroup()
rep_1324_dups <- rep_1324[rep_1324$matches >1,]
dim(rep_1324_dups) ## All 56 seem to be replicated; though, note that the exposure unit is not clear
head(rep_1324_dups[,c("e_unit","exposure_formatted","study1","outcome","study2","exp_out","replication","matches")])
table(rep_1324_dups$replication) ## Originally "not discussed" but these are in different ancestries so not explictly a replication
data$replication[which(data$ID == "1324207092")] <- "No"
rm(rep_1324, rep_1324_dups) 

## Some things have now changed having looked at the replication status within studies, therefore, outputting these replicated results into an updated "internal replication" spreadsheet
## Also need to make sure that the beta, se, p, lci and uci columns are all numeric for meta-analysis
rm(internal_rep)
internal_rep <- rbind(rep_2044_dups, rep_2179_dups, rep_2934_dups, rep_7078_dups)
rm(rep_2044_dups, rep_2179_dups, rep_2934_dups, rep_7078_dups)
table(unique(internal_rep$ID)) ## 4 of these 12 studies that apparently replicated internally had replications based on both the papers themselves and comparisons within records
dim(internal_rep) ## This represented 531 of the original 1735 replicated estimates
internal_rep$mrbeta <- as.numeric(internal_rep$mrbeta)
internal_rep$dup_mrbeta <- as.numeric(internal_rep$dup_mrbeta)
internal_rep$mrse <- as.numeric(internal_rep$mrse)
internal_rep$dup_mrse <- as.numeric(internal_rep$dup_mrse)
internal_rep$mrlci <- as.numeric(internal_rep$mrlci)
internal_rep$dup_mrlci <- as.numeric(internal_rep$dup_mrlci)
internal_rep$mruci <- as.numeric(internal_rep$mruci)
internal_rep$dup_mruci <- as.numeric(internal_rep$dup_mruci)
internal_rep$mrp <- as.numeric(internal_rep$mrp)
internal_rep$dup_mrp <- as.numeric(internal_rep$dup_mrp)
internal_rep$bin_rep <- NULL

## Let's see how many exposures, outcomes and combinations of the two there are in this general dataset
combo <- as.vector(unique(internal_rep$exp_out)) ## 223 combinations 
exposures <- as.vector(unique(internal_rep$exposure_formatted)) ## 209 different exposures
outcome_types <- as.vector(unique(internal_rep$outcome_group)) ## 3 different outcome groups
outcomes <- as.vector(unique(internal_rep$outcome)) ## 15 different outcomes
write.xlsx(internal_rep, "./Data analysis/Internal_Rep_For_Analyses.xlsx", asTable = TRUE, overwrite = TRUE, keepNA = TRUE)
rm(internal_rep_check, combo, exposures, outcome_types, outcomes)

######################################################
## Creating dataset for between-study meta-analyses ##
######################################################
## For comparison across studies, we need the exposure and outcome to be the same but the outcome data to be different
## And that this doesn't represent the MR papers that did an internal replication (i.e., the ID is the same)
## (i.e., where there are matches in exposure, matches in outcome, mismatches in outcome study and mismatches in ID)
rep_across_studies <- rep_data_dups %>%
  group_by(exp_out) %>%
  filter(n_distinct(study2) > 1,
         n_distinct(ID) > 1) %>%
  filter(n() > 1) %>%
  ungroup() ## 1237 where they seem to replicate over studies (i.e., same exposure, same outcome, different outcome study, different ID)
rep_across_studies$bin_rep <- NULL
write.xlsx(rep_across_studies, "./Data analysis/Between_Study_Data_For_Analyses.xlsx", asTable = TRUE, overwrite = TRUE, keepNA = TRUE)

## Let's see how many exposures, outcomes and combinations of the two there are in this general dataset
combo <- as.vector(unique(rep_across_studies$exp_out)) ## 545 combinations 
exposures <- as.vector(unique(rep_across_studies$exposure_formatted)) ## 80 different exposures
outcome_types <- as.vector(unique(rep_across_studies$outcome_group)) ## 9 different outcome groups
outcomes <- as.vector(unique(rep_across_studies$outcome)) ## 51 different outcomes
rm(internal_rep, rep_across_studies, combo, exposures, outcomes, outcome_types)

#############################################################################
## Creating dataset for comparing studies that have done the same analysis ##
#############################################################################
## Let's see whether there were any studies that have done the same thing but got different answers, we'll need everything to be the same
same_analysis <- rep_data_dups %>%
  filter(e_s1_o_s2 %in% e_s1_o_s2[duplicated(e_s1_o_s2)]) ## 4085 analyses that apparently have been done in the same datasets
same_analysis$bin_rep <- NULL

## Let's see how many exposures, outcomes and combinations of the two there are in the dataset where the same analyses have been run
combo <- as.vector(unique(same_analysis$exp_out)) ## 1961 combinations apparently
exposures <- as.vector(unique(same_analysis$exposure_formatted)) ## 132 different exposures
outcome_types <- as.vector(unique(same_analysis$outcome_group)) ## 15 different outcome groups
outcomes <- as.vector(unique(same_analysis$outcome)) ## 163 different outcomes
write.xlsx(same_analysis, "./Data analysis/Same_Analysis.xlsx", asTable = TRUE, overwrite = TRUE, keepNA = TRUE)
rm(rep_data, rep_data_dups, same_analysis, combo, exposures, outcomes, outcome_types)

## Write a version of the main data out as well
write.xlsx(data, "./Data analysis/Cleaned_Data_Extracted_For_Analyses.xlsx", asTable = TRUE, overwrite = TRUE, keepNA = TRUE)
rm(data)

############################################################ WITHIN-STUDY ANALYSES ############################################################

####################################################################
## Comparing the results that have been replicated within studies ##
####################################################################
internal_rep <- read.xlsx("./Data analysis/Internal_Rep_For_Analyses.xlsx")
table(internal_rep$ID) ## 4 studies so taking them study by study and meta-analysing the results within studies
table(internal_rep$outcome_formatted) ## outcomes include those relating to adiposity, autoimmune diseases, diabetes, metabolites and optic neuritis
table(internal_rep$outcome[which(internal_rep$outcome_formatted == "Autoimmune disease")]) ## Mixture of autoimmune diseases
table(internal_rep$ID[which(internal_rep$outcome_formatted == "Autoimmune disease")]) ## 707846668 
table(internal_rep$outcome[which(internal_rep$ID == "707846668")]) ## Lots of autoimmune diseases so will need to combine these two studies (if comparable) for IBD
colnames(internal_rep)

## Let's go study by study and outcome by outcome first
## First one = 2044689642
int_2044 <- internal_rep[which(internal_rep$ID == "2044689642"),]
table(int_2044$matches)
head(int_2044[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")])
table(int_2044$outcome) ## 2 outcomes
table(int_2044$exposure_formatted) ## 1 exposure
table(int_2044$study_design) ## Only two-sample 
table(unique(int_2044$e_unit)) ## Abundance (SD)
table(unique(int_2044$o_unit)) ## Mixture depending on outcome (logOR or SD), would like to present the type 2 diabetes results as an OR though
int_2044_check <- int_2044[,c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]

# Two results can be meta-analysed across studies given the same exposure (from the same study, so units are the same) and the same two outcomes (likely measured the same - units for BMI in UKBB is only noted as "outcome units" and these are never specified but it's likely to be the same as in discovery) in two different studies
# Let's do BMI first (mean difference)
rep <- int_2044[which(int_2044$outcome == "Body mass index"),]
rep[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]
label <- unique(paste0(rep$exposure_formatted, " ", rep$e_unit, " on Body mass index (kg/m^2) in ", rep$first_author, " ", rep$year))
meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "MD", hakn = FALSE, title = label)
write.csv(meta_res, "./Data analysis/Meta results/WithinStudy/2044689642/Body mass index.csv", quote = FALSE, row.names = FALSE)
pdf("./Data analysis/Meta results/WithinStudy/2044689642/Body mass index.pdf", height = 4, width = 10)
forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("studlab"), leftlabs = c("Outcome study"))
grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
dev.off()
rm(label, rep, meta_res)

# Then, let's do type 2 diabetes
rep <- int_2044[which(int_2044$outcome == "Type 2 diabetes"),]
rep[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]
label <- unique(paste0(rep$exposure_formatted, " ", rep$e_unit, " on Type 2 diabetes in ", rep$first_author, " ", rep$year))
meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "OR", hakn = FALSE, title = label)
write.csv(meta_res, "./Data analysis/Meta results/WithinStudy/2044689642/Type 2 diabetes.csv", quote = FALSE, row.names = FALSE)
pdf("./Data analysis/Meta results/WithinStudy/2044689642/Type 2 diabetes.pdf", height = 4, width = 10)
forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("studlab"), leftlabs = c("Outcome study"))
grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
dev.off()
rm(label, rep, meta_res)
rm(int_2044, int_2044_check)

## Second one = 2179267930
int_2179 <- internal_rep[which(internal_rep$ID == "2179267930"),]
table(int_2179$matches)
head(int_2179[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")])
table(int_2179$outcome) ## 9 outcomes
table(int_2179$exposure_formatted) ## 13 exposures
table(int_2179$study_design) ## Both one- and two-sample with discovery and replication 
table(unique(int_2179$e_unit)) ## Abundance
table(unique(int_2179$o_unit)) ## Not clear but consistently not clear... and all continuous metabolites
int_2179_check <- int_2179[,c("first_author","year","e_s1_o_s2", "e_unit", "o_unit","study_design","replication", "mrbeta","mrse","mrlci","mruci","mrp")]

# Want to meta-analyse for each exposure-outcome pair and include information about whether they are one- or two-sample MR
outcome_list <- as.vector(unique(int_2179$outcome))
exposure_list <- as.vector(unique(int_2179$exposure_formatted))
for(i in outcome_list){
  for(j in exposure_list){
    tryCatch({
      rep <- int_2179[which(int_2179$outcome == i & int_2179$exposure_formatted == j),]
      rep[, c("first_author","year","e_s1_o_s2", "study_design", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]
      label <- unique(paste0(j, " ", rep$e_unit, " on ", i, " in ", rep$first_author, " ", rep$year))
      meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "MD", hakn = FALSE, title = label)
      write.csv(meta_res, paste0("./Data analysis/Meta results/WithinStudy/2179267930/", j, " ", i , ".csv"), quote = FALSE, row.names = FALSE)
      pdf(paste0("./Data analysis/Meta results/WithinStudy/2179267930/", j, " ", i, ".pdf"), height = 4, width = 10)
      forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("study2","study_design", "replication"), leftlabs = c("Outcome study", "Study design", "Analysis"))
      grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
      dev.off()
      rm(label, rep, meta_res)
    }, error=function(e){cat("Error :",conditionMessage(e), "\n")})
  }
}
rm(outcome_list, exposure_list, i, j, int_2179, int_2179_check)

## Third one = 2934159401
int_2934 <- internal_rep[which(internal_rep$ID == "2934159401"),]
table(int_2934$matches)
head(int_2934[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")])
table(int_2934$outcome) ## 1 outcome
table(int_2934$exposure_formatted) ## lots of exposures
table(int_2934$study_design) ## Only two-sample 
table(unique(int_2934$e_unit)) ## Not clear though likely SD
table(unique(int_2934$o_unit)) ## OR therefore need to convert to logOR for metagen
int_2934_check <- int_2934[,c("first_author","year","e_s1_o_s2","study1","study2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]

# Want to meta-analyse for each exposure-outcome pair
exposure_list <- as.vector(unique(int_2934$exposure_formatted))
for(j in exposure_list){
  tryCatch({
    rep <- int_2934[which(int_2934$exposure_formatted == j),]
    rep[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]
    rep$mrbeta <- log(rep$mrbeta)
    rep$mrlci <- log(rep$mrlci)
    rep$mruci <- log(rep$mruci)
    label <- unique(paste0(j, " on ", rep$outcome, " in ", rep$first_author, " ", rep$year))
    meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "OR", hakn = FALSE, title = label)
    write.csv(meta_res, paste0("./Data analysis/Meta results/WithinStudy/2934159401/", j, ".csv"), quote = FALSE, row.names = FALSE)
    pdf(paste0("./Data analysis/Meta results/WithinStudy/2934159401/", j, ".pdf"), height = 4, width = 10)
    forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("studlab", "study1"), leftlabs = c("Outcome study", "Exposure study"))
    grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
    dev.off()
    rm(label, rep, meta_res)
  }, error=function(e){cat("Error :",conditionMessage(e), "\n")})
}
rm(exposure_list, j)

# But note that there are two exposure datasets here and only the analysis with the Kurilshikov dataset uses both outcome datasets (i.e., the Goodrich dataset only uses FinnGen so not directly comparable with Kurilshikov analysis)
int_2934_kuril <- int_2934[which(int_2934$study1 != "Goodrich 2016"),]
int_2934_kuril_check <- int_2934_kuril[,c("first_author","year","exposure_formatted","e_s1_o_s2","study1","study2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]
exposure_list <- as.vector(unique(int_2934_kuril$exposure_formatted))
for(j in exposure_list){
  tryCatch({
    rep <- int_2934_kuril[which(int_2934_kuril$exposure_formatted == j),]
    rep[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]
    rep$mrbeta <- log(rep$mrbeta)
    rep$mrlci <- log(rep$mrlci)
    rep$mruci <- log(rep$mruci)
    label <- unique(paste0(j, " on ", rep$outcome, " in ", rep$first_author, " ", rep$year))
    meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "OR", hakn = FALSE, title = label)
    write.csv(meta_res, paste0("./Data analysis/Meta results/WithinStudy/2934159401/MBG/", j, "_MBG.csv"), quote = FALSE, row.names = FALSE)
    pdf(paste0("./Data analysis/Meta results/WithinStudy/2934159401/MBG/", j, "_MBG.pdf"), height = 4, width = 10)
    forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("studlab"), leftlabs = c("Outcome study"))
    grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
    dev.off()
    rm(label, rep, meta_res)
  }, error=function(e){cat("Error :",conditionMessage(e), "\n")})
}
rm(exposure_list, j, int_2934, int_2934_kuril, int_2934_check, int_2934_kuril_check)

## Fourth one = 707846668
int_7078 <- internal_rep[which(internal_rep$ID == "707846668"),]
table(int_7078$matches)
head(int_7078[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")])
table(int_7078$outcome) ## 3 outcomes
table(int_7078$exposure_formatted) ## 2 exposures
table(int_7078$study_design) ## Only two-sample 
table(unique(int_7078$e_unit)) ## Not clear (possibly SD)
table(unique(int_7078$o_unit)) ## OR therefore we need to log transform these for metagen
int_7078_check <- int_7078[,c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]

# Want to meta-analyse for each exposure-outcome pair
outcome_list <- as.vector(unique(int_7078$outcome))
exposure_list <- as.vector(unique(int_7078$exposure_formatted))
for(i in outcome_list){
  for(j in exposure_list){
    tryCatch({
      rep <- int_7078[which(int_7078$outcome == i & int_7078$exposure_formatted == j),]
      rep[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]
      rep$mrbeta <- log(rep$mrbeta)
      rep$mrlci <- log(rep$mrlci)
      rep$mruci <- log(rep$mruci)
      label <- unique(paste0(j, " on ", i, " in ", rep$first_author, " ", rep$year))
      meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "OR", hakn = FALSE, title = label)
      write.csv(meta_res, paste0("./Data analysis/Meta results/WithinStudy/707846668/", j, " ", i , ".csv"), quote = FALSE, row.names = FALSE)
      pdf(paste0("./Data analysis/Meta results/WithinStudy/707846668/", j, " ", i, ".pdf"), height = 4, width = 10)
      forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("studlab"), leftlabs = c("Outcome study"))
      grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
      dev.off()
      rm(label, rep, meta_res)
    }, error=function(e){cat("Error :",conditionMessage(e), "\n")})
  }
}
rm(outcome_list, exposure_list, i, j, int_7078, int_7078_check)

## Save a copy of the data that were internally replicated
write.xlsx(internal_rep, "./Data analysis/Internal_replication_After_Meta.xlsx", asTable = TRUE, overwrite = TRUE, keepNA = TRUE)
rm(internal_rep)

############################################################ BETWEEN-STUDY ANALYSES ############################################################

###########################################################
## Comparing results that were replicated across studies ##
###########################################################
## In order to compare across studies, I am using the criteria below (where i-iv have already been used to select these results included here):
## (i) is the exposure the same (by name)?
## (ii) is the outcome the same (by name)?
## (iii) is the ID of the study different (i.e., is this across studies that I haven't already dealt with in the internal replication analysis above)?
## (iv) is the outcome study different (i.e., in order to be a replication, the outcome GWAS must be independent across studies)?
## (v) are the exposure units the same?
## (vi) are the outcome units the same?
## Therefore, I have to be strict with the exposure and outcome units - only meta-analysing those where the units are clearly the same across studies

## Most efficient way of doing this seems to be by outcome group
rep_across_studies <- read.xlsx("./Data analysis/Between_Study_Data_For_Analyses.xlsx")
combo <- as.vector(unique(rep_across_studies$exp_out)) ## 545 combinations 
exposures <- as.vector(unique(rep_across_studies$exposure_formatted)) ## 80 different exposures
outcome_types <- as.vector(unique(rep_across_studies$outcome_group)) ## 9 different outcome groups
outcomes <- as.vector(unique(rep_across_studies$outcome)) ## 51 different outcomes
table(outcome_types)
rm(combo, exposures, outcome_types, outcomes)
colnames(rep_across_studies)

## Autoimmunity = 453 analyses
autoimmune <- rep_across_studies[which(rep_across_studies$outcome_group == "Autoimmunity"),]
head(autoimmune[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(autoimmune$matches)
table(autoimmune$outcome) ## 7 outcomes
table(autoimmune$exposure_formatted) ## Lots of exposures
table(autoimmune$study_design) ## Only two-sample 
table(unique(autoimmune$e_unit)) 
keep_exp_units <- c("abundance (SD)")
autoimmune <- autoimmune[which(autoimmune$e_unit %in% keep_exp_units),] ## 6 analyses
table(unique(autoimmune$o_unit)) 
keep_out_units <- "log(OR)"
autoimmune <- autoimmune[which(autoimmune$o_unit %in% keep_out_units),] ## 3 analyses
rm(keep_exp_units, keep_out_units)
autoimmune_check <- autoimmune[,c("ID","exposure_formatted","study1","outcome","study2","exp_out","matches","mrbeta","mrse","mrlci","mruci","mrp")]

# Now figure out how many of those are matched
rep_autoimmune <- autoimmune %>%
  group_by(exp_out) %>%
  filter(n_distinct(study2) > 1,
         n_distinct(ID) > 1) %>%
  filter(n() > 1) %>%
  ungroup() ## None can be meta-analysed due to differences in units
rm(autoimmune, autoimmune_check, rep_autoimmune)

## Brain = 272 analyses
brain <- rep_across_studies[which(rep_across_studies$outcome_group == "Brain"),]
head(brain[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(brain$matches)
table(brain$outcome) ## 8 outcomes
table(brain$exposure_formatted) ## Lots of exposures
table(brain$study_design) ## Only two-sample 
table(unique(brain$e_unit)) 
keep_exp_units <- c("abundance (SD)")
brain <- brain[which(brain$e_unit %in% keep_exp_units),] ## 157 analyses
table(unique(brain$o_unit)) 
keep_out_units <- c("OR","log(OR)")
brain <- brain[which(brain$o_unit %in% keep_out_units),] ## 88 analyses
rm(keep_exp_units, keep_out_units)
brain_check <- brain[,c("ID","exposure_formatted","study1","outcome","study2","exp_out","matches","mrbeta","mrse","mrlci","mruci","mrp")]

# There are negative numbers in the study ID 122541860 so these are logOR so will create logOR for the other study ID (1977124126)
brain$mrbeta[which(brain$ID == 1977124126)] <- log(brain$mrbeta[which(brain$ID == 1977124126)])
brain$mrlci[which(brain$ID == 1977124126)] <- log(brain$mrlci[which(brain$ID == 1977124126)])
brain$mruci[which(brain$ID == 1977124126)] <- log(brain$mruci[which(brain$ID == 1977124126)])

# Now figure out how many of those are matched
rep_brain <- brain %>%
  group_by(exp_out) %>%
  filter(n_distinct(study2) > 1,
         n_distinct(ID) > 1) %>%
  filter(n() > 1) %>%
  ungroup() ## 9 combinations that can now be meta-analysed
rep_brain_check <- rep_brain[,c("ID","e_s1_o_s2","exp_out","matches","mrbeta","mrse","mrlci","mruci","mrp")]

# There were two entries for the relationshp between Bifidobacterium and Alzheimer's disease because one of the studies did an analysis with instruments selected at genome-wide significance as well as a lenient p-value
# Therefore, I'm going to make that clear in the label by making a new column that I'll use in the plot
table(unique(rep_brain$pval_threshold))
table(unique(rep_brain$study1))
rep_brain$instrument_selection <- NA
rep_brain$instrument_selection[which(rep_brain$study1 =="Hughes 2020 (Current study)")] <- "p<2.5e-08"
rep_brain$instrument_selection[which(rep_brain$study1 =="Kurilshikov 2021")] <- "p<1e-05"
rep_brain$instrument_selection[which(rep_brain$outcome == "Alzheimer's disease" & rep_brain$mrbeta == log(1.05400000))] <- "p<5e-08"
table(unique(rep_brain$instrument_selection))

# Want to meta-analyse for each exposure-outcome pair
outcome_list <- as.vector(unique(rep_brain$outcome)) ## 2 outcomes now
exposure_list <- as.vector(unique(rep_brain$exposure_formatted)) ## 3 exposures now
for(i in outcome_list){
  for(j in exposure_list){
    tryCatch({
      rep <- rep_brain[which(rep_brain$outcome == i & rep_brain$exposure_formatted == j),]
      rep[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "instrument_selection", "mrbeta","mrse","mrlci","mruci","mrp")]
      label <- unique(paste0(j, " ", rep$e_unit, " on ", i))
      rep$record <- paste0(rep$first_author, " et al., ", rep$year)
      meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "OR", hakn = FALSE, title = label)
      write.csv(meta_res, paste0("./Data analysis/Meta results/BetweenStudies/Brain/", j, " ", i , ".csv"), quote = FALSE, row.names = FALSE)
      pdf(paste0("./Data analysis/Meta results/BetweenStudies/Brain/", j, " ", i, ".pdf"), height = 4, width = 10)
      forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("record","studlab","instrument_selection"), leftlabs = c("Record ID","Outcome study", "Instrument"))
      grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
      dev.off()
      rm(label, rep, meta_res)
    }, error=function(e){cat("Error :",conditionMessage(e), "\n")})
  }
}
rm(outcome_list, exposure_list, i, j)

# Suppose it would also be good to have an "overall" estimate for just Bifidobacterium and Alzheimer's disease using the best estimates (i.e., using strongest instruments)
rep <- rep_brain[which(rep_brain$exposure_formatted == "Bifidobacterium (Genus)" & rep_brain$outcome == "Alzheimer's disease" & rep_brain$instrument_selection %in% c("p<5e-08", "p<2.5e-08")),]
rep[, c("first_author","year","e_s1_o_s2", "e_unit", "o_unit", "instrument_selection", "mrbeta","mrse","mrlci","mruci","mrp")]
label <- unique(paste0(rep$exposure_formatted, " ", rep$e_unit, " on ", rep$outcome))
rep$record <- paste0(rep$first_author, " et al., ", rep$year)
meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "OR", hakn = FALSE, title = label)
write.csv(meta_res, paste0("./Data analysis/Meta results/BetweenStudies/Brain/Bifidobacterium on AD using best instruments.csv"), quote = FALSE, row.names = FALSE)
pdf(paste0("./Data analysis/Meta results/BetweenStudies/Brain/Bifidobacterium on AD using best instruments.pdf"), height = 4, width = 10)
forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("record","studlab"), leftlabs = c("Record ID","Outcome study"))
grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
dev.off()
rm(label, rep, meta_res)
rm(brain, brain_check, rep_brain_check) ## WHILST THIS CAN BE META-ANALYSED, THE DATA FOR BOTH ALZHEIMER'S DISEASE AND PARKINSON'S DISEASE AREN'T TOTALLY INDEPENDENT

## Cardiovascular = 18 analyses
cardio <- rep_across_studies[which(rep_across_studies$outcome_group == "Cardiovascular"),]
head(cardio[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(cardio$matches)
table(cardio$outcome) ## 2 outcomes
table(cardio$exposure_formatted) ## 9 exposures
table(cardio$study_design) ## Only two-sample 
table(unique(cardio$e_unit)) ## Not clear so can't meta-analyse
table(unique(cardio$o_unit)) ## Despite having clear outcome units, exposure unit is not clear so will not be meta-analysing any of these
rm(cardio)

## Kidney = 31 analyses
kidney <- rep_across_studies[which(rep_across_studies$outcome_group == "Kidney"),]
head(kidney[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(kidney$matches)
table(kidney$outcome) ## 1 outcome
table(kidney$exposure_formatted) ## Lots of exposures
table(kidney$study_design) ## Only two-sample 
table(unique(kidney$e_unit)) ## No units are clear so cannot meta-analyse
table(unique(kidney$o_unit)) ## Whilst units are log(OR), the exposure units are not
rm(kidney)

## Metabolic health = 139 analyses
metab <- rep_across_studies[which(rep_across_studies$outcome_group == "Metabolic health"),]
head(metab[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(metab$matches)
table(metab$outcome) ## 7 outcomes
table(metab$exposure_formatted) ## Lots of exposures
table(metab$study_design) ## Mixture of one-sample and two-sample analyses
metab <- metab[which(metab$study_design != '"One-sample" but list two-sample methods'),] ## # One study says they undertook a "one-sample" analysis but listed two-sample methods so will be removing this
table(unique(metab$e_unit)) 
keep_exp_units <- c("abundance (SD)")
metab <- metab[which(metab$e_unit %in% keep_exp_units),] ## 4 analyses
table(unique(metab$o_unit)) 
keep_out_units <- c("SD") 
metab <- metab[which(metab$o_unit %in% keep_out_units),] ## 2 analyses
rm(keep_exp_units, keep_out_units)
metab_check <- metab[,c("ID","exposure_formatted","study1","outcome","study2","exp_out","matches","mrbeta","mrse","mrlci","mruci","mrp")]

# Now figure out how many of those are matched
rep_metab <- metab %>%
  group_by(exp_out) %>%
  filter(n_distinct(study2) > 1,
         n_distinct(ID) > 1) %>%
  filter(n() > 1) %>%
  ungroup() ## None can be meta-analysed across studies due to differences in outcomes
rm(metab, metab_check, rep_metab)

## Nutrition = 270 analyses
nutrition <- rep_across_studies[which(rep_across_studies$outcome_group == "Nutrition"),]
head(nutrition[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(nutrition$matches)
table(nutrition$outcome) ## Lots of outcomes
table(nutrition$exposure_formatted) ## 6 exposures
table(nutrition$study_design) ## Only two-sample 
table(unique(nutrition$e_unit)) 
table(unique(nutrition$o_unit)) ## Despite clear outcome unit, no clear exposure unit so cannot meta-analyse
rm(nutrition)

## Sexual and reproductive health = 14 analyses
reproductive <- rep_across_studies[which(rep_across_studies$outcome_group == "Sexual and reproductive health"),]
head(reproductive[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(reproductive$matches)
table(reproductive$outcome) ## 1 outcome
table(reproductive$exposure_formatted) ## 7 exposures
table(reproductive$study_design) ## Only two-sample 
table(unique(reproductive$e_unit)) ## The exposure data units are not clear so cannot meta-analyse
table(unique(reproductive$o_unit)) ## Even though the outcome data are in OR, the exposure unit information is not clear
rm(reproductive)

## Save a copy of the data that were internally replicated (the only one was of the "Brain" subtype)
write.xlsx(rep_brain, "./Data analysis/Between_Study_Data_For_Meta.xlsx", asTable = TRUE, overwrite = TRUE, keepNA = TRUE)
rm(rep_brain, rep_across_studies)

############################################################ ANALYSES DOING THE SAME THING ############################################################

##############################################################################
## Comparing results that were undertaking the same analyses across studies ##
##############################################################################
## In order to compare across studies, I am using the criteria below (where i-iii have already been used to select these results included here):
## (i) is the exposure the same (by name)?
## (ii) is the outcome the same (by name)?
## (iii) is the exposure and outcome study the same?
## (iv) are the exposure units the same?
## (v) are the outcome units the same?
## Therefore, I have to be strict with the exposure and outcome units - only saying that these are the same if all data and analyses are the same

## Most efficient way of doing this seems to be by outcome group
same <- read.xlsx("./Data analysis/Same_Analysis.xlsx") ## 4085 combinations
combo <- as.vector(unique(same$exp_out)) ## 1961 combinations 
exposures <- as.vector(unique(same$exposure_formatted)) ## 132 different exposures
outcome_types <- as.vector(unique(same$outcome_group)) ## 15 different outcome groups
outcomes <- as.vector(unique(same$outcome)) ## 163 different outcomes
table(outcome_types)
rm(combo, exposures, outcome_types, outcomes)
colnames(same)

## Autoimmunity = 637
autoimmune <- same[which(same$outcome_group == "Autoimmunity"),]
head(autoimmune[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(autoimmune$matches)
table(autoimmune$outcome) ## 7 outcomes
table(autoimmune$exposure_formatted) ## Lots of exposures
table(autoimmune$study_design) ## Only two-sample 
table(unique(autoimmune$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(autoimmune$o_unit)) ## OR or log(OR) depending on outcome 
autoimmune_check <- autoimmune[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(autoimmune_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# This usually indicates a sensitivity analysis or that the exposure data are different (e.g., labelled "abundance" and "presence/absence" but the "e_unit" is NA for because it was consistently not clear)
same_autoimmune <- autoimmune %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 26 can be compared apparently

# There is one study (707846668) which uses a genome-wide p-value threshold and another (776143774) which uses both a genome-wide and lenient p-value threshold
# We hadn't labelled these separately for the second study so I'm going to recode these such that I know which results are from when using those different thresholds
# Then, I can compare the two sets of analyses across these two studies that used the genome-wide p-value threshold
table(unique(same_autoimmune$exposure_formatted))
table(unique(same_autoimmune$pval_threshold[which(same_autoimmune$ID==776143774)]))
same_autoimmune$pval_threshold[which(same_autoimmune$ID == 776143774 & same_autoimmune$exposure_formatted == "Allisonella (Genus)" & same_autoimmune$mrbeta == 0.34)] <- "5x10-08"
same_autoimmune$pval_threshold[which(same_autoimmune$ID == 776143774 & same_autoimmune$exposure_formatted == "Bifidobacterium (Genus)" & same_autoimmune$mrbeta == -0.31)] <- "5x10-08"
same_autoimmune_check <- same_autoimmune[,c("ID","exposure_formatted","e_s1_o_s2","e_unit","o_unit","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","analysis_notes","other_gwasinfo","pval_threshold","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]

# Now remove the analyses that can't be compared
same_autoimmune <- same_autoimmune[-(which(same_autoimmune$pval_threshold !="5x10-08")),]

# Now check where there are the same exposures and outcomes
same_autoimmune_new <- same_autoimmune %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 4 can be compared

# There is one method that uses "Inverse variance weighted (both fixed and random effects)" so want to remove so we're just compared the same method
# However, I've checked with the original paper and thsi was indeed from a fixed effects, which is the same as the other paper
same_autoimmune_new$main_method[which(same_autoimmune_new$main_method =="Inverse variance weighted (both fixed and random effects)")] <- "Inverse variance weighted"
same_autoimmune_new_check <- same_autoimmune_new[,c("ID","exposure_formatted","e_s1_o_s2","e_unit","o_unit","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","analysis_notes","other_gwasinfo","pval_threshold","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]

# One is an OR so need to convert
same_autoimmune_new$mrbeta[which(same_autoimmune_new$o_unit == "OR")] <- log(same_autoimmune_new$mrbeta[which(same_autoimmune_new$o_unit == "OR")])
same_autoimmune_new$mrlci[which(same_autoimmune_new$o_unit == "OR")] <- log(same_autoimmune_new$mrlci[which(same_autoimmune_new$o_unit == "OR")])
same_autoimmune_new$mruci[which(same_autoimmune_new$o_unit == "OR")] <- log(same_autoimmune_new$mruci[which(same_autoimmune_new$o_unit == "OR")])
same_autoimmune_new_check <- same_autoimmune_new[,c("ID","exposure_formatted","e_s1_o_s2","e_unit","o_unit","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","analysis_notes","other_gwasinfo","pval_threshold","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]

# Want to meta-analyse for each exposure-outcome pair
outcome_list <- as.vector(unique(same_autoimmune_new$outcome)) ## 1 outcome now
exposure_list <- as.vector(unique(same_autoimmune_new$exposure_formatted)) ## 2 exposures now
for(i in outcome_list){
  for(j in exposure_list){
    tryCatch({
      rep <- same_autoimmune_new[which(same_autoimmune_new$outcome == i & same_autoimmune_new$exposure_formatted == j),]
      rep[, c("first_author","year","e_s1_o_s2", "study1", "e_unit", "o_unit", "mrbeta","mrse","mrlci","mruci","mrp")]
      label <- unique(paste0(j, " ", rep$e_unit, " on ", i))
      meta_res <- metagen(data = rep, TE = mrbeta, seTE = mrse, pval = mrp, level.ci = 0.95, studlab = study2, lower = mrlci, upper = mruci, method.tau = "PM", sm = "OR", hakn = FALSE, title = label)
      write.csv(meta_res, paste0("./Data analysis/Meta results/SameAnalyses/Autoimmune/", j, " ", i , ".csv"), quote = FALSE, row.names = FALSE)
      pdf(paste0("./Data analysis/Meta results/SameAnalyses/Autoimmune/", j, " ", i, ".pdf"), height = 4, width = 10)
      forest(meta_res, studlab = T, comb.fixed = F, comb.random = T, common = FALSE, digits = 3, leftcols = c("study1","studlab","first_author"), leftlabs = c("Exposure study","Outcome study","Author"))
      grid.text(label, .5, 0.8, gp=gpar(cex=1.4))
      dev.off()
      rm(label, rep, meta_res)
    }, error=function(e){cat("Error :",conditionMessage(e), "\n")})
  }
}
rm(outcome_list, exposure_list, i, j)
rm(autoimmune, autoimmune_check, same_autoimmune, same_autoimmune_check, same_autoimmune_new_check)

## Behaviour = 74
behaviour <- same[which(same$outcome_group == "Behaviour"),]
head(behaviour[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(behaviour$matches)
table(behaviour$outcome) ## Lots of outcomes
table(behaviour$exposure_formatted) ## One exposure
table(behaviour$study_design) ## Only two-sample 
table(unique(behaviour$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(behaviour$o_unit)) ## No unit so this is why they are the "same" analysis but if they've used the same outcome data, likely the same
behaviour_check <- behaviour[,c("ID","exposure_formatted","e_s1_o_s2", "outcome_def","outcome_og_gwas_case","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]

# This is all from one study where they used multiple definitions of the same trait from the same population so it's not the same exact analysis and there are no comparisons across studies
same_behaviour <- behaviour %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared
rm(behaviour, behaviour_check, same_behaviour)

## Bone = 260 
bone <- same[which(same$outcome_group == "Bone"),]
head(bone[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(bone$matches)
table(bone$outcome) ## 7 outcomes
table(bone$exposure_formatted) ## Lots of exposures
table(bone$study_design) ## Only two-sample 
table(unique(bone$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(bone$o_unit)) ## OR or log(OR) depending on outcome 
bone_check <- bone[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","analysis_notes","pval_threshold","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]

# Turns out there are 3 studies here (2419487002, 3278202937 and "Other sources")
# The information in 3278202937 says that the exposure is the same but they are either abundance / presence phenotypes but consistently labelled as NA because the unit was unclear
# The information from both 2419487002 and "Other sources" indicates that the exposure is always either "unknown genus" or "unknown family" but these are different phenotypes
same_bone <- bone %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared
rm(bone, bone_check, same_bone)

## Brain = 998
brain <- same[which(same$outcome_group == "Brain"),]
head(brain[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(brain$matches)
table(brain$outcome) ## Lots of outcomes
table(brain$exposure_formatted) ## Lots of exposures
table(brain$study_design) ## Only two-sample 
table(unique(brain$e_unit)) ## Abundance (SD)
table(unique(brain$o_unit)) ## OR or log(OR) depending on outcome 
brain_check <- brain[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(brain_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# This usually indicates a sensitivity analysis or that the exposure data are different (e.g., labelled "abundance" and "presence/absence" but the "e_unit" is NA for because it was consistently not clear)
# There are 5 studies here (1795779689, 1977124126, 2238214110, 3278202937, Other sources)
# The information from both 1795779689 and "Other sources" indicates that the exposure is always either "unknown genus" or "unknown family" but these are different phenotypes
# The 2238214110 study used a mixture of GWASs and this wasn't done in any other study
# The information in 3278202937 says that the exposure is the same but they are either abundance / presence phenotypes but consistently labelled as NA because the unit was unclear
same_brain <- brain %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared
rm(brain, brain_check, same_brain)

## Cancer = 384
cancer <- same[which(same$outcome_group == "Cancer"),]
head(cancer[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(cancer$matches)
table(cancer$outcome) ## 4 outcomes
table(cancer$exposure_formatted) ## Lots of exposures
table(cancer$study_design) ## Only two-sample 
table(unique(cancer$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(cancer$o_unit)) ## log(OR) 
cancer_check <- cancer[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(cancer_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# Just one ID, where the exposures look the same but they are in fact different (i.e., abundance / presence)
same_cancer <- cancer %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(cancer, cancer_check, same_cancer)

## Cardiovascular = 208
cardio <- same[which(same$outcome_group == "Cardiovascular"),]
head(cardio[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(cardio$matches)
table(cardio$outcome) ## 12 outcomes
table(cardio$exposure_formatted) ## Lots of exposures
table(cardio$study_design) ## Only two-sample 
table(unique(cardio$e_unit)) ## abundance (SD)
table(unique(cardio$o_unit)) ## OR, log(OR) or SD depending on outcome 
cardio_check <- cardio[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(cardio_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# There are 5 studies here (2547037187, 2662850683, 3278202937, 557846834, Other sources)
# The 2547037187 ID uses two different p-value thresholds so might be comparable with other studies
# The 2662850683 ID uses two different outcome GWASs but these are listed as "Unknown" because they didn't match with information on the IEU OpenGWAS
# The 3278202937 study uses different exposures but units were consistently unclear
# The information in 557846834 is not comparable with other studies
# The information from "Other sources" indicates that the exposure is always either "unknown genus" or "unknown family" but these are different phenotypes
same_cardio <- cardio %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(cardio, cardio_check, same_cardio)

## Eyes = 61
eyes <- same[which(same$outcome_group == "Eyes"),]
head(eyes[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(eyes$matches)
table(eyes$outcome) ## 2 outcomes
table(eyes$exposure_formatted) ## Lots of exposures
table(eyes$study_design) ## Only two-sample 
table(unique(eyes$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(eyes$o_unit)) ## OR  
eyes_check <- eyes[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(eyes_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# One study just using two different p-value thresholds
same_eyes <- eyes %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(eyes, eyes_check, same_eyes)

## Gastrointestinal tract = 100
gastro <- same[which(same$outcome_group == "Gastrointestinal tract"),]
head(gastro[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(gastro$matches)
table(gastro$outcome) ## 1 outcome
table(gastro$exposure_formatted) ## Lots of exposures
table(gastro$study_design) ## Only two-sample 
table(unique(gastro$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(gastro$o_unit)) ## log(OR)
gastro_check <- gastro[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(gastro_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# One study just using two different exposures but neither unit was clear
same_gastro <- gastro %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(gastro, gastro_check, same_gastro)

## Immune system = 194
immune <- same[which(same$outcome_group == "Immune system"),]
head(immune[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(immune$matches)
table(immune$outcome) ## 2 outcomes
table(immune$exposure_formatted) ## Lots of exposures
table(immune$study_design) ## Only two-sample 
table(unique(immune$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(immune$o_unit)) ## log(OR)  
immune_check <- immune[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(immune_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# One study just using two different exposures but neither unit was clear
same_immune <- immune %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(immune, immune_check, same_immune)

## Inflammation = 2
inflammation <- same[which(same$outcome_group == "Inflammation"),]
head(inflammation[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(inflammation$matches)
table(inflammation$outcome) ## 1 outcome
table(inflammation$exposure_formatted) ## 1 exposure
table(inflammation$study_design) ## Only two-sample 
table(unique(inflammation$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(inflammation$o_unit)) ## OR  
inflammation_check <- inflammation[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(inflammation_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# One study just using what the study labelled as the same exposure but not clear from the paper
same_inflammation <- inflammation %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(inflammation, inflammation_check, same_inflammation)

## Kidney = 184
kidney <- same[which(same$outcome_group == "Kidney"),]
head(kidney[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(kidney$matches)
table(kidney$outcome) ## 3 outcomes
table(kidney$exposure_formatted) ## Lots of exposures
table(kidney$study_design) ## Only two-sample 
table(unique(kidney$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(kidney$o_unit)) ## log(OR) or SD depending on outcome  
kidney_check <- kidney[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(kidney_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# The 3278202937 ID uses two different exposures but units were consistently unclear
# The information from "Other sources" indicates that the exposure is always either "unknown genus" or "unknown family" but these are different phenotypes
same_kidney <- kidney %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(kidney, kidney_check, same_kidney)

## Liver = 4
liver <- same[which(same$outcome_group == "Liver"),]
head(liver[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(liver$matches)
table(liver$outcome) ## 1 outcome
table(liver$exposure_formatted) ## 1 exposure
table(liver$study_design) ## Only two-sample 
table(unique(liver$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(liver$o_unit)) ## log(OR)
liver_check <- liver[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(liver_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# One study just using unknown exposure
same_liver <- liver %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(liver, liver_check, same_liver)

## Longevity = 20
longevity <- same[which(same$outcome_group == "Longevity"),]
head(longevity[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(longevity$matches)
table(longevity$outcome) ## 4 outcomes
table(longevity$exposure_formatted) ## 4 exposures
table(longevity$study_design) ## Only two-sample 
table(unique(longevity$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(longevity$o_unit)) ## log(OR)  
longevity_check <- longevity[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(longevity_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# Two studies but neither are comparable as one (2738712179) uses the same outcome but possibly different definitions and the other ("Other sources") has unknown exposures
same_longevity <- longevity %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(longevity, longevity_check, same_longevity)

## Metabolic health = 887
metab <- same[which(same$outcome_group == "Metabolic health"),]
head(metab[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(metab$matches)
table(metab$outcome) ## Lots of outcomes
table(metab$exposure_formatted) ## Lots of exposures
table(metab$study_design) ## One- and two-sample
table(unique(metab$e_unit)) ## Abundance (SD)
table(unique(metab$o_unit)) ## log(OR) or SD depending on the outcome
metab_check <- metab[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(metab_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# Four different studies (2136589463, 2179267930, 3278202937, Other sources)
# The information in both 2136589463 and 2179267930 are from one study so no other study would have done this the same way
# The information in 3278202937 uses two different exposures (though units were consistently not clear) and unknown outcome GWASs
# The information in "Other sources" uses unknown exposures so can't compare across studies
same_metab <- metab %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(metab, metab_check, same_metab)

## Nutrition = 72
nutrition <- same[which(same$outcome_group == "Nutrition"),]
head(nutrition[,c("exposure_formatted","study1","outcome","study2","exp_out","matches")])
table(nutrition$matches)
table(nutrition$outcome) ## 2 outcomes
table(nutrition$exposure_formatted) ## Lots of exposures
table(nutrition$study_design) ## Only two-sample 
table(unique(nutrition$e_unit)) ## No unit so difficult to know whether these are the exact same but if they've used the same exposure study, likely the same
table(unique(nutrition$o_unit)) ## SD
nutrition_check <- nutrition[,c("ID","exposure_formatted","e_s1_o_s2","exposure_unit","e_unit","o_unit","outcome_def","outcome_study_case","outcome_study_control","outcome_og_gwas_case","outcome_og_gwas_control","outcome_n_diff","pval_threshold","analysis_notes","other_gwasinfo","main_method","replication","mrbeta","mrse","mrlci","mruci","mrp")]
table(unique(nutrition_check$ID))

# Want to remove the ones where the analysis has been done in the same paper (because authors never undertook the exact same analysis with the exact same outcome data in the same paper unless it was a sensitivity analysis)
# Two IDs (1599546107 and 1828507192)
# Information in 1599546107 is from a table where the two exposures look the same but they might not be
# Information in 1828507192 is using two different outcome datasets so not the same thing
same_nutrition <- nutrition %>%
  group_by(e_s1_o_s2) %>%
  filter(n_distinct(ID) > 1) %>%
  ungroup() ## 0 can be compared 
rm(nutrition, nutrition_check, same_nutrition)

## Save a copy of the data that were analysed
write.xlsx(same_autoimmune_new, "./Data analysis/Same_Analysis_AfterMeta.xlsx", asTable = TRUE, overwrite = TRUE, keepNA = TRUE)
rm(same, same_autoimmune_new)

############################################################ END ############################################################

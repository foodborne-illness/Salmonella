# Salmonella - Enteritidis

Foodborne illnesses caused by a variety of risk factors, such as contaminated foods and beverages, inadequate cooking, improper hot/cold holding temperatures, contaminated equipment, and poor personal hygiene, continue to pose a major global public health problem. Of over 2500 Salmonella serotypes identified, serovars Enteritidis, Newport, Typhimurium, Typhi, Choleraesuis, Paratyphi A, and Paratyphi C from Salmonella subspecies I are responsible for most human Salmonella infections.

This project aims to identify trends and associated high-risk genotypes in Salmonella-relatedfoodborne illness outbreaks in the US and predict clinical cases with identifiedgenotypes, seasonality, and regionality. Generalized linear autoregressive moving average (GLARMA) model was utilized to predict the counts of clinically exposed cases of Salmonella Enteritidis in the US, with a combination of baseline predictors including high-risk AMR genotypes, high-risk stress genotypes, top ten SNP clusters with most isolates, human-related sources and US regions. 

## Data Source and Collection

We are using the Salmonella Enteritidis data from [National Center for Biotechnology Information’s(NCBI) Pathogens Detection database](https://www.ncbi.nlm.nih.gov/pathogens/) in our study to explore the high-risk isolates of salmonella and predict the probability of different serovars of salmonella most likely to cause an outbreak at specific times. 

The NCBI Pathogens detection system contains comprehensive information on bacterial and fungal pathogen genomic sequences. Numerous sequenced samples from clinical cases and environmental and food sources are submitted by public health institutions and researchers worldwide to NCBI for active, real-time monitoring of pathogens and potential foodborne illness outbreaks. NCBI analyzes and compares these sequences to those in the NCBI Pathogens detection database. By identifying the related sequences, the NCBI can detect closely related isolates and thus investigate their concerning potential pathogen-related outbreaks.

The [system](https://www.ncbi.nlm.nih.gov/pathogens/about/) helps public health researchers to identify transmission chains in potential foodborne illness outbreaks by clustering related pathogen genome sequences and allows them to track resistance genes by providing information on antimicrobial resistance, stress response, and virulence genes found in bacterial genomic sequences using AMRFinderPlus. 

For complete dataset, see the [link](https://drive.google.com/file/d/1kmlia5vTlS_2r9Hp5TpCGLK7Uq3U02_p/view?usp=sharing).


## About this repository

** Details about the modeling process analysis can be found in the  `Report` folder.

** R codes are written in R Studio and stored in the `EDA` and `Modeling` folder.

** The `Data` folder includes the meta data from NCBI. For complete dataset we use for modeling, see the [link](https://drive.google.com/file/d/1kmlia5vTlS_2r9Hp5TpCGLK7Uq3U02_p/view?usp=sharing).

** The `Plots` folder contains figures for the report.

** Other supplementary materials like the variable descriptions are provided in `supplement` folder.

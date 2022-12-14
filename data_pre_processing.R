############################# Data Pre Processing ##############################

library(tidyverse)

project.data <- read.csv("data_complete.csv")
# remove columns that are repeating!
project.data <- project.data %>% 
  filter(collection.date != "Collection date")

############################# LOCATION: USA ONLY ##############################
# Obtain vector of all locations that include USA
locations.USA <- unique(project.data$location)
str.USA <- locations.USA[grep("USA", locations.USA, ignore.case = F)]

# Subset data to only the USA
data.USA <- project.data %>% 
  filter(location %in% str.USA)

# Replace "" with NA
data.USA[data.USA == ""] <- NA

extract_location <- function(location_str) {
  #' Returns a string that removes the country's name
  #' @param location_str, location represented by a string
  #' @return location, string of location
  start <- unlist(gregexpr(":", location_str))[1] + 1
  stop <- nchar(location_str)
  location <- substr(location_str, start, stop)
  location <- trimws(location)
  return(location)
}

data.USA$state <- sapply(data.USA$location, extract_location)

abbreviate_state <- function(state_str) {
  #' Takes a string representing a state's full name and returns its two letter
  #' abbreviated state name 
  #' @param state_str, string of a state's full name
  #' @return state_abb, two letter abbreviation of the state's name if a valid
  #' state name and the original string if it is not a valid state name
  check <- grepl(state_str, state.name, ignore.case = T)
  idx <- which(grepl(state_str, state.name, ignore.case = T))
  state_abb <- ifelse(sum(check) == 1, state.abb[idx], state_str)
  return(state_abb)
}

# Filter weird cases that cannot be used with abbreaviate_state
data.USA <- data.USA %>% 
  mutate(state = case_when(
    grepl('New York', state, ignore.case = T) ~ "NY",
    grepl('Indiana|IN', state, ignore.case = T) ~ "IN",
    grepl('Minneapolis|Lansing', state, ignore.case = T) ~ "MI",
    grepl('North Carolina', state, ignore.case = T) ~ "NC",
    grepl('Oklahoma|Tulsa', state, ignore.case = T) ~ "OK",
    grepl('MO', state, ignore.case = T) ~ "MO",
    grepl('Texas', state, ignore.case = T) ~ "TX",
    grepl('Pennsylvania|Philadelphia', state, ignore.case = T) ~ "PA",
    grepl('Boston', state, ignore.case = T) ~ "MA",
    grepl('California|San Francisco', state, ignore.case = T) ~ "CA",
    grepl('BIFSCo', state, ignore.case = T) ~
      paste0("BIFSCo Region ",substr(state, nchar(state), nchar(state))),
    grepl('Colorado|CO', state, ignore.case = T) ~ "CO",
    grepl('Florida|Gainesville', state, ignore.case = T) ~ "FL",
    grepl('HI', state, ignore.case = T) ~ "HI",
    grepl('New Jersy', state, ignore.case = T) ~ "NJ",
    grepl('GA', state, ignore.case = T) ~ "GA",
    grepl('Nebraska', state, ignore.case = T) ~ "NE",
    grepl('TN', state, ignore.case = T) ~ "TN",
    grepl('Nebraska', state, ignore.case = T) ~ "NE",
    grepl('Kansas', state, ignore.case = T) ~ "KS",
    grepl('or|OR', state, ignore.case = T) ~ "OR",
    TRUE ~ state
  ))

# Obtain the state abbreviations for states that are not yet abbreviated
data.USA$state[nchar(data.USA$state) > 2] <- 
  sapply(data.USA$state[nchar(data.USA$state) > 2], abbreviate_state)

# List of terms that are not states:
# (USA, BIFSCo Region, Midwest, Western Region, State)
uncategorized <- unique(data.USA$state)[nchar(unique(data.USA$state)) > 2]

# Create a region column corresponding to state location
data.USA <- data.USA %>% 
  mutate(region = case_when(
    grepl('CT|ME|MA|NH|NJ|NY|PA|RI|VT|DC|DE|MD',
          state, ignore.case = T) ~ "Northeast",
    grepl('AL|AR|FL|GA|KY|LA|MS|NC|SC|TN|VA|WV', 
          state, ignore.case = T) ~ "Southeast",
    grepl('IL|IN|IA|KS|MI|MN|MO|NE|ND|OH|SD|WI|MidWest',
          state, ignore.case = T) ~ "Midwest",
    grepl('WA|OR|ID|MT|WY|NV|UT|CO|CA|Western',
          state, ignore.case = T) ~ "West",
    grepl('AZ|NM|OK|TX', state, ignore.case = T) ~ "Southwest",
    grepl('AK|HI', state, ignore.case = T) ~ "Pacific",
    TRUE ~ "other"
  ))

############################# ISOLATION SOURCE ################################
# Re-encode categories for isolation source
data.USA <- data.USA %>% 
  mutate(isolation.bin = case_when(
    is.na(isolation.source) ~ "Missing",
    grepl('human', isolation.source, ignore.case = T) ~ "Human",
    grepl('feces|manure|fecal|scat|stool', 
          isolation.source, ignore.case = T) ~ "Feces",
    grepl('chicken|gallus', isolation.source, ignore.case = T) ~ "Chicken",
    grepl('beef|bovine|cattle|bos taurus|cow|calf|veal|chuck', 
          isolation.source, ignore.case = T) ~ "Beef",
    grepl('pork|porcine|pig|swine', isolation.source, ignore.case = T) ~ "Pig",
    grepl('boar|sus scrofa', isolation.source, ignore.case = T) ~ "Boar",
    grepl('sheep|lamb|ovis aries', 
          isolation.source, ignore.case = T) ~ "Sheep",
    grepl('turkey|meleagris gallopavo', 
          isolation.source, ignore.case = T) ~ "Turkey",
    grepl('duck', isolation.source, ignore.case = T) ~ "Duck",
    grepl('poultry', isolation.source, ignore.case = T) ~ "Poultry",
    grepl('avian|bird', isolation.source, ignore.case = T) ~ "Bird",
    grepl('horse|equine|equus', isolation.source, ignore.case = T) ~ "Horse",
    grepl('water|stream|pond', isolation.source, ignore.case = T) ~ "Water",
    grepl('environmental|environment',
          isolation.source, ignore.case = T) ~ "Environmental",
    grepl('nut|pistachio|almond', isolation.source, ignore.case = T) ~ "Nuts",
    grepl('egg|yolk', isolation.source, ignore.case = T) ~ "Egg",
    grepl('soy', isolation.source, ignore.case = T) ~ "Soy",
    grepl('dog|canis|pet food|cat food|kibble|cat|dog', 
          isolation.source, ignore.case = T) ~ "Pet",
    grepl('fish|tuna|salmon|oyster|seafood|ocean|shrimp|tilapia|shellfish|
          crustaceans|mollusks|octopus', 
          isolation.source, ignore.case = T) ~ "Seafood",
    grepl('soil|sediment', isolation.source, ignore.case = T) ~ "Soil",
    grepl('urine', isolation.source, ignore.case = T) ~ "Urine",
    grepl('bone', isolation.source, ignore.case = T) ~ "Bone",
    grepl('milk', isolation.source, ignore.case = T) ~ "Milk",
    grepl('cheese|dairy|brie', isolation.source, ignore.case = T) ~ "Dairy",
    grepl('swab', isolation.source, ignore.case = T) ~ "swab",
    grepl('colon|cecal|intestine|rectal|cecum', 
          isolation.source, ignore.case = T) ~ "Lower Digestive",
    grepl('feed', isolation.source, ignore.case = T) ~ "Feed",
    grepl('produce|bean', isolation.source, ignore.case = T) ~ "Produce",
    grepl('compost', isolation.source, ignore.case = T) ~ "Compost",
    grepl('vegetable|leafy|green|cucumber|spinach|lettuce|kale', 
          isolation.source, ignore.case = T) ~ "Vegetable",
    grepl('liver', isolation.source, ignore.case = T) ~ "Liver",
    grepl('blood', isolation.source, ignore.case = T) ~ "Blood",
    grepl('fluid|tissue|bile|abscess|absess|abcess', 
          isolation.source, ignore.case = T) ~ "Body",
    grepl('herb|leaf|kratom|tree|plant|grain|lemon|orange|citrus|fruit', 
          isolation.source, ignore.case = T) ~ "Plant",
    grepl('lizard|bearded dragon|iguana|aligator|alligator|snake|reptile',
          isolation.source, ignore.case = T) ~ "Reptile",
    grepl('compost', isolation.source, ignore.case = T) ~ "Compost",
    grepl('ready to eat', 
          isolation.source, ignore.case = T) ~ "Ready to Eat Foods",
    grepl('lung', isolation.source, ignore.case = T) ~ "Lung",
    grepl('raw', isolation.source, ignore.case = T) ~ "Raw Products",
    grepl('other|unknown|missing',
          isolation.source, ignore.case = T) ~ "Missing",
    TRUE ~ isolation.source
  )) %>% 
  # Create broader encodings
  mutate(food_type = case_when(
    grepl('seafood|fish', isolation.bin, ignore.case = T) ~ "Aquatic",
    grepl('dairy|milk|cheese', isolation.bin, ignore.case = T) ~ "Dairy",
    grepl('beef|pork|sheep|goat|boar|pig|meat', 
          isolation.bin, ignore.case = T) ~ "Meat",
    grepl('poultry|chicken|turkey|duck|bird', 
          isolation.bin, ignore.case = T) ~ "Poultry",
    grepl('plant|oil|sugar|vegetable|fruit|nut|grain|seed|nuts|produce',
          isolation.bin, ignore.case = T) ~ "Plant",
    grepl('egg', isolation.bin, ignore.case = T) ~ "Egg",
    TRUE ~ isolation.bin
  ))

############################# CLINICAL DATA ONLY ##############################
# Filter data for Enteritidis in clinical sources after 2010
clinical.USA <- data.USA %>% 
  filter(isolation.type == "clinical") %>%
  filter(serovar %in% c("enteritidis", "Enteritidis"))  %>%
  filter(as.numeric(substr(collection.date,1,4)) >= 2010) %>%
  mutate(year = substr(collection.date,1,4), 
         month = substr(collection.date,6,7), 
         day = substr(collection.date,9,10)) %>%
  mutate(time = ifelse(month == "", NA,
                       zoo::as.yearmon(paste(year, month), "%Y %m")))

# Create bins based on isolation source
clinical.USA <- clinical.USA %>%  
  mutate(clinical.bins = case_when(
    grepl('abscess', isolation.source, ignore.case = T) ~ "Abscess",
    grepl('blood|wound', isolation.source, ignore.case = T) ~ "Blood",
    grepl('|feces|stool', isolation.source, ignore.case = T) ~ "Feces",
    grepl('urine', isolation.source, ignore.case = T) ~ "Urine",
    grepl('fluid|CSF|cyst|synovial',
          isolation.source, ignore.case = T) ~ "Fluid",
    grepl('chicken', isolation.source, ignore.case = T) ~ "Chicken",
    grepl('throat|sputum|aspirate', 
          isolation.source, ignore.case = T) ~ "Respiratory",
    grepl('body|human|groin|bone|hip|aneurysm sac|nasal|aorta|arm|chest|tissue|
          swab|rectal|rectum', isolation.source, ignore.case = T) ~ "Body",
    TRUE ~ "other"
  )) 

############################# AMR GENOTYPE ####################################
gene.name <- function(str) {
  #' Extracts the name of the genotype without additional information
  #' @param str, string corresponding to AMR genotype
  #' @return complete_gene, string of only the genotype
  start <- 1
  stop <- unlist(gregexpr("=", str))[1] - 1
  complete_gene <- substr(str, start, stop)
  return(complete_gene)
}

complete.gene.name <- function(str) {
  #' Extracts the name of the genotype without additional information
  #' @param str, string corresponding to AMR genotype and its category
  #'such as complete, partial, point, partial end of contig.
  #' @return genes, list of string of complete genotype
  splitted <- str_split(str, ",")
  splitted <- unlist(splitted)
  complete_genes_only <- splitted[grep("=COMPLETE", splitted)]
  genes <- sapply(complete_genes_only, gene.name)
  return(genes)
}

amr.genotypes <- clinical.USA$amr.genotypes
all.amr.genotypes <- sapply(amr.genotypes, complete.gene.name)

# Create table of all possible AMR geneotypes
table.AMR.genotypes <- table(unlist(all.amr.genotypes))
top.genotypes <- sort(table.AMR.genotypes[table.AMR.genotypes > 50])
# Vector of most prevalent AMR genotypes 
genotype_names <- names(top.genotypes)

stress.genotypes <- clinical.USA$stress.genotypes
all.stress.genotypes <- sapply(stress.genotypes, complete.gene.name)

# Create indicators for presence of most prevalent AMR genotypes
clinical.USA$mdsA <- ifelse(grepl("mdsA=COMPLETE", amr.genotypes), 1, 0)
clinical.USA$mdsB <- ifelse(grepl("mdsB=COMPLETE", amr.genotypes), 1, 0)
clinical.USA$blaTEM1 <- ifelse(grepl("blaTEM-1=COMPLETE", amr.genotypes), 1, 0)
clinical.USA$tetA <- ifelse(grepl("tet(A)=COMPLETE", amr.genotypes), 1, 0)
clinical.USA$aph6Id <- ifelse(grepl("aph(6)-Id=COMPLETE", amr.genotypes), 1, 0)
clinical.USA$sul2 <- ifelse(grepl("sul2=COMPLETE", amr.genotypes), 1, 0)
clinical.USA$aph3 <- ifelse(grepl("aph(3'')-Ib=COMPLETE", amr.genotypes), 1, 0)

# Obtain the top five AMR genotype groupings
top5.clusters.amr <- clinical.USA %>% 
  filter(year %in% c(2013:2017)) %>% 
  group_by(amr.genotypes) %>%
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  pull(amr.genotypes)

# Create indicators for presences of the top five AMR genotype groupings
clinical.USA$amr.AB <-  ifelse(top5.clusters.amr[1] == amr.genotypes, 1, 0)
clinical.USA$amr.gnAB <- ifelse(top5.clusters.amr[2] == amr.genotypes, 1, 0)
clinical.USA$amr.bAB <- ifelse(top5.clusters.amr[3] == amr.genotypes, 1, 0)
clinical.USA$amr.gyAB <- ifelse(top5.clusters.amr[4] == amr.genotypes, 1, 0)
clinical.USA$amr.ggAB <- ifelse(top5.clusters.amr[5] == amr.genotypes, 1, 0)

# Obtain the top stress clusters
top.stress.clusters <- clinical.USA %>% 
  filter(year %in% c(2010:2017)) %>% 
  group_by(stress.genotypes) %>%
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n > 10) %>% 
  pull(stress.genotypes)

# Create indicators for presences of the top five stress genotype groupings
stress.genotypes <- clinical.USA$stress.genotypes
clinical.USA$stress.1 <-  ifelse(top.stress.clusters[1] == stress.genotypes, 1, 0)
clinical.USA$stress.2 <- ifelse(top.stress.clusters[2] == stress.genotypes, 1, 0)
clinical.USA$stress.3 <- ifelse(top.stress.clusters[3] == stress.genotypes, 1, 0)
clinical.USA$stress.4 <- ifelse(top.stress.clusters[4] == stress.genotypes, 1, 0)
clinical.USA$stress.5 <- ifelse(top.stress.clusters[5] == stress.genotypes, 1, 0)


############################# SNP CLUSTERS ####################################
# obtain top 10 SNP clusters
top10.snp <- clinical.USA %>% 
  filter(year %in% c(2013:2017),
         month != "") %>% 
  group_by(snp.cluster) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% head(10) %>% 
  pull(snp.cluster)

# Replace NA with "Unknown"
clinical.USA$snp.cluster[is.na(clinical.USA$snp.cluster)] <- "Unknown"

# Create indicators for top ten SNP clusters
clinical.USA$snp1 <-  ifelse(top10.snp[1] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp2 <- ifelse(top10.snp[2] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp3 <- ifelse(top10.snp[3] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp4 <- ifelse(top10.snp[4] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp5 <- ifelse(top10.snp[5] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp6 <- ifelse(top10.snp[6] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp7 <- ifelse(top10.snp[7] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp8 <- ifelse(top10.snp[8] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp9 <- ifelse(top10.snp[9] == clinical.USA$snp.cluster, 1, 0)
clinical.USA$snp10 <- ifelse(top10.snp[10] == clinical.USA$snp.cluster, 1, 0)

############################## TEST/TRAIN SPLIT ###############################
# Create training data set: 2013-01 to 2017-08
training.data <- clinical.USA %>% 
  filter(year %in% c(2013:2016) & month != "" | 
           year == 2017 & month %in% c("01", "02", "03", "04", "05",
                                       "06", "07", "08"))

# Create test data set: 2017-09 to 2018-02
test.data <- clinical.USA %>% 
  filter(year == 2017 & month %in% c("09", "10", "11", "12") |
           year == 2018 & month %in% c("01","02"))


# Prepare design matrix
clinical.M <- training.data %>% 
  group_by(year, month) %>% 
  summarize(n = n()) %>% 
  arrange(-desc(year))

# Create indicator vector for month
v.0 <- vector("numeric", length = 12)
m.data <- as.numeric(clinical.M$month)

I.month <- function(month) {
  #' Returns a vector of indicator values corresponding to month
  #' @param month, month
  #' @return v.0, vector of indicator values
  v.0[month] <- 1
  return(v.0)
}

# Create matrix of indicator variables for month
month.mat <- t(sapply(m.data, I.month))
colnames(month.mat) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
datalen <- length(m.data)

# Obtain proportions based on the presence of AMR genotype clusters, SNP
# clusters, and stress genotypes
clinical.MRSG <- training.data %>% 
  group_by(year, month) %>% 
  summarize(n = n(),
            p.AB = mean(amr.AB),
            p.gnAB = mean(amr.gnAB),
            p.bAB = mean(amr.bAB),
            p.gyAB = mean(amr.gyAB), 
            p.ggAB = mean(amr.ggAB),
            p.snp1 = mean(snp1),
            p.snp2 = mean(snp2),
            p.snp3 = mean(snp3),
            p.snp4 = mean(snp4),
            p.snp5 = mean(snp5),
            p.snp6 = mean(snp6),
            p.snp7 = mean(snp7),
            p.snp8 = mean(snp8),
            p.snp9 = mean(snp9),
            p.snp10 = mean(snp10),
            p.stress1 = mean(stress.1),
            p.stress2 = mean(stress.2),
            p.stress3 = mean(stress.3),
            p.stress4 = mean(stress.4),
            p.stress5 = mean(stress.5))

# Prepare design matrix by including proportions for region
regions <- training.data %>% 
  group_by(year, month, region) %>%
  summarize(n = n()) %>% 
  pivot_wider(names_from = region, values_from = n, values_fill = 0) %>% 
  ungroup() %>% 
  select(-c(year, month))
total <- apply(regions, 1, sum)
regions.pct <- regions / total

# Create data frame with year, month, proportion by AMR genotype cluster, SNP
# cluster, region, and indicator for month
clinical.MRSG <- data.frame(X = rep(1, datalen), clinical.MRSG,
                            regions.pct, month.mat)

# Obtain counts by clinical source by year and month in wide format
clinical.source <- training.data %>% 
  group_by(year, month, clinical.bins) %>%
  summarize(n = n()) %>% 
  pivot_wider(names_from = clinical.bins, 
              values_from = n, values_fill = 0) %>% 
  ungroup() %>% 
  summarize(Blood = Blood,
            Feces = Feces,
            Other = other + Abscess)

n <- training.data %>% 
  group_by(year, month) %>% 
  summarize(n = n()) %>% pull(n)

clinical.source <- clinical.source / n

# Create data frame with year, month, proportion by AMR genotype cluster, SNP
# cluster, region, indicator for month, and clinical source
clinical.train <- data.frame(clinical.MRSG, clinical.source)

############################# Data Pre Processing ##############################

library(tidyverse)

project.data <- read.csv("data_complete.csv")

# remove columns that are repeating!
project.data <- project.data %>% 
  filter(collection.date != "Collection date")

# obtain vector of all locations that include USA
str.USA <- unique(project.data$location)[grep("USA", unique(project.data$location), ignore.case = F)]
# subset data to only USA
data.USA <- project.data %>% 
  filter(location %in% str.USA)

# replace "" with NA
data.USA[data.USA == ""] <- NA

# re encode categories for isolation source
data.USA <- data.USA %>% 
  mutate(isolation.bin = case_when(
    is.na(isolation.source) ~ "Missing",
    grepl('human', isolation.source, ignore.case = T) ~ "Human",
    grepl('feces|manure|fecal|scat|stool', isolation.source, ignore.case = T) ~ "Feces",
    grepl('chicken|gallus', isolation.source, ignore.case = T) ~ "Chicken",
    grepl('beef|bovine|cattle|bos taurus|cow|calf|veal|chuck', isolation.source, ignore.case = T) ~ "Beef",
    grepl('pork|porcine|pig|swine', isolation.source, ignore.case = T) ~ "Pig",
    grepl('boar|sus scrofa', isolation.source, ignore.case = T) ~ "Boar",
    grepl('sheep|lamb|ovis aries', isolation.source, ignore.case = T) ~ "Sheep",
    grepl('turkey|meleagris gallopavo', isolation.source, ignore.case = T) ~ "Turkey",
    grepl('duck', isolation.source, ignore.case = T) ~ "Duck",
    grepl('poultry', isolation.source, ignore.case = T) ~ "Poultry",
    grepl('avian|bird', isolation.source, ignore.case = T) ~ "Bird",
    grepl('horse|equine|equus', isolation.source, ignore.case = T) ~ "Horse",
    grepl('water|stream|pond', isolation.source, ignore.case = T) ~ "Water",
    grepl('environmental|environment', isolation.source, ignore.case = T) ~ "Environmental",
    grepl('nut|pistachio|almond', isolation.source, ignore.case = T) ~ "Nuts",
    grepl('egg|yolk', isolation.source, ignore.case = T) ~ "Egg",
    grepl('soy', isolation.source, ignore.case = T) ~ "Soy",
    grepl('dog|canis', isolation.source, ignore.case = T) ~ "Dog",
    grepl('fish|tuna|salmon|oyster|seafood|ocean|shrimp|tilapia', isolation.source, ignore.case = T) ~ "Seafood",
    grepl('soil|sediment', isolation.source, ignore.case = T) ~ "Soil",
    grepl('urine', isolation.source, ignore.case = T) ~ "Urine",
    grepl('bone', isolation.source, ignore.case = T) ~ "Bone",
    grepl('milk', isolation.source, ignore.case = T) ~ "Milk",
    grepl('cheese|dairy|brie', isolation.source, ignore.case = T) ~ "Dairy Other",
    grepl('swab', isolation.source, ignore.case = T) ~ "swab",
    grepl('colon|cecal|intestine|rectal|cecum', isolation.source, ignore.case = T) ~ "Lower Digestive",
    grepl('feed', isolation.source, ignore.case = T) ~ "Feed",
    grepl('produce', isolation.source, ignore.case = T) ~ "Produce",
    grepl('compost', isolation.source, ignore.case = T) ~ "Compost",
    grepl('pet food|cat food|kibble', isolation.source, ignore.case = T) ~ "Pet food",
    grepl('vegetable|leafy|green|cucumber|spinach|lettuce|kale', isolation.source, ignore.case = T) ~ "Vegetable",
    grepl('liver', isolation.source, ignore.case = T) ~ "Liver",
    grepl('blood', isolation.source, ignore.case = T) ~ "Blood",
    grepl('fluid|tissue|bile|abscess|absess|abcess', isolation.source, ignore.case = T) ~ "Bodily Stuff",
    grepl('herb|leaf|kratom|tree|plant', isolation.source, ignore.case = T) ~ "Plant",
    grepl('lizard|bearded dragon|iguana', isolation.source, ignore.case = T) ~ "Lizard",
    grepl('aligator|alligator', isolation.source, ignore.case = T) ~ "Alligator",
    grepl('snake|reptile', isolation.source, ignore.case = T) ~ "Reptile",
    grepl('compost', isolation.source, ignore.case = T) ~ "Compost",
    grepl('ready to eat', isolation.source, ignore.case = T) ~ "Ready to Eat Foods",
    grepl('lung', isolation.source, ignore.case = T) ~ "Lung",
    grepl('raw', isolation.source, ignore.case = T) ~ "Raw Products",
    grepl('other|unknown|missing', isolation.source, ignore.case = T) ~ "Missing",
    
    TRUE ~ "other"
    
  )) 

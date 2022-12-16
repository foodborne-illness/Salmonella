########################## PHP 2550 Data Exploration ##########################

library(tidyverse)
library(kableExtra)

# Read in the data
project.data <- read.csv("~/Desktop/fall 2022/PHP 2550 [PDA]/project/data_complete.csv")


######--------------------------- Missing Data --------------------------######

# find number of NA values in data
num.NA <- apply(project.data, 2, function(x){return(sum(is.na(x)))})
# find number of values represented by an empty string
num.missing <- apply(project.data, 2, function(x){return(sum( x == ""))})
# replace NA with 0
num.missing[is.na(num.missing)] <- 0

# create data frame with the number of NA's, missing denoted by "", and number of
# entries not missing in the data set
breakdown.by.missingness <- data.frame(col = names(num.NA),
                                       num.NA,
                                       num.missing,
                                       num.present = nrow(project.data) - num.NA - num.missing)

# create a breakdown of missing values by proportion missing
percent.missing.grouped <- breakdown.by.missingness %>% 
  mutate(prop.present = num.present / nrow(project.data),
         missing_bin = case_when(
           prop.present <= 0.1 ~ "Missing 90% - 100% of entries",
           prop.present <= 0.2 ~ "Missing 80% - 90% of entries",
           prop.present <= 0.3 ~ "Missing 70% - 80% of entries",
           prop.present <= 0.4 ~ "Missing 60% - 70% of entries",
           prop.present <= 0.5 ~ "Missing 50% - 60% of entries",
           prop.present <= 0.6 ~ "Missing 40% - 50% of entries",
           prop.present <= 0.7 ~ "Missing 30% - 40% of entries",
           prop.present <= 0.8 ~ "Missing 20% - 30% of entries",
           prop.present <= 0.9 ~ "Missing 10% - 20% of entries",
           prop.present <= 1 ~ "Missing 0% - 10% of entries"
           )
         ) 

# extract variable names that correspond to a missing proportion
cols.1 <- percent.missing.grouped$col[percent.missing.grouped$missing_bin == "Missing 0% - 10% of entries"]
cols.2 <- percent.missing.grouped$col[percent.missing.grouped$missing_bin == "Missing 10% - 20% of entries"]
cols.3 <- percent.missing.grouped$col[percent.missing.grouped$missing_bin == "Missing 40% - 50% of entries"]
cols.4 <- percent.missing.grouped$col[percent.missing.grouped$missing_bin == "Missing 60% - 70% of entries"]
cols.5 <- percent.missing.grouped$col[percent.missing.grouped$missing_bin == "Missing 70% - 80% of entries"]
cols.6 <- percent.missing.grouped$col[percent.missing.grouped$missing_bin == "Missing 90% - 100% of entries"]

######---------------------------- Serotypes ----------------------------######

# look at counts by serovar
by.serovar <- project.data %>% 
  group_by(serovar) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

# look at counts by serotype
by.serotype <- project.data %>% 
  group_by(Serotype) %>% 
  summarize(Counts = n(),
            `Proportion of All Cases` = paste0(round(Counts / nrow(project.data) * 100, 2), "%")) %>% 
  arrange(desc(Counts))

# compute the number of missing values by serovar and serotype
empty.serovar <- sum(project.data$serovar == "")
empty.serotype <- sum(project.data$Serotype == "")

# extract first ten serotypes with the most number of cases
top.ten.serotypes <- by.serotype %>% 
  head(10)

# create table compatible with markdown
top.ten.serotypes.tb <- kable(top.ten.serotypes, 
                            caption = "Salmonella case counts by serotype",
                            align = c("l", "r", "r")) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))

# find total number of cases accounted for in top ten serotypes
total.top.ten <- sum(top.ten.serotypes$Counts)

###############################################################################

######------------------------- Peak occurrences ------------------------######
# overall case counts from 1900 to 2021
case.counts.by.year <- project.data %>%
  mutate(year = substr(collection_date, 1, 4)) %>%
  group_by(year) %>%
  summarize(Count = n()) %>%
  filter(year != "" & year != "miss") %>%
  mutate(year = as.numeric(year))

# case counts for the top three serotypes 
top.three.str.serotypes <- by.serotype %>% head(3) %>% pull(Serotype)
top.three.over.time <-  project.data %>% 
  filter(Serotype %in% top.three.str.serotypes) %>%
  mutate(year = substr(collection_date, 1, 4)) %>% 
  filter(year != "" | year == "miss") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year, Serotype) %>% 
  summarize(Count = n())

# plot for overall and top three serotypes case counts from 1990 to 2021
p.top.three.sero <- ggplot() +
  geom_line(data = case.counts.by.year, 
            aes(x = year, y = Count, colour = "Overall")) +
  geom_line(data = top.three.over.time,
            aes(x = year, y = Count, group = Serotype, colour = Serotype)) +
  labs(title = "Figure 2: Salmonella cases for the top three serotypes from 1990 to 2021") +
  xlab("Year") +
  theme_minimal() + 
  theme(legend.position = 'bottom', legend.direction = "horizontal") +
  xlim(c(1990, 2021)) +
  scale_colour_discrete(name = "Serovar", 
                        breaks=c("Overall", top.three.str.serotypes))

# string for the top 10 contributing serotypes
top.ten.str.serotypes <- by.serotype %>% head(10) %>% pull(Serotype)

# find the peak number of occurrences for all serotypes
peak.occurences <- project.data %>% 
  mutate(year = substr(collection_date, 1, 4)) %>% 
  filter(year != "" | year == "miss") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year, Serotype) %>% 
  summarize(Count = n()) %>% 
  ungroup() %>% 
  group_by(Serotype) %>% 
  mutate(max = max(Count)) %>% 
  filter(Count == max) %>% 
  arrange(desc(Count)) %>% 
  select(Serotype,
         Year = year,
         Count)

# histogram of peak number of occurences by year
p.year.peaks <- ggplot(peak.occurences) +
  geom_histogram(aes(Year)) +
  ylab("Count") + 
  theme_minimal() +
  labs(title = "Figure 1: Distribution of peaks in cases by serotype")

# table showing number of peak cases
peak.cases.by.serotype <- peak.occurences %>%
  group_by(Year) %>%
  summarize(total = sum(Count)) %>%
  arrange(desc(total))

# find the peak number of occurrences by month
peak.month <- project.data %>% 
  filter(Serotype %in% top.ten.str.serotypes[1:3],
         nchar(collection_date) == 10) %>%
  mutate(month = as.numeric(substr(collection_date, 6,7))) %>% 
  group_by(Serotype, month) %>% 
  summarize(n = n())

# plot for the peak occurrences by month for the top three serotypes
p.top.three.sero.month <- ggplot(peak.month) +
  geom_line(aes(x = month, y = n, group = Serotype, colour = Serotype)) +
  labs(title = "Salmonella cases for the top three serotypes across month") +
  xlab("Year") +
  theme_minimal() + 
  theme(legend.position = 'bottom', legend.direction = "horizontal") + 
  scale_colour_discrete(name = "Serovar", breaks=top.ten.str.serotypes[1:3]) +
  scale_x_discrete("Month", limits = c(1:12)) +
  scale_y_discrete("Count", limits = seq(0, 800, 200))

# find the month which the top ten serotypes experience a peak in cases
peak.month.occurences <- project.data %>% 
  filter(Serotype %in% top.ten.str.serotypes, 
         nchar(collection_date) == 10) %>%
  mutate(month = as.numeric(substr(collection_date, 6,7))) %>% 
  group_by(month, Serotype) %>% 
  summarize(Count = n()) %>% 
  ungroup() %>% 
  group_by(Serotype) %>% 
  mutate(max = max(Count),
         total = sum(Count),
         prop = max/total) %>% 
  filter(Count == max) %>% 
  arrange(desc(Count)) %>% 
  select(Serotype,
         Month = month,
         Count,
         `Proportion of cases` = prop)

# create table of listing serotype, the month cases peak at and their counts
peak.month.occurences.tb <- kable(peak.month.occurences,
         caption = "Peak occurences by month grouped by serovar from 1990 to 2021") %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))

###############################################################################
######------------------------- Isolation types -------------------------######

# write out the months of the year
months.str <- c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December")

# obtain counts by grouping data by Isolation type and month
epi.type.peak.month <- project.data %>% 
  filter(nchar(collection_date) == 10) %>%
  mutate(month = as.numeric(substr(collection_date, 6,7)),
         Month = as.factor(months.str[month])) %>% 
  group_by(epi_type, Month) %>% 
  summarize(n = n())

# revel the months so that they are in order
epi.type.peak.month$Month <- factor(epi.type.peak.month$Month,
                                    levels=months.str)

# create barplot
p.epi.type.month <- ggplot(epi.type.peak.month) + 
  geom_bar(aes(Month, n, fill = epi_type),
           stat="identity", position = "dodge") +
  theme_minimal() + 
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal") +
  scale_fill_brewer(palette="Spectral",
                    name = "Isolation type",
                    labels = c("Unknown", "Clinical", "Environmental/Other")) +
  labs(title = "Figure 3: Barplot of salmonella cases by month and isolation type") 


# find the number of entries that don't report a month
n.without.month <- length(project.data$collection_date[nchar(project.data$collection_date) != 10])

# find the number of entries that do report a month
n.with.month <- length(project.data$collection_date[nchar(project.data$collection_date) == 10])


epi.types <- project.data %>% 
  group_by(epi_type) %>% 
  summarize(Count = n(),
            p = paste0(round( Count / nrow(project.data)*100, 2), "%")) %>% 
  select(`Isolation type` = epi_type, Count, `Proportion of All Cases` = p) %>% 
  mutate(`Isolation type` = case_when(
    `Isolation type` == "" ~ "Unknown",
    `Isolation type` == "clinical" ~ "Clinical",
    `Isolation type` == "environmental/other" ~ "Environmental/Other"
  ))

epi.types.counts.p.tb <- kable(epi.types, 
                               caption = "Breakdown of Isolation types",
                               align = c("l", "r", "r")) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"))

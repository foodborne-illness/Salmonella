########################### PHP 2550 Data Cleansing ###########################

# Read in the data
project.data <- read.csv("merged.data.csv")

# create function to find serotype using the computed types column
extract_serotype <- function(computed_type_str) {
  start <- unlist(gregexpr("=", computed_type_str))[1] + 1
  stop <- unlist(gregexpr(",\"antigen_formula", computed_type_str))[1] - 1
  serotype <- substr(computed_type_str, start, stop)
  return(serotype)
}

# create new column named serotype which include only serotype
project.data$Serotype <- sapply(project.data$computed_types, extract_serotype)

# write data to a csv file
write.csv(project.data,
          "data_complete.csv",
          row.names = TRUE)

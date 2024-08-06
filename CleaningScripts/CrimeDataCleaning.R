#CRIME DATA CLEANING 
library(tidyverse)
library(readr)
library(dplyr)

# Define the base path for your data files
base_path <- "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/crimedata/"

# Load and analyze the datasets for 2021
bsCrimeRate2106 <- read_csv(paste0(base_path, "2021-06/2021-06-avon-and-somerset-street.csv"))
dim(bsCrimeRate2106)
colSums(is.na(bsCrimeRate2106))

bsCrimeRate2107 <- read_csv(paste0(base_path, "2021-07/2021-07-avon-and-somerset-street.csv"))
dim(bsCrimeRate2107)
colSums(is.na(bsCrimeRate2107))

bsCrimeRate2108 <- read_csv(paste0(base_path, "2021-08/2021-08-avon-and-somerset-street.csv"))
dim(bsCrimeRate2108)
colSums(is.na(bsCrimeRate2108))

bsCrimeRate2109 <- read_csv(paste0(base_path, "2021-09/2021-09-avon-and-somerset-street.csv"))
dim(bsCrimeRate2109)
colSums(is.na(bsCrimeRate2109))

bsCrimeRate2110 <- read_csv(paste0(base_path, "2021-10/2021-10-avon-and-somerset-street.csv"))
dim(bsCrimeRate2110)
colSums(is.na(bsCrimeRate2110))

bsCrimeRate2111 <- read_csv(paste0(base_path, "2021-11/2021-11-avon-and-somerset-street.csv"))
dim(bsCrimeRate2111)
colSums(is.na(bsCrimeRate2111))

bsCrimeRate2112 <- read_csv(paste0(base_path, "2021-12/2021-12-avon-and-somerset-street.csv"))
dim(bsCrimeRate2112)
colSums(is.na(bsCrimeRate2112))

# Filter out rows with missing Crime ID
bsCrimeRate2106 <- bsCrimeRate2106 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2107 <- bsCrimeRate2107 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2108 <- bsCrimeRate2108 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2109 <- bsCrimeRate2109 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2110 <- bsCrimeRate2110 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2111 <- bsCrimeRate2111 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2112 <- bsCrimeRate2112 %>% filter(!is.na(`Crime ID`))


# Merge all datasets for 2021
bsCrimeRate2021 <- bind_rows(bsCrimeRate2106, bsCrimeRate2107, bsCrimeRate2108, bsCrimeRate2109,
                             bsCrimeRate2110, bsCrimeRate2111, bsCrimeRate2112)
colSums(is.na(bsCrimeRate2021))

# Remove the "Context" column if it's completely empty
bsCrimeRate2021 <- bsCrimeRate2021 %>% select(-Context)

# Remove duplicated rows
bsCrimeRate2021 <- bsCrimeRate2021 %>% distinct()

# Impute missing values
bsCrimeRate2021$Longitude[is.na(bsCrimeRate2021$Longitude)] <- median(bsCrimeRate2021$Longitude, na.rm = TRUE)
bsCrimeRate2021$Latitude[is.na(bsCrimeRate2021$Latitude)] <- median(bsCrimeRate2021$Latitude, na.rm = TRUE)

# Define the get_mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode for LSOA code and LSOA name
lsoaCodeMode <- get_mode(bsCrimeRate2021$`LSOA code`[!is.na(bsCrimeRate2021$`LSOA code`)])
lsoaNameMode <- get_mode(bsCrimeRate2021$`LSOA name`[!is.na(bsCrimeRate2021$`LSOA name`)])

# Impute the missing values with the calculated mode
bsCrimeRate2021$`LSOA code`[is.na(bsCrimeRate2021$`LSOA code`)] <- lsoaCodeMode
bsCrimeRate2021$`LSOA name`[is.na(bsCrimeRate2021$`LSOA name`)] <- lsoaNameMode

# Verify if all missing values are imputed
colSums(is.na(bsCrimeRate2021))


# After cleaning and merging all datasets for 2021, 2022, and 2023, proceed with further analysis
# (Similar steps as above for 2022 and 2023)

# Write the cleaned data to new CSV files
write.csv(bsCrimeRate2021, paste0("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/bsCrimeRate2021.csv"), row.names = FALSE)

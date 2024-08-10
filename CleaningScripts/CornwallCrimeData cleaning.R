# Load necessary libraries
library(tidyverse)
library(readr)
library(dplyr)

# Define the base path for your data files
base_path <- "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/crimedata/"

# Loading 
crCrimeRate2106 <- read_csv(paste0(base_path, "2021-06/2021-06-devon-and-cornwall-street.csv"))
dim(crCrimeRate2106)
colSums(is.na(crCrimeRate2106))

crCrimeRate2107 <- read_csv(paste0(base_path, "2021-07/2021-07-devon-and-cornwall-street.csv"))
dim(crCrimeRate2107)
colSums(is.na(crCrimeRate2107))

crCrimeRate2108 <- read_csv(paste0(base_path, "2021-08/2021-08-devon-and-cornwall-street.csv"))
dim(crCrimeRate2108)
colSums(is.na(crCrimeRate2108))

crCrimeRate2109 <- read_csv(paste0(base_path, "2021-09/2021-09-devon-and-cornwall-street.csv"))
dim(crCrimeRate2109)
colSums(is.na(crCrimeRate2109))

crCrimeRate2110 <- read_csv(paste0(base_path, "2021-10/2021-10-devon-and-cornwall-street.csv"))
dim(crCrimeRate2110)
colSums(is.na(crCrimeRate2110))

crCrimeRate2111 <- read_csv(paste0(base_path, "2021-11/2021-11-devon-and-cornwall-street.csv"))
dim(crCrimeRate2111)
colSums(is.na(crCrimeRate2111))

crCrimeRate2112 <- read_csv(paste0(base_path, "2021-12/2021-12-devon-and-cornwall-street.csv"))
dim(crCrimeRate2112)
colSums(is.na(crCrimeRate2112))

# Filter out rows with missing Crime ID
crCrimeRate2106 <- crCrimeRate2106 %>% filter(!is.na(`Crime ID`))
crCrimeRate2107 <- crCrimeRate2107 %>% filter(!is.na(`Crime ID`))
crCrimeRate2108 <- crCrimeRate2108 %>% filter(!is.na(`Crime ID`))
crCrimeRate2109 <- crCrimeRate2109 %>% filter(!is.na(`Crime ID`))
crCrimeRate2110 <- crCrimeRate2110 %>% filter(!is.na(`Crime ID`))
crCrimeRate2111 <- crCrimeRate2111 %>% filter(!is.na(`Crime ID`))
crCrimeRate2112 <- crCrimeRate2112 %>% filter(!is.na(`Crime ID`))

# Merge all datasets for 2021
crCrimeRate2021 <- bind_rows(crCrimeRate2106, crCrimeRate2107, crCrimeRate2108, crCrimeRate2109,
                             crCrimeRate2110, crCrimeRate2111, crCrimeRate2112)
colSums(is.na(crCrimeRate2021))

# Remove the "Context" column if it's completely empty
crCrimeRate2021 <- crCrimeRate2021 %>% select(-Context)

# Remove duplicated rows
crCrimeRate2021 <- crCrimeRate2021 %>% distinct()

# Impute missing values
crCrimeRate2021$Longitude[is.na(crCrimeRate2021$Longitude)] <- median(crCrimeRate2021$Longitude, na.rm = TRUE)
crCrimeRate2021$Latitude[is.na(crCrimeRate2021$Latitude)] <- median(crCrimeRate2021$Latitude, na.rm = TRUE)

# Define the get_mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode for LSOA code and LSOA name
lsoaCodeMode <- get_mode(crCrimeRate2021$`LSOA code`[!is.na(crCrimeRate2021$`LSOA code`)])
lsoaNameMode <- get_mode(crCrimeRate2021$`LSOA name`[!is.na(crCrimeRate2021$`LSOA name`)])

# Impute the missing values with the calculated mode
crCrimeRate2021$`LSOA code`[is.na(crCrimeRate2021$`LSOA code`)] <- lsoaCodeMode
crCrimeRate2021$`LSOA name`[is.na(crCrimeRate2021$`LSOA name`)] <- lsoaNameMode

# Verify if all missing values are imputed
colSums(is.na(crCrimeRate2021))

#2022-----------------------------------------------------------------------------------------------------------------------------------------------------------

# Load and analyze the datasets for 2022
crCrimeRate2201 <- read_csv(paste0(base_path, "2022-01/2022-01-devon-and-cornwall-street.csv"))
dim(crCrimeRate2201)
colSums(is.na(crCrimeRate2201))

crCrimeRate2202 <- read_csv(paste0(base_path, "2022-02/2022-02-devon-and-cornwall-street.csv"))
dim(crCrimeRate2202)
colSums(is.na(crCrimeRate2202))

crCrimeRate2203 <- read_csv(paste0(base_path, "2022-03/2022-03-devon-and-cornwall-street.csv"))
dim(crCrimeRate2203)
colSums(is.na(crCrimeRate2203))

crCrimeRate2204 <- read_csv(paste0(base_path, "2022-04/2022-04-devon-and-cornwall-street.csv"))
dim(crCrimeRate2204)
colSums(is.na(crCrimeRate2204))

crCrimeRate2205 <- read_csv(paste0(base_path, "2022-05/2022-05-devon-and-cornwall-street.csv"))
dim(crCrimeRate2205)
colSums(is.na(crCrimeRate2205))

crCrimeRate2206 <- read_csv(paste0(base_path, "2022-06/2022-06-devon-and-cornwall-street.csv"))
dim(crCrimeRate2206)
colSums(is.na(crCrimeRate2206))

crCrimeRate2207 <- read_csv(paste0(base_path, "2022-07/2022-07-devon-and-cornwall-street.csv"))
dim(crCrimeRate2207)
colSums(is.na(crCrimeRate2207))

crCrimeRate2208 <- read_csv(paste0(base_path, "2022-08/2022-08-devon-and-cornwall-street.csv"))
dim(crCrimeRate2208)
colSums(is.na(crCrimeRate2208))

crCrimeRate2209 <- read_csv(paste0(base_path, "2022-09/2022-09-devon-and-cornwall-street.csv"))
dim(crCrimeRate2209)
colSums(is.na(crCrimeRate2209))

crCrimeRate2210 <- read_csv(paste0(base_path, "2022-10/2022-10-devon-and-cornwall-street.csv"))
dim(crCrimeRate2210)
colSums(is.na(crCrimeRate2210))

crCrimeRate2211 <- read_csv(paste0(base_path, "2022-11/2022-11-devon-and-cornwall-street.csv"))
dim(crCrimeRate2211)
colSums(is.na(crCrimeRate2211))

crCrimeRate2212 <- read_csv(paste0(base_path, "2022-12/2022-12-devon-and-cornwall-street.csv"))
colSums(is.na(crCrimeRate2212))

# Filter out rows with missing Crime ID for each dataset
crCrimeRate2201 <- crCrimeRate2201 %>% filter(!is.na(`Crime ID`))
crCrimeRate2202 <- crCrimeRate2202 %>% filter(!is.na(`Crime ID`))
crCrimeRate2203 <- crCrimeRate2203 %>% filter(!is.na(`Crime ID`))
crCrimeRate2204 <- crCrimeRate2204 %>% filter(!is.na(`Crime ID`))
crCrimeRate2205 <- crCrimeRate2205 %>% filter(!is.na(`Crime ID`))
crCrimeRate2206 <- crCrimeRate2206 %>% filter(!is.na(`Crime ID`))
crCrimeRate2207 <- crCrimeRate2207 %>% filter(!is.na(`Crime ID`))
crCrimeRate2208 <- crCrimeRate2208 %>% filter(!is.na(`Crime ID`))
crCrimeRate2209 <- crCrimeRate2209 %>% filter(!is.na(`Crime ID`))
crCrimeRate2210 <- crCrimeRate2210 %>% filter(!is.na(`Crime ID`))
crCrimeRate2211 <- crCrimeRate2211 %>% filter(!is.na(`Crime ID`))
crCrimeRate2212 <- crCrimeRate2212 %>% filter(!is.na(`Crime ID`))

# Merge all datasets for 2022
crCrimeRate2022 <- bind_rows(crCrimeRate2201, crCrimeRate2202, crCrimeRate2203, crCrimeRate2204,
                             crCrimeRate2205, crCrimeRate2206, crCrimeRate2207, crCrimeRate2208,
                             crCrimeRate2209, crCrimeRate2210, crCrimeRate2211, crCrimeRate2212)

# Check for missing values
colSums(is.na(crCrimeRate2022))

# Remove the "Context" column if it's completely empty
crCrimeRate2022 <- crCrimeRate2022 %>% select(-Context)

# Remove duplicated rows
crCrimeRate2022 <- crCrimeRate2022 %>% distinct()

# Impute missing values
crCrimeRate2022$Longitude[is.na(crCrimeRate2022$Longitude)] <- median(crCrimeRate2022$Longitude, na.rm = TRUE)
crCrimeRate2022$Latitude[is.na(crCrimeRate2022$Latitude)] <- median(crCrimeRate2022$Latitude, na.rm = TRUE)

# Calculate the mode for LSOA code and LSOA name
lsoaCodeMode <- get_mode(crCrimeRate2022$`LSOA code`[!is.na(crCrimeRate2022$`LSOA code`)])
lsoaNameMode <- get_mode(crCrimeRate2022$`LSOA name`[!is.na(crCrimeRate2022$`LSOA name`)])

# Impute the missing values with the calculated mode
crCrimeRate2022$`LSOA code`[is.na(crCrimeRate2022$`LSOA code`)] <- lsoaCodeMode
crCrimeRate2022$`LSOA name`[is.na(crCrimeRate2022$`LSOA name`)] <- lsoaNameMode

# Verify if all missing values are imputed
colSums(is.na(crCrimeRate2022))


#
# 2023-----------------------------------------------------------------------------------------------------------------------------------------------------------

# Load and analyze the datasets for 2023
crCrimeRate2301 <- read_csv(paste0(base_path, "2023-01/2023-01-devon-and-cornwall-street.csv"))
dim(crCrimeRate2301)
colSums(is.na(crCrimeRate2301))

crCrimeRate2302 <- read_csv(paste0(base_path, "2023-02/2023-02-devon-and-cornwall-street.csv"))
dim(crCrimeRate2302)
colSums(is.na(crCrimeRate2302))

crCrimeRate2303 <- read_csv(paste0(base_path, "2023-03/2023-03-devon-and-cornwall-street.csv"))
dim(crCrimeRate2303)
colSums(is.na(crCrimeRate2303))

crCrimeRate2304 <- read_csv(paste0(base_path, "2023-04/2023-04-devon-and-cornwall-street.csv"))
dim(crCrimeRate2304)
colSums(is.na(crCrimeRate2304))

crCrimeRate2305 <- read_csv(paste0(base_path, "2023-05/2023-05-devon-and-cornwall-street.csv"))
dim(crCrimeRate2305)
colSums(is.na(crCrimeRate2305))

crCrimeRate2306 <- read_csv(paste0(base_path, "2023-06/2023-06-devon-and-cornwall-street.csv"))
dim(crCrimeRate2306)
colSums(is.na(crCrimeRate2306))

# Filter out rows with missing Crime ID for each dataset
crCrimeRate2301 <- crCrimeRate2301 %>% filter(!is.na(`Crime ID`))
crCrimeRate2302 <- crCrimeRate2302 %>% filter(!is.na(`Crime ID`))
crCrimeRate2303 <- crCrimeRate2303 %>% filter(!is.na(`Crime ID`))
crCrimeRate2304 <- crCrimeRate2304 %>% filter(!is.na(`Crime ID`))
crCrimeRate2305 <- crCrimeRate2305 %>% filter(!is.na(`Crime ID`))
crCrimeRate2306 <- crCrimeRate2306 %>% filter(!is.na(`Crime ID`))

# Merge all datasets for 2023
crCrimeRate2023 <- bind_rows(crCrimeRate2301, crCrimeRate2302, crCrimeRate2303, crCrimeRate2304,
                             crCrimeRate2305, crCrimeRate2306)

# Check for missing values
colSums(is.na(crCrimeRate2023))

# Remove the "Context" column if it's completely empty
crCrimeRate2023 <- crCrimeRate2023 %>% select(-Context)

# Remove duplicated rows
crCrimeRate2023 <- crCrimeRate2023 %>% distinct()

# Impute missing values
crCrimeRate2023$Longitude[is.na(crCrimeRate2023$Longitude)] <- median(crCrimeRate2023$Longitude, na.rm = TRUE)
crCrimeRate2023$Latitude[is.na(crCrimeRate2023$Latitude)] <- median(crCrimeRate2023$Latitude, na.rm = TRUE)

# Calculate the mode for LSOA code and LSOA name
lsoaCodeMode <- get_mode(crCrimeRate2023$`LSOA code`[!is.na(crCrimeRate2023$`LSOA code`)])
lsoaNameMode <- get_mode(crCrimeRate2023$`LSOA name`[!is.na(crCrimeRate2023$`LSOA name`)])

# Impute the missing values with the calculated mode
crCrimeRate2023$`LSOA code`[is.na(crCrimeRate2023$`LSOA code`)] <- lsoaCodeMode
crCrimeRate2023$`LSOA name`[is.na(crCrimeRate2023$`LSOA name`)] <- lsoaNameMode

# Verify if all missing values are imputed
colSums(is.na(crCrimeRate2023))

#--------------------------------------------------------------------------------------------------------------------------------------------------------

# Combining data for Cornwall (2021, 2022, 2023)
cornwallCrimeRateCleaned = bind_rows(crCrimeRate2021, crCrimeRate2022, crCrimeRate2023)
View(cornwallCrimeRateCleaned)
dim(cornwallCrimeRateCleaned)

# Load the postcode to LSOA mapping dataset
pscdToLsoa = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/Postcode to LSOA.csv")
View(pscdToLsoa)
dim(pscdToLsoa)
colnames(pscdToLsoa)

# Narrow down the dataset to necessary columns
pscdToLsoa = pscdToLsoa %>% 
  rename(postcode_space = pcds) %>% 
  rename(`LSOA code` = lsoa11cd) %>% 
  rename(city = ladnm) %>% 
  select(postcode_space, `LSOA code`, city)
View(pscdToLsoa)
dim(pscdToLsoa)

# Remove duplicate associations for better manageability
pscdToLsoa = pscdToLsoa %>% 
  group_by(`LSOA code`) %>% 
  summarize(
    postcode_space = first(postcode_space), 
    city = first(city))
dim(pscdToLsoa)
View(pscdToLsoa)

# Join Cornwall crime data with LSOA mapping
cornwallCrimeRateCleaned = cornwallCrimeRateCleaned %>% 
  rename(Year = Month) %>% 
  select(`Crime ID`, Year, `LSOA code`, `LSOA name`, `Crime type`) %>% 
  inner_join(pscdToLsoa, by = "LSOA code")
dim(cornwallCrimeRateCleaned)

# Summarize number of different crimes in Cornwall from 2021-2023
cornwallCrimeSummary = cornwallCrimeRateCleaned %>% 
  group_by(city, Year, `Crime type`) %>% 
  summarize(CrimeTypeCount = n(), .groups = 'drop') %>% 
  arrange(Year)
View(cornwallCrimeSummary)

#now lets do it for BRISTOL----------------------------

bristolCrimeRateCleaned= read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/combined_bsCrimeRate.csv")
#because the column names has "." instead of the space, it should be consistent with the cornwall's dataset column names
colnames(bristolCrimeRateCleaned) <- gsub("\\.", " ", colnames(bristolCrimeRateCleaned))


# Join Bristol crime data with LSOA mapping
bristolCrimeRateCleaned = bristolCrimeRateCleaned %>% 
  rename(Year = Month) %>% 
  select(`Crime ID`, Year, `LSOA code`, `LSOA name`, `Crime type`) %>% 
  inner_join(pscdToLsoa, by = "LSOA code")
View(bristolCrimeRateCleaned)
dim(bristolCrimeRateCleaned)

# Summarize number of different crimes in Bristol from 2021-2023
bristolCrimeSummary = bristolCrimeRateCleaned %>% 
  group_by(city, Year, `Crime type`) %>% 
  summarize(CrimeTypeCount = n(), .groups = 'drop') %>% 
  arrange(Year)
View(bristolCrimeSummary)

#SAVING THE CLEANED DATAS---------------------------------------------
# Export cleaned datasets to CSV files
write.csv(bristolCrimeRateCleaned, "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/CrimeData/bristol-crime-rate.csv", row.names = FALSE)
write.csv(bristolCrimeSummary, "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/CrimeData/bristol-crime-summary.csv", row.names = FALSE)
# Export cleaned datasets to CSV files
write.csv(cornwallCrimeRateCleaned, "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall-crime-rate.csv", row.names = FALSE)
write.csv(cornwallCrimeSummary, "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall-crime-summary.csv", row.names = FALSE)


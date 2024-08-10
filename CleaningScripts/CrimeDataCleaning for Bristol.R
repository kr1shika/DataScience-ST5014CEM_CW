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

#2022-----------------------------------------------------------------------------------------------------------------------------------------------------------

# Load and analyze the datasets for 2022
bsCrimeRate2201 <- read_csv(paste0(base_path, "2022-01/2022-01-avon-and-somerset-street.csv"))
dim(bsCrimeRate2201)
colSums(is.na(bsCrimeRate2201))

bsCrimeRate2202 <- read_csv(paste0(base_path, "2022-02/2022-02-avon-and-somerset-street.csv"))
dim(bsCrimeRate2202)
colSums(is.na(bsCrimeRate2202))

bsCrimeRate2203 <- read_csv(paste0(base_path, "2022-03/2022-03-avon-and-somerset-street.csv"))
dim(bsCrimeRate2203)
colSums(is.na(bsCrimeRate2203))

bsCrimeRate2204 <- read_csv(paste0(base_path, "2022-04/2022-04-avon-and-somerset-street.csv"))
dim(bsCrimeRate2204)
colSums(is.na(bsCrimeRate2204))

bsCrimeRate2205 <- read_csv(paste0(base_path, "2022-05/2022-05-avon-and-somerset-street.csv"))
dim(bsCrimeRate2205)
colSums(is.na(bsCrimeRate2205))

bsCrimeRate2206 <- read_csv(paste0(base_path, "2022-06/2022-06-avon-and-somerset-street.csv"))
dim(bsCrimeRate2206)
colSums(is.na(bsCrimeRate2206))

bsCrimeRate2207 <- read_csv(paste0(base_path, "2022-07/2022-07-avon-and-somerset-street.csv"))
dim(bsCrimeRate2207)
colSums(is.na(bsCrimeRate2207))

bsCrimeRate2208 <- read_csv(paste0(base_path, "2022-08/2022-08-avon-and-somerset-street.csv"))
dim(bsCrimeRate2208)
colSums(is.na(bsCrimeRate2208))

bsCrimeRate2209 <- read_csv(paste0(base_path, "2022-09/2022-09-avon-and-somerset-street.csv"))
dim(bsCrimeRate2209)
colSums(is.na(bsCrimeRate2209))

bsCrimeRate2210 <- read_csv(paste0(base_path, "2022-10/2022-10-avon-and-somerset-street.csv"))
dim(bsCrimeRate2210)
colSums(is.na(bsCrimeRate2210))

bsCrimeRate2211 <- read_csv(paste0(base_path, "2022-11/2022-11-avon-and-somerset-street.csv"))
dim(bsCrimeRate2211)
colSums(is.na(bsCrimeRate2211))

bsCrimeRate2212 <- read_csv(paste0(base_path, "2022-12/2022-12-avon-and-somerset-street.csv"))
colSums(is.na(bsCrimeRate2212))

# Filter out rows with missing Crime ID for each dataset
bsCrimeRate2201 <- bsCrimeRate2201 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2202 <- bsCrimeRate2202 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2203 <- bsCrimeRate2203 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2204 <- bsCrimeRate2204 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2205 <- bsCrimeRate2205 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2206 <- bsCrimeRate2206 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2207 <- bsCrimeRate2207 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2208 <- bsCrimeRate2208 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2209 <- bsCrimeRate2209 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2210 <- bsCrimeRate2210 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2211 <- bsCrimeRate2211 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2212 <- bsCrimeRate2212 %>% filter(!is.na(`Crime ID`))

# Merge all datasets for 2022
bsCrimeRate2022 <- bind_rows(bsCrimeRate2201, bsCrimeRate2202, bsCrimeRate2203, bsCrimeRate2204,
                             bsCrimeRate2205, bsCrimeRate2206, bsCrimeRate2207, bsCrimeRate2208,
                             bsCrimeRate2209, bsCrimeRate2210, bsCrimeRate2211, bsCrimeRate2212)

# Check for missing values
colSums(is.na(bsCrimeRate2022))

# Remove the "Context" column if it's completely empty
bsCrimeRate2022 <- bsCrimeRate2022 %>% select(-Context)

# Remove duplicated rows
bsCrimeRate2022 <- bsCrimeRate2022 %>% distinct()

# Impute missing values for Longitude and Latitude
bsCrimeRate2022$Longitude[is.na(bsCrimeRate2022$Longitude)] <- median(bsCrimeRate2022$Longitude, na.rm = TRUE)
bsCrimeRate2022$Latitude[is.na(bsCrimeRate2022$Latitude)] <- median(bsCrimeRate2022$Latitude, na.rm = TRUE)

# Define the get_mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode for LSOA code and LSOA name
lsoaCodeMode <- get_mode(bsCrimeRate2022$`LSOA code`[!is.na(bsCrimeRate2022$`LSOA code`)])
lsoaNameMode <- get_mode(bsCrimeRate2022$`LSOA name`[!is.na(bsCrimeRate2022$`LSOA name`)])

# Impute the missing values with the calculated mode
bsCrimeRate2022$`LSOA code`[is.na(bsCrimeRate2022$`LSOA code`)] <- lsoaCodeMode
bsCrimeRate2022$`LSOA name`[is.na(bsCrimeRate2022$`LSOA name`)] <- lsoaNameMode

# Verify if all missing values are imputed
colSums(is.na(bsCrimeRate2022))

#2023---------------------------------------------------------------------------------------------------------------------------------------------------
bsCrimeRate2301 <- read_csv(paste0(base_path, "2023-01/2023-01-avon-and-somerset-street.csv"))
dim(bsCrimeRate2301)
colSums(is.na(bsCrimeRate2301))

bsCrimeRate2302 <- read_csv(paste0(base_path, "2023-02/2023-02-avon-and-somerset-street.csv"))
dim(bsCrimeRate2302)
colSums(is.na(bsCrimeRate2302))

bsCrimeRate2303 <- read_csv(paste0(base_path, "2023-03/2023-03-avon-and-somerset-street.csv"))
dim(bsCrimeRate2303)
colSums(is.na(bsCrimeRate2303))

bsCrimeRate2304 <- read_csv(paste0(base_path, "2023-04/2023-04-avon-and-somerset-street.csv"))
dim(bsCrimeRate2304)
colSums(is.na(bsCrimeRate2304))

bsCrimeRate2305 <- read_csv(paste0(base_path, "2023-05/2023-05-avon-and-somerset-street.csv"))
dim(bsCrimeRate2305)
colSums(is.na(bsCrimeRate2305))

bsCrimeRate2306 <- read_csv(paste0(base_path, "2023-06/2023-06-avon-and-somerset-street.csv"))
dim(bsCrimeRate2306)
colSums(is.na(bsCrimeRate2306))

bsCrimeRate2307 <- read_csv(paste0(base_path, "2023-07/2023-07-avon-and-somerset-street.csv"))
dim(bsCrimeRate2307)
colSums(is.na(bsCrimeRate2307))

bsCrimeRate2308 <- read_csv(paste0(base_path, "2023-08/2023-08-avon-and-somerset-street.csv"))
dim(bsCrimeRate2308)
colSums(is.na(bsCrimeRate2308))

bsCrimeRate2309 <- read_csv(paste0(base_path, "2023-09/2023-09-avon-and-somerset-street.csv"))
dim(bsCrimeRate2309)
colSums(is.na(bsCrimeRate2309))

bsCrimeRate2310 <- read_csv(paste0(base_path, "2023-10/2023-10-avon-and-somerset-street.csv"))
dim(bsCrimeRate2310)
colSums(is.na(bsCrimeRate2310))

bsCrimeRate2311 <- read_csv(paste0(base_path, "2023-11/2023-11-avon-and-somerset-street.csv"))
dim(bsCrimeRate2311)
colSums(is.na(bsCrimeRate2311))

bsCrimeRate2312 <- read_csv(paste0(base_path, "2023-12/2023-12-avon-and-somerset-street.csv"))
colSums(is.na(bsCrimeRate2312))

bsCrimeRate2301 <- bsCrimeRate2301 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2302 <- bsCrimeRate2302 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2303 <- bsCrimeRate2303 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2304 <- bsCrimeRate2304 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2305 <- bsCrimeRate2305 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2306 <- bsCrimeRate2306 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2307 <- bsCrimeRate2307 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2308 <- bsCrimeRate2308 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2309 <- bsCrimeRate2309 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2310 <- bsCrimeRate2310 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2311 <- bsCrimeRate2311 %>% filter(!is.na(`Crime ID`))
bsCrimeRate2312 <- bsCrimeRate2312 %>% filter(!is.na(`Crime ID`))

# Merge all datasets for 2023
bsCrimeRate2023 <- bind_rows(bsCrimeRate2301, bsCrimeRate2302, bsCrimeRate2303, bsCrimeRate2304,
                             bsCrimeRate2305, bsCrimeRate2306, bsCrimeRate2307, bsCrimeRate2308,
                             bsCrimeRate2309, bsCrimeRate2310, bsCrimeRate2311, bsCrimeRate2312)

# Check for missing values
colSums(is.na(bsCrimeRate2023))

# Remove the "Context" column if it's completely empty
bsCrimeRate2023 <- bsCrimeRate2023 %>% select(-Context)

# Remove duplicated rows
bsCrimeRate2023 <- bsCrimeRate2023 %>% distinct()

# Impute missing values for Longitude and Latitude
bsCrimeRate2023$Longitude[is.na(bsCrimeRate2023$Longitude)] <- median(bsCrimeRate2023$Longitude, na.rm = TRUE)
bsCrimeRate2023$Latitude[is.na(bsCrimeRate2023$Latitude)] <- median(bsCrimeRate2023$Latitude, na.rm = TRUE)

# Calculate the mode for LSOA code and LSOA name
lsoaCodeMode <- get_mode(bsCrimeRate2023$`LSOA code`[!is.na(bsCrimeRate2023$`LSOA code`)])
lsoaNameMode <- get_mode(bsCrimeRate2023$`LSOA name`[!is.na(bsCrimeRate2023$`LSOA name`)])

# Impute the missing values with the calculated mode
bsCrimeRate2023$`LSOA code`[is.na(bsCrimeRate2023$`LSOA code`)] <- lsoaCodeMode
bsCrimeRate2023$`LSOA name`[is.na(bsCrimeRate2023$`LSOA name`)] <- lsoaNameMode

# Verify if all missing values are imputed
colSums(is.na(bsCrimeRate2023))

#merging--------------------------------------------------------------------------------------------------------------------

combined_bsCrimeRate <- bind_rows(bsCrimeRate2021, bsCrimeRate2022, bsCrimeRate2023)
colSums(is.na(combined_bsCrimeRate))

write.csv(combined_bsCrimeRate, paste0( "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/combined_bsCrimeRate.csv"), row.names = FALSE)

#-----------------------------------------------------------------------------------------



library(tidyverse)
library(lubridate)

# Function to load and clean a single year's data
load_and_clean_data <- function(file_path) {
  col_names <- c('Txn_ID', 'Price', 'Date_of_transfer', 'Postcode', 'Property_type', 'Old/New', 'Duration', 
                 'PAON', 'SAON', 'Street', 'Locality', 'Town/City', 'District', 'County', 'PPD_type', 'Record_status')
  data <- read_csv(file_path, col_names = col_names, col_types = cols(.default = "c"))
  
  data <- data %>%
    mutate(
      Date_of_transfer = as.Date(Date_of_transfer, format="%Y-%m-%d"),
      Postcode = ifelse(is.na(Postcode), "Unknown", Postcode),
      SAON = ifelse(is.na(SAON), "Not Applicable", SAON),
      Street = ifelse(is.na(Street), "Unknown", Street),
      Locality = ifelse(is.na(Locality), "Not Applicable", Locality)
    ) %>%
    na.omit() %>%
    distinct()
  
  return(data)
}

# File paths
file_paths <- list(
  "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/HousePricings/pp-2020.csv",
  "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/HousePricings/pp-2021.csv",
  "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/HousePricings/pp-2022.csv",
  "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/HousePricings/pp-2023.csv"
)

# Load and combine data
houseprice_data <- map_dfr(file_paths, load_and_clean_data)

# Filtering for Cornwall and City of Bristol
houseprice_data <- houseprice_data %>% 
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL'))

# Cleaning and refining the data
houseprice_data <- houseprice_data %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[^ ]+"),
    Year = year(Date_of_transfer)
  ) %>%
  select(Year, Postcode, shortPostcode, District, `Town/City`, Price, County, Locality) %>%
  distinct() %>%
  as_tibble()

# Save the cleaned data to a new CSV file
write_csv(houseprice_data, 'C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv')

# Display the first few rows of the cleaned data
head(houseprice_data)

library(tidyverse)
library(lubridate)

# Load and clean 2020 data
data_2020 <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/HousePricings/pp-2020.csv",
                      col_names = c('Txn_ID', 'Price', 'Date_of_transfer', 'Postcode', 'Property_type', 'Old/New', 'Duration', 
                                    'PAON', 'SAON', 'Street', 'Locality', 'Town/City', 'District', 'County', 'PPD_type', 'Record_status'),
                      col_types = cols(.default = "c")) %>%
  mutate(
    Date_of_transfer = as.Date(Date_of_transfer, format="%Y-%m-%d"),
    Postcode = ifelse(is.na(Postcode), "Unknown", Postcode),
    SAON = ifelse(is.na(SAON), "Not Applicable", SAON),
    Street = ifelse(is.na(Street), "Unknown", Street),
    Locality = ifelse(is.na(Locality), "Not Applicable", Locality)
  ) %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[^ ]+"),
    Year = year(Date_of_transfer)
  ) %>%
  select(Year, Postcode, shortPostcode, District, `Town/City`, Price, County, Locality) %>%
  distinct() %>%
  as_tibble()

# Display first few rows of 2020 data
head(data_2020)


# Load and clean 2021 data
data_2021 <- raw_data_2021 %>%
  mutate(
    Date_of_transfer = mdy_hm(Date_of_transfer),  # Parse the date and time
    Postcode = ifelse(is.na(Postcode), "Unknown", Postcode),
    SAON = ifelse(is.na(SAON), "Not Applicable", SAON),
    Street = ifelse(is.na(Street), "Unknown", Street),
    Locality = ifelse(is.na(Locality), "Not Applicable", Locality)
  ) %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[^ ]+"),
    Year = year(Date_of_transfer)
  ) %>%
  select(Year, Postcode, shortPostcode, District, `Town/City`, Price, County, Locality) %>%
  distinct() %>%
  as_tibble()

# Display the first few rows to confirm it's correct now
head(data_2021)

# Load the 2022 data
raw_data_2022 <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/HousePricings/pp-2022.csv",
                          col_names = c('Txn_ID', 'Price', 'Date_of_transfer', 'Postcode', 'Property_type', 'Old/New', 'Duration', 
                                        'PAON', 'SAON', 'Street', 'Locality', 'Town/City', 'District', 'County', 'PPD_type', 'Record_status'),
                          col_types = cols(.default = "c"))

# Display the first few rows to inspect the Date_of_transfer format
head(raw_data_2022$Date_of_transfer)

data_2022 <- raw_data_2022 %>%
  mutate(
    Date_of_transfer = mdy_hm(Date_of_transfer),  # Parse the date and time if in "m/d/yyyy H:M" format
    Postcode = ifelse(is.na(Postcode), "Unknown", Postcode),
    SAON = ifelse(is.na(SAON), "Not Applicable", SAON),
    Street = ifelse(is.na(Street), "Unknown", Street),
    Locality = ifelse(is.na(Locality), "Not Applicable", Locality)
  ) %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[^ ]+"),
    Year = year(Date_of_transfer)
  ) %>%
  select(Year, Postcode, shortPostcode, District, `Town/City`, Price, County, Locality) %>%
  distinct() %>%
  as_tibble()

head(data_2022)

# Load and clean 2023 data
data_2023 <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/HousePricings/pp-2023.csv",
                      col_names = c('Txn_ID', 'Price', 'Date_of_transfer', 'Postcode', 'Property_type', 'Old/New', 'Duration', 
                                    'PAON', 'SAON', 'Street', 'Locality', 'Town/City', 'District', 'County', 'PPD_type', 'Record_status'),
                      col_types = cols(.default = "c")) %>%
  mutate(
    Date_of_transfer = as.Date(Date_of_transfer, format="%Y-%m-%d"),
    Postcode = ifelse(is.na(Postcode), "Unknown", Postcode),
    SAON = ifelse(is.na(SAON), "Not Applicable", SAON),
    Street = ifelse(is.na(Street), "Unknown", Street),
    Locality = ifelse(is.na(Locality), "Not Applicable", Locality)
  ) %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[^ ]+"),
    Year = year(Date_of_transfer)
  ) %>%
  select(Year, Postcode, shortPostcode, District, `Town/City`, Price, County, Locality) %>%
  distinct() %>%
  as_tibble()

# Display first few rows of 2023 data
head(data_2023)

#now merging 
combined_data <- bind_rows(data_2020, data_2021, data_2022, data_2023)
write_csv(combined_data, 'C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv')
view(combined_data)


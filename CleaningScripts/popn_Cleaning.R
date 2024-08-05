# Load necessary libraries
library(tidyverse)
library(dplyr)
library(stringi)

# Read data
house_prices <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv")
population_data <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/Population2011_1656567141570.csv", show_col_types = FALSE)

# Check the structure of the datasets
str(house_prices)
str(population_data)

# Preview the datasets
head(population_data)
head(house_prices)

# Naming the columns of the population data
colnames(population_data) <- c('Postcode', 'Population')

pattern <- ' .*$'

# Calculate population estimates for each year from 2011 to 2023
population_data <- population_data %>%
  mutate(shortPostcode = gsub(pattern, "", Postcode)) %>%
  group_by(shortPostcode) %>%
  summarise(Population2011 = sum(Population, na.rm = TRUE)) %>%
  mutate(
    Population2012 = 1.00695353132322269 * Population2011,
    Population2013 = 1.00669740535540783 * Population2012,
    Population2014 = 1.00736463978721671 * Population2013,
    Population2015 = 1.00792367505802859 * Population2014,
    Population2016 = 1.00757874492811929 * Population2015,
    Population2017 = 1.00679374473924223 * Population2016,
    Population2018 = 1.00605929132212552 * Population2017,
    Population2019 = 1.00561255390388033 * Population2018,
    Population2020 = 1.00561255390388033 * Population2019,
    Population2021 = 1.00561255390388033 * Population2020,
    Population2022 = 1.00561255390388033 * Population2021,
    Population2023 = 1.00561255390388033 * Population2022
  ) %>%
  select(shortPostcode, Population2020, Population2021, Population2022, Population2023)

head(population_data)

house_prices <- house_prices %>%
  mutate(shortPostcode = gsub(pattern, "", Postcode)) %>%
  mutate(Year = str_trim(substring(Year, 1, 4))) %>%
  left_join(population_data, by = "shortPostcode") %>%
  select(Year, Postcode, shortPostcode, Locality, `Town/City`, District, County, Population2020, Population2021, Population2022, Population2023) %>%
  group_by(shortPostcode) %>%
  arrange(County) %>%
  as_tibble() %>%
  na.omit() %>%
  distinct()

# View the cleaned data
View(house_prices)
view(population_data)

# Save the cleaned data to a new CSV file
write_csv(house_prices, "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/popn_town.csv")

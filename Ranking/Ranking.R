#RANKING 
# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)

# Load the datasets
housing_prices <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv")
head(housing_prices)
housing_prices <- housing_prices %>%
  mutate(PostcodePrefix = substr(Postcode, 1, 2)) %>%
  filter(PostcodePrefix %in% c("BS", "TR", "PL", "EX"))
crime_data_bristol <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/bristol_crimedata.csv")
crime_data_cornwall <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall_crimedata.csv")
#has the london data in it
internet_connectivity <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband_combined.csv")
school_quality_bristol <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/BristolSchool.csv")
school_quality_cornwall <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/CornwallSchool.csv")
population_data <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/popn_2023.csv")

# Combine crime data and school quality data from both regions
crime_data <- bind_rows(crime_data_bristol, crime_data_cornwall)
unique_cities <- unique(crime_data$city)
unique_cities
# Filtering the data for Bristol and Cornwall
crime_data <- crime_data %>%
  filter(city %in% c("Bristol, City of", "Cornwall"))

school_quality <- bind_rows(school_quality_bristol, school_quality_cornwall)

head(crime_data)
crime_data <- crime_data %>%
  rename(POSTCODE = postcode_space,short_postcode = Postcode)

housing_prices <- housing_prices %>%
  rename(POSTCODE = Postcode)

internet_connectivity <- internet_connectivity %>%
  rename(POSTCODE = postcode_space)

head(housing_prices)
head(internet_connectivity)
head(crime_data)
head(school_quality)

combined_data <- housing_prices %>%
  left_join(internet_connectivity, by = "POSTCODE")%>%
  left_join(crime_data, by = "POSTCODE") 
  
head(combined_data)

# Remove duplicate rows
combined_data <- combined_data %>%
  distinct()

# Alternatively, remove duplicates based on specific columns
combined_data <- combined_data %>%
  distinct(POSTCODE, .keep_all = TRUE)

# Fill missing values with the mean for numeric columns
combined_data <- combined_data %>%
  mutate(
    Price = ifelse(is.na(Price), mean(Price, na.rm = TRUE), Price),
    Average.download.speed..Mbit.s. = ifelse(is.na(Average.download.speed..Mbit.s.), mean(Average.download.speed..Mbit.s., na.rm = TRUE), Average.download.speed..Mbit.s.)
  )
colnames(combined_data)

# Select only the relevant columns
combined_data <- combined_data %>%
  select(Year = Year.x, POSTCODE, `Town/City`, Price, County, 
         `Average.download.speed..Mbit.s.`, `Crime type`)

# View the cleaned data with only relevant columns
head(combined_data)
nrow(combined_data)

combined_data <- combined_data %>%
  mutate(`Town/City` = replace_na(`Town/City`, "Unknown"))
combined_data <- combined_data %>%
  mutate(`Crime type` = replace_na(`Crime type`, "Unknown"))

ranked_towns_price <- combined_data %>%
  group_by(`Town/City`, County) %>%
  summarize(Average_Price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(Average_Price))

head(school_quality)

# Convert town names in school_quality to uppercase
school_quality <- school_quality %>%
  mutate(TOWN = toupper(TOWN))

# View the updated school_quality dataset to confirm changes
head(school_quality)

# Now merge combined_data with the updated school_quality using Town/City and TOWN
combined_data_with_schools <- combined_data %>%
  left_join(school_quality, by = c("Town/City" = "TOWN"))

# View the result to check if the merge was successful
head(combined_data_with_schools)

# Remove rows where 'ATT8SCR' is "SUPP" or "NE"
combined_data_with_schools <- combined_data_with_schools %>%
  filter(!(ATT8SCR %in% c("SUPP", "NE")))

combined_data_cleaned <- combined_data_with_schools %>%
  select(Year.x, POSTCODE.x, `Town/City`, Price, County, `Average.download.speed..Mbit.s.`, `Crime type`, ATT8SCR) %>%
  distinct()

#----------------------------------
# Ensure numeric types for calculations
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(
    Price = as.numeric(Price),
    `Average.download.speed..Mbit.s.` = as.numeric(`Average.download.speed..Mbit.s.`),
    ATT8SCR = as.numeric(ATT8SCR)  # Ensure ATT8SCR is numeric
  ) %>%
  filter(`Town/City` != "WESTON-SUPER-MARE")  # Remove Weston-super-Mare

# Handle missing values in ATT8SCR
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(ATT8SCR = ifelse(is.na(ATT8SCR), mean(ATT8SCR, na.rm = TRUE), ATT8SCR))
# Rename POSTCODE.x to POSTCODE to match the crime_summary
combined_data_cleaned <- combined_data_cleaned %>%
  rename(POSTCODE = POSTCODE.x)


#for crime 

head(crime_data)
# Convert Year to Date format if it isn't already
crime_data <- crime_data %>%
  mutate(Year = as.Date(Year))
# Ensure POSTCODE is in the same format as your other datasets
crime_data <- crime_data %>%
  mutate(POSTCODE = toupper(POSTCODE))

crime_summary <- crime_data %>%
  group_by(POSTCODE) %>%
  summarise(Num_Crimes = n(), .groups = 'drop')

# Join the cleaned data with the crime summary
combined_data_cleaned <- combined_data_cleaned %>%
  left_join(crime_summary, by = "POSTCODE")


head(combined_data_cleaned)

#---
rank_by_price <- combined_data_cleaned %>%
  group_by(`Town/City`, County) %>%
  summarize(Average_Price = mean(Price, na.rm = TRUE)) %>%
  arrange(Average_Price) %>%  
  mutate(Price_Rank = row_number()) 

# Ensure consistency in ranking
rank_by_speed <- combined_data_cleaned %>%
  group_by(`Town/City`, County) %>%
  summarize(Average_Speed = mean(`Average.download.speed..Mbit.s.`, na.rm = TRUE)) %>%
  arrange(desc(Average_Speed)) %>%
  mutate(Speed_Rank = row_number())

rank_by_att8scr <- combined_data_cleaned %>%
  group_by(`Town/City`, County) %>%
  summarize(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE)) %>%
  arrange(desc(Average_ATT8SCR)) %>%
  mutate(ATT8SCR_Rank = row_number())

crime_summary_by_town <- combined_data_cleaned %>%
  group_by(`Town/City`, County) %>%
  summarise(Average_Num_Crimes = mean(Num_Crimes, na.rm = TRUE), .groups = 'drop') %>%
  arrange((Average_Num_Crimes)) %>%
  mutate(Crime_Rank = row_number())

#--------------------------------
combined_rankings <- rank_by_price %>%
  left_join(rank_by_speed, by = c("Town/City", "County")) %>%
  left_join(rank_by_att8scr, by = c("Town/City", "County")) %>%
  left_join(crime_summary_by_town, by = c("Town/City", "County"))

housing_weight <- 0.34
crime_weight <- 0.22
internet_weight <- 0.28
school_weight <- 0.15

# Check the ranking for prices
head(rank_by_price)

# Calculate the weighted composite score
combined_rankings <- combined_rankings %>%
  mutate(
    Weighted_Composite_Score = (Price_Rank * housing_weight +
                                  Speed_Rank * internet_weight +
                                  ATT8SCR_Rank * school_weight +
                                  Crime_Rank * crime_weight))
# View the results
head(combined_rankings)
# Exclude the rank columns from the combined_rankings dataset
combined_rankings_cleaned <- combined_rankings %>%
  select(`Town/City`, County, Average_Price, Average_Speed, Average_ATT8SCR, Average_Num_Crimes, Weighted_Composite_Score)

# Print the cleaned dataset
print(combined_rankings_cleaned)

#-----------------------------------------

# Filter for rows where Average_Weighted_Composite_Score is greater than 1 and less than 3
filtered_scores_3 <- combined_rankings_cleaned %>%
  filter(Weighted_Composite_Score > 1 & Weighted_Composite_Score < 3) %>%
  filter(Average_Speed>3.2) %>%
  filter(Average_ATT8SCR>39.99)

# View the filtered result
filtered_scores_3

# Select the top three towns with the best data
top_three_towns <- filtered_scores_10 %>%
  arrange(Weighted_Composite_Score) 

# View the result
top_three_towns




#2. House VS drugRate
library(tidyverse)
library(ggplot2)

HousePrice <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv")
BristolCrimeRate=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/bristol_crimedata.csv")
CornwallCrimeRate=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall_crimedata.csv")

bristolHP2023 <- HousePrice[HousePrice$Year == 2023 & HousePrice$County == "CITY OF BRISTOL", ]
cornwallHP2023 =HousePrice[HousePrice$Year == 2023 & HousePrice$County == "CORNWALL", ]

population23=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/popn_2023.csv")
#head(population23)
#head(bristolHP2023)
#head(BristolCrimeRate)

BristolCrimeRateCLEANED <- BristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+")) %>% 
  mutate(Year = year(as.Date(Year, format = "%Y-%m-%d"))) %>% 
  filter(Year == 2023)
head(BristolCrimeRateCLEANED)

BristolCrimeRateCLEANED <- BristolCrimeRateCLEANED %>%
  mutate(Postcode = str_trim(Postcode) %>% toupper())

population23 <- population23 %>%
  mutate(Postcode = str_replace_all(Postcode, "\\s+", ""))

# Join BristolCrimeRateCLEANED with population23 on the Postcode column
Bristol_drugrate2023 <- BristolCrimeRateCLEANED %>% 
  left_join(population23, by = "Postcode") %>% 
  filter(Crime.type == "Drugs") %>% 
  filter(city == "Bristol, City of")

Bristol_drugrate2023 <- Bristol_drugrate2023 %>% 
  filter(!is.na(Population)) %>% 
  group_by(postcode_space) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)
#--
Bristol_drugrate2023 <- Bristol_drugrate2023 %>%
  mutate(postcode_space = str_replace_all(postcode_space, "\\s+", "") %>% toupper())

bristolHP2023 <- bristolHP2023 %>%
  mutate(Postcode = str_replace_all(Postcode, "\\s+", "") %>% toupper())

nrow(Bristol_drugrate2023)

# Merge the drug offense rate data with house pricing data
Bristol_analysis2023 <- Bristol_drugrate2023 %>%
  left_join(bristolHP2023, by = c("postcode_space" = "Postcode"))

# Check the merged data
head(Bristol_analysis2023)
Bristol_analysis2023 <- Bristol_analysis2023 %>%
  filter(!is.na(shortPostcode) & !is.na(District) & !is.na(Town.City))
  
options(scipen = 1000)
# Create a scatter plot of House Price vs Drug Offense Rate
ggplot(Bristol_analysis2023, aes(x = drug_offense_rate, y = Price)) +
  geom_point(color = "darkblue", size = 2) + 
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "House Price vs Drug Offense Rate in Bristol (2023)",
       x = "Drug Offense Rate (per 10,000 people)",
       y = "House Price (in GBP)") +
  theme_minimal()
#---------------------------------------------------------------------------------


cornwallHP2023 <- HousePrice %>%
  filter(Year == 2023 & County == "CORNWALL")

# Clean and process Cornwall crime data
CornwallCrimeRateCLEANED <- CornwallCrimeRate %>%
  mutate(Postcode = str_trim(str_extract(postcode_space, "^\\S+")) %>% toupper(),
         Year = year(as.Date(Year, format = "%Y-%m-%d"))) %>%
  filter(Year == 2023 & Crime.type == "Drugs")

# Join with population data and calculate drug offense rate
Cornwall_drugrate2023 <- CornwallCrimeRateCLEANED %>%
  left_join(population23 %>% mutate(Postcode = str_replace_all(Postcode, "\\s+", "")), 
            by = "Postcode") %>%
  filter(!is.na(Population)) %>%
  group_by(postcode_space) %>%
  summarise(drug_offenses = n(),
            population = first(Population),
            drug_offense_rate = drug_offenses / population * 10000) %>%
  mutate(postcode_space = toupper(str_replace_all(postcode_space, "\\s+", "")))

# Merge with house price data
Cornwall_analysis2023 <- Cornwall_drugrate2023 %>%
  left_join(cornwallHP2023 %>% mutate(Postcode = toupper(str_replace_all(Postcode, "\\s+", ""))), 
            by = c("postcode_space" = "Postcode")) %>%
  filter(!is.na(shortPostcode) & !is.na(District) & !is.na(Town.City))

# Plot the data
ggplot(Cornwall_analysis2023, aes(x = drug_offense_rate, y = Price)) +
  geom_point(color = "darkgreen", size = 2) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "House Price vs Drug Offense Rate in Cornwall (2023)",
       x = "Drug Offense Rate (per 10,000 people)",
       y = "House Price (in GBP)") +
  theme_minimal()

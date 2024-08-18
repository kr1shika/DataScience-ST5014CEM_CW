#crime rate EDA
install.packages("fmsb")
library(tidyverse)
library(ggplot2)
library(fmsb)

CrimeRate_Cornwall= read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall_crimedata.csv")
CrimeRate_Bristol= read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/cleanedData/CrimeData/bristol_crimedata.csv")
Cornwall_summary=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall_crimesummary.csv")
BristonSummary=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/cleanedData/CrimeData/bristol_crimesummary.csv")
colnames(CrimeRate_Cornwall)
colnames(CrimeRate_Bristol)
colnames(BristonSummary)
colnames(Cornwall_summary)
head(CrimeRate_Bristol)
#population data

popn23=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/popn_2023.csv")

head(popn23)
# Filter the crime data for 2023 for both Cornwall and Bristol
cornwallCrimeRate_2023 = CrimeRate_Cornwall %>% 
  mutate(Year = as.numeric(format(ymd(Year), "%Y"))) %>% 
  filter(Year == 2023)

bristolCrimeRate_2023 = CrimeRate_Bristol %>%
  mutate(Year = as.numeric(format(ymd(Year), "%Y"))) %>% 
  filter(Year == 2023)

Cornwall_data_2023 = merge(cornwallCrimeRate_2023, popn23, by = "Postcode")
Bristol_data_2023 = merge(bristolCrimeRate_2023, popn23, by = "Postcode")
combined_data_2023 = bind_rows(Cornwall_data_2023, Bristol_data_2023)

combined_data_2023 = combined_data_2023 %>%
  filter(Crime.type == "Drugs") %>%  # Filter for drug offenses
  mutate(Drug_Offense_Rate = (1 / Population) * 100000)  # Calculate rate per 100,000 people

combined_data_2023$County = ifelse(combined_data_2023$Postcode %in% cornwallCrimeRate_2023$Postcode, "Cornwall", "Bristol")


library(ggplot2)
ggplot(combined_data_2023, aes(x = County, y = Drug_Offense_Rate)) +
  geom_boxplot() +
  labs(title = "Drug Offense Rate by County (2023)",
       x = "County",
       y = "Drug Offense Rate per 100,000 People")

#---------------------


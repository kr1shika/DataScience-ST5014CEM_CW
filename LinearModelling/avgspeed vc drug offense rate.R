#Average download speed vs Drug offense Rates per 10000 people
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)

Cornwall_crimeRate=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall_crimedata.csv")
Bristol_crimeRate=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/bristol_crimedata.csv")
Broadband=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband_combined.csv")
population_2023=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/popn_2023.csv")

head(Cornwall_crimeRate)
head(population_2023)

# Assuming 'postcode_space' is the common column for merging
merged_Cornwall <- merge(Cornwall_crimeRate, Broadband, by = "postcode_space")
merged_Bristol <- merge(Bristol_crimeRate, Broadband, by = "postcode_space")

# Merge crime data with broadband and population data
merged_Cornwall <- merge(merged_Cornwall, population_2023, by = "Postcode")
merged_Bristol <- merge(merged_Bristol, population_2023, by= "Postcode")

# Filter drug-related crimes and calculate drug offense rates
drug_crimes_Cornwall <- merged_Cornwall %>%
  filter(Crime.type == "Drugs") %>%
  group_by(postcode_space, Average.download.speed..Mbit.s.) %>%
  summarise(drug_offenses = n(), population = mean(Population)) %>%
  mutate(drug_offenses_per_10000 = (drug_offenses / population) * 10000)

drug_crimes_Bristol <- merged_Bristol %>%
  filter(Crime.type == "Drugs") %>%
  group_by(postcode_space, Average.download.speed..Mbit.s.) %>%
  summarise(drug_offenses = n(), population = mean(Population)) %>%
  mutate(drug_offenses_per_10000 = (drug_offenses / population) * 10000)



combined_data <- rbind(
  mutate(drug_crimes_Cornwall, Region = "Cornwall"),
  mutate(drug_crimes_Bristol, Region = "Bristol")
)

#now the plot
plot <- ggplot(combined_data, aes(x = Average.download.speed..Mbit.s., y = drug_offenses_per_10000, color = Region)) +
  geom_point(size = 1) +  # Adjust the point size
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Cornwall" = "#174A70", "Bristol" = "#ff7f0e")) +  
  labs(
    title = "Internet Speed vs. Drug-Related Crime: Cornwall vs. Bristol",
    x = "Average Download Speed (Mbit/s)",
    y = "Drug Offense Rates per 10,000 People"
  ) +
  theme_minimal()

interactive_plot <- ggplotly(plot)

# Show the plot
interactive_plot


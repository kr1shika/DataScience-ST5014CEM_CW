#att8scr vs House Price combined both counties 
library(tidyverse)
library(plotly)
library(ggplot2)

Cornwall_school=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/CornwallSchool.csv")
Bristol_school=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/BristolSchool.csv")
HousePrice <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv")

head(Cornwall_school)
head(Bristol_school)
head(HousePrice)

HousePrice_filtered <- HousePrice %>% filter(Year == 2023)

combined_schools <- bind_rows(Cornwall_school, Bristol_school)

combined_schools2023 =combined_schools %>% filter(Year == 2023)

combined_data2023 <- left_join(combined_schools2023, HousePrice_filtered, by = c("POSTCODE" = "Postcode"))

# Remove one of the Year columns
combined_data2023 <- combined_data2023 %>% select(-Year.y)
cleaned_data2023 <- combined_data2023 %>% filter(!is.na(Price) & !is.na(ATT8SCR))
cleaned_data2023 <- cleaned_data2023 %>% 
  distinct(POSTCODE, .keep_all = TRUE)

#-----------------------------------------

HousePrice_filtered22 <- HousePrice %>% filter(Year == 2022)

combined_schools22 <- bind_rows(Cornwall_school, Bristol_school)

combined_schools2022 =combined_schools22 %>% filter(Year == 2022)

combined_data2022 <- left_join(combined_schools2022, HousePrice_filtered22, by = c("POSTCODE" = "Postcode"))

combined_data2022 <- combined_data2022 %>% select(-Year.y)
cleaned_data2022 <- combined_data2022 %>% filter(!is.na(Price) & !is.na(ATT8SCR))
cleaned_data2022 <- cleaned_data2022 %>% 
  distinct(POSTCODE, .keep_all = TRUE)

#-----------------------------------------

Main_model <- bind_rows(cleaned_data2022, cleaned_data2023)
head(Main_model)

options(scipen = 1000)

# Create the plot
ggplot(Main_model, aes(x = as.numeric(ATT8SCR), y = as.numeric(Price), color = County)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Optional: add a linear regression line
  labs(
    title = "ATT8SCR vs. House Price for Cornwall and City of Bristol",
    x = "ATT8SCR",
    y = "House Price",
    color = "County"
  ) +
  theme_minimal()


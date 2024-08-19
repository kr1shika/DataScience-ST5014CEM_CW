#crime rate EDA
#install.packages("fmsb")
library(tidyverse)
library(ggplot2)
library(fmsb)
library(reshape2)
CrimeRate_Cornwall= read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall_crimedata.csv")
CrimeRate_Bristol= read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/cleanedData/CrimeData/bristol_crimedata.csv")
colnames(CrimeRate_Cornwall)
colnames(CrimeRate_Bristol)
head(CrimeRate_Bristol)
head(CrimeRate_Cornwall)

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


# CreatING THE GGPLOT
NARROWEDPLOT <- ggplot(combined_data_2023, aes(x = County, y = Drug_Offense_Rate)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(5, 140), breaks = seq(5, 200, by = 30)) +  # Set max to 200 and adjust breaks
  labs(title = "Drug Offense Rate by County (2023)",
       x = "County",
       y = "Drug Offense Rate per 100,000 People")

# Convert to interactive plot
ggplotly(NARROWEDPLOT)


# THE WHOLE PICTURE OF THE DATA DISTRIBUTION
FULLPLOT <- ggplot(combined_data_2023, aes(x = County, y = Drug_Offense_Rate)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 270), breaks = seq(5, 270, by = 50)) +  # Set max to 200 and adjust breaks
  labs(title = "Drug Offense Rate by County (2023)",
       x = "County",
       y = "Drug Offense Rate per 100,000 People")

# Convert to interactive plot
ggplotly(FULLPLOT)


#---------------------
#RADAR CHART: vehicle crime rate from 2020 to 2023 
vehicle_crime_bristol <- subset(CrimeRate_Bristol, Crime.type == "Vehicle crime")
vehicle_crime_cornwall <- subset(CrimeRate_Cornwall, Crime.type == "Vehicle crime")

combined_vehicle_crime <- rbind(vehicle_crime_bristol, vehicle_crime_cornwall)

combined_vehicle_crime$Year <- as.integer(format(as.Date(combined_vehicle_crime$Year), "%Y"))

# Filter the combined dataset for Bristol and Cornwall only
filtered_vehicle_crime <- combined_vehicle_crime[combined_vehicle_crime$city %in% c("Bristol, City of", "Cornwall"), ]

# Summarize the data by year and city
filtered_vehicle_crime_summary <- aggregate(Crime.ID ~ Year + city, data = filtered_vehicle_crime, FUN = length)

# Pivot the data to get the right format for the radar chart
filtered_vehicle_crime_pivot <- dcast(filtered_vehicle_crime_summary, Year ~ city, value.var = "Crime.ID", fill = 0)

# Adding row for max and min values (required by fmsb)
filtered_vehicle_crime_pivot <- rbind(
  max(filtered_vehicle_crime_pivot[-1]),
  min(filtered_vehicle_crime_pivot[-1]),
  filtered_vehicle_crime_pivot
)

# Create the radar chart for Bristol and Cornwall
radarchart(filtered_vehicle_crime_pivot, 
           axistype = 1, 
           pcol = rainbow(ncol(filtered_vehicle_crime_pivot)-1), 
           pfcol = alpha(rainbow(ncol(filtered_vehicle_crime_pivot)-1), 0.5), 
           plwd = 2, 
           title = "Vehicle Crime Rate from 2020 to 2023 (Bristol and Cornwall)")
#-------------------------------------------------------------------
#pie chart for specific month in 2023 [Robbery rate]
# Filter for robbery crimes in November 2023
PiechartModel <- rbind(CrimeRate_Cornwall, CrimeRate_Bristol)
november_robbery_2023 <- PiechartModel %>%
  filter(Crime.type == "Robbery" & format(as.Date(Year, "%Y-%m-%d"), "%Y-%m") == "2023-11")

# Add a county column based on the city
november_robbery_2023 <- november_robbery_2023 %>%
  mutate(county = ifelse(city == "Bristol, City of", "Bristol", "Cornwall"))

# Aggregate the number of robberies by city
robbery_summary_november <- november_robbery_2023 %>%
  group_by(county) %>%
  summarise(count = n())

# Create a pie chart for robbery rates in November 2023
pie(robbery_summary_november$count, 
    labels = robbery_summary_november$county, 
    main = "Robbery Rate in November 2023", 
    col = rainbow(length(robbery_summary_november$count)))

#-----------------------------------------------------------------

#DRUG OFFENSE RATE[LINE CHART] FOR BOTH COUNTIES (PER 10000 PEOPLE)

colnames(CrimeRate_Cornwall)
colnames(CrimeRate_Bristol)

CrimeRate_CornwallL = merge(CrimeRate_Cornwall, popn23, by = "Postcode")
CrimeRate_BristolL = merge(CrimeRate_Bristol, popn23, by = "Postcode")
Linegraph_model = bind_rows(CrimeRate_BristolL, CrimeRate_CornwallL)

Linegraph_model = Linegraph_model %>%
  filter(Crime.type == "Drugs") %>%  # Filter for drug offenses
  mutate(Drug_Offense_Rate = (1 / Population) * 10000)  %>% 
  filter(format(as.Date(Year), "%Y") == "2023") %>% 
  arrange(Year)
  
head(Linegraph_model)

Linegraph_model <- Linegraph_model %>%
  mutate(Year = as.Date(Year, format = "%Y-%m-%d"),
         Month = factor(format(Year, "%m"), levels = sprintf("%02d", 1:12))) %>%
  filter(city %in% c("Bristol, City of", "Cornwall")) %>%
  group_by(city, Month) %>%
  summarise(Total_Population = sum(Population),
            Total_Drug_Offenses = n(), # Assuming each row represents a single offense
            Drug_Offense_Rate = (Total_Drug_Offenses / Total_Population) * 10000) %>%
  ungroup()

ggplot(Linegraph_model, aes(x = Month, y = Drug_Offense_Rate, color = city, group = city)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(labels = month.name) +
  labs(title = "Drug Offense Rate per 10,000 People (Monthly) - 2023",
       x = "Month",
       y = "Drug Offense Rate per 10,000 People",
       color = "City") +
  theme_minimal()
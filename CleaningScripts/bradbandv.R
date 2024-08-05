library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband.csv")
hp_data <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv")

merged_data <- merge(data, hp_data, by = "county")

# Assuming your data frame is named 'broadband_data'
# Creating boxplot for Average Download Speeds in Both Counties
ggplot(data, aes(x = county, y = Avgdownload, fill = county)) +
  geom_boxplot() +
  labs(title = "Average Download Speeds in Both Counties",
       x = "County",
       y = "Average Download Speed (Mbps)") +
  theme_minimal()

# Filtering data for Bristol
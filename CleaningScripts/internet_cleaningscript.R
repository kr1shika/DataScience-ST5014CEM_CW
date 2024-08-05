library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

# Load the data
bb_performance <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/201809_fixed_pc_r03/201805_fixed_pc_performance_r03.csv")
bb_coverage <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/201809_fixed_pc_r03/201809_fixed_pc_coverage_r01.csv")
hp_data <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv")

# Clean broadband_perf Data
cleaned_perf <- bb_performance %>%
  mutate(shortPostcode = str_extract(postcode, "^[A-Z0-9]{2,4}")) %>%
  rowid_to_column("ID") %>%
  select(ID, postcode, shortPostcode, 
         Avgdownload = `Median.download.speed..Mbit.s.`, 
         AvgUpload = `Average.download.speed..Mbit.s.`, 
         Mindownload = `Minimum.download.speed..Mbit.s.`, 
         MinUpload = `Minimum.upload.speed..Mbit.s.`) %>%
  na.omit()
head(cleaned_perf)

# Clean broadband_cov Data
cleaned_cov <- bb_coverage %>%
  mutate(shortPostcode = str_extract(postcode, "^[A-Z0-9]{2,4}")) %>%
  rowid_to_column("ID") %>%
  select(ID, postcode, shortPostcode, 
         SFBB_availability = `SFBB.availability....premises.`, 
         UFBB_availability = `UFBB.availability....premises.`, 
         FTTP_availability = `FTTP.availability....premises.`, 
         unable_to_receive_2Mbit = `X..of.premises.unable.to.receive.2Mbit.s`, 
         unable_to_receive_5Mbit = `X..of.premises.unable.to.receive.5Mbit.s`, 
         unable_to_receive_10Mbit = `X..of.premises.unable.to.receive.10Mbit.s`, 
         unable_to_receive_30Mbit = `X..of.premises.unable.to.receive.30Mbit.s`, 
         unable_meet_USO = `X..of.premises.unable.meet.USO`, 
         decent_broadband_FWA = `X..of.premises.able.to.receive.decent.broadband.from.FWA`, 
         SFBB_FWA = `X..of.premises.able.to.receive.SFBB.from.FWA`, 
         NGA = `X..of.premises.able.to.receive.NGA`) %>%
  na.omit()
head(cleaned_cov)

# Clean county data
hp_data <- hp_data %>%
  mutate(shortPostcode = str_extract(PostCode, "^[A-Z0-9]{2,4}")) %>%
  select(shortPostcode, County)
head(hp_data)

# Verify that shortPostcode values match
print(unique(cleaned_perf$shortPostcode))
print(unique(cleaned_cov$shortPostcode))
print(unique(hp_data$shortPostcode))

# Merge the Cleaned Data
combined_broadband_data <- full_join(cleaned_perf, cleaned_cov, by = "shortPostcode")
head(combined_broadband_data)

# Merge with county data
combined_broadband_data <- full_join(combined_broadband_data, hp_data, by = "shortPostcode")
head(combined_broadband_data)

# Save the cleaned and combined data
write_csv(combined_broadband_data, "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband_with_county.csv")

# Check structure of the final combined data
str(combined_broadband_data)

# Plotting
ggplot(combined_broadband_data, aes(x = County, y = Avgdownload, fill = County)) +
  geom_boxplot() +
  labs(title = "Average Download Speeds in Both Counties",
       x = "County",
       y = "Average Download Speed (Mbps)") +
  theme_minimal()

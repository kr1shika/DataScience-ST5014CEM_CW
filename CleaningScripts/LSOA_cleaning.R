#cleaning for LSOA 

#install.packages("data.table")
library(data.table)
library(tidyverse)
library(dplyr)

house_prices <- read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv")
LSOA = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/Postcode to LSOA.csv")
head(LSOA)

LSOA <- LSOA %>%
  filter(!is.na(pcd7)) %>%
  rename(
    postcode = pcd7,
    lsoa_code = lsoa11cd,
    lsoa_name = lsoa11nm,
  ) 

pattern = ' .*$'
LSOA_Cleaned = LSOA %>%
  select(lsoa_code,postcode) %>% 
  mutate(shortPostcode=gsub(pattern,"",postcode)) %>% 
  right_join(house_prices,by="shortPostcode")  %>% 
  group_by(lsoa_code) %>% 
  select(lsoa_code,shortPostcode, postcode,County,'Town/City') 

LSOA_Cleaned

colnames(house_prices)
broadband=read.csv( "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband_combined.csv")

broadband

LSOA_Cleaned <- LSOA_Cleaned %>%
  distinct() %>%  # Remove duplicate rows
  drop_na()   
nrow(LSOA_Cleaned)

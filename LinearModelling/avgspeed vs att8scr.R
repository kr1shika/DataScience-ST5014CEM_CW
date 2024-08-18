#average speed vs attainment 8 score 
library(tidyverse)
library(plotly)
library(ggplot2)
Broadband=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband_combined.csv")
Cornwall_school=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/CornwallSchool.csv")
Bristol_school=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/BristolSchool.csv")

head(Broadband)
head(Cornwall_school)
head(Bristol_school)

unique_cornwall_postcodes <- unique(Cornwall_school$POSTCODE)
unique_bristol_postcodes <- unique(Bristol_school$POSTCODE)
unique_broadband_postcodes <- unique(Broadband$postcode_space)

intersect(unique_cornwall_postcodes, unique_broadband_postcodes)
intersect(unique_bristol_postcodes, unique_broadband_postcodes)

Cornwall_matched <- Broadband %>% filter(postcode_space %in% intersect(unique_cornwall_postcodes, unique_broadband_postcodes))
Bristol_matched <- Broadband %>% filter(postcode_space %in% intersect(unique_bristol_postcodes, unique_broadband_postcodes))

Cornwall_school_merged <- merge(Cornwall_school, Cornwall_matched, by.x="POSTCODE", by.y="postcode_space")
Bristol_school_merged <- merge(Bristol_school, Bristol_matched, by.x="POSTCODE", by.y="postcode_space")

Cornwall_school_merged$Region <- "Cornwall"
Bristol_school_merged$Region <- "Bristol"

combined_data <- rbind(Cornwall_school_merged, Bristol_school_merged)

model <- lm(ATT8SCR ~ Average.download.speed..Mbit.s., data=combined_data)
summary(model)

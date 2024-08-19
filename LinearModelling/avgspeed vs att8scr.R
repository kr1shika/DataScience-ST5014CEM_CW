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

# Filter out rows where ATT8SCR is "SUPP" or "NE" and clean the data
combined_data <- combined_data %>%
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%
  # Additionally, ensure there are no NA, NaN, or Inf values
  filter(!is.na(ATT8SCR) & !is.na(Average.download.speed..Mbit.s.) &
           !is.nan(ATT8SCR) & !is.nan(Average.download.speed..Mbit.s.) &
           !is.infinite(ATT8SCR) & !is.infinite(Average.download.speed..Mbit.s.))

combined_data$ATT8SCR <- as.numeric(combined_data$ATT8SCR)

model <- lm(ATT8SCR ~ Average.download.speed..Mbit.s., data=combined_data)
summary(model)


Internet_school_plot <- ggplot(combined_data, aes(x=Average.download.speed..Mbit.s., y=ATT8SCR, color=Region)) +
  geom_point(size=1) +
  geom_smooth(method="lm", se=FALSE) +
  scale_color_manual(values = c("Cornwall" = "#174A70", "Bristol" = "#ff7f0e")) +  # Correct color mapping
  labs(title="Attainment 8 Score vs Average Download Speed: Cornwall vs Bristol",
       x="Average Download Speed (Mbit/s)",
       y="Attainment 8 Score") +
  theme_minimal()
interactive_plot <- ggplotly(Internet_school_plot)
interactive_plot

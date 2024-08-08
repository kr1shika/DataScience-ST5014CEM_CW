library(tidyverse)
library(plotly)

# Load the combined broadband data
bcBroadbandSpeed = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband_combined.csv")
colnames(bcBroadbandSpeed)

#I am using this to prevent scientific notations in the chart 
options(scipen = 1000)

BroadbandSpeedBoxPlot = bcBroadbandSpeed %>% 
  mutate(County = case_when(
    `postcode area` == "BS" ~ "CITY OF BRISTOL",
    `postcode area` %in% c("PL", "TR") ~ "CORNWALL")) %>% 
  ggplot(aes(x = County, y = `Average.download.speed..Mbit.s.`, fill = County)) + 
  geom_boxplot() +
  labs(title = "Average Download Speed in Counties of Bristol and Cornwall",
       x = "County",
       y = "Average Download Speed (Mbit/s)") +
  scale_fill_manual(values = c("CITY OF BRISTOL" = "#f39c12", "CORNWALL" = "#34495e")) +
  theme_minimal()
print(BroadbandSpeedBoxPlot)


# for interactive plot, 
  # when hovered over the elements of the plot i can view the values as well.
ggplotly(BroadbandSpeedBoxPlot, tooltip = "text")

# BAR CHARTS: AVERAGE AND MAXIMUM DOWNLOAD SPEEDS
# Bristol
bcBroadbandSpeed %>%
  filter(`postcode area` == "BS") %>%
  group_by(postcode_space) %>%
  # Calculate average and maximum speed for each postcode space
  summarise(
    avg_speed = mean(Average.download.speed..Mbit.s., na.rm = TRUE),
    max_speed = max(Maximum.download.speed..Mbit.s., na.rm = TRUE)
  ) %>%
  slice_sample(n = 50) %>%
  # Combine the speed columns into one for easier plotting
  pivot_longer(cols = c(avg_speed, max_speed), names_to = "speed_type", values_to = "speed") %>%
  ggplot(aes(x = postcode_space, y = speed, fill = speed_type)) +
  # Create a grouped bar chart
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average and Maximum Download Speeds across Postcode Spaces of Bristol",
    x = "Postcode Space",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  scale_fill_manual(values = c("avg_speed" = "#f39c12", "max_speed" = "#34495e")) +
  
  theme_minimal() +
  # Rotate x-axis text for better readability
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Cornwall
bcBroadbandSpeed %>%
  filter(`postcode area` %in% c("TR", "PL")) %>%
  group_by(`postcode_space`) %>%
  summarise(
    avg_speed = mean(`Average.download.speed..Mbit.s.`, na.rm = TRUE),
    max_speed = max(`Maximum.download.speed..Mbit.s.`, na.rm = TRUE)
  ) %>%
  slice_sample(n = 60) %>%
  pivot_longer(cols = c(avg_speed, max_speed), names_to = "speed_type", values_to = "speed") %>%
  ggplot(aes(x = `postcode_space`, y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average and Maximum Download Speeds across Postcode Spaces of Cornwall",
    x = "Postcode Space",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  scale_fill_manual(values = c("avg_speed" = "#f39c12", "max_speed" = "#34495e")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





#-------------TRIAL 

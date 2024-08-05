# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the cleaned and saved CSV file
houseprice_data <- read.csv('C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv')
head(houseprice_data)
# Check the structure of the dataset
# str(houseprice_data)

# Convert the Year column to numeric type
# houseprice_data$Year <- as.numeric(houseprice_data$Year)

# Filter data for the year 2023
data_2023 <- houseprice_data %>% filter(Year == 2023)

# Custom function to format y-axis labels
format_y <- function(x) {
  return(format(x, big.mark = ",", scientific = FALSE))
}

# BOX PLOT
# Boxplot with adjusted y-axis limits [adjusted to show the full distribution]
full_boxplot_2023 <- ggplot(data_2023, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  labs(title = "Average House Price in Year 2023 (Boxplot with Adjusted Limits)",
       x = "County",
       y = "Price") +
  coord_cartesian(ylim = c(230000, 7650000)) + 
  scale_fill_manual(values = c("CITY OF BRISTOL" = "#184A70", "CORNWALL" = "#4A3B5E")) +
  scale_y_continuous(labels = format_y) +
  theme_minimal()
print(full_boxplot_2023)

# Boxplot with specific y-axis limits
avgboxplot_2023 <- ggplot(data_2023, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  labs(title = "Average House Price in Year 2023",
       x = "County",
       y = "Price") +
  coord_cartesian(ylim = c(200000, 500000)) + 
  scale_fill_manual(values = c("CITY OF BRISTOL" = "#184A70", "CORNWALL" = "#4A3B5E")) +  # Change the colors here
  scale_y_continuous(labels = format_y) +
  theme_minimal()
print(avgboxplot_2023)


# Bar graph
data_2023 <- houseprice_data %>% filter(Year == 2023) %>%
  group_by(County) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))

bar_chart_2023 <- ggplot(data_2023, aes(x = County, y = Average_Price, fill = County)) +
  geom_bar(stat = "identity") +
  labs(title = "Average House Price in Year 2023",
       x = "County",
       y = "Average Price") +
  scale_fill_manual(values = c("CITY OF BRISTOL" = "#184A70", "CORNWALL" = "#4A3B5E")) +
  scale_y_continuous(labels = format_y) +
  theme_minimal()
print(bar_chart_2023)

# 2. Average House Price From 2020 to 2023 (Line Chart) – For both counties
data_2020_2023 <- houseprice_data %>% filter(Year >= 2020 & Year <= 2023) %>%
  group_by(Year, County) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))

line_chart_2020_2023 <- ggplot(data_2020_2023, aes(x = Year, y = Average_Price, color = County)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Average House Price From 2020 to 2023",
       x = "Year",
       y = "Average Price") +
  scale_color_manual(values = c("CITY OF BRISTOL" = "#184A70", "CORNWALL" = "#4A3B5E")) +
  scale_y_continuous(labels = format_y) +
  theme_minimal()
print(line_chart_2020_2023)

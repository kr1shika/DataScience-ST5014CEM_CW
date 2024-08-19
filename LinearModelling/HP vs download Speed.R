library(tidyverse)
library(lubridate)

#LOADING THE DATASET
HousePrice <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/HPbristolcornwall.csv")
BroadBand <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband_combined.csv")

# BRISTOL [ we are working on 2023's data only]
bristol_hp_data <- HousePrice %>% filter(County == "CITY OF BRISTOL", Year == 2023)
bristol_bb_data <- BroadBand %>% filter(postcode.area == "BS")

# selecting the needed colmuns: house prices and average download speed.
bristol_data_2023 <- inner_join(bristol_hp_data, bristol_bb_data, by = c("Postcode" = "postcode_space")) %>%
  select(HousePrice = Price, DownloadSpeed = `Average.download.speed..Mbit.s.`)

# now to understand the relationship between house prices and broadband speed, we calculate the correlation coefficient.
correlation_result_bristol <- bristol_data_2023 %>%
  summarise(correlation_coefficient = cor(HousePrice, DownloadSpeed, use = "complete.obs"))
print(paste("Correlation coefficient for Bristol in 2023:", correlation_result_bristol$correlation_coefficient))

# fit a linear model to predict house prices based on broadband speed.
lm_model_bristol <- lm(HousePrice ~ DownloadSpeed, data = bristol_data_2023)
print(summary(lm_model_bristol))

# extracting the intercept and slope from our linear model.  helps us form regression line.
intercept_value_bristol <- coef(lm_model_bristol)[1]
slope_value_bristol <- coef(lm_model_bristol)[2]

# Scatter plot of house prices vs download speed
plot_bristol=ggplot(bristol_data_2023, aes(x = DownloadSpeed, y = HousePrice)) +
  geom_point(color = "darkgreen", size = 1) +   
  geom_abline(intercept = intercept_value_bristol, slope = slope_value_bristol, color = "orange") +      
  labs(
    title = "Impact of Average Download Speed on House Price in Bristol (2023)",
    x = "Average Download Speed (Mbit/s)",
    y = "House Price"
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 25000000)) +  # Adjust limits as needed
  theme_minimal()

#_____________________________________________________________________________________________________________________

#CORNWALL
cornwall_hp_data <- HousePrice %>% filter(County == "CORNWALL", Year == 2023)
cornwall_bb_data <- BroadBand %>% filter(postcode.area %in% c("TR", "PL"))

# mergING the Cornwall house price data with the corresponding broadband data.
cornwall_data_2023 <- inner_join(cornwall_hp_data, cornwall_bb_data, by = c("Postcode" = "postcode_space")) %>%
  select(HousePrice = Price, DownloadSpeed = `Average.download.speed..Mbit.s.`)

correlation_result_cornwall <- cornwall_data_2023 %>%
  summarise(correlation_coefficient = cor(HousePrice, DownloadSpeed, use = "complete.obs"))
print(paste("Correlation coefficient for Cornwall in 2023:", correlation_result_cornwall$correlation_coefficient))

# fit a linear model to predict house prices based on broadband speed.
lm_model_cornwall <- lm(HousePrice ~ DownloadSpeed, data = cornwall_data_2023)
print(summary(lm_model_cornwall))

# ExtractING the intercept and slope from the Cornwall model to use them in our plot.
intercept_value_cornwall <- coef(lm_model_cornwall)[1]
slope_value_cornwall <- coef(lm_model_cornwall)[2]

# laSTLY, THE SCATTER PLOT
plot_cornwall <- ggplot(cornwall_data_2023, aes(x = DownloadSpeed, y = HousePrice)) +
  geom_point(color = "darkblue", size = 1) +
  geom_abline(intercept = intercept_value_cornwall, slope = slope_value_cornwall, color = "orange") +
  labs(
    title = "Impact of Average Download Speed on House Price in Cornwall (2023)",
    x = "Average Download Speed (Mbit/s)",
    y = "House Price"
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000000)) +  
  theme_minimal()

grid.arrange(plot_bristol, plot_cornwall, nrow = 2)

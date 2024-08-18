#schoolEDA
library(tidyverse)
library(plotly)

Cornwall_school=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/CornwallSchool.csv")
Bristol_school=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/BristolSchool.csv")

options(scipen=1000)

#checking the summary 
summary(Cornwall_school)
summary(Bristol_school)
str(Bristol_school)

Bristol_school <- Bristol_school %>%
  mutate(Year = as.character(Year))

Cornwall_school <- Cornwall_school %>%
  mutate(Year = as.character(Year))

str(Bristol_school)

#Average attainment 8 score of 2023 for both counties [BOX PLOT]
att8scr_boxplot=bind_rows(Bristol_school,Cornwall_school) %>% 
  filter(Year == 2023 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  #The values of ATT8SCR are converted to numeric form
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  #Grouped by SCHNAME to calculate average ATT8SCR of each school
  group_by(SCHNAME, COUNTY) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>% 
  ggplot(aes(x = COUNTY, y = avg_ATT8SCR, fill = COUNTY)) +
  geom_boxplot() +
  labs(title = "Distribution of Average Attainment 8 Scores in the Academic Session 2021-2022",
       x = "County",
       y = "Attainment 8 Scores") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggplotly(att8scr_boxplot, tooltip = "text")  


#line graph 
Bristol_school %>% 
  filter(Year == 2023 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(SCHNAME) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>%
  ggplot(aes(x = SCHNAME, y = avg_ATT8SCR, group = 1)) +
  geom_line() +
  geom_point() + 
  labs(title = "Average Attainment 8 Scores across Schools of Bristol in 2022-2023",
       x = "Schools",
       y = "Average Attainment 8 Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))

#line graph cornwall 
Cornwall_school %>% 
  filter(Year == 2023 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(SCHNAME) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>%
  ggplot(aes(x = SCHNAME, y = avg_ATT8SCR, group = 1)) +
  geom_line() +
  geom_point() + 
  labs(title = "Average Attainment 8 Scores across Schools of Cornwall in 2022-2023",
       x = "Schools",
       y = "Average Attainment 8 Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))

  

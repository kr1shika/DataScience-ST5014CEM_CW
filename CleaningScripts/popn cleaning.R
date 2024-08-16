#population cleaning 
library(tidyverse)

#Data set of population from 2011 according to postcodes
population2011 = read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/Population2011_1656567141570.csv")
#View(population2011)
dim(population2011)
colSums(is.na(population2011))
colnames(population2011)
str(population2011)
#Population of 2011 is converted to that of 2023

years_between = 2023 - 2011
annual_growth_rate = 1.00561255390388033 

population2023 = population2011 %>% 
  mutate(Population = as.numeric(gsub(",", "", Population)) * (annual_growth_rate ^ years_between))

colSums(is.na(population2023))

#saving it
write.csv(population2023,"C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/popn_2023.csv", row.names = FALSE)

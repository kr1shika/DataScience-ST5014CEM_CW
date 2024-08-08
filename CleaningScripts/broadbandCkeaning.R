library(tidyverse)
library(readr)
library(dplyr)

bb_performance <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/boardbandData/201805_fixed_pc_performance_r03.csv")

View(bb_performance)
dim(bb_performance)

colnames(bb_performance)

summary(bb_performance)
colSums(is.na(bb_performance))

str(bb_performance)

bb_performance = bb_performance %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.), 
                         ifelse(sum(is.na(.)) > 0.9 * length(.), 0, median(., na.rm = TRUE)), 
                         .)))

View(bb_performance)
colSums(is.na(bb_performance))

sum(duplicated(bb_performance))

#The summary statistics of the data set is checked again to see any changes due to the removal of missing values
#The summary also shows a huge difference between 3rd quartile and max outlier
#Such outliers can influence the data set and cause skewness in visualization
summary(bb_performance)
colnames(bb_performance)

ggplot(bb_performance, aes(x = `Average.data.usage..GB.`)) +
  geom_histogram() +
  ggtitle("Original Average Data Usage Distribution")

#Log transformation has been done to make highly skewed distribution less skewed
#and to normalize or adjust the scale of the column's values
#The natural logarithm of the values are calculated
#1 is added to avoid issues when the data contains zeros (since log(0) is undefined)
summary(bb_performance)

bb_performance <- bb_performance %>%
  mutate(
    `Number.of.connections.5.10.Mbit.s..number.of.lines.` = log(`Number.of.connections.5.10.Mbit.s..number.of.lines.` + 1),
    `Number.of.connections.10.30.Mbit.s..number.of.lines..` = log(`Number.of.connections.10.30.Mbit.s..number.of.lines..` + 1),
    `Number.of.connections....30.Mbit.s..number.of.lines.` = log(`Number.of.connections....30.Mbit.s..number.of.lines.` + 1),
    `Median.download.speed..Mbit.s.` = log(`Median.download.speed..Mbit.s.` + 1),
    `Average.download.speed..Mbit.s.` = log(`Average.download.speed..Mbit.s.` + 1),
    `Minimum.download.speed..Mbit.s.` = log(`Minimum.download.speed..Mbit.s.` + 1),
    `Maximum.download.speed..Mbit.s.` = log(`Maximum.download.speed..Mbit.s.` + 1),
    `Median.upload.speed..Mbit.s.` = log(`Median.upload.speed..Mbit.s.` + 1),
    `Average.upload.speed..Mbit.s.` = log(`Average.upload.speed..Mbit.s.` + 1),
    `Minimum.upload.speed..Mbit.s.` = log(`Minimum.upload.speed..Mbit.s.` + 1),
    `Maximum.upload.speed..Mbit.s.` = log(`Maximum.upload.speed..Mbit.s.` + 1),
    `Average.upload.speed..Mbit.s..for.lines.10.30Mbit.s` = log(`Average.upload.speed..Mbit.s..for.lines.10.30Mbit.s` + 1),
    `Average.upload.speed..Mbit.s..for.SFBB.lines` = log(`Average.upload.speed..Mbit.s..for.SFBB.lines` + 1),
    `Average.data.usage..GB..for.lines...10Mbit.s` = log(`Average.data.usage..GB..for.lines...10Mbit.s` + 1),
    `Average.data.usage..GB..for.Basic.BB.lines` = log(`Average.data.usage..GB..for.Basic.BB.lines` + 1),
    `Average.data.usage..GB..for.SFBB.lines` = log(`Average.data.usage..GB..for.SFBB.lines` + 1),
    `Average.data.usage..GB.` = log(`Average.data.usage..GB.` + 1)
  )

# View the updated summary statistics
summary(bb_performance)
colSums(is.na(bb_performance))

bb_performance = bb_performance %>% 
  rename(`postcode area` = `postcode.area`)

internet_performance_filtered <- bb_performance %>%
  filter(`postcode area` %in% c("BS", "TR", "PL"))
view(internet_performance_filtered)

colSums(is.na(internet_performance_filtered))

#FOR BROADBAND SPEED COVERAGE--------------------------------------------------------------------------------------------------------------------------------

broadband_coverage <- read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/boardbandData/201809_fixed_pc_coverage_r01.csv")
dim(broadband_coverage)
colnames(broadband_coverage)

summary(broadband_coverage)

colSums(is.na(broadband_coverage)) #showed no missing values in any column of the data set
colSums(is.na(broadband_coverage))
str(broadband_coverage)

broadband_coverage <- broadband_coverage %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.), 
                         ifelse(sum(is.na(.)) > 0.9 * length(.), 0, median(., na.rm = TRUE)), 
                         .)))
View(broadband_coverage)
colSums(is.na(broadband_coverage))

sum(duplicated(broadband_coverage))
summary(broadband_coverage)

broadband_coverage <- broadband_coverage %>%
  mutate(
    `SFBB.availability....premises.` = log(`SFBB.availability....premises.` + 1),
    `UFBB.availability....premises.` = log(`UFBB.availability....premises.` + 1),
    `FTTP.availability....premises.` = log(`FTTP.availability....premises.` + 1),
    `X..of.premises.unable.to.receive.2Mbit.s` = log(`X..of.premises.unable.to.receive.2Mbit.s` + 1),
    `X..of.premises.unable.to.receive.5Mbit.s` = log(`X..of.premises.unable.to.receive.5Mbit.s` + 1),
    `X..of.premises.unable.to.receive.10Mbit.s` = log(`X..of.premises.unable.to.receive.10Mbit.s` + 1),
    `X..of.premises.unable.to.receive.30Mbit.s` = log(`X..of.premises.unable.to.receive.30Mbit.s` + 1),
    `X..of.premises.unable.meet.USO` = log(`X..of.premises.unable.meet.USO` + 1),
    `X..of.premises.able.to.receive.decent.broadband.from.FWA` = log(`X..of.premises.able.to.receive.decent.broadband.from.FWA` + 1),
    `X..of.premises.able.to.receive.SFBB.from.FWA` = log(`X..of.premises.able.to.receive.SFBB.from.FWA` + 1),
    `X..of.premises.able.to.receive.NGA` = log(`X..of.premises.able.to.receive.NGA` + 1)
  )

summary(broadband_coverage)
colSums(is.na(broadband_coverage))
colnames(broadband_coverage)
broadband_coverage = broadband_coverage %>% 
  rename(`postcode_space` = `pcds`, 
         `postcode area` = `pca`)

broadband_coverage_filtered <- broadband_coverage %>%
  filter(`postcode area` %in% c("BS", "TR", "PL"))
#checking
view(broadband_coverage_filtered)

#-------------------------------------------------------
colnames(broadband_coverage_filtered)
# Merge the cleaned datasets using inner join
combined_broadband_data <- inner_join(internet_performance_filtered, broadband_coverage_filtered, 
                                      by = c("postcode", "postcode_space", "postcode area"))

colnames(combined_broadband_data)

# Select relevant columns
combined_broadband_data <- combined_broadband_data %>% 
  select(postcode, postcode_space, `postcode area`, 
         `Average.download.speed..Mbit.s.`, `Maximum.download.speed..Mbit.s.`, `Minimum.download.speed..Mbit.s.`,
         `Average.upload.speed..Mbit.s.`, `Maximum.upload.speed..Mbit.s.`, `Minimum.upload.speed..Mbit.s.`,
         `Average.data.usage..GB.`, `All.Premises`, `All.Matched.Premises`)

# View the combined data
colSums(is.na(combined_broadband_data))
dim(combined_broadband_data)

# Save the combined data
write.csv(combined_broadband_data, "C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/cleanedBroadband_combined.csv", row.names = FALSE)






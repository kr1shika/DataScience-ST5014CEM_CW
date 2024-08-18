#SCHOOL DATA CLEANING 
library(readr)
library(dplyr)

# BRISTOL SCHOOLS IN 2022
bsKs4Final22 = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/School dataset/city of bristol/Performancetables_151334/2021-2022/801_ks4final.csv")
bsKs4Final22 = bsKs4Final22 %>% 
  mutate(Year = "2022") %>% 
  select(PCODE, Year, SCHNAME, ATT8SCR) %>% 
  drop_na(SCHNAME) %>% 
  drop_na(ATT8SCR) %>% 
  rename(POSTCODE = PCODE)

bsSchoolInfo22 = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/School dataset/city of bristol/Performancetables_151334/2021-2022/801_school_information.csv")
bsSchoolInfo22 = bsSchoolInfo22 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)

bsSchools22 = inner_join(bsKs4Final22, bsSchoolInfo22, by = c("POSTCODE", "SCHNAME")) %>%
  filter(TOWN == "Bristol" | is.na(TOWN)) %>%
  mutate(TOWN = replace_na(TOWN, "Bristol"))

# BRISTOL SCHOOLS IN 2023_______________________________________________________________________________________________________
bsKs4Final23 = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/School dataset/city of bristol/Performancetables_151556/2022-2023/801_ks4final.csv")
bsKs4Final23 = bsKs4Final23 %>% 
  mutate(Year = "2023") %>% 
  select(PCODE, Year, SCHNAME, ATT8SCR) %>% 
  drop_na(SCHNAME) %>% 
  drop_na(ATT8SCR) %>% 
  rename(POSTCODE = PCODE)

bsSchoolInfo23 = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/School dataset/city of bristol/Performancetables_151556/2022-2023/801_school_information.csv")
bsSchoolInfo23 = bsSchoolInfo23 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)

bsSchools23 = inner_join(bsKs4Final23, bsSchoolInfo23, by = c("POSTCODE", "SCHNAME")) %>%
  filter(TOWN == "Bristol" | is.na(TOWN)) %>%
  mutate(TOWN = replace_na(TOWN, "Bristol"))

# CORNWALL SCHOOLS IN 2022____________________________________________
cwKs4Final22 = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/School dataset/Cornwall/Performancetables_151934/2021-2022/908_ks4final.csv")
colnames(cwKs4Final22)
cwKs4Final22 = cwKs4Final22 %>% 
  mutate(Year = "2022") %>% 
  select(PCODE, Year, SCHNAME, ATT8SCR) %>% 
  drop_na(SCHNAME) %>% 
  rename(POSTCODE = PCODE)

cwSchoolInfo22 = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/School dataset/Cornwall/Performancetables_151934/2021-2022/908_school_information.csv")
cwSchoolInfo22 = cwSchoolInfo22 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)

cwSchools22 = inner_join(cwKs4Final22, cwSchoolInfo22, by = c("POSTCODE", "SCHNAME")) %>%
  mutate(TOWN = if_else(is.na(TOWN) & SCHNAME == "The Lowen School", "Gunnislake", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Red Moor School", "Lanlivery", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Taliesin Education Ltd", "Liskeard", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Truro School", "Truro", TOWN))

bristolSchools = bind_rows(bsSchools22, bsSchools23)

# CORNWALL SCHOOLS IN 2023_____________________________________________________________________
cwKs4Final23 = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/School dataset/Cornwall/Performancetables_152035/2022-2023/908_ks4final.csv")
cwKs4Final23 = cwKs4Final23 %>% 
  mutate(Year = "2023") %>% 
  select(PCODE, Year, SCHNAME, ATT8SCR) %>% 
  drop_na(SCHNAME) %>% 
  rename(POSTCODE = PCODE)

cwSchoolInfo23 = read_csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/School dataset/Cornwall/Performancetables_152035/2022-2023/908_school_information.csv")
cwSchoolInfo23 = cwSchoolInfo23 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)

cwSchools23 = inner_join(cwKs4Final23, cwSchoolInfo23, by = c("POSTCODE", "SCHNAME")) %>%
  mutate(TOWN = if_else(is.na(TOWN) & SCHNAME == "The Lowen School", "Gunnislake", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Red Moor School", "Lanlivery", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Taliesin Education Ltd", "Liskeard", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Truro School", "Truro", TOWN))

cornwallSchools = bind_rows(cwSchools22, cwSchools23)

#now merging-------


# Cleaning Bristol Schools Data
mode_ofstedrating = bristolSchools %>%
  filter(!is.na(OFSTEDRATING)) %>%
  count(OFSTEDRATING) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(OFSTEDRATING)

bristolSchools = bristolSchools %>% 
  mutate(COUNTY = "City of Bristol") %>% 
  mutate(OFSTEDRATING = if_else(is.na(OFSTEDRATING), mode_ofstedrating, OFSTEDRATING))

# Cleaning Cornwall Schools Data
mode_ofstedrating_cw = cornwallSchools %>%
  filter(!is.na(OFSTEDRATING)) %>%
  count(OFSTEDRATING) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(OFSTEDRATING)

cornwallSchools = cornwallSchools %>% 
  mutate(COUNTY = "Cornwall") %>% 
  mutate(OFSTEDRATING = if_else(is.na(OFSTEDRATING), mode_ofstedrating_cw, OFSTEDRATING))

write.csv(cornwallSchools,"C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/CornwallSchool.csv")
write.csv(bristolSchools,"C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/SchoolData/BristolSchool.csv")

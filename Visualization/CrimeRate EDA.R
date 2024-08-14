#crime rate EDA
#•	Drug offence rate in both counties towns or districts in the year 2022 ( Box- plot)
#•	Vehicle Crime Rate per 10000 people in the Specific month of your choice in year 2022– (Radar Chart) check GraphR
#•	Robbery crime rate per 10000 people in the specific month of your choice in year 2022 – (Pie Chart)
#•	Drug offense rate per 10000 people in both counties (Line Chart) , should be shown in one single image


CrimeRate_Cornwall= read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall-crime-rate.csv")
CrimeRate_Bristol= read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/CrimeData/bristol-crime-rate.csv")
Cornwall_summary=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/CleanedData/CrimeData/cornwall-crime-summary.csv")
BristonSummary=read.csv("C:/Users/ASUS/OneDrive/Desktop/semester_4th/DS_assignment/obtained/CrimeData/bristol-crime-summary.csv")
colnames(CrimeRate_Cornwall)
colnames(CrimeRate_Bristol)
colnames(BristonSummary)
colnames(Cornwall_summary)
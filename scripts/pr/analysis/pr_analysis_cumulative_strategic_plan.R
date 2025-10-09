# PR FLOW - Cumulative strategic ----

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/pr/", Sys.Date(), "/analysis/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# Load ----
df <- read.csv("data/cleaned/PR Combiner - For Analysis.csv", encoding = "UTF-8")


## clean the level_codes
df %<>%
  mutate(Type = trimws(level_code))


## Filtering ----
# saving a version for the numer of programs with data analysis
root <- df

# remove NA optimization phase
df <- df %>%
  filter(exercice == cumulative & ActiveStrategicPlan == "Yes" &
           (unit == "Number #" | is.na(unit)))

## Totals per investment and gender ----
totals_gender <- df %>%
  group_by(Typeofinvestment, Type, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(ComputedTotal = Female + Male,
         BeneficiariesWithoutDisaggregation = Total - ComputedTotal,
         PerFemale = Female / Total,
         PerMale = Male / Total,
         PerUnknown = BeneficiariesWithoutDisaggregation / Total)
totals_gender

# addWorksheet(wb, "GenderInvestment")
# writeData(wb, sheet = "GenderInvestment", totals_gender, colNames = TRUE, rowNames = FALSE)

## Totals per investment and Level ----
totals_type <- df %>%
  group_by(Typeofinvestment, Type, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Type, values_from = Total) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Child = if("Child" %in% colnames(.)) Child else 0,
         School = if("School" %in% colnames(.)) School else 0,
         Teacher = if("Teacher" %in% colnames(.)) Teacher else 0,
         Caregiver = if("Caregiver" %in% colnames(.)) Caregiver else 0,
         TotalKnown = Child + Teacher + School + Caregiver,
         PerChildren = Child / TotalKnown,
         PerTeachers = Teacher / TotalKnown,
         PerSchools = School / TotalKnown,
         PerCaregivers = Caregiver / TotalKnown) %>%
  rename(Children = Child,
         Teachers = Teacher,
         Schools = School,
         Caregivers = Caregiver)
totals_type

# addWorksheet(wb, "TypeInvestment")
# writeData(wb, sheet = "TypeInvestment", totals_type, colNames = TRUE, rowNames = FALSE)

## Totals per investment, code and gender ----
totals_gender <- df %>%
  group_by(Typeofinvestment, code, Type, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(ComputedTotal = Female + Male,
         BeneficiariesWithoutDisaggregation = Total - ComputedTotal,
         PerFemale = Female / Total,
         PerMale = Male / Total,
         PerUnknown = BeneficiariesWithoutDisaggregation / Total) %>%
  mutate(BeneficiariesWithoutDisaggregation =
           ifelse(Type == "School", 0, BeneficiariesWithoutDisaggregation),
         PerUnknown = 
           ifelse(Type == "School", 0, PerUnknown))
totals_gender

totals_totals_gender <- df %>%
  group_by(code, Type, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(ComputedTotal = Female + Male,
         BeneficiariesWithoutDisaggregation = Total - ComputedTotal,
         PerFemale = Female / Total,
         PerMale = Male / Total,
         PerUnknown = BeneficiariesWithoutDisaggregation / Total,
         Typeofinvestment = "TOTAL") %>%
  mutate(BeneficiariesWithoutDisaggregation =
           ifelse(Type == "School", 0, BeneficiariesWithoutDisaggregation),
         PerUnknown = 
           ifelse(Type == "School", 0, PerUnknown)) %>%
  select(Typeofinvestment, code, Type, Female, Male, Total, ComputedTotal, BeneficiariesWithoutDisaggregation, 
         PerFemale, PerMale, PerUnknown)
totals_totals_gender

totals_gender <- rbind(totals_gender, totals_totals_gender)
totals_gender

addWorksheet(wb, "CodeGenderInvestment")
writeData(wb, sheet = "CodeGenderInvestment", totals_gender, colNames = TRUE, rowNames = FALSE)

## Totals per investment, code ----
totals_type <- df %>%
  group_by(Typeofinvestment, code, Type, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Type, values_from = Total) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Caregiver = if("Caregiver" %in% colnames(.)) Caregiver else 0,
         Child = if("Child" %in% colnames(.)) Child else 0,
         School = if("School" %in% colnames(.)) School else 0,
         Teacher = if("Teacher" %in% colnames(.)) Teacher else 0,
         Other = if("Other" %in% colnames(.)) Other else 0) %>%
  rename(Caregivers = Caregiver, 
         Children = Child,
         Schools = School,
         Teachers = Teacher)


totals_totals_type <- df %>%
  group_by(code, Type, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Type, values_from = Total) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Caregiver = if("Caregiver" %in% colnames(.)) Caregiver else 0,
         Child = if("Child" %in% colnames(.)) Child else 0,
         School = if("School" %in% colnames(.)) School else 0,
         Teacher = if("Teacher" %in% colnames(.)) Teacher else 0,
         Other = if("Other" %in% colnames(.)) Other else 0,
         Typeofinvestment = "TOTAL") %>%
  rename(Caregivers = Caregiver, 
         Children = Child,
         Schools = School,
         Teachers = Teacher) %>%
  select(Typeofinvestment, code, Gender, Schools, Teachers, Other, Children, Caregivers)

totals_type <- rbind(totals_type, totals_totals_type)
totals_type

addWorksheet(wb, "CodeTypeInvestment")
writeData(wb, sheet = "CodeTypeInvestment", totals_type, colNames = TRUE, rowNames = FALSE)

## Average and median ----
mean <- df %>%
  filter(Total != 0 & !is.na(Total) &
           Gender == "Total") %>%
  group_by(Typeofinvestment, code) %>%
  summarise(MeanWithout0 = mean(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = MeanWithout0) %>%
  rename(FERmean = FER,
         MYRPmean = MYRP)
mean

median <- df %>%
  filter(Total != 0 & !is.na(Total) &
           Gender == "Total") %>%
  group_by(Typeofinvestment, code) %>%
  summarise(MedianWithout0 = median(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = MedianWithout0) %>%
  rename(FERmedian = FER,
         MYRPmedian = MYRP)
median

meanwith0 <- df %>%
  filter(Gender == "Total") %>%
  group_by(Typeofinvestment, code) %>%
  summarise(MeanWith0 = mean(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = MeanWith0) %>%
  rename(FERmeanwith0 = FER,
         MYRPmeanwith0 = MYRP)
meanwith0

medianwith0 <- df %>%
  filter(Gender == "Total") %>%
  group_by(Typeofinvestment, code) %>%
  summarise(MedianWith0 = median(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = MedianWith0) %>%
  rename(FERmedianwith0 = FER,
         MYRPmedianwith0 = MYRP)
medianwith0

joined <- mean %>%
  left_join(median, by = "code") %>%
  left_join(meanwith0, by = "code") %>%
  left_join(medianwith0, by = "code")
joined

addWorksheet(wb, "MeanMedianAnalysisCode")
writeData(wb, sheet = "MeanMedianAnalysisCode", joined, colNames = TRUE, rowNames = FALSE)


## Overall mean and median ----
mean <- df %>%
  filter(Total != 0 & !is.na(Total) &
           Gender == "Total") %>%
  group_by(code) %>%
  summarise(MeanWithout0 = mean(Total, na.rm = TRUE))

median <- df %>%
  filter(Total != 0 & !is.na(Total) &
           Gender == "Total") %>%
  group_by(code) %>%
  summarise(MedianWithout0 = median(Total, na.rm = TRUE))

meanwith0 <- df %>%
  filter(Gender == "Total") %>%
  group_by(code) %>%
  summarise(MeanWith0 = mean(Total, na.rm = TRUE))

medianwith0 <- df %>%
  filter(Gender == "Total") %>%
  group_by(code) %>%
  summarise(MedianWith0 = median(Total, na.rm = TRUE))

overall_joined <- mean %>%
  left_join(median, by = "code") %>%
  left_join(meanwith0, by = "code") %>%
  left_join(medianwith0, by = "code")
overall_joined

addWorksheet(wb, "MeanMedianOverall")
writeData(wb, sheet = "MeanMedianOverall", overall_joined, colNames = TRUE, rowNames = FALSE)

## Annual datapoints df ---

datapoints <- df %>%
  filter(Gender == "Total") %>%
  select(Typeofinvestment, LeadGRN, code, Total, Exercise)

addWorksheet(wb, "TotalDatapoints")
writeData(wb, sheet = "TotalDatapoints", datapoints, colNames = TRUE, rowNames = FALSE)


## Number programs with data ----
# filter out lines with 0 in number
df <- root

df <- df %>%
  filter(exercice == annual)

number_programs <- df %>%
  filter(Total != 0 & !is.na(Total)) %>%
  group_by(Typeofinvestment, code) %>%
  summarise(Programs = n_distinct(LeadGRN)) %>%
  pivot_wider(names_from = Typeofinvestment, values_from = Programs)

addWorksheet(wb, "NumberProgramsWData")
writeData(wb, sheet = "NumberProgramsWData", number_programs, colNames = TRUE, rowNames = FALSE)

## Programs with data list ----

programs_list <- df %>%
  filter(Total != 0 & !is.na(Total)) %>%
  group_by(Typeofinvestment, code) %>%
  select(Typeofinvestment, code, LeadGRN, Country) %>%
  unique()

addWorksheet(wb, "ListProgramsWData")
writeData(wb, sheet = "ListProgramsWData", programs_list, colNames = TRUE, rowNames = FALSE)

# Output ----
# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Program Results - Cumulative Strategic Plan.xlsx"), overwrite = TRUE)

rm(list = ls())
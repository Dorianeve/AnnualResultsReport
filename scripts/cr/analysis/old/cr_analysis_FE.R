# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/analysis/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR ANALYSIS ANNUAL ----
df <- read.csv("data/cleaned/CR Combiner - For Analysis.csv", encoding = "UTF-8")

## Annual reached (Activein2024 | annual) ----
df_filtered <- df %>%
  filter(
    get(paste0("Activein", report_year)) == "Yes" &
      exercice == annual & 
      Status == "Reached" &
      Typeofeducation == "Formal education" &
      (Typeofreporting != "Individual" | is.na(Typeofreporting)) &
      (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
      Covid19related == "Non-Covid-19 related data"
  )


### Type of beneficiary ----
type_ben <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities" & 
           Typeofbeneficiary != "Total" &
           Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Typeofinvestment, Gender, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  group_by(Typeofinvestment) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Total = Female + Male) %>%
  mutate(PerBens = Total / TotalInvestment,
         PerFemale = Female / Total)

type_ben_tot <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities" & 
           Typeofbeneficiary != "Total" &
           Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Gender, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerBens = Total / TotalInvestment,
         PerFemale = Female / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
type_ben_tot


type_ben <- bind_rows(type_ben, type_ben_tot)

### Level of education ----
level_edu <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities" & 
           Typeofbeneficiary != "Total" &
           Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Typeofinvestment, Gender, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  group_by(Typeofinvestment) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Total = Female + Male) %>%
  mutate(PerEdu = Total / TotalInvestment,
         PerFemale = Female / Total)
level_edu

level_edu_tot <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities" & 
           Typeofbeneficiary != "Total" &
           Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Gender, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerEdu = Total / TotalInvestment,
         PerFemale = Female / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
level_edu_tot

level_edu <- bind_rows(level_edu, level_edu_tot)


### CWD ----
cwd <- df_filtered %>%
  filter(Gender != "Total") %>%
  group_by(Typeofinvestment, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(PerCWD = `Children with disabilities` / Total)
cwd

cwd_total <- df_filtered %>%
  filter(Gender != "Total") %>%
  group_by(Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(PerCWD = `Children with disabilities` / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
cwd_total

cwd <- bind_rows(cwd, cwd_total)
cwd

### CWD Gender ----
cwd_gender <- df_filtered %>%
  filter(Gender != "Total" &
           Typeofbeneficiary == "Children with disabilities") %>%
  group_by(Typeofinvestment, Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerFemale = Female / Total) %>%
  mutate(PerMale = Male / Total)
cwd_gender

cwd_gender_total <- df_filtered %>%
  filter(Gender != "Total" &
           Typeofbeneficiary == "Children with disabilities") %>%
  group_by(Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerFemale = Female / Total) %>%
  mutate(PerMale = Male / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
cwd_gender_total

cwd_gender <- bind_rows(cwd_gender, cwd_gender_total)
cwd_gender

## Output annual ----
# List of checks and their corresponding sheet names
sheets <- list(
  "Reached_Type_Bens" = type_ben,
  "Reached_Level_Edu" = level_edu,
  "Reached_CWD" = cwd,
  "Reached_CWD_Gender" = cwd_gender
)

# Loop through each check object and add to the workbook
for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Children Reached annual FE.xlsx"), overwrite = TRUE)

rm(df_filtered, cwd, cwd_gender, cwd_gender_total, cwd_total,
   level_edu_tot, type_ben_tot,
   type_ben, level_edu, sheets, wb)

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/analysis/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR ANALYSIS CUMULATIVE STRATEGIC----
df <- read.csv("data/cleaned/CR Combiner - For Analysis.csv", encoding = "UTF-8")

## Cumulative reached (cumulative | ActiveStrategicPlan) ----
df_filtered <- df %>%
  filter(
    exercice == cumulative & 
      Status == "Reached" &
      Typeofeducation == "Formal education" &
      (Typeofreporting != "Individual" | is.na(Typeofreporting)) &
      ActiveStrategicPlan == "Yes" &
      (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
      Covid19related == "Non-Covid-19 related data"
  )



### Type of beneficiary ----
type_ben <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities" & 
           Typeofbeneficiary != "Total" &
           Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Typeofinvestment, Gender, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  group_by(Typeofinvestment) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Total = Female + Male) %>%
  mutate(PerBens = Total / TotalInvestment,
         PerFemale = Female / Total)

type_ben_tot <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities" & 
           Typeofbeneficiary != "Total" &
           Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Gender, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerBens = Total / TotalInvestment,
         PerFemale = Female / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
type_ben_tot


type_ben <- bind_rows(type_ben, type_ben_tot)

### Level of education ----
level_edu <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities" & 
           Typeofbeneficiary != "Total" &
           Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Typeofinvestment, Gender, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  group_by(Typeofinvestment) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Total = Female + Male) %>%
  mutate(PerEdu = Total / TotalInvestment,
         PerFemale = Female / Total)
level_edu

level_edu_tot <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities" & 
           Typeofbeneficiary != "Total" &
           Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Gender, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerEdu = Total / TotalInvestment,
         PerFemale = Female / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
level_edu_tot

level_edu <- bind_rows(level_edu, level_edu_tot)


### CWD ----
cwd <- df_filtered %>%
  filter(Gender != "Total") %>%
  group_by(Typeofinvestment, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(PerCWD = `Children with disabilities` / Total)
cwd

cwd_total <- df_filtered %>%
  filter(Gender != "Total") %>%
  group_by(Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(PerCWD = `Children with disabilities` / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
cwd_total

cwd <- bind_rows(cwd, cwd_total)
cwd

### CWD Gender ----
cwd_gender <- df_filtered %>%
  filter(Gender != "Total" &
           Typeofbeneficiary == "Children with disabilities") %>%
  group_by(Typeofinvestment, Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerFemale = Female / Total) %>%
  mutate(PerMale = Male / Total)
cwd_gender

cwd_gender_total <- df_filtered %>%
  filter(Gender != "Total" &
           Typeofbeneficiary == "Children with disabilities") %>%
  group_by(Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerFemale = Female / Total) %>%
  mutate(PerMale = Male / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
cwd_gender_total

cwd_gender <- bind_rows(cwd_gender, cwd_gender_total)
cwd_gender

## Output cumulative strategic ----
# List of checks and their corresponding sheet names
sheets <- list(
  "Reached_Type_Bens" = type_ben,
  "Reached_Level_Edu" = level_edu,
  "Reached_CWD" = cwd,
  "Reached_CWD_Gender" = cwd_gender
)

# Loop through each check object and add to the workbook
for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Children Reached cumulative strategic FE.xlsx"), overwrite = TRUE)

rm(list = ls())

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/analysis/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR ANALYSIS CUMULATIVE INCEPTION ----
df <- read.csv("data/cleaned/CR Combiner - For Analysis.csv", encoding = "UTF-8")

## Cumulative reached (cumulative) ----
df_filtered <- df %>%
  filter(
    exercice == cumulative & 
      Status == "Reached" &
      Typeofeducation == "Formal education" &
      (Typeofreporting != "Individual" | is.na(Typeofreporting)) & 
      (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
      Covid19related == "Non-Covid-19 related data"
  )

### Type of beneficiary ----
type_ben <- df_filtered %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Typeofinvestment, Gender, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  group_by(Typeofinvestment) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Total = Female + Male) %>%
  mutate(PerBens = Total / TotalInvestment,
         PerFemale = Female / Total)

type_ben_tot <- df_filtered %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Gender, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerBens = Total / TotalInvestment,
         PerFemale = Female / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
type_ben_tot


type_ben <- bind_rows(type_ben, type_ben_tot)

### Level of education ----
level_edu <- df_filtered %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Typeofinvestment, Gender, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  group_by(Typeofinvestment) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Total = Female + Male) %>%
  mutate(PerEdu = Total / TotalInvestment,
         PerFemale = Female / Total)
level_edu

level_edu_tot <- df_filtered %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Gender, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TotalInvestment = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerEdu = Total / TotalInvestment,
         PerFemale = Female / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
level_edu_tot

level_edu <- bind_rows(level_edu, level_edu_tot)


### CWD ----
cwd <- df_filtered %>%
  filter(Gender != "Total") %>%
  group_by(Typeofinvestment, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(PerCWD = `Children with disabilities` / Total)
cwd

cwd_total <- df_filtered %>%
  filter(Gender != "Total") %>%
  group_by(Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(PerCWD = `Children with disabilities` / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
cwd_total

cwd <- bind_rows(cwd, cwd_total)
cwd

### CWD Gender ----
cwd_gender <- df_filtered %>%
  filter(Gender != "Total" &
           Typeofbeneficiary == "Children with disabilities") %>%
  group_by(Typeofinvestment, Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerFemale = Female / Total) %>%
  mutate(PerMale = Male / Total)
cwd_gender

cwd_gender_total <- df_filtered %>%
  filter(Gender != "Total" &
           Typeofbeneficiary == "Children with disabilities") %>%
  group_by(Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(Total = Female + Male) %>%
  mutate(PerFemale = Female / Total) %>%
  mutate(PerMale = Male / Total) %>%
  mutate(Typeofinvestment = "TOTAL")
cwd_gender_total

cwd_gender <- bind_rows(cwd_gender, cwd_gender_total)
cwd_gender

## Output cumulative inception ----
# List of checks and their corresponding sheet names
sheets <- list(
  "Reached_Type_Bens" = type_ben,
  "Reached_Level_Edu" = level_edu,
  "Reached_CWD" = cwd,
  "Reached_CWD_Gender" = cwd_gender
)

# Loop through each check object and add to the workbook
for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Children Reached cumulative inception FE.xlsx"), overwrite = TRUE)

rm(list = ls())


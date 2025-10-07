# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/analysis/FE/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR FE ANALYSIS ANNUAL ----
df <- read.csv("data/cleaned/CR Combiner - For Analysis.csv", encoding = "UTF-8")

## Annual reached / targeted ----
df_filtered <- df %>%
  filter(
      exercice == annual &
      (Typeofreporting != "Individual" | is.na(Typeofreporting)) &
      Typeofeducation == "Formal education" &
      (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
      Covid19related == "Non-Covid-19 related data"
  )

### Type of investment ----
reach_targ_inv <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities") %>%
  group_by(Typeofinvestment, Gender, Typeofbeneficiary, Levelofeducation, Status) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Status, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TowardsTarget = Reached / Targeted)
reach_targ_inv

### Totals ----
reach_targ_tot <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities") %>%
  group_by(Gender, Typeofbeneficiary, Levelofeducation, Status) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  mutate(id = "TOTAL") %>%
  select(id, Gender, Typeofbeneficiary, Levelofeducation, Status, Total) %>%
  pivot_wider(names_from = Status, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TowardsTarget = Reached / Targeted)
reach_targ_tot 

## Annual reached (w gender percentage) ----
df_filtered <- df %>%
  filter(
      exercice == annual &
      (Typeofreporting != "Individual" | is.na(Typeofreporting)) & Status == "Reached" &
      (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
      Covid19related == "Non-Covid-19 related data"
  )

### Type of investment ----
reach_inv <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities") %>%
  group_by(Typeofinvestment, Gender, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  arrange(Typeofinvestment, Levelofeducation) %>%
  mutate(PerFemale = Female / Total,
         PerMale = Male / Total) %>%
  select(Typeofinvestment, Levelofeducation, Typeofbeneficiary, Female, Male, Total, PerFemale, PerMale)
reach_inv

### Total ----
reach_tot <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities") %>%
  group_by(Gender, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  mutate(id = "TOTAL") %>%
  select(id, Gender, Typeofbeneficiary, Levelofeducation, Total) %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  arrange(id, Levelofeducation) %>%
  mutate(PerFemale = Female / Total,
         PerMale = Male /Total) %>%
  select(id, Levelofeducation, Typeofbeneficiary, Female, Male, Total, PerFemale, PerMale)
reach_tot

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
  mutate(PerBens = Total / TotalInvestment)

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
  mutate(PerEdu = Total / TotalInvestment)
level_edu

### CWD ----
cwd <- df_filtered %>%
  filter(Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Typeofinvestment, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(PerCWD = `Children with disabilities` / Total)

## Output annual ----
# List of checks and their corresponding sheet names
sheets <- list(
  "Reached_Targeted_Investments" = reach_targ_inv,
  "Reached_Targeted_Totals" = reach_targ_tot,
  "Reached_Investments" = reach_inv,
  "Reached_Totals" = reach_tot,
  "Reached_Type_Bens" = type_ben,
  "Reached_Level_Edu" = level_edu,
  "Reached_CWD" = cwd
)

# Loop through each check object and add to the workbook
for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Children Reached annual FE inception.xlsx"), overwrite = TRUE)

rm(df_filtered, reach_inv, reach_targ_inv, reach_targ_tot, reach_tot,
   type_ben, level_edu, sheets, wb)

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/analysis/FE/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR FE ANALYSIS CUMULATIVE ----
df <- read.csv("data/cleaned/CR Combiner - For Analysis.csv", encoding = "UTF-8")

## Cumulative reached / targeted ----
df_filtered <- df %>%
  filter(
      exercice == cumulative &
      (Typeofreporting != "Individual" | is.na(Typeofreporting)) &
      Typeofeducation == "Formal education" &
      (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
      Covid19related == "Non-Covid-19 related data"
  )

### Type of investment ----
reach_targ_inv <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities") %>%
  group_by(Typeofinvestment, Gender, Typeofbeneficiary, Levelofeducation, Status) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Status, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TowardsTarget = Reached / Targeted)
reach_targ_inv

### Totals ----
reach_targ_tot <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities") %>%
  group_by(Gender, Typeofbeneficiary, Levelofeducation, Status) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  mutate(id = "TOTAL") %>%
  select(id, Gender, Typeofbeneficiary, Levelofeducation, Status, Total) %>%
  pivot_wider(names_from = Status, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(TowardsTarget = Reached / Targeted)
reach_targ_tot 

## Cumulative reached (w gender percentage) ----
df_filtered <- df %>%
  filter(
      exercice == cumulative &
      (Typeofreporting != "Individual" | is.na(Typeofreporting)) & Status == "Reached" &
      (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
      Covid19related == "Non-Covid-19 related data"
  )

### Type of investment ----
reach_inv <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities") %>%
  group_by(Typeofinvestment, Gender, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  arrange(Typeofinvestment, Levelofeducation) %>%
  mutate(PerFemale = Female / Total,
         PerMale = Male / Total) %>%
  select(Typeofinvestment, Levelofeducation, Typeofbeneficiary, Female, Male, Total, PerFemale, PerMale)
reach_inv

### Total ----
reach_tot <- df_filtered %>%
  filter(Typeofbeneficiary != "Children with disabilities") %>%
  group_by(Gender, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  mutate(id = "TOTAL") %>%
  select(id, Gender, Typeofbeneficiary, Levelofeducation, Total) %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  arrange(id, Levelofeducation) %>%
  mutate(PerFemale = Female / Total,
         PerMale = Male /Total) %>%
  select(id, Levelofeducation, Typeofbeneficiary, Female, Male, Total, PerFemale, PerMale)
reach_tot

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
  mutate(PerBens = Total / TotalInvestment)

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
  mutate(PerEdu = Total / TotalInvestment)
level_edu

### CWD ----
cwd <- df_filtered %>%
  filter(Levelofeducation != "Total" &
           Gender != "Total") %>%
  group_by(Typeofinvestment, Typeofbeneficiary, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(PerCWD = `Children with disabilities` / Total)

## Output cumulative ----
# List of checks and their corresponding sheet names
sheets <- list(
  "Reached_Targeted_Investments" = reach_targ_inv,
  "Reached_Targeted_Totals" = reach_targ_tot,
  "Reached_Investments" = reach_inv,
  "Reached_Totals" = reach_tot,
  "Reached_Type_Bens" = type_ben,
  "Reached_Level_Edu" = level_edu,
  "Reached_CWD" = cwd
)

# Loop through each check object and add to the workbook
for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Children Reached cumulative FE inception.xlsx"), overwrite = TRUE)

rm(list = ls())

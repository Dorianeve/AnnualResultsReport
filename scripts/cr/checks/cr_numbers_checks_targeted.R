# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR FIGURES CHECKS ----
df <- read.csv("data/cleaned/CR Combiner - For Checks.csv", encoding = "UTF-8")

## Annual ----
df_filtered_root <- df %>%
  filter(exercice == annual & Status == "Targeted") 

### Gender ----
df_filtered <- df_filtered_root %>%
  filter(get(paste0("Activein", report_year)) == "Yes" &
           Typeofbeneficiary == "Total" &
           Levelofeducation == "Total")

gender_check <- df_filtered %>%
  group_by(ProgrammeID, LeadGRN, Typeofreporting, Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(Typeofreporting, Gender), values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(JointComputed = Joint_Female + Joint_Male,
         IndividualComputed = Individual_Female + Individual_Male) %>%
  mutate(JointCheck = JointComputed == Joint_Total,
         IndividualCheck = IndividualComputed == Individual_Total) %>%
  mutate(JointIndividualCheck = Joint_Total <= Individual_Total,
         JointIndividualCheck = case_when(
           Individual_Total == 0 ~ TRUE,
           TRUE  ~ JointIndividualCheck
         )) %>%
  select(ProgrammeID, LeadGRN, Joint_Female, Joint_Male, Joint_Total, JointComputed,
  Individual_Female, Individual_Male, Individual_Total, IndividualComputed, 
  JointCheck, IndividualCheck, JointIndividualCheck)

summary_gender <- gender_check %>%
  filter(!JointCheck | !IndividualCheck | !JointIndividualCheck) %>%
  mutate(
    message = case_when(
      !JointCheck ~ "Joint computed gender totals do not match inputed totals",
      !IndividualCheck ~ "Individual computed gender totals do not match inputed totals",
      !JointIndividualCheck ~ "Inputed Joint gender Totals are greater than Individual inputed totals"
    )
  ) %>%
  select(ProgrammeID, LeadGRN, message) %>%
  mutate(Check = "Gender") %>%
  distinct()


### Type of beneficiary ----
df_filtered <- df_filtered_root %>%
  filter(get(paste0("Activein", report_year)) == "Yes" &
           Gender == "Total" &
           Levelofeducation == "Total" &
           Typeofbeneficiary != "Children with disabilities")

bens_check <- df_filtered %>%
  group_by(ProgrammeID, LeadGRN, Typeofreporting, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(Typeofreporting, Typeofbeneficiary), values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(JointComputed = Joint_IDP  + Joint_OAPs + Joint_Refugee + Joint_Unknown, 
         IndividualComputed = Individual_IDP  + Individual_OAPs + Individual_Refugee + Individual_Unknown) %>%
  mutate(JointCheck = JointComputed == Joint_Total,
         IndividualCheck = IndividualComputed == Individual_Total) %>%
  mutate(JointIndividualCheck = Joint_Total <= Individual_Total,
         JointIndividualCheck = case_when(
           Individual_Total == 0 ~ TRUE,
           TRUE  ~ JointIndividualCheck
         )) %>%
  select(ProgrammeID, LeadGRN, Joint_IDP, Joint_OAPs, Joint_Refugee, Joint_Unknown, Joint_Total, JointComputed,
         Individual_IDP, Individual_OAPs, Individual_Refugee, Individual_Unknown, Individual_Total, IndividualComputed,
         JointCheck, IndividualCheck, JointIndividualCheck)

summary_beneficiary <- bens_check %>%
  filter(!JointCheck | !IndividualCheck | !JointIndividualCheck) %>%
  mutate(
    message = case_when(
      !JointCheck ~ "Joint computed Typeofbeneficiary totals do not match inputed totals",
      !IndividualCheck ~ "Individual computed Typeofbeneficiary totals do not match inputed totals",
      !JointIndividualCheck ~ "Inputed Joint Typeofbeneficiary Totals are greater than Individual inputed totals"
    )
  ) %>%
  select(ProgrammeID, LeadGRN, message) %>%
  mutate(Check = "Beneficiary") %>%
  distinct()

### Level of education ----
df_filtered <- df_filtered_root %>%
  filter(get(paste0("Activein", report_year)) == "Yes" &
           Gender == "Total" &
           Typeofbeneficiary == "Total")

edu_check <- df_filtered %>%
  group_by(ProgrammeID, LeadGRN, Typeofreporting, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(Typeofreporting, Levelofeducation), values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(JointComputed = Joint_Primary + Joint_Secondary + 
           `Joint_Unknown level of education` + `Joint_Pre-primary`, 
         IndividualComputed = Individual_Primary + Individual_Secondary + 
           `Individual_Unknown level of education` + `Individual_Pre-primary`) %>%
  mutate(JointCheck = JointComputed == Joint_Total,
         IndividualCheck = IndividualComputed == Individual_Total) %>%
  mutate(JointIndividualCheck = Joint_Total <= Individual_Total,
         JointIndividualCheck = case_when(
           Individual_Total == 0 ~ TRUE,
           TRUE  ~ JointIndividualCheck
         )) %>%
  select(ProgrammeID, LeadGRN, `Joint_Pre-primary`, Joint_Primary, Joint_Secondary, `Joint_Unknown level of education`, Joint_Total, JointComputed,
         `Individual_Pre-primary`, Individual_Primary, Individual_Secondary, `Individual_Unknown level of education`, Individual_Total, IndividualComputed,
         JointCheck, IndividualCheck, JointIndividualCheck)

summary_education <- edu_check %>%
  filter(!JointCheck | !IndividualCheck | !JointIndividualCheck) %>%
  mutate(
    message = case_when(
      !JointCheck ~ "Joint computed Levelofeducation totals do not match inputed totals",
      !IndividualCheck ~ "Individual computed Levelofeducation totals do not match inputed totals",
      !JointIndividualCheck ~ "Inputed Joint Levelofeducation Totals are greater than Individual inputed totals"
    )
  ) %>%
  select(ProgrammeID, LeadGRN, message) %>%
  mutate(Check = "Education") %>%
  distinct()

summary_annual <- rbind(summary_gender, summary_beneficiary, summary_education)

### Output ----
# Add the summary sheet to the workbook
addWorksheet(wb, "SummaryAnnual")
writeData(wb, "SummaryAnnual", summary_annual)

# List of checks and their corresponding sheet names
checks_list <- list(
  "AnnualGenderCheck" = gender_check,
  "AnnualEdu_check" = edu_check,
  "AnnualBens_check" = bens_check
)

# Loop through each check object and add to the workbook
for (sheet_name in names(checks_list)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, checks_list[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Number checks.xlsx"), overwrite = TRUE)

rm(bens_check, checks_list, edu_check, gender_check,
   summary_annual, summary_beneficiary, summary_education, summary_gender)

## Cumulative ----
df_filtered_root <- df %>%
  filter(exercice == cumulative & Status == "Targeted") 

### Gender ----
df_filtered <- df_filtered_root %>%
  filter(get(paste0("Activein", report_year)) == "Yes" &
           Typeofbeneficiary == "Total" &
           Levelofeducation == "Total")

gender_check <- df_filtered %>%
  group_by(ProgrammeID, LeadGRN, Typeofreporting, Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(Typeofreporting, Gender), values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(JointComputed = Joint_Female + Joint_Male,
         IndividualComputed = Individual_Female + Individual_Male) %>%
  mutate(JointCheck = JointComputed == Joint_Total,
         IndividualCheck = IndividualComputed == Individual_Total) %>%
  mutate(JointIndividualCheck = Joint_Total <= Individual_Total,
         JointIndividualCheck = case_when(
           Individual_Total == 0 ~ TRUE,
           TRUE  ~ JointIndividualCheck
         )) %>%
  select(ProgrammeID, LeadGRN, Joint_Female, Joint_Male, Joint_Total, JointComputed,
         Individual_Female, Individual_Male, Individual_Total, IndividualComputed, 
         JointCheck, IndividualCheck, JointIndividualCheck)

summary_gender <- gender_check %>%
  filter(!JointCheck | !IndividualCheck | !JointIndividualCheck) %>%
  mutate(
    message = case_when(
      !JointCheck ~ "Joint computed gender totals do not match inputed totals",
      !IndividualCheck ~ "Individual computed gender totals do not match inputed totals",
      !JointIndividualCheck ~ "Inputed Joint gender Totals are greater than Individual inputed totals"
    )
  ) %>%
  select(ProgrammeID, LeadGRN, message) %>%
  mutate(Check = "Gender") %>%
  distinct()


### Type of beneficiary ----
df_filtered <- df_filtered_root %>%
  filter(get(paste0("Activein", report_year)) == "Yes" &
           Gender == "Total" &
           Levelofeducation == "Total" &
           Typeofbeneficiary != "Children with disabilities")

bens_check <- df_filtered %>%
  group_by(ProgrammeID, LeadGRN, Typeofreporting, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(Typeofreporting, Typeofbeneficiary), values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(JointComputed = Joint_IDP  + Joint_OAPs + Joint_Refugee + Joint_Unknown, 
         IndividualComputed = Individual_IDP  + Individual_OAPs + Individual_Refugee + Individual_Unknown) %>%
  mutate(JointCheck = JointComputed == Joint_Total,
         IndividualCheck = IndividualComputed == Individual_Total) %>%
  mutate(JointIndividualCheck = Joint_Total <= Individual_Total,
         JointIndividualCheck = case_when(
           Individual_Total == 0 ~ TRUE,
           TRUE  ~ JointIndividualCheck
         )) %>%
  select(ProgrammeID, LeadGRN, Joint_IDP, Joint_OAPs, Joint_Refugee, Joint_Unknown, Joint_Total, JointComputed,
         Individual_IDP, Individual_OAPs, Individual_Refugee, Individual_Unknown, Individual_Total, IndividualComputed,
         JointCheck, IndividualCheck, JointIndividualCheck)

summary_beneficiary <- bens_check %>%
  filter(!JointCheck | !IndividualCheck | !JointIndividualCheck) %>%
  mutate(
    message = case_when(
      !JointCheck ~ "Joint computed Typeofbeneficiary totals do not match inputed totals",
      !IndividualCheck ~ "Individual computed Typeofbeneficiary totals do not match inputed totals",
      !JointIndividualCheck ~ "Inputed Joint Typeofbeneficiary Totals are greater than Individual inputed totals"
    )
  ) %>%
  select(ProgrammeID, LeadGRN, message) %>%
  mutate(Check = "Beneficiary") %>%
  distinct()

### Level of education ----
df_filtered <- df_filtered_root %>%
  filter(get(paste0("Activein", report_year)) == "Yes" &
           Gender == "Total" &
           Typeofbeneficiary == "Total")

edu_check <- df_filtered %>%
  group_by(ProgrammeID, LeadGRN, Typeofreporting, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(Typeofreporting, Levelofeducation), values_from = Total) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(JointComputed = Joint_Primary + Joint_Secondary  + 
           `Joint_Unknown level of education` + `Joint_Pre-primary`, 
         IndividualComputed = Individual_Primary + Individual_Secondary + 
           `Individual_Unknown level of education` + `Individual_Pre-primary`) %>%
  mutate(JointCheck = JointComputed == Joint_Total,
         IndividualCheck = IndividualComputed == Individual_Total) %>%
  mutate(JointIndividualCheck = Joint_Total <= Individual_Total,
         JointIndividualCheck = case_when(
           Individual_Total == 0 ~ TRUE,
           TRUE  ~ JointIndividualCheck
         )) %>%
  select(ProgrammeID, LeadGRN, `Joint_Pre-primary`, Joint_Primary, Joint_Secondary, `Joint_Unknown level of education`, Joint_Total, JointComputed,
         `Individual_Pre-primary`, Individual_Primary, Individual_Secondary, `Individual_Unknown level of education`, Individual_Total, IndividualComputed,
         JointCheck, IndividualCheck, JointIndividualCheck)

summary_education <- edu_check %>%
  filter(!JointCheck | !IndividualCheck | !JointIndividualCheck) %>%
  mutate(
    message = case_when(
      !JointCheck ~ "Joint computed Levelofeducation totals do not match inputed totals",
      !IndividualCheck ~ "Individual computed Levelofeducation totals do not match inputed totals",
      !JointIndividualCheck ~ "Inputed Joint Levelofeducation Totals are greater than Individual inputed totals"
    )
  ) %>%
  select(ProgrammeID, LeadGRN, message) %>%
  mutate(Check = "Education") %>%
  distinct()

#
summary_cumulative <- rbind(summary_gender, summary_beneficiary, summary_education)

### Output ----
# Add the summary sheet to the workbook
addWorksheet(wb, "SummaryCumulative")
writeData(wb, "SummaryCumulative", summary_cumulative)

# List of checks and their corresponding sheet names
checks_list <- list(
  "CumulativeGenderCheck" = gender_check,
  "CumulativeEdu_check" = edu_check,
  "CumulativeBens_check" = bens_check
)

# Loop through each check object and add to the workbook
for (sheet_name in names(checks_list)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, checks_list[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Number checks - Targeted.xlsx"), overwrite = TRUE)
rm(list = ls())

# CR FLOW - Data checks ----

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# GRANTSDB CHECK ----
grants <- read.csv(paste0("data/input/", grants_db), encoding = "UTF-8") 

grants %<>%
  rename(Active = paste0("Activein", report_year),
         StartDate = Startdate,
         CurrentEndDate = Currentenddate)

grants %<>%
  select(ProgrammeID, GMGRN, Active, StartDate, CurrentEndDate)

## Active programs starting date ----
# Create list StartDate 15-12 / 31-12 for check
grants %<>%
  mutate(StartDec = case_when(
    Active == "Yes" &
           (StartDate >= as.Date(paste0(report_year, "-12-15")) &
      StartDate <= as.Date(paste0(report_year, "-12-31"))) ~ TRUE,
    TRUE ~ FALSE ))

## Active programs ending date ----
# Create list EndDate 01-01 / 31-01 for check
grants %<>%
  mutate(EndJan = case_when(
    Active == "Yes" &
      (CurrentEndDate >= as.Date(paste0(report_year, "-01-01")) &
         CurrentEndDate <= as.Date(paste0(report_year, "-01-31"))) ~ TRUE,
    TRUE ~ FALSE ))

list_grants_StartDec <- grants %>%
  filter(StartDec == TRUE) %>%
  select(ProgrammeID)

list_grants_EndJan <- grants %>%
  filter(EndJan == TRUE) %>%
  select(ProgrammeID)

# CR CHECKS ----
df <- read.csv("data/cleaned/CR Combiner - For Checks.csv", encoding = "UTF-8")

## MnE import for filtering -----
mne <- read_xlsx(mne_approval,
                 sheet = "Results_Reports",
                 skip = 6)

mne %<>%
  filter(`GM Status` == "Completed")

## CR filtering ----
df %<>%
  filter(exercice == annual |
           exercice == cumulative)

df %<>%
  filter(ProgrammeID %in% mne$`Programme ID`)

## Missing information ----
### Date ----
df %<>%
  mutate(MissingDate = case_when(
    date == "Missing" ~ TRUE,
    TRUE ~ FALSE
  ))

### Active in Year ----
df %<>%
  mutate(MissingActiveinyear = case_when(
    is.na(!!sym(paste0("Activein", report_year))) ~ TRUE,
    TRUE ~ FALSE
  ))

### ProgrammeID ----
df %<>%
  mutate(MissingProgrammeID = case_when(
    is.na(ProgrammeID) ~ TRUE,
    TRUE ~ FALSE
  ))


### LeadGRN ----
df %<>%
  mutate(MissingLeadGRN = case_when(
    is.na(LeadGRN) ~ TRUE,
    TRUE ~ FALSE
  ))

### GMGRN ----
df %<>%
  mutate(MissingGMGRN = case_when(
    is.na(GMGRN) ~ TRUE,
    TRUE ~ FALSE
  ))

### Number integer and NA input ----
df %<>%
  mutate(
    Number = as.character(Number),  # Ensure column is character for consistent logic
    Number = if_else(is.na(Number), "0", Number),  # Replace NA with "0"
    NumberCoerce = is.na(suppressWarnings(as.integer(Number))) & !is.na(Number),
    Number = as.integer(Number)
  )

### JointCR ----
df %<>%
  mutate(MissingJointCR = case_when(
    Typeofreporting == "Joint" & is.na(Number) ~ TRUE,
    TRUE ~ FALSE
  ))

### IndividualCR ----
df %<>%
  mutate(MissingIndividualCR = case_when(
    Typeofreporting == "Individual" & is.na(Number) ~ TRUE,
    TRUE ~ FALSE
  ))

### Type of reporting ----
df %<>%
  mutate(MissingTypeofReporting = case_when(
    yearstart >= 2012 & 
      is.na(Typeofreporting) &  
      !!sym(paste0("Activein", report_year)) == "Yes"~ TRUE,
    TRUE ~ FALSE
  ))

## Duplicates ----
### All row ----
df %<>%
  group_by(across(everything())) %>%  # Group by all columns to find duplicate rows
  mutate(AllRowDuplicate = n() > 1) %>% # Add column indicating duplicates
  ungroup()

### Lowest unit ----
df %<>%
  group_by(ProgrammeID, LeadGRN, GMGRN, Typeofinvestment, Exercise, Covid19related, Sourceofdata, Typeofreporting, 
           Typeofeducation, Status, Typeofbeneficiary, Levelofeducation, Gender) %>%
  mutate(LowestUnitDuplicated = n() > 1) %>%  # Add column indicating duplicates
  ungroup() %>%
  mutate(LowestUnitDuplicated = case_when(
    is.na(GMGRN) ~ FALSE,
    TRUE ~ LowestUnitDuplicated
  ))

## Data source check ----
df %<>%
  mutate(WrongSource = case_when(
    get(paste0("Activein", report_year)) == "Yes" & 
      Sourceofdata != source_cr & Covid19related == "Non-Covid-19 related data" ~ TRUE,
    get(paste0("Activein", report_year)) == "No" & 
      Sourceofdata == source_cr & Covid19related == "Non-Covid-19 related data" ~ TRUE,
    TRUE ~ FALSE
  ))

# OUTPUT ----
# Mapping of column names to descriptive labels
label_map <- c(
  "MissingDate" = "Date missing",
  "MissingActiveinyear" = "Active in Year missing",
  "MissingProgrammeID" = "ProgrammeID missing",
  "MissingLeadGRN" = "LeadGRN missing",
  "MissingGMGRN" = "GM GRN missing",
  "NumberCoerce" = "Number is not integer",
  "MissingJointCR" = "Missing Joint Reporting",
  "MissingIndividualCR" = "Missing Individual Reporting",
  "MissingTypeofReporting" = "Missing Type of Reporting",
  "AllRowDuplicate" = "Entire row duplicated",
  "LowestUnitDuplicated" = "Lowest reporting unit duplicated",
  "WrongSource" = "Wround Sourceofdata"
)

# Subset the data frame
columns_to_check <- names(label_map)
df_subset <- df %>% select(ProgrammeID, LeadGRN, GMGRN, all_of(columns_to_check))
cube <- df_subset %>% unique()

# FILTER Log decisions ----
log <- read.xlsx("data/input/log_decisions/log_decisions.xlsx", sheet = "cr")

cube %<>%
  pivot_longer(cols = all_of(columns_to_check), names_to = "Issue", values_to = "Flag") %>%
  mutate(Concat = paste0(ProgrammeID, LeadGRN, GMGRN, Issue))

log %<>%
  mutate(Concat = paste0(ProgrammeID, LeadGRN, GMGRN, Issue))

cube %<>%
  mutate(Flag = ifelse(Concat %in% log$Concat, FALSE, Flag)) %>%
  select(-Concat)

cube %<>%
  arrange(desc(Flag)) %>%  # put TRUE first
  distinct(ProgrammeID, LeadGRN, GMGRN, Issue, .keep_all = TRUE)

cube %<>%
  pivot_wider(names_from = Issue, values_from = Flag)


# Create a summary sheet with TRUE flags
summary <- cube %>%
  pivot_longer(cols = all_of(columns_to_check), names_to = "issue", values_to = "flag") %>%
  filter(flag == TRUE) %>%
  mutate(issue = label_map[issue]) %>%
  select(ProgrammeID, LeadGRN, GMGRN, issue) %>%
  unique()

# Add the summary sheet to the workbook
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary)
# Add cube check sheet to the workbook
addWorksheet(wb, "Cube")
writeData(wb, "Cube", cube)


# Create individual sheets for each check, containing only TRUE rows
for (col in columns_to_check) {
  sheet_data <- cube %>%
    filter(!!sym(col) == TRUE) %>%
    select(ProgrammeID, LeadGRN, GMGRN, !!sym(col)) %>%
    unique()

  if (nrow(sheet_data) > 0) {  # Only create the sheet if there are TRUE rows
    addWorksheet(wb, col)
    writeData(wb, col, sheet_data)
  }
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Data checks.xlsx"), overwrite = TRUE)

# Clean up
rm(columns_to_check, df_subset, summary)
rm(list = ls())


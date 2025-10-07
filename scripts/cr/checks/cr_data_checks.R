# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# GRANTSDB CHECK ----
# grants <- read.xlsx(paste0("data/input/", combiner), sheet = sheet_grants, 
#                     detectDates = TRUE, sep.names = " ")
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

## CR filtering ----
df %<>%
  filter(exercice == annual |
           exercice == cumulative)

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

## MUTED issing grants CR / GrantsDB Annual ----
# grantsdb_programmeid <- grants %>% filter(Active == "Yes") %>% select(ProgrammeID) %>% unique()
# grantsdb_gmgrn <- grants %>% filter(Active == "Yes") %>% select(GMGRN) %>% unique()
# cr_programmeid_annual <- df %>% filter(exercice == annual) %>% select(ProgrammeID) %>% unique()
# cr_gmgrn_annual <- df %>% filter(exercice == annual) %>% select(GMGRN) %>% unique()
# 
# # Find missing LeadGRN and GMGRN between grants and df
# missing_programmeid_in_cr <- setdiff(grantsdb_programmeid$ProgrammeID, cr_programmeid_annual$ProgrammeID)
# missing_programmeid_in_grantsdb <- setdiff(cr_programmeid_annual$ProgrammeID, grantsdb_programmeid$ProgrammeID)
# missing_gmgrn_in_cr <- setdiff(grantsdb_gmgrn$GMGRN, cr_gmgrn_annual$GMGRN)
# missing_gmgrn_in_grantsdb <- setdiff(cr_gmgrn_annual$GMGRN, grantsdb_gmgrn$GMGRN)
# 
# # Create data frames for each type of missing list if there are missing values
# missing_programmeid_in_cr_df <- create_missing_df(missing_programmeid_in_cr, "Missing Annual ProgrammeID in CR")
# missing_programmeid_in_grantsdb_df <- create_missing_df(missing_programmeid_in_grantsdb, "Missing Annual ProgrammeID in GrantsDB")
# missing_gmgrn_in_cr_df <- create_missing_df(missing_gmgrn_in_cr, "Missing Annual GMGRN in CR")
# missing_gmgrn_in_grantsdb_df <- create_missing_df(missing_gmgrn_in_grantsdb, "Missing Annual GMGRN in GrantsDB")
# 
# # Combine all non-NULL data frames into one
# combined_missing_grant_cr_annual <- bind_rows(
#   missing_programmeid_in_cr_df,
#   missing_programmeid_in_grantsdb_df,
#   missing_gmgrn_in_cr_df,
#   missing_gmgrn_in_grantsdb_df
# )
# 
# rm(missing_gmgrn_in_cr, missing_gmgrn_in_grantsdb, missing_programmeid_in_cr, missing_programmeid_in_grantsdb)
# rm(grantsdb_gmgrn, grantsdb_programmeid)
# 
# ## MUTED Missing grants CR / GrantsDB Cumulative ----
# grantsdb_programmeid <- grants %>% filter(Active == "Yes") %>% select(ProgrammeID) %>% unique()
# grantsdb_gmgrn <- grants %>% filter(Active == "Yes") %>% select(GMGRN) %>% unique()
# cr_programmeid_cumulative <- df %>% filter(exercice == cumulative) %>% select(ProgrammeID) %>% unique()
# cr_gmgrn_cumulative <- df %>% filter(exercice == cumulative) %>% select(GMGRN) %>% unique()
# 
# # Find missing LeadGRN and GMGRN between grants and df
# missing_programmeid_in_cr <- setdiff(grantsdb_programmeid$ProgrammeID, cr_programmeid_cumulative$ProgrammeID)
# missing_programmeid_in_grantsdb <- setdiff(cr_programmeid_cumulative$ProgrammeID, grantsdb_programmeid$ProgrammeID)
# missing_gmgrn_in_cr <- setdiff(grantsdb_gmgrn$GMGRN, cr_gmgrn_cumulative$GMGRN)
# missing_gmgrn_in_grantsdb <- setdiff(cr_gmgrn_cumulative$GMGRN, grantsdb_gmgrn$GMGRN)
# 
# # Create data frames for each type of missing list if there are missing values
# missing_programmeid_in_cr_df <- create_missing_df(missing_programmeid_in_cr, "Missing Cumulative ProgrammeID in CR")
# missing_programmeid_in_grantsdb_df <- create_missing_df(missing_programmeid_in_grantsdb, "Missing Cumulative ProgrammeID in GrantsDB")
# missing_gmgrn_in_cr_df <- create_missing_df(missing_gmgrn_in_cr, "Missing Cumulative GMGRN in CR")
# missing_gmgrn_in_grantsdb_df <- create_missing_df(missing_gmgrn_in_grantsdb, "Missing Cumulative GMGRN in GrantsDB")
# 
# # Combine all non-NULL data frames into one
# combined_missing_grant_cr_cumulative <- bind_rows(
#   missing_programmeid_in_cr_df,
#   missing_programmeid_in_grantsdb_df,
#   missing_gmgrn_in_cr_df,
#   missing_gmgrn_in_grantsdb_df
# )
# 
# rm(missing_gmgrn_in_cr, missing_gmgrn_in_grantsdb, missing_programmeid_in_cr, missing_programmeid_in_grantsdb)
# rm(grantsdb_gmgrn, grantsdb_programmeid)

## MUTED Comparing CR / MnE ----
# mne <- read.csv("data/cleaned/MnE Report Tracker.csv", encoding = "UTF-8")
# 
# # mne %<>%
# #   rename(Active = paste0("Activein", report_year, "."))
#          
# mne_leadgrn <- mne %>% 
#   # filter(Active == "Yes") %>% 
#   select(ProgrammeID) %>% unique()
# mne_gmgrn <- mne %>% 
#   # filter(Active == "Yes") %>% 
#   select(GMGRN) %>% unique()
# cr_leadgrn <- df %>% filter(exercice == annual) %>% select(ProgrammeID) %>% unique()
# cr_gmgrn <- df %>% filter(exercice == annual) %>% select(GMGRN) %>% unique()
# 
# # Find missing LeadGRN and GMGRN between mne and df
# missing_programmeid_in_cr <- setdiff(mne_leadgrn$ProgrammeID, cr_leadgrn$ProgrammeID)
# missing_programmeid_in_mne <- setdiff(cr_leadgrn$ProgrammeID, mne_leadgrn$ProgrammeID)
# missing_gmgrn_in_cr <- setdiff(mne_gmgrn$GMGRN, cr_gmgrn$GMGRN)
# missing_gmgrn_in_mne <- setdiff(cr_gmgrn$GMGRN, mne_gmgrn$GMGRN)
# 
# # Create data frames for each type of missing list if there are missing values
# missing_programmeid_in_cr_df <- create_missing_df(missing_programmeid_in_cr, "Missing ProgrammeID in CR")
# missing_programmeid_in_mne_df <- create_missing_df(missing_programmeid_in_mne, "Missing ProgrammeID in MnE")
# missing_gmgrn_in_cr_df <- create_missing_df(missing_gmgrn_in_cr, "Missing GMGRN in CR")
# missing_gmgrn_in_mne_df <- create_missing_df(missing_gmgrn_in_mne, "Missing GMGRN in MnE")
# 
# # Combine all non-NULL data frames into one
# combined_missing_mne_cr <- bind_rows(
#   missing_programmeid_in_cr_df,
#   missing_programmeid_in_mne_df,
#   missing_gmgrn_in_cr_df,
#   missing_gmgrn_in_mne_df
# )
# 
# rm(missing_gmgrn_in_cr_df, missing_gmgrn_in_mne_df, missing_programmeid_in_cr_df,
#    missing_programmeid_in_mne_df, mne_gmgrn, mne_leadgrn, cr_gmgrn, cr_leadgrn)

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

# # MUTED Accuracy of list of programs active current year ----
# # Cross check Grants DB and CR
# cr_start_end <- df %>%
#   filter(ProgrammeID %in% list_grants_StartDec | ProgrammeID %in% list_grants_EndJan &
#            Status == "Reached") %>%
#   group_by(ProgrammeID, Typeofreporting, Gender) %>%
#   summarise(NumberCR = sum(Number))


# OUTPUT ----
# Prep consistency CR - GrantsDB - MnE Tracker
# consistency_cr_db_mne <- rbind(combined_missing_grant_cr_annual, combined_missing_grant_cr_cumulative,
#                                combined_missing_mne_cr)
# # Add the sheet to the workbook
# addWorksheet(wb, "CR_MnE_DB_consistency")
# writeData(wb, "CR_MnE_DB_consistency", consistency_cr_db_mne)
# # Add the sheet to the workbook
# addWorksheet(wb, "AccuracyProgramsCurrentYear")
# writeData(wb, "AccuracyProgramsCurrentYear", cr_start_end)


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

# Create a summary sheet with TRUE flags
summary <- df_subset %>%
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
  sheet_data <- df_subset %>%
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


# PR FLOW - Data checks ----

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/pr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

## Load ----
df <- read.csv("data/cleaned/PR Combiner - For Checks.csv", encoding = "UTF-8")

## MnE import for filtering -----
mne <- read_xlsx(mne_approval,
                 sheet = "Results_Reports",
                 skip = 6)

mne %<>%
  filter(`GM Status` == "Completed")

df %<>%
  filter(ProgrammeID %in% mne$`Programme ID`)

## Filter annual / cumulative ----
df %<>%
  filter(exercice == annual |
           exercice == cumulative)


## Codes spelling ----
# adding here the check for analysis codes as it is not possible in 2023 to do it before filtering
list_codes <- read_excel(paste0("data/input/analysis_codes/", codes), sheet = "in")
codes_old <- read_excel(paste0("data/input/analysis_codes/", codes), sheet = "old")

list_codes <- list_codes %>%
  mutate(codes = if_else(grepl("n2cwd", codes, ignore.case = TRUE), 
                         "n2_cwd", codes))
list_codes <- list_codes %>%
  mutate(codes = if_else(grepl("t2a", codes), 
                         "t2_a", codes))
list_codes <- list_codes %>%
  mutate(codes = if_else(grepl("t6a", codes), 
                         "t6_a", codes))

list_codes <- list_codes %>%
  mutate(codes = if_else(grepl("t6c", codes), 
                         "t6_c", codes))

list_codes <- tolower(list_codes$codes)

codes_old <- trimws(tolower(codes_old$codes))

df <- df %>%
  mutate(WrongCode = !(code %in% list_codes | code %in% codes_old)
         & !is.na(code))

## Code NA ----
codeNA <- df %>%
  group_by(ProgrammeID, LeadGRN) %>%
  filter(all(is.na(code))) %>%
  ungroup() %>% distinct(LeadGRN)

wrong_code <- df %>%
  filter(WrongCode == TRUE) %>% 
  select(ProgrammeID, LeadGRN, code) %>% 
  unique()


# Output ----
# Add the sheets to the workbook
addWorksheet(wb, "WrongCode")
writeData(wb, "WrongCode", wrong_code)

addWorksheet(wb, "CodeNA")
writeData(wb, "CodeNA", codeNA)

## Comparing PR / MnE / GMS ----
grants <- read.csv(paste0("data/input/", grants_db), encoding = "UTF-8")
mne <- read.csv("data/cleaned/MnE Report Tracker.csv", encoding = "UTF-8")

# CHECK Unique ProgrammeID / LeadGRN PR and GMS
unique_pr <- df %>% distinct(ProgrammeID, LeadGRN) %>% rename(GRN = LeadGRN)
unique_gms <- grants %>% filter(Reportingrole != "Non-lead") %>% distinct(ProgrammeID, GMGRN) %>% rename(GRN = GMGRN)
unique_mne <- df %>% filter(.data[[paste0("Activein", report_year)]] == "Yes") %>% distinct(ProgrammeID, LeadGRN) %>% rename(GRN = LeadGRN)

# Create the cube
all_pairs <- union(union(unique_pr, unique_gms), unique_mne)

cube_pairs <- all_pairs %>%
  left_join(unique_pr  %>% mutate(inPR  = TRUE),
            by = c("ProgrammeID","GRN")) %>%
  left_join(unique_gms %>% mutate(inGMS = TRUE),
            by = c("ProgrammeID","GRN")) %>%
  left_join(unique_mne %>% mutate(inMNE = TRUE),
            by = c("ProgrammeID","GRN")) %>%
  mutate(across(c(inPR,inGMS,inMNE), ~replace_na(.x, FALSE))) %>%
  mutate(
    GMSnotPR =  inGMS & !inPR,
    MNEnotPR =  inMNE & !inPR,
  ) %>%
  arrange(ProgrammeID, GRN)


# Filter codes and LeadGRN----
# From here only priority codes and LeadGRN are under checks
# Load priority list
priority_codes <- read.xlsx("data/input/patch/priority_grn_codes.xlsx",
                      sheet = "Codes")
priority_leadGRN <- read.xlsx("data/input/patch/priority_grn_codes.xlsx",
                      sheet = "LeadGRN")

priority_codes %<>% select(Analysis.code, Priority, Level)
priority_codes %<>% 
  rename(code = Analysis.code,
          level_code = Level) %>%
  mutate(code = trimws(tolower(code)))

df %<>% 
  left_join(priority_codes, by = "code")

# Filter priority
priority_codes %<>%
  filter(Priority == 1)

priority_leadGRN %<>% 
  mutate(LeadGRN = trimws(Lead.Grantee.GRN))

df %<>%
  filter(LeadGRN %in% priority_leadGRN$LeadGRN)

df %<>%
  filter(code %in% priority_codes$code)

## Unit of measure ----
### Unit of measure missing ----
df %<>%
  mutate(
    NoUnitMeasure = (is.na(unit) | trimws(unit) == "" | unit == 0) &
      (!is.na(code) & trimws(code) != "") &
      !is.na(Total)
  )

### Unit consistent over time ----
# on Percentage and Number
# to be saved in cube
unit_overtime <- df %>%
  group_by(ProgrammeID, LeadGRN, code, unit) %>%
  summarise(Total = n(), .groups = 'drop') %>%
  pivot_wider(names_from = unit, values_from = Total)

unit_overtime <- unit_overtime %>%
  mutate(non_empty_count = rowSums(!is.na(select(., `Number #`, `Percentage %`)) & 
                                     select(., `Number #`, `Percentage %`) != "")) %>%
  filter(non_empty_count > 1)

### Unit consistency with indicator library ----
df %<>%
  rename(Indicatorfromlibrary = IndicatorFromLibrary) %>%
  mutate(InconsistencyUnitLibrary = case_when(
    (grepl("%|#|percentage|number", Indicatorfromlibrary, ignore.case = TRUE) & is.na(unit)) ~ TRUE,
           (grepl("%|percentage", Indicatorfromlibrary, ignore.case = TRUE) & unit == "Number #") ~ TRUE,
           (grepl("#|number", Indicatorfromlibrary, ignore.case = TRUE) & unit == "Percentage %") ~ TRUE,
    TRUE ~ FALSE))

## Figure-unit consistency ----
df %<>%
  mutate(FigureUnitInconsistency = case_when(
    unit == "Number #" & (Total > 0 & Total < 1) ~ TRUE,
    unit == "Percentage %" & (Total > 0 & Total < 1.01) ~ TRUE,
      TRUE ~ FALSE
  ))

### Unit consistency priority codes ----
# to be saved in cube
unit_consistency_priority <- df %>%
  group_by(code, unit) %>%
  summarise(Total = n(), .groups = 'drop') %>%
  pivot_wider(names_from = unit, values_from = Total)


unit_consistency_priority <- unit_consistency_priority %>%
  rowwise() %>%
  mutate(non_empty_count = sum(!is.na(across(2:ncol(unit_consistency_priority))) & across(2:ncol(unit_consistency_priority)) != "")) %>%
  ungroup() %>%
  filter(non_empty_count > 1)

## Source ----
df %<>%
  mutate(
    WrongSource = case_when(
      Sourceofdata != source_pr &
        (exercice %in% c(annual, cumulative)) &
        .data[[paste0("Activein", report_year)]] == "yes" ~ TRUE,
      .default = FALSE
    )
  )

## Indicators not missing ----
df %<>%
  mutate(IndicatorMissing = case_when(
    Gender == "Total" & code == "" &
    (code == "" | is.na(code)) 
    & exercice == annual ~ TRUE,
    TRUE ~ FALSE
  ))

## Past indicators not missing ----
df %<>%
  mutate(PastIndicatorMissing = case_when(
    Gender == "Total" & 
      (code == "" | is.na(code)) & exercice == past_annual ~ TRUE,
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
  group_by(ProgrammeID, LeadGRN, Sourceofdata, Gender, Reportingwindow, Exercise, code) %>%
  mutate(LowestUnitDuplicated = n() > 1) %>%  # Add column indicating duplicates
  ungroup() 

## Data source check ----
df %<>%
  mutate(
    WrongSource = case_when(
      .data[[paste0("Activein", report_year)]] == "Yes" & Sourceofdata != source_pr ~ TRUE,
      .data[[paste0("Activein", report_year)]] == "No"  & Sourceofdata == source_pr ~ TRUE,
      TRUE ~ FALSE
    )
  )

## e46 check ----
e46 <- df %>%
  filter(grepl("^e46", code, ignore.case = TRUE)) %>%   # any code starting with e46
  group_by(LeadGRN, code) %>%
  summarize(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = code, values_from = Count, values_fill = 0) %>%
  mutate(
    # TRUE if "e46" is 0, but sum of other e46* codes > 0
    e46_missing_but_others_present =
      ifelse(`e46` == 0 & rowSums(select(., starts_with("e46"))) > 0, TRUE, FALSE)
  )
  

# Output ----
# Add the sheet to the workbook
addWorksheet(wb, "PR_MnE_GMS_Cube")
writeData(wb, "PR_MnE_GMS_Cube", cube_pairs)

addWorksheet(wb, "e46_check")
writeData(wb, "e46_check", e46)

# Mapping of column names to descriptive labels
label_map <- c(
  "NoUnitMeasure" = "Missing Unit of Measure",
  "InconsistencyUnitLibrary" = "Unit of Measure inconsistent per indicator",
  "FigureUnitInconsistency" = "Unit inconsistent with figure",
  "WrongSource" = "Wrong PR",
  "IndicatorMissing" = "Indicator specification missing",
  "PastIndicatorMissing" = "Indicator specification missing for past pr",
  "AllRowDuplicate" = "Entire row duplicated",
  "LowestUnitDuplicated" = "Lowest reporting unit duplicated"
)

# Subset the data frame
columns_to_check <- names(label_map)
df_subset <- df %>% select(ProgrammeID, LeadGRN, code, all_of(columns_to_check))
cube <- df_subset %>% unique()

## FILTER Log decisions ----
log <- read.xlsx("data/input/log_decisions/log_decisions.xlsx", sheet = "pr")

cube %<>%
  pivot_longer(cols = all_of(columns_to_check), names_to = "Issue", values_to = "Flag") %>%
  mutate(Concat = paste0(ProgrammeID, LeadGRN, code, Issue))

log %<>%
  mutate(Concat = paste0(ProgrammeID, LeadGRN, code, Issue))

cube %<>%
  mutate(Flag = ifelse(Concat %in% log$Concat, FALSE, Flag)) %>%
  select(-Concat)

cube %<>%
  arrange(desc(Flag)) %>%  # put TRUE first
  distinct(ProgrammeID, LeadGRN, code, Issue, .keep_all = TRUE)

cube %<>%
  pivot_wider(names_from = Issue, values_from = Flag)



# Create a summary sheet with TRUE flags
summary <- cube %>%
  pivot_longer(cols = all_of(columns_to_check), names_to = "issue", values_to = "flag") %>%
  filter(flag == TRUE) %>%
  mutate(issue = label_map[issue]) %>%
  select(ProgrammeID, LeadGRN, code, issue) %>%
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
    select(ProgrammeID, LeadGRN, code, !!sym(col)) %>%
    unique()
  
  if (nrow(sheet_data) > 0) {  # Only create the sheet if there are TRUE rows
    addWorksheet(wb, col)
    writeData(wb, col, sheet_data)
  }
}

# Output unit consistency
addWorksheet(wb, "ConsistencyUnitOvertimeLeadGRN")
writeData(wb, "ConsistencyUnitOvertimeLeadGRN", unit_overtime)
addWorksheet(wb, "ConsistencyUnitCode")
writeData(wb, "ConsistencyUnitCode", unit_consistency_priority)

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Data checks.xlsx"), overwrite = TRUE)

rm(list = ls())

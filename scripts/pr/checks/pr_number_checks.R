# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/pr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# PR CHECKS ----
## Load ---
df <- read.csv("data/cleaned/PR Combiner - For Checks.csv", encoding = "UTF-8")

df %<>% 
  filter(Exercise == "ARR24 Annual" | Exercise == "ARR24 Cumulative")

# adding here the check for analysis codes as it is not possible in 2023 to do it before filtering
list_codes <- read_excel(paste0("data/input/analysis_codes/", codes), sheet = "in")

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

# Computed vs Imputed Totals ----
## Annual ----
### Filtering ----

df1 <- df %>%
  filter(exercice == annual & unit == "Number #")

annual_totals <- df1 %>%
  group_by(ProgrammeID, LeadGRN, Reportingwindow, code, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(c(Female, Male, Total), ~ tidyr::replace_na(., 0))) %>%
  mutate(
    ComputedTotal = Female + Male,
    Consistent = Total == ComputedTotal,
    ID = paste0(LeadGRN, "___", code)
  )


rm(df1)

## Cumulative ----
### Filtering ----

df1 <- df %>%
  filter(exercice == cumulative & unit == "Number #")

cumulative_totals <- df1 %>%
  group_by(ProgrammeID, LeadGRN, Reportingwindow, code, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Gender, values_from = Total) %>%
  mutate(across(c(Female, Male, Total), ~ tidyr::replace_na(., 0))) %>%
  mutate(ComputedTotal = Female + Male,
         Consistent = Total == ComputedTotal,
         ID = paste0(LeadGRN, "___", code))


rm(df1)

## Summary ----

annual_totals_summary <- annual_totals %>%
  filter(ComputedTotal != 0 & Consistent != TRUE) %>%
  mutate(ID = "Annual")

cumulative_totals_summary <- cumulative_totals %>%
  filter(ComputedTotal != 0 & Consistent != TRUE) %>%
  mutate(ID = "Cumulative")

totals_summary <- rbind(annual_totals_summary, cumulative_totals_summary)

# Reporting windows ----
## Filtering ----

df1 <- df %>%
  filter(Exercise == "ARR24 Cumulative") %>%
  filter(grepl("cumulative", Exercise, ignore.case = TRUE) & unit == "Number #") %>%
  mutate(extracted_chars = substr(Exercise, 4, 5),
         RW = trimws(paste(extracted_chars, Reportingwindow))) %>%
  arrange(RW)

sequential <- df1 %>%
  filter(Gender == "Total" &
           !grepl("achieved", RW, ignore.case = TRUE)) %>%
  group_by(ProgrammeID, LeadGRN, code, RW) %>%
  summarise(Total = sum(Total), na.rm = TRUE) %>%
  pivot_wider(names_from = RW, values_from = Total) %>%
  select(-c(`24 Revised_target`, `24 Target`, `24 Baseline`))

# Get the sorted columns
sorted_columns <- sort(colnames(sequential)[grepl("rw", colnames(sequential), ignore.case = TRUE)])

# Get the remaining columns that do not match "rw"
remaining_columns <- setdiff(colnames(sequential), sorted_columns)

# Combine the sorted columns and the remaining columns
all_columns <- c(remaining_columns, sorted_columns)

# Reorder the data frame
sequential <- sequential[, all_columns, drop = FALSE]

sequential <- sequential %>%
  rowwise() %>%
  mutate(non_na_counts = sum(!is.na(across(4:last_col()))))

rw_check <- sequential %>%
  filter(non_na_counts > 1) %>%
  select(-non_na_counts)

# yr <- "24"
# 
# rw_cols <- grep(paste0("^\\s*", yr, "\\s*RW[1-6]\\s*$"),
#                 names(sequential), value = TRUE)
# 
# # Make sure they’re ordered RW1, RW2, …, RW6
# rw_cols <- rw_cols[order(as.integer(str_extract(rw_cols, "RW(\\d+)")))]
# 
# step1 <- sequential %>%
#   rowwise(LeadGRN, code) %>%
#   mutate(vals = list(c_across(all_of(rw_cols)))) %>%
#   ungroup()
# 
# step2 <- step1 %>%
#   rowwise(LeadGRN, code) %>%
#   mutate(non_na_idx = list(which(!is.na(vals)))) %>%
#   ungroup()
# 
# step3 <- step2 %>%
#   rowwise(LeadGRN, code) %>%
#   mutate(
#     latest_idx   = if (length(non_na_idx[[1]]) >= 1) tail(non_na_idx[[1]], 1) else NA_integer_,
#     previous_idx = if (length(non_na_idx[[1]]) >= 2) tail(non_na_idx[[1]], 2)[1] else NA_integer_,
#     latest_val      = if (!is.na(latest_idx))   vals[[1]][latest_idx]   else NA_real_,
#     previous_val    = if (!is.na(previous_idx)) vals[[1]][previous_idx] else NA_real_,
#     latest_col      = if (!is.na(latest_idx))   rw_cols[latest_idx]     else NA_character_,
#     previous_col    = if (!is.na(previous_idx)) rw_cols[previous_idx]   else NA_character_
#   ) %>%
#   ungroup()
# 
# # Now you have the two non-NA RW values rowwise:
# # step3 %>% select(LeadGRN, code, latest_col, latest_val, previous_col, previous_val) %>% slice(1:5)




# Function to check if each value is greater than or equal to the value in the previous column
# Function to check if each value is greater than or equal to the value in the previous column and append a flag column
check_non_na_order <- function(df) {
  # Subset the dataframe to only include columns from the 4th onwards
  df_sub <- df[, 4:ncol(df)]
  # Function to check if each value in the row is >= the next non-NA value
  is_ordered <- function(row) {
    # Remove NAs from the row
    row <- row[!is.na(row)]
    # Check if the row is in non-increasing order
    all(diff(row) < 0)
  }
  # Apply the is_ordered function to each row
  order_check <- apply(df_sub, 1, is_ordered)
  # Add the flag column to the original dataframe
  df$Flag <- order_check
  return(df)
}


rw_check <- check_non_na_order(rw_check)

rw_check_summary <- rw_check %>%
  mutate(Flag = ifelse(Flag == TRUE, "Discrepancy", NA)) %>%
  filter(Flag == "Discrepancy")


# Summary Sheet ----
totals_checks_summary<- totals_summary %>%
  mutate(Issue = "Computed total not equal imputed total") %>%
  select(LeadGRN, code, Issue)

rw_check_summary <- rw_check_summary %>%
  mutate(Issue = "Latest Reporting Window smaller than previous ones") %>%
  select(LeadGRN, code, Issue)

summary <- rbind(totals_checks_summary, rw_check_summary)

# List of checks and their corresponding sheet names
checks_list <- list(
  "OverallSummary" = summary,
  "ComputedVSImputed" = totals_summary,
  "SequentialRW" = sequential,
  "RWCheck" = rw_check_summary
)

# Loop through each check object and add to the workbook
for (sheet_name in names(checks_list)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, checks_list[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Number checks.xlsx"), overwrite = TRUE)



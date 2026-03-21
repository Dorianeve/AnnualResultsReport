### Prep environment ----
source("scripts/prep_env.R")

# Find the latest ISO-date folder (YYYY-MM-DD, no suffix) within a base path
get_latest_folder <- function(base_path) {
  dirs <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  date_dirs <- dirs[grepl("^\\d{4}-\\d{2}-\\d{2}$", dirs)]
  if (length(date_dirs) == 0) stop("No ISO-date folders found in: ", base_path)
  file.path(base_path, sort(date_dirs, decreasing = TRUE)[1])
}

# Load all sheets whose name contains "summary" (case-insensitive) from an xlsx file.
# Returns a named list of data frames, or NULL if the file doesn't exist / has no summary sheets.
load_summary_sheets <- function(file_path) {
  if (!file.exists(file_path)) {
    message("File not found, skipping: ", file_path)
    return(NULL)
  }
  sheets <- excel_sheets(file_path)
  summary_sheets <- sheets[grepl("summary", sheets, ignore.case = TRUE)]
  if (length(summary_sheets) == 0) {
    message("No summary sheets found in: ", basename(file_path))
    return(NULL)
  }
  setNames(
    lapply(summary_sheets, function(s) read_excel(file_path, sheet = s)),
    summary_sheets
  )
}

### CR checks ----
cr_latest <- get_latest_folder("data/output/cr")
message("Loading CR checks from: ", cr_latest)

cr_data_checks  <- load_summary_sheets(file.path(cr_latest, "Data checks.xlsx"))
# cr_new_checks   <- load_summary_sheets(file.path(cr_latest, "New checks.xlsx"))
cr_num_reached  <- load_summary_sheets(file.path(cr_latest, "Number checks - Reached.xlsx"))
cr_num_targeted <- load_summary_sheets(file.path(cr_latest, "Number checks - Targeted.xlsx"))

### PR checks ----
pr_latest <- get_latest_folder("data/output/pr")
message("Loading PR checks from: ", pr_latest)

pr_data_checks <- load_summary_sheets(file.path(pr_latest, "Data checks.xlsx"))
pr_num_checks  <- load_summary_sheets(file.path(pr_latest, "Number checks.xlsx"))

### Tag and combine ----

# Add FLOW and CheckType columns to every data frame in a list, then row-bind them
tag_checks <- function(checks_list, flow, check_type) {
  if (is.null(checks_list)) return(NULL)
  bind_rows(lapply(checks_list, function(df) {
    df %>% mutate(FLOW = flow, CheckType = check_type, .before = 1)
  }))
}

all_checks <- bind_rows(
  tag_checks(cr_data_checks,  "CR", "Data checks"),
  tag_checks(cr_num_reached,  "CR", "Number checks - Reached"),
  tag_checks(cr_num_targeted, "CR", "Number checks - Targeted"),
  tag_checks(pr_data_checks,  "PR", "Data checks"),
  tag_checks(pr_num_checks,   "PR", "Number checks")
) 

all_checks %<>%
  mutate(Issue = ifelse(!is.na(Issue), Issue, issue)) %>%
  select(-c(issue))

all_checks %<>%
  mutate(Issue = ifelse(!is.na(Check), Check, Issue)) %>%
  select(-c(Check))

all_checks %<>%
  rename(AnalysisCode = code,
         Message = message)

all_checks %<>%
  select(FLOW, ProgrammeID, LeadGRN, GMGRN, CheckType, AnalysisCode, Issue, Message)

write.csv(all_checks, "data/output/checks/airtable_checks.csv", row.names = FALSE)

push_to_airtable(all_checks, AIRTABLE_TOKEN_ARR, BASE_ID_ARR, TABLE_NAME_ARR)


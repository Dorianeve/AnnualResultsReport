# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/grants db/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# GRANTSDB CHECK ----
# grants_typereporting <- read.xlsx(paste0("data/input/", combiner), sheet = sheet_grants, 
#                     detectDates = TRUE, sep.names = " ")

cr_targets <- read.xlsx(paste0("data/input/", combiner), sheet = sheet_cr_targets, 
                    detectDates = TRUE, sep.names = " ")

grants <- read.csv("data/input/new_grants_db.csv", encoding = "UTF-8")

cr_targets %<>% 
  rename_with(~ gsub(" ", "", .x))

# grants_typereporting %<>%
#   rename_with(~ gsub(" ", "", .x))

grants %<>%
  rename_with(~ gsub("\\.", "", .x))

grants %<>%
         rename(CurrentEndDate = Currentenddate)

cr_targets %<>%
  rename(LeadGRN = LeadgranteeGRN,
         CurrentEndDate = Currentenddate)

cr_targets %<>% 
  distinct(ProgrammeID, LeadGRN, GMGRN)

# grants_typereporting %<>%
#   select(ProgrammeID, GMGRN, Reportingrole)

grants %<>%
  left_join(cr_targets, by = c("ProgrammeID","GMGRN"))

# updated_db <- grants %>%
#   select(ProgrammeID, LeadGRN, GMGRN, Startdate, Enddate, CurrentEndDate)
# write.csv(updated_db, paste0("data/lists/", today, " grants_db_updated.csv"),
#                              row.names = FALSE)

number_leadgrn <-
  grants %>%
  filter(!is.na(LeadGRN)) %>%
  group_by(ProgrammeID) %>%
  summarize(CountLeadGRN = n_distinct(LeadGRN)) %>%
  filter(CountLeadGRN > 1)
number_leadgrn


number_gmgrn <-
  grants %>%
  group_by(ProgrammeID) %>%
  summarize(CountGMGRN = n_distinct(GMGRN)) %>%
  arrange(desc(CountGMGRN))
number_gmgrn

unique_gmgrn_programme <- number_gmgrn %>% filter(CountGMGRN == 1) %>% select(ProgrammeID)
two_gmgrn_programme <- number_gmgrn %>% filter(CountGMGRN == 2) %>% select(ProgrammeID)
more_gmgrn_programme <- number_gmgrn %>% filter(CountGMGRN > 2) %>% select(ProgrammeID)

single_reporting <- grants %>%
  filter(ProgrammeID %in% unique_gmgrn_programme$ProgrammeID,
         Reportingrole != "Single") %>%
  select(ProgrammeID, LeadGRN, GMGRN, Reportingrole)
single_reporting

two_reporting <- grants %>%
  filter(ProgrammeID %in% two_gmgrn_programme$ProgrammeID) %>%
  group_by(ProgrammeID, Reportingrole) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = Reportingrole, values_from = Count) %>%
  filter(Lead != 1 | `Non-lead` != 1 | is.na(Lead) | is.na(`Non-lead`))
two_reporting

more_reporting <- grants %>%
  filter(ProgrammeID %in% more_gmgrn_programme$ProgrammeID) %>%
  group_by(ProgrammeID, Reportingrole) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = Reportingrole, values_from = Count) %>%
  filter(Lead != 1 | `Non-lead` < 2 | is.na(Lead) | is.na(`Non-lead`))
more_reporting

role_consistent <- grants %>%
  filter(LeadGRN == GMGRN) %>%
  filter(Reportingrole != "Single" & Reportingrole != "Lead") %>%
  select(ProgrammeID, LeadGRN, GMGRN, Reportingrole)
role_consistent

role_non_lead <- grants %>%
  filter(LeadGRN != GMGRN) %>%
  filter(Reportingrole != "Non-lead") %>%
  select(ProgrammeID, LeadGRN, GMGRN, Reportingrole)
role_non_lead

excom_date <- grants %>%
  group_by(ProgrammeID, GMGRN) %>%
  summarize(UniqueDates = n_distinct(Excomapprovaldate)) %>%
  filter(UniqueDates > 1)
excom_date
  
context_emergency_empty <- grants %>%
  filter((is.na(Crisiscontext) | Crisiscontext == "") |
         (Typeofemergency == "" | is.na(Typeofemergency))) %>%
  select(ProgrammeID, LeadGRN, GMGRN, Crisiscontext, Typeofemergency)
context_emergency_empty

list_programmes <- grants %>%
  select(ProgrammeID) %>%
  unique()

rm(grants, cr_targets, unique_gmgrn_programme, two_gmgrn_programme, more_gmgrn_programme)

list_programmes %<>%
  mutate(Issue = case_when(
    ProgrammeID %in% number_leadgrn$ProgrammeID ~ "LeadGRN per ProgrammeID",
    ProgrammeID %in% single_reporting$ProgrammeID ~ "Reporting role assigned - single",
    ProgrammeID %in% two_reporting$ProgrammeID ~ "Reporting role assigned - two",
    ProgrammeID %in% more_reporting$ProgrammeID ~ "Reporting role assigned - more",
    ProgrammeID %in% role_consistent$ProgrammeID ~ "Reporting role consistent - lead/single",
    ProgrammeID %in% role_non_lead$ProgrammeID ~ "Reporting role consistent - non-lead",
    ProgrammeID %in% excom_date$ProgrammeID ~ "ExCom date with ProgrammeID consistent",
    ProgrammeID %in% context_emergency_empty$ProgrammeID ~ "Crisis context and type of emergency missing values",
    TRUE ~ NA
  ))

summary <- list_programmes %<>%
  filter(!is.na(Issue))

rm(list_programmes)

objects <- c("summary",
             "number_leadgrn", 
             "single_reporting", "two_reporting", "more_reporting",
             "role_consistent", "role_non_lead", "excom_date", "context_emergency_empty")

# Function to add each object as a sheet
add_sheet_to_wb <- function(sheet_name, data) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
}

# Loop through the objects and add them as sheets (only if they exist and are data frames)
for (obj in objects) {
  if (exists(obj) && is.data.frame(get(obj))) {  
    add_sheet_to_wb(obj, get(obj))
  }
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Grant db checks.xlsx"), overwrite = TRUE)

rm(list = ls())
# CR FLOW - Data checks New ----

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR New checks ----
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

# Load GrantsDB ----
grants <- read.csv(paste0("data/input/", grants_db), encoding = "UTF-8")

# Load MnE approval ----
mne <- read.csv("data/cleaned/MnE Report Tracker.csv", encoding = "UTF-8")

# CHECK Unique ProgrammeID / LeadGRN CR and GMS ----
unique_cr <- df %>% distinct(ProgrammeID, LeadGRN) %>% rename(GRN = LeadGRN)
unique_gms <- grants %>% filter(Reportingrole != "Non-lead") %>% distinct(ProgrammeID, GMGRN) %>% rename(GRN = GMGRN)

missing_gms <- setdiff(unique_cr, unique_gms)
missing_cr <- setdiff(unique_gms, unique_cr)

# CHECK Annual / Joint / GMS / MnE ----
pid_cr <- df %>% filter(exercice == annual & Typeofreporting == "Joint") %>% distinct(ProgrammeID)
pid_gms <- grants %>% filter(get(paste0("Activein", report_year)) == "Yes") %>% distinct(ProgrammeID)
pid_mne <- mne %>% distinct(ProgrammeID)

# Step 2: Pull vectors of ProgrammeIDs
cr_ids <- pid_cr$ProgrammeID
gms_ids <- pid_gms$ProgrammeID
mne_ids <- pid_mne$ProgrammeID

# Step 3: Compare
only_in_cr   <- setdiff(cr_ids, union(gms_ids, mne_ids))
only_in_gms  <- setdiff(gms_ids, union(cr_ids, mne_ids))
only_in_mne  <- setdiff(mne_ids, union(cr_ids, gms_ids))

in_cr_not_in_gms <- setdiff(cr_ids, gms_ids)
in_cr_not_in_mne <- setdiff(cr_ids, mne_ids)

in_gms_not_in_cr <- setdiff(gms_ids, cr_ids)
in_mne_not_in_cr <- setdiff(mne_ids, cr_ids)

# CHECK Cumulative / Joint / GMS ----
pid_cr <- df %>% filter(exercice == cumulative & Typeofreporting == "Joint") %>% distinct(ProgrammeID)
pid_gms <- grants %>% distinct(ProgrammeID)

only_in_cr_cum <- setdiff(pid_cr, pid_gms)
only_in_gms_cum <- setdiff(pid_gms, pid_cr)

# CHECK Individual GMGRN / GMS ----
gmgrn_cr <- df %>% filter(exercice == annual & Typeofreporting == "Individual") %>% distinct(GMGRN)
gmgrn_gms <- grants %>% filter(get(paste0("Activein", report_year)) == "Yes" &
                                 Excomapprovaldate < ex_com_approval_date_cutoff) %>% distinct(GMGRN)

only_in_cr_gmgrn <- setdiff(gmgrn_cr, gmgrn_gms)
only_in_gms_gmgrn <- setdiff(gmgrn_gms, gmgrn_cr)


# Output ----
# Building CUBE
cube_pid <- grants$ProgrammeID
cube_pid <- as.data.frame(cube_pid)

cube_pid %<>% 
  mutate(PIDAnnualJointinCRnotGMS =
           ifelse(cube_pid %in% in_cr_not_in_gms, TRUE, FALSE),
         PIDAnnualJointinCRnotMnE = 
           ifelse(cube_pid %in% in_cr_not_in_mne, TRUE, FALSE),
         PIDAnnualJointinGMSnotCR = 
           ifelse(cube_pid %in% in_gms_not_in_cr, TRUE, FALSE),
         PIDAnnualJointinMnEnotCR = 
           ifelse(cube_pid %in% in_mne_not_in_cr, TRUE, FALSE))

cube_pid %<>% 
  mutate(PIDCumulativeJointinCRnotGMS =
           ifelse(cube_pid %in% only_in_cr_cum, TRUE, FALSE),
         PIDCumulativeJointinGMSnotCR = 
           ifelse(cube_pid %in% only_in_gms_cum, TRUE, FALSE))

cube_grn <- grants$GMGRN
cube_grn <- as.data.frame(cube_grn)

cube_grn %<>%
  mutate(GRNinCRnotGMS =
           ifelse(cube_grn %in% missing_gms$GRN, TRUE, FALSE),
         GRNinGMSnotCR =
           ifelse(cube_grn %in% missing_cr$GRN, TRUE, FALSE),
         GRNAnnualIndividualinCRnotGMS = 
           ifelse(cube_grn %in% only_in_cr_gmgrn$GMGRN, TRUE, FALSE),
         GRNAnnualIndividualinGMSnotCR = 
           ifelse(cube_grn %in% only_in_gms_gmgrn$GMGRN, TRUE, FALSE))


## FILTER Log decisions GRN ----
log <- read.xlsx("data/input/log_decisions/log_decisions.xlsx", sheet = "cr")

cube_grn %<>%
  pivot_longer(cols = -1, names_to = "Issue", values_to = "Flag") %>%
  mutate(Concat = paste0(cube_grn, Issue))

log %<>%
  mutate(Concat = paste0(GMGRN, Issue))

cube_grn %<>%
  mutate(Flag = ifelse(Concat %in% log$Concat, FALSE, Flag)) %>%
  select(-Concat)

cube_grn %<>%
  arrange(desc(Flag)) %>%  # put TRUE first
  distinct(cube_grn, Issue, .keep_all = TRUE)

cube_grn %<>%
  pivot_wider(names_from = Issue, values_from = Flag)

## FILTER Log decisions PID ----
log <- read.xlsx("data/input/log_decisions/log_decisions.xlsx", sheet = "cr")

cube_pid %<>%
  pivot_longer(cols = -1, names_to = "Issue", values_to = "Flag") %>%
  mutate(Concat = paste0(cube_pid, Issue))

log %<>%
  mutate(Concat = paste0(ProgrammeID, Issue))

cube_pid %<>%
  mutate(Flag = ifelse(Concat %in% log$Concat, FALSE, Flag)) %>%
  select(-Concat)

cube_pid %<>%
  arrange(desc(Flag)) %>%  # put TRUE first
  distinct(cube_pid, Issue, .keep_all = TRUE)

cube_pid %<>%
  pivot_wider(names_from = Issue, values_from = Flag)

checks_list <- list(
  "Cube_ProgrammeID" = cube_pid,
  "Cube_GRN" = cube_grn
)

cube_grn %<>% unique()
cube_pid %<>% unique()

# Loop through each check object and add to the workbook
for (sheet_name in names(checks_list)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, checks_list[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "New checks.xlsx"), overwrite = TRUE)
rm(list = ls())                  

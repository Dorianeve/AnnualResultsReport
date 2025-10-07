# CR FLOW - Data checks New ----

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR New checks ----
df <- read.csv("data/cleaned/CR Combiner - For Checks.csv", encoding = "UTF-8")

## CR filtering ----
df %<>%
  filter(exercice == annual |
           exercice == cumulative)

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
pid_gms <- grants %>% filter(Activein2024 == "Yes") %>% distinct(ProgrammeID)
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
gmgrn_gms <- grants %>% filter(Activein2024 == "Yes" &
                                 Excomapprovaldate < "2024-01-01") %>% distinct(GMGRN)

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

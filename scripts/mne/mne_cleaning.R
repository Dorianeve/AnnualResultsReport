# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

# CLEAN MNE TRACKER ----
# Load MnE tracker ----
# put it in another script
df <- read.xlsx(paste0("data/input/", mne_approval), 
                sheet = sheet_mne, 
                startRow = 4,
                detectDates = TRUE, sep.names = " ")

## Column names ----
colnames(df) <- gsub("\\s+", "", colnames(df))

df <- df %>%
  rename(# LeadGRN = LeadgranteeGRN,
         Grantee = Granteeorganization,
         MEFocalPerson = `M&EFocalPerson`)

## ARR report year filter ----
# df <- df %>%
#   filter(ARRreportyear == report_year) %>%
#   select(-c(Latesubmissionapproveddate,
#             Extendeddeadline, 
#             Folder, 
#             LeadFolder, 
#             Templatesaved, 
#             Templateshared, 
#             GMComments, 
#             MnEcommentsforGM, 
#             MEFocalPerson, 
#             Linktotemplate, 
#             Commentsontemplate, 
#             Supervisorsincharge )) %>%
#   arrange(Country, Typeofinvestment, LeadGRN, Leadgrantee)


# Filter ----
df %<>%
  filter(Typeofinvestment != "AF",
         Reportingperiodupto > "2023-12-31")

df %<>%
  mutate(Activein2024 =
           ifelse(Startdate <= "2024-12-31" &
                    Currentenddate >= "2024-01-01", "Yes", "No"))

df %<>%
  mutate(ActiveStrategicPlan =
           ifelse(Startdate <= "2024-12-31" &
                    Currentenddate > "2023-01-01", "Yes", "No"))

# saving csv
write.csv(df, "data/cleaned/MnE Report Tracker.csv", row.names = FALSE)

rm(list = ls())

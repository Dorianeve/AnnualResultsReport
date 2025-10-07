# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/pr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

## Shorlisted LeadGRN and analysis codes filter ----
# adding lines for patches focusing on specific grants

## Load ----
df <- read.csv("data/cleaned/PR Combiner - For Checks.csv", encoding = "UTF-8")

priority_codes <- read.xlsx("data/input/patch/priority_grn_codes.xlsx",
                            sheet = "Codes")
# priority_leadGRN <- read.xlsx("data/input/patch/priority_grn_codes.xlsx",
#                               sheet = "LeadGRN")

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

# priority_leadGRN %<>% 
#   mutate(LeadGRN = trimws(Lead.Grantee.GRN))

# df %<>%
#   filter(LeadGRN %in% priority_leadGRN$LeadGRN)

df %<>%
  filter(code %in% priority_codes$code)

# df %<>% 
#   filter(Exercise == "ARR24 Annual" | Exercise == "ARR24 Cumulative")

# Take the latest RW
df %<>%
  filter(grepl("RW", Reportingwindow, ignore.case = TRUE))

df %<>%
  mutate(
    RW_number = case_when(
      Reportingwindow == "RW1" ~ 1,
      Reportingwindow == "RW2" ~ 2,
      Reportingwindow == "RW3" ~ 3,
      Reportingwindow == "RW4" ~ 4,
      Reportingwindow == "RW5" ~ 5,
      Reportingwindow == "RW6" ~ 6,
      TRUE ~ NA_real_
    )
  ) %>%
  # For each LeadGRN x RW_number, does this RW have any non-NA Value?
  group_by(LeadGRN, code, Exercise, RW_number) %>%
  mutate(rw_has_value = any(!is.na(Value))) %>%
  # For each LeadGRN, find the latest RW that has any data
  group_by(LeadGRN, code, Exercise) %>%
  mutate(target_rw = if (any(rw_has_value, na.rm = TRUE)) 
    max(RW_number[rw_has_value], na.rm = TRUE) 
    else NA_real_) %>%
  ungroup() %>%
  # Keep all rows from that RW (including rows with NA Value if others in the RW have data)
  filter(!is.na(target_rw), RW_number == target_rw) %>%
  select(-rw_has_value, -target_rw)


# Prep the GMS info ----
table(df$Activein2024, useNA = "always")
table(df$ActiveStrategicPlan, useNA = "always")

gms <- read.csv(paste0("data/input/", grants_db), encoding = "UTF-8")

gms %<>% select(ProgrammeID, GMGRN, Activein2024, ActiveStrategicPlan)

gms %<>%
  rename(Active2024control = Activein2024,
         ActivePlancontrol = ActiveStrategicPlan,
         LeadGRN = GMGRN)

df %<>%
  left_join(gms, by = c("ProgrammeID", "LeadGRN"))

df %<>%
  select(-c(Activein2024, ActiveStrategicPlan)) %>%
  rename(Activein2024 = Active2024control,
         ActiveStrategicPlan = ActivePlancontrol)

# Save ----
# saving csv (consider adding timestamp)
# write.csv(df, "data/cleaned/PR Combiner - For Checks.csv", row.names = FALSE)
write.csv(df, "data/cleaned/PR Combiner - For Analysis.csv", row.names = FALSE)

rm(list = ls())

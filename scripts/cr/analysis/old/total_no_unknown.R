# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/analysis/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

# CR ANALYSIS ANNUAL ----
df <- read.csv("data/cleaned/CR Combiner - For Analysis.csv", encoding = "UTF-8")

# filter
df %<>%
  filter(Status == "Reached" &
           (Typeofreporting != "Individual" | is.na(Typeofreporting)) &
           (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
           Covid19related == "Non-Covid-19 related data")

# totals
total <- df %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Exercise) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  mutate(Attribute = "Total (FER + MYRP)")

# gender
gender <- df %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Exercise, Gender) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  rename(Attribute = Gender)

# type of beneficiary
bens <- df %>%
  filter(Typeofbeneficiary != "Total" & Typeofbeneficiary != "Unknown" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Exercise, Typeofbeneficiary) %>%
  summarise(Total = sum(Number, na.rm = TRUE))%>%
  rename(Attribute = Typeofbeneficiary)
bens

# levelofeducation
edu_level <- df %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation != "Total" & Levelofeducation != "Unknown level of education" &
           Gender != "Total") %>%
  group_by(Exercise, Levelofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  rename(Attribute = Levelofeducation)
edu_level

# typeofeducation
edu_type <- df %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Exercise, Typeofeducation) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  rename(Attribute = Typeofeducation)
edu_type
# join
tb <- bind_rows(total, gender, bens, edu_level, edu_type)

# Table processing ----
# pivot wider
tb %<>%
  pivot_wider(names_from = Attribute, values_from = Total)
tb

# Step 1: Calculate totals needed per unknwon categories percentage columns
tb %<>%
  mutate(TotalLevelofEducation = `Pre-primary` + Primary + Secondary,
         TotalTypeofBens = Refugee + IDP + OAPs)

tb %<>%
  mutate(
    `PerFemale` = Female / `Total (FER + MYRP)`,
    `PerMale` = Male / `Total (FER + MYRP)`,
    `PerRefugee` = Refugee / `TotalTypeofBens`,
    `PerIDP` = IDP / `TotalTypeofBens`,
    `PerOAPs` = OAPs / `TotalTypeofBens`,
    `PerPre-primary` = `Pre-primary` / `TotalLevelofEducation`,
    `PerPrimary` = Primary / `TotalLevelofEducation`,
    `PerSecondary` = Secondary / `TotalLevelofEducation`,
    `PerChildren with disabilities` = `Children with disabilities` / `TotalTypeofBens`,
    `PerFormal education` = `Formal education` / `Total (FER + MYRP)`,
    `PerNon-formal education` = `Non-formal education` / `Total (FER + MYRP)`
  ) %>%
  select(-c(TotalLevelofEducation, TotalTypeofBens))

# Step 2: Pivot longer and extract correct BaseVar
tb_long <- tb %>%
  pivot_longer(-Exercise, names_to = "Variable", values_to = "Value") %>%
  mutate(
    Type = if_else(str_starts(Variable, "Per"), "Per", "Total"),
    BaseVar = if_else(
      Type == "Per",
      str_remove(Variable, "^Per"),
      Variable
    )
  )

# Step 3: Pivot wider with aligned BaseVar
tb_wide <- tb_long %>%
  pivot_wider(
    id_cols = BaseVar,
    names_from = c(Exercise, Type),
    values_from = Value,
    names_sep = " "
  ) %>%
  rename(Variable = BaseVar)

# Get all the current column names (excluding 'Variable')
col_order <- names(tb_wide)[-1]

# Separate and sort into cumulative and annual groups
cumulative_cols <- col_order[str_detect(col_order, "Cumulative")]
annual_cols     <- col_order[str_detect(col_order, "Annual")]

# Sort within each group to preserve year + Total/Per pairing
sort_within_group <- function(cols) {
  years <- str_extract(cols, "ARR\\d{2}")
  types <- str_extract(cols, "Total|Per")
  tibble(col = cols, year = years, type = types) %>%
    arrange(year, type) %>%
    pull(col)
}

# Final ordered column list
final_order <- c("Variable",
                 sort_within_group(cumulative_cols),
                 sort_within_group(annual_cols))

# Apply reordered columns to tb_wide
tb_wide <- tb_wide %>% select(all_of(final_order))
tb_wide

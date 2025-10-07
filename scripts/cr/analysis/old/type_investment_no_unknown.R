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
  group_by(Exercise, Typeofinvestment) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  mutate(Attribute = "Total")

# gender
gender <- df %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Exercise, Gender, Typeofinvestment) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  rename(Attribute = Gender)

# type of beneficiary
bens <- df %>%
  filter(Typeofbeneficiary != "Total" & Typeofbeneficiary != "Unknown" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Exercise, Typeofbeneficiary, Typeofinvestment) %>%
  summarise(Total = sum(Number, na.rm = TRUE))%>%
  rename(Attribute = Typeofbeneficiary)
bens

# levelofeducation
edu_level <- df %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation != "Total" & Levelofeducation != "Unknown level of education" &
           Gender != "Total") %>%
  group_by(Exercise, Levelofeducation, Typeofinvestment) %>%
  summarise(Total = sum(Number, na.rm = TRUE)) %>%
  rename(Attribute = Levelofeducation)
edu_level

# typeofeducation
edu_type <- df %>%
  filter(Typeofbeneficiary == "Total" &
           Levelofeducation == "Total" &
           Gender != "Total") %>%
  group_by(Exercise, Typeofeducation, Typeofinvestment) %>%
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
    `PerFemale` = Female / `Total`,
    `PerMale` = Male / `Total`,
    `PerRefugee` = Refugee / `TotalTypeofBens`,
    `PerIDP` = IDP / `TotalTypeofBens`,
    `PerOAPs` = OAPs / `TotalTypeofBens`,
    `PerPre-primary` = `Pre-primary` / `TotalLevelofEducation`,
    `PerPrimary` = Primary / `TotalLevelofEducation`,
    `PerSecondary` = Secondary / `TotalLevelofEducation`,
    `PerChildren with disabilities` = `Children with disabilities` / `TotalTypeofBens`,
    `PerFormal education` = `Formal education` / `Total`,
    `PerNon-formal education` = `Non-formal education` / `Total`
  ) %>%
  select(-c(TotalLevelofEducation, TotalTypeofBens))

# Step 3: Pivot longer
tb_long <- tb %>%
  pivot_longer(cols = -c(Exercise, Typeofinvestment), names_to = "Variable", values_to = "Value") %>%
  mutate(
    Type = if_else(str_starts(Variable, "Per"), "Per", "Total"),
    BaseVar = if_else(str_starts(Variable, "Per"), str_remove(Variable, "^Per"), Variable)
  )

# Step 4: Pivot wider, keeping TypeofInvestment in rows
tb_wide <- tb_long %>%
  pivot_wider(
    id_cols = c(BaseVar, Typeofinvestment),
    names_from = c(Exercise, Type),
    values_from = Value,
    names_sep = " "
  ) %>%
  rename(Variable = BaseVar)

# Step 5: Reorder columns (Cumulative before Annual, Total before Per)
col_order <- names(tb_wide)[-c(1,2)]  # skip Variable, TypeofInvestment

# Split into cumulative/annual and sort
sort_cols <- function(cols) {
  tibble(col = cols) %>%
    mutate(
      Year = str_extract(col, "ARR\\d{2}"),
      Period = if_else(str_detect(col, "Cumulative"), "Cumulative", "Annual"),
      Type = str_extract(col, "Total|Per")
    ) %>%
    arrange(Period, Year, Type) %>%
    pull(col)
}

final_cols <- c("Variable", "Typeofinvestment", sort_cols(col_order))
tb_wide <- tb_wide %>% select(all_of(final_cols))
tb_wide

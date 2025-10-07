# Prep env ----
## NOT TO USE ATM AS INCLUDED -----
source("scripts/prep_env.R")

# PR ----
## Load ---
df <- read.csv("data/cleaned/PR Combiner - For Checks.csv", encoding = "UTF-8")
list_codes <- read_excel(paste0("data/input/analysis_codes/", codes))

list_codes %<>%
  mutate(codes = tolower(trimws(codes)))

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
         
df %<>%
  filter(code %in% list_codes$codes)

# Save ----
# saving csv (consider adding timestamp)
write.csv(df, "data/cleaned/PR Combiner - For Analysis.csv", row.names = FALSE)

rm(list = ls())

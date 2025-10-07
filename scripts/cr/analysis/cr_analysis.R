# CR FLOW - Analysis ----
# Tables are created by functions 
# They are repeated per Reached and Targeted specifying the df and reached/ targeted 

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/analysis/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
wb <- createWorkbook()

df <- read.csv("data/cleaned/CR Combiner - For Analysis.csv", encoding = "UTF-8")

total_reached <- total_table(df, "Reached")
investments_reached <- type_investment_table(df, "Reached")
total_no_unknown_reached <- total_no_unknown_table(df, "Reached")
investments_no_uknown_reached <- type_investment_no_unknown_table(df, "Reached")
programme_reached <- programme_table(df, "Reached")
country_reached <- country_table(df, "Reached")
country_investments_reached <- country_investment_table(df, "Reached")

total_targeted <- total_table(df, "Targeted")
investments_targeted <- type_investment_table(df, "Targeted")
total_no_unknown_targeted <- total_no_unknown_table(df, "Targeted")
investments_no_uknown_targeted <- type_investment_no_unknown_table(df, "Targeted")
programme_targeted <- programme_table(df, "Targeted")
country_targeted <- country_table(df, "Targeted")
country_investments_targeted <- country_investment_table(df, "Targeted")

## Output annual ----
# List of checks and their corresponding sheet names
sheets <- list(
  "Reached_Totals" = total_reached,
  "Reached_Investments" = investments_reached,
  "Reached_Totals_NoUknown" = total_no_unknown_reached,
  "Reached_Investments_NoUnknown" = investments_no_uknown_reached,
  "Reached_Programme" = programme_reached,
  "Reached_Country" = country_reached,
  "Reached_Country_Investments" = country_investments_reached,
  "Targeted_Totals" = total_targeted,
  "Targeted_Investments" = investments_targeted,
  "Targeted_Totals_NoUknown" = total_no_unknown_targeted,
  "Targeted_Investments_NoUnknown" = investments_no_uknown_targeted,
  "Targeted_Programme" = programme_targeted,
  "Targeted_Country" = country_targeted,
  "Targeted_Country_Investments" = country_investments_targeted
)

# Loop through each check object and add to the workbook
for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
}

# Save the workbook
saveWorkbook(wb, paste0(folder_path, "Children Reached.xlsx"), overwrite = TRUE)

rm(list = ls())
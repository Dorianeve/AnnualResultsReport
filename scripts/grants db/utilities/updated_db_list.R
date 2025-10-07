source("scripts/prep_env.R")
today <- today()
grants <- read.csv("data/lists/2025-03-21 grants_db_updated.csv", encoding = "UTF-8")

grants %<>%
  mutate(Startdate = as.Date(Startdate, format = "%d-%b-%y"),
         Enddate = as.Date(Enddate, format = "%d-%b-%y"),
         CurrentEndDate = as.Date(CurrentEndDate, format = "%d-%b-%y" ))

grants %<>%
  mutate(
    Startdate = as.Date(Startdate),
    Enddate = as.Date(Enddate),
    CurrentEndDate = as.Date(CurrentEndDate),
    Activein2023 = ifelse(Startdate <= as.Date("2023-12-31") & Enddate >= as.Date("2023-01-01"), "Yes", "No"),
    Activein2024 = ifelse(Startdate <= as.Date("2024-12-31") & Enddate >= as.Date("2024-01-01"), "Yes", "No"),
    ActiveinStrategy = ifelse(CurrentEndDate >= as.Date("2023-01-01"), "Yes", "No")  # Adjust as needed
  )

write.csv(grants, paste0("data/lists/grants_db_updated.csv"),
                              row.names = FALSE)

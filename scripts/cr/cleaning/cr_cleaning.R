# CR FLOW - Cleaning ----

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/cr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

# Load cr ----
df <- read.xlsx(paste0("data/input/", combiner_cr),
                detectDates = TRUE, sep.names = " ")

# Cleaning ----
## Column names ----
colnames(df) <- gsub("\\s+", "", colnames(df))

## NAs - Typos ----
variables <- c("Typeofinvestment", "Country", "Granteeorganization", "GMGRN", "LeadGRN", 
               "Typeofeducation", "Indicator", "Levelofeducation", "Typeofbeneficiary", 
               "Status", "Gender", "Commentsandcontext", "Typeofreporting", "Filename", 
               "Sourceofdata", "Covid19relateddata", "GRID", "Exercise")

check_variables <- c("Typeofinvestment", "Country", "Granteeorganization", "GMGRN", "LeadGRN", 
                     "Typeofeducation", "Levelofeducation", "Typeofbeneficiary", 
                     "Status", "Gender", "Typeofreporting", 
                     "Sourceofdata", "Covid19relateddata")

### Trimws and NA ----
# Step 1: Iterate over each variable in the vector `variables`
for (v in variables) {
  # Step 2: Convert the column to character if it's not already
  if (!is.character(df[[v]])) {
    df[[v]] <- as.character(df[[v]])
  }
  # Step 3: Replace unwanted values with NA
  replacement_values <- c("-", "n/a", "NA", 
                          "N/A", 
                          "", "                                             -  ")
  df[[v]] <- ifelse(trimws(df[[v]]) %in% replacement_values, NA, df[[v]])
}

## Bens conservative cleaning ----
df <- df %>%
  mutate(Typeofbeneficiary = ifelse(substr(Typeofbeneficiary, 1, 7) == "Type of", 
                                    "Unknown", Typeofbeneficiary)) %>%
  mutate(Typeofbeneficiary = ifelse(substr(Typeofbeneficiary, 1, 7) == "Other a", 
                                    "OAPs", Typeofbeneficiary)) %>%
  mutate(Typeofbeneficiary = ifelse(substr(Typeofbeneficiary, 1, 5) == "TOTAL", 
                                    "Total", Typeofbeneficiary))

# rename some columns
df <- df %>%
  rename(Grantee = Granteeorganization,
         Covid19related = `Covid19relateddata`)

## Date variable ----
df <- df %>%
  mutate(date = "yes") %>%  # Equivalent to gen date="yes"
  mutate(date = ifelse(is.na(Currentenddate) | is.na(Startdate), "Missing", date))  # Equivalent to replace date="missing" if Currentenddate==.
# Attempt to parse with flexible formats
df$Startdate <- parse_date_time(df$Startdate, orders = c("ymd", "dmy", "mdy"))
df$Currentenddate <- parse_date_time(df$Currentenddate, orders = c("ymd", "dmy", "mdy"))

# Now extract year
df$yearstart <- year(df$Startdate)
df$yearend <- year(df$Currentenddate)

## Exercise variable ----
df <- df %>%
  mutate(exercice = case_when(
    grepl("20 cumulative", Exercise, ignore.case = TRUE) ~ 1,  # If 'Sourceofdata' contains '20'
    grepl("21 annual", Exercise, ignore.case = TRUE) ~ 2,
    grepl("21 cumulative", Exercise, ignore.case = TRUE) ~ 3,
    grepl("22 annual", Exercise, ignore.case = TRUE) ~ 4,
    grepl("22 cumulative", Exercise, ignore.case = TRUE) ~ 5,
    grepl("23 annual", Exercise, ignore.case = TRUE) ~ 6,
    grepl("23 cumulative", Exercise, ignore.case = TRUE) ~ 7,
    grepl("24 annual", Exercise, ignore.case = TRUE) ~ 8,
    grepl("24 cumulative", Exercise, ignore.case = TRUE) ~ 9,
    grepl("25 annual", Exercise, ignore.case = TRUE) ~ 10,
    grepl("25 cumulative", Exercise, ignore.case = TRUE) ~ 11,
    grepl("26 annual", Exercise, ignore.case = TRUE) ~ 12,
    grepl("26 cumulative", Exercise, ignore.case = TRUE) ~ 13,
    grepl("27 annual", Exercise, ignore.case = TRUE) ~ 14,
    grepl("27 cumulative", Exercise, ignore.case = TRUE) ~ 15,
    TRUE ~ NA                   # Otherwise, keep the original value
  ))


## Source of data ----
print("Clean 'Sourceofdata'")
df <- df %>%
  mutate(Sourceofdata = case_when(
    grepl("20", Sourceofdata) ~ "CR20",  # If 'Sourceofdata' contains '20'
    grepl("21", Sourceofdata) ~ "CR21",  # If 'Sourceofdata' contains '21'
    grepl("22", Sourceofdata) ~ "CR22",  # If 'Sourceofdata' contains '20'
    grepl("23", Sourceofdata) ~ "CR23",
    grepl("24", Sourceofdata) ~ "CR24",  
    grepl("25", Sourceofdata) ~ "CR25",  
    grepl("26", Sourceofdata) ~ "CR26",  
    grepl("27", Sourceofdata) ~ "CR27",
    TRUE ~ Sourceofdata                   # Otherwise, keep the original value
  ))

### Cleaning log print ----

sink(paste0(folder_path, "cleaning log.txt"))
for (v in check_variables) {
  print(table(df[[v]]), useNA = "always")
}
print(paste0("Numeric columns: "))
print(sapply(df, is.numeric))

print("Clean 'date' frequency table")
print(table(df$yearstart, df$yearend))

print("Exercise encoding frequency table")
print(table(df$Exercise, df$exercice))

print("Exercise vs sourceofdata frequency table")
table(df$Exercise, df$Sourceofdata)

print("Typeofinvestment vs sourceofdata frequency table")
table(df$Typeofinvestment, df$Sourceofdata)

sink()


# Save ----
write.csv(df, "data/cleaned/CR Combiner - For Checks.csv", row.names = FALSE)
write.csv(df, "data/cleaned/CR Combiner - For Analysis.csv", row.names = FALSE)


# Save NoUnknown version ----
df %<>%
  filter(Typeofbeneficiary != "Unknown")

write.csv(df, "data/cleaned/CR Combiner - For Analysis_NoUnknown.csv", row.names = FALSE)

rm(df, variables, v, check_variables, replacement_values)
rm(list = ls())


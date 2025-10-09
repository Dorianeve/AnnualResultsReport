# PR FLOW - Cleaning ----

# Prep env ----
source("scripts/prep_env.R")
today <- today()
folder_path <- paste0("data/output/pr/", Sys.Date(), "/")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

# CLEANING PR ----
# Load pr ----
df <- read.xlsx(paste0("data/input/", combiner_pr),
                detectDates = TRUE, sep.names = " ")

# Cleaning ----
## Column names ----
colnames(df) <- gsub("\\s+", "", colnames(df))

# Renaming columns
df <- df %>%
  rename(Grantee = Granteeorganization,
         indicatoradmin = IndicatorAdmin,
         code = AnalysisCode,
         unit = UnitOfMeasurement)

## Remove "No data" from "Program specific indicators"
df %<>%
  mutate(ProgramSpecificIndicator = 
           ifelse(ProgramSpecificIndicator == "No data", "",
                  ProgramSpecificIndicator))

## Trimws and NAs over all columns ----
# Step 1: Iterate over each column in the data frame
for (v in names(df)) {
  # Step 2: Convert the column to character if it's not already
  if (!is.character(df[[v]])) {
    df[[v]] <- as.character(df[[v]])
  }
  # Step 3: Replace unwanted values with NA
  replacement_values <- c("-", "n/a", "NA", "N/A", "", "                                             -  ")
  df[[v]] <- ifelse(trimws(df[[v]]) %in% replacement_values, NA, df[[v]])
}

## Encode exercise ----
df %<>%
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

## Encode Covid19 ----
df$c19 <- ifelse(df$Covid19related == "Covid-19 related data", 1, 0)
df$c19 <- factor(df$c19, labels = c("No", "Yes"))

## Clean Total column ----
# Remove leading and trailing whitespaces
df %<>%
  mutate(Total = Value) %>%
  # Step 1: Remove leading and trailing whitespaces
  mutate(Total = trimws(Total)) %>%
  # Step 2: Replace specific patterns with empty strings
  mutate(Total = ifelse(Total %in% c("NA", "na", "N A", "N/A", "n/a"), "", Total)) %>%
  # Step 3: Remove spaces and replace commas with dots
  mutate(Total = gsub(" ", "", Total),
         Total = gsub(",", ".", Total)) %>%
  # Step 4: Remove rows with certain values in 'Total'
  filter(!(Total %in% c("", "0", "-", "\u00a0"))) %>%
  # Step 5: Convert 'Total' to numeric
  mutate(Total = as.numeric(Total))


## Sourceofdata cleaning ----
df <- df %>%
  rename(Sourceofdata = SourceOfData) %>%
  mutate(Sourceofdata = case_when(
    str_detect(Sourceofdata, "20") ~ "PR20",  # Replace when '20' is anywhere in the string
    str_detect(Sourceofdata, "21") ~ "PR21",  # Replace when '21' is anywhere in the string
    str_detect(Sourceofdata, "22") ~ "PR22",  # Replace when '22' is anywhere in the string
    str_detect(Sourceofdata, "23") ~ "PR23",  # Replace when '23' is anywhere in the string
    str_detect(Sourceofdata, "24") ~ "PR24",  # Replace when '24' is anywhere in the string
    str_detect(Sourceofdata, "25") ~ "PR25",  # Replace when '25' is anywhere in the string
    TRUE ~ Sourceofdata                       # Otherwise, keep the original value
  ))

## Codes ----
df <- df %>%
  mutate(code = trimws(code))

df <- df %>%
  mutate(code  = case_when(
    code  == "Enter code" | 
      grepl("enter_code", code, ignore.case = TRUE) |
      grepl("no", code , ignore.case = TRUE) | 
      grepl("do_not_count", code , ignore.case = TRUE) |
      grepl("outlier", code, ignore.case = TRUE) | 
      grepl("duplicate", code, ignore.case = TRUE) |
      grepl("error", code, ignore.case = TRUE) ~ NA,
    TRUE ~ code
  ))

df <- df %>%
  mutate(code = tolower(code),
         code = if_else(grepl("e46_drr", code, ignore.case = TRUE), 
                        "e46_drr", code))

df <- df %>%
  mutate(code = if_else(grepl("n2cwd", code), 
                        "n2_cwd", code))

df <- df %>%
  mutate(code = if_else(grepl("t6c", code), 
                        "t6_c", code))

df <- df %>%
  mutate(code = if_else(grepl("e46_learning", code, ignore.case = TRUE),
                        "e46_learning", code))

df <- df %>%
  mutate(code = ifelse(str_detect(code, "_\\d+$"),
                       str_extract(code, "^[^_]+"),
                       code))


df <- df %>%
  mutate(
    unit = gsub("\\s+", " ", unit),         # Replace all types of whitespace with a single space
    unit = trimws(unit),                   # Trim leading and trailing spaces
    unit = case_when(
      grepl("%", unit, ignore.case = TRUE) ~ "Percentage %",
      grepl("Number|#", unit, ignore.case = TRUE) ~ "Number #",
      grepl("N/A", unit, ignore.case = TRUE) ~ NA_character_,
      grepl("US|\\$", unit, ignore.case = TRUE) ~ "US$",
      grepl("yes|no", unit, ignore.case = TRUE) ~ "Yes/No",
      unit == "" ~ NA_character_,          # Check for completely empty strings
      TRUE ~ unit
    )
  )

# Patch of old units
units <- read.csv("data/input/patch/patch_units.csv", encoding = "UTF-8")

units %<>%
  rename(revised_unit = Unit) %>% select(LeadGRN, code, Exercise, revised_unit)

df %<>%
  left_join(units, by = c("LeadGRN", "code", "Exercise"))

df %<>%
  mutate(unit = ifelse(is.na(unit), revised_unit, unit))

## Input units for SI ----
# second unit is to handle NA
df %<>% 
  mutate(unit = if_else(source == "SI", "Number #", unit, unit))

## n/p unit2 ----
df <- df %>%
  mutate(unit2 = if_else(unit != "", 
                         "n", "")) %>%  # Generate 'unit2' as "n" if 'unit' is not empty
  mutate(unit2 = if_else(unit == "Percentage %" | unit == "%", 
                         "p", unit2)) 

## Date ----
df$Startdate <- as.Date(df$Startdate)
df$yearend <- year(df$CurrentEndDate)
df$yearstart <- year(df$Startdate)

df$monthstart <- month(df$Startdate)
df$monthend <- month(df$CurrentEndDate)
df$daystart <- day(df$Startdate)
df$dayend <- day(df$CurrentEndDate)

## Active in year ----
# Generate new variable with value "yes"
df <- df %>%
  mutate(!!paste0("Activein", report_year) := "yes")

# Replace values with "no" based on conditions
df <- df %>%
  mutate(!!paste0("Activein", report_year) := 
           if_else(yearstart > report_year_number | 
                  yearend < report_year_number, "no", "yes"))

# Replace values with empty string where yearstart or yearend is missing
df <- df %>%
  mutate(!!paste0("Activein", report_year) := if_else(is.na(yearstart) | 
                                                        is.na(yearend), 
                                                      NA, !!sym(paste0("Activein", report_year))))

## Indicator ----
df <- df %>%
  mutate(Indicator = if_else(Indicator == "Enter a grantee selected 
                             Program Specific indicator --->", NA, Indicator))


## Reporting window ----
df <- df %>%
  rename(Reportingwindow = ReportingWindow) %>%
  mutate(Reportingwindow = case_when(
    grepl("1", Reportingwindow, ignore.case = TRUE) ~ "RW1",
    grepl("2", Reportingwindow, ignore.case = TRUE) ~ "RW2",
    grepl("3", Reportingwindow, ignore.case = TRUE) ~ "RW3",
    grepl("4", Reportingwindow, ignore.case = TRUE) ~ "RW4",
    grepl("5", Reportingwindow, ignore.case = TRUE) ~ "RW5",
    grepl("6", Reportingwindow, ignore.case = TRUE) ~ "RW6",
    grepl("7", Reportingwindow, ignore.case = TRUE) ~ "RW7",
    grepl("8", Reportingwindow, ignore.case = TRUE) ~ "RW8",
    grepl("9", Reportingwindow, ignore.case = TRUE) ~ "RW9",
    TRUE ~ Reportingwindow
  ))


## Concatenate for analysis code check
df <- df %>%
  mutate(concat = paste0(unit, "___", IndicatorFromLibrary))

### Cleaning log print ----

sink(paste0(folder_path, "cleaning log.txt"))
for (v in names(df)) {
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
# saving csv (consider adding timestamp)
write.csv(df, "data/cleaned/PR Combiner - For Checks.csv", row.names = FALSE)

rm(list = ls())

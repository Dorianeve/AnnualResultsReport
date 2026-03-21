# =============================================================================
# INGEST DATA FROM AIRTABLE
# Pulls all records from the configured Airtable base/table into a data frame.
# =============================================================================

source("scripts/prep_env.R")

# --- Load config -------------------------------------------------------------
source("config.yml")
token     <- AIRTABLE_TOKEN_ARR
base_id   <- BASE_ID_ARR
table_name <- TABLE_NAME_ARR

# --- Fetch all records (handles pagination) ----------------------------------
fetch_airtable <- function(token, base_id, table_name, sleep = 0.25) {

  stopifnot(nchar(base_id) > 0, nchar(token) > 0)

  url <- paste0(
    "https://api.airtable.com/v0/", base_id, "/",
    URLencode(table_name, reserved = TRUE)
  )

  headers <- httr::add_headers(
    Authorization  = paste("Bearer", token),
    `Content-Type` = "application/json"
  )

  all_records <- list()
  offset <- NULL

  repeat {
    query <- if (!is.null(offset)) list(offset = offset) else list()
    resp  <- httr::GET(url, headers, query = query)

    if (httr::status_code(resp) >= 400)
      stop("Airtable GET failed (HTTP ", httr::status_code(resp), "): ",
           httr::content(resp, as = "text", encoding = "UTF-8"))

    parsed  <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )

    all_records <- c(all_records, parsed$records)
    message("Fetched ", length(all_records), " records so far...")

    offset <- parsed$offset
    if (is.null(offset)) break
    Sys.sleep(sleep)
  }

  message("Done. Total records: ", length(all_records))
  all_records
}

# --- Convert list of records to a data frame ---------------------------------
records_to_df <- function(records) {
  if (length(records) == 0) {
    message("No records found.")
    return(data.frame())
  }

  rows <- lapply(records, function(rec) {
    fields <- rec$fields
    fields[["airtable_id"]] <- rec$id
    fields[["created_time"]] <- rec$createdTime
    as.data.frame(fields, stringsAsFactors = FALSE, check.names = FALSE)
  })

  # Bind rows tolerantly (fills missing columns with NA)
  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) {
    missing <- setdiff(all_cols, names(r))
    r[missing] <- NA
    r[all_cols]
  })

  do.call(rbind, rows)
}

# --- Run ---------------------------------------------------------------------
message("Connecting to Airtable base: ", base_id, " / table: ", table_name)

records <- fetch_airtable(token, base_id, table_name)
df      <- records_to_df(records)

message("Data ingested: ", nrow(df), " rows x ", ncol(df), " columns")
print(head(df))


df %<>% select(FLOW, ProgrammeID_text, LeadGRN, GMGRN, Issue, Addressed, Ignore, Justification,
               Responsible_text, LastModified)

df %<>%
  rename(
    ProgrammeID = ProgrammeID_text,
    Responsible = Responsible_text)

cr <- df %>%
  filter(FLOW == "CR" & Ignore == TRUE) %>%
  select(-c(FLOW, Ignore))

pr <- df %>%
  filter(FLOW == "PR" & Ignore == TRUE) %>%
  select(-c(FLOW, Ignore))

# --- Export to Excel ---------------------------------------------------------
out_path <- file.path("data/input/log_decisions/log_decisions.xlsx")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "cr")
openxlsx::addWorksheet(wb, "pr")
openxlsx::addWorksheet(wb, "log")
openxlsx::writeData(wb, sheet = "cr", x = cr)
openxlsx::writeData(wb, sheet = "pr", x = pr)
openxlsx::writeData(wb, sheet = "log", x = df)
openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

message("Saved: ", out_path)


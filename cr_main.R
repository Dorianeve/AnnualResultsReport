# ANNUAL RESULTS REPORT ----
## CR FLOW ----

### Prep environment ----
source("scripts/prep_env.R")
### Cleaning ----
source("scripts/cr/cleaning/cr_cleaning.R")
source("scripts/mne/mne_cleaning.R")
### Checks ----
source("scripts/airtable/ingest_from_airtable.R")
source("scripts/cr/checks/cr_data_checks.R")
source("scripts/cr/checks/cr_data_checks_new.R")
source("scripts/cr/checks/cr_numbers_checks_reached.R")
source("scripts/cr/checks/cr_numbers_checks_targeted.R")
source("scripts/airtable/load_checks.R")
### Analysis ----
source("scripts/cr/analysis/cr_analysis.R")


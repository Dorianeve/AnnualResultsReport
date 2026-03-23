# ANNUAL RESULTS REPORT ----
## PR FLOW ----

# If checks are ongoing the whole process should be re-run before new checks iterations.


### Prep environment ----
source("scripts/prep_env.R")
### Cleaning ----
source("scripts/pr/cleaning/pr_cleaning.R")
# source("scripts/mne/mne_cleaning.R")
### Checks ----
source("scripts/airtable/ingest_from_airtable.R")
source("scripts/pr/checks/pr_data_checks.R")
source("scripts/pr/checks/pr_number_checks.R")
source("scripts/airtable/load_checks.R")
### Create the subset for analysis ----
source("scripts/pr/cleaning/pr_patch.R")

### Analysis ----
source("scripts/pr/analysis/pr_analysis_annual.R")
source("scripts/pr/analysis/pr_analysis_annual_active_in_year.R")
source("scripts/pr/analysis/pr_analysis_cumulative.R")
source("scripts/pr/analysis/pr_analysis_cumulative_strategic_plan.R")

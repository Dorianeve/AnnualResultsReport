# Annual Results Report Workflow

## Environmental prep

Before running the scripts:

-   Create the folder `data/input` with 3 sub-folders

    -   `analysis_codes` (where to put the repository of analysis codes list)

    -   `log_decisions` (folder with pending use as a log of decisions for each year)

    -   `patch` (folder with patch related to focus on specific `LeadGRN` or `Analysis Code` - patch is optional)

-   Put the `combiner` file along with `MnE reports approval` files in the main `data/input` folder

-   Open the `config.yml` file and complete with the requested information. It is important to be accurate here as the code refers to these files and parameters when run. These parameters change over reporting years.

## Running the code

The code can be run as a standalone or in bits. The suggestion is to have the first iteration run in steps to control and check especially during the cleaning phase, as there might be some additional cleaning steps to be added due to specific challenges in the data. After the first iteration it is recommended to run the entire flow utilising `cr_main.R` and `pr_main.R` files.

### The flow

The flow for both CR and PR follow this logic:

-   cleaning

-   data checks (controls over data inconsistencies and gaps)

-   number checks (cross-checks over numbers imputed)

In case of PR patches, run the patch before the checks.

Every workflow, CR or PR, have all the additional scripts into `scripts/` folder, divided into `cr/`, `pr/`, and `mne/`.

## Outputs

All the iterations produce clean datasets into the folders `data/cleaned/` where the different components of the workflows are saved as `.csv` files. The cleaned files are overwritten on every iterations, aiming for the last iteration to have the cleanest possible version of the dataset.

Every iteration also produces checks and analysis outputs into the folders `data/output/cr` or `data/output/pr`, organized by folders with dates name in `YYYY-MM-DD` format. Every folder contains:

-   Data and number checks in the main folder

-   Cleaning log `txt` in the main folder

-   Analysis outcome into the subfolder names `analysis`

Again outputs are organized per flow, either CR or PR.

The `excel` files with outputs tend to follow the logic of having a `Summary` sheet with the summary of checks, a `Cube` sheet providing information on every check outcome per either `LeadGRN` and `GMGRN` (for CR checks) and per `LeadGRN` and `Analysis Code` (for PR checks). All the other sheets contain information on the specific datapoints outputting errors.

### Log of changes

Every iterative check is submitted to ECW staff for action on the original datapoint, that it's updated periodically and re-run. If there are specific decisions to be taken on specific datapoints, these are to be logged into the `log_decisions` folder. In a TBD file.

# 2025 to-do-list

-   [ ] Log decisions implementation
-   [ ] PR figures checks resume
-   [ ] PR flow control and refinement
-   [ ] AoE pipeline
-   [ ] Refine analysis outcome

# Annual Results Report Workflow

## Environmental prep

Before running the scripts:

-   Put the `combiner` files along with `MnE reports approval` file and `grants_db.csv` (output of the gms) in the main `data/input` folder

-   Open the `config.yml` file and complete with the requested information. It is important to be accurate here as the code refers to these files and parameters when run. These parameters change over reporting years.

## Running the code

The code can be run as a standalone or in bits. The suggestion is to have the first iteration run in steps to control and check especially during the cleaning phase, as there might be some additional cleaning steps to be added due to specific challenges in the data. After the first iteration it is recommended to run the entire flow utilizing `cr_main.R` and `pr_main.R` files.

### The flow

The flow for both CR and PR follow this logic:

-   cleaning

-   data checks (controls over data inconsistencies and gaps)

-   number checks (cross-checks over numbers imputed)

-   analysis

Every workflow, CR or PR, have all the additional scripts into `scripts/` folder, divided into `cr/` and `pr/`.

## Outputs

All the iterations produce clean datasets into the folders `data/cleaned/` where the different components of the workflows are saved as `.csv` files. The cleaned files are overwritten on every iterations, aiming for the last iteration to have the cleanest possible version of the dataset.

Every iteration also produces checks and analysis outputs into the folders `data/output/cr` or `data/output/pr`, organized by folders with dates name in `YYYY-MM-DD` format. Every folder contains:

-   Data and number checks in the main folder

-   Cleaning log `txt` in the main folder

-   Analysis outcome into the subfolder names `analysis`

Again outputs are organized per flow, either CR or PR.

The `excel` files with outputs tend to follow the logic of having a `Summary` sheet with the summary of checks, a `Cube` sheet providing information on every check outcome per either `LeadGRN` and `GMGRN` (for CR checks) and per `LeadGRN` and `Analysis Code` (for PR checks). All the other sheets contain information on the specific datapoints outputting errors.

### Log of changes

Every iterative check is submitted to ECW staff for action on the original datapoint, that it's updated periodically and re-run. If there are specific decisions to be taken on specific datapoints, these are to be logged into the `log_decisions` folder. The file has two sheets for either `cr` and `pr`. The log acts on the checks that are present in the `cube` sheets. The columns request the essential information to filter out the mistakes that are spotted but to be ignored. In the column `issue` is to be provided the column name of the issue spotted, copy pasted. If it is misspelled the filtering will not work.

### Priority GRNs and codes

This file controls the priority `codes` and `LeadGRN` to be included in the flow. In the `LeadGRN` sheet utilize the column `'Lead grantee GRN'` without changing the name. In the `Codes` sheet only utilize the `Priority` column with `boolean` (0 for No and 1 for Yes) values. The `Level` column specifies the type of analysis code (Children, Caregivers, Teachers, Schools). The analysis is run exclusively on priority `LeadGRNs` and priority `codes`.

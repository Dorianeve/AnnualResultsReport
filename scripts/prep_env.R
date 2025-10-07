# clean the environment
rm(list = ls())
setwd(getwd())
# load custom functions in functions folder
file_list <- list.files(path = "functions", pattern = "\\.R$", full.names = TRUE)
for (file in file_list) {
  source(file)
}

file_list_analysis <- list.files(path = "functions/analysis_cr", pattern = "\\.R$", full.names = TRUE)
for (file in file_list_analysis) {
  source(file)
}
# load necessary libraries
source("requirements/libraries.R")
# loading environmental variables with set parameters
source("config.yml")

rm(file, file_list)

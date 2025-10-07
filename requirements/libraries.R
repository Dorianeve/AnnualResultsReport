# libraries required for the ARR pipeline

if(!require(openxlsx)){
  install.packages('openxlsx')
  library(openxlsx)
}

if(!require(Hmisc)){
  install.packages('Hmisc')
  library(Hmisc)
}

if(!require(tidyverse)){
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(stringdist)){
  install.packages('stringdist')
  library(stringdist)
}

if(!require(foreach)){
  install.packages('foreach')
  library(foreach)
}

if(!require(janitor)) {
  install.packages("janitor")
  library(janitor)
}

if(!require(ggplot2))  {
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(scales))  {
  install.packages("scales")
  library(scales)
}

if(!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

if(!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

if(!require(purrr)) {
  install.packages("purrr")
  library(purrr)
}

if(!require(yaml)) {
  install.packages("yaml")
  library(yaml)
}

if(!require(ggplot2)) {
  install.packages(("ggplot2"))
}

if(!require(magrittr)) {
  install.packages(("magrittr"))
}

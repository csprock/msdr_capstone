## code to prepare `DATASET` dataset goes here
library(readr)
library(stringr)

raw_noaa_sample <- readr::read_delim("data-raw/earthquakes.tsv", delim="\t") %>%
  filter(str_detect(`Location Name`, "MEXICO:") | str_detect(`Location Name`, "CALIFORNIA:"))


usethis::use_data(raw_noaa_sample, overwrite = TRUE)

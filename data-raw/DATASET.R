## code to prepare `DATASET` dataset goes here
library(readr)
library(stringr)
library(magrittr)

raw_noaa_sample <- readr::read_delim("data-raw/earthquakes.tsv", delim="\t") %>%
  filter(str_detect(`Location Name`, "MEXICO:") | str_detect(`Location Name`, "CALIFORNIA:"))

test_data_1 <- raw_noaa_sample[100, ]

usethis::use_data(raw_noaa_sample, overwrite = FALSE)
usethis::use_data(test_data_1, overwrite=TRUE)

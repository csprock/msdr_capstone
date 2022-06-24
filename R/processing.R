
parse_date <- function(data) {
  data %>%
    dplyr::mutate(
      Mo = na_if(Mo, 1),
      Dy = na_if(Dy, 1),
      date = lubridate::make_date(
        year=Year,
        month=Mo,
        day=Dy
      )
    ) %>%
    select(-c("Year", "Mo", "Dy"))
}


clean_name <- function(str) {
  str_split_fixed(str, ":", n=2) %>%
    .[, 2] %>%
    str_trim() %>%
    str_to_title()
}

country_name <- function(str) {
  str_split_fixed(str, ":", n=2) %>%
    .[, 1] %>%
    str_trim() %>%
    str_to_title()
}



eq_location_clean <- function(data) {
  data %>%
    mutate(
      `Country Name` = country_name(`Location Name`),
      `Country Name` = if_else(`Country Name` == "", NA_character_, `Country Name`),
      `Location Name` = clean_name(`Location Name`),
      `Location Name` = if_else(`Location Name` == "", NA_character_, `Location Name`),
    )
}


convert_lat_lon <- function(data) {
  data %>%
    mutate(
      Latitude=as.numeric(Latitude),
      Longitude=as.numeric(Longitude)
    )
}


eq_clean_data <- function(data) {
  data %>%
    parse_date() %>%
    eq_location_clean() %>%
    convert_lat_lon()
}




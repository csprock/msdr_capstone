
parse_date <- function(data) {
  data %>%
    dplyr::mutate(
      Mo = dplyr::na_if(Mo, 1),
      Dy = dplyr::na_if(Dy, 1),
      date = lubridate::make_date(
        year=Year,
        month=Mo,
        day=Dy
      )
    ) %>%
    dplyr::select(-c("Year", "Mo", "Dy"))
}


clean_name <- function(str) {
  stringr::str_split_fixed(str, ":", n=2) %>%
    .[, 2] %>%
    stringr::str_trim() %>%
    stringr::str_to_title()
}

country_name <- function(str) {
  stringr::str_split_fixed(str, ":", n=2) %>%
    .[, 1] %>%
    stringr::str_trim() %>%
    stringr::str_to_title()
}



eq_location_clean <- function(data) {
  data %>%
    dplyr::mutate(
      `Country Name` = country_name(`Location Name`),
      `Country Name` = dplyr::if_else(`Country Name` == "", NA_character_, `Country Name`),
      `Location Name` = clean_name(`Location Name`),
      `Location Name` = dplyr::if_else(`Location Name` == "", NA_character_, `Location Name`),
    )
}


#' Convert coordinates to numerics
#'
#' Convert latitude and longitudes to numerics and rename them for compatibility with leaflet.
#'
#' @importFrom magrittr %>%
convert_lat_lon <- function(data) {
  data %>%
    dplyr::mutate(
      Latitude=as.numeric(Latitude),
      Longitude=as.numeric(Longitude)
    ) %>%
    dplyr::rename(
      lat=Latitude,
      lng=Longitude
    )
}


#' Clean NOAA Earthquake Data
#'
#' Cleans columns of the NOAA earthquake data
#'
#' @param data a dataframe or tibble containing raw NOAA data
#'
#' @importFrom magrittr %>%
#' @export
eq_clean_data <- function(data) {
  data %>%
    parse_date() %>%
    eq_location_clean() %>%
    convert_lat_lon()
}




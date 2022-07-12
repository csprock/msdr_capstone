# processing.R
#
# contains functions for processing the raw NOAA data

#' Format NOAA dates
#'
#' Convert year, month and day columns into a single datetime column.
#'
#' @seealso [eq_clean_data()]
#'
#' @param data dataframe containing raw NOAA earthquake data
#'
#' @return a dataframe with a "date" column added and "Year", "Mo" and "Dy" columns removed.
#'
#' @importFrom magrittr %>%
parse_date <- function(data) {
  data %>%
    dplyr::mutate(
      Mo = dplyr::na_if(.data[["Mo"]], 1),
      Dy = dplyr::na_if(.data[["Dy"]], 1),
      date = lubridate::make_date(
        year=.data[["Year"]],
        month=.data[["Mo"]],
        day=.data[["Dy"]]
      )
    ) %>%
    dplyr::select(-c("Year", "Mo", "Dy"))
}


#' Parse location names
#'
#' Parse and clean country and location names
#'
#' This family of functions parses the "Location Name" column of the NOAA earthquake dataset
#' and separates the location from the country, creating two columns containing each. These functions
#' also trim whitespace and convert the names to title case.
#'
#' @param str string containing the raw value from the "Location Name" column
#'
#' @seealso [eq_location_clean()]
#'
#' @importFrom magrittr %>%
#' @name clean_location_name
NULL

#' @rdname clean_location_name
clean_location <- function(str) {
  tmp <- stringr::str_split_fixed(str, ":", n=2)[, 2] %>%
    stringr::str_split_fixed(",", n=2)
  tmp[,1] %>%
    stringr::str_trim() %>%
    stringr::str_to_title()
}

#' @rdname clean_location_name
country_name <- function(str) {
  stringr::str_split_fixed(str, ":", n=2)[, 1] %>%
    stringr::str_trim() %>%
    stringr::str_to_title()
}


#' Create location columns
#'
#' Splits location name column into separate country and location columns.
#'
#' This function splits the "Location Name" into a "Country Name" column containing
#' the country and replaces the contents of "Location Name" with just the location.
#'
#' @param data dataframe containing raw NOAA earthquake data
#'
#' @seealso [eq_clean_data()], [country_name()], [clean_location()]
#'
#' @return dataframe with an additional "Country Name" column.
#' @importFrom magrittr %>%
eq_location_clean <- function(data) {
  data %>%
    dplyr::mutate(
      `Country Name` = country_name(.data[["Location Name"]]),
      `Location Name` = clean_location(.data[["Location Name"]])
    ) %>%
    dplyr::mutate(
      `Country Name` = dplyr::if_else(.data[["Country Name"]] == "", NA_character_, .data[["Country Name"]]),
      `Location Name` = dplyr::if_else(.data[["Location Name"]] == "", NA_character_, .data[["Location Name"]])
    )
}


#' Convert coordinates to numerics
#'
#' Convert latitude and longitudes to numerics and renames them for compatibility with leaflet.
#'
#' @param df dataframe containing raw NOAA data
#' @seealso [eq_clean_data()]
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
convert_lat_lon <- function(df) {
  df %>%
    dplyr::mutate(
      Latitude=as.numeric(.data[["Latitude"]]),
      Longitude=as.numeric(.data[["Longitude"]])
    ) %>%
    dplyr::rename(
      lat=.data[["Latitude"]],
      lng=.data[["Longitude"]]
    )
}


#' Clean NOAA Earthquake Data
#'
#' Prepares the NOAA earthquake data for use by downstream functions in this library.
#'
#' This function cleans the columns of the [NOAA earthquake data](https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search)
#' for further use with this library's plotting functions. This function performs the following:
#' \itemize{
#' \item consolidates the month, day and year columns into a single datetime column
#' \item separates the country and location into two separate columns, trims whitespace and converts them to titlecase
#' \item converts latitude and longitude into numerics and renames these columns for compatibility with Leaflet
#' }
#'
#' @param df a dataframe or tibble containing raw NOAA data
#'
#' @return a cleaned dataframe
#'
#' @examples
#' \dontrun{
#'  data <- eq_clean_data(raw_data)
#' }
#'
#' @importFrom magrittr %>%
#' @export
eq_clean_data <- function(df) {
  df %>%
    parse_date() %>%
    eq_location_clean() %>%
    convert_lat_lon()
}




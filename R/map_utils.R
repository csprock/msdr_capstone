# map_utils.R
#
# Contains utility functions for generating the leaflet map.

#' Create HTML labels
#'
#' Create HTML labels for earthquake leaflet map using fields from NOAA data
#'
#' Given a location, magnitude and number of deaths, this function returns a formatted
#' HTML string for use with Leaflet. If any of the values are NA, that value will not
#' be included in the label.
#'
#' @param location string containing the name of earthquake location
#' @param mag numeric with the earthquake magnitude
#' @param deaths numeric with the number of deaths
#'
#' @return string containing the HTML label
#'
#' @family Mapping
html_label <- function(location, mag, deaths) {

  row_templates <- c(
    ifelse(is.na(location), NA, stringr::str_replace("<b>Location:</b> REP", "REP", location)),
    ifelse(is.na(mag), NA, stringr::str_replace("<b>Magnitude:</b> REP", "REP", as.character(mag))),
    ifelse(is.na(deaths), NA, stringr::str_replace("<b>Total deaths:</b> REP", "REP", as.character(deaths)))
  )

  return(stringr::str_c(Filter(Negate(is.na), row_templates), collapse="<br>") )
}


#' Create an HTML label column
#'
#' Creates a new column from a dataframe containing NOAA earthquake data
#'
#' The function expects that the following named columns are present:
#' \itemize{
#'  \item Location Name
#'  \item Mag
#'  \item Deaths
#' }
#' The values of these columns are used to create a new column containing
#' the HTML labels that is returned
#'
#' @param df dataframe containing NOAA earthquake data
#'
#' @return a character vector containing the labels
#'
#' @family Mapping
#'
#' @examples
#' \dontrun{
#' df %>%
#'  mutate(labels = eq_create_label(.))
#' }
#'
#' @export
eq_create_label <- function(df) {
  df %>% dplyr::rowwise() %>%
    dplyr::mutate(temp = html_label(.data[["Location Name"]], .data[["Mag"]], .data[["Deaths"]])) %>%
    dplyr::pull("temp")
}

#' Create a map of earthquake locations
#'
#' Create a leaflet map showing the locations of earthquakes
#'
#' @param df dataframe containing NOAA earthquake data
#' @param annot_col optional, a column label to use for annotating the earthquake markers
#'
#' @return a leaflet map object
#'
#' @family Mapping
#'
#' @examples
#' \dontrun{
#'   m <- df %>%
#'    mutate(label = eq_create_labels(.)) %>%
#'    eq_map(annot_col="label")
#'   m
#' }
#' @importFrom stats as.formula
#' @import leaflet
#' @export
eq_map <- function(df, annot_col=NULL) {

  df <- df %>%
    dplyr::mutate(Mag= 2500*.data[["Mag"]])  # scale up magnitude for better viewing on the map

  if (is.null(annot_col)) {
    m <- df %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircles(radius=~Mag) # make circles proprtional to magnitude
  } else {

    f <- as.formula(gsub("PATTERN", annot_col, "~`PATTERN`"))

    df <- df %>%
      dplyr::mutate_at(dplyr::vars(annot_col), as.character)

    m <- df %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircles(radius=~Mag, popup=f)
  }

  return(m)
}

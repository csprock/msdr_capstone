html_label <- function(location, mag, deaths) {

  row_templates <- c(
    ifelse(is.na(location), NA, stringr::str_replace("<b>Location:</b> REP", "REP", location)),
    ifelse(is.na(mag), NA, stringr::str_replace("<b>Magnitude:</b> REP", "REP", as.character(mag))),
    ifelse(is.na(deaths), NA, stringr::str_replace("<b>Total deaths:</b> REP", "REP", as.character(deaths)))
  )

  return(stringr::str_c(Filter(Negate(is.na), row_templates), collapse="<br>") )
}


eq_create_label <- function(df) {
  df %>% dplyr::rowwise() %>%
    dplyr::mutate(temp = html_label(`Location Name`, Mag, Deaths)) %>%
    dplyr::pull(temp)
}


eq_map <- function(df, annot_col=NULL) {

  df <- df %>%
    dplyr::mutate(Mag = 2500*Mag)

  if (is.null(annot_col)) {
    m <- df %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircles(radius=~Mag)
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

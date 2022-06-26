html_label <- function(location, mag, deaths) {

  row_templates <- c(
    ifelse(is.na(location), NA, str_replace("<b>Location:</b> REP", "REP", location)),
    ifelse(is.na(mag), NA, str_replace("<b>Magnitude:</b> REP", "REP", as.character(mag))),
    ifelse(is.na(deaths), NA, str_replace("<b>Total deaths:</b> REP", "REP", as.character(deaths)))
  )

  return(str_c(Filter(Negate(is.na), row_templates), collapse="<br>") )
}


eq_create_label <- function(df) {
  df %>% rowwise() %>%
    mutate(temp = html_label(`Location Name`, Mag, Deaths)) %>%
    pull(temp)
}


eq_map <- function(df, annot_col=NULL) {

  df <- df %>%
    mutate(Mag = 2500*Mag)

  if (is.null(annot_col)) {
    m <- df %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(radius=~Mag)
  } else {

    f <- as.formula(gsub("PATTERN", annot_col, "~`PATTERN`"))

    df <- df %>%
      mutate_at(vars(annot_col), as.character)

    m <- df %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(radius=~Mag, popup=f)
  }

  return(m)
}

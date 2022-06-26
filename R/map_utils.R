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


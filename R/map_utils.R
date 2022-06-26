html_label <- function(location, mag, deaths) {

  deaths <- as.character(if_else(is.na(deaths), 0, deaths))
  mag <- as.character(mag)

  row_templates <- c(
    "<b>Location:</b> LOCATION",
    "<b>Magnitude:</b> MAG",
    "<b>Total deaths:</b> DEATHS"
  )

  row_templates %>%
    str_c(collapse="<br>") %>%
    str_replace_all(c("LOCATION"=location, "MAG"=mag, "DEATHS"=deaths))

}


eq_create_label <- function(df) {
  df %>% rowwise() %>%
    mutate(temp = html_label(`Location Name`, Mag, Deaths)) %>%
    pull(temp)
}


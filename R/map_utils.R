# create html label function

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



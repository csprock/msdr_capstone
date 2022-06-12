
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

eq_location_clean <- function(data) {
  data %>%
    mutate(
      `Location Name` = clean_name(`Location Name`),
      `Location Name` = if_else(`Location Name` == "", NA_character_, `Location Name`)
    ) 
}
  
  


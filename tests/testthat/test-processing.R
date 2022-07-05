
test_that("String cleaning functions work", {

  test_string <- "CALIFORNIA: SAN JOSE"
  test_df <- dplyr::tibble(`Location Name` = test_string)

  expect_equal(clean_location(test_string), "San Jose")
  expect_equal(country_name(test_string), "California")

  expect_equal(
    eq_location_clean(test_df),
    dplyr::tibble(
      `Location Name` = "San Jose",
      `Country Name` = "California"
    )
    )

})

test_that("Date formatting works", {


  data("test_data_1", package="MSDRCapstone", envir=environment())
  on.exit(rm(test_data_1, envir=environment()), add=TRUE)

  result <- parse_date(test_data_1)

  expect_s3_class(result, "tbl_df")
  expect_true("date" %in% colnames(result))
  expect_setequal(c("Mo", "Dy", "Year") %in% colnames(result), c("FALSE"))
  expect_equal(result$date[1], lubridate::ymd("1875-02-11"))

})


test_that("location formatting works", {

  data("test_data_1", package="MSDRCapstone", envir=environment())
  on.exit(rm(test_data_1, envir=environment()), add=TRUE)

  result <- convert_lat_lon(test_data_1)
  expect_setequal(c("lat", "lng") %in% colnames(result), c("TRUE"))

})



test_that("Test that geom_timeline works", {

  data("test_data_1", package="MSDRCapstone", envir=environment())
  on.exit(rm(test_data_1, envir=environment()), add=TRUE)

  p <- raw_noaa_sample %>%
    dplyr::mutate(y=stringr::str_detect(`Location Name`, "MEXICO")) %>%
    ggplot2::ggplot(aes(x=Year, y=y, size=Mag, fill=Deaths)) +
    geom_timeline()

  expect_s3_class(p, "ggplot")

})

test_that("geom_label_timeline works", {
  data("test_data_1", package="MSDRCapstone", envir=environment())
  on.exit(rm(test_data_1, envir=environment()), add=TRUE)

  p <- raw_noaa_sample %>%
    dplyr::mutate(y=stringr::str_detect(`Location Name`, "MEXICO")) %>%
    ggplot2::ggplot(aes(x=Year, y=y, size=Mag, fill=Deaths)) +
    geom_timeline() +
    geom_label_timeline(aes(label=`Location Name`), n_max=3)

  expect_s3_class(p, "ggplot")
})

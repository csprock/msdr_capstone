

test_that("that eq_create_label and html_label work", {


  data("test_data_1", package="MSDRCapstone", envir=environment())
  on.exit(rm(test_data_1, envir=environment()), add=TRUE)

  expect_equal(eq_create_label(test_data_1), "<b>Location:</b> MEXICO:  SAN CRISTOBAL DE LA BARRANCA<br><b>Magnitude:</b> 7.5<br><b>Total deaths:</b> 25")
  expect_equal(html_label(test_data_1$`Location Name`, test_data_1$Mag, test_data_1$Deaths), "<b>Location:</b> MEXICO:  SAN CRISTOBAL DE LA BARRANCA<br><b>Magnitude:</b> 7.5<br><b>Total deaths:</b> 25")

})

test_that("that eq_map returns correct type", {


  data("test_data_1", package="MSDRCapstone", envir=environment())
  on.exit(rm(test_data_1, envir=environment()), add=TRUE)

  m <- eq_map(test_data_1)

  expect_s3_class(m, "leaflet")
})



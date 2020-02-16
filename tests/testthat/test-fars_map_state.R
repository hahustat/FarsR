context("Testing the fars_map_state function")

test_that("fars_map_state works", {
  setwd(system.file("extdata", package = "farsR"))

  expect_null(fars_map_state(21, 2013))
  expect_error(fars_map_state(15, 2014), "nothing to draw: all regions out of bounds")
  expect_error(fars_map_state(14, 2013), "invalid STATE number: 14")
})

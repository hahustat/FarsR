context("Test the fars_summarize_years function")


test_that("single year data is summarized successfully", {
  fars_summary_2014 <- tibble::tribble(
    ~MONTH, ~`2014`,
    1,   2168,
    2,   1893,
    3,   2245,
    4,   2308,
    5,   2596,
    6,   2583,
    7,   2696,
    8,   2800,
    9,   2618,
    10,  2831,
    11,  2714,
    12,  2604

    )

  setwd(system.file("extdata", package = "Fars"))


  fars_summary_2014 <- purrr::map_df(fars_summary_2014, as.integer)
  fars_summary_2014$MONTH <- as.numeric(fars_summary_2014$MONTH)

  expect_equal(dim(fars_summarize_years(2015)), c(12, 2))
  expect_equal(fars_summarize_years(2014), fars_summary_2014)
})


test_that("mulitple years", {
  setwd(system.file("extdata", package = "Fars"))

  fars_summary_2013_to_15 <- tibble::tribble(
    ~MONTH, ~`2013`, ~`2014`, ~`2015`,
    7,   2660,   2696,   2998,
    8,   2899,   2800,   3016,
    9,   2741,   2618,   2865,
    10,   2768,   2831,   3019,
    11,   2615,   2714,   2724,
    12,   2457,   2604,   2781,
  )
  fars_summary_2013_to_15 <- purrr::map_df(fars_summary_2013_to_15, as.integer)
  fars_summary_2013_to_15$MONTH <- as.numeric(fars_summary_2013_to_15$MONTH)


  expect_equal(dim(fars_summarize_years(c(2013, 2014, 2015))), c(12, 4))
  expect_equal(tail(fars_summarize_years(c(2013, 2014, 2015))),
               fars_summary_2013_to_15)
})

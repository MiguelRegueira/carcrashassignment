library(testthat)
library(carcrashassignment)

test_that("Make file names", {
  filename <- make_filename(2014)
  expect_is(filename, "character")
  expect_equal(filename, "accident_2014.csv.bz2")
  expect_equal(filename, make_filename("2014"))
  expect_warning(make_filename("2014f"))
})

test_that("Load accident file data", {
  filename <- file.path(system.file("extdata/", package = "carcrashassignment"), make_filename(2014))
  data <- fars_read(filename)
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 30056)
  expect_equal(ncol(data), 50)
  expect_error(fars_read("unknown file"))
})

test_that("Summarize car accident data", {
  setwd(system.file("extdata/", package = "carcrashassignment"))
  data <- fars_summarize_years(years = c(2014, 2015))
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 12)
  expect_equal(ncol(data), 3)
  expect_equal(data$`2014`[1], 2168)
  expect_error(fars_summarize_years(years = c("2a015")))
})

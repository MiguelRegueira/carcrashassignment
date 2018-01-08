library(testthat)
library(carcrashassignment)

test_that("Make file names", {
  filename <- make_filename(2014)
  expect_that(filename, is_a("character"))
})

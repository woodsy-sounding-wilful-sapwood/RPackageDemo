context("Test the make_filename function of the FARS package")

library(fars)

test_that("Test the correct filename is generated", {
  expect_that(make_filename("2013"),
            equals("accident_2013.csv.bz2"))
})


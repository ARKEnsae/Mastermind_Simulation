
context("MasterMind - hideCode")

source('../../R/server.R')

test_that("hideCode exists", {
  hideCode <- mmServer('hideCode')
  expect_failure(expect_null(hideCode))

})

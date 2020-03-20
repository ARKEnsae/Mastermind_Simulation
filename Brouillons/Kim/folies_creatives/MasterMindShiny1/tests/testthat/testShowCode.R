
context("MasterMind - showCode")

source('../../R/server.R')

test_that("showCode numOfPicks=1", {
  showCode <- mmServer('showCode')
  expect_failure(expect_null(showCode))

})

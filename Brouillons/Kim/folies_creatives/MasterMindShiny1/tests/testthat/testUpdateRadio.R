
context("MasterMind - updateRadio")

source('../../R/server.R')

test_that("updateRadio - ", {
  updateRadio <- mmServer('updateRadio')
  expectedId <- 'xxx'
  expectedLabel <- 'cell1'
  expectedValue <- 'red'
  session = new.env(hash = TRUE)

  sendInputMessageCalled = FALSE

  session$sendInputMessage <- function(id, message){
    sendInputMessageCalled <<- TRUE
    expect_equal(id, expectedId)
    expect_equal(message$label, expectedLabel)
    expect_equal(message$value, expectedValue)
  }

  updateRadio(session, expectedId, expectedLabel, expectedValue)

  expect_equal(sendInputMessageCalled, TRUE)
})

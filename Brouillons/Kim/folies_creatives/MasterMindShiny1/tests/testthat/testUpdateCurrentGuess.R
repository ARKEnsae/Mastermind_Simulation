
context("MasterMind - updateCurrentGuess")

source('../../R/server.R')

test_that("updateCurrentGuess - numOfPicks=1", {
  numOfPicks <- 1
  updateCurrentGuess <- mmServer('updateCurrentGuess')
  output = list()
  input = list(radiocell1 = 'red')

  result <- updateCurrentGuess(input, output, numOfPicks)
  expect_equal(result$guesscell1(), "<p style='color: red ;'>O</p>")
  expect_equal(result$guesscell2(), '')
  expect_equal(result$guesscell3(), '')
  expect_equal(result$guesscell4(), '')
})

test_that("updateCurrentGuess - numOfPicks=2", {
  numOfPicks <- 2
  updateCurrentGuess <- mmServer('updateCurrentGuess')
  output = list()
  input = list(radiocell1 = 'orange', radiocell2 = 'blue')

  result <- updateCurrentGuess(input, output, numOfPicks)
  expect_equal(result$guesscell1(), "<p style='color: orange ;'>O</p>")
  expect_equal(result$guesscell2(), "<p style='color: blue ;'>O</p>")
  expect_equal(result$guesscell3(), '')
  expect_equal(result$guesscell4(), '')
})

test_that("updateCurrentGuess - numOfPicks=3", {
  numOfPicks <- 3
  updateCurrentGuess <- mmServer('updateCurrentGuess')
  output = list()
  input = list(radiocell1 = 'orange', radiocell2 = 'blue', radiocell3 = 'orange')

  result <- updateCurrentGuess(input, output, numOfPicks)
  expect_equal(result$guesscell1(), "<p style='color: orange ;'>O</p>")
  expect_equal(result$guesscell2(), "<p style='color: blue ;'>O</p>")
  expect_equal(result$guesscell3(), "<p style='color: orange ;'>O</p>")
  expect_equal(result$guesscell4(), '')
})

test_that("updateCurrentGuess - numOfPicks=4", {
  numOfPicks <- 4
  updateCurrentGuess <- mmServer('updateCurrentGuess')
  output = list()
  input = list(radiocell1 = 'orange', radiocell2 = 'blue', radiocell3 = 'orange', radiocell4 = 'purple')

  result <- updateCurrentGuess(input, output, numOfPicks)
  expect_equal(result$guesscell1(), "<p style='color: orange ;'>O</p>")
  expect_equal(result$guesscell2(), "<p style='color: blue ;'>O</p>")
  expect_equal(result$guesscell3(), "<p style='color: orange ;'>O</p>")
  expect_equal(result$guesscell4(), "<p style='color: purple ;'>O</p>")
})


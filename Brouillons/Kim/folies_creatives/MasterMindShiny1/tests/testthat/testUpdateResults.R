
context("MasterMind - updateResults")

source('../../R/server.R')

test_that("updateResults - numOfPicks=1", {
  numOfPicks <- 1
  updateResults <- mmServer('updateResults')

  result <- updateResults(c('black'), numOfPicks, 'black', 'red', 'red', 'black')
  expect_equal(result, c('black', '', 'black'))

  result <-
    updateResults(c('red'), numOfPicks, 'black', 'red', 'red', 'black')
  expect_equal(result, c('black', '', ''))
})

test_that("updateResults - numOfPicks=2", {
  numOfPicks <- 2
  updateResults <- mmServer('updateResults')

  result <-
    updateResults(c('black', 'red'),
                  numOfPicks,
                  'black',
                  'black',
                  'red',
                  'black')
  expect_equal(result, c('black', 'black', '', 'black', ''))

  result <-
    updateResults(c('black', 'red'), numOfPicks, 'red', 'black', 'red', 'black')
  expect_equal(result, c('red', 'black', '', 'white', 'white'))

  result <-
    updateResults(c('black', 'red'), numOfPicks, 'black', 'red', 'red', 'black')
  expect_equal(result, c('black', 'red', '', 'black', 'black'))

  result <-
    updateResults(c('black', 'black'),
                  numOfPicks,
                  'black',
                  'red',
                  'red',
                  'black')
  expect_equal(result, c('black', 'red', '', 'black', ''))

  result <-
    updateResults(c('black', 'black'),
                  numOfPicks,
                  'red',
                  'black',
                  'red',
                  'black')
  expect_equal(result, c('red', 'black', '', 'black', ''))

  result <-
    updateResults(c('black', 'black'),
                  numOfPicks,
                  'black',
                  'black',
                  'red',
                  'black')
  expect_equal(result, c('black', 'black', '', 'black', 'black'))

  result <-
    updateResults(c('black', 'black'), numOfPicks, 'red', 'red', 'red', 'black')
  expect_equal(result, c('red', 'red', '', '', ''))
})

test_that("updateResults - numOfPicks=3",
          {
            numOfPicks <- 3
            updateResults <- mmServer('updateResults')

            result <-
              updateResults(c('black', 'red', 'orange'),
                            numOfPicks,
                            'black',
                            'black',
                            'black',
                            'black')
            expect_equal(result, c('black', 'black', 'black', '', 'black', '', ''))

            result <-
              updateResults(c('black', 'red', 'orange'),
                            numOfPicks,
                            'orange',
                            'orange',
                            'orange',
                            'black')
            expect_equal(result, c('orange', 'orange', 'orange', '', 'black', '', ''))

            result <-
              updateResults(c('black', 'red', 'orange'),
                            numOfPicks,
                            'black',
                            'blue',
                            'red',
                            'black')
            expect_equal(result, c('black', 'blue', 'red', '', 'black', 'white', ''))

            result <-
              updateResults(c('black', 'red', 'orange'),
                            numOfPicks,
                            'black',
                            'orange',
                            'red',
                            'black')
            expect_equal(result,
                         c('black', 'orange', 'red', '', 'black', 'white', 'white'))

            result <-
              updateResults(c('black', 'red', 'orange'),
                            numOfPicks,
                            'red',
                            'orange',
                            'black',
                            'black')
            expect_equal(result,
                         c('red', 'orange', 'black', '', 'white', 'white', 'white'))

            result <-
              updateResults(c('black', 'red', 'orange'),
                            numOfPicks,
                            'black',
                            'red',
                            'orange',
                            'black')
            expect_equal(result,
                         c('black', 'red', 'orange', '', 'black', 'black', 'black'))

            result <-
              updateResults(c('black', 'orange', 'orange'),
                            numOfPicks,
                            'red',
                            'red',
                            'red',
                            'black')
            expect_equal(result, c('red', 'red', 'red', '', '', '', ''))

            result <-
              updateResults(c('black', 'orange', 'orange'),
                            numOfPicks,
                            'red',
                            'red',
                            'orange',
                            'black')
            expect_equal(result, c('red', 'red', 'orange', '', 'black', '', ''))

            result <-
              updateResults(c('black', 'orange', 'orange'),
                            numOfPicks,
                            'orange',
                            'red',
                            'orange',
                            'black')
            expect_equal(result, c('orange', 'red', 'orange', '', 'black', 'white', ''))

            result <-
              updateResults(c('black', 'orange', 'orange'),
                            numOfPicks,
                            'orange',
                            'black',
                            'orange',
                            'black')
            expect_equal(result,
                         c('orange', 'black', 'orange', '', 'black', 'white', 'white'))

            result <-
              updateResults(c('black', 'orange', 'orange'),
                            numOfPicks,
                            'black',
                            'orange',
                            'orange',
                            'black')
            expect_equal(result,
                         c('black', 'orange', 'orange', '', 'black', 'black', 'black'))
          })

test_that("updateResults - numOfPicks=4", {
  numOfPicks <- 4
  updateResults <- mmServer('updateResults')

  result <-
    updateResults(c('black', 'red', 'orange', 'blue'),
                  numOfPicks,
                  'white',
                  'purple',
                  'pink',
                  'brown')
  expect_equal(result, c('white', 'purple', 'pink', 'brown', '', '', '', '', ''))

  result <-
    updateResults(c('black', 'red', 'orange', 'blue'),
                  numOfPicks,
                  'black',
                  'black',
                  'black',
                  'black')
  expect_equal(result,
               c('black', 'black', 'black', 'black', '', 'black', '', '', ''))

  result <-
    updateResults(c('black', 'red', 'orange', 'blue'),
                  numOfPicks,
                  'black',
                  'black',
                  'blue',
                  'blue')
  expect_equal(result,
               c('black', 'black', 'blue', 'blue', '', 'black', 'black', '', ''))

  result <-
    updateResults(c('black', 'red', 'orange', 'blue'),
                  numOfPicks,
                  'red',
                  'black',
                  'blue',
                  'orange')
  expect_equal(result,
               c(
                 'red',
                 'black',
                 'blue',
                 'orange',
                 '',
                 'white',
                 'white',
                 'white',
                 'white'
               ))

  result <-
    updateResults(c('black', 'red', 'brown', 'blue'),
                  numOfPicks,
                  'red',
                  'black',
                  'pink',
                  'brown')
  expect_equal(result,
               c(
                 'red',
                 'black',
                 'pink',
                 'brown',
                 '',
                 'white',
                 'white',
                 'white',
                 ''
               ))

  result <-
    updateResults(c('black', 'red', 'brown', 'blue'),
                  numOfPicks,
                  'black',
                  'red',
                  'brown',
                  'blue')
  expect_equal(result,
               c(
                 'black',
                 'red',
                 'brown',
                 'blue',
                 '',
                 'black',
                 'black',
                 'black',
                 'black'
               ))
})

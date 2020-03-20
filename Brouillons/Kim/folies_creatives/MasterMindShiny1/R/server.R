library(shiny)
library(shinyjs)

library(xtable)
library(DT)
library(dplyr)

mmServer <- function(funct) {
  hideCode <- function(out) {
    js$clearCircle('codeCell1')
    js$clearCircle('codeCell2')
    js$clearCircle('codeCell3')
    js$clearCircle('codeCell4')
    out
  }

  generateResultTable <- function(numOfPicks, body){
    headers <- c()
    for (i in 1:numOfPicks){
      headers[[i]] <- tags$th(as.character(i))
    }
    tags$table(class = "table",
               tags$thead(
                 tags$tr(
                   tags$th("Guess", 'colspan' = numOfPicks, 'style' = 'text-align: center'),
                   tags$th("Result", 'colspan' = 4, 'style' = 'text-align: center')
                 ),
                 tags$tr(headers)
               ),
               tags$tbody(
                 body
               )
    )
  }
  setupRadio <-
    function (session,
              inputId,
              gameColors,
              shouldEnable) {
      if (shouldEnable) {
        enable(inputId)
        i <- 1
        for (col in gameColors) {
          id <- paste0(inputId, i)
          shinyjs::enable(id = id)
          js$drawCircle(id, col)
          i <- i + 1
        }
        if (i <= 6) {
          for (j in i:6) {
            id <- paste0(inputId, j)
            sel <- paste0("input[type=radio][name=", inputId, "][value=", j, "]")
            # Would prefer to use hide here but where this disables the individual radio buttons
            # hide will hide the whole radio group.
            shinyjs::disable(selector = sel)
          }
        }
      } else{
        disable(inputId)
      }
    }
  showCode <- function (out, code, numOfPicks) {
    js$drawCircle('codeCell1', code[1])
    if (numOfPicks > 1) {
      js$drawCircle('codeCell2', code[2])
    }
    if (numOfPicks > 2) {
      js$drawCircle('codeCell3', code[3])
    }
    if (numOfPicks > 3) {
      js$drawCircle('codeCell4', code[4])
    }
    out
  }
  updateCurrentGuess <-
    function(input, output, numOfPicks, gameColors) {
      output$guesscell1 <- renderText({
        print(
          paste(
            'numOfPicks',
            numOfPicks,
            'input$radiocell1',
            input$radiocell1,
            'gameColors[input$radiocell1]=',
            gameColors[as.numeric(input$radiocell1)]
          )
        )
        if (numOfPicks > 0) {
          js$drawCircle('guesscell1js', gameColors[as.numeric(input$radiocell1)])
          ''
        } else{
          ''
        }
        ''
      })
      output$guesscell2 <- renderText({
        if (numOfPicks > 1) {
          js$drawCircle('guesscell2js', gameColors[as.numeric(input$radiocell2)])
        } else{
          ''
        }
        ''
      })
      output$guesscell3 <- renderText({
        if (numOfPicks > 2) {
          js$drawCircle('guesscell3js', gameColors[as.numeric(input$radiocell3)])
          paste("<p style='color:", input$radiocell3, ";'>O</p>")
        } else{
          ''
        }
        ''
      })
      output$guesscell4 <- renderText({
        if (numOfPicks > 3) {
          js$drawCircle('guesscell4js', gameColors[as.numeric(input$radiocell4)])
          paste("<p style='color:", input$radiocell4, ";'>O</p>")
        } else{
          ''
        }
        ''
      })

      output
    }
  updateClient <-
    function (session,
              inputId,
              label = NULL,
              value = NULL) {
      message <- list(label = label, value = value)
      session$sendInputMessage(inputId, message)
    }
  updateResults <-
    function (code,
              numOfPicks,
              gameColors,
              guess1,
              guess2,
              guess3,
              guess4) {
      resultPos <- numOfPicks + 2
      guesses <- c(guess1,
                   guess2,
                   guess3,
                   guess4)

      myGuess <- c()
      for (i in 1:numOfPicks) {
        myGuess[i] <- gameColors[as.numeric(guesses[i])]
      }

      result <- myGuess[1:numOfPicks]
      result <- append(result, '')
      result <- c(result, c('')[1:numOfPicks])

      print(paste('code:', code))
      print(paste('myGuess:', myGuess))
      posFound <- c()
      posNotFound <- c()
      for (pos in 1:numOfPicks) {
        if (myGuess[pos] == code[pos]) {
          posFound <- append(posFound, pos)
          result[resultPos] <- 'black'
          resultPos <- resultPos + 1
        } else {
          posNotFound <- append(posNotFound, pos)
        }
      }
      for (posNF in posNotFound) {
        color = myGuess[posNF]
        for (pos2 in 1:numOfPicks) {
          if (match(pos2, posFound, nomatch = 0) == 0) {
            if (color == code[pos2]) {
              posFound <- append(posFound, pos2)
              result[resultPos] <- 'white'
              resultPos <- resultPos + 1
            }
          }
        }
      }
      if (length(result) >= resultPos) {
        for (pos in resultPos:length(result)) {
          result[pos] <- ''
        }
      }
      print(paste('result=', result))
      result
    }
  generateRowTags <- function(numOfPicks, currentRowIndex) {

    rows <- list()
    for (j in 1:currentRowIndex) {
      row <- list()
      for (i in 1:((numOfPicks * 2) + 1)) {
        id <- paste0('pick', j, i)
        cell <- tags$td(tags$canvas(id=id, width=20, height=20))
        row[[i]] <- cell
      }
      rows[[j]] <- tags$tr(row)
    }
    tags$tbody(rows)
  }

  serve <- function(input, output, session) {
    print('starting server')

    # global variables

    numOfColors <- 0
    numOfPicks <- 0
    gameColors <- NULL
    code <- NULL
    localBoard <- NULL
    currentRowIndex <- 1
    rowResults <- list()
    availableColors <-
      c('pink', 'aqua', 'green', 'orange', 'red', 'purple')

    # show pre-game tab first.
    output$mindState <- reactive({
      'preGame'
    })

    # buttons
    observeEvent(input$startNewGame, {
      hideCode(output)
      output$mindState <- renderText('preGame')
    })

    observeEvent(input$quitGame, {
      stopApp()
    })

    observeEvent(input$startGame, {
      print("Start Game:")
      numOfColors <<- as.numeric(input$numOfColors)
      numOfPicks <<- as.numeric(input$numOfPicks)
      print(paste("  numOfPicks", input$numOfPicks))
      print(paste("  numOfColors", input$numOfColors))

      output$resultTable <- renderUI(
        generateResultTable(numOfPicks, list())
      )
      rowResults <<- list()
      currentRowIndex <<- 1
      gameColors <<- availableColors[1:numOfColors]
      print(paste("  gameColors", gameColors))

      updateCurrentGuess(input, output, numOfPicks, gameColors)
      # set radio buttons based on number of colors
      setupRadio(session, radioId1, gameColors, numOfPicks > 0)
      setupRadio(session, radioId2, gameColors, numOfPicks > 1)
      setupRadio(session, radioId3, gameColors, numOfPicks > 2)
      setupRadio(session, radioId4, gameColors, numOfPicks > 3)
      shinyjs::enable("showCode")
      shinyjs::enable("showResults")

      code <<-
        sample(gameColors,
               numOfPicks,
               replace = length(gameColors) < numOfPicks)
      print(paste("  code=", code))
      output$mindState <- renderText('mindGame')
    })

    observeEvent(input$showCode, {
      showCode(output, code, numOfPicks)
    })

    observeEvent(input$showResults, {
      rowResult <-
        updateResults(
          code,
          numOfPicks,
          gameColors,
          input$radiocell1,
          input$radiocell2,
          input$radiocell3,
          input$radiocell4
        )

      output$resultTable <- renderUI(
        generateResultTable(numOfPicks, generateRowTags(numOfPicks, currentRowIndex))
      )

      rowResults[[currentRowIndex]] <<- rowResult
      if (rowResult[length(rowResult)] == 'black') {
        showCode(output, code, numOfPicks)
        shinyjs::disable("showCode")
        shinyjs::disable("showResults")
      }
      currentRowIndex <<- currentRowIndex + 1
      # The problem was finding a time to draw the pegs.  The board is not drawn yet.
      # So set a value on the client which causes a shiny event to happen.
      # By the time we catch it, the board is updated.
      updateClient(session, 'hiddenInput', label = 'hideLable', value = as.character(currentRowIndex))
    })

    # draw pegs after result table has been displayed
    observeEvent(input$hiddenInput, {
      if (length(rowResults) > 0) {
        for (j in 1:length(rowResults)) {
          rr <- rowResults[[j]]
          for (i in 1:length(rr)) {
            id <- paste0('pick', j, i)
            cellColor <- rr[i]
            if (cellColor != '') {
              js$drawCircle(id, cellColor)
            } else {
              js$clearCircle(id)
            }
          }
        }
      }
    })
  }

  # make some functions available for testing.
  if (funct == 'serve') {
    serve
  } else if (funct == 'setupRadio') {
    setupRadio
  } else if (funct == 'hideCode') {
    hideCode
  } else if (funct == 'showCode') {
    showCode
  } else if (funct == 'updateCurrentGuess') {
    updateCurrentGuess
  } else if (funct == 'updateClient') {
    # print(paste('funct=', funct))
    # print(showCode)
    updateClient
    # showCode
  } else if (funct == 'updateResults') {
    updateResults
  } else {
    paste('unknown funct:', funct)
  }
}

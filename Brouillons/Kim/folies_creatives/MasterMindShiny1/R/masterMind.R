library(shiny)
library(shinyjs)

library(xtable)
library(DT)
library(dplyr)

# if running the app, the default dir is R
# if running tests, the default is root dir.
if (file.exists('ui.R')) {
  source('ui.R')
  source('server.R')
} else {
  source('R/ui.R')
  source('R/server.R')
}

xxx <- mmServer('serve')
# print(xxx)

shinyApp(ui = mmUI(), server = xxx)

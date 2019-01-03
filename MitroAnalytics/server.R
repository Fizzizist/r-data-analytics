library(shiny)
source("uiControl.R")

server <- function(input, output, session){
  loadUI(input, output, session)
}
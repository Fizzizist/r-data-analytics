source("uiPlotControl.R")
source("uiFilterControl.R")

loadUI <- function(input, output, session){
  loadFlags <- reactiveValues(
    loadedStatHist = FALSE
  )
  observeEvent(input$sidebarMenu, {
      if(input$sidebarMenu != "statHistogram") return()
      if(loadFlags$loadedStatHist) return()
      loadFlags$loadedStatHist <- TRUE
      print("inside hist")
      
      sessions <- read.csv("data/sessions.csv") # To be replace with a call to DB or the Stat module
      
      renderHistSessionFilter(output, sessions)
      observeHistSelectSessionEvent(input, output, session)
    }
  )
}
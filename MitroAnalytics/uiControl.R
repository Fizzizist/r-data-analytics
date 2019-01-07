source("uiPlotControl.R")
source("uiFilterControl.R")
source("uiAuthControl.R")

loadUI <- function(input, output, session){
  uiReactValues <- reactiveValues(
    loadedStatHist = FALSE,
    loadedIntPlot = FALSE,
    authenticated = FALSE
  )
  
  displayLoginView(output, session)
  observeUserLogin(input, output, session, uiReactValues$authenticated)
  observeUserLogout(input, output, session, uiReactValues$authenticated)
  
  observeEvent(input$sidebarMenu, 
    {
      if(input$sidebarMenu != "statHistogram") return()
      if(uiReactValues$loadedStatHist) return()
      uiReactValues$loadedStatHist <- TRUE
      print("inside hist")
      
      sessions <- read.csv("data/sessions.csv") # To be replace with a call to DB or the Stat module
      
      renderHistSessionFilter(output, sessions)
      observeHistSelectSessionEvent(input, output, session)
    }
  )
  
  # 3. Add tab click event observer to prevent autoloading
  observeEvent(input$sidebarMenu, 
    {
      if(input$sidebarMenu != "statIntPlot") return()
      if(uiReactValues$loadedIntPlot) return()
      uiReactValues$loadedIntPlot <- TRUE
      print("inside intPlot")
      # Initialize samp.elems 
      if(!exists("samp.elem")) {
        samp.elem <- readRDS("data/samp.elem.rds") # To be replaced with a database call.
      }
      renderIntPlotElemFilter(output)
      drawInteractivePlot(input, output, samp.elem)
    }
  )
}
source("uiPlotControl.R")
source("uiFilterControl.R")
source("uiAuthControl.R")
source("io.R")
source("stats.R")

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
      
      sessions <- getSessionList()
      renderHistSessionFilter(output, sessions[["session_id"]])
      observeHistSelectSessionEvent(input, output, session)
    }
  )
  
  # 3. Add tab click event observer to prevent autoloading
  observeEvent(input$sidebarMenu, 
    {
      # Initialize samp.elems 
      if(!exists("samp.elem")) {
        samp.elem <- getPTValues() 
      if(input$sidebarMenu != "statIntPlot") return()
      if(uiReactValues$loadedIntPlot) return()
      uiReactValues$loadedIntPlot <- TRUE
      print("inside intPlot")
      }
      renderIntPlotElemFilter(output, names(samp.elem))
      drawInteractivePlot(input, output, samp.elem)
    }
  )
}
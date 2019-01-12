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
      
      burns <- getBurnList()
      renderHistBurnFilter(output, burns[["burn_id"]])
      observeHistSelectBurnEvent(input, output, session)
    }
  )
  
  observeEvent(input$sidebarMenu, 
    {
      if(input$sidebarMenu != "statIntPlot") return()
      if(uiReactValues$loadedIntPlot) return()
      uiReactValues$loadedIntPlot <- TRUE
      
      if(!exists("samp.elem")) {
        samp.elem <- getPTValues()
      }
      
      renderIntPlotElemFilter(output, names(samp.elem))
      observeIntPlotSelectElemEvent(input, output, session, samp.elem)
    }
  )
}
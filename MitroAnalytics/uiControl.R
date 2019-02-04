source("uiPlotControl.R")
source("uiFilterControl.R")
source("uiAuthControl.R")
source("io.R")
source("stats.R")

loadUI <- function(input, output, session){
  uiReactValues <- reactiveValues(
    loadedStatHist = FALSE,
    loadedIntPlot = FALSE,
    loadedPlotlyPlot = FALSE,
    loadIntRDS = FALSE,
    authenticated = FALSE
  )
  
  session$userData$username <- NULL
  
  # the following objects will be consolidated once the plotly save workspace implementation is finished. 
  session$userData$sampElem <- NULL
  session$userData$elemNames <- NULL
  session$userData$elemSelected <- NULL
  session$userData$sampElemPlotly <- NULL
  session$userData$elemSelectedPlotly <- NULL
  
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
      renderSelectInput(output, 'selectHistBurn', 'Select burn:', burns[["burn_id"]])
      observeHistSelectBurnEvent(input, output, session)
    }
  )
  
  observeEvent(input$sidebarMenu, 
    {
      if(input$sidebarMenu != "statIntPlot") return()
      if(uiReactValues$loadedIntPlot) return()
      uiReactValues$loadedIntPlot <- TRUE
      
      burns <- getBurnList()
      renderSelectInput(output, 'selectIntPlotBurn', 'Select burn:', burns[["burn_id"]])
      observeIntPlotSelectBurnEvent(input, output, session)
    }
  )

observeEvent(input$sidebarMenu, 
      {
        if(input$sidebarMenu != "statPlotly") return()
        if(uiReactValues$loadedPlotlyPlot) return()
        uiReactValues$loadedPlotlyPlot <- TRUE
        
        burns <- getBurnList()
        renderSelectInput(output, 'selectPlotlyPlotBurn', 'Select burn:', burns[["burn_id"]])
        observePlotlyPlotSelectBurnEvent(input, output, session)
      }
    )
}
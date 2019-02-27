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
  
  session$userData$sampElem <- NULL
  session$userData$elemNames <- NULL
  session$userData$elemSelected <- NULL
  
  displayLoginView(output, session)
  observeUserLogin(input, output, session, uiReactValues$authenticated)
  observeUserLogout(input, output, session, uiReactValues$authenticated)

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
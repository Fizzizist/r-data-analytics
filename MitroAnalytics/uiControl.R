source("uiPlotControl.R")
source("uiFilterControl.R")
source("uiAuthControl.R")
source("io.R")
source("stats.R")

loadUI <- function(input, output, session){
  uiReactValues <- reactiveValues(
    loadedStatHist = FALSE,
    loadedIntPlot = FALSE,
    loadedDataCleaning = FALSE,
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
          if(uiReactValues$loadedDataCleaning) return()
          uiReactValues$loadedDataCleaning <- TRUE
          print("uiControl.R - Load statPlotly")
          
          burns <- getBurnList()
          burnChoices <- c(c('%'),burns[['burn_id']])
          names(burnChoices) <- c(c('All'),burns[['burn_id']])
          renderSelectInput(output, 'selectDataCleaningBurn', 'Select burn:', burnChoices , '%')
          observeDataCleaningSelectBurnEvent(input, output, session)
        }
      )
  }
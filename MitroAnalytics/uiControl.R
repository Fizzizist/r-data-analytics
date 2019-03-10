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
    loadedDataExploring = FALSE,
    loadIntRDS = FALSE,
    authenticated = FALSE
  )
  
  session$userData$username <- NULL
  
  session$userData$sampElem <- NULL
  session$userData$elemSelected <- NULL
  session$userData$elemNames <- NULL
  
  
  session$userData$burnChoices <- NULL
  session$userData$elemChoices <- NULL
  
  # sampDataset object keeps track of any changes in selected burn, element or plotted dataset. 
  session$userData$sampDataset$selectedBurn <- NULL
  session$userData$sampDataset$selectedElement <- NULL
  session$userData$sampDataset$sampData <- NULL
  
  displayLoginView(output, session)
  observeUserLogin(input, output, session, uiReactValues$authenticated)
  observeUserLogout(input, output, session, uiReactValues$authenticated)

  observeEvent(input$sidebarMenu, 
        {
          if(input$sidebarMenu != "statDataCleaning") return()
          if(uiReactValues$loadedDataCleaning) return()
          uiReactValues$loadedDataCleaning <- TRUE
          print("uiControl.R - Load statDataCleaning")
          
          burns <- getBurnList()
          burnChoices <- c(c('%'),burns[['burn_id']])
          names(burnChoices) <- c(c('All'),burns[['burn_id']])
          
          # burnChoices keeps track of the available burns. This is used when loading the saved dataset.
          session$userData$burnChoices <- burnChoices
          
          # elemChoices keeps track of the available elements. This is used when loading the saved dataset.
          session$userData$elemChoices <- getElemChoices()
          
          renderSelectInput(output, 'selectDataCleaningBurn', 'Select burn:', burnChoices , '%')
          observeDataCleaningSelectBurnEvent(input, output, session)
          
          renderSelectInput(output, 'selectDataCleaningElement', "Select an element:", session$userData$elemChoices, session$userData$sampDataset$selectedElement)
          observeDataCleaningSelectElemEvent(input, output, session)
          
          observeDataCleaningBtnEvent(input, output, session)
        }
      )

  observeEvent(input$sidebarMenu, 
        {
          if(input$sidebarMenu != "statDataExploring") return()
          if(uiReactValues$loadedDataExploring) return()
          uiReactValues$loadedDataExploring <- TRUE
          print("uiControl.R - Load statDataExploring")
          
          burns <- getBurnList()
          burnChoices <- c(c('%'),burns[['burn_id']])
          names(burnChoices) <- c(c('All'),burns[['burn_id']])
          renderSelectInput(output, 'selectDataExploringBurn', 'Select burn:', burnChoices , '%')
          observeDataExploringSelectBurnEvent(input, output, session)
        }
      )
  }
source("uiPlotControl.R")
source("uiFilterControl.R")
source("uiAuthControl.R")
source("io.R")
source("stats.R")

loadUI <- function(input, output, session){
  uiReactValues <- reactiveValues(
    loadedStatHist = FALSE,
    loadedIntPlot = FALSE,
    loadedDataCleaner = FALSE,
    loadedDataExplorer = FALSE,
    loadedDataAnalyzer = FALSE,
    loadIntRDS = FALSE,
    authenticated = FALSE
  )
  
  session$userData$username <- NULL
  
  #session$userData$sampElem <- NULL
  #session$userData$elemSelected <- NULL
  #session$userData$elemNames <- NULL
  
  
  session$userData$burnChoices <- NULL
  session$userData$elemChoices <- NULL
  
  # sampDataset object keeps track of any changes in selected burn, element or plotted dataset. 
  # For now, the data is stored in different objects depending on hich tab it comes from, but this could be merged into one set of objects in the future.
  session$userData$sampDataset$selectedBurn <- NULL
  session$userData$sampDataset$selectedElement <- NULL
  session$userData$sampDataset$sampData <- NULL
  
  session$userData$sampDatasetExp$selectedBurn <- NULL
  session$userData$sampDatasetExp$selectedElement <- NULL
  session$userData$sampDatasetExp$sampData <- NULL

  session$userData$sampDatasetAlz$selectedBurn <- NULL
  session$userData$sampDatasetAlz$selectedElement <- NULL
  session$userData$sampDatasetAlz$sampData <- NULL
  
  displayLoginView(output, session)
  observeUserLogin(input, output, session, uiReactValues$authenticated)
  observeUserLogout(input, output, session, uiReactValues$authenticated)

  observeEvent(input$sidebarMenu, 
        {
          if(input$sidebarMenu != "statDataCleaner") return()
          if(uiReactValues$loadedDataCleaner) return()
          uiReactValues$loadedDataCleaner <- TRUE
          print("uiControl.R - Load statDataCleaner")
          
          burns <- getBurnList()
          burnChoices <- c(c('%'),burns[['burn_id']])
          names(burnChoices) <- c(c('All'),burns[['burn_id']])
          
          renderSelectInput(output, 'selectDataCleanerBurn', 'Select burn:', burnChoices , '%')
          observeDataCleanerSelectBurnEvent(input, output, session)
          
          renderSelectInput(output, 'selectDataCleanerElement', "Select an element:", getElemChoices(), session$userData$sampDataset$selectedElement)
          observeDataCleanerSelectElemEvent(input, output, session)
          
          observeDataCleanerBtnEvent(input, output, session)
        }
      )

  observeEvent(input$sidebarMenu, 
        {
          if(input$sidebarMenu != "statDataExplorer") return()
          if(uiReactValues$loadedDataExplorer) return()
          uiReactValues$loadedDataExplorer <- TRUE
          print("uiControl.R - Load statDataExplorer")
          
          burns <- getBurnList()
          burnChoices <- c(c('%'),burns[['burn_id']])
          names(burnChoices) <- c(c('All'),burns[['burn_id']])
          
          renderSelectInput(output, 'selectDataExplorerBurn', 'Select burn:', burnChoices , '%')
          observeDataExplorerSelectBurnEvent(input, output, session)
          
          renderSelectInput(output, 'selectDataExplorerElement', "Select an element:", getElemChoices(), session$userData$sampDatasetExp$selectedElement)
          observeDataExplorerSelectElemEvent(input, output, session)
          
          observeDataExplorerBtnEvent(input, output, session)
        }
      )

  observeEvent(input$sidebarMenu, 
        {
          if(input$sidebarMenu != "statDataAnalyzer") return()
          if(uiReactValues$loadedDataAnalyzer) return()
          uiReactValues$loadedDataAnalyzer <- TRUE
          print("uiControl.R - Load statDataAnalyzer")
          
          burns <- getBurnList()
          burnChoices <- c(c('%'),burns[['burn_id']])
          names(burnChoices) <- c(c('All'),burns[['burn_id']])
          
          renderSelectInput(output, 'selectDataAnalyzerBurn', 'Select burn:', burnChoices , '%')
          observeDataAnalyzerSelectBurnEvent(input, output, session)
          
          renderSelectInput(output, 'selectDataAnalyzerElement', "Select an element:", getElemChoices(), session$userData$sampDatasetAlz$selectedElement)
          observeDataAnalyzerSelectElemEvent(input, output, session)
          
          observeDataAnalyzerBtnEvent(input, output, session)
        }
      )
  }
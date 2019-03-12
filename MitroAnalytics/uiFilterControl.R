source("uiPlotControl.R")
source("io.R")

filterReactValues <- reactiveValues(
  loadingSavedData = FALSE
)

renderSelectInput <- function(output, id, inputLabel, selectOptions, selected=NULL){
  output[[id]] <- renderUI({
    selectInput(inputId = id, label = inputLabel, choices = selectOptions, selected = selected)
  })
}

#
# DataCleaner Plot tab UI filter events
#
observeDataCleanerSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectDataCleanerBurn,
    {
      print("uiFilterControl.R - initialize data")
      
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset. 
      session$userData$sampDataset$selectedBurn <- input$selectDataCleanerBurn
      session$userData$sampDataset$sampData <- getSolConcTreat(input$selectDataCleanerElement,input$selectDataCleanerBurn,'%')
      drawDataCleaner(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
    }
  )
}

observeDataCleanerSelectElemEvent <- function(input, output, session){ #, dataset
  observeEvent(
    input$selectDataCleanerElement,
    {
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset.
      session$userData$sampDataset$selectedElement <- input$selectDataCleanerElement
      session$userData$sampDataset$sampData <- getSolConcTreat(input$selectDataCleanerElement,input$selectDataCleanerBurn,'%')
      drawDataCleaner(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
      
      print("uiFilterControl.R - drawDataCleaner")
    }
  )
}
#'
#' Observer for filtering by treatment type
#'

# observeDataCleanerTreatmentEvent <- function(input, output, session, dataset){ }

observeDataCleanerBtnEvent <- function(input, output, session){
  
  observeEvent(input$btnDataCleanerSave,
   {
     showModal(showSaveDataModal(session$userData$username, session$userData$sampDataset$selectedBurn, session$userData$sampDataset$selectedElement))
     print("uiFilterControl.R - drawDataCleaner Save Prompt")
   }
  )
  
  observeEvent(input$btnSaveExecute,
    {
      filename = input$txtSavedDataFileName
      saveUserDataset(session$userData$sampDataset, session$userData$username, filename)
      drawDataCleaner(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
      removeModal()
      print("uiFilterControl.R - drawDataCleaner Save Execute")
    }
  )
  
  observeEvent(input$btnDataCleanerLoad,
   {
     filenames <- getSavedDatasetList(session$userData$username)
     showModal(showLoadSavedDataModal(filenames))
   }
  )
  
  observeEvent(
    input$btnLoadCleaneredData,
    {
      print(input$selectSavedCleaneredData)
      session$userData$sampDataset <- getSavedDataset(session$userData$username, input$selectSavedCleaneredData)
      drawDataCleaner(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
      removeModal()
    }
  )
  
}

showSaveDataModal <- function(username, burnID, element){
  if(burnID=="%"){burnID = "All"}
  defaultFileName <- dataname <- paste0(username, "-", burnID, "-", element, "-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))
  modalDialog(
    title = "Save the cleaned data",
    textInput("txtSavedDataFileName", label='Enter the dataset name: ', value=defaultFileName),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("btnSaveExecute", "OK")
    )
  )
}

showLoadSavedDataModal <- function(filenames){
  modalDialog(
    title = "Select a dataset to load: ",
    selectInput('selectSavedCleaneredData', NULL, filenames),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("btnLoadCleaneredData", "OK")
    )
  )
}


###############################################################################################################################

#
# DataExplorer tab UI filter events
#
observeDataExplorerSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectDataExplorerBurn,
    {
      print("uiFilterControl.R - initialize data")
      
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset. 
      session$userData$sampDatasetExp$selectedBurn <- input$selectDataExplorerBurn
      session$userData$sampDatasetExp$sampData <- getSolConcTreat(input$selectDataExplorerElement,input$selectDataExplorerBurn,'%')
      drawDataCleaner(input, output, session, session$userData$sampDatasetExp$sampData, session$userData$sampDatasetExp$selectedElement)
      
      #session$userData$sampElem <- getSolConcTreat(input$selectDataExplorerElement,input$selectDataExplorerBurn,'%')
      #session$userData$elemNames <- getElemChoices()
      #renderSelectInput(output, 'selectDataExplorerElement', "Select an element:", session$userData$elemNames, 'Zn')
      #observeDataExplorerSelectElemEvent(input, output, session, input$selectDataExplorerElement)
      
    }
  )
}

observeDataExplorerSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$selectDataExplorerElement,
    {
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset.
      session$userData$sampDatasetExp$selectedElement <- input$selectDataExplorerElement
      session$userData$sampDatasetExp$sampData <- getSolConcTreat(input$selectDataExplorerElement,input$selectDataBurnExp,'%')
      drawDataExplorer(input, output, session, session$userData$sampDatasetExp$sampData, session$userData$sampDatasetExp$selectedElement)
      
      #drawDataExplorer(input, output, session, getSolConcTreat(input$selectDataExplorerElement,input$selectDataExplorerBurn,'%'), input$selectDataExplorerElement)
      print("uiFilterControl.R - drawDataExplorer")
    }
  )
}
#'
#' Observer for filtering by treatment type
#'

# observeDataExploreringTreatmentEvent <- function(input, output, session, dataset){ }

observeDataExplorerBtnEvent <- function(input, output, session){
  observeEvent(input$btnDataExplorerSave,
   {
     # print(session$userData$sampElemDataExplorer)
     # saveUserDataset(session$userData$sampElem, session$userData$username)
     # drawDataExplorer(input, output, session, session$userData$sampElem, session$userData$elemSelected)
     updateSelectInput(session, "dlDataset",choices=getSavedDatasetList(session$userData$username))
     showModal(showSaveDataModalExp(session$userData$username, session$userData$sampDatasetExp$selectedBurn, session$userData$sampDatasetExp$selectedElement))
     print("uiFilterControl.R - drawDataExplorer Save")
   }
  )
  
  observeEvent(input$btnSaveExecuteExp,
   {
     filename = input$txtSavedDataFileNameExp
     saveUserDataset(session$userData$sampDatasetExp, session$userData$username, filename)
     drawDataExplorer(input, output, session, session$userData$sampDatasetExp$sampData, session$userData$sampDatasetExp$selectedElement)
     removeModal()
     print("uiFilterControl.R - drawDataExplorer Save Execute")
   }
  )

  observeEvent(input$btnDataExplorerLoad,
   {
     # sampElem <- getSampElem(session$userData$username) # Loading sampElem from the save file?
     # drawDataExplorer(input, output, session, sampElem, session$userData$elemSelected)
     
     filenames <- getSavedDatasetList(session$userData$username)
     showModal(showLoadSavedDataModalExp(filenames))
     
     print("uiFilterControl.R - drawDataExplorer Load")
   }
  )
  
  observeEvent(
    input$btnLoadDataExp,
    {
      print(input$selectSavedDataExp)
      session$userData$sampDatasetExp <- getSavedDataset(session$userData$username, input$selectSavedDataExp)
      drawDataExplorer(input, output, session, session$userData$sampDatasetExp$sampData, session$userData$sampDatasetExp$selectedElement)
      removeModal()
    }
  )
}

showSaveDataModalExp <- function(username, burnID, element){
  if(burnID=="%"){burnID = "All"}
  defaultFileName <- dataname <- paste0(username, "-", burnID, "-", element, "-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))
  modalDialog(
    title = "Save the explored data",
    textInput("txtSavedDataFileNameExp", label='Enter the dataset name: ', value=defaultFileName),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("btnSaveExecuteExp", "OK")
    )
  )
}

showLoadSavedDataModalExp <- function(filenames){
  modalDialog(
    title = "Select a dataset to load: ",
    selectInput('selectSavedDataExp', NULL, filenames),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("btnLoadDataExp", "OK")
    )
  )
}


##########################################################################################################################################################################################

#
# DataAnalyzer tab UI filter events
#
observeDataAnalyzerSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectDataAnalyzerBurn,
    {
      print("uiFilterControl.R - initialize data")
      
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset. 
      session$userData$sampDatasetAlz$selectedBurn <- input$selectDataAnalyzerBurn
      session$userData$sampDatasetAlz$sampData <- getSolConcTreat(input$selectDataAnalyzerElement,input$selectDataAnalyzerBurn,'%')
      drawDataCleaner(input, output, session, session$userData$sampDatasetAlz$sampData, session$userData$sampDatasetAlz$selectedElement)
      
      #session$userData$sampElem <- getSolConcTreat(input$selectDataAnalyzerElement,input$selectDataAnalyzerBurn,'%')
      #session$userData$elemNames <- getElemChoices()
      #renderSelectInput(output, 'selectDataAnalyzerElement', "Select an element:", session$userData$elemNames, 'Zn')
      #observeDataAnalyzerSelectElemEvent(input, output, session, input$selectDataAnalyzerElement)
      
    }
  )
}

observeDataAnalyzerSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$selectDataAnalyzerElement,
    {
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset.
      session$userData$sampDatasetAlz$selectedElement <- input$selectDataAnalyzerElement
      session$userData$sampDatasetAlz$sampData <- getSolConcTreat(input$selectDataAnalyzerElement,input$selectDataBurnAlz,'%')
      drawDataAnalyzer(input, output, session, session$userData$sampDatasetAlz$sampData, session$userData$sampDatasetAlz$selectedElement)
      
      #drawDataAnalyzer(input, output, session, getSolConcTreat(input$selectDataAnalyzerElement,input$selectDataAnalyzerBurn,'%'), input$selectDataAnalyzerElement)
      print("uiFilterControl.R - drawDataAnalyzer")
    }
  )
}
#'
#' Observer for filtering by treatment type
#'

# observeDataAnalyzeringTreatmentEvent <- function(input, output, session, dataset){ }

observeDataAnalyzerBtnEvent <- function(input, output, session){
  observeEvent(input$btnDataAnalyzerSave,
   {
     # print(session$userData$sampElemDataAnalyzer)
     # saveUserDataset(session$userData$sampElem, session$userData$username)
     # drawDataAnalyzer(input, output, session, session$userData$sampElem, session$userData$elemSelected)
     updateSelectInput(session, "dlDataset",choices=getSavedDatasetList(session$userData$username))
     showModal(showSaveDataModalAlz(session$userData$username, session$userData$sampDatasetAlz$selectedBurn, session$userData$sampDatasetAlz$selectedElement))
     print("uiFilterControl.R - drawDataAnalyzer Save")
   }
  )
  
  observeEvent(input$btnSaveExecuteAlz,
   {
     filename = input$txtSavedDataFileNameAlz
     saveUserDataset(session$userData$sampDatasetAlz, session$userData$username, filename)
     drawDataAnalyzer(input, output, session, session$userData$sampDatasetAlz$sampData, session$userData$sampDatasetAlz$selectedElement)
     removeModal()
     print("uiFilterControl.R - drawDataAnalyzer Save Execute")
   }
  )

  observeEvent(input$btnDataAnalyzerLoad,
   {
     # sampElem <- getSampElem(session$userData$username) # Loading sampElem from the save file?
     # drawDataAnalyzer(input, output, session, sampElem, session$userData$elemSelected)
     
     filenames <- getSavedDatasetList(session$userData$username)
     showModal(showLoadSavedDataModalAlz(filenames))
     
     print("uiFilterControl.R - drawDataAnalyzer Load")
   }
  )
  
  observeEvent(
    input$btnLoadDataAlz,
    {
      print(input$selectSavedDataAlz)
      session$userData$sampDatasetAlz <- getSavedDataset(session$userData$username, input$selectSavedDataAlz)
      drawDataAnalyzer(input, output, session, session$userData$sampDatasetAlz$sampData, session$userData$sampDatasetAlz$selectedElement)
      removeModal()
    }
  )
}

showSaveDataModalAlz <- function(username, burnID, element){
  if(burnID=="%"){burnID = "All"}
  defaultFileName <- dataname <- paste0(username, "-", burnID, "-", element, "-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))
  modalDialog(
    title = "Save the analyzed data",
    textInput("txtSavedDataFileNameAlz", label='Enter the dataset name: ', value=defaultFileName),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("btnSaveExecuteAlz", "OK")
    )
  )
}

showLoadSavedDataModalAlz <- function(filenames){
  modalDialog(
    title = "Select a dataset to load: ",
    selectInput('selectSavedDataAlz', NULL, filenames),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("btnLoadDataAlz", "OK")
    )
  )
}

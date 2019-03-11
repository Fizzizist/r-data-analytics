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
# DataCleaning Plot tab UI filter events
#
observeDataCleaningSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectDataCleaningBurn,
    {
      print("uiFilterControl.R - initialize data")
      
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset. 
      session$userData$sampDataset$selectedBurn <- input$selectDataCleaningBurn
      session$userData$sampDataset$sampData <- getSolConcTreat(input$selectDataCleaningElement,input$selectDataCleaningBurn,'%')
      drawDataCleaning(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
    }
  )
}

observeDataCleaningSelectElemEvent <- function(input, output, session){ #, dataset){
  observeEvent(
    input$selectDataCleaningElement,
    {
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset.
      session$userData$sampDataset$selectedElement <- input$selectDataCleaningElement
      session$userData$sampDataset$sampData <- getSolConcTreat(input$selectDataCleaningElement,input$selectDataCleaningBurn,'%')
      drawDataCleaning(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
      
      print("uiFilterControl.R - drawDataCleaning")
    }
  )
}
#'
#' Observer for filtering by treatment type
#'

# observeDataCleaningTreatmentEvent <- function(input, output, session, dataset){ }

observeDataCleaningBtnEvent <- function(input, output, session){
  
  observeEvent(input$btnDataCleaningSave,
   {
     showModal(showSaveDataModal(session$userData$username, session$userData$sampDataset$selectedBurn, session$userData$sampDataset$selectedElement))
     print("uiFilterControl.R - drawDataCleaning Save Prompt")
   }
  )
  
  observeEvent(input$btnSaveExecute,
    {
      filename = input$txtSavedDataFileName
      saveUserDataset(session$userData$sampDataset, session$userData$username, filename)
      drawDataCleaning(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
      removeModal()
      print("uiFilterControl.R - drawDataCleaning Save Execute")
    }
  )
  
  observeEvent(input$btnDataCleaningLoad,
   {
     filenames <- getSavedDatasetList(session$userData$username)
     showModal(showLoadSavedDataModal(filenames))
   }
  )
  
  observeEvent(
    input$btnLoadCleanedData,
    {
      print(input$selectSavedCleanedData)
      session$userData$sampDataset <- getSavedDataset(session$userData$username, input$selectSavedCleanedData)
      drawDataCleaning(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
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
    selectInput('selectSavedCleanedData', NULL, filenames),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("btnLoadCleanedData", "OK")
    )
  )
}


###############################################################################################################################

#
# DataExploring Plot tab UI filter events
#
observeDataExploringSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectDataExploringBurn,
    {
      print("uiFilterControl.R - initialize data")
      
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset. 
      session$userData$sampDatasetExp$selectedBurn <- input$selectDataExploringBurn
      session$userData$sampDatasetExp$sampData <- getSolConcTreat(input$selectDataExploringElement,input$selectDataExploringBurn,'%')
      drawDataCleaning(input, output, session, session$userData$sampDatasetExp$sampData, session$userData$sampDatasetExp$selectedElement)
      
      #session$userData$sampElem <- getSolConcTreat(input$selectDataExploringElement,input$selectDataExploringBurn,'%')
      #session$userData$elemNames <- getElemChoices()
      #renderSelectInput(output, 'selectDataExploringElement', "Select an element:", session$userData$elemNames, 'Zn')
      #observeDataExploringSelectElemEvent(input, output, session, input$selectDataExploringElement)
      
    }
  )
}

observeDataExploringSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$selectDataExploringElement,
    {
      # sampDataset object keeps track of any changes in selected burn, element or plotted dataset.
      session$userData$sampDatasetExp$selectedElement <- input$selectDataExploringElement
      session$userData$sampDatasetExp$sampData <- getSolConcTreat(input$selectDataExploringElement,input$selectDataBurnExp,'%')
      drawDataExploring(input, output, session, session$userData$sampDatasetExp$sampData, session$userData$sampDatasetExp$selectedElement)
      
      #drawDataExploring(input, output, session, getSolConcTreat(input$selectDataExploringElement,input$selectDataExploringBurn,'%'), input$selectDataExploringElement)
      print("uiFilterControl.R - drawDataExploring")
    }
  )
}
#'
#' Observer for filtering by treatment type
#'

# observeDataExploringingTreatmentEvent <- function(input, output, session, dataset){ }

observeDataExploringBtnEvent <- function(input, output, session){
  observeEvent(input$btnDataExploringSave,
   {
     # print(session$userData$sampElemDataExploring)
     # saveUserDataset(session$userData$sampElem, session$userData$username)
     # drawDataExploring(input, output, session, session$userData$sampElem, session$userData$elemSelected)
     updateSelectInput(session, "dlDataset",choices=getSavedDatasetList(session$userData$username))
     showModal(showSaveDataModalExp(session$userData$username, session$userData$sampDatasetExp$selectedBurn, session$userData$sampDatasetExp$selectedElement))
     print("uiFilterControl.R - drawDataExploring Save")
   }
  )
  
  observeEvent(input$btnSaveExecuteExp,
   {
     filename = input$txtSavedDataFileNameExp
     saveUserDataset(session$userData$sampDatasetExp, session$userData$username, filename)
     drawDataExploring(input, output, session, session$userData$sampDatasetExp$sampData, session$userData$sampDatasetExp$selectedElement)
     removeModal()
     print("uiFilterControl.R - drawDataCleaning Save Execute")
   }
  )

  observeEvent(input$btnDataExploringLoad,
   {
     # sampElem <- getSampElem(session$userData$username) # Loading sampElem from the save file?
     # drawDataExploring(input, output, session, sampElem, session$userData$elemSelected)
     
     filenames <- getSavedDatasetList(session$userData$username)
     showModal(showLoadSavedDataModalExp(filenames))
     
     print("uiFilterControl.R - drawDataExploring Load")
   }
  )
  
  observeEvent(
    input$btnLoadDataExp,
    {
      print(input$selectSavedDataExp)
      session$userData$sampDatasetExp <- getSavedDataset(session$userData$username, input$selectSavedDataExp)
      drawDataExploring(input, output, session, session$userData$sampDatasetExp$sampData, session$userData$sampDatasetExp$selectedElement)
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

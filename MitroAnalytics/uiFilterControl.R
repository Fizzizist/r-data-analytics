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
     #saveUserDataset(session$userData$sampDataset, session$userData$username)
     #drawDataCleaning(input, output, session, session$userData$sampDataset$sampData, session$userData$sampDataset$selectedElement)
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
      session$userData$sampElem <- getSolConcTreat(input$selectDataExploringElement,input$selectDataExploringBurn,'%') # this needs to take in the burn id like getPTValues(input$selectDataCleaningBurn)
      session$userData$elemNames <- getElemChoices()
      renderSelectInput(output, 'selectDataExploringElement', "Select an element:", session$userData$elemNames, 'Zn')
      observeDataExploringSelectElemEvent(input, output, session, input$selectDataExploringElement)
      observeDataExploringBtnEvent(input, output, session)
    }
  )
}

observeDataExploringSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$selectDataExploringElement,
    {
      drawDataExploring(input, output, session, getSolConcTreat(input$selectDataExploringElement,input$selectDataExploringBurn,'%'), input$selectDataExploringElement)
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
     print(session$userData$sampElemDataExploring)
     saveUserDataset(session$userData$sampElem, session$userData$username)
     drawDataExploring(input, output, session, session$userData$sampElem, session$userData$elemSelected)
     print("uiFilterControl.R - drawDataExploring Save")
   }
  )
  
  observeEvent(input$btnDataExploringLoad,
   {
     sampElem <- getSampElem(session$userData$username) # Loading sampElem from the save file?
     drawDataExploring(input, output, session, sampElem, session$userData$elemSelected)
    print("uiFilterControl.R - drawDataExploring Load")
   }
  )
}

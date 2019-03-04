source("uiPlotControl.R")
source("io.R")

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
      session$userData$sampElem <- getSolConcTreat(input$selectDataCleaningElement,input$selectDataCleaningBurn,'%') # this needs to take in the burn id like getPTValues(input$selectDataCleaningBurn)
      session$userData$elemNames <- getElemChoices()
      renderSelectInput(output, 'selectDataCleaningElement', "Select an element:", session$userData$elemNames, 'Zn')
      observeDataCleaningSelectElemEvent(input, output, session, input$selectDataCleaningElement)
      observeDataCleaningBtnEvent(input, output, session)
    }
  )
}

observeDataCleaningSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$selectDataCleaningElement,
    {
      drawDataCleaning(input, output, session, getSolConcTreat(input$selectDataCleaningElement,input$selectDataCleaningBurn,'%'), input$selectDataCleaningElement)
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
     print(session$userData$sampElemDataCleaning)
     saveUserDataset(session$userData$sampElem, session$userData$username)
     session$userData$solVec <- session$userData$sampElem$solution_id
     drawDataCleaning(input, output, session, session$userData$sampElem, session$userData$elemSelected)
     print("uiFilterControl.R - drawDataCleaning Save")
   }
  )
  
  observeEvent(input$btnDataCleaningLoad,
   {
     sampElem <- getSampElem(session$userData$username) # Loading sampElem from the save file?
     drawDataCleaning(input, output, session, sampElem, session$userData$elemSelected)
    print("uiFilterControl.R - drawDataCleaning Load")
   }
  )
  
  observeEvent(input$btnDataCleaningReset,
   {
     session$userData$sampElem <- getSolConcTreat(input$selectDataCleaningElement,input$selectDataCleaningBurn,NULL) # I am not sure if I should be calling the input$variable values here?
     print(session$userData$sampElem)
     renderSelectInput(output, 'selectDataCleaningElement', "Select an element:", session$userData$elemNames, session$userData$elemSelected)
     observeDataCleaningSelectElemEvent(input, output, session, session$userData$sampElem)
   }
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
  
  observeEvent(input$btnDataExploringReset,
   {
     session$userData$sampElem <- getSolConcTreat(input$selectDataExploringElement,input$selectDataExploringBurn,) # I am not sure if I should be calling the input$variable values here?
     print(session$userData$sampElem)
     renderSelectInput(output, 'selectDataExploringElement', "Select an element:", session$userData$elemNames, session$userData$elemSelected)
     observeDataExploringSelectElemEvent(input, output, session, session$userData$sampElem)
   }
  )
}

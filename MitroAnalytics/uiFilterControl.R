source("uiPlotControl.R")
source("io.R")

renderSelectInput <- function(output, id, inputLabel, selectOptions, selected=NULL){
  output[[id]] <- renderUI({
    selectInput(inputId = id, label = inputLabel, choices = selectOptions, selected = selected)
  })
}

#
# Plotly Plot tab UI filter events
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

#' Observer for filtering by treatment type
#'

# observeDataCleaningTreatmentEvent <- function(input, output, session, dataset){ }

observeDataCleaningBtnEvent <- function(input, output, session){
  observeEvent(input$btnPlotlySave,
   {
     print(session$userData$sampElemPlotly)
     saveUserDataset(session$userData$sampElem, session$userData$username)
     drawDataCleaning(input, output, session, session$userData$sampElem, session$userData$elemSelected)
     print("uiFilterControl.R - drawDataCleaning Save")
   }
  )
  
  observeEvent(input$btnPlotlyLoad,
   {
     sampElem <- getSampElem(session$userData$username) # Loading sampElem from the save file?
     drawDataCleaning(input, output, session, sampElem, session$userData$elemSelected)
    print("uiFilterControl.R - drawDataCleaning Load")
   }
  )
  
  observeEvent(input$btnPlotlyReset,
   {
     session$userData$sampElem <- getSolConcTreat(input$selectDataCleaningElement,input$selectDataCleaningBurn,NULL) # I am not sure if I should be calling the input$variable values here?
     print(session$userData$sampElem)
     renderSelectInput(output, 'selectDataCleaningElement', "Choose an element:", session$userData$elemNames, session$userData$elemSelected)
     observeDataCleaningSelectElemEvent(input, output, session, session$userData$sampElem)
     #print("input$btnPlotlyReset")
   }
  )
}

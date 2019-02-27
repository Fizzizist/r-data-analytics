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
observePlotlyPlotSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectPlotlyPlotBurn,
    {
      session$userData$sampElem <- getPTValues() # this needs to take in the burn id like getPTValues(input$selectPlotlyPlotBurn)
      session$userData$elemNames <- names(session$userData$sampElem)
      renderSelectInput(output, 'selectPlotlyPlotElement', "Choose an element:", session$userData$elemNames, 'Zn')
      observePlotlyPlotSelectElemEvent(input, output, session, session$userData$sampElem)
      observePlotlyPlotBtnEvent(input, output, session)
    }
  )
}

observePlotlyPlotSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$selectPlotlyPlotElement,
    {
      drawPlotlyPlot(input, output, session, dataset[[input$selectPlotlyPlotElement]], input$selectPlotlyPlotElement)
    }
  )
}

observePlotlyPlotBtnEvent <- function(input, output, session){
  observeEvent(input$btnPlotlySave,
   {
     print(session$userData$sampElemPlotly)
     saveUserDataset(session$userData$sampElem, session$userData$username)
     drawPlotlyPlot(input, output, session, session$userData$sampElem, session$userData$elemSelected)
   }
  )
  
  observeEvent(input$btnPlotlyLoad,
   {
     sampElem <- getSampElem(session$userData$username)
     drawPlotlyPlot(input, output, session, sampElem, session$userData$elemSelected)
   }
  )
  
  observeEvent(input$btnPlotlyReset,
   {
     session$userData$sampElem <- getPTValues()
     print(session$userData$sampElem)
     renderSelectInput(output, 'selectPlotlyPlotElement', "Choose an element:", session$userData$elemNames, session$userData$elemSelected)
     observePlotlyPlotSelectElemEvent(input, output, session, session$userData$sampElem)
   }
  )
}

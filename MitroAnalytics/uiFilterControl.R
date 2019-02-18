source("uiPlotControl.R")
source("io.R")

renderSelectInput <- function(output, id, inputLabel, selectOptions, selected=NULL){
  output[[id]] <- renderUI({
    selectInput(inputId = id, label = inputLabel, choices = selectOptions, selected = selected)
  })
}

#
# Hist Plot tab UI filter events
#
renderHistFilters <- function(output, solutionNames, elementNames, selectedSolution, selectedElements){
  output$solutionCheckboxes <- renderUI({
    checkboxGroupInput("solutionCheckboxes", "Samples: ", choices=solutionNames, selected=selectedSolution)
  })
  output$elementCheckboxes <- renderUI({
    checkboxGroupInput("elementCheckboxes", "Elements: ", choices=elementNames, selected=selectedElements)
  })
}

observeHistBuildEvent <- function(input, output, sample){
  observeEvent(
    input$btnBuild,
    {
      df <- isolate({
        selectedSamples <- sample[sample$label %in% input$solutionCheckboxes,]
        selectedSamples = selectedSamples[selectedSamples$element_id %in% input$elementCheckboxes,]
        data.frame(selectedSamples)
      })
      selectedSolution = unique(df$label)
      selectedElement = unique(df$element_id)
      drawHistogram(output, df)
    }
  )
}

observeHistResetEvent <- function(input, output, session, histData, solutionNames, elementNames, selectedSolution, selectedElements){
  observeEvent(
    input$btnReset,
    {
      drawHistogram(output, histData)
      updateCheckboxGroupInput(session, "solutionCheckboxes", choices=solutionNames, selected = selectedSolution)
      updateCheckboxGroupInput(session, "elementCheckboxes", choices=elementNames, selected = selectedElements)
    }
  )
}

observeHistSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectHistBurn,
    {
      dataset <- getBurnSolutionConcentration(input$selectHistBurn)
      solNames <- unique(dataset["label"])
      elemNames <- unique(dataset["element_id"])
      histData <- dataset[which(dataset$label == solNames[4,]),]

      renderHistFilters(output, solNames[,], elemNames[,], solNames[4,], elemNames[,])
      drawHistogram(output, histData)
      observeHistBuildEvent(input, output, dataset)
      observeHistResetEvent(input, output, session, histData, solNames[,], elemNames[,], solNames[4,], elemNames[,])
    }
  )
}

#
# Interactive Plot tab UI filter events
#
observeIntPlotSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectIntPlotBurn,
    {
      session$userData$sampElem <- getPTValues() # this needs to take in the burn id like getPTValues(input$selectPlotlyPlotBurn)
      session$userData$elemNames <- names(session$userData$sampElem)
      renderSelectInput(output, 'selectIntElement', "Choose an element:", session$userData$elemNames, 'Zn')
      observeIntPlotSelectElemEvent(input, output, session, session$userData$sampElem)
      observeIntPlotBtnEvent(input, output, session)
    }
  )
}

observeIntPlotSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$selectIntElement,
    {
      session$userData$elemSelected <- input$selectIntElement
      drawInteractivePlot(input, output, session, dataset[[input$selectIntElement]], input$selectIntElement)
    }
  )
}

observeIntPlotBtnEvent <- function(input, output, session){
  observeEvent(input$btnIntSave,
   {
     saveUserDataset(session$userData$sampElem, session$userData$username)
     drawInteractivePlot(input, output, session, session$userData$sampElem, session$userData$elemSelected, saved=TRUE)
     session$sendCustomMessage("setSavedHandler", TRUE)
   }
  )
  
  observeEvent(input$btnIntLoad,
   {
     sampElem <- getSampElem(session$userData$username)
     renderSelectInput(output, 'selectIntElement', "Choose an element:", session$userData$elemNames, session$userData$elemSelected)
     drawInteractivePlot(input, output, session, sampElem, session$userData$elemSelected)
   }
  )
  
  observeEvent(input$btnIntReset,
   {
     session$userData$sampElem <- getPTValues()
     renderSelectInput(output, 'selectIntElement', "Choose an element:", session$userData$elemNames, session$userData$elemSelected)
     observeIntPlotSelectElemEvent(input, output, session, session$userData$sampElem)
   }
  )
}

#
# Plotly Plot tab UI filter events
#
observePlotlyPlotSelectBurnEvent <- function(input, output, session){
  observeEvent(
    input$selectPlotlyPlotBurn,
    {
      session$userData$sampElemPlotly <- getPTValues() # this needs to take in the burn id like getPTValues(input$selectPlotlyPlotBurn)
      session$userData$elemNames <- names(session$userData$sampElemPlotly)
      renderSelectInput(output, 'selectPlotlyPlotElement', "Choose an element:", session$userData$elemNames, 'Zn')
      observePlotlyPlotSelectElemEvent(input, output, session, session$userData$sampElemPlotly)
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
     saveUserDataset(session$userData$sampElemPlotly, session$userData$username)
     drawPlotlyPlot(input, output, session, session$userData$sampElemPlotly, session$userData$elemSelectedPlotly)
   }
  )
  
  observeEvent(input$btnPlotlyLoad,
   {
     sampElem <- getSampElem(session$userData$username)
     drawPlotlyPlot(input, output, session, sampElem, session$userData$elemSelectedPlotly)
   }
  )
  
  observeEvent(input$btnPlotlyReset,
   {
     session$userData$sampElemPlotly <- getPTValues()
     print(session$userData$sampElemPlotly)
     renderSelectInput(output, 'selectPlotlyPlotElement', "Choose an element:", session$userData$elemNames, session$userData$elemSelectedPlotly)
     observePlotlyPlotSelectElemEvent(input, output, session, session$userData$sampElemPlotly)
   }
  )
}

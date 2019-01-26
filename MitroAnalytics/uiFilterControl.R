source("uiPlotControl.R")
source("io.R")

renderSelectInput <- function(output, id, inputLabel, selectOptions, selected=NULL){
  output[[id]] <- renderUI({
    selectInput(inputId = id, label = inputLabel, choices = selectOptions, selected = selected)
  })
}

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
    input$selectBurn,
    {
      dataset <- getBurnSolutionConcentration(input$selectBurn)
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
     drawInteractivePlot(input, output, session, session$userData$sampElem, session$userData$elemSelected)
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

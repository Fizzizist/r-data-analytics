source("uiPlotControl.R")
source("io.R")

# Fiter functions for Histogram
renderHistBurnFilter <- function(output, burns){
  output$selectBurn <- renderUI({
    selectInput('selectBurn', 'Select burn:', burns)
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



# Filter functions for Interactive Plot
renderIntPlotElemFilter <- function(output, selectItems){
  output$selectIntElement <- renderUI({
    selectInput("intElemChoice", "Choose an element:", choices = selectItems, selected = "Zn")
  })
}

observeIntPlotSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$intElemChoice,
    {
      drawInteractivePlot(input, output, session, dataset[[input$intElemChoice]], input$intElemChoice)
    }
  )
}

# Filter functions for Plotly Plot
renderPoltlyPlotElemFilter <- function(output, selectItems){
  output$selectPlotlyPlotElement <- renderUI({
    selectInput("plotlyElemChoice", "Choose an element:", choices = selectItems, selected = "Zn")
  })
}

observePlotlyPlotSelectElemEvent <- function(input, output, session, dataset){
  observeEvent(
    input$plotlyElemChoice,
    {
      drawPlotlyPlot(input, output, session, dataset[[input$plotlyElemChoice]], input$plotlyElemChoice)
    }
  )
}

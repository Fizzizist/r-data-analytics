source("uiPlotControl.R")
source("io.R")

# Fiter functions for Histogram
renderHistSessionFilter <- function(output, sessions){
  output$selectSession <- renderUI({
    selectInput('selectSession', 'Select session:', sessions)
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

observeHistSelectSessionEvent <- function(input, output, session){
  observeEvent(
    input$selectSession,
    {
      dataset <- getSessionSolutionConcentration(input$selectSession)
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

# 4. Filter functions for Interactive Plot 
# The selectInput to be populated with the database data.
# Need another select input to select which session, and maybe have the elements as radio buttons?
renderIntPlotElemFilter <- function(output, selectItems){
  output$selectIntElement <- renderUI({
    selectInput(
      "intElemChoice",
      "Choose an element:",
      choices = selectItems,
      selected = "Zn"
    )
  })
}

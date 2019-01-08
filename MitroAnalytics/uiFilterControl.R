source("uiPlotControl.R")

# Fiter functions for Histogram
renderHistSessionFilter <- function(output, sessions){
  dfSessions = data.frame(names = sessions$name, values = sessions$file)
  sessionChoices <- as.list(dfSessions$values)
  names(sessionChoices) <- dfSessions$names
  output$selectSession <- renderUI({
    selectInput('selectSession', 'Select session:', sessionChoices)
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
        selectedSamples <- sample[sample$Solution.Label %in% input$solutionCheckboxes,]
        selectedSamples = selectedSamples[selectedSamples$Element %in% input$elementCheckboxes,]
        data.frame(selectedSamples)
      })
      selectedSolution = unique(df$Solution.Label)
      selectedElement = unique(df$Element)
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
      sample <- read.csv(paste("data/", input$selectSession, sep=""), stringsAsFactors = FALSE) # To be replace with a call to DB or the Stat module
      
      solutionNames <- unique(sample$Solution.Label)
      elementNames <- unique(sample$Element)
      histData <- data.frame(sample[sample$Solution.Label == solutionNames[3],])
      renderHistFilters(output, solutionNames, elementNames, solutionNames[3], elementNames)
      drawHistogram(output, histData)
      observeHistBuildEvent(input, output, sample)
      observeHistResetEvent(input, output, session, histData, solutionNames, elementNames, solutionNames[3], elementNames)
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
      #choices = c("Al", "As", "Ba", "Ca", "Cd", "Cu", "Co", "Cr", "Cu", "Fe", "K", "Mg", "Mn", "Mo", "Ni", "Pb", "Se", "Sr", "Y", "Zn"),
      choices = selectItems,
      selected = "Zn"
    )
  })
}

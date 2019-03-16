library(dplyr)
library(ggplot2)
library(DT)
library(crosstalk)
library(shiny)
library(plotly)

# Function to output the plotly plot
drawDataCleaner <- function(input, output, session, data, selectedElement) {
  #session$userData$elemSelected <- selectedElement

  # Initialize data values and reset selections
  observeEvent(selectedElement, {
    print("uiPlotControl.R - Initialize data")

    js$resetSelected()
    dataTableProxy('dataCleanerDT') %>% selectRows(NULL)

    dataCleanerCurrentTibble <<- data %>% tibble::remove_rownames()
    dataCleanerCurrentSharedData <<- SharedData$new(dataCleanerCurrentTibble, ~solution_id)
  })

  # Renders Datatable
  output$dataCleanerDT <- renderDT({ # Renders the datatable
    
    updateDataTable <- event_data("plotly_selected")

    print("uiPlotControl.R - Rendering datatableCleaner")

    datatableCleaner <- DT::datatable(dataCleanerCurrentTibble, option = list(pageLength = 20))
    datatableCleaner

  },server=FALSE)
  
  output$dataCleanerScatter <- renderPlotly({
    
    selectedRowsDT <- input$dataCleanerDT_rows_selected

    print("uiPlotControl.R - renderPlotly(dataCleanerScatter)")
    
    if (length(selectedRowsDT)) {

      print("uiPlotControl.R - Render scatterPlotSelected")

        scatterPlotSelected <- dataCleanerCurrentTibble %>%
        plot_ly() %>% 
        add_trace(x = ~solid_conc, 
                  y = ~solution_id, 
                  type = "scatter", 
                  color = I('black'), 
                  name = 'Unfiltered',
                  mode = "markers",
                  transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
      
      # selected data
      scatterPlotSelected <- add_trace(scatterPlotSelected, 
                data = dataCleanerCurrentTibble[selectedRowsDT, , drop = F], 
                type = 'scatter', 
                mode = 'markers',
                x = ~solid_conc, 
                y = ~solution_id, 
                color = I('#B40000'), 
                name = 'Filtered',
                transforms = list(list(type='groupby',groups=selectedElement)))
      
    } else {
      print("uiPlotControl.R - Render scatterPlotUnselected")

        scatterPlotUnselected <- dataCleanerCurrentSharedData %>%
        plot_ly(x = ~solid_conc, 
                y = ~solution_id, 
                type = 'scatter', 
                color = I('black'), 
                name = 'Unfiltered',
                mode = "markers",
                transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
    
    }
  })
  
  # Renders interactive boxplot
  output$dataCleanerBox <- renderPlotly({
    print("uiPlotControl.R - renderPlotly(dataCleanerBox)")
    boxData <- dataCleanerCurrentTibble[input$dataCleanerDT_rows_selected, ] # Stores the datatable rows which are selected

    if(length(input$dataCleanerDT_rows_selected)){ # Renders when there are selected rows
      print("uiPlotControl.R - Render bb")
      boxSelected <- boxData %>%
        plot_ly(x=~solid_conc,
                type = "box",
                boxpoints = "all",
                color = I("#B40000"),
                jitter = 0.3,
                pointpos = -1.5,
                boxmean = TRUE
        )
    } else { # Renders when there aren't selected rows
      print("uiPlotControl.R - Render b")
      boxUnselected <- dataCleanerCurrentTibble %>%
        plot_ly(x=~solid_conc,
                type = "box",
                color = I('black'),
                boxpoints = "all",
                jitter = 0.3,
                pointpos = -1.5,
                boxmean = TRUE
        )
    }
  })
  
  # Pass current selection to save function
  output$dataCleanerSaveData <- renderPrint({
    session$userData$sampDataset$sampData <- dataCleanerCurrentTibble[input$dataCleanerDT_rows_selected, ]
  })
  
  observeEvent(dataCleanerCurrentSharedData$selection(),{
    print("uiPlotControl.R - observeEvent(selectRows(tabSelect)")
    updatePlotValues <- event_data("plotly_selected")
    tabSelect <- which(dataCleanerCurrentTibble$solution_id %in% updatePlotValues$y) # Retrieve indices of seleted rows
    dataTableProxy('dataCleanerDT') %>% selectRows(as.character(tabSelect))
  })
  
}

###################################################################################################################################################

drawDataExplorer <- function(input, output, session, data, selectedElement) {
  #session$userData$elemSelected <- selectedElement

  # Initialize data values and reset selections
  observeEvent(selectedElement, {
    print("uiPlotControl.R - Initialize data")

    js$resetSelected()
    dataTableProxy('dataExplorerDT') %>% selectRows(NULL)

    dataExplorerCurrentTibble <<- data %>% tibble::remove_rownames()
    dataExplorerCurrentSharedData <<- SharedData$new(dataExplorerCurrentTibble, ~solution_id)
  })

  # Renders Datatable
  output$dataExplorerDT <- renderDT({ # Renders the datatable
    
    updateDataTable <- event_data("plotly_selected")

    print("uiPlotControl.R - Rendering datatableExplorer")

    datatableExplorer <- DT::datatable(dataExplorerCurrentTibble, option = list(pageLength = 20))
    datatableExplorer

  },server=FALSE)
  
  output$dataExplorerScatter <- renderPlotly({
    
    selectedRowsDT <- input$dataExplorerDT_rows_selected

    print("uiPlotControl.R - renderPlotly(dataExplorerScatter)")
    
    if (length(selectedRowsDT)) {

      print("uiPlotControl.R - Render scatterPlotSelected")

        scatterPlotSelected <- dataExplorerCurrentTibble %>%
        plot_ly() %>% 
        add_trace(x = ~solid_conc, 
                  y = ~solution_id, 
                  type = "scatter", 
                  color = I('black'), 
                  name = 'Unfiltered',
                  mode = "markers",
                  transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
      
      # selected data
      scatterPlotSelected <- add_trace(scatterPlotSelected, 
                data = dataExplorerCurrentTibble[selectedRowsDT, , drop = F], 
                type = 'scatter', 
                mode = 'markers',
                x = ~solid_conc, 
                y = ~solution_id, 
                color = I('#B40000'), 
                name = 'Filtered',
                transforms = list(list(type='groupby',groups=selectedElement)))
      
    } else {
      print("uiPlotControl.R - Render scatterPlotUnselected")

        scatterPlotUnselected <- dataExplorerCurrentSharedData %>%
        plot_ly(x = ~solid_conc, 
                y = ~solution_id, 
                type = 'scatter', 
                color = I('black'), 
                name = 'Unfiltered',
                mode = "markers",
                transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
    
    }
  })
  
  # Renders histogram
  output$dataExplorerHist <- renderPlotly({
    print("uiPlotControl.R - renderPlotly(dataExplorerBox)")
    histData <- dataExplorerCurrentTibble[input$dataExplorerDT_rows_selected, ] # Stores the datatable rows which are selected

    if(length(input$dataExplorerDT_rows_selected)){ # Renders when there are selected rows
      print("uiPlotControl.R - Render boxSelected")
      histSelected <- histData %>%
        plot_ly(x=~solid_conc,
                type = "histogram",
                histnorm = "percent",
                marker=list(color=rep(I('#B40000'), 1000))
        )
    } else { # Renders when there aren't selected rows
      print("uiPlotControl.R - Render b")
      histUnselected <- dataExplorerCurrentTibble %>%
        plot_ly(x=~solid_conc,
                type = "histogram",
                histnorm = "percent",
                marker=list(color=rep(I("black"), 1000))
        )
    }
  })

  # Renders interactive violin plot
  output$dataExplorerViolin <- renderPlotly({

    violData <- dataExplorerCurrentTibble[input$dataExplorerDT_rows_selected, ]

    if(length(input$dataExplorerDT_rows_selected)){ # Renders when there are selected rows
      print("uiPlotControl.R - Render boxSelected")
      violSelected <- violData %>%
        plot_ly(x=~solid_conc,
                type = "violin",
                points="suspectedoutliers",
                jitter=0.2,
                pointpos=0,
                box=list(visible=TRUE),
                line=list(color="B40000"),
                meanline=list(visible=TRUE,color="black"),
                marker=list(color=rep(I('black'), 1000))
        )
    } else { # Renders when there aren't selected rows
      print("uiPlotControl.R - Render b")
      violUnselected <- dataExplorerCurrentTibble %>%
        plot_ly(x=~solid_conc,
                type = "violin",
                points = "suspectedoutliers",
                jitter=0.2,
                pointpos=0,
                box=list(visible=TRUE),
                line=list(color="black"),
                meanline=list(visible=TRUE,color="black"),
                marker=list(color=rep(I("black"), 1000))
        )
    }
  })

  # Renders summary statistics table
  output$dataExplorerStatsDT <- renderDT({
    
    if(length(input$dataExplorerDT_rows_selected)){
      dataTibble <- as.vector(dataExplorerCurrentTibble[input$dataExplorerDT_rows_selected,3])
    } else {
      dataTibble <- as.vector(dataExplorerCurrentTibble[,3])
    }
    dataExplorerStatsVector<- c(
              min(dataTibble),
              mean(dataTibble),
              median(dataTibble),
              max(dataTibble),
              sd(dataTibble)
              )
    dataExplorerStatsVector <- formatC(dataExplorerStatsVector,digits=3)
    names(dataExplorerStatsVector) <- c("min","mean","median","max","sd")
    dataExplorerSumStats <- rbind(dataExplorerStatsVector)
    dataExplorerDTStats <- DT::datatable(dataExplorerSumStats,options=list(dom='t'), rownames=FALSE)
    dataExplorerDTStats
  })
  
  # Pass current selection to save function
  output$dataExplorerSaveData <- renderPrint({
    # Use sampDatasetExp object to save data saved from the Explorer tab.
    session$userData$sampDatasetExp$sampData <- dataExplorerCurrentTibble[input$dataExplorerDT_rows_selected, ] # Selection from data table using 'crosstalk' package.
  })
  
  observeEvent(dataExplorerCurrentSharedData$selection(),{
    print("uiPlotControl.R - observeEvent(selectRows(tabSelect)")
    updatePlotValues <- event_data("plotly_selected")
    tabSelect <- which(dataExplorerCurrentTibble$solution_id %in% updatePlotValues$y) # Retrieve indices of seleted rows
    dataTableProxy('dataExplorerDT') %>% selectRows(as.character(tabSelect))
  }) 
}

####################################################################################################################################################

drawDataAnalyzer <- function(input, output, session, data, selectedElement) {

  # Initialize data values and reset selections
  observeEvent(selectedElement, {
    print("uiPlotControl.R - Initialize data")

    js$resetSelected()
    dataTableProxy('dataAnalyzerDT') %>% selectRows(NULL)

    dataAnalyzerCurrentTibble <<- data %>% tibble::remove_rownames()
    dataAnalyzerCurrentSharedData <<- SharedData$new(dataAnalyzerCurrentTibble, ~solution_id)
  })

  # Renders Datatable
  output$dataAnalyzerDT <- renderDT({ # Renders the datatable
    
    updateDataTable <- event_data("plotly_selected")

    print("uiPlotControl.R - Rendering datatableAnalyzer")

    datatableAnalyzer <- DT::datatable(dataAnalyzerCurrentTibble, option = list(pageLength = 20))
    datatableAnalyzer

  },server=FALSE)
  
  output$dataAnalyzerScatter <- renderPlotly({
    
    selectedRowsDT <- input$dataAnalyzerDT_rows_selected

    print("uiPlotControl.R - renderPlotly(dataAnalyzerScatter)")
    
    if (length(selectedRowsDT)) {

      print("uiPlotControl.R - Render scatterPlotSelected")

        scatterPlotSelected <- dataAnalyzerCurrentTibble %>%
        plot_ly() %>% 
        add_trace(x = ~solid_conc, 
                  y = ~solution_id, 
                  type = "scatter", 
                  color = I('black'), 
                  name = 'Unfiltered',
                  mode = "markers",
                  transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
      
      # selected data
      scatterPlotSelected <- add_trace(scatterPlotSelected, 
                data = dataAnalyzerCurrentTibble[selectedRowsDT, , drop = F], 
                type = 'scatter', 
                mode = 'markers',
                x = ~solid_conc, 
                y = ~solution_id, 
                color = I('#B40000'), 
                name = 'Filtered',
                transforms = list(list(type='groupby',groups=selectedElement)))
      
    } else {
      print("uiPlotControl.R - Render scatterPlotUnselected")

        scatterPlotUnselected <- dataAnalyzerCurrentSharedData %>%
        plot_ly(x = ~solid_conc, 
                y = ~solution_id, 
                type = 'scatter', 
                color = I('black'), 
                name = 'Unfiltered',
                mode = "markers",
                transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
    
    }
  })
  
  # Renders histogram
  output$dataAnalyzerHist <- renderPlotly({
    print("uiPlotControl.R - renderPlotly(dataAnalyzerBox)")
    histData <- dataAnalyzerCurrentTibble[input$dataAnalyzerDT_rows_selected, ] # Stores the datatable rows which are selected

    if(length(input$dataAnalyzerDT_rows_selected)){ # Renders when there are selected rows
      print("uiPlotControl.R - Render boxSelected")
      histSelected <- histData %>%
        plot_ly(x=~solid_conc,
                type = "histogram",
                histnorm = "percent",
                marker=list(color=rep(I('#B40000'), 1000))
        )
    } else { # Renders when there aren't selected rows
      print("uiPlotControl.R - Render b")
      histUnselected <- dataAnalyzerCurrentTibble %>%
        plot_ly(x=~solid_conc,
                type = "histogram",
                histnorm = "percent",
                marker=list(color=rep(I("black"), 1000))
        )
    }
  })

  # Renders interactive violin plot
  output$dataAnalyzerViolin <- renderPlotly({

    violData <- dataAnalyzerCurrentTibble[input$dataAnalyzerDT_rows_selected, ]

    if(length(input$dataAnalyzerDT_rows_selected)){ # Renders when there are selected rows
      print("uiPlotControl.R - Render boxSelected")
      violSelected <- violData %>%
        plot_ly(x=~solid_conc,
                type = "violin",
                points="suspectedoutliers",
                jitter=0.2,
                pointpos=0,
                box=list(visible=TRUE),
                line=list(color="B40000"),
                meanline=list(visible=TRUE,color="black"),
                marker=list(color=rep(I('black'), 1000))
        )
    } else { # Renders when there aren't selected rows
      print("uiPlotControl.R - Render b")
      violUnselected <- dataAnalyzerCurrentTibble %>%
        plot_ly(x=~solid_conc,
                type = "violin",
                points = "suspectedoutliers",
                jitter=0.2,
                pointpos=0,
                box=list(visible=TRUE),
                line=list(color="black"),
                meanline=list(visible=TRUE,color="black"),
                marker=list(color=rep(I("black"), 1000))
        )
    }
  })

  # Renders summary statistics table
  output$dataAnalyzerStatsDT <- renderDT({
    
    if(length(input$dataAnalyzerDT_rows_selected)){
      dataTibble <- as.vector(dataAnalyzerCurrentTibble[input$dataAnalyzerDT_rows_selected,3])
    } else {
      dataTibble <- as.vector(dataAnalyzerCurrentTibble[,3])
    }
    dataAnalyzerStatsVector<- c(
              min(dataTibble),
              mean(dataTibble),
              median(dataTibble),
              max(dataTibble),
              sd(dataTibble)
              )
    dataAnalyzerStatsVector <- formatC(dataAnalyzerStatsVector,digits=3)
    names(dataAnalyzerStatsVector) <- c("min","mean","median","max","sd")
    dataAnalyzerSumStats <- rbind(dataAnalyzerStatsVector)
    dataAnalyzerDTStats <- DT::datatable(dataAnalyzerSumStats,options=list(dom='t'), rownames=FALSE)
    dataAnalyzerDTStats
  })
  
  # Pass current selection to save function
  output$dataAnalyzeSaveData <- renderPrint({
    # Use sampDatasetAlz object to save data saved from the Analyzer tab.
    session$userData$sampDatasetAlz$sampData <- dataAnalyzerCurrentTibble[input$dataAnalyzeDT_rows_selected, ] # Selection from data table using 'crosstalk' package.
  })
  
  observeEvent(dataAnalyzerCurrentSharedData$selection(),{
    print("uiPlotControl.R - observeEvent(selectRows(tabSelect)")
    updatePlotValues <- event_data("plotly_selected")
    tabSelect <- which(dataAnalyzerCurrentTibble$solution_id %in% updatePlotValues$y) # Retrieve indices of seleted rows
    dataTableProxy('dataAnalyzerDT') %>% selectRows(as.character(tabSelect))
  }) 
}
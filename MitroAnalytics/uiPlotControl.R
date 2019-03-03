library(dplyr)
library(ggplot2)
library(DT) # Chris: Added library to draw Datatable.
library(crosstalk)
library(shiny)
library(plotly)

# Function to output the plotly plot
# Can likely be split up more
drawDataCleaning <- function(input, output, session, data, selectedElement) {
  session$userData$elemSelected <- selectedElement

  # Initialize data values and reset selections
  observeEvent(selectedElement, {
    print("uiPlotControl.R - Initialize data")

    js$resetSelected()
    dataTableProxy('dataCleanDT') %>% selectRows(NULL)

    dataCleaningCurrentTibble <<- data %>% tibble::remove_rownames()
    dataCleaningCurrentSharedData <<- SharedData$new(dataCleaningCurrentTibble, ~solution_id)
  })

  # Renders Datatable
  output$dataCleanDT <- renderDT({ # Renders the datatable
    
    updateDataTable <- event_data("plotly_selected")

    print("uiPlotControl.R - Rendering datatableCleaning")

    datatableCleaning <- DT::datatable(dataCleaningCurrentTibble, option = list(pageLength = 20))
    datatableCleaning

  },server=FALSE)
  
  output$dataCleanScatter <- renderPlotly({
    
    selectedRowsDT <- input$dataCleanDT_rows_selected

    print("uiPlotControl.R - renderPlotly(dataCleanScatter)")
    
    if (length(selectedRowsDT)) {

      print("uiPlotControl.R - Render scatterPlotSelected")

        scatterPlotSelected <- dataCleaningCurrentTibble %>%
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
                data = dataCleaningCurrentTibble[selectedRowsDT, , drop = F], 
                type = 'scatter', 
                mode = 'markers',
                x = ~solid_conc, 
                y = ~solution_id, 
                color = I('#B40000'), 
                name = 'Filtered',
                transforms = list(list(type='groupby',groups=selectedElement)))
      
    } else {
      print("uiPlotControl.R - Render scatterPlotUnselected")

        scatterPlotUnselected <- dataCleaningCurrentSharedData %>%
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
  output$dataCleanBox <- renderPlotly({
    print("uiPlotControl.R - renderPlotly(dataCleanBox)")
    boxData <- dataCleaningCurrentTibble[input$dataCleanDT_rows_selected, ] # Stores the datatable rows which are selected

    if(length(input$dataCleanDT_rows_selected)){ # Renders when there are selected rows
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
      boxUnselected <- dataCleaningCurrentTibble %>%
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
  output$dataCleanSaveData <- renderPrint({
    fromTable <- dataCleaningCurrentTibble[input$dataCleanDT_rows_selected, ] # Selection from data table using 'crosstalk' package.
    session$userData$sampElem <- fromTable
  })
  
  observeEvent(dataCleaningCurrentSharedData$selection(),{
    print("uiPlotControl.R - observeEvent(selectRows(tabSelect)")
    updatePlotValues <- event_data("plotly_selected")
    tabSelect <- which(dataCleaningCurrentTibble$solution_id %in% updatePlotValues$y) # Retrieve indices of seleted rows
    dataTableProxy('dataCleanDT') %>% selectRows(as.character(tabSelect))
  }) 
}


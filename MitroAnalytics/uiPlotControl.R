library(dplyr)
library(ggplot2)
library(DT) # Chris: Added library to draw Datatable.
library(crosstalk)
library(shiny)
library(plotly)

# Function to output the plotly plot
# Can likely be split up more
drawPlotlyPlot <- function(input, output, session, data, selectedElement) {
  session$userData$elemSelected <- selectedElement

  # Initialize data values and reset selections
  observeEvent(selectedElement, {
    print("uiPlotControl.R - Initialize data")

    js$resetSelected()
    dataTableProxy('data1') %>% selectRows(NULL)

    dataCleaningCurrentTibble <<- data %>% tibble::remove_rownames()
    dataCleaningCurrentSharedData <<- SharedData$new(dataCleaningCurrentTibble, ~solution_id)
  })

  # Renders Datatable
  output$data1 <- renderDT({ # Renders the datatable
    
    updateDataTable <- event_data("plotly_selected")

    print("uiPlotControl.R - Rendering datatableCleaning")

    datatableCleaning <- DT::datatable(dataCleaningCurrentTibble, option = list(pageLength = 20))
    datatableCleaning

  },server=FALSE)
  
  output$plot1 <- renderPlotly({
    
    s <- input$data1_rows_selected # Maybe if I make s a reactive value it will update when rows are deselected?

    print("uiPlotControl.R - renderPlotly(plot1)")
    
    if (!length(s)) {
      print("uiPlotControl.R - Render p")
      p <- dataCleaningCurrentSharedData %>%
        plot_ly(x = ~solid_conc, 
                y = ~solution_id, 
                type = 'scatter', 
                color = I('black'), 
                name = 'Unfiltered',
                mode = "markers",
                transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
      
    } else if (length(s)) {
      print("uiPlotControl.R - Render pp")

      pp <- dataCleaningCurrentTibble %>%
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
      pp <- add_trace(pp, 
                      data = dataCleaningCurrentTibble[s, , drop = F], 
                      type = 'scatter', 
                      mode = 'markers',
                      x = ~solid_conc, 
                      y = ~solution_id, 
                      color = I('#B40000'), 
                      name = 'Filtered',
                      transforms = list(list(type='groupby',groups=selectedElement)))
    }
  })
  
  # Renders interactive boxplot
  output$plot2 <- renderPlotly({
    print("uiPlotControl.R - renderPlotly(plot2)")
    boxData <- dataCleaningCurrentTibble[input$data1_rows_selected, ] # Stores the datatable rows which are selected

    if(length(input$data1_rows_selected)){ # Renders when there are selected rows
      print("uiPlotControl.R - Render bb")
      bb <- boxData %>%
        plot_ly(x=~solid_conc,
                type = "box",
                boxpoints = "all",
                color = I("red"),
                jitter = 0.3,
                pointpos = -1.5,
                boxmean = TRUE
        )
    } else { # Renders when there aren't selected rows
      print("uiPlotControl.R - Render b")
      b <- dataCleaningCurrentTibble %>%
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
  output$crosstalk1 <- renderPrint({
    fromTable <- dataCleaningCurrentTibble[input$data1_rows_selected, ] # Selection from data table using 'crosstalk' package.
    session$userData$sampElem <- fromTable
  })
  
  observeEvent(dataCleaningCurrentSharedData$selection(),{
    print("uiPlotControl.R - observeEvent(selectRows(tabSelect)")
    updatePlotValues <- event_data("plotly_selected")
    tabSelect <- which(dataCleaningCurrentTibble$solution_id %in% updatePlotValues$y) # Retrieve indices of seleted rows
    dataTableProxy('data1') %>% selectRows(as.character(tabSelect))
  }) 
}


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
  #Interactive Object UI
  
  # Initializing interactive data variables
  
  m <- NULL
  d <- NULL
  s <- reactiveVal(NULL)
  
  observeEvent(selectedElement, {
    print("uiPlotControl.R - Initialize data")

    
    dataTableProxy('data1') %>% selectRows(NULL)

    m <<- data %>% tibble::remove_rownames()
    # print(m)
    d <<- SharedData$new(m, ~solution_id)
    js$resetSelected()
  })

  output$data1 <- renderDT({ # Renders the datatable
    
    updateDataTable <- event_data("plotly_selected")

    # print("Rendering dt")
    # print("Event data")
    # print(paste(updateDataTable$y))
    # print(which(m$solution_id %in% updateDataTable$y))
    # print(paste("Length of m is ", length(which(m$solution_id %in% updateDataTable$y))))
    dt <- DT::datatable(m, option = list(pageLength = 20))
    dt

  },server=FALSE)
  
  output$plot1 <- renderPlotly({
    
    s <- input$data1_rows_selected # Maybe if I make s a reactive value it will update when rows are deselected?

    print("uiPlotControl.R - renderPlotly(plot1)")
    
    if (!length(s)) {
      print("uiPlotControl.R - Render p")
      p <- d %>%
        plot_ly(x = ~solid_conc, 
                y = ~solution_id, 
                type = 'scatter', 
                color = I('black'), 
                name = 'Unfiltered',
                mode = "markers",
                source = "A",
                transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
      
    } else if (length(s)) {
      print("uiPlotControl.R - Render pp")

      pp <- m %>%
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
                      data = m[s, , drop = F], 
                      type = 'scatter', 
                      mode = 'markers',
                      x = ~solid_conc, 
                      y = ~solution_id, 
                      color = I('red'), 
                      name = 'Filtered',
                      transforms = list(list(type='groupby',groups=selectedElement)))
    }
  })
  
  # Renders interactive boxplot
  output$plot2 <- renderPlotly({
    print("uiPlotControl.R - renderPlotly(plot2)")
    boxData <- m[input$data1_rows_selected, ] # Stores the datatable rows which are selected
    # print(paste("Input data1: ", input$data1_rows_selected))

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
      b <- m %>%
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
  
  # Depricated: leave in as check condition in case of data selection problems
  output$p1Select <- renderPrint({
    #fromGraph <<- event_data("ploty_selected") # Selection from the graph using built-in Plotly 'event_data()' function.
    #print("From Graph:")
    #fromGraph
  })
  
  # Depricated: leave in as check condition in case of selection problems
  output$crosstalk1 <- renderPrint({
    fromTable <- m[input$data1_rows_selected, ] # Selection from data table using 'crosstalk' package.
    session$userData$sampElem <- fromTable
    #print("From Table:")
    #print(fromTable)
    #print("Selected Rows:")
    #input$data1_rows_selected
  })
  
  observeEvent(d$selection(),{
    print("uiPlotControl.R - observeEvent(selectRows(tabSelect)")
    updatePlotValues <- event_data("plotly_selected")
    tabSelect <- which(m$solution_id %in% updatePlotValues$y)
    # tabSelect <- which(d$selection()) # Returns rows for which selection is true
    # print("Tab Select:")
    # print(sort(as.numeric(tabSelect)))
    # print(tabSelect)
    dataTableProxy('data1') %>% selectRows(as.character(tabSelect))
  })
  
  #
  # TESTING: Does this do anything important?
  #
  observeEvent("plotly_selected",{ # Observes changes in plot selections
    print("Event plotly_selected")
    test <- event_data("plotly_selected")
    # print(test)
  })
  
  #
  # TESTING: Does this do anything important?
  #
  # observeEvent(ctr(),{ # Observes changes in input$elemChoice
  #   print("NULL selected rows by ctr() event")
  #   proxy %>% selectRows(NULL)
  # })
}


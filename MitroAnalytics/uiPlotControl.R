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
  #Interative Object UI
  
  # Initializing interactive data variables
  
  ctr <- reactiveVal(0) #Initializing reactive value to trigger datatable update when input$elemChoice is changed
  m <- reactiveVal(NULL)
  d <- reactiveVal(NULL)
  
  observeEvent(selectedElement, {
    m <<- data %>% tibble::remove_rownames()
    d <<- SharedData$new(m, ~solution_id)
    inc <- ctr() + 1
    ctr(inc)
  })
  
  output$plot1 <- renderPlotly({
    
    s <- input$data1_rows_selected
    
    if (!length(s)) {
      p <- d %>%
        plot_ly(x = ~solid_conc, 
                y = ~solution_id, 
                type = 'scatter', 
                color = I('black'), 
                name = 'Unfiltered',
                source = "a",
                transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
      
    } else if (length(s)) {
      pp <- d %>%
        plot_ly() %>% 
        add_trace(x = ~solid_conc, 
                  y = ~solution_id, 
                  type = "scatter", 
                  color = I('black'), 
                  name = 'Unfiltered',
                  source = "a",
                  transforms = list(list(type='groupby',groups=selectedElement))) %>%
        highlight(on = "plotly_selected",off="plotly_deselect") %>%
        layout(showlegend = T, dragmode = "select")
      
      # selected data
      pp <- add_trace(pp, 
                      data = m[s, , drop = F], 
                      type = 'scatter', 
                      x = ~solid_conc, 
                      y = ~solution_id, 
                      color = I('red'), 
                      name = 'Filtered',
                      transforms = list(list(type='groupby',groups=input$elemChoice))) %>%
        layout(showLegend = T)
    }
  })
  
  # Renders interactive boxplot
  output$plot2 <- renderPlotly({

    boxData <- m[input$data1_rows_selected, ] # Stores the datatable rows which are selected

    if(length(input$data1_rows_selected)){ # Renders when there are selected rows
      b <- boxData %>%
        plot_ly(x=~solid_conc,
                type = "box",
                boxpoints = "all",
                color = I("red"),
                jitter = 0.3,
                pointpos = -1.5,
                boxmean = TRUE
        )
    } else if (!length(input$data1_rows_selectedm)) { # Renders when there aren't selected rows
      bb <- d %>%
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
    #fromGraph <<- event_data("ploty_selected", source = "a") # Selection from the graph using built-in Plotly 'event_data()' function.
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
  
  output$data1 <- renderDT({ # Renders the datatable
    
    m2 <- m[d$selection(),]
    dt <- DT::datatable(m, option = list(pageLength = 18, rownames = FALSE))
    if (NROW(m2) == 0) {
      dt
    } else {
      DT::formatStyle(dt, "solution_id", target = "row",
                      color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                      backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
    }
  })
  
  proxy = dataTableProxy('data1') # Allows update of datatable after initial rendering
  
  observeEvent(d$selection(),{
    tabSelect <- which(d$selection()) # Returns rows for which selection is true
    print(tabSelect)
    proxy %>% selectRows(tabSelect)
  })
  
  observeEvent("plotly_selected",{ # Observes changes in plot selections
    test <- event_data("plotly_selected",source = "a")
    if (is.null(test)) {
      proxy %>% selectRows(NULL)
    }
  })
  
  observeEvent(ctr(),{ # Observes changes in input$elemChoice
    proxy %>% selectRows(NULL)
  })
}


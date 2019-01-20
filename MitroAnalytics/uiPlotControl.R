library(dplyr)
library(ggplot2)
library(DT) # Chris: Added library to draw Datatable.
library(crosstalk)
library(shiny)
library(plotly)

drawHistogram <- function(output, df) {
  output$barGraph <- renderPlot({
    ggplot(data = df, aes(factor(element_id), solid_conc, fill = label)) +
      geom_bar(stat = "identity", position = 'dodge') +
      scale_fill_brewer(palette = "Set1") +
      xlab("Element") +
      ylab("Concentration") +
      labs(fill = "Sample(s)") +
      coord_flip()
  })
}

drawInteractivePlot <- function(input, output, session, data, selectedElement) {
  ranges <- reactiveValues(x = NULL, y = NULL)
  print(data)
  output$statIntPlot1 <- renderPlot({
    ggplot(data, aes(x = selectedElement, y = solid_conc)) +
      stat_summary() +
      geom_dotplot(
        aes(colour = factor(element_id)),
        binaxis = 'y',
        stackdir = 'center',
        dotsize = 0.25
      ) +
      labs(x = "Element", y = "Solid Concentration (ppm)", colour = "Emission Wavelength")
  })

  output$statIntPlot2 <- renderPlot({
    ggplot(data, aes(x = selectedElement, y = solid_conc)) +
      stat_summary() +
      geom_dotplot(
        aes(colour = factor(element_id)),
        binaxis = 'y',
        stackdir = 'center',
        dotsize = 0.25
      ) +
      labs(x = "Element", y = "Solid Concentration (ppm)", colour = "Emission Wavelength") +
     coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    })

    output$statIntData1 <- DT::renderDataTable({
      datatable(data,
                rownames = FALSE,
                options = list(pageLength = 50))
    })

    output$statIntData2 <- DT::renderDataTable({
      datatable(
        brushedPoints(
          df = data,
          brush = input$plot1_brush,
          xvar = 'element_id',
          yvar = "solid_conc"),
        rownames = FALSE,
        options = list(pageLength = 50))
    })

    observe({
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        session$userData$sampElem <- brushedPoints(df = data, brush = brush, xvar = 'element_id', yvar = "solid_conc")
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
}

# Function to output the plotly plot
# Can likely be split up more
drawPlotlyPlot <- function(input, output, session, data, selectedElement2) {
  
  #Interative Object UI
  
  # Initializing interactive dataw variables
  
  m <- reactiveVal(NULL)
  d <- reactiveVal(NULL)
  
  observeEvent(selectedElement2, {
    m <<- data %>% tibble::remove_rownames()
    d <<- SharedData$new(m, ~solution_id)
  })
  
  output$plot1 <- renderPlotly({
    
    s <- input$data1_rows_selected
    
    if (!length(s)) {
      set.seed(1)      
      p <- d %>%
        plot_ly(x = ~solid_conc, y = ~element_id, type = 'box', boxpoints = 'all', pointpos = 2, color = I('black'), name = 'Unfiltered') %>%
        highlight(on = "plotly_selected") %>%
        layout(showlegend = T, boxgap = 0.5, dragmode = "select")
      
    } else if (length(s)) {
      set.seed(1)
      pp <- d %>%
        plot_ly() %>% 
        add_trace(x = ~solid_conc, y = ~element_id, type = 'box', boxpoints = 'all', pointpos = 2, color = I('black'), name = 'Unfiltered') %>%
        highlight(on = "plotly_selected") %>%
        layout(showlegend = T, boxgap = 0.5, dragmode = "select")
      
      # Red trace for selected data
      set.seed(1)
      pp <- add_trace(pp, data = m[s, , drop = F], type = 'box', boxpoints = 'all', pointpos = 2, x = ~solid_conc, y = ~element_id, color = I('red'), name = 'Filtered') %>%
        layout(showLegend = T, boxgap = 0.5)
    }
  })
  
  output$p1Select <- renderPrint({
    fromGraph <<- event_data("plotly_selected") # Selection from the graph using built-in Plotly 'event_data()' function.
    print("From Graph:")
    fromGraph
  })
  
  output$crosstalk1 <- renderPrint({
    fromTable <- m[input$data1_rows_selected, ] # Selection from data table using 'crosstalk' package.
    session$userData$sampElemPlotly <- fromTable
    print("From Table:")
    print(fromTable) 
    print("Selected Rows:")
    input$data1_rows_selected
  })
  
  output$data1 <- renderDT({
    
    update <- selectedElement2
    m2 <- m[d$selection(),]
    dt <- DT::datatable(m, option = list(pageLength = 50, rownames = FALSE))
    if (NROW(m2) == 0) {
      dt
    } else {
      DT::formatStyle(dt, "solution_id", target = "row",
                      color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                      backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
    }
  })
  
  proxy = dataTableProxy('data1')
  
  observeEvent(d$selection(),{
    #test <- event_data("plotly_selected")
    #tabSelect <- test[[2]] + 1
    #print(tabSelect)
    #print(input$data1_rows_selected)
    tabSelect <- which(d$selection())
    tabSelect <- rbind(tabSelect,input$data1_rows_selected)
    proxy %>% selectRows(tabSelect)
  })
  
  observeEvent("plotly_selected",{
    test <- event_data("plotly_selected")
    if (is.null(test)) {
      proxy %>% selectRows(NULL)
    }
  })
}

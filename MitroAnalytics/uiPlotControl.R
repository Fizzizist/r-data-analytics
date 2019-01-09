library(dplyr)
library(ggplot2)
library(DT) # Chris: Added library to draw Datatable.

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

drawInteractivePlot <- function(input, output, data, selectedElement) {
  ranges <- reactiveValues(x = NULL, y = NULL)
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
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
}
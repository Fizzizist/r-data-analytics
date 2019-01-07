library(ggplot2)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux

ui <- fluidPage(#fluidRow(),
  
  fluidRow(tagList(
    column(
      width = 5,
      height = 300,
      class = "well",
      fluidRow(column(
        width = 4,
        selectInput(
          "elemChoice",
          h4("Choose an element:"),
          choices = c(
            "Al",
            "As",
            "Ba",
            "Ca",
            "Cd",
            "Cu",
            "Co",
            "Cr",
            "Cu",
            "Fe",
            "K",
            "Mg",
            "Mn",
            "Mo",
            "Ni",
            "Pb",
            "Se",
            "Sr",
            "Y",
            "Zn"
          ),
          selected = "Zn"
        )
      ))
    ),
    
    column(
      width = 10,
      class = "well",
      h4("Left plot controls right plot"),
      fluidRow(column(
        width = 5,
        plotOutput(
          "plot1",
          height = 300,
          brush = brushOpts(id = "plot1_brush",
                            resetOnNew = TRUE)
        )
      ),
      column(width = 5,
             plotOutput("plot2", height = 300)))
    )
  )))

server <- function(input, output) { 
  #---------------------------------------------------------------------
  # Initialize samp.elems 
  if(!exists("samp.elem")) {
    samp.elem <- readRDS("samp.elem.rds")
  }
  
  # -------------------------------------------------------------------
  # Linked plots (left and right)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(samp.elem[[input$elemChoice]], aes(x = input$elemChoice, y = solid_conc)) +
      stat_summary() +
      geom_dotplot(
        aes(colour = factor(element_id)),
        binaxis = 'y',
        stackdir = 'center',
        dotsize = 0.25
      ) +
      labs(x = "Element", y = "Solid Concentration (ppm)", colour = "Emission Wavelength")
  })
  
  output$plot2 <- renderPlot({
    ggplot(samp.elem[[input$elemChoice]], aes(x = input$elemChoice, y = solid_conc)) +
      stat_summary() +
      geom_dotplot(
        aes(colour = factor(element_id)),
        binaxis = 'y',
        stackdir = 'center',
        dotsize = 0.25
      ) +
      labs(x = "Element", y = "Solid Concentration (ppm)", colour = "Emission Wavelength") +
      coord_cartesian(xlim = ranges$x,
                      ylim = ranges$y,
                      expand = FALSE)
  })
  
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
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

shinyApp(ui, server)

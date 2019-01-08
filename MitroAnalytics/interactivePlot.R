library(shiny)
library(shinyBS)
library(ggplot2)
library(DT)
library(Cairo) # For nicer ggplot2 output when deployed on Linux

ui <- fluidPage(

  fluidRow(
    column(
      width = 12,
      height = 300,
      class = "well",
      fluidRow(
        column(
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
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
        class = "well",
        h4("Left plot controls right plot"),
          fluidRow(
            column(
            width = 4,
            plotOutput(
              "plot1",
              height = 300,
              brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)
            )
          ),
          column(width = 4,
                 plotOutput("plot2", height = 300),
                 offset = 2
          )
        )
      )
    ),
  fluidRow(
    column(
      width = 12,
      class = "well",
      fluidRow(
        column(
          width = 4, ("Unedited Dataset"), DT::dataTableOutput("data1")
        ),
        column(
          width = 4, ("Edited Dataset"), DT::dataTableOutput("data2"), offset = 2
        ) #,
#column(width=2, "SE", tableOutput("test")
#)
      )
    )
  )
)

server <- function(input, output) {
  #---------------------------------------------------------------------
  # Initialize samp.elems 
  if (!exists("samp.elem")) {
    samp.elem <- readRDS("samp.elem.rds")
  }

  # -------------------------------------------------------------------
  # Linked plots (left and right)

  # Defining reactive values.
  ranges <- reactiveValues(x = NULL, y = NULL)

  # Interactive plot.
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

  # Reactive plot.
  output$plot2 <- renderPlot({
    ggplot(samp.elem[[input$elemChoice]], aes(x = input$elemChoice, y = solid_conc)) +
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
  ## Returns the actual dataset.
  output$data1 <- DT::renderDataTable({
    samp.elem[[input$elemChoice]]
  })

  ## Returns the updated dataset.
  output$data2 <- DT::renderDataTable({
    if (is.null(input$plot1_brush)) return("")
    else {
      brushedPoints(
        df = samp.elem[[input$elemChoice]],
        brush = input$plot1_brush,
        xvar = 'element_id',
        yvar = "solid_conc"
        )
    }
  })

  # Sets x and y co-ordinates for brush input.
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

# Run app.
shinyApp(ui, server)

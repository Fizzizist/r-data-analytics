library(shiny)
#library(shinyBS)
library(ggplot2)
library(DT)
library(Cairo) # For nicer ggplot2 output when deployed on Linux

ui <- fluidPage(fluidRow(column(
  width = 12,
  height = 300,
  class = "well",
  fluidRow(column(
    width = 2,
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
)),
fluidRow(column(
  width = 12,
  class = "well",
  h4("Left plot controls right plot"),
  fluidRow(
    column(
      width = 4,
      plotOutput(
        "plot1",
        height = 400,
        brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE),
        hover = hoverOpts(id = "plot1_hover")
      )
    ),
    column(
      width = 4,
      plotOutput("plot2", height = 400),
      hover = hoverOpts(id = "plot2_hover"),
      offset = 2
    )
  )
)),
fluidRow(column(
  width = 12,
  class = "well",
  fluidRow(
    column(width = 4, ("Original Data"), DT::dataTableOutput("data1")),
    column(
      width = 4,
      ("Selected Data"),
      DT::dataTableOutput("data2"),
      offset = 2
    )
  )
)))


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------


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

  # Defines the response to hovering near a point.
  output$hover_info1 <- renderPrint({
    if (!is.null(input$plot1_hover)) {
      hover = input$plot1_hover
      dist = sqrt((hover$x - mtcars$mpg) ^ 2 + (hover$y - mtcars$disp) ^ 2)
      cat("Weight (lb/1000)\n")
      if (min(dist) < 3)
        mtcars$wt[which.min(dist)]
    }
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

  ## Returns the original dataset.
  output$data1 <- DT::renderDataTable({
    samp.elem[[input$elemChoice]]
  })

  ## Returns the selected dataset.
  output$data2 <- DT::renderDataTable({
    if (is.null(input$plot1_brush))
      return()
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
shinyApp(ui = ui, server = server)

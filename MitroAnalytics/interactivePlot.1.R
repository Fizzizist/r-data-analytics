library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(Cairo) # For nicer ggplot2 output when deployed on Linux

ui <- fluidPage(
  fluidRow(column(width = 2),
           column(
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
             
           )),
  fluidRow(
    column(width = 2),
    column(width = 8,
           plotlyOutput("plot1")),
    column(width = 2)
  ),
  
  fluidRow(column(width = 2),
           column(width = 8,
                  DTOutput("data1")),
           column(width = 2))
)

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------


server <- function(input, output) {
  #---------------------------------------------------------------------
  # Initialize samp.elems
  if (!exists("samp.elem")) {
    samp.elem <- readRDS("samp.elem.rds")
  }
  
  # -------------------------------------------------------------------
  # Interative Object UI
  
  # Interactive plot.
  output$plot1 <- renderPlotly({
    data <- samp.elem[[input$elemChoice]] 
    
    plot <- ggplot(data, aes(x = input$elemChoice, y = solid_conc)) +
      geom_jitter(width = 0.01, height = 0)
    
    ggplotly(plot) %>%
      group_by(element_id) %>%
      layout(showlegend = T) 
  })
  
  ## Interactive
  output$data1 <- renderDT({
    datatable(samp.elem[[input$elemChoice]],
              rownames = FALSE,
              options = list(pageLength = 50))
  })
}

# Run app.
shinyApp(ui, server)

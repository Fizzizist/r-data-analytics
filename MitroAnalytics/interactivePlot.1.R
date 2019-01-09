library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(crosstalk)

source("stats.R")

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
           #("tib"),
           plotlyOutput("plot1")),
    column(width = 2)
  ),
  
  fluidRow(
    column(width = 2),
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
    samp.elem <- getPTValues()
  }
  
  # -------------------------------------------------------------------
  # Interative Object UI
  
  # Highlight selected rows in the scatterplot
  
  output$plot1 <- renderPlotly({
    
    data <- samp.elem[[input$elemChoice]] 
    m <- data %>%
          tibble::rownames_to_column()
    
  #output$tib <- renderPrint({
  #    paste(m)
  #  })

    d <- SharedData$new(m, ~rowname)     
    
    s <- input$data_rows_selected
    
    if(!length(s)) {

      p <- m %>%
        plot_ly(x = ~solid_conc, y = ~element_id, type= "box", jitter = 0.3, pointpos = 1.8, color = ~element_id, boxpoints = "all",name = "Filtered") %>%
        layout(boxgap = 0.5)
    }else if (length(s)) {
      pp <- m %>%
        ggploty(plot) %>% 
        add_trace(x = ~input$elemChoice, y = ~solid_conc, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
        layout(showlegend = T)
      
      #selected data
      pp <- add_trace(pp, data = m[s, , drop = F], x = ~input$elemChoice, y = ~solid_conc, mode = "markers",
                      color = I('red'), name = 'Filtered')
    }
    
  })
    output$data1 <- renderDT({
    m2 <- m[data$selection(),]
    dt <- DT::datatable(m)
    if (NROW(m2) == 0) {
      dt
    } else {
      DT::formatStyle(dt, "rowname", target = "row",
                      color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                      backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
    }
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

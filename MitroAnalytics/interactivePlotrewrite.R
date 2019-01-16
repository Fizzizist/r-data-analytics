library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(crosstalk)

source("stats.R")

if (!exists("samp.elem")) {
  samp.elem <- getPTValues()
}

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
                 "Zn"
               ),
               selected = "Zn"
             )
             
           )),
  fluidRow(
           plotlyOutput("plot1")#,
           #verbatimTextOutput("p1Select")
  ),
  
  fluidRow(
    column(width = 2),
    column(width = 8,
           verbatimTextOutput("crosstalk1"),
           DTOutput("data1")),
    column(width = 2))
)


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

server <- function(input, output) {
  
  # -------------------------------------------------------------------
  # Interative Object UI
  
  # Highlight selected rows in the scatterplot
  
  m <- reactiveVal(NULL)
  d <- reactiveVal(NULL)
  
  observeEvent(input$elemChoice, {
    m <<- samp.elem[[input$elemChoice]] %>% tibble::remove_rownames()
    d <<- SharedData$new(m, ~solution_id)
    
  })
  
  # Drawing the plot and tracing the selected points.
  output$plot1 <- renderPlotly({
    
    s <- input$data1_rows_selected
    
    if (!length(s)) {
      set.seed(1)      
      p <- d %>%
        plot_ly(y = ~solid_conc, x = ~input$elemChoice, type = 'box', boxpoints = 'all', pointpos = 2, color = I('black'), name = 'Unfiltered', source = "p1_select") %>%
        highlight(on = "plotly_selected") %>%
        layout(showlegend = T, boxgap = 0.5, dragmode = "select")
      
    } else if (length(s)) {
      set.seed(1)
      pp <- d %>%
        plot_ly() %>% 
        add_trace(y = ~solid_conc, x = ~input$elemChoice, type = 'box', boxpoints = 'all', pointpos = 2, color = I('black'), name = 'Unfiltered') %>%
        highlight(on = "plotly_selected") %>%
        layout(showlegend = T, boxgap = 0.5, dragmode = "select")
      
      subplot(
        pp,
        select <- plot_ly(data = m[s, , drop = F], type = 'box', boxpoints = 'all', pointpos = 2, y = ~solid_conc, x = ~input$elemChoice, color = I('red'), name = 'Filtered') %>%
          highlight(on= "plotly_selected")

      )  %>% layout(showLegend = T, boxgap = 0.5) 


    }
  })
  
  #### Examining Selected Data
  
  #output$p1Select <- renderPrint({
  #  fromGraph <- event_data("plotly_selected") # Selection from the graph using built-in Plotly 'event_data()' function.
  #  print("From Graph:")
  #  fromGraph
  #})
  
  output$crosstalk1 <- renderPrint({
    fromTable <- m[input$data1_rows_selected, ] # Selection from data table using 'crosstalk' package.
    print("From Table:")
    print(fromTable) 
    print("Selected Rows:")
    input$data1_rows_selected
  })
  
  output$data1 <- renderDT({
    m2 <- m[d$selection(),]
    dt <- DT::datatable(m, option = list(pageLength = 50, rownames = FALSE)) # %>% DT::formatStyle('solution_id', target = 'row', valueColumns = , backgroundColor = 'red')
  })
  
  
  #### Event Monitoring
  
  proxy = dataTableProxy('data1')
  
  observeEvent(d$selection(),{
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

shinyApp(ui, server)
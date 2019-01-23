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
    column(width = 2),
    column(width = 8,
           plotlyOutput("plot1"),
           verbatimTextOutput("p1Select")),
    
    column(width = 2)
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
  
  ctr <- reactiveVal(0)
  m <- reactiveVal(NULL)
  d <- reactiveVal(NULL)
  
  observeEvent(input$elemChoice, {
    m <<- samp.elem[[input$elemChoice]] %>% tibble::remove_rownames()
    d <<- SharedData$new(m, ~solution_id)
    inc <- ctr() + 1
    ctr(inc)
  })
  
  output$plot1 <- renderPlotly({
    
    #update <- input$elemChoice
    
    s <- input$data1_rows_selected
    
    if (!length(s)) {
      set.seed(1)      
      p <- d %>%
        plot_ly(x = ~solid_conc, 
          y = ~solution_id, 
          type = 'scatter', 
          color = I('black'), 
          name = 'Unfiltered', 
          source=ctr(), 
          transforms = list(list(type='groupby',groups=input$elemChoice))) %>%
        highlight(on = "plotly_selected") %>%
        layout(showlegend = T, dragmode = "select")
      
    } else if (length(s)) {
      set.seed(1)
      pp <- d %>%
        plot_ly() %>% 
        add_trace(x = ~solid_conc, 
          y = ~solution_id, 
          type = "scatter", 
          color = I('black'), 
          name = 'Unfiltered',
          source = ctr(),
          transforms = list(list(type='groupby',groups=input$elemChoice))) %>%
        highlight(on = "plotly_selected") %>%
        layout(showlegend = T, dragmode = "select")
      
      # selected data
      set.seed(1)
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
  
  output$p1Select <- renderPrint({
    fromGraph <<- event_data("plotly_selected",source=ctr()) # Selection from the graph using built-in Plotly 'event_data()' function.
    print("From Graph:")
    fromGraph
  })
  
  output$crosstalk1 <- renderPrint({
    fromTable <- m[input$data1_rows_selected, ] # Selection from data table using 'crosstalk' package.
    print("From Table:")
    print(fromTable) 
    print("Selected Rows:")
    input$data1_rows_selected
  })
  
  output$data1 <- renderDT({
    
    m <- samp.elem[[input$elemChoice]] %>% tibble::remove_rownames()
    
    m2 <- m[d$selection(),]
    dt <- DT::datatable(d, option = list(pageLength = 50, rownames = FALSE))
    if (NROW(m2) == 0) {
      dt
    } else {
      DT::formatStyle(dt, "solution_id", target = "row",
                      color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                      backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
    }
  },server=FALSE)
  
  proxy = dataTableProxy('data1')
  
  observeEvent(d$selection(),{
    #test <- event_data("plotly_selected")
    #tabSelect <- test[[2]] + 1
    #print(tabSelect)
    #print(input$data1_rows_selected)
    tabSelect <- which(d$selection())
    #tabSelect <- rbind(tabSelect,input$data1_rows_selected)
    proxy %>% selectRows(tabSelect)
  })
  
  observeEvent("plotly_selected",{
    test <- event_data("plotly_selected")
    if (is.null(test)) {
      proxy %>% selectRows(NULL)
    }
  })
  
  observeEvent(ctr(),{
    proxy %>% selectRows(NULL)
  })
}

shinyApp(ui, server)
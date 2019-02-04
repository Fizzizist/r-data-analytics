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
  
  fluidRow(
    column(width=5,
           # Save buttons here
           DTOutput("data1")#,
           #verbatimTextOutput("crosstalk1")
    ),
    column(width = 7,
           selectInput(
             "elemChoice",
             h4("Choose an element:"),
             choices = c(
               "Ca",
               "Cu",
               "Fe",
               "K",
               "Mg",
               "Se",
               "Zn"),
             selected = "Zn"),
           plotlyOutput("plot1"),
           plotlyOutput("plot2")#,
           #verbatimTextOutput("p1Select")
    )
  )
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
  
  output$plot2 <- renderPlotly({
    
    boxData <- m[input$data1_rows_selected, ]
    
    if(length(input$data1_rows_selected)){
      b <- boxData %>%
        plot_ly(x=~solid_conc,
                type = "box",
                boxpoints = "all",
                color = I("red"),
                jitter = 0.3,
                pointpos = -1.5,
                boxmean = TRUE
        )
    } else {
      b <- d %>% 
        plot_ly() %>%
        add_trace(x=~solid_conc,
                  type = "box",
                  color = I('black'),
                  boxpoints = "all",
                  jitter = 0.3,
                  pointpos = -1.5,
                  boxmean = TRUE
        )
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
    tabSelect <- which(d$selection())
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
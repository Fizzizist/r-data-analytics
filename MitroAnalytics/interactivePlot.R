#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Element Loss Dot-plot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      # Sidebar layout for inputs ----
      sidebarPanel(
        
        # Input: Selector for choosing dataset ----
        # Copy the line below to make a select box 

        selectInput("elemChoice", h3("Select an element:"), 
                    choices = c("Al","As","Ba","Ca","Cd",
                                "Cu","Co","Cr","Cu","Fe",
                                "K","Mg","Mn","Mo","Ni",
                                "Pb","Se","Sr","Y","Zn"),
                    selected = "Zn")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        verbatimTextOutput("summary"),
        tableOutput("view"),
        plotOutput("dotPlot")

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #### Function to draw plot. 
  drawDotplot <- function(i="Zn"){
    
    ##### Creating samp.elem if it doesn't exist.
    if(!exists("samp.elem")) {
        samp.elem <- readRDS("samp.elem.rds")
    else { break }
    
    #### samp.elem already exists
    if(dim(samp.elem[[elem]]) %in% c(0,NULL)){
      output$dotPlot <- renderText({
        paste('No statistically significant data points for ',i,'!\n')
    }else{
      output$dotPlot <- renderPlot({
        ggplot(samp.elem[[elem]], aes(x=i, y=solid_conc)) +
          stat_summary(data = samp.elem[[i]]) +
          geom_dotplot(aes(colour=factor(element_id)),binaxis='y',stackdir='center', dotsize = 0.25) +
          labs(x="Element",y="Solid Concentration (ppm)",colour="Emission Wavelength")
        })
      })
    }
  }
}
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    elem <- datasetInput()
    summary(elemChoice)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = 1000)
  })
  
  i = input$elemChoice
  drawDotplot(i)
  
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(shinydashboard)
library(shinyjs)

source("uiAuthControl.R")

jsCode <- getUIAuthJS()

header <- dashboardHeader()
header$children[[2]]$children <- tags$div(class='header-title', 
                                          tags$img(class='header-logo', src='favicon.ico', width='50px'),
                                          "MitroAnalytics")

ui <- dashboardPage(
  skin="black",
  title="MitroAnalytics",
  header,
  dashboardSidebar(
    width=150,
    sidebarMenu(id="sidebarMenu",
      menuItem("Home", tabName='home', icon=icon('home')),
      menuItem("File Upload", tabName='fileUpload', icon=icon('file-upload')),
      menuItem("Histogram", tabName='statHistogram', icon=icon('chart-bar')),
      menuItem("Interactive Plot", tabName='statIntPlot', icon=icon('microscope')), # 1. Add menu item.
	    menuItem("File Download", tabName='fileDownload', icon=icon('download')),
	    uiOutput("logout")
    )
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("hideMenu")),
    tags$head(
      tags$title("MitroAnalytics"),
      tags$link(rel="shortcut icon", type="", href="favicon.ico"),
      tags$link(rel="stylesheet", type="text/css", href="style.css")
    ),
    tabItems(
      tabItem(tabName='home',
        fluidRow(
          tags$div(class='center',
                   div(id='home-title', img(class='header-logo', src='favicon.ico', width='100px'), "MitroAnalytics"),
                   uiOutput('authentication')
          )
        )
      ),
      tabItem(tabName='fileUpload',
        fluidRow(
          column(width=3,
            h2('Upload ICP Data')
          )
        ),
        fluidRow(
          tags$div(class='center',
            fileInput("file1", "", multiple=FALSE, accept=c("text/csv","text/comma-separated-values","text/plain",".csv"), width='100%'),
		        verbatimTextOutput("uploaded")
          )
        )
      ),
      tabItem(tabName='statHistogram',
        fluidRow(
          column(width=2,
            h1('Histogram')
          ),
          column(width=3,
            uiOutput("selectSession")
          )
        ),
        fluidRow(
          column(width=2,
            uiOutput("solutionCheckboxes")
          ),
          column(width=2,
            uiOutput("elementCheckboxes")
          ),
          column(width=8,
            plotOutput("barGraph")
          )
        ),
        fluidRow(
          column(width=2,
            actionButton("btnReset", "Reset", class = "btn-warning")
          ),
          column(width=2,
            actionButton("btnBuild", "Build", class = "btn-primary")
          )
        )
      ),
      # 2. Add tabItem, organize output
      tabItem(tabName='statIntPlot',
        fluidRow(
          column(width=3,
            h1('Interactive Plot')
          ),
          column(width=3,
            uiOutput('selectIntElement')
          )
        ),
        fluidRow(
          column(width = 5,
            plotOutput("statIntPlot1", height = 300, brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE))
          ),
          column(width = 5,
            plotOutput("statIntPlot2", height = 300)
          )
        )
      ),
	    tabItem(tabName='fileDownload',
        fluidRow(
          column(width=3,
            h2('Download files')
          )
        ),
        fluidRow(
          tags$div(class='center',
           #Downloader
      		selectInput("tableChecks", "Choose a dataset:",
      			c("Solutions" = "solutions",
      			"Elements per solution" = "solution_elements",
      			"replicates" = "replicates")),
      		uiOutput("sessionChecks"),
      		selectInput("dlFormat", "Choose a format:",
      			c("CSV" = "csv",
      			"Excel" = "xlsx")),
      		  uiOutput("dlButton") 
          )
        )
      )
    )
  )
)

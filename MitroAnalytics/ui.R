library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT) # Chris: Added library to draw Datatable.
library(plotly)

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
	    menuItem("File Download", tabName='fileDownload', icon=icon('download')),
      menuItem("Plotly Plot", tabName="statPlotly", icon=icon('chart-line')),
	    uiOutput("logout")
    )
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("hideMenu")),
    tags$head(
      tags$title("MitroAnalytics"),
      tags$link(rel="shortcut icon", type="", href="favicon.ico"),
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      tags$script(src="getIP.js"),
      tags$script(src="onClose.js")
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
      # Plotly Plot Tab
      tabItem(tabName="statPlotly",
        fluidRow(
          # UI Elements (Reactive Values)
          column(width=3,
                 h1('Plotly Plot')
          ),
          column(width=3,
                uiOutput('selectPlotlyPlotBurn')
          ),
          column(width=3,
                 uiOutput('selectPlotlyPlotElement')
          ),
          column(width=1,
                actionButton("btnPlotlySave", "Save")
          ),
          column(width=1,
                actionButton("btnPlotlyLoad", "Load")
          ),
          column(width=1,
                actionButton("btnPlotlyReset", "Reset")
          )
        ),
        # Output to Browswer (~Reactive Observers)
        fluidRow(
          column(width=5,
                 DTOutput("data1"),
                 verbatimTextOutput("crosstalk1")
          ),
          column(width = 7,
                 plotlyOutput("plot1"),
                 plotlyOutput("plot2"),
                 verbatimTextOutput("p1Select")
          )),
                    column(width = 2)
          ),
      # File Download Tab
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
                         uiOutput("burnChecks"),
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


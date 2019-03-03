library(shiny)
library(shinydashboard)
library(shinyjs)
library(V8)
library(DT)
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
      menuItem("Data Cleaning", tabName="statDataCleaning", icon=icon('chart-line')),
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
      # DataCleaning Plot Tab
      tabItem(tabName="statDataCleaning",
        fluidRow(
          # UI Elements (Reactive Values)
          column(width=3,
                 h1('Data Cleaning')
          ),
          column(width=3,
                uiOutput('selectDataCleaningBurn')
          ),
          column(width=3,
                 uiOutput('selectDataCleaningElement')
          ),
          column(width=1,
                actionButton("btnDataCleaningSave", "Save")
          ),
          column(width=1,
                actionButton("btnDataCleaningLoad", "Load")
          ),
          column(width=1,
                actionButton("btnDataCleaningReset", "Reset")
          )
        ),
        # Output to Browswer (~Reactive Observers)
        fluidRow(
          column(width=5,
          useShinyjs(),
          # code to reset plotlys event_data("plotly_click", source="A") to NULL -> executed upon action button click
          # note that "A" needs to be replaced with plotly source string if used
          extendShinyjs(text = "shinyjs.resetSelected = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); }"),
          tags$style(HTML("table.dataTable tbody tr.selected td, table.dataTable td.selected{background-color:#B40000 !important;}")),
          DTOutput("dataCleanDT"),
          verbatimTextOutput("dataCleanSaveData")
          ),
          column(width = 7,
                 plotlyOutput("dataCleanScatter"),
                 plotlyOutput("dataCleanBox")
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
                         selectInput("dlFormat", "Choose a format:",
                                     c("CSV" = "csv",
                                       "Excel" = "xlsx")),
                         uiOutput("dlButton"),
                         verbatimTextOutput("dlOut") 
                )
              )
            )
        )
    )
)


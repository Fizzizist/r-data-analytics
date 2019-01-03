library(shiny)
library(shinydashboard)

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
      menuItem("Histogram", tabName='statHistogram', icon=icon('chart-bar'))
    )
  ),
  dashboardBody(
    tags$head(
      tags$title("MitroAnalytics"),
      tags$link(rel="shortcut icon", type="", href="favicon.ico"),
      tags$link(rel="stylesheet", type="text/css", href="style.css")
    ),
    tabItems(
      tabItem(tabName='home',
        fluidRow(
          column(width=10,
            h1('Welcome to MitroAnalytics!')
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
            fileInput("fileUpload", "", multiple=FALSE, accept=c("text/csv"), width='100%')
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
      )
    )
  )
)
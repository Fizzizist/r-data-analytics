library(shiny)
library(shinydashboard)
library(shinyjs)
library(V8)
library(DT)
library(plotly)
library(shinyBS)

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
      menuItem("Data Cleaner", tabName="statDataCleaner", icon=icon('chart-line')),
      menuItem("Data Explorer", tabName="statDataExplorer", icon=icon('chart-line')),
      menuItem("Data Analyzer", tabName="statDataAnalyzer", icon=icon('chart-line')),
      menuItem("User Settings", tabName="userSettings", icon=icon('user')),
      hidden(
        menuItem("Manage Users", tabName="userManage", icon=icon('users-cog'))
      ),
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
      # DataCleaner Plot Tab
      tabItem(tabName="statDataCleaner",
        fluidRow(
          # UI Elements (Reactive Values)
          column(width=3,
                 h1('Data Cleaner')
          ),
          column(width=3,
                uiOutput('selectDataCleanerBurn')
          ),
          column(width=3,
                 uiOutput('selectDataCleanerElement')
          ),
          column(width=1,
                actionButton("btnDataCleanerSave", "Save")
          ),
          column(width=1,
                actionButton("btnDataCleanerLoad", "Load")
          )
        ),
        # Output to Browswer (~Reactive Observers)
        fluidRow(
          column(width=3,
          useShinyjs(),
          # code to reset plotlys event_data("plotly_click", source="A") to NULL -> executed upon action button click
          # note that "A" needs to be replaced with plotly source string if used
          extendShinyjs(text = "shinyjs.resetSelected = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); }"),
          tags$style(HTML("table.dataTable tbody tr.selected td, table.dataTable td.selected{background-color:#B40000 !important;}")),
          DTOutput("dataCleanerDT"),
          verbatimTextOutput("dataCleanerSaveData")
          ),
          column(width = 9,
                 plotlyOutput("dataCleanerScatter"),
                 plotlyOutput("dataCleanerBox")
          )),
          column(width = 2)
          ),
      # Data Exploration Tab
      tabItem(tabName="statDataExplorer",
        fluidRow(
          # UI Elements (Reactive Values)
          column(width=3,
                 h1('Data Explorer')
          ),
          column(width=3,
                uiOutput('selectDataExplorerBurn')
          ),
          column(width=3,
                 uiOutput('selectDataExplorerElement')
          ),
          column(width=1,
                actionButton("btnDataExplorerSave", "Save")
          ),
          column(width=1,
                actionButton("btnDataExplorerLoad", "Load")
          )
        ),
        # Output to Browswer (~Reactive Observers)
        fluidRow(
          column(width=3,
            useShinyjs(),
            # code to reset plotlys event_data("plotly_click", source="A") to NULL -> executed upon action button click
            # note that "A" needs to be replaced with plotly source string if used
            extendShinyjs(text = "shinyjs.resetSelected = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); }"),
            tags$style(HTML("table.dataTable tbody tr.selected td, table.dataTable td.selected{background-color:#B40000 !important;}")),
            DTOutput("dataExplorerDT"),
            verbatimTextOutput("dataExplorerSaveData")
          ),
          column(width=9,
            fluidRow(
              column(width=6,
                DTOutput("dataExplorerStatsDT")
              ), 
              column(width=6)
            ),
            column(width = 6,
              plotlyOutput("dataExplorerScatter")
            ),
            column(width = 6,
              plotlyOutput("dataExplorerHist"),
              plotlyOutput("dataExplorerViolin")
            )
        )#,
          #column(width = 2)
      )
      ),
      tabItem(tabName="statDataAnalyzer",
        fluidRow(
          # UI Elements (Reactive Values)
          column(width=3,
                 h1('Data Analyzer')
          ),
          column(width=3,
                uiOutput('selectDataAnalyzerBurn')
          ),
          column(width=3,
                 uiOutput('selectDataAnalyzerElement')
          ),
          column(width=1,
                actionButton("btnDataAnalyzerSave", "Save")
          ),
          column(width=1,
                actionButton("btnDataAnalyzerLoad", "Load")
          )
        ),
        # Output to Browswer (~Reactive Observers)
        fluidRow(
          column(width=3,
            useShinyjs(),
            # code to reset plotlys event_data("plotly_click", source="A") to NULL -> executed upon action button click
            # note that "A" needs to be replaced with plotly source string if used
            extendShinyjs(text = "shinyjs.resetSelected = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); }"),
            tags$style(HTML("table.dataTable tbody tr.selected td, table.dataTable td.selected{background-color:#B40000 !important;}")),
            DTOutput("dataAnalyzerDT"),
            verbatimTextOutput("dataAnalyzerSaveData")
          ),
          column(width=9,
            fluidRow(
              column(width=6,
                DTOutput("dataAnalyzerStatsDT")
              ), 
              column(width=6)
            ),
            column(width = 6,
              plotlyOutput("dataAnalyzerScatter")
            ),
            column(width = 6,
              plotlyOutput("dataAnalyzerHist"),
              plotlyOutput("dataAnalyzerViolin")
            )
          )#,
          # column(width = 2)
        )
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
                    uiOutput("dlMenu"),
                    selectInput("dlFormat", "Choose a format:",
                                c("CSV" = "csv",
                                  "Excel" = "xlsx")),
                    uiOutput("dlButton"),
                    verbatimTextOutput("dlOut") 
          )
        )
      ),
      #' User settings tab for changing name and password
      tabItem(tabName='userSettings',
        fluidRow(
          column(width=3,
                  h2('User Settings')
          )
        ),
        fluidRow(
          tags$div(class='center',
            bsButton("changePass", "Change Password"),
            uiOutput("screenMessageUserSettings"),
            bsModal("changePassForm", "Change Password","changePass",
              passwordInput("oldPassword","Old Password:"),
              passwordInput("newUserPassword","New Password:"),
              passwordInput("newRepeatedUserPassword","Repeat new password:"),
              actionButton("submitChangePass", "Submit")
            )
          )
        )
      ),
      #' Manage users tab for admin only
      tabItem(tabName='userManage',
        fluidRow(
          column(width=3,
                  h2('Manage User Accounts')
          )
        ),
        fluidRow(
          tags$div(class='center',  
            uiOutput("userListMenu"),
            bsButton("delete", "Delete Account"),
            bsButton("addUser", "Add new user"),
            uiOutput("screenMessage"),
            bsModal("addAccountForm", "Add User Account","addUser",
              textInput("newUsername","Username:"),
              passwordInput("newPassword","Password:"),
              passwordInput("newRepeatedPassword","Repeat password:"),
              checkboxInput("adminStatus", "Administrator Account"),
              actionButton("submitNewUser", "Submit")
            ),
            bsModal("deleteAlert", "Are you sure you want to delete this account?", "delete",
              actionButton("yesDelete", "Yes")
            )
          )
        )
      ) 
    )
  )
)


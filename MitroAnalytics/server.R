library(shiny)
library(xlsx)

source("uiControl.R")
source("io.R")
source("stats.R")

server <- function(input, output, session){
  #renders most of ui
  loadUI(input, output, session)
	
	#runs the insert function from io.R and outputs success or error statements to the user
        output$uploaded <- renderText({
                f1 <- input$file1

                if (is.null(f1))
                        return(NULL)

                insertCSV(f1)

                return("Data successfully inserted into the database!")
        })

	sessList <- getBurnList()[,1]
        #download tab checkboxes
        output$burnChecks <- renderUI({
                checkboxGroupInput("burns", "Choose burn:",
                        choiceNames = as.character(sessList),
                        choiceValues = as.character(sessList))
        })	

	#render the download button upon the correct conditions
        output$dlButton <- renderUI({
                req(input$burns)
                downloadButton("downloadData", "Download")
        })


        #download handler for download tab - allows for csv/excel optionality
        output$downloadData <- downloadHandler(
                filename = function() {
                        if(input$dlFormat == "csv"){
                                paste0("dataset", Sys.Date(),".csv")
                        } else if(input$dlFormat == "xlsx") {
                                paste0("dataset", Sys.Date(),".xlsx")
                        }
                },
                content = function(file){
                        dataset <- getDownloadData(input$burns, input$tableChecks)
                        if(input$dlFormat == "csv") {
                                write.csv(dataset, file)
                        } else if(input$dlFormat == "xlsx") {
                                write.xlsx(dataset, file)
                        }
                }
        )
}

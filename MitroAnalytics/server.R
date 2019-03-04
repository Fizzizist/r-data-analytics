library(shiny)
library(xlsx)

source("uiControl.R")
source("io.R")
source("stats.R")

server <- function(input, output, session){
        #renders most of ui
        loadUI(input, output, session)
	
		# runs the insert function from io.R and outputs success or error statements 
        # to the user
        output$uploaded <- renderText({
                f1 <- input$file1

                if (is.null(f1))
                        return(NULL)

                insertCSV(f1)

                return("Data successfully inserted into the database!")
        })	

		#render the download button upon the correct conditions
        output$dlButton <- renderUI({
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
                        #print(session$userData$sampElem$solution_id)
                        dataset <- getDownloadData(session$userData$solVec)
						print(dataset)
						if(is.null(dataset)){
							output$dlOut <- renderPrint("You need to save data before you can download it.")
						} else if(input$dlFormat == "csv") {
								write.csv(dataset, file)
						} else if(input$dlFormat == "xlsx") {
								write.xlsx(dataset, file)
						}
                }
        )

        #collect usage data and push to www/userlog.log file
        IP <- reactive({input$getIP})
        observe({
                #cat(capture.output(str(IP()), split=TRUE))
                logData(capture.output(str(IP()), split=TRUE))
        })
}
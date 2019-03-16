library(shiny)
library(xlsx)
library(shinyBS)

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

        output$dlMenu <- renderUI({
                dataList <- getSavedDatasetList(session$userData$username)
                selectInput("dlDataset", "Choose a dataset:", dataList)
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
                        savedData <- getSavedDataset(session$userData$username,input$dlDataset)
                        print(savedData)
                        solVec <- savedData$sampData$solution_id
                        elVec <- savedData$sampData$element_id
                        print(solVec)
                        print(elVec)
                        dataset <- getDownloadData(solVec, elVec)
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

        #' Select user menu for 'Manage user' tab
        output$userListMenu <- renderUI({
                userList <- getUserList()
                selectInput("userListSelect", "Select a user:", userList)
        })

        #' Event the 'Delete Account' button in the 'Manage User' tab
        observeEvent(input$yesDelete,{
                deletedUser <- input$userListSelect
                removeUser(deletedUser)
                output$screenMessage <- renderText(paste0(deletedUser, " deleted Successfully."))
                toggleModal(session,"deleteAlert",toggle="close")
                updateSelectInput(session,"userListSelect",choices=getUserList())
        })

        #' Event for 'Submit' button of the 'Add New User' Form 
        #' in the 'Manage Users' tab.
        observeEvent(input$submitNewUser,{
                output$screenMessage <- renderText(
                        createUser(input$newUsername, input$newPassword,
                                input$newRepeatedPassword, input$adminStatus)
                )
                toggleModal(session,"addAccountForm",toggle="close")
                updateSelectInput(session,"userListSelect",choices=getUserList())
        })

        #' Event for 'Submit' button of the 'Change Password' window in 'User Settings'
        observeEvent(input$submitChangePass, {
                output$screenMessageUserSettings <- renderText(
                        changePass(session$userData$username, input$oldPassword,
                                input$newUserPassword, input$newRepeatedUserPassword)
                )
                toggleModal(session,"changePassForm",toggle="close")
        })
}
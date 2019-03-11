#' Script for storing all IO functions reading data to and from the DBMS
#'
#' @authors Peter Vlasveld, Christopher Eeles and Minoru Nakano
library(DBI)
library(anytime)
library(openssl)

source("queries.R")

#' function for establishing MySQL connection.
#'
#' @return the connection to the DBMS
getConnect <- function () {
        conn <- DBI::dbConnect(
                drv = RMySQL::MySQL(),
                dbname = "igpprototype",
                host = "108.162.177.90",
                username = "test",
                password = "thispassword",
                port = 3306
        )   
        return(conn)
}

#---------Functions to GET tables from database-----------------

#' general dbGetQuery function to reduce redundancy in code
#' 
#' @param str string to be read into MySQL to query the database
#'
#' @return the dataframe returned from the DBMS
getOneQuery <- function (str) {
	conn <- getConnect()
	ret <- dbGetQuery(conn, str)
	dbDisconnect(conn)
	return(ret)
}

#' sends a query to database such as an insert update or delete
#' (anything that does not require a return value)
#' 
#' @param str query string to be sent to database
#' 
#' @return shouldn't return anything
sendOneQuery <- function(str) {
        conn <- getConnect()
        query <- dbSendQuery(conn, str)
        dbClearResult(query)
        dbDisconnect(conn)
        return()
}

#' get number of burns
#' 
#' @return data.frame of a single number for burn count
getNumOfBurns <- function () {
        query <- getOneQuery("SELECT COUNT(burn_id) FROM burns;")
	return(query)
}

#' returns a list of burn IDs
#'
#' @return data.frame of burn IDs
getBurnList <- function () {
	print("io.R - getBurnList")
	query <- getOneQuery("SELECT burn_id FROM burns;")
	return(query)
}

#' retreives a whole table (for testing only!)
#'
#' @param tblName name of the table to be returned
#' 
#' @return data.frame of table specified by tblName
getWholeTable <- function(tblName){
	query <- getOneQuery(paste0("SELECT * FROM ", tblName, ";"))
	return(query)
}

#' function that returns specific download data for download tab
#' 
#' @param solIDs vector of solution IDs
getDownloadData <- function (solIDs, elIDs){
	print(solIDs)
	tryCatch({
		query <- getOneQuery( 
			paste0("SELECT solution_id, element_id, solid_conc, treatment FROM  
				filtered_solconc_treatment WHERE (solution_id = ", 
				paste(solIDs, collapse=" or solution_id = "), ") AND (element_id LIKE '",
				paste(elIDs, collapse="%' or element_id LIKE '"), "%');"))
		# print(paste0("SELECT solution_id, element_id, solid_conc, treatment FROM  
		# 		filtered_solconc_treatment WHERE (solution_id = ", 
		# 		paste(solIDs, collapse=" or solution_id = "), ") AND (element_id LIKE '",
		# 		paste(elIDs, collapse="%' or element_id LIKE '"), "%');"))
	},
	error = function(cond){
		message("SQL syntax error.")
		return(NULL)
	},
	warning = function(cond){
		message("SQL syntax error.")
		return(NULL)
	})
}

#### DEPRICATED ####

# #' get sample data view
# #' 
# #' @return data.frame of sample_data view
# getSampleData <- function (){
# 	query <- getOneQuery("SELECT * FROM sample_data;")
# 	return(query)
# }

###################

# #' initializes and returns burn choices for selectDataCleaningBurn
# #'
# #'@return - named vector of possible burn choices
# getBurnChoices() <- function(){
# 	burnOptions <- getBurnList()
# 	print(burnOptions)
# }

#' initializes and returns element choices for selectDataCleaningElement
#'
#' @return - vector of possible element choices
getElemChoices <- function(){
	print("io.R - getElemChoices")
	if(!exists("elemChoices")) {
		elemChoices <- c("Zn","Se","Mg", "K", "Fe","Cu","Ca")
	}
	return(elemChoices)
}

#' get filtered solid_conc and treatment data
#'
#' @param el - element being selected for
#' @param burn - burn being selected for
#' @param treat - treatment being selected for
#'
#' @return - table of solid_conc, treatment, solution_id and element_id
getSolConcTreat <- function(el, burn, treat){
	print("io.R - getSolConcTreat")
	query <- getOneQuery(
		paste0("SELECT solution_id, element_id, solid_conc, treatment 
		FROM filtered_solconc_treatment
		WHERE element_id LIKE '", el, "%'
		AND burn_id LIKE '", burn, "'
		AND treatment LIKE '", treat, "';"))
	return(query)
}

#' Takes in username and password, and outputs either a TRUE boolean value or a string indicating the
#' error that took place.
#' 
#' @param username username to be queried in database
#' @param password password to be checked against username
#' 
#' @return an error message string indicating what went wrong
#' @return a TRUE boolean value indicating authentication
getAuthentication <- function(username, password){
        #get db row for username
        credRow <- getOneQuery(
                paste0("SELECT username, password, failed_attempts FROM usernames WHERE username = '",
                username, "';"))
        print(credRow)  
        #if username doesn't exist, send error message
        if (is.na(credRow[1,"username"])){
                return("The username does not appear in our database")
        #else if failed attempts is creater than 3, send error message
        } else if (credRow[1,"failed_attempts"] > 3){ 
                return("You have attempted to log in too many times with the wrong password.\n
                        Please contact the database administrator.")
        #otherwise check the password and authenticate (clearing failed attempts),
        #or at 1 to failed attempts and return error message
        } else {
                if (sha512(password) == credRow[1, "password"]){
                        sendOneQuery(paste0("UPDATE usernames SET failed_attempts = 0 WHERE username = '",
                                username, "';"))
                        return(TRUE)
                } else {
                        sendOneQuery(
				paste0("UPDATE usernames SET failed_attempts = failed_attempts + 1 WHERE username = '",
                                	username, "';"))
                        return("The username and password do not match")
                }
        }
        return
}
#' Function for getting a list of users for user management tab
#' 
#' @return a list of users currently registered
getUserList <- function(){
	query <- getOneQuery("SELECT username FROM usernames");
	return(query)
}

#' Function to get the admin status of a user
#'
#' @param user username in question
#' 
#' @return true or false depending on if the user is or is not admin
getAdminStatus <- function(user){
	query <- getOneQuery(paste0("SELECT admin FROM usernames WHERE username = '",
		user, "';"))
	if(query == 1){
		return(TRUE)
	} else {
		return(FALSE)
	}
}


#' getter for solution concentrations based on burn_id
#' 
#' @param burn_id which burn id to pull concentration for
#' 
#' @return the solid concentration for the specified burn id
getBurnSolutionConcentration <- function(burn_id){
  solConc <- getOneQuery(paste0("SELECT label, element_id, solid_conc 
                                FROM solutions s JOIN solution_elements se ON s.solution_id = se.solution_id 
                                WHERE s.burn_id = ", burn_id, ";"))
  return(solConc)
}

#-------Functions to INSERT into database---------------
#' insert data from csv
#'
#' @param inFile the csv file with ICP data to be populated into database
#'
#' @return NA for error and NULL for warning
#' @return Nothing if all goes well
insertCSV <- function (inFile) {
	
    	#error checking
	if (is.null(inFile))
		return(NULL)

	#read lines into data frame from file
	tbl <- tryCatch({
			read.csv(inFile$datapath, header = TRUE, skip=1, sep = ',')

		},
		error = function(cond){
			message("Something went wrong while reading the .csv")
			message("Original error:")
			message(cond)
			return(NA)
		},
		warning = function(cond){
			message("A warning was produced when this file was read")
			message("Original warning:")
			message(cond)
			return(NULL)
		}
	)
	
	#more error handling
	if (length(tbl[1,]) != 29)
		stop("The file must contain 29 columns")
	
	#slice last 3 lines off the end
	tbl <- tbl[1:(length(tbl[,1])-3),] 
	
	#fix up column names
	colnames(tbl)[c(1,5,6,7,8,12,20,21)] <- c("label","type_of","element_id","flag","solid_conc",
		"intensity","act_wgt","act_vol")

	#make sure there are no open connections (for testing purposes)
	#cons <- dbListConnections(RMySQL::MySQL())
	#for (con in cons)
	#	dbDisconnect(con)

	#remove all Rinse and Blank columns from tbl
	tbl <- tbl[tbl$label!="Rinse" & tbl$label!="Blank",]
	#print(tbl["label"])
	#connect to database
	conn <- getConnect()
	
	#print(paste0("file path is: ", inFile$datapath, " tadah"))

	#make a new burn in burns table
	dbSendQuery(conn, paste0("INSERT INTO burns (date) values (curdate());"))

	#get latest burn
	lastBurn <- dbGetQuery(conn, "SELECT MAX(burn_id) FROM burns;")

	#add burn column to table
	burnID <- c(rep(as.numeric(lastBurn[1]),each=length(tbl[,1])))
	tbl$burn_id <- burnID
	
	
	#contruct solutions dataframe
	#empty solutions data frame
	solData <- data.frame(
		label = character(),
		type_of = character(),
		act_wgt = numeric(),
		act_vol = numeric(),
		DF = numeric(),
		burn_id = numeric()
	)
	dates <- vector()
	tempStr <- c("BLAHBLAH")
	for(row in 1:length(tbl[,1])){
		if(tbl[row,1] != tempStr){
			solData <- rbind(solData, tbl[row,c(1,5,20,21,22,30)])
			tempDate <- anydate(tbl[row,14])
			dates <- c(dates, as.character(tempDate))
			tempStr <- tbl[row,1]
		}
	}
		
	#add reformatted dates to solData
	solData$date <- dates
	#write to solutions table
	dbWriteTable(conn,"solutions", solData, 
		append = TRUE, row.names = FALSE, overwrite = FALSE)
		
	#write all of the elements to elements table
	#whatever elements are already present won't be added due to primary key constraint
	dbWriteTable(conn, "elements", data.frame(element_id = tbl[1:length(tbl[,1]),6]), 
		append = TRUE, row.names = FALSE, overwrite = FALSE)

	#construct dataframe for replicate table
	#get list of solution_ids
	lastSolut <- dbGetQuery(conn, "SELECT MAX(solution_id) FROM solutions;")
	lastSolut[1] <- lastSolut[1] - length(solData[,1])

	#empty replicate data frame
	repData <- data.frame(
		value = numeric(),
		solution_id = integer(),
		element_id = character()
	)
	#fill replicates dataframe with replicate data
	#make a vector for solution_id
	tempStr <- c("BLAHBLAH")
	solIDCol <- numeric()
	for (row in 1:length(tbl[,1])){
		if(tbl[row,1] != tempStr){
			lastSolut[1] <- lastSolut[1] + 1
			tempStr <- tbl[row,1]
		}
		repData <- rbind(repData, data.frame(value = c(as.numeric(tbl[row,24]), 
			as.numeric(tbl[row,26]), as.numeric(tbl[row,28])),
			solution_id = c(rep(as.numeric(lastSolut[1]),each=3)),
			element_id = c(rep(as.character(tbl[row,6]), each=3))
			)
			
		)
		solIDCol[row] <- as.numeric(lastSolut[1])
	}

	#add solution_id vector to tbl dataframe
	tbl$solution_id <- solIDCol
	
	#fill the solution_elements table
	dbWriteTable(conn, "solution_elements", tbl[1:length(tbl[,1]),c(6,7,8,12,15,16,17,18,19,31)],
		append = TRUE, row.names = FALSE, overwrite = FALSE)

	#fill replicate table
	dbWriteTable(conn, "replicates", repData, append = TRUE, row.names = FALSE,
		overwrite=FALSE)

	#disconnect from db
	dbDisconnect(conn)
	
	return
}

#------------Functions for saving and loading userData files and logging------------------
#' Function for saving the user data for other functions and for after logout
#'
#' @param dataset R dataset to be saved for later
#' @param username indicates which user to sabe the data for
#'
#' @return an error or a message indicating dataset has been saved
saveUserDataset <- function (dataset, username){
  if(dataset$selectedBurn=="%"){burnID = "All"} else {burnID = dataset$selectedBurn}
  element <- dataset$selectedElement
  
  dataname <- paste0(username, "-", burnID, "-", element, "-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))

	tryCatch({
		dir.create(paste0("data/",username), showWarnings=FALSE, recursive = TRUE)
		save(dataset, file=paste0("data/",username,"/",dataname,".RData"))
	}, warning = function(w){
		print(w)
		return("The dataset was saved with warnings")
	}, error = function(e){
		print(e)
		return("There was an error while saving your dataset!")
	}, finally = {
		print("Data saved!")
		return("Dataset was saved to the server successfully!")
	})
	return("Dataset has been saved!")
}

#' Function to remove a user from database
#'
#' @param username The username of the user being removed
removeUser <- function(username){
	sendOneQuery(paste0("DELETE FROM usernames WHERE username = '", username, "';"))
	return()
}

#' Function to create a new user from the 'Manage users' tab
#' 
#' @param username New username for the user
#' @param pass new password for the user
#' @param repass repeated password of the user
#' @param admin boolean of admin status
#' 
#' @return message indicating success or failure
createUser <- function(username, pass, repass,admin){
	#' get list of existing usernames
	userList <- getUserList()
	#' check if username already exists
	for(i in userList){
		if(i == username){
			return("Username already exists. Please choose another.")
		}
	}
	#' make sure passwords match
	if(pass != repass){
		return("The provided passwords do not match!")
	}
	#' if it gets to this point, send the query to the DBMS
	sendOneQuery(paste0("INSERT INTO usernames (username, password, admin) VALUES ('",
		username, "',SHA2('", pass,"',512),",admin,");"))
	return(paste0("Successfully added ",username))
}

#' Function for changing the user password from the 'Change Password' form in the
#' 'User Settings' tab
#'
#' @param user username of the changing password
#' @param oldPass Old password of the user
#' @param newPass New password for the user
#' @param repNewPass repeated new password for validation
#'
#' @return A string indicating success or failure of the operation to be output to the user 
changePass <- function(user, oldPass, newPass, repNewPass){
	#' get the old password
	checkedOldPass <- getOneQuery(paste0("SELECT password FROM usernames WHERE username = '",
		user, "';"))
	#' Check if the provided password matches the real one
	if(checkedOldPass != sha512(oldPass)){
		return("The old password provided is incorrect.")
	}
	#' Check if the new password matches the repeated one
	if(newPass != repNewPass){
		return("The new passwords that you have provided do not match!")
	}
	#' If it gets to this point, change the password
	sendOneQuery(paste0("UPDATE usernames SET password = SHA2('", newPass, 
		"',512) WHERE username = '", user, "';"))
	return("Password changed successfully.")
}

#' load data from saved sampElem file back into the app
#' 
#' @param username username to indicate which file to load
#'
#' @return saved R user data
getSavedDataset <- function (username, filename){
	dataset <- get(load(paste0("data/",username,"/", filename)))
	return(dataset)
}

#' returns a list of .RData files saved in the user's directory
#' 
#' @param username username to indicate the directory
#' 
#' @return list of R user data files.
getSavedDatasetList <- function(username){
  files <- list.files(paste0("data/",username,"/"))
  return(files)
}

#' write usage data to userlog.log file
#' 
#' @param usage string of data about the user to be logged
logData <- function(usage) {
	write(usage, file="www/userlog.log", append=TRUE)
}
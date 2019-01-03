library(DBI)
library(anytime)

source("queries.R")

#function for establishing MySQL connection.
getConnect <- function () {
        conn <- DBI::dbConnect(
                drv = RMySQL::MySQL(),
                dbname = "igpprototype",
                host = "108.162.133.226",
                username = "test",
                password = "thispassword",
                port = 3306
        )   
        return(conn)
}

#---------Functions to GET tables from database-----------------
#general dbGetQuery function to reduce redundancy in code
getOneQuery <- function (str) {
	conn <- getConnect()
	ret <- dbGetQuery(conn, str)
	dbDisconnect(conn)
	return(ret)
}

#get number of sessions
getNumOfSessions <- function () {
        query <- getOneQuery("SELECT COUNT(session_id) FROM sessions;")
	return(query)
}

#returns a list of session IDs
getSessionList <- function () {
	query <- getOneQuery("SELECT session_id FROM sessions;")
	return(query)
}

#retreives a whole table (for testing only!)
getWholeTable <- function(tblName){
	query <- getOneQuery(paste0("SELECT * FROM ", tblName, ";"))
	return(query)
}

#function that returns specific download data for download tab
getDownloadData <- function(sessions, tblName){
	switch(tblName,
		solutions={
			query <- getOneQuery( 
				paste0("SELECT ", getTableColumnNames(tblName), " FROM ", tblName, 
					" WHERE session_id = ", paste(sessions, 
						collapse=" or session_id = "), ";"))
		},
		solution_elements={
			query <- getOneQuery( 
				paste0("SELECT ", getTableColumnNames(tblName), " FROM ", tblName, 
					" se, solutions s 
					WHERE se.solution_id = s.solution_id and (session_id = ", 
					paste(sessions, 
						collapse=" or session_id = "), ");"))
		},
		replicates={
			query <- getOneQuery( 
				paste0("SELECT ", getTableColumnNames(tblName), " FROM ", tblName, 
					" r, solutions s, solution_elements se 
					WHERE se.solution_id = s.solution_id 
					and se.solution_id = r.solution_id 
					and se.element_id = r.element_id and (session_id = ", 
					paste(sessions, 
						collapse=" or session_id = "), ");"))
		}
	)
}

#return sample_data view
getSampleData <- function (){
	query <- getOneQuery("SELECT * FROM sample_data;")
	return(query)
}

#-------Functions to INSERT into database---------------
#insert data from csv
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

	#connect to database
	conn <- getConnect()
	
	print(paste0("file path is: ", inFile$datapath, " tadah"))

	#make a new session in sessions table
	dbSendQuery(conn, paste0("INSERT INTO sessions (date) values (curdate());"))

	#get latest session
	lastSess <- dbGetQuery(conn, "SELECT MAX(session_id) FROM sessions;")

	#add session column to table
	sessID <- c(rep(as.numeric(lastSess[1]),each=length(tbl[,1])))
	tbl$session_id <- sessID
	
	
	#contruct solutions dataframe
	#empty solutions data frame
	solData <- data.frame(
		label = character(),
		type_of = character(),
		act_wgt = numeric(),
		act_vol = numeric(),
		DF = numeric(),
		session_id = numeric()
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

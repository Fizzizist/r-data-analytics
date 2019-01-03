#----COLUMN SELECTS-------------

#input table name, output all column names in one string
getTableColumnNames <- function(tblName) {
	switch(tblName,
	solutions={
		return("solutions.solution_id, solutions.session_id, label, type_of, date, act_wgt, act_vol, DF")
	},
	solution_elements={
		return(
		"se.solution_id, se.element_id, time, solid_conc, intensity, SD, RSD, intSD, intRSD, flag")
	},
	replicates={
		return("r.replicate_id, r.solution_id, r.element_id, value")
	}
	)
}

#-------------WHERE CLAUSES----------------------



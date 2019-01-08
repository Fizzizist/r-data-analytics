library(DBI)
library(RMySQL)
library(dplyr)

source("io.R")
#### Collecting data from database.

# Connect to DB.
#igpdb = dbConnect(MySQL(),username='sqluser',password='password',dbname='igpprototype',host='localhost')

# Create the view I will be using to query and write to database.
#dbSendQuery(igpdb, "create view sample_data as select solution_id, element_id, solid_conc, SD from solution_elements")



#### Defining functions for statistical tests.

# This implements a single sided t-test with difference between means (du) from
# the null and alternative hypotheses, the standard deviation of the distribution (sd)
# and the sample size (n).

ptval <- function(du,sd,n) { # Returns both p and t values.
  t = du / (sd/sqrt(n))
  p = pt(q=t,df=(n-1), lower.tail=F)
  res =c(t,p)
  return(res)
}

tval <- function(du,sd,n) { # Returns just t values.
  t = du / (sd/sqrt(n))
  p = pt(q=t,df=(n-1), lower.tail=F)
  res =c(t,p)
  return(t)
}

pval <- function(du,sd,n) { # Returns just p values.
  t = du / (sd/sqrt(n))
  p = pt(q=t,df=(n-1), lower.tail=F)
  res =c(t,p)
  return(p)
}

#### Manipulating the data frame.
getPTValues <- function() {
	# Read data from view into 'sample.data' data frame.
	sample.data = getSampleData()
	#sample.data = fetch(result, n = 1000)
	# Taking the tval and pval of each row and adding them as columns t and p to a new data 
	#frame, 'trans.sample.data'.
	trans.sample.data <- transform(sample.data,t=tval(solid_conc,SD,3),p=pval(solid_conc,SD,3)) 
	# n is three because there are three replicates for each sample.

	# Removing rows with negative or infinite t values, insignificant and negative p values.
	filt.sample.data <- filter(trans.sample.data, p < 0.05, p > 0, t > 0, t < Inf)

	# Grouping by element into a list.

	elem <- c("Al","As","Ba","Cd","Cu","K","Mn","Ni","Pb","Se","Zn")
	samp.elem <- list()
	regex <- c()

	for (i in 1:length(elem)) {
	  regex[i] <- paste("^",elem[i],".*",sep='')
	  samp.elem[[i]] <- data.frame(filt.sample.data[grep(regex[i], filt.sample.data[,2], 
	  	perl = TRUE), ])
	}

	names(samp.elem) <- elem

	# Summary statistics for each element.

	#print(samp.elem)
	return(samp.elem)
}
# Now we can pass each set of sold_conc to the front end in response to selecting an element. 
# It will display a box-plot of that element and some summary statistics.
# I played around with some histograms but the scales are too different to show all elements 
#simultaneously. 
# There are also some pretty bad outliers, but we need to define a criteria for removing them.
# The box-plots can be used to visualize the distribution of the amount of each element lost for all 
#statistically signficant samples.
# Since the list is composed of data frames, we can call all any
#of the information we may need about each significant result (e.g., retrieve the sample_id). 

# R-Analytics
A web app written in R Shiny for the purpose of data cleaning and analysis in a graphical environment. This was originally was built for analyzing ICP data, but can be altered to perform data cleaning and analysis on any dataset of your choosing.
A [demo](http://108.162.177.90:3838/r-analytics/) version using an example dataset.

# Installation
The app can either be run from inside R or on a shiny-server installation.
## From inside R
1. Install all of the necessary R packages in your R environment using the following command:
```
install.packages(c("shiny","shinydashboard","shinyBS","shinyjs","DBI","RMySQL","openssl","anytime","xlsx","dplyr","DT","V8","plotly","ggplot2","crosstalk"))
```
Note that some of the above packages have dependencies outside of R that may need to be installed on your linux distribution.<br>
2. clone this repository and cd into the appropriate folder from a bash terminal:
```
git clone https://github.com/Fizzizist/r-data-analytics
cd r-data-analytics
```
3. Finally, open R, and run these 2 commands:
```
library(shiny)
runApp("rAnalytics")
```
## From a shiny-server installation
1. Install all of the necessary R packages in your R environment using the following command:
```
install.packages(c("shiny","shinydashboard","shinyBS","shinyjs","DBI","RMySQL","openssl","anytime","xlsx","dplyr","DT","V8","plotly","ggplot2","crosstalk"))
```
2. Follow the [instructions](https://www.rstudio.com/products/shiny/download-server/) to install shiny-server if you haven't already
3. Clone the repository and transfer the rAnalytics folder and its contents to /srv/shiny-server or wherever you have install shiny-server
4. Simply point your browser to the server and port of your shiny-server installation

# Authentication System
The app comes with a built-in authentication system that is disabled in the demo but is enabled by default.
Usernames are read from a usernames table in your MySQL DBMS.
# MySQL DBSM setup
In order to use the app, you will be required to create a MySQL database and update the authentication parameters in the following block of code found in the io.R file:
```
getConnect <- function () {
        conn <- DBI::dbConnect(
                drv = RMySQL::MySQL(),
                dbname = "databaseName",
                host = "localhost",
                username = "username",
                password = "password",
                port = 3306
        )   
        return(conn)
}
```
The app was originally built to work with ICP data, so you will have to go through the io.R file and tailor it to the database that you are using.
# Using the App
## File Upload
The file upload tab on the dashboard allows a user to upload a csv file of data which will then be populated automatically into the DBMS. Users looking to make use of this tab will need to modify the upload algorithm (insertCSV) in the io.R file to suit whatever kind of data columns you are passing in with your csv into your DBMS. 
## File Download
The file download tab allows the user to download (as a CSV or xlsx) a data file containing the data points that were saved using either the Data Cleaning or Data Exploration tabs.
## Data Cleaning
The Data Cleaning tab allows the user to select the datapoints that they feel are relevant to their analysis. For the purpose of ICP data, the user is also able to choose the chemical element that they would like to analyze. Once the appropriate data has been selected, either using the table on the left or the plot on the right, the datapoints can be saved using the "save" button. At the top of the screen. These data points can then be loaded in the Data Explorer tab, or downloaded in the File Download tab.
## Data Explorer
The user can explore either the full dataset, or load in a dataset that they previously saved using the "load" button at the top. The data can then be explored further using the plots on the left or the table on the right. The further refined data can then be again saved, either for further analysis or for download in the File Download tab.
## User Settings (If authentication is enabled)
The authentication system is not extremely robust, so the only option currently available in the user settings is to change the user's password.
## Manage Users (If authentication is enabled)
The Manage Users tab is only available to administrative users (selected at time of user creation). The administrator has the ability to create and delete users using this tab. This is currently the only way to add users, thus, an initial installation of the app requires that an admin user account is already present in the database.

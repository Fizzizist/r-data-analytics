# R-Analytics
A web app written in R Shiny for the purpose of data cleaning and analysis in a graphical environment.

# Installation
The app can either be run from inside R or on a shiny-server installation.
## From inside R
1. Install all of the necessary R packages in your R environment using the following command:
```
install.packages(c("shiny","shinydashboard","shinyBS","shinyjs","DBI","RMySQL","openssl","anytime","xlsx","dplyr","DT","V8","plotly","ggplot2","crosstalk"))
```
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
2. Follow the instructions at this url: https://www.rstudio.com/products/shiny/download-server/ to install shiny-server if you haven't already
3. Clone the repository and transfer the rAnalytics folder and its contents to /srv/shiny-server or wherever you have install shiny-server
4. Simply point your browser to the server and port of your shiny-server installation

# Authentication System
The app comes with a built-in authentication system that is disabled in the demo but is enabled when you install the app.

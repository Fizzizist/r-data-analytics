source("io.R")

getUIAuthJS <- function(){
  return("shinyjs.hideMenu = function(display){$('header').css('display', display);}")
}

displayLoginView <- function(output, session, message=""){
  updateTabItems(session, "sidebarMenu", "home")
  addClass(selector = "body", class = "sidebar-collapse") 	#Comment these lines to disable login (testing)
  js$hideMenu('none')										#Comment these lines to disable ligon (testing)
  output$authentication <- renderUI({
    tagList(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      actionButton("login", "Log In"),
      div(message, style="color:red")
    )
  })
}

displayWelcomeView <- function(output, authenticated){
  js$hideMenu('')
  removeClass(selector = "body", class = "sidebar-collapse")
  output$authentication <- renderUI({
    h1("Welcome!")
  })
  output$logout <- renderUI({
    if(authenticated){
      actionButton("btnLogout", "Log Out")
    }
  })
}

observeUserLogin <- function(input, output, session, authenticated){
  observeEvent(
    input$login,
    {
      user <- input$username
      password <- input$password
      
      auth = getAuthentication(user, password)
      
      if(auth == TRUE){
        authenticated <- TRUE
        displayWelcomeView(output, authenticated)
      }else{
        displayLoginView(output, session, auth)
      }
    }
  )
}

observeUserLogout <- function(input, output, session, authenticated){
  observeEvent(
    input$btnLogout,
    {
      authenticated <- FALSE
      displayLoginView(output, session)
    }
  )
}
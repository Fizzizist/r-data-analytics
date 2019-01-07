source("mockDBAuth.R")

getUIAuthJS <- function(){
  return("shinyjs.hideMenu = function(display){$('header').css('display', display);}")
}

displayLoginView <- function(output, session, message=""){
  updateTabItems(session, "sidebarMenu", "home")
  addClass(selector = "body", class = "sidebar-collapse")
  js$hideMenu('none')
  output$authentication <- renderUI({
    tagList(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      actionButton("login", "Log In"),
      div(message)
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
      
      auth = authenticate(user, password) #replace with a backend call
      
      if(auth[["isAuth"]]){
        authenticated <- TRUE
        displayWelcomeView(output, authenticated)
      }else{
        displayLoginView(output, sesion, auth[["message"]])
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
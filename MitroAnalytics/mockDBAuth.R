# This is a temporary code to simulate user authentication DB call.

credentials <- list(user1="12345")

# Returns a list containing a boolean value to indicate whether the user has been authenticated or not, 
# and an error message if the authentication fails.
authenticate <- function(user, password){
  if(!user %in% names(credentials)){
    return("incorrect username")
  }
  if(!password == credentials[[user]]){
    return("incorrect password")
  }
  return(TRUE)
}
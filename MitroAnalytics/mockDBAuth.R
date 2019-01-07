# This is a temporary code to simulate user authentication DB call.

credentials <- list(user1="12345")

# Returns a list containing a boolean value to indicate whether the user has been authenticated or not, 
# and an error message if the authentication fails.
authenticate <- function(user, password){
  if(!user %in% names(credentials)){
    return(list(isAuth=FALSE, message="incorrect username"))
  }
  if(!password == credentials[[user]]){
    return(list(isAuth=FALSE, message="incorrect password"))
  }
  return(list(isAuth=TRUE, message=""))
}
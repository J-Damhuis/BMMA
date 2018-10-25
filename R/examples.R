#' @export
# This function will add two numbers
add <- function(x,y)
{
  # This is where the addition occurs
  z <- x + y 
  return(z)
}

#' @export
algebra <- function(x,y)
{
  addition <- x + y
  mult <- x * y
  return(list(sum = addition,multiplication = mult))
}
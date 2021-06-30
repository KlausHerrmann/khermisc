
#' Check if an argument is a real scalar value.
#'
#' @param x Value to be tested.
#' @return Boolean TRUE if it is a real scalar value, FALSE otherwise
#' @export
is.scalar <- function(x) is.atomic(x) && length(x) == 1L && !is.character(x) && Im(x)==0 #https://stackoverflow.com/questions/38088392/how-do-you-check-for-a-scalar-in-r

#WRITE A FUNCTION FOR COMPLEX VALUE TOO

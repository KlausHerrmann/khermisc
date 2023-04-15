
#' Check if an argument is a real scalar value.
#'
#' @param x Value to be tested.
#' @return Boolean TRUE if it is a real scalar value, FALSE otherwise
#' @export
is.realNumber <- function(x) is.atomic(x) && length(x) == 1L && !is.character(x) && Im(x)==0 #https://stackoverflow.com/questions/38088392/how-do-you-check-for-a-scalar-in-r

#' Check if an argument is a complex scalar value.
#'
#' @param x Value to be tested.
#' @return Boolean TRUE if it is a complex scalar value, FALSE otherwise
#' @export
is.complexNumber <- function(x) is.atomic(x) && length(x) == 1L && !is.character(x) #https://stackoverflow.com/questions/38088392/how-do-you-check-for-a-scalar-in-r

#' Check if an argument is a scalar integer.
#'
#' @param x Value to be tested.
#' @return Boolean TRUE if it is a integer scalar value, FALSE otherwise
#' @export
is.integerNumber <- function(x) is.atomic(x) && length(x) == 1L && !is.character(x) && x%%1 == 0


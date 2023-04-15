
#' Get a color blind friendly color palette with eight colors
#'
#' @param x order of the palette
#' @return vector with colors in the specified order
#' @export
OkabeIto <- function(x=1:8){
    stopifnot(all.equal(sort(x)==1:8,rep(TRUE,8)))
    palette <- c(
                rgb(0,0,0,max=255),
                rgb(230,159,0,max=255),
                rgb(86,180,233,max=255),
                rgb(0,158,115,max=255),
                rgb(240,228,66,max=255),
                rgb(0,114,178,max=255),
                rgb(213,94,0,max=255),
                rgb(204,121,167,max=255))
    names <- c("black", "orange", "skyblue", "green", "yellow", "blue", "vermillion", "purple")
    palette <- palette[x]
    names(palette) <- names[x]
    return(palette)
}


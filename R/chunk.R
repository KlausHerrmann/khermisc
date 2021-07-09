
#' Partition vector into blocks (possibly overlapping) - R implementation
#'
#' @param x Input vector
#' @param chunkSize Integer, size of each block.
#' @param overlap Integer, number of overlapping data points.
#' @export
chunkR <- function(x,chunkSize,overlap=0){
  
  stopifnot(overlap>=0,overlap<chunkSize,chunkSize>0)
  
  effectiveSize <- chunkSize-overlap
  n <- length(x)
  
  if (n%%effectiveSize==0){
    m <- floor(n/effectiveSize)
  }else{
    m <- floor(n/effectiveSize)+1
  }
  
  chunks <- matrix(NA,m,chunkSize)
  
  #add entry to vector to avoid index out of bounds
  highestIndex <- m*chunkSize - (m-1)*overlap #do a table to see that upperIndex takes this value in the last iteration
  
  if (highestIndex>n){
    x <- c(x,rep(NA,(highestIndex-n)))
  }
  
  i <- 1
  for (k in 1:m) {
    
    upperIndex <- i+chunkSize-1
    
    ind <- i:upperIndex
    chunks[k,] <- x[ind]
    
    i <- i + effectiveSize
  }
  
  return(chunks)  
}

#' Partition vector into blocks (possibly overlapping)
#'
#' @param x Input vector
#' @param chunkSize Integer, size of each block.
#' @param overlap Integer, number of overlapping data points.
#' @param type Toggle either the R or the C++ implementation.
#' @export
chunk <- function(x,chunkSize,overlap=0,type="C++"){

    stopifnot(overlap>=0,overlap<chunkSize,chunkSize>0)

    if (type=="R"){
        return(chunkR(x=x,chunkSize=chunkSize,overlap=overlap))
    
    } else if (type=="C++"){
        return(chunkCpp(x=x,chunkSize=chunkSize,overlap=overlap))
    } else {
        stop("unknown compute type")
    }


}
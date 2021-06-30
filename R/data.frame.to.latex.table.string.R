
#' Output the contents of a numeric matrix or dataframe into LaTeX type table notation string
#'
#' @param dframe A data.frame or matrix
#' @param topLeft A string that will fill the top left corner (first cell of the first row) of the table
#' @param header A boolean toggle. If true a full latex table with begin{table} etc is created, if false only cells are converted.
#' @param cellFormat A function that will convert the cell content to a latex compatible string. The function has to accept three arguments, row, column and cell value and has to return a string. It is applied to the cells (i,j > 0) and to the margins (i,j = 0) including topLeft which sits at (0,0).
#' @return A vector of strings where each entry is one line of latex code.
#' @examples
#' n <- 3
#' m <- 6
#' M1 <- matrix(1:(n*m),n,m)
#' rownames(M1) <- 1:n
#' colnames(M1) <- 1:m
#' S1 <- data.frame.to.latex.table.string(M1,topLeft="TL")
#' @export
data.frame.to.latex.table.string <- function(dframe,topLeft="",header=TRUE,cellFormat=function(i,j,cell){return(cell)}){

    stopifnot(is.data.frame(dframe)||is.matrix(dframe), is.logical(header), length(header)==1)

    strVarName <- deparse(substitute(dframe)) #get name of the input variable
 
    #if we have a matrix convert to data frame (gives automatic row and column names if they are not set)
    if (is.matrix(dframe)){dframe <- as.data.frame(dframe)}

    n <- nrow(dframe)
    m <- ncol(dframe)

    #vector for output - all rows plus the top row with the column names
    vecOut <- rep(NA,n+1)

    rn <- rownames(dframe)
    cn <- colnames(dframe)

    if (is.null(rn)){
        rn <- paste("Row",1:n)
    }

    if (is.null(cn)){
        cn <- paste("Col",1:m)
    }

    strOut <- cellFormat(0,0,topLeft) #format cell content according to user specification.
    for (j in 1:m) {
        cell.ij <- cellFormat(0,j,cn[j]) #format cell content according to user specification.
        strOut <- paste(strOut,cell.ij,sep=" & ")
    }
    vecOut[1] <- paste(strOut," \\\\",sep="")

    #fill all rows below
    for (i in 1:n) {
        strOut <- cellFormat(i,0,rn[i]) #format cell content according to user specification.
        for (j in 1:m) {
            cell.ij <- cellFormat(i,j,dframe[i,j]) #format cell content according to user specification.
            strOut <- paste(strOut,cell.ij,sep=" & ")
        }

        if (i < n){
            strOut <- paste(strOut," \\\\",sep="")
        }
        vecOut[i+1] <- strOut
    }

    #if the header option is TRUE add everything around to make a full latex table
    if (header){
        strH <- rep(NA,5)
        strH[1] <- "\\begin{table}"
        strH[2] <- "\\begin{center}"
        strH[3] <- paste("\\caption{Auto-generated table for variable ",strVarName," (",format(Sys.time(), "%a %b %d %X %Y"),")}",sep="")
        strH[4] <- paste("\\label{tab:",strVarName,"}",sep="")
        strH[5] <- paste("\\begin{tabular}{",paste(rep("c",length.out=(m+1)), sep ="", collapse=""),"}",sep="")

        strE <- rep(NA,3)
        strE[1] <- "\\end{tabular}"
        strE[2] <- "\\end{center}"
        strE[3] <- "\\end{table}"
        vecOut <- c(strH,vecOut,strE)
    }

    return(vecOut)
}




example.matrix.and.data.frame.to.latex.table.string <- function(){

    #numeric matrix
    n <- 3
    m <- 6
    M1 <- matrix(1:(n*m),n,m)
    rownames(M1) <- 1:n
    colnames(M1) <- 1:m
    S1 <- data.frame.to.latex.table.string(M1,topLeft="T1")

    write("","")
    write(S1,"")
    write("","")

    #S11 <- data.frame.to.latex.table.string(M1,topLeft="T1",fmt="%0.3f",header=FALSE)
    S11 <- data.frame.to.latex.table.string(M1,topLeft="T1",header=FALSE)

    write(S11,"")
    write("","")

    #string matrix
    M2 <- matrix(letters[1:(n*m)],n,m)
    S2 <- data.frame.to.latex.table.string(M2,topLeft="T2")

    write(S2,"")
    write("","")


    # example of a custom format function
    myFormat <- function(i,j,cell){
      #format only the contents of the data frame, not the row and column names
      if (i > 0){
        if (j > 0){
          if (is.numeric(cell)){
            return(sprintf(cell,fmt="%f")) #make gray if lower than ... or bold if higher than ...
          }
        }
        
      }
      return(cell)
    }

    #data frame
    M3 <- data.frame(letters[1:10],101:110,LETTERS[11:20])
    #S3 <- data.frame.to.latex.table.string(M3,header=TRUE)
    S3 <- data.frame.to.latex.table.string(M3,header=TRUE,topLeft="T1",cellFormat=myFormat)
    #write(S3,file ="BBB.tex")
    write(S3,"")
}


#example.matrix.and.data.frame.to.latex.table.string()


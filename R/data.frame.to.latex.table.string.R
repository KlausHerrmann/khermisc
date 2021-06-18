


#output the contents of a numeric matrix or dataframe into LaTeX type table notation string
#' @export
data.frame.to.latex.table.string <- function(dframe,topLeft="",fmt="%f",header=TRUE){

    stopifnot(is.data.frame(dframe)||is.matrix(dframe), is.logical(header), length(header)==1)

    strVarName <- deparse(substitute(dframe)) #get name of the input variable

    #if we have a numeric matrix use sprintf to convert to strings
    if (is.matrix(dframe)){

        numCheck <- is.numeric(dframe) #FIX - FIND BETTER is.numeric FUNCTION MAYBE

        rn <- rownames(dframe)
        cn <- colnames(dframe)
        dframe <- as.data.frame(dframe)

        if (numCheck){
            dframe <- lapply(dframe, sprintf, fmt = fmt)
            dframe <- do.call(cbind, dframe)
        }

        rownames(dframe) <- rn
        colnames(dframe) <- cn 
    }

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

    #top row in the table
    strOut <- paste(c(topLeft,cn), sep = "", collapse = " & ")
    vecOut[1] <- paste(strOut," \\\\",sep="")

    #fill all rows below
    for (i in 1:n) {
        strOut <- rn[i]
        for (j in 1:m) {
            cell.ij <- dframe[i,j]
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

    S11 <- data.frame.to.latex.table.string(M1,topLeft="T1",fmt="%0.3f")

    write(S11,"")
    write("","")

    #string matrix
    M2 <- matrix(letters[1:(n*m)],n,m)
    S2 <- data.frame.to.latex.table.string(M2,topLeft="T2")

    write(S2,"")
    write("","")

    #data frame
    M3 <- data.frame(letters[1:10],101:110,LETTERS[11:20])
    S3 <- data.frame.to.latex.table.string(M3,header=TRUE)
    write(S3,file ="BBB.tex")

}


#example.matrix.and.data.frame.to.latex.table.string()


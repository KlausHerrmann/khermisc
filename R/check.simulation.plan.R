
#' Compute hash value from a vector by collapsing it into one string
#'
#' @param vec A vector for which the hash is computed.
#' @export
row.hash <- function(vec){
    library(digest)
    hash <- digest::digest(paste(vec,collapse=""), algo="md5", serialize=F)
}

#' Check which computations of a compute plan are done
#'
#' @param strPlan A string specifying the RData file with the compute plan.
#' @param zipDir A string specifying the folder with the computation results.
#' @param pattern A string to identify the zip files holding computation results.
#' @param pattern A string to identify the file inside the zip folder that contains the hash connected to the starting value file (a row in the compute plan).
#' @export
check.computation.plan <- function(strPlan,zipDir=".",pattern="*_out.zip", hashFile="inputFile.hash"){

    #load computation plan from RData file
    load(strPlan)
    n <- nrow(computePlan)
    done <- rep("{\\color{red}{$\\times$}}",n)

    #create hash values from compute plan per row
    hashPerRow <- apply(computePlan, 1, row.hash)

    files <- list.files(pattern=pattern, recursive = TRUE, path=zipDir)

    m <- length(files)
    hashZip <- rep(NA,m)
    if (m > 0){
        for (k in 1:m) {
            #unzip and get the hash
            hashZip[k] <- read.table(unz(files[k], hashFile), nrows=1, header=F, sep="")
        }
        #compare the hash to the possible values
        ind <- match(hashZip,hashPerRow)
    } else {
        ind <- NA
    }

    if (is.na(ind)){
        print("no match for hash values")
    } else {
        done[ind] <- "{\\color{green}{\\checkmark}}"
    }

    #create latex table that gives an overview on which simulations are done and which not
    dframe <- data.frame(computePlan,done)

    table <- data.frame.to.latex.table.string(dframe)
}


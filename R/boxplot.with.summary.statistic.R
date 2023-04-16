
#' Boxplot that adds an additional summary statistic (default: mean)
#'
#' @param x List, data.frame or matrix for where for each column a boxplot is made
#' @param summaryFunc A function that computes a summary statistic for a vector of values. Default is mean
#' @param mpch The pch value used to plot the value of summaryFunc in the plot
#' @param mcol Color (or vector of colors) used to plot the value of summaryFunc
#' @param addSummary Boolean toggle to plot the summaryFunc or not.
#' @param ... Parameters passed to boxplot
#' @return A boxplot with the value of the summary statistic indicated.
#' @export
myBoxplot <- function(x,summaryFunc=mean,mpch=19,mcol="black",addSummary=TRUE,...){
    
    graphics::boxplot(x,...)
  
  #add additional summary statistic (mean)
  if (addSummary){

    if (is.list(x)){ #list or data.frame
      
        mVec <- sapply(x,summaryFunc)
    
    } else if (is.matrix(x)){ #apply function per column to the matrix
        
        mVec <- apply(x,MARGIN=2,FUN=summaryFunc)

    } else { #apply the function - input should be a vector or scalar
      
        mVec <- summaryFunc(x)

    }

    points(1:length(mVec),mVec,pch=mpch,col=mcol)
  }
}


#' Builds a shell script that runs a job on a compute cluster
#'
#' @param maxTime A string specifying the maximum run time after which the job is canceled.
#' @param account A string specifying the account for billing.
#' @param memory A string specifying the memory requirements for the job.
#' @param modules A vector of strings with the modules.
#' @param exports A vector of strings with exports.
#' @param scriptName A string that identifies where the R script is located (full path).
#' @param inputFile A string that identifies the input RData file (file name only).
#' @param inputFolder A string that identifies the folder where the inputFile is located.
#' @param outputFolder A string that identifies where the output file is located.
#' @export
slurm.job.R <- function(maxTime,account,memory,modules,exports,scriptName,inputFile,inputFolder,outputFolder=""){

    if (outputFolder==""){
        outputFolder=inputFolder
    }

    strSbatch <- c(
    "#!/bin/bash",
    paste("#SBATCH --time=",maxTime,sep=""),
    paste("#SBATCH --account=",account,sep=""),
    paste("#SBATCH --mem-per-cpu=",memory,sep=""))

    strModule <- ""
    if (length(modules) >=1){
        for (module in modules) {
            strModule <- c(strModule,paste("module load",module))
        }
    }

    strExport <- ""
    if (length(exports) >=1){
        for (export in exports) {
            strExport <- c(strExport,paste("export",export))
        }
    }

    #remove .RData ending to be able to define the output file
    outputFile <- paste(outputFolder,tools::file_path_sans_ext(inputFile),"_out_${SLURM_JOBID}.RData",sep="")
    
    strRun <- c("",
    paste("scriptName=",scriptName,sep=""),
    paste("inputFile=",inputFolder,inputFile,sep=""),
    paste("outputFile=",outputFile,sep=""),
    "",
    paste("Rscript $scriptName $inputFile $outputFile",sep=""))

    strFinal <- c(strSbatch,strModule,strExport,strRun)
}




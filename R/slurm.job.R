
#' Builds a shell script that runs a job on a compute cluster
#'
#' @param maxTime A string specifying the maximum run time after which the job is canceled.
#' @param account A string specifying the account for billing.
#' @param memory A string specifying the memory requirements for the job.
#' @param modules A vector of strings with the modules.
#' @param exports A vector of strings with exports.
#' @param strRun A vector of strings with the instructions (bash code) for the job.
#' @return A string that can be written with writeLines to a .sh file that can be executed on a cluster.
#' @export
slurm.job.R <- function(maxTime,account,memory,modules=slurm.job.standard.modules(),exports=slurm.job.standard.exports(),strRun){

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

    strFinal <- c(strSbatch,strModule,strExport,"",strRun)
}

#' Returns standard modules that are used for R on the Compute Canada cluster
#'
#' @return A vector of strings that can be used as input to for the modules argument in slurm.job.R
#' @export
slurm.job.standard.modules <- function(){
    modules <- c(
      "nixpkgs/16.09",
      "intel/2018.3",
      "gsl/2.5",
      "gcc/7.3.0",
      "r/3.5.2")
}

#' Returns standard exports that are used for R on the Compute Canada cluster
#'
#' @return A vector of strings that can be used as input to for the exports argument in slurm.job.R
#' @export
slurm.job.standard.exports <- function(){
    exports <-c(
      "LIBRARY_PATH=~/GLPK/lib",
      "LD_LIBRARY_PATH=~/GLPK/lib",
      "CPATH=~/GLPK/includereq")
}

#' Returns standard shell code that can be used for R on the Compute Canada cluster
#'
#' @param inputFileName A string specifying the name of the compute plan file without the file ending .CompPlan
#' @param row An integer specifying the row of the compute plan that should be used for the computations.
#' @param scriptName A string specifying the name of the R script that will perform the calculations.
#' @return A list with a vector of strings that can be used as input to for the strRun argument in slurm.job.R, and a string that is the name of the shell script that one should use to write it.
#' @export
slurm.job.standard.run <- function(CompPlanName,row,scriptName){
    inputFile <- paste(CompPlanName,".CompPlan",sep="")
    outputFile <- paste(CompPlanName,"_",row,".RData",sep="")
    shellScript <- paste(CompPlanName,"_",row,".sh",sep="")

    #content of the script
    strRun <- c("","## SCRIPT ##",
    paste("echo \"SHELL SCRIPT: ",shellScript,"\"",sep=""),
    "echo \"SLURM JOB: ${SLURM_JOBID}\"", "",
    paste("scriptName=",scriptName,sep=""),
    paste("inputFile=",inputFile,sep=""),
    paste("row=",row,sep=""),
    paste("outputFile=",outputFile,sep=""),
    "",
    paste("Rscript $scriptName $inputFile $row $outputFile",sep=""))
    return(list(strRun=strRun,shellScript=shellScript))
}







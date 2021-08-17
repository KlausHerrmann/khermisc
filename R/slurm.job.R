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
slurm.job.R <- function(maxTime,account,memory,modules,exports,strRun){

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




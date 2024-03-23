#' Retrieve and Summarize the Oceanographic Connectivity Database
#' 
#' \code{getDataBase} This function manages the retrieval and loading of the oceanographic connectivity database. It can download the database 
#' from an online repository if it does not exist locally or if the 'overwrite' flag is set. The function also provides a summary of the database's key characteristics.
#'
#' @param myFolder (Optional) The local folder where the database will be stored. Defaults to "./Database".
#' @param overwrite (Optional) A logical flag. If TRUE, the existing database will be overwritten with a fresh download. Defaults to FALSE.
#'
#' @examples
#' \dontrun{
#' # Download and load the database into the default folder
#' getDataBase()
#'
#' # Download the database to a custom folder and force overwrite 
#' getDataBase(myFolder = "my_data_folder", overwrite = TRUE)
#' }
#'
#' @export getDataBase

getDataBase <- function(myFolder="./Database", overwrite=FALSE){

    cat("\n")
    cat("# ---------------------------------------------","\n")
    cat("# Get connectivity database","\n")
    cat("# ---------------------------------------------","\n")
    cat("# \n")
    
    options(timeout=10000)
    gc(reset=TRUE, full = TRUE)

    if ( ! dir.exists(myFolder) ) { dir.create(myFolder, recursive = TRUE) }

    if( ! file.exists(paste0(myFolder,"/oceanographicConnectivity.RData")) | overwrite ) {
        cat("# Downloading file database from online repository.","\n")
        download.file("https://github.com/jorgeassis/coastalNetRepoLFS/raw/main/oceanographicConnectivity.RData", paste0(myFolder,"/oceanographicConnectivity.RData"))
    }

    cat("# Loading database.","\n")
    
    if( exists("oceanographicConnectivity") ){ rm(oceanographicConnectivity, envir = .GlobalEnv) }
    
    oceanographicConnectivity <<- loadRData(paste0(myFolder,"/oceanographicConnectivity.RData"))
    options(timeout=60)

    data("summaryBM")

    cat("# \n")
    cat("# ---------------------------------------------","\n")
    cat("# Database summary:\n")
    cat("# \n")
    cat("# Version:",summaryBM$version,",",paste0(summaryBM$date),"\n")
    cat("# Years:",min(summaryBM$years),"-",max(summaryBM$years),"\n")
    cat("# Months:",1,"-",12,"\n")
    cat("# Days:",1,"-",31,"\n")
    cat("# Maximum period:",summaryBM$period,"(days)\n")
    cat("# Hexagon sites:",summaryBM$hexagons.n,"\n")
    cat("# Connectivity events:",summaryBM$events.n,"\n")
    cat("# ---------------------------------------------","\n")

}

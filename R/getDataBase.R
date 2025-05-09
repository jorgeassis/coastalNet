#' Retrieve and Summarize the Oceanographic Connectivity Database
#' 
#' \code{getDataBase} This function manages the retrieval and loading of the oceanographic connectivity database. It can download the database 
#' from an online repository if it does not exist locally or if the 'overwrite' flag is set. The function also provides a summary of the database's key characteristics.
#'
#' @param fileName (Optional) A character string specifying the local .RData file where the database will be stored. Defaults to "./Database/connectivity.RData".
#' @param overwrite (Optional) A logical flag. If 'TRUE', the existing database will be overwritten with a fresh download. Defaults to 'FALSE'.
#'
#' @return
#' An object containing the connectivity database.
#'
#' @examples
#' \dontrun{
#' # Download and load the database into the default folder
#' connectivity <- getDataBase()
#'
#' # Download the database to a custom folder and force overwrite 
#' connectivity <- getDataBase(fileName = "Database/connectivity.RData", overwrite = TRUE)
#' }
#'
#' @export getDataBase

getDataBase <- function(fileName="./Database/connectivity.RData", overwrite=FALSE){
  
  cat("\n")
  cat("# ---------------------------------------------","\n")
  cat("# Get connectivity database","\n")
  cat("# ---------------------------------------------","\n")
  cat("# \n")
  
  gc(reset=TRUE, full = TRUE)
  
  myFolder <- dirname(fileName)
  
  if ( ! dir.exists(myFolder) ) { dir.create(myFolder, recursive = TRUE) }
  
  if( ! file.exists( fileName ) | overwrite ) {
    options(timeout=10000000)
    cat("# Downloading database from online repository.","\n")
    download.file("https://figshare.com/ndownloader/files/45641262", fileName, quiet = FALSE, mode = "wb")
    options(timeout=60)
  }
  
  cat("# Loading database.","\n")
  
  error <- FALSE
  tryCatch( connectivity <- loadRData( fileName ) , error=function(e) { error <<- TRUE } )
  
  if( error ) {
    cat("# The file was not correctly downloaded or the file was corrupted. Please try downloading again with a stable internet connection.","\n")
    file.remove( fileName )
  }
  
  if( ! exists("summaryBM")) {
    
    hexagons <- unique(connectivity$connectivityEventStartHexagon)
    
    summaryBM <- list(years=unique(connectivity$connectivityEventStartYear),
                      hexagons=hexagons,
                      hexagons.n=length(hexagons),
                      period=max(connectivity$connectivityEventTravelTime),
                      events.n=nrow(connectivity),
                      version=1.0,
                      date="2025-03-01")
    
  }
  
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
  
  return(connectivity)
  
}

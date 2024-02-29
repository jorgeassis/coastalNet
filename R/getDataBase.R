#' Get connectivity database from on-line repository
#' 
#' \code{getDataBase} This function downloads and loads the required data for the coastal oceanographic connectivity analysis.
#' 
#' @param myFolder the folder path where the data files will be stored. Default is "./Database".
#' @param overwrite a logical value indicating whether to overwrite existing data files. Default is FALSE.
#'
#' @export

getDataBase <- function(myFolder="./Database", overwrite=FALSE){

    cat("\n")
    cat("# ---------------------------------------------","\n")
    cat("Get connectivity database from on-line repository","\n")

        options(timeout=10000)
        gc(reset=TRUE, full = TRUE)

        if ( ! dir.exists(myFolder) ) { dir.create(myFolder, recursive = TRUE) }

        if( ! file.exists(paste0(myFolder,"/oceanographicConnectivity.RData")) | overwrite ) { 
            download.file("https://github.com/jorgeassis/coastalNetRepoLFS/raw/main/oceanographicConnectivity.RData", paste0(myFolder,"/oceanographicConnectivity.RData"))
        }

        if( exists("oceanographicConnectivity") ){rm(oceanographicConnectivity, envir = .GlobalEnv)}
        
        oceanographicConnectivity <<- loadRData(paste0(myFolder,"/oceanographicConnectivity.RData"))
        names(oceanographicConnectivity) <<- c("connectivityEventID","connectivityEventStartHexagon","connectivityEventStartYear","connectivityEventStartMonth","connectivityEventStartDay","connectivityEventEndHexagon","connectivityEventTravelTime")
        options(timeout=60)

    data("referenceTable")

    cat("Years:",oceanographicConnectivity[1,"connectivityEventStartYear"],"-",oceanographicConnectivity[nrow(oceanographicConnectivity),"connectivityEventStartYear"],"\n")
    cat("Months:",oceanographicConnectivity[1,"connectivityEventStartMonth"],"-",oceanographicConnectivity[nrow(oceanographicConnectivity),"connectivityEventStartMonth"],"\n")
    cat("Days:",1,"-",31,"\n")
    cat("Number of hexagon sites:",nrow(referenceTable),"\n")
    cat("Number of connectivity events:",nrow(oceanographicConnectivity),"\n")
    cat("# ---------------------------------------------","\n")


}
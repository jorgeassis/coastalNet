#' Load R Data
#' 
#' \code{loadRData} This function loads an R data file and returns the objects stored in it.
#' 
#' @param fileName the name of the R data file to be loaded.
#'
#' @export

loadRData <- function(fileName){
    if(grepl("http",fileName)) {        
        download.file(fileName, destfile = "temp.RData", mode = "wb")
        fileName <- "temp.RData"
    }
    load(fileName)
    get(ls()[ls() != "fileName"])
}
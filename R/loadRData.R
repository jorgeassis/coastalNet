#' Load R Data
#' 
#' \code{loadRData} A function to load RData files from either a local filepath or a web-accessible URL. The function handles downloading from URLs if necessary.
#' 
#' @param fileName The filepath or URL of the RData file to load.
#' @return The loaded data object from the RData file.'
#'
#' @examples
#' \dontrun{
#' # Example with a local file
#' mydata <- loadRData("data/my_data.RData")
#'
#' # Example with a URL
#' remote_data <- loadRData("https://mywebsite.com/data/ocean_data.RData")
#' }
#' 
#' @export loadRData

loadRData <- function(fileName){
    if(grepl("http",fileName)) {        
        download.file(fileName, destfile = "temp.RData", mode = "wb")
        fileName <- "temp.RData"
    }
    load(fileName)
    get(ls()[ls() != "fileName"])
}
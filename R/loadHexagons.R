#' Load R Data
#' 
#' \code{loadHexagons} A function to load the hexagons that define the source and sink locations of connectivity.
#' 
#' @param fileName The filepath of the sf file to load.
#' @return The loaded data object from the sf file.'
#'
#' @examples
#' \dontrun{
#' # Example with default hexagons
#' hexagonCells <- loadHexagons()
#'
#' # Example with a local file
#' hexagonCells <- loadHexagons(fileName="data/my_hexagons.sf")
#' }
#'
#'#' @importFrom sf read_sf
#'
#' @export loadHexagons

loadHexagons <- function(fileName=NULL) {
  
  if(is.null(fileName)) { 
    data("hexagonCells")
  }
  
  if(!is.null(fileName)) { 
    
    error <- FALSE
    tryCatch(hexagonCells <- read_sf(fileName), error = function(e) error <<- TRUE)
    if( error & class(hexagonCells)[1] != "sf") { stop("Error :: The file provided does not generate a valid sf class object.") }
    
  }
  
  return(hexagonCells)
  
}
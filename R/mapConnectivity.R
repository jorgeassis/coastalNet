#' Visualize Pairwise Connectivity on a Map
#'
#' \code{mapConnectivity} This function creates a map visualization of pairwise oceanographic connectivity. It takes the output of the 'getPairwiseConnectivity' function (or a similar data frame) and plots connections between hexagon locations on a map, with the thickness or color of the connections representing the strength of the connectivity.
#'
#' @param connectivityPairs A data frame containing pairwise connectivity results. It should have the following columns:
#'   * 'From' (character): The name or ID of the "from" region.
#'   * 'To' (character): The name or ID of the "to" region.
#'   * 'FromHexagon' (character): The hexagon ID of the starting point of the connection.
#'   * 'ToHexagon' (character): The hexagon ID of the ending point of the connection.
#'   * 'Value' (numeric): The connectivity value, used to determine the visual representation (e.g., line thickness or color) of the connection.
#' @param obj A spatial object involved in the connectivity estimates. It must be an 'sf' object representing points or polygons, or a data.frame or matrix with two columns representing longitude and latitude locations.
#' @param featureName A vector with the name of the elements of the spatial object or, the column name of the attribute table of the spatial object used to grouped or aggregate input spatial feature.
#' @param print (Optional) A logical flag. If 'TRUE', the map is displayed directly. Defaults to 'FALSE'.
#'
#' @return A list containing:
#'   * 'mappingData': The input 'connectivityPairs' data frame, augmented with longitude ('hexagonsFromLon', 'hexagonsToLon') and latitude ('hexagonsFromLat', 'hexagonsToLat') coordinates for the hexagons.
#'   * 'lineConnections': An 'sf' object representing the line connections between hexagons, with the 'Value' column used for visualization.
#'   * 'objCentroid': An 'sf' object containing the centroids spatial object involved in the connections.
#'
#' @examples
#' \dontrun{
#' # Assuming you have the results from 'getPairwiseConnectivity' stored in 'pw_results'
#' map_data <- mapConnectivity(connectivityPairs = pw_results$connectivityPairs)
#' 
#' # Display the map:
#' mapConnectivity(connectivityPairs = pw_results$connectivityPairs, print = TRUE)
#' }
#' 
#' @importFrom sp merge 
#' @importFrom sf st_centroid st_coordinates st_as_sf st_crs
#' @importFrom geosphere gcIntermediate
#' @importFrom ggplot2 ggplot geom_sf scale_color_gradient2 coord_sf theme_minimal
#' @importFrom dplyr %>% 
#' @export mapConnectivity

mapConnectivity <- function(obj=NULL , featureName = NULL, connectivityPairs=NULL, print=FALSE) {
  
  if( is.null(connectivityEvents)) { stop("The connectivityPairs parameter is required.") }
  if( is.null(obj)) { stop("The obj parameter is required.") }
  if( ! "data.frame" %in% class(connectivityPairs) ) { stop("The connectivityPairs parameter must be of class data.frame.") }
  if( ! "sf" %in% class(obj) & ! "data.frame" %in% class(obj)  & ! "matrix" %in% class(obj)  ) { stop("The obj parameter must be of class sf, data.frame or matrix") }
  
  cat("\n")
  cat("# ---------------------------------------------","\n")
  cat("Map connectivity between pairs of sites","\n")
  cat("# ---------------------------------------------","\n")
  
  options(warn=-1)
  
  if( "sf" %in% class(obj) ) {
    
    if( is.null(featureName) ) { stop("The featureName parameter is required") }
    if( ! "character" %in% class(featureName) ) { stop("The featureName parameter must be of class character") }
    
    coords <- st_centroid(obj)
    coords <- as.data.frame(st_coordinates(coords))
    
    if( length(featureName) == nrow(coords)) { 
      coords$featureName <- featureName
    }
    
    if( length(featureName) == 1) { 
      if( ! featureName %in% names(obj) ) { stop("The featureName parameter is not a column name of the obj parameter") }
      coords$featureName <- as.data.frame(obj)[,featureName]
    }
    
    
  }
  
  if( ! "sf" %in% class(obj) & ("data.frame" %in% class(obj) | "matrix" %in% class(obj)) ) {
    
    if( ncol(obj) != 2 ) { stop("The obj parameter must have two columns (i.e., longitude and latitude)") }
    if( is.null(featureName)) { stop("The featureName parameter is required") }
    if( ! "character" %in% class(featureName)) { stop("The featureName parameter must be of class character") }
    if( length(featureName) != nrow(obj)) { stop("The featureName parameter must have the same number of elements as the obj parameter") }
    
    coords <- obj
    coords <- as.data.frame(coords)
    colnames(coords) <- c("X","Y")
    coords$featureName <- featureName
    
  }
  
  # Get pairwise connectivity between sites
  connectivityPairs <- connectivityPairs[connectivityPairs$From != connectivityPairs$To, ]
  connectivityPairs <- connectivityPairs[connectivityPairs$Value != 0, ]
  connectivityPairs$hexagonsFromLon <- NA
  connectivityPairs$hexagonsFromLat <- NA
  connectivityPairs$hexagonsToLon <- NA
  connectivityPairs$hexagonsToLat <- NA
  
  # Compute matching pairs
  
  connectivityPairs[, "hexagonsFromLon"] <- coords[match(connectivityPairs$From,as.character(coords$featureName)),"X"]
  connectivityPairs[, "hexagonsFromLat"] <- coords[match(connectivityPairs$From,coords$featureName),"Y"]
  connectivityPairs[, "hexagonsToLon"] <- coords[match(connectivityPairs$To,coords$featureName),"X"]
  connectivityPairs[, "hexagonsToLat"] <- coords[match(connectivityPairs$To,coords$featureName),"Y"]
  
  connectivityPairs <- connectivityPairs[sort(connectivityPairs$Value, index.return = T, decreasing = FALSE)$ix, ]
  
  # Get lines
  
  lineConnections <- list()
  for (i in 1:nrow(connectivityPairs)) {
    
    pointFrom <- c(connectivityPairs[i, "hexagonsFromLon"], connectivityPairs[i, "hexagonsFromLat"])
    pointTo <- c(connectivityPairs[i, "hexagonsToLon"], connectivityPairs[i, "hexagonsToLat"])
    routes_sl <- gcIntermediate(matrix(pointFrom, ncol = 2), matrix(pointTo, ncol = 2), n = 100, addStartEnd = TRUE, sp = TRUE, breakAtDateLine = TRUE)
    lineConnections <- c(lineConnections,list(routes_sl))
    
  }
  
  lineConnections <- st_as_sf(do.call(rbind,lineConnections))
  lineConnections$Value <- connectivityPairs[, "Value"]
  
  suppressWarnings( st_crs(lineConnections) <- 4326 )
  
  coords <- st_as_sf(data.frame(coords[,c("X","Y")]), coords = c("X", "Y"), crs = 4326)
  
  if( print ) { 
    
    # Plot hexagonCells
    plot1 <- ggplot() + 
      geom_sf(data = coords, color = "#000000", fill = "#000000", size=1.5) +
      geom_sf(data = lineConnections, aes(color = Value), show.legend = TRUE, linewidth=0.5) +
      scale_color_gradient2(low = "#bdcde9", mid="#ffe600", high = "#990b0b", midpoint=(range(lineConnections$Value)[2] - range(lineConnections$Value)[1]) / 2) +
      geom_sf(data = coords, color = "#000000", fill = "#000000", size=1) +
      theme_minimal() +
      coord_sf()
    print(plot1)
    
  }
  
  options(warn=0)
  
  return(list(mappingData=connectivityPairs,lineConnections=lineConnections, objCentroid=coords))
  
}

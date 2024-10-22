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
#' @param print (Optional) A logical flag. If 'TRUE', the map is displayed directly. Defaults to 'FALSE'.
#'
#' @return A list containing:
#'   * 'mappingData': The input 'connectivityPairs' data frame, augmented with longitude ('hexagonsFromLon', 'hexagonsToLon') and latitude ('hexagonsFromLat', 'hexagonsToLat') coordinates for the hexagons.
#'   * 'lineConnections': An 'sf' object representing the line connections between hexagons, with the 'Value' column used for visualization.
#'   * 'hexagonCells': An 'sf' object containing the hexagon polygons involved in the connections.
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
#' @importFrom sf st_centroid st_coordinates st_as_sf 
#' @importFrom geosphere gcIntermediate
#' @importFrom ggplot2 ggplot geom_sf scale_color_gradient2 coord_sf theme_minimal
#' @importFrom dplyr %>% 
#' @export mapConnectivity

mapConnectivity <- function(connectivityPairs=NULL, print=FALSE) {
  
  if( is.null(connectivityEvents)) { stop("The connectivityPairs parameter is required.") }
  if( class(connectivityPairs)[1] != "data.frame" ) { stop("The connectivityPairs parameter must be of class data.frame.") }
  
  cat("\n")
  cat("# ---------------------------------------------","\n")
  cat("Map connectivity between pairs of sites","\n")
  cat("# ---------------------------------------------","\n")
  
  data("referenceTable")
  data("hexagonCells")
  
  options(warn=-1)
  
  # Get pairwise connectivity between sites
  connectivityPairs <- connectivityPairs[connectivityPairs$From != connectivityPairs$To, ]
  connectivityPairs <- connectivityPairs[connectivityPairs$Value != 0, ]
  connectivityPairs$hexagonsFromLon <- NA
  connectivityPairs$hexagonsFromLat <- NA
  connectivityPairs$hexagonsToLon <- NA
  connectivityPairs$hexagonsToLat <- NA
  
  # Compute matching pairs
  
  hexagonCellsCoord <- data.frame(st_coordinates( st_centroid(hexagonCells) ))
  hexagonCellsCoord$ID <- hexagonCells$ID
  
  connectivityPairs[, "hexagonsFromLon"] <- hexagonCellsCoord[match(connectivityPairs$FromHexagon,hexagonCellsCoord$ID),1]
  connectivityPairs[, "hexagonsFromLat"] <- hexagonCellsCoord[match(connectivityPairs$FromHexagon,hexagonCellsCoord$ID),2]
  connectivityPairs[, "hexagonsToLon"] <- hexagonCellsCoord[match(connectivityPairs$ToHexagon,hexagonCellsCoord$ID),1]
  connectivityPairs[, "hexagonsToLat"] <- hexagonCellsCoord[match(connectivityPairs$ToHexagon,hexagonCellsCoord$ID),2]
  
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
  
  st_crs(lineConnections) <- 4326
  
  hexagonCells.i <- hexagonCells[hexagonCells$ID %in% unique(c(connectivityPairs$FromHexagon,connectivityPairs$ToHexagon)), 1]
  options(warn = 0)
  
  if( print ) { 
    
    # Plot hexagonCells
    plot1 <- ggplot() + 
      geom_sf(data = hexagonCells.i, color = "#000000", fill = "#000000", size=1.5) +
      geom_sf(data = lineConnections, aes(color = Value), show.legend = TRUE, linewidth=0.5) +
      scale_color_gradient2(low = "#bdcde9", mid="#ffe600", high = "#990b0b", midpoint=(range(lineConnections$Value)[2] - range(lineConnections$Value)[1]) / 2) +
      geom_sf(data = hexagonCells.i, color = "#000000", fill = "#f3a53e", size=1) +
      theme_minimal() +
      coord_sf()
    print(plot1)
    
  }
  
  options(warn=0)
  
  return(list(mappingData=connectivityPairs,lineConnections=lineConnections, hexagonCells=hexagonCells.i))
  
}

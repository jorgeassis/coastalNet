#' Visualize Pairwise Connectivity on a Map
#'
#' This function takes pairwise connectivity data and generates a map visualization highlighting the connections and their associated strengths. It requires external spatial data (`referenceTable`, `hexagonCells`). 
#'
#' @param connectivityPairs A data frame containing pairwise connectivity results, ideally generated by the 'getPairwiseConnectivity' function. Should include columns: 'From', 'To', 'FromHexagon', 'ToHexagon', and 'Value'.
#' @param print (Optional) A logical flag. If TRUE, displays the map visualization directly. Defaults to FALSE.
#'
#' @return  A list containing:
#'  * **mappingData:** The input 'connectivityPairs' data frame, augmented with longitude and latitude coordinates.
#'  * **lineConnections:** An 'sf' object representing the connections to be plotted.
#'  * **hexagonCells:** An 'sf' object of hexagons involved in the visualized connections.
#'
#' @examples
#' \dontrun{
#' # Assuming you have results from 'getPairwiseConnectivity' stored in 'pw_results'
#' map_data <- mapConnectivity(connectivityPairs = pw_results$connectivityPairs)
#' 
#' # To display the map:
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

    for( i in 1:nrow(connectivityPairs)) {

        locFrom <- st_coordinates(st_centroid(hexagonCells[hexagonCells$ID == connectivityPairs$FromHexagon[i],1]))
        locTo <- st_coordinates(st_centroid(hexagonCells[hexagonCells$ID == connectivityPairs$ToHexagon[i],1]))
        connectivityPairs[i,"hexagonsFromLon"] <- locFrom[1]
        connectivityPairs[i,"hexagonsFromLat"] <- locFrom[2]
        connectivityPairs[i,"hexagonsToLon"] <- locTo[1]
        connectivityPairs[i,"hexagonsToLat"] <- locTo[2]
    }
    
    connectivityPairs <- connectivityPairs[sort(connectivityPairs$Value, index.return=T, decreasing = FALSE)$ix,]
        
    for( i in 1:nrow(connectivityPairs) ){
        pointFrom <- c(connectivityPairs[i,"hexagonsFromLon"],connectivityPairs[i,"hexagonsFromLat"])
        pointTo <- c(connectivityPairs[i,"hexagonsToLon"],connectivityPairs[i,"hexagonsToLat"])
        routes_sl <- gcIntermediate(matrix(pointFrom,ncol=2),matrix(pointTo,ncol=2),n = 100, addStartEnd = TRUE, sp = TRUE, breakAtDateLine=TRUE)
        routes_sl <- st_as_sf(routes_sl)
        routes_sl <- merge(routes_sl, data.frame(Value=connectivityPairs[i,"Value"]))
        
        if( i == 1){ lineConnections <- routes_sl }
        if( i != 1){ lineConnections <- rbind(lineConnections,routes_sl) }
    }
    
    if( print ) { 
                
        hexagonCellsID <- getHexagonID(lineConnections, level="extent", buffer=2, print=FALSE)
        hexagonCells <- hexagonCells[hexagonCells$ID %in% unique(unlist(hexagonCellsID)),1]

        # Plot hexagonCells
        plot1 <- ggplot() + 
            geom_sf(data = hexagonCells, color = "black", fill = "#f3a53e") +
            geom_sf(data = lineConnections, aes(color = Value), show.legend = TRUE, linewidth=0.5) +
            scale_color_gradient2(low = "#bdcde9", mid="#ffe600", high = "#990b0b", midpoint=(range(lineConnections$Value)[2] - range(lineConnections$Value)[1]) / 2) +
            geom_sf(data = hexagonCells[ hexagonCells$ID %in% unique(unlist(connectivityPairs[,c("From","To")])),], color = "#000000", fill = "#000000") +
            theme_minimal() +
            coord_sf()

        print(plot1)
        
    }

    hexagonCellsID <- getHexagonID(lineConnections, level="site", buffer=0, print=FALSE)
    hexagonCells <- hexagonCells[hexagonCells$ID %in% unlist(hexagonCellsID),1]

    options(warn=0)
    
    return(list(mappingData=connectivityPairs,lineConnections=lineConnections, hexagonCells=hexagonCells))
    
}

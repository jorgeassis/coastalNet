#' Maps connectivity between pairs of sites
#'
#' This function maps the connectivity between pairs of sites based on pairwise connectivity estimates and coordinate sites.
#'
#' @param connectivityPairs a data.frame containing probability / time estimates of connectivity between pairs of sites.
#' @param obj a data.frame containing the coordinates of the sites.
#' @param print a logical value indicating whether to print the mapped pairwise connectivity. Default is FALSE.
#' @return a data.frame matching the pairwise connectivity data.frame with the coordinates of sites and a polygon of class sf with line connections between pairs of sites.
#' @import geosphere sf ggplot2
#' @export

mapConnectivity <- function(connectivityPairs=NULL, obj=NULL, print=FALSE) {
    
    if( is.null(connectivityEvents)) { stop("The connectivityPairs parameter is required.") }
    if( is.null(obj)) { stop("The obj parameter is required.") }

    if( class(obj)[1] != "data.frame" ) { stop("The obj parameter must be of class data.frame.") }
    if( class(connectivityPairs)[1] != "data.frame" ) { stop("The connectivityPairs parameter must be of class data.frame.") }

    data("referenceTable")
    data("hexagonCells")

    options(warn=-1)
    
    # Get pairwise connectivity between sites
    connectivityPairs <- connectivityPairs[connectivityPairs$hexagonsFrom != connectivityPairs$hexagonsTo, ]
    connectivityPairs <- connectivityPairs[connectivityPairs$value != 0, ]
    connectivityPairs$hexagonsFromLon <- NA
    connectivityPairs$hexagonsFromLat <- NA
    connectivityPairs$hexagonsToLon <- NA
    connectivityPairs$hexagonsToLat <- NA

    # Compute matching pairs

    for( i in 1:nrow(connectivityPairs)) {

        locFrom <- st_coordinates(st_centroid(hexagonCells[hexagonCells$ID == connectivityPairs$hexagonsFrom[i],1]))
        locTo <- st_coordinates(st_centroid(hexagonCells[hexagonCells$ID == connectivityPairs$hexagonsTo[i],1]))
        connectivityPairs[i,"hexagonsFromLon"] <- locFrom[1]
        connectivityPairs[i,"hexagonsFromLat"] <- locFrom[2]
        connectivityPairs[i,"hexagonsToLon"] <- locTo[1]
        connectivityPairs[i,"hexagonsToLat"] <- locTo[2]
    }
    
    connectivityPairs <- connectivityPairs[sort(connectivityPairs$value, index.return=T, decreasing = FALSE)$ix,]
        
    for( i in 1:nrow(connectivityPairs) ){
        pointFrom <- c(connectivityPairs[i,"hexagonsFromLon"],connectivityPairs[i,"hexagonsFromLat"])
        pointTo <- c(connectivityPairs[i,"hexagonsToLon"],connectivityPairs[i,"hexagonsToLat"])
        routes_sl <- gcIntermediate(matrix(pointFrom,ncol=2),matrix(pointTo,ncol=2),n = 100, addStartEnd = TRUE, sp = TRUE, breakAtDateLine=TRUE)
        routes_sl <- st_as_sf(routes_sl)
        routes_sl <- merge(routes_sl, data.frame(value=connectivityPairs[i,"value"]))
        
        if( i == 1){ lineConnections <- routes_sl }
        if( i != 1){ lineConnections <- rbind(lineConnections,routes_sl) }
        
    }
    
    if(print) { 
                
        hexagonCellsID <- getHexagonID(lineConnections, level="extent", buffer=1, print=FALSE)
        hexagonCells <- hexagonCells[hexagonCells$ID %in% hexagonCellsID,1]

        # Plot hexagonCells
        plot1 <- ggplot() + 
            geom_sf(data = hexagonCells, color = "black", fill = "#f3a53e") +
            geom_sf(data = lineConnections, aes(color = value), show.legend = TRUE, linewidth=0.5) +
            scale_color_gradient2(low = "#bdcde9", mid="#ffe600", high = "#990b0b", midpoint=(range(lineConnections$value)[2] - range(lineConnections$value)[1]) / 2) +
            geom_sf(data = hexagonCells[ hexagonCells$ID %in% unique(unlist(connectivityPairs[,c("hexagonsFrom","hexagonsTo")])),], color = "#000000", fill = "#000000") +
            theme_minimal() +
            coord_sf()

        print(plot1)
        
    }

    options(warn=0)
    
    return(list(mappingData=connectivityPairs,lineConnections=lineConnections))
    
}
#' Get the hexagon IDs based on a given spatial object and detail level
#' 
#' \code{getHexagonID} This function retrieves the hexagon IDs (i.e., coastal sites) given spatial object and detail level.
#' 
#' @param obj a spatial object of class (1) a matrix or data.frame with Longitude and Latitude coordinates (WGS84), (2) a numeric vector with c(minLon,maxLon,minLat,maxLat), (3) a polygon (sf) or (4) raster (SpatRaster) layer.
#' @param level the level of detail. Can be "extent" or "site". For the "extent" level, the function returns the hexagon IDs that intersect with the extent of the spatial object. For the "site" level, the function returns the hexagon IDs that are closest to the points or polygons of the spatial object. Default is "extent". 
#' @param buffer a value defining the buffer distance (in decimal degrees) to expand the extent of the spatial object. Default is 0.
#' @param print a logical value indicating whether to print the hexagon cells. Default is FALSE.
#'
#' @import terra sf FNN ggplot2 raster
#' @export

getHexagonID <- function(obj, level="extent", buffer=0, print=FALSE){

    data("referenceTable")

    if( ! exists("referenceTable")) { stop("The reference table is not available. Please, run the getDataBase function.") }

    if( class(obj)[1] == "RasterLayer" ) { obj <- rast(obj) }

    options(warn=-1)
    
    if( level == "extent") {
        
        if( ! class(obj)[1] %in% c("SpatRaster","sf","data.frame","matrix","numeric") ) { stop("obj needs to be of class numeric, data.frame, matrix, sf or SpatRaster.") }

        if( class(obj)[1] == "SpatRaster" ) { obj <- as.numeric(as.vector(ext(obj))) }
        
        if( class(obj)[1] == "sf" ) { obj <- as.numeric(st_bbox(obj) )[c(1,3,2,4)] }
 
        if( class(obj)[1] == "Extent" ) { obj <- c(obj[1],obj[2],obj[3],obj[4]) }
        
        if( class(obj)[1] == "matrix" | class(obj)[1] == "data.frame" ) { obj <- c(min(obj[,1]),max(obj[,1]),min(obj[,2]),max(obj[,2])) }
        
        if( class(obj)[1] == "numeric" ) { 
        
        cellID <- referenceTable[ referenceTable$longitude >= obj[1] - buffer &
                                  referenceTable$longitude <= obj[2] + buffer &
                                  referenceTable$latitude >= obj[3] - buffer &
                                  referenceTable$latitude <= obj[4] + buffer , "cellID"]
        }
        
    }
    
    if( level == "site") {
        
        if( ! class(obj)[1] %in% c("SpatRaster","sf","data.frame","matrix") ) { stop("obj needs to be of class data.frame, matrix, sf or SpatRaster.") }

        if( class(obj)[1] == "SpatRaster" ) { obj <- crds(obj, na.rm=TRUE, df=TRUE) }
        
        if( class(obj)[1] == "sf" ) { obj <- data.frame(st_coordinates(st_centroid(obj))) }
        
        if( class(obj)[1] == "data.frame" | class(obj)[1] == "matrix" ) { 
        
        closestCells <- get.knnx( as.matrix(referenceTable[,2:3]) , as.matrix(obj), k=1 , algorithm="kd_tree" )$nn.index
        cellID <- referenceTable[ closestCells , "cellID"]
        
        }
        
    }
    
    if(print) { 
                
        data("hexagonCells")
        hexagonCells <- hexagonCells[hexagonCells$ID %in% cellID, 1]

        # Plot hexagonCells
        plot1 <- ggplot() + geom_sf(data = hexagonCells, color="black", fill = "#f3a53e") +
                            theme_minimal() +   
                            theme(legend.position = "none") +
                            coord_sf()

        print(plot1)
        
    }
        
    options(warn=0)
    
    return(cellID)
  
}
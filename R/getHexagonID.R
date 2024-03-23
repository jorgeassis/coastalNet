#' Hexagon IDs for Spatial Objects or Coordinates
#'
#' \code{getHexagonID} This versatile function determines the hexagon IDs from the coastalNet database, based on a provided spatial object or coordinates. It offers three extraction levels: 'extent', 'site', and 'centroid'. The function also provides a visualization option for selected hexagons.
#'
#' @param obj A spatial object of class 'SpatRaster', 'sf', 'data.frame', or 'matrix', or a vector of coordinates.
#' @param level  The extraction level. Options are:
#'    * 'extent':  Extract hexagon IDs based on the extent of the input object.
#'    * 'site':  Derive hexagon IDs from point locations within the object.
#'    * 'centroid':  Obtain hexagon IDs based on the spatial object's centroid.
#' @param buffer (Optional) A numeric value to expand the area considered when extracting hexagon IDs at the 'extent' level. Defaults to 0 (no buffer).
#' @param print (Optional) A logical flag. If TRUE, creates a map visualization highlighting the selected hexagons. Defaults to FALSE.
#'
#' @return A list of hexagon IDs. If 'obj' contains multiple points (e.g., 'sf' object), the list elements correspond to each input point.

#' @examples
#' \dontrun{
#' # Example: RasterLayer object (extent-based)
#' my_raster <- raster(extent(0, 10, 5, 15)) 
#' hex_ids <- getHexagonID(my_raster, level = "extent")
#'
#' # Example: A data frame of coordinates (site-based)
#' sites <- data.frame(longitude = c(-8.5, -9.2), latitude = c(37.1, 38.5))
#' hex_ids <- getHexagonID(sites, level = "site")
#' 
#' # Example: 'sf' object (centroid-based with visualization)
#' my_polygons <- st_read("path/to/my_polygons.shp") 
#' hex_ids <- getHexagonID(my_polygons, level = "centroid", print = TRUE)
#' }
#' 
#' @importFrom sf st_centroid st_intersects st_coordinates st_crs st_as_sf st_bbox
#' @importFrom graphics plot 
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_minimal theme
#' @importFrom FNN get.knnx
#' @importFrom dplyr %>% 
#' @importFrom terra crds ext
#' 
#' @export getHexagonID 

getHexagonID <- function(obj, level="extent", buffer=0, print=FALSE){

    data("referenceTable")

    cat("\n")
    cat("# ---------------------------------------------","\n")
    cat("# Get the hexagon IDs based on spatial object","\n")

    options(warn=-1)
    
    if( class(obj)[1] == "RasterLayer" ) { obj <- rast(obj) }
    if( class(obj)[1] == "data.frame" ) { if ( ncol(obj) > 2 ) { siteNames <- obj[,3] } }
    if( class(obj)[1] == "sf" ) {
      
              siteNames <- as.data.frame(obj)[,1]
             if( class(siteNames)[1] == "numeric" ) { 
               siteNames <- as.character(1:length(siteNames))
    
                              }  }
    
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

        if( class(obj)[1] == "SpatRaster" ) { 

          obj <- crds(obj, na.rm=TRUE, df=TRUE)
          closestCells <- get.knnx( as.matrix(referenceTable[,2:3]) , as.matrix(obj), k=1 , algorithm="kd_tree" )$nn.index
          cellID <- referenceTable[ closestCells , "cellID"]
          cellID <- unique(cellID)

        }
                
        if( class(obj)[1] == "data.frame" | class(obj)[1] == "matrix" ) { 
          
          closestCells <- get.knnx( as.matrix(referenceTable[,2:3]) , as.matrix(obj), k=1 , algorithm="kd_tree" )$nn.index
          cellID <- referenceTable[ closestCells , "cellID"]
          
        }

        if( class(obj)[1] == "sf" ) { 
          
          referenceTableSP <- st_as_sf(referenceTable,coords=c("longitude", "latitude"), crs = 4326)
          cellID <- list()
          st_crs(obj) <- st_crs(referenceTableSP)
          
          for( j in 1:nrow(obj)) {

              obj.j <- obj[j,]
              suppressMessages(intersected <- st_intersects(referenceTableSP, obj.j))
              intersected <- data.frame(intersected)

              if( nrow(intersected) > 0) { cellID <- c(cellID, list(referenceTable[intersected[,1], "cellID"] )) }

              if( nrow(intersected) == 0) { 
                                
                closestCells <- get.knnx( as.matrix(referenceTable[,2:3]) , as.matrix( st_coordinates(st_centroid( obj.j )) ), k=1 , algorithm="kd_tree" )$nn.index
                cellID <- c(cellID, list( referenceTable[ closestCells , "cellID"] ) ) 
                
              }

          }

        }

    }
    
    if( level == "centroid" ) {
        
        if( ! class(obj)[1] %in% c("SpatRaster","sf","data.frame","matrix") ) { stop("obj needs to be of class data.frame, matrix, sf or SpatRaster.") }

        if( class(obj)[1] == "sf" ) { 
          obj <- st_centroid(obj)
          obj <- data.frame(st_coordinates(obj))
        }
        
        if( class(obj)[1] == "SpatRaster" ) { 
          obj <- crds(obj, na.rm=TRUE, df=TRUE) 
          obj <- data.frame( x = mean(obj$x), y = mean(obj$y) )
        }
                
        if( class(obj)[1] == "data.frame" | class(obj)[1] == "matrix" ) { 
        
        closestCells <- get.knnx( as.matrix(referenceTable[,2:3]) , as.matrix(obj), k=1 , algorithm="kd_tree" )$nn.index
        cellID <- referenceTable[ closestCells , "cellID"]
        
        }
        
    }
    
    if( print ) { 
                
        data("hexagonCells")
        hexagonCellsID <- hexagonCells[hexagonCells$ID %in% unlist(cellID), 1]

        plot1 <- ggplot() + geom_sf(data = hexagonCells, color="#b4b4b4", fill = "#b4b4b4") +
                            geom_sf(data = hexagonCellsID, color="black", fill = "black") +
                            coord_sf(crs= "+proj=robin") +
                            theme_minimal() +   
                            theme(legend.position = "none")

        print(plot1)
        
    }
        
    options(warn=0)
    
    if( class(cellID) != "list" & (class(obj)[1] == "integer" | class(obj)[1] == "SpatRaster" | class(obj)[1] == "numeric" | class(obj)[1] == "data.frame" | class(obj)[1] == "matrix") ) { cellID <- as.list(cellID) }
    if( class(cellID) != "list" & level == "centroid" ) { cellID <- as.list(cellID) }
    
    if( exists("siteNames") ) { if( length(siteNames) == length(cellID) ) { names(cellID) <- siteNames } }
    
    cat("# \n")
    cat("# Number of hexagon sites:",length( unique(unlist(cellID)) ),"\n")

    return(cellID)
  
}

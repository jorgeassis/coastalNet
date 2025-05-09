#' Find Hexagon IDs for Spatial Objects or Coordinates
#'
#' \code{getHexagonID} This function determines the hexagon IDs from the 'coastalNet' database based on a provided spatial object or coordinates. It offers flexibility in how you define the spatial input and how the hexagon IDs are extracted. 
#'
#' @param obj A spatial object from which to extract hexagon IDs. It can be one of the following classes:
#'   * 'SpatRaster': A raster object from the 'terra' package.
#'   * 'sf': An 'sf' object representing points, lines, or polygons.
#'   * 'data.frame': A data frame with at least two columns representing longitude and latitude coordinates.
#'   * 'matrix': A matrix with at least two columns representing longitude and latitude coordinates.
#'   * 'numeric': A numeric vector of length 4 representing the extent of an area (xmin, xmax, ymin, ymax).
#' @param level A character string specifying the extraction level. Options are:
#'   * 'extent': Extract hexagon IDs that are within with the bounding box of the input object (potentially expanded by the 'buffer').
#'   * 'site': Extract hexagon IDs that are within the individual spatial objects (typically points, or polygons) of the input object. 
#'   * 'centroid': Extract the hexagon ID that is the centroid of the spatial objects (typically polygons) of the input object
#' @param hexagonCells An sf object defining the hexagons that define the source and sink locations of connectivity retrieved from loadHexagons function.
#' @param groupByFeature Logical. If TRUE, hexagon IDs are grouped or aggregated by each input spatial feature (typically polygons). Defaults to TRUE.
#' @param groupByFeatureName The column name of the attribute table of the object spatial object from which to extract hexagon IDs.
#' @param buffer (Optional) A numeric value indicating the buffer distance (in degrees) to expand the bounding box when 'level = "extent"'. Defaults to 0 (no buffer).
#' @param print (Optional) A logical flag. If 'TRUE', a map visualizing the selected hexagons is displayed. Defaults to 'FALSE'.
#'
#' @return A list of hexagon IDs (character). If 'obj' contains multiple geometries (e.g., an 'sf' object with multiple features), each element of the list corresponds to the hexagon IDs associated with each feature. If no hexagons are found, an empty list is returned.
#'
#' @examples
#' \dontrun{
#' # Example: SpatRaster object (extent-based)
#' library(terra) # Ensure the 'terra' package is loaded
#' my_raster <- rast(extent(0, 10, 5, 15))
#' hex_ids <- getHexagonID(my_raster, level = "extent")
#' 
#' # Example: data.frame of coordinates (site-based)
#' sites <- data.frame(longitude = c(-8.5, -9.2), latitude = c(37.1, 38.5))
#' hex_ids <- getHexagonID(sites, level = "site")
#'
#' # Example: 'sf' object (centroid-based with visualization)
#' # (Ensure 'sf' package is loaded and you have example polygon data)
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

getHexagonID <- function(obj, level="extent", hexagonCells=NULL, groupByFeature=TRUE, groupByFeatureName=NULL, buffer=0, print=FALSE){
  
  if( is.null(hexagonCells)) { stop("The hexagonCells parameter is missing.")}
  if( ! "sf" %in% class(hexagonCells) ) { stop("The hexagonCells object is not of class sf.")}
  if( ! "ID" %in% names(hexagonCells) ) { stop("The hexagonCells object does not contain the required ID infomraiton on the attribute table.")}
  
  cat("\n")
  cat("# ---------------------------------------------","\n")
  cat("# Get the hexagon IDs based on spatial object","\n")
  
  options(warn=-1)
  
  hexagonCellsCoords <- st_coordinates(st_centroid(hexagonCells))
  
  referenceTable <- data.frame( cellID = as.data.frame(hexagonCells)[,"ID"],
                                longitude = hexagonCellsCoords[,"X"],
                                latitude = hexagonCellsCoords[,"Y"])
  
  if( class(obj)[1] == "data.frame" ) { if ( ncol(obj) > 2 ) { siteNames <- obj[,3] } }
  
  if( class(obj)[1] == "sfc_POLYGON" | class(obj)[1] == "sfc" ) { obj <- st_sf(obj) }
  
  if( class(obj)[1] == "sf" ) {
    
    siteNames <- as.data.frame(obj)[,1] 
    
    if( !is.null(groupByFeatureName) ) { siteNames <- as.data.frame(obj)[,groupByFeatureName] }
    
    if( class(siteNames)[1] == "numeric" ) { 
      siteNames <- as.character(1:length(siteNames))
      
    }  }
  
  if( level == "extent" ) {
    
    if( ! class(obj)[1] %in% c("sfc","sfc_POLYGON","SpatRaster","sf","data.frame","matrix","numeric") ) { stop("obj needs to be of class numeric, data.frame, matrix, sf or SpatRaster.") }
    
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
    
    if( ! class(obj)[1] %in% c("sfc","sfc_POLYGON","SpatRaster","sf","data.frame","matrix") ) { stop("obj needs to be of class data.frame, matrix, sf or SpatRaster.") }
    
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
      
      sf_use_s2(FALSE)
      referenceTableSP <- st_as_sf(referenceTable,coords=c("longitude", "latitude"), crs = 4326)
      
      cellID <- list()
      st_crs(obj) <- st_crs(referenceTableSP)
      referenceTableSP <- st_make_valid(referenceTableSP)
      obj <- st_make_valid(obj)
      
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
  
  if( ! groupByFeature ) { cellID <- sapply(unlist(cellID),function(x) as.list(x)) }
  
  if(is.null(names(cellID))) { names(cellID) <- as.character(1:length(cellID)) }
  
  cat("# \n")
  cat("# Number of hexagon sites:",length( unique(unlist(cellID)) ),"\n")
  
  cat("# Hexagon sites grouped by feature:",groupByFeature,"\n")
  cat("# Number of features:",length(cellID),"\n")
  
  return(cellID)
  
}

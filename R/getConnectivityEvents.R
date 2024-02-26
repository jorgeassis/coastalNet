#' Get connectivity events based on specified criteria
#'
#' \code{getConnectivityEvents} This function retrieves connectivity events from the database based on the specified criteria.
#'
#' @param hexagonID a vector of hexagon IDs.
#' @param year a vector of years to filter the events by. Default is NULL.
#' @param month a vector of months to filter the events by. Default is NULL.
#' @param day a vector of days to filter the events by. Default is NULL.
#' @param period A numeric vector with one or two elements representing the time period in which the events occurred. Default is 180.
#'   If a single value is provided, events with travel time less than or equal to the period will be returned.
#'   If two values are provided, events with travel time between the two values (inclusive) will be returned.
#'
#' @return A subset of the oceanographicConnectivity database containing the selected connectivity events.
#'
#' @export

getConnectivityEvents <- function(hexagonID, year=NULL, month=NULL, day=NULL, period=180 ){

    if( ! exists("oceanographicConnectivity")) { stop("The object oceanographicConnectivity is not available. Please, run the getDataBase function.") }

    if( is.null(hexagonID)) { stop("The hexagonFromID parameter is required.") }

    if( length(period) != 1 & length(period) != 2 ) { stop("Period should be a unique value or a numeric vector with two elements.") }
    if( length(period) == 2 ) { if( period[1] >= period[2] ) { stop("The first element of the period vector should be smaller than the second element.") } }
    
    if( ! is.null(year) ) { if( ! all(year %in% unique(oceanographicConnectivity$connectivityEventStartYear)) ) { stop("The year(s) provided are not available in the database.") } }
    if( ! is.null(month) ) { if( ! all(month %in% unique(oceanographicConnectivity$connectivityEventStartMonth)) ) { stop("The month(s) provided are not available in the database.") } }
    if( ! is.null(day) ) { if( ! all(day %in% unique(oceanographicConnectivity$connectivityEventStartDay)) ) { stop("The day(s) provided are not available in the database.") } }

    options(warn=-1)
    
    hexagonID <- unique(hexagonID)
    
    oceanographicConnectivitySubset <- oceanographicConnectivity[oceanographicConnectivity$connectivityEventStartHexagon %in% hexagonID | oceanographicConnectivity$connectivityEventEndHexagon %in% hexagonID, ]
    
    if( length(period) == 1 ) { oceanographicConnectivitySubset <- oceanographicConnectivitySubset[oceanographicConnectivitySubset$connectivityEventTravelTime <= period , ] } 
    if( length(period) == 2 ) { oceanographicConnectivitySubset <- oceanographicConnectivitySubset[oceanographicConnectivitySubset$connectivityEventTravelTime >= period[1] & oceanographicConnectivitySubset$connectivityEventTravelTime <= period[2] , ] } 

    if( ! is.null(year) ) { oceanographicConnectivitySubset <- oceanographicConnectivitySubset[oceanographicConnectivitySubset$connectivityEventStartYear %in% year , ] }
    if( ! is.null(month) ) { oceanographicConnectivitySubset <- oceanographicConnectivitySubset[oceanographicConnectivitySubset$connectivityEventStartMonth %in% month , ] }
    if( ! is.null(day) ) { oceanographicConnectivitySubset <- oceanographicConnectivitySubset[oceanographicConnectivitySubset$connectivityEventStartDay %in% day , ] }
    
    events <- nrow(oceanographicConnectivitySubset)

    cat("\n")
    cat(paste0("Number of hexagons: ",length(hexagonID) ),"\n")
    cat(paste0("Number of connectivity events: ",events,"\n\n"))
    
    options(warn=0)
    
    return(oceanographicConnectivitySubset)
  
}
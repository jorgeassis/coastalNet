#' Retrieve Oceanographic Connectivity Events by Criteria
#'
#' \code{getConnectivityEvents} This function extracts a subset of oceanographic connectivity events from the main database ('oceanographicConnectivity') based on user-specified criteria including hexagon IDs, time periods (year, month, day), and the duration of events.
#'
#' @param hexagonID A vector of hexagon IDs to filter events.
#' @param year (Optional) A numeric vector of years to limit the results. 
#' @param month (Optional) A numeric vector of months to limit the results.
#' @param day (Optional) A numeric vector of days to limit the results. 
#' @param period  A numeric vector specifying the minimum and maximum duration (days) of events to include.  Can be a single value or a vector of length 2 (e.g., period = c(10, 30)). Maximum value is 180 days.
#'
#' @return A data frame containing the filtered oceanographic connectivity events. 
#'
#' @examples
#' \dontrun{
#' # Example: Filter by a single hexagon and year range
#' my_events <- getConnectivityEvents(hexagonID = "131", year = 2010:2012) 
#' 
#' # Example: Filter by multiple hexagons, month, and duration
#' my_events <- getConnectivityEvents(hexagonID = c("131", "132"), 
#'                                  month = 6, 
#'                                  period = c(15, 45)) 
#' }
#' 
#' @importFrom dplyr %>% 
#' @import data.table
#' @export getConnectivityEvents

getConnectivityEvents <- function(hexagonID, year=NULL, month=NULL, day=NULL, period=180 ){

    if( ! exists("oceanographicConnectivity")) { stop("The object oceanographicConnectivity is not available. Please, run the getDataBase function.") }

    if( is.null(hexagonID)) { stop("The hexagonFromID parameter is required.") }

    hexagonID <- unique(unlist(hexagonID))

    if( length(period) != 1 & length(period) != 2 ) { stop("Period should be a unique value or a numeric vector with two elements.") }
    if( length(period) == 2 ) { if( period[1] >= period[2] ) { stop("The first element of the period vector should be smaller than the second element.") } }
    
    fromYear <- as.numeric(oceanographicConnectivity[1,"connectivityEventStartYear"])
    toYear <- as.numeric(oceanographicConnectivity[nrow(oceanographicConnectivity),"connectivityEventStartYear"])
    
    if( ! is.null(year) ) { if( ! all(year %in% fromYear:toYear) ) { stop("The year(s) provided are not available in the database.") } }
    if( ! is.null(month) ) { if( ! all(month %in% 1:12) ) { stop("The month(s) provided are not available in the database.") } }
    if( ! is.null(day) ) { if( ! all(day %in% 1:31) ) { stop("The day(s) provided are not available in the database.") } }

    cat("\n")
    cat("# ---------------------------------------------","\n")
    cat("# Get connectivity events based on criteria","\n")
    
    cat("# Hegaxon sites:",length(hexagonID),"\n")
    cat("# Years:",ifelse(!is.null(year),year,paste0(fromYear,"-",toYear)),"\n")
    cat("# Months:",ifelse(!is.null(month),month,paste0(1,"-",12)),"\n")
    cat("# Days:",ifelse(!is.null(day),day,paste0(1,"-",31)),"\n")
    cat("# Period:",period,"\n")
    cat("# ---------------------------------------------","\n")

    if( length(period) == 1 ) { period <- c(0,period) } 
    
    options(warn=-1)
        
    setindex(oceanographicConnectivity, connectivityEventStartHexagon)
    oceanographicConnectivity <- oceanographicConnectivity[connectivityEventStartHexagon %in% hexagonID | connectivityEventEndHexagon %in% hexagonID, ]
    oceanographicConnectivity <- oceanographicConnectivity[connectivityEventTravelTime >= period[1] & connectivityEventTravelTime <= period[2] , ]

    if( ! is.null(year) ) { oceanographicConnectivity <- oceanographicConnectivity[connectivityEventStartYear %in% year , ] }
    if( ! is.null(month) ) { oceanographicConnectivity <- oceanographicConnectivity[connectivityEventStartMonth %in% month , ] }
    if( ! is.null(day) ) { oceanographicConnectivity <- oceanographicConnectivity[connectivityEventStartDay %in% day , ] }
    
    cat("\n")
    cat(paste0("Number of hexagons: ",length(hexagonID) ),"\n")
    cat(paste0("Number of connectivity events: ",nrow(oceanographicConnectivity),"\n\n"))
    
    options(warn=0)
    
    return(oceanographicConnectivity)
  
}

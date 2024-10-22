#' Retrieve Oceanographic Connectivity Events by Criteria
#'
#' \code{getConnectivityEvents} This function extracts a subset of oceanographic connectivity events from the connectivity database based on user-specified criteria. You can filter events by hexagon IDs, time periods (year, month, day), and the duration of events.
#'
#' @param connectivity A data.table object containing the oceanographic connectivity database. This should be the result of calling 'getDataBase()' or a similar data loading function.
#' @param hexagonID A vector of hexagon IDs to filter events.
#' @param year (Optional) A numeric vector of years to limit the results.
#' @param month (Optional) A numeric vector of months (1-12) to limit the results.
#' @param day (Optional) A numeric vector of days (1-31) to limit the results.
#' @param period (Optional) A numeric vector specifying the minimum and maximum duration (fraction of days) of events to include. It can be a single value (maximum duration) or a vector of length 2 (e.g., 'c(10, 30)'). The maximum allowed value is 180 days. Defaults to 180 (no duration restriction).
#'
#' @return A data frame containing the filtered oceanographic connectivity events. 
#'
#' @examples
#' \dontrun{
#' # Load the database (replace with your actual data loading code)
#' connectivity <- getDataBase() 
#' 
#' # Example: Filter by a single hexagon and year range
#' my_events <- getConnectivityEvents(connectivity, hexagonID = "131", year = 2010:2012)
#'
#' # Example: Filter by multiple hexagons, month, and duration
#' my_events <- getConnectivityEvents(connectivity, hexagonID = c("131", "132"), month = 6, period = c(15, 45))
#' }
#' 
#' @importFrom dplyr %>% 
#' @import data.table
#' @export getConnectivityEvents

getConnectivityEvents <- function(connectivity=NULL, hexagonID=NULL, year=NULL, month=NULL, day=NULL, period=180 ){
  
  if( is.null(connectivity) ) { stop("The object connectivity is not available. Please, import it to the environment using the getDataBase function.") }
  
  if( is.null(hexagonID) ) { stop("The hexagonFromID parameter is required.") }
  
  hexagonID <- unique(unlist(hexagonID))
  
  if( length(period) != 1 & length(period) != 2 ) { stop("Period should be a unique value or a numeric vector with two elements.") }
  if( length(period) == 2 ) { if( period[1] >= period[2] ) { stop("The first element of the period vector should be smaller than the second element.") } }
  
  fromYear <- as.numeric(connectivity[1,"connectivityEventStartYear"])
  toYear <- as.numeric(connectivity[nrow(connectivity),"connectivityEventStartYear"])
  
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
  
  setindex(connectivity, connectivityEventStartHexagon)
  connectivity <- connectivity[connectivityEventStartHexagon %in% hexagonID | connectivityEventEndHexagon %in% hexagonID, ]
  connectivity <- connectivity[connectivityEventTravelTime >= period[1] & connectivityEventTravelTime <= period[2] , ]
  
  if( ! is.null(year) ) { connectivity <- connectivity[connectivityEventStartYear %in% year , ] }
  if( ! is.null(month) ) { connectivity <- connectivity[connectivityEventStartMonth %in% month , ] }
  if( ! is.null(day) ) { connectivity <- connectivity[connectivityEventStartDay %in% day , ] }
  
  cat("\n")
  cat(paste0("Number of hexagons: ",length(hexagonID) ),"\n")
  cat(paste0("Number of connectivity events: ",nrow(connectivity),"\n\n"))
  
  options(warn=0)
  
  return(connectivity)
  
}

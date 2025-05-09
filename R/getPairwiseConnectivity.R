#' Calculate Pairwise Connectivity Estimates
#'
#' \code{getPairwiseConnectivity} This function calculates pairwise measures of oceanographic connectivity between sets of hexagons. It offers flexibility in defining the direction of connectivity (forward or backward), the connectivity metric (probability, events, or time), and the consideration of stepping-stone paths.
#'
#' @param connectivityEvents A data.table object containing connectivity events, as obtained from functions like 'getConnectivityEvents'. It must have columns: 
#'  * 'connectivityEventStartHexagon' (character)
#'  * 'connectivityEventEndHexagon' (character)
#'  * 'connectivityEventTravelTime' (numeric, in days) - required if 'value = "Time"'.
#' @param hexagonIDFrom A list of character vectors. Each element of the list represents a spatial feature (a group of hexagon IDs) "from" which particles where sent from.
#' @param hexagonIDTo (Optional) A list of character vectors mirroring the structure of 'hexagonIDFrom', Each element of the list represents a spatial feature (a group of hexagon IDs) "to" which particles produced connectivity events. If not provided, pairwise connectivity is calculated among all pairs of 'hexagonIDFrom'.
#' @param connType A character string specifying the direction of connectivity: "Forward" (default) or "Backward".
#' @param value A character string specifying the connectivity metric: "Probability" (default), "Events", or "Time".
#' @param steppingStone (Optional) A logical flag indicating whether to consider stepping-stone connectivity. Defaults to 'FALSE'.
#' @param nStepStones (Optional) If 'steppingStone' is 'TRUE', an integer specifying the maximum number of steps in a stepping-stone path. If not provided, there is no limit.
#' @param parallelCores (Optional) The number of parallel cores to use for stepping-stone computations (if applicable). If not provided, it defaults to the number of available cores minus 1.
#'
#' @return
#' A list containing the following components:
#'   * 'connectivityPairs': A data.table with pairwise connectivity results, including columns:
#'      - 'From' (character): The name of the "from" region.
#'      - 'To' (character): The name of the "to" region.
#'      - 'FromHexagon' (list of character): The hexagon IDs in the "from" region.
#'      - 'ToHexagon' (list of character): The hexagon IDs in the "to" region.
#'      - 'Value' (numeric): The calculated connectivity value.
#'      - 'Steps' (numeric): If stepping-stone connectivity is used, the number of steps in the path. Otherwise, 1.
#'   * 'connectivityMatrix': A matrix where rows represent "from" regions, columns represent "to" regions, and values are the connectivity estimates.
#'   * 'connectivityGraph': An 'igraph' object representing the connectivity network.
#'   * 'sitesConnected': A character vector of "to" region names that have connections.
#'   * 'sitesNotConnected': A character vector (or 'NA') of "to" region names without connections.
#'
#' @examples
#' \dontrun{
#' # Load your data (replace with your actual data loading code)
#' connectivity_events <- getConnectivityEvents(...) # Assuming you have the necessary data
#'
#' # Example: Basic connectivity (from hexagon 1 to 2)
#' pw_results <- getPairwiseConnectivity(connectivityEvents = connectivity_events,
#'                                      hexagonIDFrom = list(c("1"), c("2")))
#' 
#' # Example: Stepping-stone connectivity, up to 3 steps, with parallel processing
#' pw_results <- getPairwiseConnectivity(connectivityEvents = connectivity_events,
#'                                      hexagonIDFrom = list(c("1")),
#'                                      hexagonIDTo = list(c("2")),
#'                                      steppingStone = TRUE,
#'                                      nStepStones = 3,
#'                                      parallelCores = 2)
#' }
#' 
#' @importFrom igraph graph.edgelist get.shortest.paths delete.edges simplify as.undirected E
#' @import data.table
#' @import parallel doParallel foreach
#' @importFrom dplyr %>%  
#' @export getPairwiseConnectivity

getPairwiseConnectivity <- function(connectivityEvents=NULL, hexagonIDFrom=NULL, hexagonIDTo=NULL, connType="Forward", value="Probability", steppingStone=FALSE, nStepStones=NULL, parallelCores=NULL){
    
    cat("\n")
    cat("# ---------------------------------------------","\n")
    cat("Get pairwise connectivity estimates","\n")
    cat("Connectivity type:",connType,"\n")
    cat("Connectivity estimate:",value,"\n")
    cat("Stepping stone connectivity:",steppingStone,"\n")
    
    if( steppingStone ) { 
      cat("Number of maximum stepping stone events:",ifelse(!is.null(nStepStones),nStepStones,"Unlimited"),"\n") 
      cat("Number of parallel cores:",ifelse(!is.null(parallelCores),parallelCores,detectCores() - 1),"\n")
    }
    
    if( missing(connectivityEvents)) { stop("The connectivityEvents parameter is required.") }
    
    if( class(hexagonIDFrom) != "list" ) { stop("hexagonIDFrom needs to be a list\n")  }
    if( ! is.null(hexagonIDTo)) { if (class(hexagonIDTo) != "list" ) { stop("hexagonIDTo needs to be a list\n")  } }
    
    ## -------------
    
    options(warn=-1)
    
    if( is.null(hexagonIDFrom)) { hexagonIDFrom <- list( unique(connectivityEvents$connectivityEventStartHexagon )) }
    if( is.null(hexagonIDTo)) { hexagonIDTo <- hexagonIDFrom }
    
    cat("From:",length(hexagonIDFrom), "spatial feature(s) /",length(unlist(hexagonIDFrom)),"hexagons","\n")
    cat("To:",length(hexagonIDTo), "spatial feature(s) /",length(unlist(hexagonIDTo)),"hexagons","\n")
    cat("# ---------------------------------------------","\n")
    
    ## ---------------------------------------------------
    
    if( value == "Probability" | value == "Events" ) {
      
      connectivityEvents[, value := 1]
      pairwiseConnectivity <- connectivityEvents[,list(value = sum(value)), by = 'connectivityEventStartHexagon,connectivityEventEndHexagon']
      
      if( connType == "Forward" ) { 
        
        yearMin <- min(connectivityEvents$connectivityEventStartYear)
        yearMax <- max(connectivityEvents$connectivityEventStartYear)
        months <- unique(connectivityEvents$connectivityEventStartMonth)
        days <- unique(connectivityEvents$connectivityEventStartDay)
        
        bd <- as.Date(paste0(min(yearMin),"-01-01"))
        ed <- as.Date(paste0(max(yearMax),"-12-31"))
        seqd <- seq(bd, ed, by="1 day")
        seqd <- seqd[ which( as.numeric(substr(seqd,6,7)) %in% months) ]
        seqd <- seqd[ which( as.numeric(substr(seqd,9,10)) %in% days) ]
        eventsPerCell <- length(seqd)
        
      }
      
      if( connType == "Backward" ) { 
        
        endHexagonEvents <- aggregate(connectivityEvents, value ~ connectivityEventEndHexagon, FUN=sum)
        eventsPerCell <- sapply( pairwiseConnectivity$connectivityEventEndHexagon, function(x) { endHexagonEvents[ endHexagonEvents$connectivityEventEndHexagon == x , 2 ] } )
        
      }
      
    }
    
    if( value == "Probability" ) {
      
      pairwiseConnectivity[, value := value / eventsPerCell ]
      
    }
    
    if( value == "Time" ) {
      
      if( connType == "Forward" ) { 
        pairwiseConnectivity <- connectivityEvents[,list(value = mean(value)), by = 'connectivityEventStartHexagon,connectivityEventEndHexagon']
      }
      
      if( connType == "Backward" ) { 
        pairwiseConnectivity <- connectivityEvents[,list(value = mean(value)), by = 'connectivityEventEndHexagon,connectivityEventStartHexagon']
        pairwiseConnectivity <- pairwiseConnectivity[,.(connectivityEventStartHexagon,connectivityEventEndHexagon,value)]
      }
      
    }
    
    names(pairwiseConnectivity) <- c("hexagonsFrom","hexagonsTo","value")
    pairwiseConnectivity$steps <- 1
    
    ## ---------------------------------------------------
    
    hexagonIDFromUl <- unique(unlist(hexagonIDFrom))
    missingFrom <- hexagonIDFromUl[which( ! hexagonIDFromUl %in% pairwiseConnectivity$hexagonsFrom )]
    if( length(missingFrom) > 0) {
      for( m in missingFrom) {
        pairwiseConnectivity <- rbind(pairwiseConnectivity,pairwiseConnectivity[1,])
        pairwiseConnectivity[nrow(pairwiseConnectivity),"hexagonsFrom"] <- m
        pairwiseConnectivity[nrow(pairwiseConnectivity),"value"] <- 0
      }
    }
    
    hexagonIDToUl <- unique(unlist(hexagonIDTo))
    missingTo <- hexagonIDToUl[which( ! hexagonIDToUl %in% pairwiseConnectivity$hexagonsTo )]
    if( length(missingTo) > 0) {
      for( m in missingTo) {
        pairwiseConnectivity <- rbind(pairwiseConnectivity,pairwiseConnectivity[1,])
        pairwiseConnectivity[nrow(pairwiseConnectivity),"hexagonsTo"] <- m
        pairwiseConnectivity[nrow(pairwiseConnectivity),"value"] <- 0
      }
    }
    
    ## ---------------------------------------------------
    
    if( steppingStone ) {
      
      comb <- pairwiseConnectivity
      comb <- as.data.frame( comb[ sort(comb$value , decreasing = TRUE, index.return =TRUE)$ix , c("hexagonsFrom","hexagonsTo","value")] )
      graph.obj <- graph.edgelist( cbind( as.character( comb[,1]) , as.character(comb[,2]) ) , directed = TRUE )
      
      # Hock, Karlo Mumby, Peter J 2015
      if(value == "Probability") { igraph::E(graph.obj)$weight = ifelse(-log(comb[,3]) == Inf,0,-log(comb[,3])) }
      
      # The wheight has a negative impact on finding the closest path
      if(value == "Time" | value == "Events" ) { igraph::E(graph.obj)$weight = comb[,3] }
      
      graph.obj <- delete.edges(graph.obj, which( igraph::E(graph.obj)$weight == 0) )
      graph.obj <- simplify(graph.obj)
      
      if(is.null(parallelCores)){ numberCores <- detectCores(logical = TRUE) - 1 } else { numberCores <- parallelCores }
      if(numberCores == 0) { numberCores <- 1 }
      
      cl <- makeCluster(numberCores)
      registerDoParallel(cl)
      
      pairwiseConnectivity <- foreach(from=unlist(hexagonIDFrom), .combine = rbind, .verbose=FALSE, .packages=c("igraph","data.table")) %dopar% { 
        
        temp.res <- data.frame()
        
        for( to in unlist(hexagonIDTo) ) {
          
          stones.t <- get.shortest.paths(graph.obj, as.character( from ) , as.character( to ),mode="out")$vpath
          stones.t <- as.numeric(names(stones.t[[1]]))
          stones.t.interm <- cbind(stones.t[-length(stones.t)],stones.t[-1])
          path.values <- apply( stones.t.interm , 1 , function(z) { comb[ comb[,1] == z[1] & comb[,2] == z[2] , 3 ][1] }   )
          
          if( ! is.null(nStepStones)) { if( length(path.values) > nStepStones ) { path.values[] <- 0 }  }
          if( length(path.values) == 0 | sum(path.values) == 0 ) { path.values <- 0 }
          
          res.connectivity.to.steps <- ifelse( sum(path.values) != 0 ,length(path.values) , 0  )
          
          if( value == "Probability") {  path.values <- apply( t(path.values) , 1 , prod )  }
          if( value == "Time" | value == "Events" ) {  path.values <- apply( t(path.values) , 1 , sum )  }
          
          if( from == to ) { path.values <- as.numeric(pairwiseConnectivity[pairwiseConnectivity$hexagonsFrom == from & pairwiseConnectivity$hexagonsTo == to,"value"]) }
          
          temp.res <- rbind(temp.res,
                            data.frame( hexagonsFrom = from,
                                        hexagonsTo = to , 
                                        value = path.values,
                                        steps = res.connectivity.to.steps))
          
        }
        
        temp.res <- temp.res[temp.res$value != 0,]
        return(temp.res)
        
      }
      
      stopCluster(cl) ; rm(cl)
      
      ## --------------
      
      pairwiseConnectivity <- as.data.table(pairwiseConnectivity)
      
    }
    
    ## ---------------------------------------------------
    
    # Restrict to resquested pairs
    
    pairwiseConnectivity <- pairwiseConnectivity[pairwiseConnectivity$hexagonsFrom %in% unique(unlist(hexagonIDFrom)) & pairwiseConnectivity$hexagonsTo %in% unique(unlist(hexagonIDTo)), ]
    
    # Get connected hexagons
    
    hexagonsConnected <- unlist(unique(pairwiseConnectivity[,"hexagonsTo"]))
    names(hexagonsConnected) <- NULL
    hexagonsNotConnected <- unlist(hexagonIDTo)
    hexagonsNotConnected <- hexagonsNotConnected[ ! hexagonsNotConnected %in% hexagonsConnected ]
    names(hexagonsNotConnected) <- NULL
    
    ## ---------------------------------------------------
    
    hexagonIDFromUl <- unique(unlist(hexagonIDFrom))
    missingFrom <- hexagonIDFromUl[which( ! hexagonIDFromUl %in% pairwiseConnectivity$hexagonsFrom )]
    if( length(missingFrom) > 0) {
      for( m in missingFrom) {
        pairwiseConnectivity <- rbind(pairwiseConnectivity,pairwiseConnectivity[1,])
        pairwiseConnectivity[nrow(pairwiseConnectivity),"hexagonsFrom"] <- m
        pairwiseConnectivity[nrow(pairwiseConnectivity),"value"] <- 0
      }
    }
    
    hexagonIDToUl <- unique(unlist(hexagonIDTo))
    missingTo <- hexagonIDToUl[which( ! hexagonIDToUl %in% pairwiseConnectivity$hexagonsTo )]
    if( length(missingTo) > 0) {
      for( m in missingTo) {
        pairwiseConnectivity <- rbind(pairwiseConnectivity,pairwiseConnectivity[1,])
        pairwiseConnectivity[nrow(pairwiseConnectivity),"hexagonsTo"] <- m
        pairwiseConnectivity[nrow(pairwiseConnectivity),"value"] <- 0
      }
    }
    
    ## ---------------------------------------------------
    
    # Produce results at the hexagon level
    
    if( length(hexagonIDFrom) == length(unlist(hexagonIDFrom)) ) {
      
      pairwiseConnectivitySquare <- as.matrix(dcast(pairwiseConnectivity, hexagonsFrom ~ hexagonsTo, mean, value.var = "value"))
      connectivityEventsAggSquareFromNames <- as.character(pairwiseConnectivitySquare[,1])
      connectivityEventsAggSquareToNames <- colnames(pairwiseConnectivitySquare)[-1]
      pairwiseConnectivitySquare <- pairwiseConnectivitySquare[,-1]
      rownames(pairwiseConnectivitySquare) <- connectivityEventsAggSquareFromNames
      colnames(pairwiseConnectivitySquare) <- connectivityEventsAggSquareToNames
      
      pairwiseConnectivitySquareSteps <- as.matrix(dcast(pairwiseConnectivity, hexagonsFrom ~ hexagonsTo, mean, value.var = "steps"))
      pairwiseConnectivitySquareSteps <- pairwiseConnectivitySquareSteps[,-1]
      rownames(pairwiseConnectivitySquareSteps) <- connectivityEventsAggSquareFromNames
      colnames(pairwiseConnectivitySquareSteps) <- connectivityEventsAggSquareToNames
      
    }
    
    # Produce results at the feature level. Aggregate polygon at feature
    
    if( length(hexagonIDFrom) != length(unlist(hexagonIDFrom)) ) {
      
      pairwiseConnectivitySquare <- matrix(NA,ncol=length(hexagonIDTo),nrow=length(hexagonIDFrom))
      pairwiseConnectivitySquareSteps <- matrix(NA,ncol=length(hexagonIDTo),nrow=length(hexagonIDFrom))
      
      for( i in 1:length(hexagonIDFrom) ) {
        for( j in 1:length(hexagonIDTo) ) {
          
          val <- unlist(pairwiseConnectivity[ hexagonsFrom %in% hexagonIDFrom[[i]] & hexagonsTo %in% hexagonIDTo[[j]] , "value"])
          pairwiseConnectivitySquare[i,j] <- ifelse(length(val) == 0,0, mean(val))
          
          val <- unlist(pairwiseConnectivity[ hexagonsFrom %in% hexagonIDFrom[[i]] & hexagonsTo %in% hexagonIDTo[[j]] , "steps"])
          pairwiseConnectivitySquareSteps[i,j] <- ifelse(length(val) == 0,0, mean(val))
          
        }
      }
      
      rownames(pairwiseConnectivitySquare) <- as.character(sapply(1:length(hexagonIDFrom) , function(x) ifelse(!is.null(names(hexagonIDFrom)[x]), names(hexagonIDFrom)[x] , x) ))
      colnames(pairwiseConnectivitySquare) <- as.character(sapply(1:length(hexagonIDTo) , function(x) ifelse(!is.null(names(hexagonIDTo)[x]), names(hexagonIDTo)[x] , x) ))
      
      rownames(pairwiseConnectivitySquareSteps) <- as.character(sapply(1:length(hexagonIDFrom) , function(x) ifelse(!is.null(names(hexagonIDFrom)[x]), names(hexagonIDFrom)[x] , x) ))
      colnames(pairwiseConnectivitySquareSteps) <- as.character(sapply(1:length(hexagonIDTo) , function(x) ifelse(!is.null(names(hexagonIDTo)[x]), names(hexagonIDTo)[x] , x) ))
      
    }
    
    # -----------
    
    pairwiseConnectivitySquare <- pairwiseConnectivitySquare[match(as.character(unlist(hexagonIDFrom)),rownames(pairwiseConnectivitySquare)),match(as.character(unlist(hexagonIDTo)),colnames(pairwiseConnectivitySquare))]
    pairwiseConnectivitySquareSteps <- pairwiseConnectivitySquareSteps[match(as.character(unlist(hexagonIDFrom)),rownames(pairwiseConnectivitySquareSteps)),match(as.character(unlist(hexagonIDTo)),colnames(pairwiseConnectivitySquareSteps))]
    
    pairwiseConnectivitySquare[is.na(pairwiseConnectivitySquare)] <- 0
    pairwiseConnectivitySquareSteps[is.na(pairwiseConnectivitySquareSteps)] <- 0
    
    # -----------
    
    names(pairwiseConnectivity) <- c("From","To","Value","Steps")
    pairwiseConnectivity <- pairwiseConnectivity[which(pairwiseConnectivity$Value != 0), ]
    pairwiseConnectivity$From <- as.character(pairwiseConnectivity$From)
    pairwiseConnectivity$To <- as.character(pairwiseConnectivity$To)
    
    # ------
    
    comb <- as.data.frame(pairwiseConnectivity)
    comb <- comb[ comb[,1] != comb[,2] ,]
    comb <- as.data.frame( comb[ sort(comb[,"Value"] , decreasing = TRUE, index.return =TRUE)$ix , c("From","To","Value")] )
    graph.obj <- graph.edgelist( cbind( as.character( comb[,1]) , as.character(comb[,2]) ) , directed = TRUE )
    igraph::E(graph.obj)$weight = comb[,3] # Hock, Karlo Mumby, Peter J 2015
    graph.obj <- delete_edges(graph.obj, which( E(graph.obj)$weight ==0) )
    graph.obj <- delete_edges(graph.obj, which( is.na(E(graph.obj)$weight) ))
    graph.obj <- simplify(graph.obj)
    
    ## --------------
    
    options(warn=0)
    
    featuresConnected <- unique(pairwiseConnectivity$To)
    
    if( length(hexagonIDFrom) == length(unlist(hexagonIDFrom)) & sum( names(hexagonIDTo) %in% featuresConnected ) == 0 ) {
      
      names(hexagonIDTo) <- as.character(unlist(hexagonIDTo))
      
    } 
    
    featuresConnected <- names(hexagonIDTo)[ which(names(hexagonIDTo) %in% featuresConnected) ]
    featuresNotConnected <- names(hexagonIDTo)[ which(! names(hexagonIDTo) %in% featuresConnected) ]
    
    if( is.null(featuresNotConnected)) { featuresNotConnected <- NA }
    if( length(featuresNotConnected) == 0 ) { featuresNotConnected <- NA }
    if( is.null(hexagonsNotConnected)) { hexagonsNotConnected <- NA }
    if( length(hexagonsNotConnected) == 0 ) { hexagonsNotConnected <- NA }
    
    ## --------------
    
    pairwiseConnectivity <- as.data.frame(pairwiseConnectivity)
    pairwiseConnectivitySquare <- as.matrix(pairwiseConnectivitySquare)
    ## --------------
    
    return( list(connectivityPairs=pairwiseConnectivity,
                 connectivityMatrix=pairwiseConnectivitySquare,
                 connectivityGraph=graph.obj,
                 featuresConnected=featuresConnected,
                 featuresNotConnected=featuresNotConnected,
                 hexagonsConnected=hexagonsConnected,
                 hexagonsNotConnected=hexagonsNotConnected) )
    
}
#' Calculate pairwise connectivity
#'
#' This function calculates pairwise connectivity between hexagon sites based on connectivity events.
#'
#' @param connectivityEvents A data frame containing all connectivity events.
#' @param hexagonIDFrom A numeric vector specifying the hexagon IDs from which connectivity is calculated.
#' @param hexagonIDTo A numeric vector specifying the hexagon IDs to which connectivity is calculated. If NULL it will be as hexagonIDFrom.
#' @param connType A character string specifying the type of connectivity ("Forward" or "Backward"). Default is "Forward".
#' @param value A character string specifying the value to be calculated ("Probability" or "Time"). Default is "Probability".
#' @param steppingStone A logical value indicating whether to calculate stepping stone connectivity. Default is FALSE and will calculate direct connectivity.
#' @param nStepStones An integer specifying the maximum number of stepping stones to consider (if steppingStone is TRUE).
#' @param parallelCores An integer specifying the number of parallel cores to use in the calculation of stepping stone connectivity (if steppingStone is TRUE). Default is NULL and will use all available cores - 1.
#'
#' @return A list containing the pairwise connectivity information, including connectivity pairs, connectivity matrix, connectivity graph, the ID of the hexagons connected and unconnected and also the number of stepping-stone steps per connection.
#'
#' @import igraph data.table parallel doParallel
#' @export

getPairwiseConnectivity <- function(connectivityEvents, hexagonIDFrom, hexagonIDTo=NULL, connType="Forward", value="Probability", steppingStone=FALSE, nStepStones=NULL, parallelCores=NULL){

    cat("\n")
    cat("# ---------------------------------------------","\n")
    cat("Get pairwise connectivity estimates","\n")
    cat("Connectivity type:",connType,"\n")
    cat("Connectivity estimate:",value,"\n")
    cat("Stepping stone connectivity:",steppingStone,"\n")
    cat("Number of maximum stepping stone events:",ifelse(!is.null(nStepStones),nStepStones,"Unlimited"),"\n")
    cat("Number of parallel cores:",ifelse(!is.null(parallelCores),parallelCores,detectCores() - 1),"\n")

    if( missing(connectivityEvents)) { stop("The connectivityEvents parameter is required.") }
    if( missing(hexagonIDFrom)) { stop("Missing hexagonIDFrom parameter.") }

    if( class(hexagonIDFrom) == "data.frame" | class(hexagonIDFrom) == "matrix" ) { stop("hexagonIDFrom needs to be a numeric vector\n")  }
    if( class(hexagonIDTo) == "data.frame" | class(hexagonIDTo) == "matrix" ) { stop("hexagonIDTo needs to be a numeric vector\n")  }
    
    data("referenceTable")

    ## -------------

    options(warn=-1)
    
    if( is.null(hexagonIDFrom)) { hexagonIDFrom <- unique(connectivityEvents$connectivityEventStartCell ) }
    if( is.null(hexagonIDTo)) { hexagonIDTo <- hexagonIDFrom }
    
    hexagonIDFrom <- unique(hexagonIDFrom)
    hexagonIDTo <- unique(hexagonIDTo)
  
    cat("Hexagons from:",length(hexagonIDFrom),"\n")
    cat("Hexagons to:",length(hexagonIDTo),"\n")
    cat("# ---------------------------------------------","\n")

  ## ---------------------------------------------------
  
  if( value == "Probability" ) {
    
    connectivityEvents$value <- 1
    
    pairwiseConnectivity <- aggregate(connectivityEvents, value ~ connectivityEventStartHexagon + connectivityEventEndHexagon, FUN=sum)

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
    
    pairwiseConnectivity$value <- pairwiseConnectivity$value / eventsPerCell
    names(pairwiseConnectivity) <- c("hexagonsFrom","hexagonsTo","value")
    
  }
  
  if( value == "Time" ) {
    
    if( connType == "Forward" ) { 
      pairwiseConnectivity <- aggregate(connectivityEvents, connectivityEventTravelTime ~ connectivityEventStartCell + connectivityEventEndCell, FUN=mean)
    }
    if( connType == "Backward" ) { 
      pairwiseConnectivity <- aggregate(connectivityEvents, connectivityEventTravelTime ~ connectivityEventEndCell + connectivityEventStartCell, FUN=mean) 
      pairwiseConnectivity[,1:2] <- pairwiseConnectivity[,c(2,1)]
    }

    names(pairwiseConnectivity) <- c("hexagonsFrom","hexagonsTo","value")

  }
  
  ## ---------------------------------------------------
  
  missingFrom <- hexagonIDFrom[which( ! hexagonIDFrom %in% pairwiseConnectivity$hexagonsFrom )]
  if( length(missingFrom) > 0) {
    for( m in missingFrom) {
      pairwiseConnectivity <- rbind(pairwiseConnectivity,pairwiseConnectivity[1,])
      pairwiseConnectivity[nrow(pairwiseConnectivity),"hexagonsFrom"] <- m
      pairwiseConnectivity[nrow(pairwiseConnectivity),"value"] <- 0
    }
  }
  
  missingTo <- hexagonIDTo[which( ! hexagonIDTo %in% pairwiseConnectivity$hexagonsTo )]
  if( length(missingTo) > 0) {
    for( m in missingTo) {
      pairwiseConnectivity <- rbind(pairwiseConnectivity,pairwiseConnectivity[1,])
      pairwiseConnectivity[nrow(pairwiseConnectivity),"hexagonsTo"] <- m
      pairwiseConnectivity[nrow(pairwiseConnectivity),"value"] <- 0
    }
  }

  ## ---------------------------------------------------

  connectivityEventsAggSquare <- dcast(pairwiseConnectivity, hexagonsFrom ~ hexagonsTo, mean, value.var = "value") 
  connectivityEventsAggSquareFromNames <- as.character(connectivityEventsAggSquare[,1])
  connectivityEventsAggSquareToNames <- colnames(connectivityEventsAggSquare)[-1]
  connectivityEventsAggSquare <- connectivityEventsAggSquare[,-1]
  rownames(connectivityEventsAggSquare) <- connectivityEventsAggSquareFromNames
  colnames(connectivityEventsAggSquare) <- connectivityEventsAggSquareToNames
  connectivityEventsAggSquare[is.na(connectivityEventsAggSquare)] <- 0
  
  if( ! steppingStone ) {
    
    pairwiseConnectivitySquare <- connectivityEventsAggSquare[ match(hexagonIDFrom,rownames(connectivityEventsAggSquare)) , match(hexagonIDTo,colnames(connectivityEventsAggSquare)) ]
    
    pairwiseConnectivity <- as.data.frame(as.table(as.matrix(pairwiseConnectivitySquare)))
    names(pairwiseConnectivity) <- c("hexagonsFrom","hexagonsTo","value")
    pairwiseConnectivity <- pairwiseConnectivity[which(pairwiseConnectivity$value != 0), ]
    pairwiseConnectivity$hexagonsFrom <- as.numeric(as.character(pairwiseConnectivity$hexagonsFrom))
    pairwiseConnectivity$hexagonsTo <- as.numeric(as.character(pairwiseConnectivity$hexagonsTo))
    
  }
  
  if( steppingStone ) {

    comb <- pairwiseConnectivity
    comb <- as.data.frame( comb[ sort(comb$value , decreasing = TRUE, index.return =TRUE)$ix , c("hexagonsFrom","hexagonsTo","value")] )
    graph.obj <- graph.edgelist( cbind( as.character( comb[,1]) , as.character(comb[,2]) ) , directed = TRUE )
    
    # Hock, Karlo Mumby, Peter J 2015
    if(value == "Probability") { E(graph.obj)$weight = ifelse(-log(comb[,3]) == Inf,0,-log(comb[,3])) }
    
    # The wheight has a negative impact on finding the closest path
    if(value == "Time") { E(graph.obj)$weight = comb[,3] }

    graph.obj <- delete.edges(graph.obj, which( E(graph.obj)$weight == 0) )
    #graph.obj <- delete.edges(graph.obj, which( is.na(E(graph.obj)$weight) ))
    #graph.obj <- as.undirected(graph.obj, mode = "collapse", edge.attr.comb = "mean")
    graph.obj <- simplify(graph.obj)

    if(is.null(parallelCores)){ numberCores <- detectCores() - 1 } else { numberCores <- parallelCores }
    if(numberCores == 0) { numberCores <- 1 }

    cl <- makeCluster(numberCores)
    registerDoParallel(cl)
    
    pairwiseConnectivity <- foreach(from=hexagonIDFrom, .combine = rbind, .verbose=FALSE, .packages=c("igraph")) %dopar% { 
      
      res.connectivity.to <- numeric(0)
      res.connectivity.to.steps <- numeric(0)
      
      for( to in hexagonIDTo ) {
        
        stones.t <- get.shortest.paths(graph.obj, as.character( from ) , as.character( to ),mode="out")$vpath
        stones.t <- as.numeric(names(stones.t[[1]]))
        stones.t.interm <- cbind(stones.t[-length(stones.t)],stones.t[-1])
        path.values <- apply( stones.t.interm , 1 , function(z) { comb[ comb[,1] == z[1] & comb[,2] == z[2] , 3 ][1] }   )
        
        if( ! is.null(nStepStones)) { if( length(path.values) > nStepStones ) { path.values[] <- 0 }  }
        if( length(path.values) == 0) { path.values <- 0 }
        
        res.connectivity.to.steps <- c(res.connectivity.to.steps, ifelse( sum(path.values) != 0 ,length(path.values) , 0  ) )

        path.values <- apply( t(path.values) , 1 , prod ) 
        
        if( from == to ) { path.values <- 1 }
        
        res.connectivity.to <- c(res.connectivity.to,path.values)
        
      }
      
      ## ---------------------
 
      temp.res <- data.frame( hexagonsFrom = from,
                              hexagonsTo = hexagonIDTo , 
                              value = res.connectivity.to,
                              steps = res.connectivity.to.steps )
      
      return(temp.res)
      
      ## ---------------------
      
    }
    
    stopCluster(cl) ; rm(cl)
    
    pairwiseConnectivity$hexagonsFrom <- as.numeric(as.character(pairwiseConnectivity$hexagonsFrom))
    pairwiseConnectivity$hexagonsTo <- as.numeric(as.character(pairwiseConnectivity$hexagonsTo))
    
    connectivityEventsAggSquare <- dcast(pairwiseConnectivity, hexagonsFrom ~ hexagonsTo, mean, value.var = "value")
    connectivityEventsAggSquareFromNames <- as.character(connectivityEventsAggSquare[,1])
    connectivityEventsAggSquareToNames <- colnames(connectivityEventsAggSquare)[-1]
    connectivityEventsAggSquare <- connectivityEventsAggSquare[,-1]
    rownames(connectivityEventsAggSquare) <- connectivityEventsAggSquareFromNames
    colnames(connectivityEventsAggSquare) <- connectivityEventsAggSquareToNames
    connectivityEventsAggSquare[is.na(connectivityEventsAggSquare)] <- 0
    connectivityEventsAggSquare <- connectivityEventsAggSquare[match(hexagonIDFrom,rownames(connectivityEventsAggSquare)) , match(hexagonIDTo,colnames(connectivityEventsAggSquare)) ]
    pairwiseConnectivitySquare <- connectivityEventsAggSquare
    
  }

  comb <- pairwiseConnectivity
  comb <- comb[ comb[,1] != comb[,2] ,]
  comb <- as.data.frame( comb[ sort(comb[,"value"] , decreasing = TRUE, index.return =TRUE)$ix , c("hexagonsFrom","hexagonsTo","value")] )
  graph.obj <- graph.edgelist( cbind( as.character( comb[,1]) , as.character(comb[,2]) ) , directed = TRUE )
  E(graph.obj)$weight = comb[,3] # Hock, Karlo Mumby, Peter J 2015
  graph.obj <- delete.edges(graph.obj, which( E(graph.obj)$weight ==0) )
  graph.obj <- delete.edges(graph.obj, which( is.na(E(graph.obj)$weight) ))
  graph.obj <- simplify(graph.obj)
  
  ## --------------
  
  options(warn=0)
  
  sitesConnected <- unique(pairwiseConnectivity$hexagonsTo)
  sitesConnected <- hexagonIDTo[ which(hexagonIDTo %in% sitesConnected) ]
  sitesNotConnected <- hexagonIDTo[ which(! hexagonIDTo %in% sitesConnected) ]
  
  if( ! "steps" %in% colnames(pairwiseConnectivity) ) { pairwiseConnectivity$steps <- 1 }

  return( list(connectivityPairs=pairwiseConnectivity,
               connectivityMatrix=pairwiseConnectivitySquare,
               connectivityGraph=graph.obj,
               sitesConnected=sitesConnected,
               sitesNotConnected=sitesNotConnected))
  
}
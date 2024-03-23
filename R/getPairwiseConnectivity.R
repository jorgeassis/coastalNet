#' Calculate Pairwise Connectivity Estimates
#'
#' \code{getPairwiseConnectivity} This function calculates pairwise measures of oceanographic connectivity between sets of hexagons, considering various estimation metrics (probability, events, time),  forward or backward connectivity, and optional stepping stone calculations.

#' @param connectivityEvents A data frame containing connectivity events.
#' @param hexagonIDFrom A list where each element is a vector of hexagon IDs representing 'from' regions. 
#' @param hexagonIDTo (Optional) A list mirroring the structure of 'hexagonalIDFrom', specifying 'to' regions. If not provided, pairwise connectivity will be calculated for all pairs within 'hexagonIDFrom'.   
#' @param connType  A string specifying the type of connectivity direction. Options are "Forward" or "Backward". Defaults to "Forward".
#' @param value A string specifying the connectivity metric. Options are "Probability", "Events", or "Time". Defaults to "Probability".
#' @param steppingStone (Optional) A logical flag indicating whether stepping-stone connectivity should be considered. Defaults to FALSE. 
#' @param nStepStones (Optional) If 'steppingStone' is TRUE, an integer specifying the maximum number of steps in a stepping-stone path. Defaults to unlimited.
#' @param parallelCores (Optional) The number of parallel cores to use for stepping-stone computations. If not provided, it defaults to available cores - 1. 

#' @return A list containing:
#'  * **connectivityPairs:** A data frame of pairwise connectivity results (from, to, value, steps)
#'  * **connectivityMatrix:** A matrix where rows are 'from' regions and columns are 'to' regions, containing connectivity values.
#'  * **connectivityGraph:** An 'igraph' object representing the connectivity network
#'  * **sitesConnected:** A vector of 'to' hexagon names that have connections 
#'  * **sitesNotConnected:** A vector of  'to' hexagon names with no connections, or NA if all are connected. 

#' @examples
#' \dontrun{
#' # Example: Basic connectivity (from hexagon 1 to 2)
#' pw_results <- getPairwiseConnectivity(connectivityEvents = my_events,
#'                                       hexagonIDFrom = list(c("1"), c("2")))
#'
#' # Example: Stepping-stone connectivity, up to 3 steps, with parallel processing
#' pw_results <- getPairwiseConnectivity(connectivityEvents = my_events,
#'                                      hexagonIDFrom = list(c("1")), 
#'                                      hexagonIDTo =  list(c("2")), 
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
    
    data("referenceTable")

    ## -------------

    options(warn=-1)
    
    if( is.null(hexagonIDFrom)) { hexagonIDFrom <- list( unique(connectivityEvents$connectivityEventStartHexagon )) }
    if( is.null(hexagonIDTo)) { hexagonIDTo <- hexagonIDFrom }
    
    cat("From:",length(hexagonIDFrom), "element(s) /",length(unlist(hexagonIDFrom)),"hexagons","\n")
    cat("To:",length(hexagonIDTo), "element(s) /",length(unlist(hexagonIDTo)),"hexagons","\n")
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
  
    connectivityEventsAggSquare <- as.matrix(dcast(pairwiseConnectivity, hexagonsFrom ~ hexagonsTo, mean, value.var = "value"))
    connectivityEventsAggSquareFromNames <- as.character(connectivityEventsAggSquare[,1])
    connectivityEventsAggSquareToNames <- colnames(connectivityEventsAggSquare)[-1]
    connectivityEventsAggSquare <- connectivityEventsAggSquare[,-1]
    rownames(connectivityEventsAggSquare) <- connectivityEventsAggSquareFromNames
    colnames(connectivityEventsAggSquare) <- connectivityEventsAggSquareToNames
    connectivityEventsAggSquare[is.na(connectivityEventsAggSquare)] <- 0
    
    if( length(hexagonIDFrom) == length(unlist(hexagonIDFrom)) ) {
      
      pairwiseConnectivitySquare <- matrix(0,ncol=length(hexagonIDTo),nrow=length(hexagonIDFrom))
      hexagonIDFromUl <- unlist(hexagonIDFrom)
      hexagonIDToUl <- unlist(hexagonIDTo)
      
      row.i <- match(hexagonIDFromUl,rownames(connectivityEventsAggSquare))
      for( i in 1:length(row.i) ) {
        pairwiseConnectivitySquare[i,] <- connectivityEventsAggSquare[row.i[i],match(hexagonIDToUl,colnames(connectivityEventsAggSquare))]
      }
    }
    
    if( length(hexagonIDFrom) != length(unlist(hexagonIDFrom)) ) {
      
      pairwiseConnectivitySquare <- matrix(NA,ncol=length(hexagonIDTo),nrow=length(hexagonIDFrom))
      
      t1 <- Sys.time()
      for( i in 1:length(hexagonIDFrom) ) {
        for( j in 1:length(hexagonIDTo) ) {
          val <- unlist(pairwiseConnectivity[ hexagonsFrom %in% hexagonIDFrom[[i]] & hexagonsTo %in% hexagonIDTo[[j]] , "value"])
          pairwiseConnectivitySquare[i,j] <- ifelse(length(val) == 0,0, mean(val))
        }
      }
      Sys.time() - t1
      
      
    }

    
    pairwiseConnectivitySquareSteps <- pairwiseConnectivitySquare

    if( steppingStone ) {
  
      comb <- pairwiseConnectivity
      comb <- as.data.frame( comb[ sort(comb$value , decreasing = TRUE, index.return =TRUE)$ix , c("hexagonsFrom","hexagonsTo","value")] )
      graph.obj <- graph.edgelist( cbind( as.character( comb[,1]) , as.character(comb[,2]) ) , directed = TRUE )
      
      # Hock, Karlo Mumby, Peter J 2015
      if(value == "Probability") { igraph::E(graph.obj)$weight = ifelse(-log(comb[,3]) == Inf,0,-log(comb[,3])) }
      
      # The wheight has a negative impact on finding the closest path
      if(value == "Time" | value == "Events" ) { igraph::E(graph.obj)$weight = comb[,3] }
  
      graph.obj <- delete.edges(graph.obj, which( igraph::E(graph.obj)$weight == 0) )
      #graph.obj <- delete.edges(graph.obj, which( is.na(E(graph.obj)$weight) ))
      #graph.obj <- as.undirected(graph.obj, mode = "collapse", edge.attr.comb = "mean")
      graph.obj <- simplify(graph.obj)
  
      if(is.null(parallelCores)){ numberCores <- detectCores() - 1 } else { numberCores <- parallelCores }
      if(numberCores == 0) { numberCores <- 1 }
  
      cl <- makeCluster(numberCores)
      registerDoParallel(cl)
      
      pairwiseConnectivity <- foreach(i=1:length(hexagonIDFrom), .combine = rbind, .verbose=FALSE, .packages=c("igraph","data.table")) %dopar% { 
        
          res <- data.frame()
  
          for( j in 1:length(hexagonIDTo) ) {
  
              temp.res <- data.frame()
  
              for( from in hexagonIDFrom[[i]] ) {
  
                  res.connectivity.to <- numeric(0)
                  res.connectivity.to.steps <- numeric(0)
                  
                  for( to in hexagonIDTo[[j]] ) {
                    
                    stones.t <- get.shortest.paths(graph.obj, as.character( from ) , as.character( to ),mode="out")$vpath
                    stones.t <- as.numeric(names(stones.t[[1]]))
                    stones.t.interm <- cbind(stones.t[-length(stones.t)],stones.t[-1])
                    path.values <- apply( stones.t.interm , 1 , function(z) { comb[ comb[,1] == z[1] & comb[,2] == z[2] , 3 ][1] }   )
                    
                    if( ! is.null(nStepStones)) { if( length(path.values) > nStepStones ) { path.values[] <- 0 }  }
                    if( length(path.values) == 0) { path.values <- 0 }
                    
                    res.connectivity.to.steps <- c(res.connectivity.to.steps, ifelse( sum(path.values) != 0 ,length(path.values) , 0  ) )
  
                    if( value == "Probability") {  path.values <- apply( t(path.values) , 1 , prod )  }
                    if( value == "Time" | value == "Events" ) {  path.values <- apply( t(path.values) , 1 , sum )  }

                    if( from == to ) { path.values <- pairwiseConnectivitySquare[i,j] }
                    
                    res.connectivity.to <- c(res.connectivity.to,path.values)
                    
                  }
  
                  temp.res <- rbind(temp.res ,
                                    data.frame( hexagonsFrom = i,
                                                hexagonsTo = j , 
                                                value = mean(res.connectivity.to, na.rm=T),
                                                steps = mean(res.connectivity.to.steps, na.rm=T)))
  
  
              }
  
              temp.res <- data.table(temp.res)
              temp.res <- as.data.frame(temp.res[, .(value  = mean(value), steps  = mean(steps) ), by=.(hexagonsFrom, hexagonsTo)] )
              res <- rbind(res,temp.res)
  
          }
  
        ## ---------------------
  
        return(res)
        
        ## ---------------------
        
      }
      
      stopCluster(cl) ; rm(cl)
      
      ## --------------
  
      connectivityEventsAggSquare <- dcast(pairwiseConnectivity, hexagonsFrom ~ hexagonsTo, mean, value.var = "value")
      connectivityEventsAggSquareFromNames <- as.character(connectivityEventsAggSquare[,1])
      connectivityEventsAggSquareToNames <- colnames(connectivityEventsAggSquare)[-1]
      connectivityEventsAggSquare <- connectivityEventsAggSquare[,-1]
      rownames(connectivityEventsAggSquare) <- connectivityEventsAggSquareFromNames
      colnames(connectivityEventsAggSquare) <- connectivityEventsAggSquareToNames
      connectivityEventsAggSquare[is.na(connectivityEventsAggSquare)] <- 0
      pairwiseConnectivitySquare <- connectivityEventsAggSquare
      
      connectivityEventsAggSquareSteps <- dcast(pairwiseConnectivity, hexagonsFrom ~ hexagonsTo, mean, value.var = "steps")
      connectivityEventsAggSquareFromNames <- as.character(connectivityEventsAggSquareSteps[,1])
      connectivityEventsAggSquareToNames <- colnames(connectivityEventsAggSquareSteps)[-1]
      connectivityEventsAggSquareSteps <- connectivityEventsAggSquareSteps[,-1]
      rownames(connectivityEventsAggSquareSteps) <- connectivityEventsAggSquareFromNames
      colnames(connectivityEventsAggSquareSteps) <- connectivityEventsAggSquareToNames
      connectivityEventsAggSquareSteps[is.na(connectivityEventsAggSquareSteps)] <- 0
      pairwiseConnectivitySquareSteps <- connectivityEventsAggSquareSteps
      
    }
  
    rownames(pairwiseConnectivitySquare) <- sapply(1:length(hexagonIDFrom) , function(x) ifelse(!is.null(names(hexagonIDFrom)[x]), names(hexagonIDFrom)[x] , x) )
    colnames(pairwiseConnectivitySquare) <- sapply(1:length(hexagonIDTo) , function(x) ifelse(!is.null(names(hexagonIDTo)[x]), names(hexagonIDTo)[x] , x) )
  
    rownames(pairwiseConnectivitySquareSteps) <- sapply(1:length(hexagonIDFrom) , function(x) ifelse(!is.null(names(hexagonIDFrom)[x]), names(hexagonIDFrom)[x] , x) )
    colnames(pairwiseConnectivitySquareSteps) <- sapply(1:length(hexagonIDTo) , function(x) ifelse(!is.null(names(hexagonIDTo)[x]), names(hexagonIDTo)[x] , x) )
    
    pairwiseConnectivity <- as.data.frame(as.table(as.matrix(pairwiseConnectivitySquare)))
    pairwiseConnectivity$Steps <- as.data.frame(as.table(as.matrix(pairwiseConnectivitySquareSteps)))[,3]
    
    names(pairwiseConnectivity) <- c("From","To","Value","Steps")
    pairwiseConnectivity <- pairwiseConnectivity[which(pairwiseConnectivity$Value != 0), ]
    pairwiseConnectivity$From <- as.character(pairwiseConnectivity$From)
    pairwiseConnectivity$To <- as.character(pairwiseConnectivity$To)
    
    comb <- pairwiseConnectivity
    comb <- comb[ comb[,1] != comb[,2] ,]
    comb <- as.data.frame( comb[ sort(comb[,"Value"] , decreasing = TRUE, index.return =TRUE)$ix , c("From","To","Value")] )
    graph.obj <- graph.edgelist( cbind( as.character( comb[,1]) , as.character(comb[,2]) ) , directed = TRUE )
    igraph::E(graph.obj)$weight = comb[,3] # Hock, Karlo Mumby, Peter J 2015
    graph.obj <- delete.edges(graph.obj, which( E(graph.obj)$weight ==0) )
    graph.obj <- delete.edges(graph.obj, which( is.na(E(graph.obj)$weight) ))
    graph.obj <- simplify(graph.obj)
    
    ## --------------
    
    options(warn=0)
    
    if(is.null(names(hexagonIDTo))) { names(hexagonIDTo) <- as.character(1:length(hexagonIDTo)) }
    if(is.null(names(hexagonIDFrom))) { names(hexagonIDFrom) <- as.character(1:length(hexagonIDFrom)) }
    
    sitesConnected <- unique(pairwiseConnectivity$To)
    sitesConnected <- names(hexagonIDTo)[ which(names(hexagonIDTo) %in% sitesConnected) ]
    sitesNotConnected <- names(hexagonIDTo)[ which(! names(hexagonIDTo) %in% sitesConnected) ]
    
    if( length(sitesNotConnected) == 0) { sitesNotConnected <- NA }
    if( ! "Steps" %in% colnames(pairwiseConnectivity) ) { pairwiseConnectivity$Steps <- 1 }
  
    ## --------------
    
    pairwiseConnectivity$FromHexagon <- sapply(match(pairwiseConnectivity$From , names(hexagonIDFrom)), function(x) { hexagonIDFrom[[x]]} )
    pairwiseConnectivity$ToHexagon <- sapply(match(pairwiseConnectivity$To , names(hexagonIDTo)), function(x) { hexagonIDTo[[x]]} )
    pairwiseConnectivity <- pairwiseConnectivity[,c(1,2,5,6,3,4)]
    
    ## --------------
    
    return( list(connectivityPairs=pairwiseConnectivity,
                 connectivityMatrix=pairwiseConnectivitySquare,
                 connectivityGraph=graph.obj,
                 sitesConnected=sitesConnected,
                 sitesNotConnected=sitesNotConnected) )
  
}

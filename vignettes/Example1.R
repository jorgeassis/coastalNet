# ---------------------------------------------
# Clean environment and load packages

rm(list = ls())
gc(reset=TRUE)
library(coastalNet)
library(lme4)
library(rnaturalearth)
library(viridis)

# ---------------------

# Load data.frame containing coordinates (as longitude and longitude, WGS84) of sites sampled for the marine species Laminaria ochroleuca.
laminariaRecords <- read.csv("https://raw.githubusercontent.com/jorgeassis/coastalNet/main/vignettes/data/Laminaria%20ochroleuca%20Coords.csv", sep=";", header = TRUE)

# Load data.frame containing pairwise genetic differentiation estimates between coordinate sites
laminariaPopDifferentiation <- read.csv("https://raw.githubusercontent.com/jorgeassis/coastalNet/main/vignettes/data/Laminaria%20ochroleuca%20JostD.csv", sep=";", header = FALSE)

# ---------------------

# Load database
getDataBase(myFolder="Database", overwrite=FALSE)

# Get hexagon IDs that define the study region
hexagonIDRegion <- getHexagonID(obj=laminariaRecords, level="extent", buffer=5, print=TRUE)

# Get connectivity events for the study region (all years, all months, all days, 120 days period)
connectivityEvents <- getConnectivityEvents(hexagonID=hexagonIDRegion, period=120 )

# Get hexagon IDs of the sampling sites
hexagonIDSites <- getHexagonID(obj=laminariaRecords, level="site", buffer=0, print=FALSE)

# Get pairwise connectivity estimates between coordinate sites
pairwiseConnectivity <- getPairwiseConnectivity(connectivityEvents, hexagonIDFrom=hexagonIDSites, connType="Forward", value="Probability", steppingStone=TRUE)

# ---------------------

# Produce and data.frame matching pairs of oceanographic connectivity and pairs of population differentiation 
modelDataFrame <- data.frame()
for( from in 1:nrow(laminariaRecords)) {
  for( to in 1:nrow(laminariaRecords)) {
    if( from == to ) { next }
    modelDataFrame <- rbind(modelDataFrame,data.frame(from = from, to = to, connectivity = mean(pairwiseConnectivity$connectivityMatrix[from,to], pairwiseConnectivity$connectivityMatrix[to,from], na.rm=T), differentiation = laminariaPopDifferentiation[from,to]))
  }
}

# Remove zero connectivity values, missing values and Log-transform connectivity
modelDataFrame <- modelDataFrame[modelDataFrame$connectivity != 0 ,]
modelDataFrame <- modelDataFrame[complete.cases(modelDataFrame),]
modelDataFrame$connectivity <- -log(modelDataFrame$connectivity)

# Model oceanographic connectivity to population differentiation
model <- lmer(differentiation ~ connectivity + (1|from), modelDataFrame, REML=F)
observedPredictedDF <- data.frame(Observed=(modelDataFrame$differentiation), Predicted=predict(model))
model <- lm(Observed ~ Predicted, data = observedPredictedDF)
r2 <- summary(model)$adj.r.squared
Pearson <- cor(observedPredictedDF)[1,2]

plot1 <- ggplot() + geom_point(data = observedPredictedDF, aes(x=Observed, y=Predicted), color="#848484", fill="gray", size=1.25 ) + 
  geom_smooth(data = observedPredictedDF, method=lm, aes(x=Observed, y=Predicted), linetype = "dashed", fill="#848484", col='black', size=0.5) + 
  theme(axis.text.x = element_text(angle = 247, hjust = 1, vjust = 0.5)) + 
  xlab(paste0("Observed population differentiation")) + ylab("Predicted population differentiation") +
  theme_minimal() + 
  theme( panel.grid.major = element_blank() ,
         text = element_text(size=12) ,
         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)) ,
         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)) ,
         legend.title = element_blank()) +
         annotate("label", alpha = 0.5, label.padding=unit(0.5, "lines"), x = 0, y = 0.735, hjust=0,vjust=1 , label = paste0("Adjusted R2: ", format(round(r2, 3), nsmall = 3),"\nPearson's Corr.: ",format(round(Pearson, 3), nsmall = 3))) +
         ggtitle("Role of oceanographic connectivity to population differentiation")

plot1

# ---------------------

# Map oceanographic connectivity between populations
mappedConnectivity <- mapConnectivity(connectivityPairs=pairwiseConnectivity$connectivityPairs,obj=laminariaRecords)

# Load the worldmap and crop to the atudy region
worldMap <- ne_countries(scale = "medium", returnclass = "sf")[,1]
worldMap <- st_crop(worldMap,c(xmin=min(laminariaRecords[,1])-5,xmax=max(laminariaRecords[,1])+7.5,ymin=min(laminariaRecords[,2])-5,ymax=max(laminariaRecords[,2])+5))

# Get hexagon IDs that retrieved oceanographic connectivity estimates
hexagonIDConnected <- pairwiseConnectivity$sitesConnected

# Get a data.frame of the location of hexagons that retrieved oceanographic connectivity estimates
data("hexagonCells")
hexagonCellsConnected <- hexagonCells[hexagonCells$ID %in% hexagonIDConnected,1]
hexagonCellsConnected <- st_coordinates(st_centroid(hexagonCellsConnected))

# Make a plot of the oceanographic connectivity between populations
plot2 <- ggplot() + 
  geom_sf(data = worldMap , fill="#CDCDCD", colour = "#9E9E9E" , size=0.25) +
  geom_point(data = hexagonCellsConnected, aes(x = X, y = Y), colour = "#000000",size=2.5) +
  geom_point(data = hexagonCellsConnected, aes(x = X, y = Y), colour = "#FFFFFF",size=1) +
  geom_sf(data = mappedConnectivity$lineConnections , linewidth = 0.25 , aes(colour = value)) +
  scale_color_gradientn(colours=rev(magma(6)),na.value = "#FDD76C", trans = "log") +
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.ticks.y=element_blank(), legend.position = "none") +
  ggtitle("Stepping-stone oceanographic connectivity between populations")

plot2



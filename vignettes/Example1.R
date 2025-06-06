# ---------------------------------------------

# Clean R environment:
closeAllConnections()
rm(list = ls())
gc(reset = TRUE)

# Load necessary libraries:
library(sf)
library(terra)
library(ggplot2)
library(coastalNet)
library(lme4)
library(rnaturalearth)
library(viridis)

# ---------------------

# Download files from repository
download.file("https://figshare.com/ndownloader/files/47592263", "Laminaria-ochroleuca-Coords.csv", quiet = TRUE, mode = "wb")
download.file("https://figshare.com/ndownloader/files/47592254", "Laminaria-ochroleuca-JostD.csv", quiet = TRUE, mode = "wb")

# Load data.frame containing coordinates (as longitude and longitude, WGS84) of sites sampled for the marine species Laminaria ochroleuca.
laminariaRecords <- read.csv("Laminaria-ochroleuca-Coords.csv", sep=";", header = TRUE)

# Load data.frame containing pairwise genetic differentiation estimates between coordinate sites
laminariaPopDifferentiation <- read.csv("Laminaria-ochroleuca-JostD.csv", sep=";", header = FALSE)

# ---------------------

# Load database of connectivity
oceanographicConnectivity <- getDataBase()

# Load hexagons (i.e., source and sink locations)
hexagonCells <- loadHexagons()

# Inspect the object hexagonCells
ggplot() + 
  geom_sf(data = hexagonCells, color="black") +
  coord_sf(crs= "+proj=robin") +
  theme_minimal()

# Get hexagon IDs that define the study region
hexagonIDRegion <- getHexagonID(obj=laminariaRecords, hexagonCells=hexagonCells, level="extent", buffer=5, print=TRUE)

# Get all connectivity events within the study region (all years, all months, all days, 120 days period)
connectivityEvents <- getConnectivityEvents(connectivity=oceanographicConnectivity,hexagonID=hexagonIDRegion, period=120 )

# Get hexagon IDs of the sampling sites
hexagonIDSites <- getHexagonID(obj=laminariaRecords, hexagonCells=hexagonCells, level="site", buffer=0, print=FALSE)

# Get pairwise connectivity estimates between sampling sites, with stepping-stone events across the strudy site
pairwiseConnectivity <- getPairwiseConnectivity(connectivityEvents, hexagonIDFrom=hexagonIDSites, connType="Forward", value="Probability", steppingStone=TRUE)

# ---------------------

# Produce and data.frame matching pairs of oceanographic connectivity and pairs of population differentiation 
modelDataFrame <- data.frame()
for( from in 1:nrow(laminariaRecords)) {
  for( to in 1:nrow(laminariaRecords)) {
    if( from == to ) { next }
    modelDataFrame <- rbind(modelDataFrame,data.frame(from = from, to = to, connectivity = mean(pairwiseConnectivity$connectivityMatrix[from,to], pairwiseConnectivity$connectivityMatrix[to,from], na.rm=T), differentiation = as.numeric(laminariaPopDifferentiation[from,to])))
  }
}

# Remove zero connectivity values, missing values and Log-transform connectivity
modelDataFrame <- modelDataFrame[modelDataFrame$connectivity != 0 ,]
modelDataFrame <- modelDataFrame[complete.cases(modelDataFrame),]
modelDataFrame$connectivity <- log(modelDataFrame$connectivity)

# Model oceanographic connectivity to population differentiation
model <- lm(connectivity ~ differentiation, data = modelDataFrame)
r2 <- summary(model)$adj.r.squared
Pearson <- cor(modelDataFrame$connectivity, modelDataFrame$differentiation)

ggplot() + 
  geom_point(data = modelDataFrame, aes(x=connectivity, y=differentiation), color="#000000", fill="#000000", size=2 ) + 
  geom_point(data = modelDataFrame, aes(x=connectivity, y=differentiation), color="white", fill="white", size=1 ) + 
  geom_smooth(data = modelDataFrame, method=lm, aes(x=connectivity, y=differentiation), linetype = "dashed", fill="#c5593c", col='black', linewidth=0.5, alpha = 0.5) + 
  xlab(paste0("Oceanographic connectivity [ log(probability) ]")) + ylab("Population genetic differentiation (Jost D)") +
  theme_minimal() + 
  theme( panel.grid.major = element_blank() ,
         text = element_text(size=12) ,
         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)) ,
         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)) ,
         legend.title = element_blank()) +
  annotate("label", alpha = 0.5, label.padding=unit(0.5, "lines"), x = -77, y = 0.2, hjust=0,vjust=1 , label = paste0("Adjusted R2: ", format(round(r2, 3), nsmall = 3),"\nPearson's Corr.: ",format(round(Pearson, 3), nsmall = 3)))

# ---------------------

# Map oceanographic connectivity between populations
mappedConnectivity <- mapConnectivity(obj = laminariaRecords, featureName = as.character(unlist(hexagonIDSites)) , connectivityPairs=pairwiseConnectivity$connectivityPairs)

# Load the worldmap and crop to the atudy region
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
worldMap <- st_crop(worldMap,mappedConnectivity$lineConnections)

# Make a plot of the oceanographic connectivity between populations
ggplot() + 
  geom_sf(data = worldMap , fill="#CDCDCD", colour = "#9E9E9E" , size=0.25) +
  geom_sf(data = worldMap , fill="#CDCDCD", colour = "#9E9E9E" , size=0.25) +
  geom_sf(data = mappedConnectivity$objCentroid, colour = "#000000",size=3.5) +
  geom_sf(data = mappedConnectivity$objCentroid, colour = "#FFFFFF",size=1.75) +
  geom_sf(data = mappedConnectivity$lineConnections , linewidth = 0.35 , aes(colour = Value), alpha=0.75) +
  scale_color_gradientn(colours=rev(magma(6)),na.value = NA, trans = "log") +
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.ticks.y=element_blank(), legend.position = "none") +
  coord_sf()

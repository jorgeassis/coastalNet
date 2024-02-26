# ---------------------------------------------
# Clean environment and load packages

rm(list = ls())
gc(reset=TRUE)
library(coastalNet)
library(rnaturalearth)
library(viridis)

# ---------------------

# Load a polygon of class sf containing locations (WGS84) of Mediterranean Marine Protected Areas.
europeanMPA <- loadRData("https://raw.githubusercontent.com/jorgeassis/coastalNet/main/vignettes/data/MPAEurope.RData")

# Transform the polygon into a data.frame
europeanMPA <- data.frame(st_coordinates(st_centroid(europeanMPA)))

# ---------------------

# Load database
getDataBase(myFolder="Database", overwrite=FALSE)

# Get hexagon IDs that define the study region
hexagonIDRegion <- getHexagonID(obj=europeanMPA, level="extent", buffer=6, print=TRUE)

# Get connectivity events for the study region (all years, all months, all days, 32 days period)
connectivityEvents <- getConnectivityEvents(hexagonID=hexagonIDRegion, period=32 )

# Get hexagon IDs of the sampling sites
hexagonIDSites <- getHexagonID(obj=europeanMPA, level="site", buffer=0, print=FALSE)

# Get pairwise connectivity estimates between coordinate sites
pairwiseConnectivity <- getPairwiseConnectivity(connectivityEvents, hexagonIDFrom=hexagonIDSites, connType="Forward", value="Probability", steppingStone=FALSE)

# ---------------------

# Map oceanographic connectivity
mappedConnectivity <- mapConnectivity(connectivityPairs=pairwiseConnectivity$connectivityPairs,obj=europeanMPA)

# Load the worldmap and crop to the atudy region
worldMap <- ne_countries(scale = "medium", returnclass = "sf")[,1]
worldMap <- st_crop(worldMap,c(xmin=min(europeanMPA[,1])-5,xmax=max(europeanMPA[,1])+7.5,ymin=min(europeanMPA[,2])-5,ymax=max(europeanMPA[,2])+5))

# Make a plot of the oceanographic connectivity between populations
plot1 <- ggplot() + 
  geom_sf(data = worldMap , fill="#CDCDCD", colour = "#9E9E9E" , size=0.25) +
  geom_point(data = europeanMPA, aes(x = X, y = Y), colour = "#000000",size=2.5) +
  geom_point(data = europeanMPA, aes(x = X, y = Y), colour = "#FFFFFF",size=1) +
  geom_sf(data = mappedConnectivity$lineConnections , linewidth = 0.75 , aes(colour = value)) +
  scale_color_gradientn(colours=rev(magma(6)),na.value = NA, trans = "log") +
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.ticks.y=element_blank(), legend.position = "none") +
  ggtitle("Oceanographic connectivity between Mediterranean Marine Protected Areas")

plot1

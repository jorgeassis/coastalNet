# ---------------------------------------------

# Clean environment
closeAllConnections()
rm(list = ls())
gc(reset=TRUE)

# Load necessary libraries:
library(coastalNet)
library(rnaturalearth)
library(viridis)
library(ggplot2)
library(sf)

# ---------------------

# Download files from repository
download.file("https://figshare.com/ndownloader/files/47592251", "MPAEurope.RData", quiet = TRUE, mode = "wb")

# Load a polygon of class sf containing locations (WGS84) of Mediterranean Marine Protected Areas.
mediterraneanMPA <- loadRData("MPAEurope.RData")

# ---------------------

# Load database
oceanographicConnectivity <- getDataBase()

# Load hexagons (i.e., source and sink locations)
hexagonCells <- loadHexagons()

# Inspect the object hexagonCells
ggplot() + 
  geom_sf(data = hexagonCells, color="black") +
  coord_sf(crs= "+proj=robin") +
  theme_minimal()

# Get hexagon IDs that define the study region
hexagonIDRegion <- getHexagonID(obj=mediterraneanMPA, hexagonCells=hexagonCells, level="extent", buffer=6, print=TRUE)

# Get connectivity events for the study region (all years, all months, all days, 32 days period)
connectivityEvents <- getConnectivityEvents(connectivity=oceanographicConnectivity,hexagonID=hexagonIDRegion, period=32 )

# Get hexagon IDs of the MPA sites
hexagonIDSites <- getHexagonID(obj=mediterraneanMPA, hexagonCells=hexagonCells, level="site", buffer=0, print=FALSE)

# Get pairwise connectivity estimates between MPA sites
pairwiseConnectivity <- getPairwiseConnectivity(connectivityEvents, hexagonIDFrom=hexagonIDSites, connType="Forward", value="Probability", steppingStone=FALSE)

# ---------------------

# Map oceanographic connectivity
mappedConnectivity <- mapConnectivity(obj = mediterraneanMPA, featureName = as.character(unlist(hexagonIDSites)) , connectivityPairs=pairwiseConnectivity$connectivityPairs)

# Load the worldmap and crop to the atudy region
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
worldMap <- st_crop(worldMap,st_bbox(mappedConnectivity$lineConnections)[c(1,3,2,4)] + c(-5,5,-5,5))

# Make a plot of the oceanographic connectivity between populations
ggplot() + 
  geom_sf(data = worldMap , fill="#CDCDCD", colour = "#9E9E9E" , size=0.5) +
  geom_sf(data = mappedConnectivity$objCentroid, colour = "#000000",size=4) +
  geom_sf(data = mappedConnectivity$objCentroid, colour = "#FFFFFF",size=2) +
  geom_sf(data = mappedConnectivity$lineConnections , linewidth = 1 , aes(colour = Value)) +
  scale_color_gradientn(colours=rev(magma(6)),na.value = NA, trans = "log") +
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.ticks.y=element_blank(), legend.position = "none") +
  coord_sf()

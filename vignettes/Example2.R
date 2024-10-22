# ---------------------------------------------
# Clean environment and load packages

rm(list = ls())
gc(reset=TRUE)
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
oceanographicConnectivity <- getDataBase(myFolder="Database", overwrite=FALSE)

# Get hexagon IDs that define the study region
hexagonIDRegion <- getHexagonID(obj=mediterraneanMPA, level="extent", buffer=6, print=TRUE)

# Get connectivity events for the study region (all years, all months, all days, 32 days period)
connectivityEvents <- getConnectivityEvents(connectivity=oceanographicConnectivity,hexagonID=hexagonIDRegion, period=32 )

# Get hexagon IDs of the MPA sites
hexagonIDSites <- getHexagonID(obj=mediterraneanMPA, level="site", buffer=0, print=FALSE)

# Get pairwise connectivity estimates between MPA sites
pairwiseConnectivity <- getPairwiseConnectivity(connectivityEvents, hexagonIDFrom=hexagonIDSites, connType="Forward", value="Probability", steppingStone=FALSE)

# ---------------------

# Map oceanographic connectivity
mappedConnectivity <- mapConnectivity(connectivityPairs=pairwiseConnectivity$connectivityPairs)

# Get hexagon IDs that retrieved oceanographic connectivity estimates
hexagonIDConnected <- unique(c(pairwiseConnectivity$connectivityPairs$FromHexagon,pairwiseConnectivity$connectivityPairs$ToHexagon))

# Get a data.frame of the location of hexagons that retrieved oceanographic connectivity estimates
data("hexagonCells")
hexagonCellsConnected <- hexagonCells[hexagonCells$ID %in% hexagonIDConnected,1]
hexagonCellsConnected <- st_coordinates(st_centroid(hexagonCellsConnected))

# Load the worldmap and crop to the atudy region
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
worldMap <- st_crop(worldMap,st_bbox(mappedConnectivity$lineConnections)[c(1,3,2,4)] + c(-5,5,-5,5))

# Make a plot of the oceanographic connectivity between populations
plot1 <- ggplot() + 
  geom_sf(data = worldMap , fill="#CDCDCD", colour = "#9E9E9E" , size=0.5) +
  geom_sf(data = st_as_sf(data.frame(hexagonCellsConnected), coords = c("X", "Y"), crs = 4326), colour = "#000000",size=4) +
  geom_sf(data = st_as_sf(data.frame(hexagonCellsConnected), coords = c("X", "Y"), crs = 4326), colour = "#FFFFFF",size=2) +
  geom_sf(data = mappedConnectivity$lineConnections , linewidth = 1 , aes(colour = Value)) +
  scale_color_gradientn(colours=rev(magma(6)),na.value = NA, trans = "log") +
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.ticks.y=element_blank(), legend.position = "none") +
  coord_sf()

pdf(file="../../Example 2 1.pdf", width=12, height=8)
plot1
dev.off()


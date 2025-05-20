# ---------------------------------------------

# Clean environment
closeAllConnections()
rm(list = ls())
gc(reset=TRUE)

# Load necessary libraries:
library(coastalNet)
library(rnaturalearth)
library(sf)
library(pheatmap)
library(ggplot2)
library(viridis)

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

# Define regions [lon-lat boxes]
region1 <- c(xmin = 3.3 , xmax = 4.1, ymin = 51.35, ymax = 51.95)
region2 <- c(xmin = 4.675, xmax = 8, ymin = 51.95, ymax = 55.5)

# Convert boxes to 'sf' object
region1 <- st_as_sfc(st_bbox(region1))
region2 <- st_as_sfc(st_bbox(region2))

# Explicitly set the CRS
region1 <- st_set_crs(region1, 4326)
region2 <- st_set_crs(region2, 4326)

# Get hexagon IDs that define the individual study regions, and the combined region
hexagonIDRegion1 <- getHexagonID(obj=region1, hexagonCells=hexagonCells, level="extent", buffer=0.5, print=TRUE)
hexagonIDRegion2 <- getHexagonID(obj=region2, hexagonCells=hexagonCells, level="extent", buffer=0.5, print=TRUE)

# Get hexagon IDs of the combined region
hexagonIDCombinedRange <- unique(c(unlist(hexagonIDRegion1),unlist(hexagonIDRegion2)))

# Get connectivity events for the study region (all years, all months, all days, 180 days period)
connectivityEvents <- getConnectivityEvents(connectivity=oceanographicConnectivity,hexagonID=hexagonIDCombinedRange, period=180)

# Get pairwise connectivity estimates between coordinate sites
pairwiseConnectivity <- getPairwiseConnectivity(connectivityEvents = connectivityEvents,hexagonIDFrom = hexagonIDRegion1,hexagonIDTo = hexagonIDRegion2,connType="Forward",value="Probability", steppingStone=FALSE)

# Get the connectivity matrix
pairwiseConnectivityRegions <- pairwiseConnectivity$connectivityMatrix

# Range of forward pairwise connectivity estimates
range(pairwiseConnectivityRegions)

# Heat map of pairwise connectivity
heatmap(pairwiseConnectivityRegions, display_numbers = FALSE, angle_col=0, cluster_row = FALSE, cluster_cols= FALSE, main = "Heatmap of pairwise connectivity" )

# Get a data.frame of the location of hexagons for which oceanographic connectivity was retrieved
hexagonCellsConnected <- hexagonCells[hexagonCells$ID %in% unlist(hexagonIDCombinedRange),1]
hexagonCellsConnected <- st_coordinates(st_centroid(hexagonCellsConnected))

# Map oceanographic connectivity between populations
mappedConnectivity <- mapConnectivity(obj = hexagonCellsConnected, featureName = as.character(hexagonIDCombinedRange) , connectivityPairs=pairwiseConnectivity$connectivityPairs)

# Load the worldmap and crop to the atudy region
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
worldMap <- st_crop(worldMap,mappedConnectivity$lineConnections)

ggplot() + 
  geom_sf(data = worldMap , fill="#CDCDCD", colour = "#9E9E9E" , size=0.25) +
  geom_sf(data = mappedConnectivity$objCentroid, colour = "#000000",size=2.5) +
  geom_sf(data = mappedConnectivity$objCentroid, colour = "#FFFFFF",size=1.25) +
  geom_sf(data = mappedConnectivity$lineConnections , linewidth = 0.35 , aes(colour = Value), alpha=0.75) +
  scale_color_gradientn(colours=rev(magma(6)),na.value = NA, trans = "log") +
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.ticks.y=element_blank(), legend.position = "none") +
  coord_sf()


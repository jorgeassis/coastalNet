# ---------------------------------------------
# Clean environment and load packages

rm(list = ls())
gc(reset=TRUE)
library(coastalNet)
library(rnaturalearth)
library(sf)
library(pheatmap)

# ---------------------

# Load database
oceanographicConnectivity <- getDataBase(myFolder="Database", overwrite=FALSE)

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
hexagonIDRegion1 <- getHexagonID(obj=region1, level="extent", buffer=0.5, print=TRUE)
hexagonIDRegion2 <- getHexagonID(obj=region2, level="extent", buffer=0.5, print=TRUE)

# Get hexagon IDs of the combined region
hexagonIDCombinedRange <- c(hexagonIDRegion1,hexagonIDRegion2)

# Get connectivity events for the study region (all years, all months, all days, 180 days period)
connectivityEvents <- getConnectivityEvents(connectivity=oceanographicConnectivity,
                                            hexagonID=hexagonIDCombinedRange, period=180)

# Get pairwise connectivity estimates between coordinate sites
pairwiseConnectivity <- getPairwiseConnectivity(connectivityEvents = connectivityEvents,
                                                hexagonIDFrom = hexagonIDRegion1,
                                                hexagonIDTo = hexagonIDRegion2,
                                                connType="Forward",
                                                value="Probability", steppingStone=FALSE)

# Get the connectivity matrix
pairwiseConnectivityRegions <- pairwiseConnectivity$connectivityMatrix

# Range of forward pairwise connectivity estimates
range(pairwiseConnectivityRegions)

# Heat map of pairwise connectivity
plot1 <- pheatmap(pairwiseConnectivityRegions, display_numbers = FALSE, angle_col=0, cluster_row = FALSE, cluster_cols= FALSE, main = "Heatmap of pairwise connectivity" )

pdf(file="../../Example 4 1.pdf", width=8, height=8)
plot1
dev.off()

# Map oceanographic connectivity between populations
mappedConnectivity <- mapConnectivity(connectivityPairs=pairwiseConnectivity$connectivityPairs)

# Load the worldmap and crop to the atudy region
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
worldMap <- st_crop(worldMap,mappedConnectivity$lineConnections)

# Get a data.frame of the location of hexagons for which oceanographic connectivity was retrieved
hexagonCellsConnected <- hexagonCells[hexagonCells$ID %in% unlist(hexagonIDCombinedRange),1]
hexagonCellsConnected <- st_coordinates(st_centroid(hexagonCellsConnected))

plot2 <- ggplot() + 
  geom_sf(data = worldMap , fill="#CDCDCD", colour = "#9E9E9E" , size=0.25) +
  geom_point(data = hexagonCellsConnected, aes(x = X, y = Y), colour = "#000000",size=2.5) +
  geom_point(data = hexagonCellsConnected, aes(x = X, y = Y), colour = "#FFFFFF",size=1.25) +
  geom_sf(data = mappedConnectivity$lineConnections , linewidth = 0.35 , aes(colour = Value), alpha=0.75) +
  scale_color_gradientn(colours=rev(magma(6)),na.value = NA, trans = "log") +
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.ticks.y=element_blank(), legend.position = "none") +
  coord_sf()

pdf(file="../../Example 4 2.pdf", width=8, height=8)
plot2
dev.off()



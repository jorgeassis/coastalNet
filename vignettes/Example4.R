# ---------------------------------------------
# Clean environment and load packages

rm(list = ls())
gc(reset=TRUE)
library(coastalNet)
library(rnaturalearth)
library(sf)

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
combinedRange <- unique(rbind(coastlines1f_filtered,coastlines2f_filtered))

hexagonIDRegion_combined_f <- getHexagonID(obj=combinedRange_f, level="extent", buffer=0.5, print=TRUE)
# Get connectivity events for the study region (all years, all months, all days, 180 days period)
connectivityEvents_f <- getConnectivityEvents(connectivity=oceanographicConnectivity,
                                              hexagonID=hexagonIDRegion_combined_f, period=180)
# Get hexagon IDs of the distribution sites
hexagonIDSitesFrom_f <- getHexagonID(obj=coastlines1f_filtered, level="site", buffer=0, print=FALSE)
hexagonIDSitesTo_f <- getHexagonID(obj=coastlines2f_filtered, level="site", buffer=0, print=FALSE)
# Get pairwise connectivity estimates between coordinate sites
pairwiseConnectivity_f <- getPairwiseConnectivity(connectivityEvents = connectivityEvents_f,
                                                  hexagonIDFrom = hexagonIDSitesFrom_f,
                                                  hexagonIDTo = hexagonIDSitesTo_f,
                                                  connType="Forward",
                                                  value="Probability", steppingStone=FALSE)
print(min(pairwiseConnectivity_f$connectivityPairs$Value))
# Map oceanographic connectivity between populations
mappedConnectivity_f <- mapConnectivity(connectivityPairs=pairwiseConnectivity_f$connectivityPairs)




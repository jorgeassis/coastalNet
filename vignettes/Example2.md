## Fish larvae connectivity among Mediterranean Marine Protected Areas

Networks of Marine Protected Areas (MPAs) must ensure sufﬁcient stepping-stone connectivity for proper conservation of regional pools of biodiversity. This code focuses on mapping fish connectivity among a network of Mediterranean Marine Protected Areas (MPAs). It begins by loading a map of MPA locations and retrieving oceanographic connectivity data from the coastalNet database for the associated region. The code then identifies hexagons encompassing the MPAs and calculates pairwise connectivity probabilities between them, focusing on direct connections of fish populations (average propagule duration of 32 days). Finally, it creates a map visualization highlighting the network of connections between MPAs. The connections are represented by lines, with thicker lines signifying stronger oceanographic connectivity.

By combining oceanographic connectivity information derived from coastalNet package with the distribution of MPAs and larvae duration periods, this script provides an overall view of how fish populations are connected across the Mediterranean MPAs.

Reference: Assis, J., Fragkopoulou, E., Serrão, E. A., Horta e Costa, B. arbara, Gandra, M., & Abecasis, D. (2021). Weak biodiversity connectivity in the European network of no-take marine protected areas. Science of the Total Environment, 773, 1–24. https://doi.org/10.1016/j.scitotenv.2021.145664

### Environment Preparation and Package Loading

Cleans the R environment and forces garbage collection to ensure a clean workspace. Loads necessary R packages for the analysis, which include coastalNet package.

```r 
# Clean environment and load packages
closeAllConnections()
rm(list = ls())
gc(reset=TRUE)
library(coastalNet)
library(rnaturalearth)
library(viridis)
library(ggplot2)
library(sf)
```

### Data Loading

A polygon of class sf representing locations of Mediterranean MPAs is loaded from an online repository.

```r 
# Download files from repository
download.file("https://figshare.com/ndownloader/files/47592251", "MPAEurope.RData", quiet = TRUE, mode = "wb")

# Load a polygon of class sf containing locations (WGS84) of Mediterranean Marine Protected Areas.
mediterraneanMPA <- loadRData("MPAEurope.RData")
```

### Connectivity Analysis

Loads the database of connectivity events (downloads also if not already present). Determines hexagon IDs defining the study region based on the extent of the sampled sites, with a user-defined 6-degree buffer. This buffer allows for the inclusion of connectivity events that extend beyond the defined areas, which is particularly useful for identifying stepping-stone connectivity. Calculates connectivity events within the study region for a specified period (32 days, the mean larvae duration of fish), considering all years, months, and days. Obtains pairwise connectivity estimates between the precise locations of the MPAs.

```r 
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
```

<img src="../img/Example2_img_1.png" alt="Hexagon IDs (in black) defining the study region" style="width:520px;"/>

*Figure: Hexagon IDs (in black) defining the study region.*
<br>

```r 
# Get connectivity events for the study region (all years, all months, all days, 32 days period)
connectivityEvents <- getConnectivityEvents(connectivity=oceanographicConnectivity,hexagonID=hexagonIDRegion, period=32 )

# Get hexagon IDs of the MPA sites
hexagonIDSites <- getHexagonID(obj=mediterraneanMPA, hexagonCells=hexagonCells, level="site", buffer=0, print=FALSE)

# Get pairwise connectivity estimates between MPA sites
pairwiseConnectivity <- getPairwiseConnectivity(connectivityEvents, hexagonIDFrom=hexagonIDSites, connType="Forward", value="Probability", steppingStone=FALSE)

```

### Mapping Connectivity

A comprehensive visualization of oceanographic connectivity between MPAs is generated, illustrating the connections on a map. This visualization includes: A base map of the world, cropped to the study region, serving as a geographical context. Points representing the MPAs and line connections between pairs of MPAs, colored according to the probability or strength of connectivity, providing a clear visual representation of potential ecological links across the marine landscape.

```r
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
```

<img src="../img/Example2_img_2.png" alt="Fish connectivity between Mediterranean Marine Protected Areas" style="width:520px;"/>

*Figure: Fish connectivity between Mediterranean Marine Protected Areas.*


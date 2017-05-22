# Script to make an interactive map for TERENO traps and sites

# ----------------------------------
# Call/install packages
# ----------------------------------
# List of packages for session (add here any new desired packages)
.packages = c("rgdal", "mapview", "ggplot2", "ggsn", "broom", "data.table", "raster")
# Install CRAN packages if not already installed
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
sapply(.packages, require, character.only=TRUE)

# ================================================
# Prepare data
# ================================================

# ----------------------------------
# read layer info from KML
# ----------------------------------
kml.lyr.sites <- rgdal::ogrListLayers("Data/TERENO_2017_Sites.kml")
kml.lyr.traps <- rgdal::ogrListLayers("Data/TERENO_Insect_Traps_2010_17.kml")

# ----------------------------------
# read KML as SpatialPointsDataFrame
# ----------------------------------
# sites - has only one layer
SPdf.sites <- rgdal::readOGR(dsn      = "Data/TERENO_2017_Sites.kml", 
                             layer    = kml.lyr.sites[1], 
                             encoding = "UTF-8")
SPdf.sites@data$Type <- "sites"

# traps - has several layers, needs lapply (or a loop) to read from each layer
lst.SPdf.traps <- lapply(kml.lyr.traps,
                         function(lyr)
                         {
                             sp.lyr <- rgdal::readOGR(dsn      = "Data/TERENO_Insect_Traps_2010_17.kml", 
                                                      layer    = lyr, 
                                                      encoding = "UTF-8")
                             sp.lyr@data$Location <- lyr
                             sp.lyr@data$Type <- "traps"
                             return(sp.lyr)
                         })
# bind the list of SpatialPointsDataFrame-s in a single SpatialPointsDataFrame
SPdf.traps <- do.call(raster::bind, lst.SPdf.traps)

# ================================================
# Make interactive map
# ================================================
# bind traps with sites
SPdf.pts <- raster::bind(SPdf.traps, SPdf.sites)

mapView(SPdf.pts,
        zcol         = "Type",
        col.regions  = c("#f1a340", "#998ec3"), 
        legend       = TRUE,
        layer.name   = "")

# ================================================
# Make static map with ggplot
# ================================================
lst.SPdf.traps <- lapply(lst.SPdf.traps, broom::tidy)
df.traps <- data.table::rbindlist(lst.SPdf.traps, idcol=TRUE)

df.sites <- broom::tidy(SPdf.sites)



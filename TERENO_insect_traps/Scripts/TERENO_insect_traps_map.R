
require(rgdal)

kml.lyr.sites <- rgdal::ogrListLayers("Data/TERENO_2017_Sites.kml")
kml.lyr.sites[1]


SPdf.sites <- rgdal::readOGR(dsn      = "Data/TERENO_2017_Sites.kml", 
                             layer    = kml.lyr.sites[1], 
                             encoding = "UTF-8")
mapview::mapView(SPdf.sites)

kml.lyr.traps <- rgdal::ogrListLayers("Data/TERENO_Insect_Traps_2010_17.kml")
kml.lyr.traps[1:6]
attr(kml.lyr.traps, "nlayers")
length(kml.lyr.traps[1:6])

SPdf.traps <- lapply(kml.lyr.traps,
                     function(lyr)
                         rgdal::readOGR(dsn      = "Data/TERENO_Insect_Traps_2010_17.kml", 
                                        layer    = lyr, 
                                        encoding = "UTF-8"))

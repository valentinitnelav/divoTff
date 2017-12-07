# Test is point 308 was corrected

# Read table from Tiffany with coordinates of new sampled sites.
sites_dt <- data.table(readxl::read_excel(path = "data/sites2017.xlsx", 
                                          sheet = 1, 
                                          col_names = TRUE))
# Keep desired columns and rename to avoid spaces in names.
sites_dt <- sites_dt[,.(site, long, `lat (N)`)]
setnames(sites_dt, old = "lat (N)", new = "lat")


sites_2017_row <- sp::SpatialPointsDataFrame(coords      = sites_dt[site %in% c(307,308,309), .(long, lat)], # order matters
                                             data        = sites_dt[site %in% c(307,308,309),], 
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

mapview(sites_2017_row)
# I modified latitude of point 308. I expect it is 46. instead of 45.
# as the visual inspections of the map confirms

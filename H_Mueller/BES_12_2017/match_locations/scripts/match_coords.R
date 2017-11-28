###############################################################################
# Script to match sampled point locations using minimum distance approach
###############################################################################

library(readxl)
library(data.table)
library(geosphere)
library(mapview)
library(sp)

# =============================================================================
# Read & prepare data
# =============================================================================
# Read table from Tiffany with coordinates of new sampled sites.
sites_dt <- data.table(readxl::read_excel(path = "data/sites2017.xlsx", 
                                       sheet = 1, 
                                       col_names = TRUE))
# Keep desired columns and rename to avoid spaces in names.
sites_dt <- sites_dt[,.(site, long, `lat (N)`)]
setnames(sites_dt, old = "lat (N)", new = "lat")

# Read the Mueller sites.
# These are coordinates extracted from the KMZ files from Walter Durka.
wd_coords_dt <- fread("data/Locations_uniq_records_KMZ_coord.csv")
# Keep desired columns and remove rows without coordinates
wd_coords_dt <- wd_coords_dt[!is.na(LonPartialMatch), .(location3, LonPartialMatch, LatPartialMatch)]
# Rename selected columns
setnames(wd_coords_dt, c("location3_estimate", "lon_loc3_kmz", "lat_loc3_kmz"))

# =============================================================================
# Match by using minimum distance
# =============================================================================
# Compute matrix of great circle distances between new sampled sites and old sites
dist_mat <- geosphere::distm(x = sites_dt[,.(long, lat)],
                             y = wd_coords_dt[,.(lon_loc3_kmz, lat_loc3_kmz)],
                             fun = distHaversine)

# Identify the old sites corresponding to the min dist
old_site_idx <- apply(dist_mat, 1, which.min)

# Get minimum distance for each pair of locations
sites_dt[, min_dist := apply(dist_mat, 1, min, na.rm = TRUE)]

# Bind the old site data to new site data based on min distance matching index
sites_bind <- cbind(sites_dt, 
                    wd_coords_dt[old_site_idx, .(location3_estimate, 
                                                 lon_loc3_kmz, 
                                                 lat_loc3_kmz)])

# Write results to csv file
fwrite(sites_bind, file = "output/sites2017_min_dist_match_location3.csv")

# =============================================================================
# Plot results for visual inspection
# =============================================================================
# Make spatial objects for old sites
mueller_sites_from_kmz <- sp::SpatialPointsDataFrame(coords      = wd_coords_dt[, .(lon_loc3_kmz, lat_loc3_kmz)], # order matters
                                                     data        = wd_coords_dt[, .(location3_estimate)], 
                                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Make spatial objects for new sites
sites_2017 <- sp::SpatialPointsDataFrame(coords      = sites_bind[, .(long, lat)], # order matters
                                         data        = sites_bind[, .(site, location3_estimate, min_dist)], 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Make spatial objects for lines of minimum distance
min_dist_lines <- vector(mode = "list", length = nrow(sites_bind))
for (i in seq_along(min_dist_lines)) {
  min_dist_lines[[i]] <- Lines(slinelist = list(Line(coords = rbind(as.numeric(sites_bind[i, .(long, lat)]), 
                                                                    as.numeric(sites_bind[i, .(lon_loc3_kmz, lat_loc3_kmz)])))), 
                               ID = as.character(i))
}
min_dist_lines <- SpatialLines(min_dist_lines, proj4string = sites_2017@proj4string)
# min_dist_lines@proj4string <- sites_2017@proj4string
min_dist_lines <- SpatialLinesDataFrame(sl = min_dist_lines, 
                                        data = sites_bind[,.(site, location3_estimate, min_dist)], 
                                        match.ID = FALSE)

# Plot
my_map <- mapview(min_dist_lines) + 
  mapview(mueller_sites_from_kmz, col.regions = "red", cex = 10) + 
  mapview(sites_2017, cex = 10)

my_map

# /////////////////////////////////////////////////////////////////////////
## Global maps for pollen limitation dataset for ESA talk of TK.
# /////////////////////////////////////////////////////////////////////////

rm(list = ls()); gc(reset = TRUE)

# List of packages for session (add here any new desired packages)
.myPackages = c("rgdal", "sp", "data.table", "ggplot2", "mapview" ,"gplots")
# Install CRAN packages (if not already installed)
.inst <- .myPackages %in% installed.packages()
if(length(.myPackages[!.inst]) > 0) install.packages(.myPackages[!.inst])
# Load packages into session 
sapply(.myPackages, require, character.only = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read & prepare data -----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read data - get only columns of interest

pl_dt <- fread("data/PL_ANALYSIS_12_04_2018.csv", 
               colClasses = "character",
               na.strings = c("NA","N/A","null", ""),
               select = c("unique_number", 
                          "Year",
                          "lon_decimal_PTL_JMB", 
                          "lat_decimal_PTL_JMB"))
setnames(pl_dt,
         old = c("lon_decimal_PTL_JMB", "lat_decimal_PTL_JMB"),
         new = c("Longitude", "Latitude"))

# Transform year to integer
sort(unique(pl_dt$Year), na.last = TRUE) # check values
pl_dt[, year_int := as.integer(Year)]
pl_dt[Year == "2003b"]
pl_dt[Year == "2003b", year_int := 2003]

# Remove records with NA for year
pl_dt <- pl_dt[!is.na(year_int)]

# Transform year to factor with two levels so to map it into color aesthetic in
# ggplot(). Two year categories.
range(pl_dt$year_int, na.rm = TRUE)
pl_dt[, year_ctg := ifelse(year_int %between% c(1981, 2003), 
                           yes = "1981 - 2003",
                           no = "2004 - 2015")]
pl_dt[, year_ctg := factor(year_ctg, 
                           levels = c("1981 - 2003", "2004 - 2015"),
                           labels = c("1981 - 2003", "2004 - 2015"),
                           ordered = TRUE)]

# Transform longitude to numeric
pl_dt[, lon := as.numeric(Longitude)]

# Transform latitude to numeric
pl_dt[, lat := as.numeric(Latitude)]

# Some routine checking of coordinates values
range(pl_dt[,lon]) %between% c(-180, 180)
range(pl_dt[,lat]) %between% c(-90, 90)

# Get unique pairs of coordinates
# ES_dt_unq <- unique(pl_dt, by = c("lon", "lat"))

# Transform unprojected long-lat in Robinson coordinates
pl_dt[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon, lat),
                                                        proj = "+init=ESRI:54030"))]
# "+init=ESRI:54030" same as "+proj=robin"

# OPTIONAL - check points with interactive map
points_WGS84 <- 
  sp::SpatialPointsDataFrame(coords      = pl_dt[,.(lon,lat)], # order matters
                             data        = pl_dt[,.(unique_number)], 
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
html_map <- mapview(points_WGS84)
# html_map
# save as html
# mapshot(html_map, url = "Global_map_html.html")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load & prepare NaturalEarth shapefiles ----------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("Data/NaturalEarth.RData"); setDT(lbl.Y); setDT(lbl.X)
# Details about NaturalEarth shapefiles:
#  The files were already downloaded from http://www.naturalearthdata.com/
#  Graticules were adjusted to 10 dg for latitude lines and 20 dg for longitude
#  lines (some editing was carried in ArcMap)

# Project from long-lat (unprojected) to Robinson projection
NE_countries_rob  <- spTransform(NE_countries, CRS("+proj=robin"))
NE_graticules_rob <- spTransform(NE_graticules, CRS("+proj=robin"))
NE_box_rob        <- spTransform(NE_box, CRS("+proj=robin"))

# Shift longitude of OX graticule labales. 
# This was needed because for example 160dg label ended up 
# on the 180 longitude line when projecting to Robinson.
# For each degree in the vector 
seq(from = 160, to = 0, by = -20)
# apply the corersponding shift from below 
shift <- c(10, 10, 9, 8, 8, 5, 2, 0, 0)
lbl.X[, shift := rep(c(shift, -rev(shift)[-1]),2)]
lbl.X
lbl.X[, lon := lon - shift] # apply shift
lbl.X[, shift := NULL] # delete column

# Project labales for graticules to Robinson
lbl.Y[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon, lat),
                                                        proj = "+proj=robin"))]
lbl.X[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon, lat),
                                                        proj = "+proj=robin"))]
# Create helper columns with nudged coordinates for plotting graticule labales
# For lbl.Y nudge longitude and for lbl.X nudge latitude.
# Give nudge values in dg (if you change, re-run also the projection lines above)
my_nudge <- cbind(nudge_lon = 10, 
                  nudge_lat = 4) 
my_nudge <- rgdal::project(my_nudge, proj = "+proj=robin")
lbl.Y[, X.prj := ifelse(lon < 0, 
                        yes = X.prj - my_nudge[1,1], 
                        no = X.prj + my_nudge[1,1])]
lbl.X[, Y.prj := ifelse(lat < 0, 
                        yes = Y.prj - my_nudge[1,2], 
                        no = Y.prj + my_nudge[1,2])]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot map ----------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ... Prepare simple map --------------------------------------------------

base_map <- 
  ggplot() +
  # ___ add graticules projected to Robinson
  geom_path(data = NE_graticules_rob, 
            aes(x     = long, 
                y     = lat, 
                group = group), 
            linetype = "dotted", 
            color    = "grey50", 
            size     = 0.1) +
  # ___ add Natural Earth countries projected to Robinson
  geom_polygon(data = NE_countries_rob, 
               aes(x     = long,
                   y     = lat, 
                   group = group), 
               colour = "grey60", # country border color
               fill   = "gray90", # country fill color
               size   = 0.2) +
  # ___ add graticule labels - latitude and longitude
  geom_text(data = lbl.Y, 
            aes(x     = X.prj, 
                y     = Y.prj, 
                label = lbl), 
            color   = "grey50", 
            size    = 1) +
  geom_text(data = lbl.X, 
            aes(x     = X.prj, 
                y     = Y.prj, 
                label = lbl), 
            color   = "grey50", 
            size    = 1) +
  # ___ add Natural Earth box projected to Robinson
  geom_polygon(data = NE_box_rob, 
               aes(x = long, 
                   y = lat), 
               colour ="black", 
               # try also fill="lightblue" but add a separate polygon as first layer
               fill   ="transparent", 
               size   = 0.2) +
  # "Regions defined for each Polygons" warning has to do with fortify
  # transformation. Might get deprecated in future! 
  # ___ the default ratio = 1 in coord_fixed ensures that one unit on the x-axis 
  # is the same length as one unit on the y-axis
  coord_fixed(ratio = 1) +
  # ___ remove the background and default gridlines
  theme_void()

# ... Adjust theme components ---------------------------------------------

custom_theme <- 
  theme(
    # Set font size & family - affects legend only 
    # "sans" = "Arial" and is the default on Windows OS; check windowsFonts()
    text = element_text(size = 8, family = "sans"),
    # Grab bottom-right (x=1, y=0) legend corner 
    legend.justification = c(1, 0),
    # and position it in the bottom-right plot area.
    legend.position = c(1.05, 0.05),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    # Set height of legend items (keys).
    legend.key.height = unit(3, "mm"),
    # Set margin around entire plot.
    plot.margin = unit(c(t = 0, r = 0.8, b = 0, l = -0.5), "cm")
  )

# ... Add study locations (points) ----------------------------------------

# TK: "A figure that shows the map only of studies conducted from 1981-2003.
# This would be the state of our knowledge from the first meta-analysis I
# conducted. I just want one color for dots on the map"
map_1 <- base_map +
  geom_point(data = pl_dt, 
             aes(x = X.prj, 
                 y = Y.prj),
             color = "black",
             size  = 0.6,
             shape = 1,
             alpha = 1) +
  custom_theme

ggsave(plot = map_1, filename = "output/global_map_all_pts_draft_2.png", 
       width = 14, height = 7, units = "cm", dpi = 600)


# TK: "A figure that shows the map of all studies"
map_2 <- base_map +
  geom_point(data = pl_dt[year_int %between% c(1981, 2003)], 
             aes(x = X.prj, 
                 y = Y.prj),
             color = "black",
             size  = 0.6,
             shape = 1,
             alpha = 1) +
  custom_theme

ggsave(plot = map_2, filename = "output/global_map_1981_2003_draft_2.png", 
       width = 14, height = 7, units = "cm", dpi = 600)


# TK: "Can I trouble you for one more figure in which the points from early
# years (1981-2003) and late years (2004-present) are on the same global map but
# the dots are colored to indicate whether they are from the early years vs.
# late years."
# VS: A big issue here is overplotting.

# Try out combinations of colors
colors_lst <- list(
  c("1981 - 2003" = "#1b9e77",  # green
    "2004 - 2015" = "#d95f02"), # red
  
  c("1981 - 2003" = "#d95f02",  # red
    "2004 - 2015" = "#1b9e77"), # green
  
  c("1981 - 2003" = "#d95f02",  # red
    "2004 - 2015" = "#7570b3")  # violet
)

for (i in 1:length(colors_lst)){
  map_3 <- base_map +
    geom_point(data = pl_dt, 
               aes(x = X.prj, 
                   y = Y.prj,
                   color = year_ctg),
               alpha = 0.6,
               size  = 0.6,
               shape = 1) +
    scale_color_manual(name   = "Period",
                       values = colors_lst[[i]]) +
    custom_theme
  
  ggsave(plot = map_3, 
         filename = paste0("output/global_map_year_cls_draft_", i, "-1.png"), 
         width = 14, height = 7, units = "cm", dpi = 600)
  
  # Try out fill for points
  map_3_fill <- base_map +
    geom_point(data = pl_dt, 
               aes(x = X.prj, 
                   y = Y.prj,
                   color = year_ctg,
                   fill = year_ctg),
               alpha = 0.6,
               size  = 0.6,
               shape = 21) +
    scale_color_manual(name   = "Period",
                       values = colors_lst[[i]]) +
    scale_fill_manual(name   = "Period",
                      values = colors_lst[[i]]) +
    custom_theme
  
  ggsave(plot = map_3_fill, 
         filename = paste0("output/global_map_year_cls_draft_", i, "-2.png"), 
         width = 14, height = 7, units = "cm", dpi = 600)
}

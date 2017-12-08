###############################################################################
## Script to clean and bind past, 2016, 2017 data together
###############################################################################
library(data.table)
library(readxl)
library(writexl)

# =============================================================================
# Read historic data
# =============================================================================
mueller_past <- readxl::read_excel(path = "data/Jeroen/Mueller Plants Pollinators 1874-1879 6weeks _ v20171208-2.xlsx", 
                                   sheet = "all data", 
                                   col_names = TRUE)
# The warnings are not affecting current columns of interest

setDT(mueller_past) # transform to data.table object

# Select needed columns only
mueller_past <- mueller_past[,.(year, altitude_mean_rounded,
                                location3, location5,
                                insectspecies, InsectOrderNew_WD,
                                `new plant species (FH)`)]

# =============================================================================
# Read current data 2016
# =============================================================================
mueller_2016 <- readxl::read_excel(path = "data/Jeroen/Mueller Plants Pollinators 2016 _ v20171208-2.xls", 
                                   sheet = "Data", 
                                   col_names = TRUE)

setDT(mueller_2016) # transform to data.table object

# Select needed columns only
mueller_2016 <- mueller_2016[,.(Site, location, location3, location5, 
                                insectspecies, Order, Plant)]

# -------------------------------------
# Merge altitude data with main table
# -------------------------------------
mueller_2016_altitude <- readxl::read_excel(path = "data/Tiffany/mueller data 2016_with elevation.xlsx", 
                                            sheet = "locations", 
                                            col_names = TRUE)
setDT(mueller_2016_altitude) # transform to data.table object

mueller_2016 <- merge(x = mueller_2016,
                      y = mueller_2016_altitude[,.(site, `elevation (m)`)],
                      by.x = "Site",
                      by.y = "site",
                      all.x = TRUE)
rm(mueller_2016_altitude)

# -------------------------------------
# Update missing location3 & location5
# -------------------------------------
# check missing location5 records
mueller_2016[is.na(location5)]

# read assigned minimum distance location 3 to site without location 3 correspondence
loc3_2016 <- data.table(read.csv("../match_locations/output/sites2016_min_dist_match_location3.csv"))

# location 3 - 5 correspondence
locations <- mueller_past[, .(location5 = unique(location5)), by = location3]

# check some differences
setdiff(loc3_2016$location3, locations$location3)
setdiff(locations$location3, loc3_2016$location3)

# add loc 5 info to loc3_2016
loc3_2016 <- merge(x = loc3_2016,
                   y = locations,
                   by.x = "location3_estimate",
                   by.y = "location3",
                   all.x = TRUE)
rm(locations)

# rename some columns to avoid errros when updating in place
setnames(x = loc3_2016, 
         old = c("site", "location5"), 
         new = c("Site", "location5_estimate"))
# Update in place missing location5
mueller_2016[loc3_2016, on = .(Site), location5 := location5_estimate]
# Update in place missing location3
mueller_2016[loc3_2016, on = .(Site), location3 := location3_estimate]

# check missing location5 & 3 records
mueller_2016[is.na(location5)]
mueller_2016[is.na(location3)]

rm(loc3_2016)

# =============================================================================
# Read current data 2017
# =============================================================================
mueller_2017 <- readxl::read_excel(path = "data/Jeroen/Mueller Plants Pollinators 2017 _ v20171208-3.xlsx", 
                                   sheet = "observations", 
                                   col_names = TRUE)

setDT(mueller_2017) # transform to data.table object

# Select needed columns only
mueller_2017 <- mueller_2017[,.(Site, insectspecies, `insect order`, `plant species-correctedWD`)]

# -------------------------------------
# Merge altitude data with main table
# -------------------------------------
mueller_2017_altitude <- readxl::read_excel(path = "data/Jeroen/Mueller Plants Pollinators 2017 _ v20171208-3.xlsx", 
                                            sheet = "sites", 
                                            col_names = TRUE)
setDT(mueller_2017_altitude)
mueller_2017 <- merge(x = mueller_2017,
                      y = mueller_2017_altitude[,.(site, elevation)],
                      by.x = "Site",
                      by.y = "site",
                      all.x = TRUE)
rm(mueller_2017_altitude)

# -------------------------------------
# Add location3 info
# -------------------------------------
# Read matching location3 estimates for 2017 data
# The csv file below is the output of the script 
# BES_12_2017\match_locations\scripts\match_coords.R
loc3_2017 <- fread(input = "../match_locations/output/sites2017_min_dist_match_location3.csv")
# Merge
mueller_2017 <- merge(x = mueller_2017,
                      y = loc3_2017[,.(site, location3_estimate)],
                      by.x = "Site",
                      by.y = "site",
                      all.x = TRUE)
rm(loc3_2017)

# -------------------------------------
# Add location5 info
# -------------------------------------
locations <- mueller_past[, .(location5 = unique(location5)), by = location3]

setdiff(mueller_2017$location3_estimate, locations$location3)
setdiff(locations$location3, mueller_2017$location3_estimate)

mueller_2017 <- merge(x = mueller_2017,
                      y = locations,
                      by.x = "location3_estimate",
                      by.y = "location3",
                      all.x = TRUE)
rm(locations)

# =============================================================================
# Bind all data
# =============================================================================
# Create helper columns
mueller_past[, period := "mueller_past"]
mueller_2016[, period := "mueller_2016"]
mueller_2017[, period := "mueller_2017"]

mueller_past[, site := NA]

mueller_2016[, year := 2016]
mueller_2017[, year := 2017]

# Bind
mueller_all <- 
  rbindlist(
    list(mueller_past[,.(period,
                         year,
                         location3, 
                         location5, 
                         altitude_mean_rounded, 
                         `new plant species (FH)`, 
                         insectspecies,
                         InsectOrderNew_WD,
                         site)], 
         mueller_2016[,.(period,
                         year,
                         location3,
                         location5,
                         `elevation (m)`,
                         Plant,
                         insectspecies,
                         Order,
                         Site)], 
         mueller_2017[,.(period,
                         year,
                         location3_estimate,
                         location5,
                         elevation,
                         `plant species-correctedWD`,
                         insectspecies,
                         `insect order`,
                         Site)]))

setnames(x = mueller_all, 
         old = c("location3", "location5", "altitude_mean_rounded", 
                 "new plant species (FH)", "insectspecies", "InsectOrderNew_WD"),
         new = c("loc_3", "loc_5", "altitude", "plant_sp", "insect_sp", "insect_order"))

# -------------------------------------
# Create column without umlautes;
# they can create problems when aggregating with data.table
# -------------------------------------
mueller_all[, loc_3_umlaut := loc_3]
mueller_all[, loc_3 := gsub(pattern = "ä", replacement = "ae", x = loc_3)]
mueller_all[, loc_3 := gsub(pattern = "ö", replacement = "oe", x = loc_3)]
mueller_all[, loc_3 := gsub(pattern = "ü", replacement = "ue", x = loc_3)]
# check
sort(unique(mueller_all$loc_3))

mueller_all[, loc_5_umlaut := loc_5]
mueller_all[, loc_5 := gsub(pattern = "ä", replacement = "ae", x = loc_5)]
mueller_all[, loc_5 := gsub(pattern = "ö", replacement = "oe", x = loc_5)]
mueller_all[, loc_5 := gsub(pattern = "ü", replacement = "ue", x = loc_5)]
# check
sort(unique(mueller_all$loc_5))

# =============================================================================
# Add location 3 coordinates
# =============================================================================
# These are coordinates extracted from the KMZ files from Walter Durka.
loc3_wd_coords <- fread("../match_locations/data/Locations_uniq_records_KMZ_coord.csv")
setnames(loc3_wd_coords, 
         old = c("LonPartialMatch", "LatPartialMatch"),
         new = c("x_loc_3", "y_loc_3"))
loc3_wd_coords[, location3_umlaut := location3]
loc3_wd_coords[, location3 := gsub(pattern = "ä", replacement = "ae", x = location3)]
loc3_wd_coords[, location3 := gsub(pattern = "ö", replacement = "oe", x = location3)]
loc3_wd_coords[, location3 := gsub(pattern = "ü", replacement = "ue", x = location3)]
write.csv(loc3_wd_coords[is.na(x_loc_3)], "output/check_loc3_NAs_in_coords.csv")

# loc3_wd_coords <- loc3_wd_coords[!is.na(x_loc_3)]

# differences are because of missing coordinates as pointed above
setdiff(loc3_wd_coords$location3, mueller_all$loc_3)
setdiff(mueller_all$loc_3, loc3_wd_coords$location3)
mueller_all[loc_3 == "Piz Lagalp"]

mueller_all <- merge(x = mueller_all,
                     y = loc3_wd_coords[,.(location3, x_loc_3, y_loc_3)],
                     by.x = "loc_3",
                     by.y = "location3",
                     all.x = TRUE,
                     sort = FALSE)

mueller_all[, x_loc_5 := mean(unique(x_loc_3), na.rm = TRUE), by = loc_5]
mueller_all[, y_loc_5 := mean(unique(y_loc_3), na.rm = TRUE), by = loc_5]

rm(loc3_wd_coords)

# =============================================================================
# Harmonize insect species names
# =============================================================================
# -------------------------------------
# Read Syrphidae species names from lookup table
# -------------------------------------
names_syrph <- readxl::read_excel(path = "data/Tiffany/syrphidae3.xlsx", 
                                  sheet = "species names for analysis", 
                                  col_names = TRUE)
setDT(names_syrph)

# check for typos
sort(unique(names_syrph$`name for analysis`), na.last = TRUE)
# print each species name on a separate line
cat(sort(unique(names_syrph$`name for analysis`), na.last = TRUE), sep = "\n") 
# DELETE
# remove the DELETE
names_syrph <- names_syrph[`name for analysis` != "DELETE"]

# check duplicates by Syrphidae_species 
names_syrph[duplicated(names_syrph, by = "Syrphidae_species")]
# 1: Cheilosia caerulescens     old Cheilosia caerulescens

# remove the duplicates
names_syrph <- names_syrph[!duplicated(names_syrph, by = "Syrphidae_species")]

# -------------------------------------
# Read Bombus species names
# -------------------------------------
names_bombus <- readxl::read_excel(path = "data/Jeroen/Bombus recompiled.xlsx", 
                                  sheet = "Bombus", 
                                  col_names = TRUE)
setDT(names_bombus)

# check for typos
sort(unique(names_bombus$`name for analysis`), na.last = TRUE)
# print each species name on a separate line
cat(sort(unique(names_bombus$`name for analysis`), na.last = TRUE), sep = "\n") 

# remove the DELETE
names_bombus <- names_bombus[`name for analysis` != "DELETE"]

# check duplicates 
names_bombus[duplicated(names_bombus, by = "species")]
# none

# -------------------------------------
# Read Lepidoptera species names
# -------------------------------------
names_lepi <- readxl::read_excel(path = "data/Tiffany/leps for analysis.xlsx", 
                                 sheet = "Lepidoptera_species_names_VS", 
                                 col_names = TRUE)
setDT(names_lepi)

# remove moths
names_lepi <- names_lepi[is.na(moth)]

# check for typos
sort(unique(names_lepi$`name for analysis`), na.last = TRUE)
# print each species name on a separate line
cat(sort(unique(names_lepi$`name for analysis`), na.last = TRUE), sep = "\n")
# Found typo:
# Euphydrias aurinia debilis
# Euphydryas aurinia debilis
# correct typo based on discussion with Jeroen
names_lepi[`name for analysis` == "Euphydrias aurinia debilis",
           `name for analysis` := "Euphydryas aurinia debilis"]
# I noticed also an "NA"
names_lepi[is.na(`name for analysis`)]
# 1: Hecatera dysodea     old                NA   NA
# seems to be a moth https://en.wikipedia.org/wiki/Hecatera_dysodea
# and I deleted the record
names_lepi <- names_lepi[insect_sp != "Hecatera dysodea"]

# check duplicates 
names_lepi[duplicated(names_lepi, by = "insect_sp")]
# 1: Plebeius optilete     new Plebejus optilete   NA

# remove the duplicates
names_lepi <- names_lepi[!duplicated(names_lepi, by = "insect_sp")]

# -------------------------------------
# Bind all correct species names
# -------------------------------------
my_lst_dt <- list(names_syrph[,.(Syrphidae_species, `name for analysis`, dataset)], 
                  names_bombus, 
                  names_lepi[,.(insect_sp, `name for analysis`, dataset)])
names(my_lst_dt) <- c("syrphidae", "bombus", "lepidoptera")
names_all <- rbindlist(my_lst_dt, idcol = "group_for_analysis")

setnames(x = names_all, 
         old = c("Syrphidae_species", "name for analysis"),
         new = c("insect_sp", "insect_sp_analysis"))

# -------------------------------------
# Merge with big dataset
# -------------------------------------
mueller_all <- merge(x = mueller_all,
                     y = names_all,
                     by = "insect_sp",
                     all.x = TRUE,
                     sort = FALSE)

setcolorder(x = mueller_all,
            neworder = c("period", "year",
                         "loc_5", "loc_5_umlaut", "x_loc_5", "y_loc_5",
                         "loc_3", "loc_3_umlaut", "x_loc_3", "y_loc_3",
                         "site", "altitude",
                         "plant_sp", 
                         "insect_sp", "insect_sp_analysis", "dataset",
                         "group_for_analysis", "insect_order"))

rm(my_lst_dt, names_all, names_syrph, names_bombus, names_lepi)

# =============================================================================
# Write results
# =============================================================================
write.csv(mueller_all, "output/mueller_all.csv", row.names = FALSE)
writexl::write_xlsx(mueller_all, path = "output/mueller_all.xlsx")


# =============================================================================
# Some aggregation results
# =============================================================================
# mueller_all <- data.table(read.csv("output/mueller_all.csv", stringsAsFactors = FALSE))
loc5_year_counts <- mueller_all[, .N, by = c("loc_5", "year")]
write.csv(loc5_year_counts, "output/loc5_year_counts.csv", row.names = FALSE)
writexl::write_xlsx(loc5_year_counts, path = "output/loc5_year_counts.xlsx")

# Create histogram of altitudes for each location
library(ggplot2)
source("scripts/helpers/altitude_histogram_panel.R")
my_histos <- altitude_histogram_panel(data = mueller_all, 
                                      varb = "altitude", 
                                      wrap_varb = "loc_5", 
                                      xintercept = 2500)
ggsave(filename = "output/all_data_loc5_histogram_altitude.pdf", 
       plot = my_histos, 
       width = 29.7, 
       height = 21, 
       units = "cm")

# redo aggregation, but exclude sites
sites_2remove <- c("Agums, Glurns" ,
                   "Filisur, Schmitten, Wiesen, Alvaneu",
                   "Landeck-Flirsch",
                   "Oberengadin (2)",
                   "Oberengadin (3)",
                   "Surava_Tiefencastel",
                   "Unterengadin",
                   "Val Viola, Bormio")
# exclude sites and some years
loc5_year_counts_removed_loc5_year <- mueller_all[!(loc_5 %in% sites_2remove) & !(year %in% c(1878, 1879)),
                                                  .N, 
                                                  by = c("loc_5", "year")]
sort(unique(loc5_year_counts_removed_loc5_year$loc_5))
rm(sites_2remove)

# Save results
write.csv(loc5_year_counts_removed_loc5_year, 
          file = "output/loc5_year_counts_removed_loc5_year.csv", 
          row.names = FALSE)
writexl::write_xlsx(loc5_year_counts_removed_loc5_year, 
                    path = "output/loc5_year_counts_removed_loc5_year.xlsx")

# remove not needed objects
rm(altitude_histogram_panel, 
   my_histos, 
   loc5_year_counts, 
   loc5_year_counts_removed_loc5_year)
###############################################################################
# Script for testing how Syrphidae insect species vary in space
# Running NMDS on aggregated Syrphidae data
###############################################################################

library(vegan)
library(data.table)
library(readxl)

# =============================================================================
# Read & prepare data
# =============================================================================

### Read Syrphidae species names
# -----------------------------------------------------------------------------
syrph_names <- readxl::read_excel(path = "data/syrphidae3.xlsx", 
                                  sheet = "species names for analysis", 
                                  col_names = TRUE)
setDT(syrph_names)

### Read historic data
# -----------------------------------------------------------------------------
mueller_past <- readxl::read_excel(path = "data/Mueller Plants Pollinators 1874-1879 6weeks _ v20171122.xlsx", 
                                   sheet = "all data", 
                                   col_names = TRUE)
# The warnings are not affecting current columns of interest
setDT(mueller_past)
# Subset
mueller_past <- mueller_past[,.(altitude_mean_rounded, `altitude (m)`,
                                location3, location4, location5,
                                insectspecies, InsectFamilyOld_WD_JE, InsectFamily_TK)]
# rename columns so not to have spaces in column names
setnames(x = mueller_past, 
         old = "altitude (m)",
         new = "altitude_range")
# label all columns with suffix
setnames(x = mueller_past, paste0(names(mueller_past), "_past"))

### Read current data 2016
# -----------------------------------------------------------------------------
mueller_2016 <- readxl::read_excel(path = "data/Mueller Plants Pollinators 2016 _ v20171122.xls", 
                                   sheet = "Data", 
                                   col_names = TRUE)
setDT(mueller_2016)
# Subset
mueller_2016 <- mueller_2016[,.(Site, location, location3, location4, location5, 
                                insectspecies, Family)]
# label all columns with suffix
setnames(x = mueller_2016, paste0(names(mueller_2016), "_2016"))

### Read current data 2017
# -----------------------------------------------------------------------------
mueller_2017 <- readxl::read_excel(path = "data/Mueller Plants Pollinators 2017 _ v20171122.xlsx", 
                                   sheet = "observations", 
                                   col_names = TRUE)
mueller_2017_altitude <- readxl::read_excel(path = "data/Mueller Plants Pollinators 2017 _ v20171122.xlsx", 
                                            sheet = "sites", 
                                            col_names = TRUE)
setDT(mueller_2017)
setDT(mueller_2017_altitude)
# Subset
mueller_2017 <- mueller_2017[,.(Site, insectspecies, `insect family`)]
# rename columns so not to have spaces in column names
setnames(x = mueller_2017, 
         old = "insect family",
         new = "Family")

# Merge altitude data with main table
mueller_2017 <- merge(x = mueller_2017,
                      y = mueller_2017_altitude[,.(site, elevation)],
                      by.x = "Site",
                      by.y = "site",
                      all.x = TRUE)
rm(mueller_2017_altitude)

# Read matching location3 estimates for 2017 data
# The csv file below is th eoutput of the script 
# BES_12_2017\match_locations\scripts\match_coords.R
loc3_2017 <- fread(input = "../match_locations/output/sites2017_min_dist_match_location3.csv")

# Merge with location table
mueller_2017 <- merge(x = mueller_2017,
                      y = loc3_2017[,.(site, location3_estimate)],
                      by.x = "Site",
                      by.y = "site",
                      all.x = TRUE)
rm(loc3_2017)

# label all columns with suffix
setnames(x = mueller_2017, paste0(names(mueller_2017), "_2017"))

##########
merged_dt <- merge(x = mueller_past,
                   y = mueller_2016,
                   by.x = "insectspecies_past",
                   by.y = "insectspecies_2016",
                   all.x = TRUE)
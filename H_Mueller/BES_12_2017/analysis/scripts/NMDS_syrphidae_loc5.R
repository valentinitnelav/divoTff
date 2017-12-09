###############################################################################
## Script for testing how Syrphidae insect species vary in space
###############################################################################

library(data.table)
library(readxl)
library(writexl)
library(ggplot2)
library(ggrepel) # for geom_text_repel() - repel overlapping text labels
library(gplots)  # for plotting text
library(geosphere)

# packages for running nMDS:
library(vegan)
library(checkmate) # was needed by smacof below
library(smacof)

# =============================================================================
# Read data
# =============================================================================
# -------------------------------------
# Read Mueller dataset (past + 2016 + 2017)
# -------------------------------------
# mueller_all.csv was obtain with the prepare_data.R script
mueller_all <- data.table(read.csv("output/mueller_all.csv", stringsAsFactors = FALSE))

# =============================================================================
# Prepare data
# =============================================================================
# Select only syrphidae
insects_dt <- mueller_all[group_for_analysis == "syrphidae"]

# give the folder name where all results will be saved
# this folder will be inside the folder "output"
my_folder <- "syrphidae"

# -------------------------------------
# check plant species names
# -------------------------------------
sort(unique(insects_dt$plant_sp), na.last = TRUE)

# Found following artefacts:
#  "?"  as plant name

# "Helianthemum alpestre (jacq.) DC."      mueller_2016                              
# "Helianthemum alpestre (Jacq.) DC."      mueller_past
# 
# "Knautia dipsacifolia Kreutzer"          mueller_2016                              
# "Knautia dipsacifolia Kreutzer s.l."     mueller_2017
# 
# Senecio rupestris W. et K."            mueller_2016                                  
# "Senecio rupestris Waldst. & Kit."      mueller_2016 & mueller_past

insects_dt <- insects_dt[plant_sp != "?"] # remove the "?" plant records

# Correct the artefacts
insects_dt[plant_sp == "Helianthemum alpestre (jacq.) DC.", 
           plant_sp := "Helianthemum alpestre (Jacq.) DC."]

insects_dt[plant_sp == "Knautia dipsacifolia Kreutzer", 
           plant_sp := "Knautia dipsacifolia Kreutzer s.l."]

insects_dt[plant_sp == "Senecio rupestris W. et K.", 
           plant_sp := "Senecio rupestris Waldst. & Kit."]


# -------------------------------------
# Remove given sites (not enough sampled)
# -------------------------------------
sort(unique(insects_dt$loc_5), na.last = TRUE)

sites_2remove <- c("Agums, Glurns" ,
                   "Filisur, Schmitten, Wiesen, Alvaneu",
                   "Landeck-Flirsch",
                   "Oberengadin (2)",
                   "Oberengadin (3)",
                   "Surava_Tiefencastel",
                   "Unterengadin",
                   "Val Viola, Bormio")
insects_dt <- insects_dt[!(loc_5 %in% sites_2remove)]
rm(sites_2remove)

# save results - they will be used further for the time analysis
write.csv(insects_dt, 
          file = paste0("output/", my_folder, "/", my_folder,
                        "_selected_sites_past&present.csv"), 
          row.names = FALSE)
writexl::write_xlsx(insects_dt, 
                    path = paste0("output/", my_folder, "/", my_folder,
                                  "_selected_sites_past&present.xlsx"))

# -------------------------------------
# Plot altitude histograms
# -------------------------------------
# load helper function
source("scripts/helpers/altitude_histogram_panel.R")

my_histos <- altitude_histogram_panel(data = insects_dt, 
                                      varb = "altitude", 
                                      wrap_varb = "loc_5", 
                                      xintercept = 2500)

# Save multiplot histograms of altitudes in PDF file at:
my_histo_pdf <- paste0("output/", my_folder, "/", my_folder, 
                       "_loc5_histogram_altitude.pdf"); my_histo_pdf

ggsave(filename = my_histo_pdf,
       plot = my_histos, 
       width = 29.7, 
       height = 21, 
       units = "cm")

# remove not needed objects
rm(my_histos, altitude_histogram_panel, my_histo_pdf)

# -------------------------------------
# Split given sites by altitude threshold
# -------------------------------------
altit_threshold <- 2500 # altitude to split a site

sites_2split <- c("Oberengadin (5)",
                  "Stelvio, Piz Umbrail, Spondalonga")

insects_dt[, loc_5_orig := loc_5]
insects_dt[, loc_5 := ifelse( (loc_5 %in% sites_2split) & 
                                (altitude >= altit_threshold),
                              yes = paste0(loc_5, 
                                           " (>=", 
                                           altit_threshold, 
                                           " m)"),
                              no = loc_5 )]


# remove objects that are not needed
rm(sites_2split, altit_threshold)

# =============================================================================
# Run nMDS with vegan & smacof packages
# =============================================================================
# check for NA locations
insects_dt[is.na(loc_5)]
# insects_dt <- insects_dt[!is.na(loc_5)]

# -------------------------------------
# Create location-by-insect-species matrix
# -------------------------------------
commat_loc5_insects_mat <- table( insects_dt[,.(loc_5, insect_sp_analysis)] )
# for easy visual inspection transform to data.frame object
commat_loc5_insects_df <- as.data.frame.matrix(commat_loc5_insects_mat)

# Keep only those locations with at least 5 insect species counts
# commat_loc5_insects_df <- commat_loc5_insects_df[rowSums(commat_loc5_insects_df) >= 5, ]

# Keep only those locations with at least 5 different insect species
# A first test showed that coincidently, this gives identical results as above, 
# but should not happen always
# commat_loc5_insects_df <- commat_loc5_insects_df[rowSums(commat_loc5_insects_df != 0) >= 5, ]

# -------------------------------------
# Run nMDS 
# -------------------------------------
# load helper function
source("scripts/helpers/run_nmds.R")
# run nmds function
nmds_results <- run_nmds(commat_df = commat_loc5_insects_df)
# remove not needed objects
rm(commat_loc5_insects_mat, run_nmds)

# =============================================================================
# Prepare & plot nMDS results
# =============================================================================
# load helper function
source("scripts/helpers/prepare_nmds_coords.R")
# apply function
nmds_points <- prepare_nmds_coords(insects_dt = insects_dt,
                                   nmds_pts   = copy(nmds_results$nmds_points),
                                   altitude_breaks = c(-Inf, 1500, 2000, 2500, Inf))
# remove not needed objects
rm(prepare_nmds_coords)

# -----------------------------------------------------------------------------
# Plot nMDS results
# -----------------------------------------------------------------------------
# load helper function
source("scripts/helpers/plot_nmds_space.R")
# Apply custom plotting function
nmds_plot <- plot_nmds_space(nmds_xy = nmds_points$nmds_pts_altit,
                             label_varb = "my_varb",
                             fill_varb = "altitude_gr",
                             pj = position_jitter(width = 0, height = 0),
                             expand_x = c(1, 0), # passed to scale_x_continuous()
                             expand_y = c(1, 0)) # passed to scale_y_continuous()

# Save plot to PDF file at:
nmds_pdf_file <- paste0("output/", my_folder, "/", my_folder,
                        "_loc5_NMDS_plot_altitude.pdf"); nmds_pdf_file
set.seed(66)
ggsave(filename = nmds_pdf_file,
       plot = nmds_plot, 
       width = 29.7, 
       height = 15, 
       units = "cm")

# remove not needed objects
rm(nmds_plot, plot_nmds_space, nmds_pdf_file)

# =============================================================================
# Exploratory graphs
# =============================================================================
# load helper function
source("scripts/helpers/explore_plots_space.R")

# run helper function and save exploratory graphs in PDF file at:
pdf_file <- paste0("output/", my_folder, "/", 
                   my_folder, "_loc5_exploratory_graphs.pdf"); pdf_file

explore_plots_space(insects_dt    = insects_dt, 
                    nmds_results  = nmds_results, 
                    site_altitude = copy(nmds_points$aggreg_altitude),
                    path = pdf_file)
# Defensively shuts down all open graphics devices.
# This is needed in case the plotting function returns with error and
# doesn't get to run dev.off()
graphics.off()

# remove not needed objects
rm(pdf_file, explore_plots_space)
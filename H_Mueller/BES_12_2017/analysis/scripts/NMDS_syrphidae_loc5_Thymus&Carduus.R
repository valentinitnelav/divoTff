###############################################################################
## Script for testing how Syrphidae insect species vary in space for
# Thymus serpyllum aggr.
# Carduus defloratus L. s.l.
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
# Prepare data
# =============================================================================
directory <- "syrphidae" # folder corresponding to a certain insect for analysis group 

insects_dt <- data.table(read.csv(paste0("output/", directory, 
                                         "/syrphidae_selected_sites_past&present.csv"), 
                                  stringsAsFactors = FALSE))

# check NA locations
insects_dt[is.na(loc_5)]
# insects_dt <- insects_dt[!is.na(loc_5)]

########################################
# NOTE: Change here for subsetting:
# First choose Thymus case and then run all remaining code after this section.
insects_subst <- insects_dt[plant_sp == "Thymus serpyllum aggr."]; my_folder <- "Thymus"
# When you run the line above, skip the one line below for Carduus and 
# jump directly to next section ("Plot altitude histograms")

# Then return here and re-run for Carduus case
insects_subst <- insects_dt[plant_sp == "Carduus defloratus L. s.l."]; my_folder <- "Carduus"
########################################

# -------------------------------------
# Plot altitude histograms
# -------------------------------------
# load helper function
source("scripts/helpers/altitude_histogram_panel.R")

my_histos <- altitude_histogram_panel(data = insects_subst, 
                                      varb = "altitude", 
                                      wrap_varb = "loc_5", 
                                      xintercept = 2500)

# Save multiplot histograms of altitudes in PDF file at:
my_histo_pdf <- paste0("output/", directory, "/", my_folder, "/", my_folder,
                       "_syrphidae_loc5_histogram_altitude.pdf"); my_histo_pdf

ggsave(filename = my_histo_pdf, 
       plot = my_histos, 
       width = 29.7, 
       height = 21, 
       units = "cm")

# remove not needed objects
rm(my_histos, altitude_histogram_panel, my_histo_pdf)

# =============================================================================
# Run nMDS with vegan & smacof packages
# =============================================================================
# Create location-by-insect-species matrix
commat_loc5_insects_mat <- table( insects_subst[,.(loc_5, insect_sp_analysis)] )
# for easy visual inspection transform to data.frame object
commat_loc5_insects_df <- as.data.frame.matrix(commat_loc5_insects_mat)

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
nmds_points <- prepare_nmds_coords(insects_dt = insects_subst,
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
nmds_pdf_file <- paste0("output/", directory, "/", my_folder, "/",
                        my_folder,
                        "_syrphidae_loc5_NMDS_plot_altitude.pdf"); nmds_pdf_file

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
pdf_file <- paste0("output/", directory, "/", my_folder, "/", 
                   my_folder, "_syrphidae_loc5_exploratory_graphs.pdf"); pdf_file

explore_plots_space(insects_dt    = insects_subst, 
                    nmds_results  = nmds_results, 
                    site_altitude = copy(nmds_points$aggreg_altitude),
                    path = pdf_file)
# Defensively shuts down all open graphics devices.
# This is needed in case the plotting function returns with error and
# doesn't get to run dev.off()
graphics.off()

# remove not needed objects
rm(pdf_file, explore_plots_space)
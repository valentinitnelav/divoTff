###############################################################################
## Script for testing how Syrphidae insect species vary in time
###############################################################################

library(data.table)
library(ggplot2)
library(writexl)
library(gplots) # for plotting text

# packages for running nMDS:
library(vegan)
library(checkmate) # was needed by smacof below
library(smacof)

# =============================================================================
# Prepare & plot nMDS results
# =============================================================================
# Give directory name corresponding to a certain group of insects for analysis.
# imput/output if files takes place in this directory
directory <- "bombus"

# Build relative path
my_path <- paste0("output/", directory, "/", directory)

# Read selected sites dataset
insects_dt <- data.table(read.csv(paste0(my_path, "_selected_sites_past&present.csv"), 
                                  stringsAsFactors = FALSE))

# remove 1879 & 1878
insects_dt <- insects_dt[!(year %in% c(1878, 1879))]

# =============================================================================
# Run nMDS with vegan & smacof packages
# =============================================================================
# Create year-by-insect-species matrix
commat_year_insects_mat <- table( insects_dt[,.(year, insect_sp_analysis)] )
# for easy visual inspection transform to data.frame object
commat_year_insects_df <- as.data.frame.matrix(commat_year_insects_mat)

# -------------------------------------
# Run nMDS 
# -------------------------------------
# load helper function
source("scripts/helpers/run_nmds.R")
# run nmds function
nmds_results <- run_nmds(commat_df = commat_year_insects_df)
# remove not needed objects
rm(commat_year_insects_mat, run_nmds)

# -----------------------------------------------------------------------------
# Plot nMDS results
# -----------------------------------------------------------------------------
nmds_plot <- 
  ggplot(data = nmds_results$nmds_points, 
         aes(x = MDS1, 
             y = MDS2)) +
  geom_label(aes(label = my_varb), 
             colour = "black", 
             fill = "gray75",
             fontface = "bold",
             alpha = 0.3,
             size = 6) +
  # Adjust the distance (gap) from axis
  scale_x_continuous(expand = c(0.15, 0)) +
  scale_y_continuous(expand = c(0.15, 0)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ package, 
             scales = "free", 
             labeller = label_both)

# Save plot to PDF file at:
nmds_pdf_file <- paste0(my_path, "_year_NMDS_plot.pdf"); nmds_pdf_file

ggsave(filename = nmds_pdf_file, 
       plot = nmds_plot, 
       width = 29.7, 
       height = 15, 
       units = "cm")

# remove not needed objects
rm(nmds_plot, nmds_pdf_file)

# =============================================================================
# Exploratory graphs
# =============================================================================
# load helper function
source("scripts/helpers/plot_insects_vs_plants_time.R")

# run helper function and save exploratory graphs in PDF file at:
pdf_file <- paste0(my_path, "_year_Jaccard_insects_vs_plants.pdf"); pdf_file

plot_insects_vs_plants_time(insects_dt = insects_dt,
                            nmds_results = nmds_results,
                            main_title = paste0(toupper(directory), "; time"),
                            path = pdf_file)
# Defensively shuts down all open graphics devices.
# This is needed in case the plotting function returns with error and
# doesn't get to run dev.off()
graphics.off()

# remove not needed objects
rm(pdf_file, plot_insects_vs_plants_time)

# =============================================================================
# Aggregation by loc_5 and year
# =============================================================================
counts_by_loc5_year <- insects_dt[, .N, by = c("loc_5", "year")]
writexl::write_xlsx(counts_by_loc5_year, 
                    path = paste0(my_path, "_counts_by_loc5_year.xlsx"))
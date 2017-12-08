# Thymus serpyllum aggr.
# Carduus defloratus L. s.l.

library(data.table)
library(readxl)
library(writexl)
library(ggplot2)
library(ggrepel) # for geom_text_repel() - repel overlapping text labels
library(gplots) # for plotting text

# packages for running nMDS:
library(vegan)
library(checkmate) # was needed by smacof below
library(smacof)

library(geosphere)

# =============================================================================
# Prepare data
# =============================================================================
syrphidae <- data.table(read.csv("output/syrphidae/syrphidae_selected_sites_past&present.csv", 
                                 stringsAsFactors = FALSE))

# check NA locations
syrphidae[is.na(loc_5)]
# syrphidae <- syrphidae[!is.na(loc_5)]

########################################
# change here for subsetting
my_sp_short_name <- "Thymus"
syrph_subst <- syrphidae[plant_sp == "Thymus serpyllum aggr."]

my_sp_short_name <- "Carduus"
syrph_subst <- syrphidae[plant_sp == "Carduus defloratus L. s.l."]
########################################

# -------------------------------------
# Plot altitude histograms
# -------------------------------------
source("scripts/helpers/altitude_histogram_panel.R")
my_histos <- altitude_histogram_panel(data = syrph_subst, 
                                      varb = "altitude", 
                                      wrap_varb = "loc_5", 
                                      xintercept = 2500)

ggsave(filename = paste0("output/syrphidae/", my_sp_short_name, "/", my_sp_short_name,
                         "_syrphidae_loc5_histogram_altitude.pdf"), 
       plot = my_histos, 
       width = 29.7, 
       height = 21, 
       units = "cm")

# remove not needed objects
rm(my_histos, altitude_histogram_panel)

# =============================================================================
# Run nMDS with vegan & smacof packages
# =============================================================================
# Create location-by-insect-species matrix
commat_loc5_insects_mat <- table( syrph_subst[,.(loc_5, insect_sp)] )
# for easy visual inspection transform to data.frame object
commat_loc5_insects_df <- as.data.frame.matrix(commat_loc5_insects_mat)

# Compute Jaccard index
jaccard_dist_insects <-  vegan::vegdist(commat_loc5_insects_df, 
                                        method = "jaccard", 
                                        binary = TRUE)

# -------------------------------------
# vegan - Jaccard
# -------------------------------------
set.seed(2017)
nmds_jaccard_vegan <- vegan::metaMDS(comm = jaccard_dist_insects, k = 2)
nmds_jaccard_vegan # stress is given as proportion from 0 to 1 (from ?metaMDS, section Value)
# Shepard Diagram
stressplot(nmds_jaccard_vegan)

# -------------------------------------
# smacof - Jaccard
# -------------------------------------
set.seed(2017)
nmds_jaccard_smacof <- smacof::mds(delta = jaccard_dist_insects, type = "ordinal")
nmds_jaccard_smacof # stress-1 is given as proportion from 0 to 1
# Shepard Diagram
plot(nmds_jaccard_smacof, plot.type = "Shepard")

# =============================================================================
# Prepare & plot nMDS results
# =============================================================================
# Aggregate altitude
aggreg_altitude <- syrph_subst[, .(altitude_avg_round_100 = round(mean(altitude, na.rm = TRUE)/100)*100,
                                   altitude_avg  = mean(altitude, na.rm = TRUE),
                                   altitue_range = paste0(round(range(altitude, 
                                                                      na.rm = TRUE)/100)*100, 
                                                          collapse = "-")), 
                               by = loc_5]

# Prepare data for ggplot
nmds_points <- rbind(nmds_jaccard_vegan$points,
                     nmds_jaccard_smacof$conf)

nmds_points <- data.table(nmds_points,
                          id = 1:nrow(commat_loc5_insects_df),
                          loc_5 = rownames(commat_loc5_insects_df),
                          package = rep(c("vegan", "smacof"), each = nrow(commat_loc5_insects_df)),
                          dist_idx = "Jaccard")

# Merge coordinates with altitude info
nmds_points <- merge(x = nmds_points,
                     y = aggreg_altitude,
                     by = "loc_5",
                     all.x = TRUE, 
                     sort = FALSE)

# -----------------------------------------------------------------------------
# Plot nMDS results
# -----------------------------------------------------------------------------
source("scripts/helpers/plot_nmds.R")
nmds_points[, altitude_avg_round_100 := factor(altitude_avg_round_100)]

nmds_plot <- plot_nmds(nmds_xy = nmds_points,
                       label_varb = "loc_5",
                       fill_varb = "altitude_avg_round_100",
                       pj = position_jitter(width = 0, height = 0),
                       expand_x = c(0.5, 0), # passed to scale_x_continuous()
                       expand_y = c(0.5, 0)) # passed to scale_y_continuous()

# my_plot
ggsave(filename = paste0("output/syrphidae/", my_sp_short_name, "/",
                         my_sp_short_name,
                         "_syrphidae_loc5_NMDS_plot_altitude.pdf"), 
       # syrphidae_loc5_NMDS_plot.pdf or syrphidae_loc5_NMDS_plot_altitude_groups.pdf
       plot = nmds_plot, 
       width = 29.7, 
       height = 15, 
       units = "cm")

# =============================================================================
# Exploratory graphs
# =============================================================================

# -----------------------------------------------------------------------------
# Jaccard's similarity between sites in Syrphidae (site's jaccard's index from Syrphidae abundances) 
# vs.
# Distance between sites (km)
# -----------------------------------------------------------------------------
loc5_XY <- unique(syrph_subst[,.(loc_5, x_loc_5, y_loc_5)], by = "loc_5")
# It is very important to set order the same as in other "dist" objects:
setorder(loc5_XY, loc_5)
identical(attributes(jaccard_dist_insects)$Labels, loc5_XY$loc_5) # should be TRUE

# Compute matrix of great circle distances between new sampled sites and old sites
dist_mat <- geosphere::distm(x = loc5_XY[,.(x_loc_5, y_loc_5)],
                             y = loc5_XY[,.(x_loc_5, y_loc_5)],
                             fun = distHaversine)
dist_mat <- dist_mat/1000
# row and column names
dimnames(dist_mat) <- list(loc5_XY$loc_5, loc5_XY$loc_5)

# transform to class "dist"
dist_km <- as.dist(dist_mat, diag = TRUE)
# diag = TRUE is just for printing reasons
identical(attributes(jaccard_dist_insects)$Labels,
          attributes(dist_km)$Labels) # should be TRUE

summary(lm(jaccard_dist_insects ~ dist_km))

plot(jaccard_dist_insects ~ dist_km,
     xlab = "Distance between sites (km)")
abline(lm(jaccard_dist_insects ~ dist_km))

# -----------------------------------------------------------------------------
# Jaccard's similarity between sites in Syrphidae (site's jaccard's index from Syrphidae abundances) 
# vs.
# Latitude differences between sites
# -----------------------------------------------------------------------------
# Compute matrix of pair-wise differences in latitude
latitude_dif_mat <- outer(X = loc5_XY$y_loc_5, 
                          Y = loc5_XY$y_loc_5, 
                          FUN = "-")
# row and column names
dimnames(latitude_dif_mat) <- list(loc5_XY$loc_5, loc5_XY$loc_5)

# transform to class "dist"
latitude_dif <- as.dist(abs(latitude_dif_mat), diag = TRUE)
# diag = TRUE is just for printing reasons
identical(attributes(jaccard_dist_insects)$Labels,
          attributes(latitude_dif)$Labels) # should be TRUE

summary(lm(jaccard_dist_insects ~ latitude_dif))

plot(jaccard_dist_insects ~ latitude_dif,
     xlab = "Differences in latitude between sites (degrees)")
abline(lm(jaccard_dist_insects ~ latitude_dif))

# -----------------------------------------------------------------------------
# Jaccard's similarity between sites in Syrphidae (site's jaccard's index from Syrphidae abundances) 
# vs.
# Longitude differences between sites
# -----------------------------------------------------------------------------
# Compute matrix of pair-wise differences in latitude
longitude_dif_mat <- outer(X = loc5_XY$x_loc_5, 
                           Y = loc5_XY$x_loc_5, 
                           FUN = "-")
# row and column names
dimnames(longitude_dif_mat) <- list(loc5_XY$loc_5, loc5_XY$loc_5)

# transform to class "dist"
longitude_dif <- as.dist(abs(longitude_dif_mat), diag = TRUE)
# diag = TRUE is just for printing reasons
identical(attributes(jaccard_dist_insects)$Labels,
          attributes(longitude_dif)$Labels) # should be TRUE

summary(lm(jaccard_dist_insects ~ longitude_dif))

plot(jaccard_dist_insects ~ longitude_dif,
     xlab = "Differences in longitude between sites (degrees)")
abline(lm(jaccard_dist_insects ~ longitude_dif))

# -----------------------------------------------------------------------------
# Jaccard's similarity between sites in Syrphidae (site's jaccard's index from Syrphidae abundances) 
# vs.
# Altitude differences between sites
# -----------------------------------------------------------------------------
site_altitude <- copy(aggreg_altitude)
setorder(site_altitude, loc_5)
identical(attributes(jaccard_dist_insects)$Labels, site_altitude$loc_5)

# Compute matrix of pair-wise differences in altitude
alt_dif_mat <- outer(X = site_altitude$altitude_avg, 
                     Y = site_altitude$altitude_avg, 
                     FUN = "-")
# row and column names
dimnames(alt_dif_mat) <- list(site_altitude$loc_5, site_altitude$loc_5)

# transform to class "dist"
alt_dif <- as.dist(abs(alt_dif_mat), diag = TRUE)
# attributes(alt_dif)$Labels <- site_altitude$loc_5

summary(lm(jaccard_dist_insects ~ alt_dif))

plot(jaccard_dist_insects ~ alt_dif,
     xlab = "Altitude differences between sites (m)")
abline(lm(jaccard_dist_insects ~ alt_dif))

# -----------------------------------------------------------------------------
# Plot all in one PDF file
# -----------------------------------------------------------------------------
pdf(file = paste0("output/syrphidae/", my_sp_short_name, "/",
                  my_sp_short_name,
                  "_syrphidae_loc5_exploratory_graphs.pdf"),
    width = 15/2.54, height = 12/2.54, 
    family = "Times", pointsize = 14)

plot(jaccard_dist_insects ~ dist_km,
     xlab = "Distance between sites (km)")
abline(lm(jaccard_dist_insects ~ dist_km))
gplots::textplot(object = capture.output(summary(lm(jaccard_dist_insects ~ dist_km))),
                 cex = 0.4) 

plot(jaccard_dist_insects ~ latitude_dif,
     xlab = "Differences in latitude between sites (degrees)")
abline(lm(jaccard_dist_insects ~ latitude_dif))
gplots::textplot(object = capture.output(summary(lm(jaccard_dist_insects ~ latitude_dif))),
                 cex = 0.4) 

plot(jaccard_dist_insects ~ longitude_dif,
     xlab = "Differences in longitude between sites (degrees)")
abline(lm(jaccard_dist_insects ~ longitude_dif))
gplots::textplot(object = capture.output(summary(lm(jaccard_dist_insects ~ longitude_dif))),
                 cex = 0.4) 

plot(jaccard_dist_insects ~ alt_dif,
     xlab = "Altitude differences between sites (m)")
abline(lm(jaccard_dist_insects ~ alt_dif))
gplots::textplot(object = capture.output(summary(lm(jaccard_dist_insects ~ alt_dif))),
                 cex = 0.4) 

# close the device
dev.off()
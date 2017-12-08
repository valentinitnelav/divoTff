###############################################################################
## Script for testing how Syrphidae insect species vary in space
###############################################################################

library(data.table)
library(readxl)
library(writexl)
library(ggplot2)
library(ggrepel)    # for geom_text_repel() - repel overlapping text labels
library(gplots) # for plotting text

# packages for running nMDS:
library(vegan)
library(checkmate)  # was needed by smacof below
library(smacof)

library(geosphere)

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

# [76] "Helianthemum alpestre (jacq.) DC."      mueller_2016                              
# [77] "Helianthemum alpestre (Jacq.) DC."      mueller_past
# 
# [90] "Knautia dipsacifolia Kreutzer"          mueller_2016                              
# [91] "Knautia dipsacifolia Kreutzer s.l."     mueller_2017
# 
# [168] "Senecio rupestris W. et K."            mueller_2016                                  
# [169] "Senecio rupestris Waldst. & Kit."      mueller_2016 & mueller_past

insects_dt[plant_sp == "Helianthemum alpestre (jacq.) DC.", 
           plant_sp := "Helianthemum alpestre (Jacq.) DC."]

insects_dt[plant_sp == "Knautia dipsacifolia Kreutzer", 
           plant_sp := "Knautia dipsacifolia Kreutzer s.l."]

insects_dt[plant_sp == "Senecio rupestris W. et K.", 
           plant_sp := "Senecio rupestris Waldst. & Kit."]


# -------------------------------------
# Remove given sites (not enough sampled)
# -------------------------------------
sort(unique(insects_dt$loc_5))

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
source("scripts/helpers/altitude_histogram_panel.R")

my_histos <- altitude_histogram_panel(data = insects_dt, 
                                      varb = "altitude", 
                                      wrap_varb = "loc_5", 
                                      xintercept = 2500)

ggsave(filename = paste0("output/", my_folder, "/", my_folder,
                         "_loc5_histogram_altitude.pdf"), 
       plot = my_histos, 
       width = 29.7, 
       height = 21, 
       units = "cm")

# remove not needed objects
rm(my_histos, altitude_histogram_panel)

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
# Create location-by-insect-species matrix
# =============================================================================
# check for NA locations
insects_dt[is.na(loc_5)]
# insects_dt <- insects_dt[!is.na(loc_5)]

commat_loc5_insects_mat <- table( insects_dt[,.(loc_5, insect_sp)] )

# for easy visual inspection transform to data.frame object
commat_loc5_insects_df <- as.data.frame.matrix(commat_loc5_insects_mat)

# Keep only those locations with at least 5 insect species counts
# commat_loc5_insects_df <- commat_loc5_insects_df[rowSums(commat_loc5_insects_df) >= 5, ]

# Keep only those locations with at least 5 different insect species
# A first test showed that coincidently, this gives identical results as above, 
# but should not happen always
# commat_loc5_insects_df <- commat_loc5_insects_df[rowSums(commat_loc5_insects_df != 0) >= 5, ]


# =============================================================================
# Run nMDS
# =============================================================================

# -------------------------------------
# Compute Jaccard index
# -------------------------------------
jaccard_dist_insects <-  vegan::vegdist(commat_loc5_insects_df, 
                                        method = "jaccard", 
                                        binary = TRUE)

# -----------------------------------------------------------------------------
# Run nMDS with vegan & smacof packages
# -----------------------------------------------------------------------------

# -------------------------------------
# vegan - Jaccard
# -------------------------------------
set.seed(2017)
nmds_jaccard_vegan <- vegan::metaMDS(comm = jaccard_dist_insects, k = 2)

# -------------------------------------
# smacof - Jaccard
# -------------------------------------
set.seed(2017)
nmds_jaccard_smacof <- smacof::mds(delta = jaccard_dist_insects, type = "ordinal")

# =============================================================================
# Prepare & plot nMDS results
# =============================================================================
# Aggregate altitude
aggreg_altitude <- insects_dt[, .(altitude_avg_round_100 = round(mean(altitude, na.rm = TRUE)/100)*100,
                                  altitude_avg  = mean(altitude, na.rm = TRUE),
                                  altitue_range = paste0(round(range(altitude, 
                                                                     na.rm = TRUE)/100)*100, 
                                                         collapse = "-")), 
                              by = loc_5]

aggreg_altitude[, altitude_gr := cut(altitude_avg,
                                     breaks = c(seq(from = floor(round(min(altitude_avg)/100)*100), 
                                                    to = ceiling(round(max(altitude_avg)/100)*100), 
                                                    by = 500),
                                                round(max(altitude_avg)/100)*100),
                                     include.lowest = TRUE, 
                                     right = FALSE)]


# Check unique values
sort(unique(aggreg_altitude$altitude_avg_round_100))
sort(unique(aggreg_altitude$altitue_range))

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

str(nmds_points)
nmds_points[, altitude_avg_round_100 := factor(altitude_avg_round_100)]

nmds_plot <- plot_nmds(nmds_xy = nmds_points,
                       label_varb = "loc_5",
                       fill_varb = "altitude_gr",
                       pj = position_jitter(width = 0.01, height = 0.01),
                       expand_x = c(0.5, 0), # passed to scale_x_continuous()
                       expand_y = c(0.5, 0)) # passed to scale_y_continuous()

set.seed(66)
ggsave(filename = paste0("output/", my_folder, "/", my_folder,
                         "_loc5_NMDS_plot_altitude.pdf"),
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
loc5_XY <- unique(insects_dt[,.(loc_5, x_loc_5, y_loc_5)], by = "loc_5")
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
# Trials with Eckert IV projection gave same results as unprojected coordinates
# library(rgdal)
# # give the PORJ.4 string for Eckert IV projection
# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
# # project coordinates
# # assign matrix of projected coordinates as two columns in data table
# loc5_XY[, c("X","Y") := data.table(project(cbind(x_loc_5, y_loc_5), proj=PROJ))]

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
# Jaccard's similarity in plant community between sites (site's jaccard's index from plant abundances)
# vs.
# Altitude differences between sites
# -----------------------------------------------------------------------------
# Create location-by-plant-species matrix
commat_loc5_plants_mat <- table( insects_dt[,.(loc_5, plant_sp)] )
commat_loc5_plants_df <- as.data.frame.matrix(commat_loc5_plants_mat)

# Jaccard's index from plant abundances per site matrix (above)
jaccard_dist_plants <- vegan::vegdist(commat_loc5_plants_df, method = "jaccard", binary = TRUE)

summary(lm(jaccard_dist_plants ~ alt_dif))

plot(jaccard_dist_plants ~ alt_dif,
     xlab = "Altitude differences between sites (m)")
abline(lm(jaccard_dist_plants ~ alt_dif))

# -----------------------------------------------------------------------------
# Jaccard's similarity between sites in Syrphidae (site's jaccard's index from Syrphidae abundances) 
# vs.
# Jaccard's similarity in plant community between sites (site's jaccard's index from plant abundances)
# -----------------------------------------------------------------------------

summary(lm(jaccard_dist_insects ~ jaccard_dist_plants))
cor(x = jaccard_dist_plants, y = jaccard_dist_insects)

plot(jaccard_dist_insects ~ jaccard_dist_plants)
abline(lm(jaccard_dist_insects ~ jaccard_dist_plants))

# -----------------------------------------------------------------------------
# Plot all in one PDF file
# -----------------------------------------------------------------------------

pdf(file = paste0("output/", my_folder, "/", my_folder,
                  "_loc5_exploratory_graphs.pdf"),
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

plot(jaccard_dist_plants ~ alt_dif,
     xlab = "Altitude differences between sites (m)")
abline(lm(jaccard_dist_plants ~ alt_dif))
gplots::textplot(object = capture.output(summary(lm(jaccard_dist_plants ~ alt_dif))),
                 cex = 0.4) 

plot(jaccard_dist_insects ~ jaccard_dist_plants)
abline(lm(jaccard_dist_insects ~ jaccard_dist_plants))
gplots::textplot(object = capture.output(summary(lm(jaccard_dist_insects ~ jaccard_dist_plants))),
                 cex = 0.4) 

# close the device
dev.off()

# =============================================================================
# References
# =============================================================================
# Jacoby, (2017). NONMETRIC MDS IN R
#     URL: http://www.polisci.msu.edu/jacoby/icpsr/scaling/handouts/mds3/Nonmetric%20MDS%20in%20R,%202017%20Handout.pdf
#
# Mair, P., de Leeuw, J., & Groenen, P. J. (2015). Multidimensional scaling in R: smacof. Technical report. 
#     URL: https://cran.r-project.org/web/packages/smacof/vignettes/smacof.pdf
#
# Oksanen, J. (2009). Multivariate analysis of ecological communities in R: vegan tutorial. 
#     URL: http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
#
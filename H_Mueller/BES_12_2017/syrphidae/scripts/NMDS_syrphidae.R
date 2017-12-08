###############################################################################
## Script for testing how Syrphidae insect species vary in space
## Running NMDS on aggregated Syrphidae data
###############################################################################

library(data.table)
library(readxl)
library(ggplot2)
library(ggrepel)    # for geom_text_repel() - repel overlapping text labels

# packages for running nMDS:
library(vegan)
library(MASS)
library(checkmate)  # was needed by smacof below
library(smacof)

library(geosphere)

# =============================================================================
# Read data
# =============================================================================
# -------------------------------------
# Read Mueller dataset (past + 2016 + 2017)
# -------------------------------------
mueller_all <- data.table(read.csv("output/mueller_all.csv", stringsAsFactors = FALSE))

# -------------------------------------
# Read Syrphidae species names
# -------------------------------------
syrph_names <- readxl::read_excel(path = "data/syrphidae3.xlsx", 
                                  sheet = "species names for analysis", 
                                  col_names = TRUE)
setDT(syrph_names)
# rename columns so not to have spaces in column names
setnames(x = syrph_names, 
         old = "name for analysis",
         new = "syrphidae_analysis")

# check for typos
sort(unique(syrph_names$syrphidae_analysis))
# print each species name on a separate line
cat(sort(unique(syrph_names$syrphidae_analysis)), sep = "\n") 

# check duplicates by Syrphidae_species 
syrph_names[duplicated(syrph_names, by = "Syrphidae_species")]
# 1: Cheilosia caerulescens     old Cheilosia caerulescens

# remove the duplicates
syrph_names <- syrph_names[!duplicated(syrph_names, by = "Syrphidae_species")]


# =============================================================================
# Prepare data
# =============================================================================
# Select only syrphidae
syrph_dt <- mueller_all[insect_sp %in% syrph_names$Syrphidae_species]
# syrph_dt[, insect_sp_orig := insect_sp]

# Attach column with insect species names for analysis
syrph_dt <- merge(x = syrph_dt,
                  y = syrph_names,
                  by.x = "insect_sp",
                  by.y = "Syrphidae_species",
                  all.x = TRUE,
                  sort = FALSE)

# -------------------------------------
# Plot altitude histograms
# -------------------------------------
source("scripts/helpers/altitude_histogram_panel.R")
my_histos <- altitude_histogram_panel(data = syrph_dt, 
                                      varb = "altitude", 
                                      wrap_varb = "loc_5", 
                                      xintercept = 2500)

ggsave(filename = "output/syrphidae_loc5_histogram_altitude.pdf", 
       plot = my_histos, 
       width = 29.7, 
       height = 21, 
       units = "cm")

# remove not needed objects
rm(my_histos, altitude_histogram_panel)

# -------------------------------------
# check plant species names
# -------------------------------------
sort(unique(syrph_dt$plant_sp))

# Found following artefacts:

# [76] "Helianthemum alpestre (jacq.) DC."      mueller_2016                              
# [77] "Helianthemum alpestre (Jacq.) DC."      mueller_past
# 
# [90] "Knautia dipsacifolia Kreutzer"          mueller_2016                              
# [91] "Knautia dipsacifolia Kreutzer s.l."     mueller_2017
# 
# [168] "Senecio rupestris W. et K."            mueller_2016                                  
# [169] "Senecio rupestris Waldst. & Kit."      mueller_2016 & mueller_past

syrph_dt[plant_sp == "Helianthemum alpestre (jacq.) DC.", 
         plant_sp := "Helianthemum alpestre (Jacq.) DC."]

syrph_dt[plant_sp == "Knautia dipsacifolia Kreutzer", 
         plant_sp := "Knautia dipsacifolia Kreutzer s.l."]

syrph_dt[plant_sp == "Senecio rupestris W. et K.", 
         plant_sp := "Senecio rupestris Waldst. & Kit."]

write.csv(syrph_dt, 
          file = "output/syrphidae_all_data_past&present.csv", 
          row.names = FALSE)

# -------------------------------------
# Remove given sites (not enough sampled)
# -------------------------------------
sort(unique(syrph_dt$loc_5))

sites_2remove <- c("Agums, Glurns" ,
                   "Filisur, Schmitten, Wiesen, Alvaneu",
                   "Landeck-Flirsch",
                   "Oberengadin (2)",
                   "Oberengadin (3)",
                   "Surava_Tiefencastel",
                   "Unterengadin",
                   "Val Viola, Bormio")
syrph_dt <- syrph_dt[!(loc_5 %in% sites_2remove)]
rm(sites_2remove)

# -------------------------------------
# Split given sites by altitude threshold
# -------------------------------------
altit_threshold <- 2500 # altitude to split a site

sites_2split <- c("Oberengadin (5)",
                  "Stelvio, Piz Umbrail, Spondalonga")

# sites_2split <- c("Davos, Dischma, Strela",
#                   "Franzenshoehe, Madatsch",
#                   "Oberengadin (5)",
#                   "Stelvio, Piz Umbrail, Spondalonga",
#                   "Suldental")

syrph_dt[, loc_5_orig := loc_5]
syrph_dt[, loc_5 := ifelse( (loc_5 %in% sites_2split) & 
                              (altitude >= altit_threshold),
                            yes = paste0(loc_5, 
                                         " (>=", 
                                         altit_threshold, 
                                         " m)"),
                            no = loc_5 )]

write.csv(syrph_dt, 
          file = "output/syrphidae_selected_sites_past&present.csv", 
          row.names = FALSE)

# remove objects that are not needed
rm(sites_2split, altit_threshold, syrph_names)

# =============================================================================
# Create location-by-insect-species matrix
# =============================================================================
# remove NA locations ( 10 records)
syrph_dt[is.na(loc_5)]
syrph_dt <- syrph_dt[!is.na(loc_5)]

commat_loc5_insects_mat <- table( syrph_dt[,.(loc_5, insect_sp)] )

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
# "Jaccard index has identical rank order (as Bray-Curtis), but has better metric properties, 
# and probably should be preferred." (Oksanen, J., 2009)
jaccard_dist_insects <-  vegan::vegdist(commat_loc5_insects_df, method = "jaccard", binary = TRUE)
# Note that if not mentioning binary = TRUE in vegdist() then "Jaccard index is computed as 2B/(1+B), 
# where B is Bray-Curtis dissimilarity" (from ?vegdist {vegan})
# Nevertheless, the binary argument is not mentioned in the help file of metaMDS() nor of monoMDS(), 
# which are called from metaMDS() at run time.
# check the bug/issue here:
# http://stats.stackexchange.com/questions/242110/nmds-from-jaccard-and-bray-curtis-identical-is-that-a-bad-thing
# https://github.com/joey711/phyloseq/issues/572
# IMPORTANT: Note the arguments of the "vegan" authors at:
# https://github.com/vegandevs/vegan/issues/153


# -----------------------------------------------------------------------------
# Run nMDS with vegan, MASS, & smacof packages
# -----------------------------------------------------------------------------

# -------------------------------------
# vegan - Jaccard
# -------------------------------------
set.seed(2017)
nmds_jaccard_vegan <- vegan::metaMDS(comm = jaccard_dist_insects, k = 2)
nmds_jaccard_vegan # stress is given as proportion from 0 to 1 (from ?metaMDS, section Value)
# Shepard Diagram
stressplot(nmds_jaccard_vegan)

# -------------------------------------
# MASS - Jaccard
# -------------------------------------
set.seed(2017)
nmds_jaccard_MASS <- MASS::isoMDS(d = jaccard_dist_insects, k = 2)
nmds_jaccard_MASS
stressplot(nmds_jaccard_MASS, dis = jaccard_dist_insects)

# -------------------------------------
# smacof - Jaccard
# -------------------------------------
set.seed(2017)
nmds_jaccard_smacof <- smacof::mds(delta = jaccard_dist_insects, type = "ordinal", verbose = TRUE)
nmds_jaccard_smacof # stress-1 is given as proportion from 0 to 1
# Shepard Diagram
plot(nmds_jaccard_smacof, plot.type = "Shepard")

# Compute "Spearman rank-order correlation between input dissimilarities and scaled distances" (Jacoby, 2017)
# This correlation should be the one reported in stressplot() together with the R^2= 1 - S^2
# see details in Oksanen, J. (2009)
cor(x = nmds_jaccard_smacof$delta, 
    y = nmds_jaccard_smacof$confdist, 
    method = "spearman")
1 - nmds_jaccard_smacof$stress^2 # R^2= 1 - S^2

# -------------------------------------
# NOTE about stress values:
# -------------------------------------
# << A large stress value does not necessarily indicate bad fit
# The lower bound of the stress value is 0 (perfect fitt), the upper bound is nontrivial 
# (see De Leeuw and Stoop 1984). What is a 'good' stress value then? 
# Kruskal (1964a) gave some stress-1 benchmarks for ordinal MDS: 
# 0.20 = poor, 0.10 = fair, 0.05 = good, 0.025 = excellent, and 0.00 = perfect. 
# As always, such general rules of thumb are problematic since there are
# many aspects that need to be considered when judging stress 
# (see Borg, Groenen, and Mair 2012, for details).>> in Mair (2015).

# -----------------------------------------------------------------------------
# Comparing models with Procrustes rotation
# -----------------------------------------------------------------------------
# suggested by Jari Oksanen in Oksanen, J. (2009)

# "The descriptive statistic is 'Procrustes sum of squares' 
# or the sum of squared arrows in the Procrustes plot" (Oksanen, J., 2009)
plot(vegan::procrustes(nmds_jaccard_vegan, nmds_jaccard_MASS))
# "You can use identify() function to identify points in an interactive session, 
# or you can ask a plot of residual differences only:" (Oksanen, J., 2009)

# "With argument symmetric = TRUE, both solutions are first scaled to unit variance, 
# and a more scale-independent and symmetric statistic is found (often known as Procrustes m2)."
# (Oksanen, J., 2009)
plot(vegan::procrustes(nmds_jaccard_vegan, nmds_jaccard_MASS, symmetric = TRUE))

# Procrustes between vegan and smacof outputs need to be treated differently.
# e.g. Mair, et. al. (2015). Multidimensional scaling in R: smacof. Technical report.
plot(smacof::Procrustes(nmds_jaccard_vegan$points, nmds_jaccard_smacof$conf))
# same as above, but rotate axis
plot(smacof::Procrustes(nmds_jaccard_smacof$conf, nmds_jaccard_vegan$points))

# =============================================================================
# Prepare & plot nMDS results
# =============================================================================
# Aggregate altitude
aggreg_altitude <- syrph_dt[, .(altitude_avg_round_100 = round(mean(altitude, na.rm = TRUE)/100)*100,
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
pj <- position_jitter(width = 0.01, height = 0.01)
set.seed(66)
my_plot <- 
  ggplot(data = nmds_points, 
         aes(x = MDS1, 
             y = MDS2)) +
  geom_point(aes(fill = factor(altitude_gr)), # altitude_gr or altitude_avg_round_100
             size = 2, 
             pch = 21,
             position = pj,
             show.legend = FALSE) +
  # geom_text(aes(label = loc5),
  #           vjust = 1,
  #           size = 1,
  #           position = pj) +
  geom_label_repel(aes(label = loc_5,
                       fill = factor(altitude_gr)), # altitude_gr or altitude_avg_round_100
                   show.legend = TRUE,
                   fontface = 'bold', 
                   color = 'black',
                   # Add extra padding around each text label.
                   box.padding = 0.6,
                   # Add extra padding around each data point.
                   point.padding = 0.2,
                   # label size
                   size = 1.7,
                   alpha = 0.6,
                   # Color of the line segments.
                   segment.color = '#cccccc',
                   # line segment transparency
                   segment.alpha = .6,
                   # line segment thickness
                   segment.size = .25,
                   # the force of repulsion between overlapping text labels
                   force = 50,
                   # maximum number of iterations to attempt to resolve overlaps
                   max.iter = 10e4,
                   min.segment.length = 0.01,
                   seed = 2017) +
  # Adjust the distance (gap) from axis
  scale_x_continuous(expand = c(0.5, 0)) +
  scale_y_continuous(expand = c(0.5, 0)) +
  labs(fill = 'Average altitude (m)') +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(package ~ dist_idx, 
             scales = "free", 
             labeller = label_both)
# my_plot
ggsave(filename = "output/syrphidae_loc5_NMDS_plot_altitude_groups.pdf", # syrphidae_loc5_NMDS_plot.pdf or syrphidae_loc5_NMDS_plot_altitude_groups.pdf
       plot = my_plot, 
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
loc5_XY <- unique(syrph_dt[,.(loc_5, x_loc_5, y_loc_5)], by = "loc_5")
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
commat_loc5_plants_mat <- table( syrph_dt[,.(loc_5, plant_sp)] )
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
pdf(file = "output/sryphidae_loc5_exploratory_graphs.pdf",
    width = 15/2.54, height = 12/2.54, 
    family = "Times", pointsize = 14)

plot(jaccard_dist_insects ~ dist_km,
     xlab = "Distance between sites (km)")
abline(lm(jaccard_dist_insects ~ dist_km))

plot(jaccard_dist_insects ~ latitude_dif,
     xlab = "Differences in latitude between sites (degrees)")
abline(lm(jaccard_dist_insects ~ latitude_dif))

plot(jaccard_dist_insects ~ longitude_dif,
     xlab = "Differences in longitude between sites (degrees)")
abline(lm(jaccard_dist_insects ~ longitude_dif))

plot(jaccard_dist_insects ~ alt_dif,
     xlab = "Altitude differences between sites (m)")
abline(lm(jaccard_dist_insects ~ alt_dif))

plot(jaccard_dist_plants ~ alt_dif,
     xlab = "Altitude differences between sites (m)")
abline(lm(jaccard_dist_plants ~ alt_dif))

plot(jaccard_dist_insects ~ jaccard_dist_plants)
abline(lm(jaccard_dist_insects ~ jaccard_dist_plants))

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
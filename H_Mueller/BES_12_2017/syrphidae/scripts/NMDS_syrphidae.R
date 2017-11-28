###############################################################################
# Script for testing how Syrphidae insect species vary in space
# Running NMDS on aggregated Syrphidae data
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

# =============================================================================
# Read & prepare data
# =============================================================================

### Read Syrphidae species names
# -----------------------------------------------------------------------------
syrph_names <- readxl::read_excel(path = "data/syrphidae3.xlsx", 
                                  sheet = "species names for analysis", 
                                  col_names = TRUE)
setDT(syrph_names)
# rename columns so not to have spaces in column names
setnames(x = syrph_names, 
         old = "name for analysis",
         new = "syrphidae_sp")

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

# Split altitude range in min & max values & compute average
mueller_past[, c("alt1", "alt2") := tstrsplit(x = altitude_range, 
                                              split = '-', 
                                              type.convert = TRUE)]
mueller_past[, altitude_avg :=  rowMeans(.SD, na.rm = TRUE), 
             .SDcols = c("alt1", "alt2")]

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
setDT(mueller_2017)
# Subset
mueller_2017 <- mueller_2017[,.(Site, insectspecies, `insect family`)]
# rename columns so not to have spaces in column names
setnames(x = mueller_2017, 
         old = "insect family",
         new = "Family")

# Merge altitude data with main table
# -------------------------------------
mueller_2017_altitude <- readxl::read_excel(path = "data/Mueller Plants Pollinators 2017 _ v20171122.xlsx", 
                                            sheet = "sites", 
                                            col_names = TRUE)
setDT(mueller_2017_altitude)
mueller_2017 <- merge(x = mueller_2017,
                      y = mueller_2017_altitude[,.(site, elevation)],
                      by.x = "Site",
                      by.y = "site",
                      all.x = TRUE)
rm(mueller_2017_altitude)

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

# Add location5 info
# -------------------------------------
locations <- mueller_past[, .(location5_past = unique(location5_past)), 
                          by = location3_past]
mueller_2017 <- merge(x = mueller_2017,
                      y = locations,
                      by.x = "location3_estimate",
                      by.y = "location3_past",
                      all.x = TRUE)
rm(locations)

# label all columns with suffix
# -------------------------------------
setnames(x = mueller_2017, paste0(names(mueller_2017), "_2017"))


# =============================================================================
# Count syrphidae by locations
# =============================================================================

# First prepare table of common column names and then rbind them in a long format
# -----------------------------------------------------------------------------

# For past data
# -------------------------------------
syrph_past <- mueller_past[insectspecies_past %in% syrph_names[,syrphidae_sp], 
                           .(insectspecies_past, location5_past, altitude_avg_past)]
setnames(x = syrph_past, c("insect_sp", "loc_5", "altitude"))
syrph_past[, period := "mueller_past"]

# For 2016 data
# -------------------------------------
syrph_2016 <- mueller_2016[insectspecies_2016 %in% syrph_names[,syrphidae_sp], 
                           .(insectspecies_2016, location5_2016)]
setnames(x = syrph_2016, c("insect_sp", "loc_5"))
# there is no altitude data so far;
# create empty column for merging reasons
syrph_2016[, altitude := NA] 
syrph_2016[, period := "mueller_2016"]

# For 2017 data
# -------------------------------------
syrph_2017 <- mueller_2017[insectspecies_2017 %in% syrph_names[,syrphidae_sp], 
                           .(insectspecies_2017, location5_past_2017, elevation_2017)]
setnames(x = syrph_2017, c("insect_sp", "loc_5", "altitude"))
syrph_2017[, period := "mueller_2017"]

# rbind (get the long format)
# -------------------------------------
syrph_dt <- rbindlist(list(syrph_past, syrph_2016, syrph_2017))

# remove objects that are not needed
# rm(syrph_past, syrph_2016, syrph_2017, syrph_names,
#    mueller_past, mueller_2016, mueller_2017)


# Create location-by-insect-species matrix
# -----------------------------------------------------------------------------
# remove NA locations
syrph_dt <- syrph_dt[!is.na(loc_5)]
loc5sp_mat <- table( syrph_dt[,.(loc_5, insect_sp)] )

# for easy visual inspection transform to data.frame object
loc5sp_df <- as.data.frame.matrix(loc5sp_mat)

# Keep only those locations with at least 5 insect species counts
# loc5sp_df <- loc5sp_df[rowSums(loc5sp_df) >= 5, ]

# Keep only those locations with at least 5 different insect species
# A first test showed that coincidently, this gives identical results as above, 
# but should not happen always
# loc5sp_df <- loc5sp_df[rowSums(loc5sp_df != 0) >= 5, ]

# =============================================================================
# Run nMDS
# =============================================================================

# Compute distances
# -----------------------------------------------------------------------------

# Bray-Curtis index
# -------------------------------------
bray_dist <- vegdist(loc5sp_df, method = "bray")

# Jaccard index
# -------------------------------------
# "Jaccard index has identical rank order (as Bray-Curtis), but has better metric properties, 
# and probably should be preferred." (Oksanen, J., 2009)
jaccard_dist <- vegdist(loc5sp_df, method = "jaccard", binary = TRUE)
# Note that if not mentioning binary = TRUE in vegdist() then "Jaccard index is computed as 2B/(1+B), 
# where B is Bray-Curtis dissimilarity" (from ?vegdist {vegan})
# Nevertheless, the binary argument is not mentioned in the help file of metaMDS() nor of monoMDS(), 
# which are called from metaMDS() at run time.
# check the bug/issue here:
# http://stats.stackexchange.com/questions/242110/nmds-from-jaccard-and-bray-curtis-identical-is-that-a-bad-thing
# https://github.com/joey711/phyloseq/issues/572
# IMPORTANT: Note the arguments of the "vegan" authors at:
# https://github.com/vegandevs/vegan/issues/153


# Run nMDS with vegan, MASS, & smacof packages
# -----------------------------------------------------------------------------

# vegan - Bray-Curtis
# -------------------------------------
set.seed(2017)
nmds_bray_vegan <- vegan::metaMDS(comm = bray_dist, k = 2, autotransform = FALSE)
nmds_bray_vegan # stress is given as proportion from 0 to 1 (from ?metaMDS, section Value)
# Shepard Diagram
stressplot(nmds_bray_vegan)

# vegan - Jaccard
# -------------------------------------
set.seed(2017)
nmds_jaccard_vegan <- vegan::metaMDS(comm = jaccard_dist, k = 2, autotransform = FALSE)
nmds_jaccard_vegan
stressplot(nmds_jaccard_vegan)

# MASS - Bray-Curtis
# -------------------------------------
set.seed(2017)
nmds_bray_MASS <- MASS::isoMDS(d = bray_dist, k = 2)
nmds_bray_MASS # stress is given as percent from 0 to 100% (from ?metaMDS, section Value)
# Shepard Diagram
stressplot(nmds_bray_MASS, dis = bray_dist)

# MASS - Jaccard
# -------------------------------------
nmds_jaccard_MASS <- MASS::isoMDS(d = jaccard_dist, k = 2)
nmds_jaccard_MASS
stressplot(nmds_jaccard_MASS, dis = jaccard_dist)

# smacof - Bray-Curtis
# -------------------------------------
set.seed(2017)
nmds_bray_smacof <- smacof::mds(delta = bray_dist, type = "ordinal", verbose = TRUE)
nmds_bray_smacof # stress-1 is given as proportion from 0 to 1
# Shepard Diagram
plot(nmds_bray_smacof, plot.type = "Shepard")

# Compute "Spearman rank-order correlation between input dissimilarities and scaled distances" (Jacoby, 2017)
# This correlation should be the one reported in stressplot() together with the R^2= 1 - S^2
# see details in Oksanen, J. (2009)
cor(x = nmds_bray_smacof$delta, 
    y = nmds_bray_smacof$confdist, 
    method = "spearman")
1 - nmds_bray_smacof$stress^2 # R^2= 1 - S^2

# smacof - Jaccard
# -------------------------------------
nmds_jaccard_smacof <- smacof::mds(delta = jaccard_dist, type = "ordinal", verbose = TRUE)
nmds_jaccard_smacof
plot(nmds_jaccard_smacof, plot.type = "Shepard")
cor(x = nmds_jaccard_smacof$delta, 
    y = nmds_jaccard_smacof$confdist, 
    method = "spearman")
1 - nmds_jaccard_smacof$stress^2 # R^2= 1 - S^2


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


# Comparing models with Procrustes rotation
# -----------------------------------------------------------------------------
# suggested by Jari Oksanen in Oksanen, J. (2009)

# Bray-Curtis
# -------------------------------------
pro <- vegan::procrustes(nmds_bray_vegan, nmds_bray_MASS)
# "The descriptive statistic is 'Procrustes sum of squares' 
# or the sum of squared arrows in the Procrustes plot" (Oksanen, J., 2009)
pro
# "You can use identify() function to identify points in an interactive session, 
# or you can ask a plot of residual differences only:" (Oksanen, J., 2009)
plot(pro)
plot(pro, kind = 2)

# "With argument symmetric = TRUE, both solutions are first scaled to unit variance, 
# and a more scale-independent and symmetric statistic is found (often known as Procrustes m2)."
# (Oksanen, J., 2009)
pro_sym <- vegan::procrustes(nmds_bray_vegan, nmds_bray_MASS, symmetric = TRUE)
pro_sym
plot(pro_sym)
# Between the vegan and MASS outputs one does not expect noticeable differences since vegan
# actually calls to MASS::isoMDS() function.

# Procrustes between vegan and smacof outputs need to be treated differently.
# e.g. Mair, et. al. (2015). Multidimensional scaling in R: smacof. Technical report.
# URL 
pro2 <- smacof::Procrustes(nmds_bray_vegan$points, nmds_bray_smacof$conf)
pro2
plot(pro2)
# or rotate axis
plot(smacof::Procrustes(nmds_bray_smacof$conf, nmds_bray_vegan$points))

# Jaccard
# -------------------------------------
plot(vegan::procrustes(nmds_jaccard_vegan, nmds_jaccard_MASS))

plot(smacof::Procrustes(nmds_jaccard_vegan$points, nmds_jaccard_smacof$conf))
# same as above, but rotate axis
plot(smacof::Procrustes(nmds_jaccard_smacof$conf, nmds_jaccard_vegan$points))

# =============================================================================
# Prepare & plot nMDS results
# =============================================================================
# Aggregate altitude
aggreg_altitude <- syrph_dt[, .(altitude_avg = round(mean(altitude, na.rm = TRUE)/100)*100,
                                altitue_range = paste0(round(range(altitude, 
                                                                   na.rm = TRUE)/100)*100, 
                                                       collapse = "-")), 
                            by = loc_5]
# Check unique values
sort(unique(aggreg_altitude$altitude_avg))
sort(unique(aggreg_altitude$altitue_range))

# Prepare data for ggplot
nmds_points <- rbind(nmds_bray_vegan$points,
                     nmds_jaccard_vegan$points,
                     nmds_bray_smacof$conf,
                     nmds_jaccard_smacof$conf)
nmds_points <- data.table(nmds_points,
                          id = 1:nrow(loc5sp_df),
                          loc5 = rownames(loc5sp_df),
                          package = rep(c("vegan", "smacof"), each = 2* nrow(loc5sp_df)),
                          dist_idx = rep(c("Bray-Curtis", "Jaccard"), 
                                         each = nrow(loc5sp_df), times = 2))
# Merge coordinates with altitude info
nmds_points <- merge(x = nmds_points,
                     y = aggreg_altitude,
                     by.x = "loc5",
                     by.y = "loc_5",
                     all.x = TRUE, 
                     sort = FALSE)

# Plot nMDS results
# -----------------------------------------------------------------------------
pj <- position_jitter(width = 0.01, height = 0.01)
set.seed(66)
my_plot <- 
  ggplot(data = nmds_points, 
         aes(x = MDS1, 
             y = MDS2)) +
  geom_point(aes(fill = factor(altitude_avg)),
             size = 2, 
             pch = 21,
             position = pj,
             show.legend = FALSE) +
  # geom_text(aes(label = loc5),
  #           vjust = 1,
  #           size = 1,
  #           position = pj) +
  geom_label_repel(aes(label = loc5,
                       fill = factor(altitude_avg)),
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
ggsave(filename = "output/nMDS_loc5.pdf", 
       plot = my_plot, 
       width = 29.7, 
       height = 21, 
       units = "cm")

# Plot altitude histograms
# -----------------------------------------------------------------------------
my_histos <- 
  ggplot(data = syrph_dt,
         aes(altitude)) +
  geom_histogram() +
  facet_wrap(~loc_5) +
  theme_bw() +
  # edit strip text for each panel
  theme(strip.text = element_text(size = 8, 
                                  face = "bold"))

ggsave(filename = "output/histo_altitude_loc5.pdf", 
       plot = my_histos, 
       width = 29.7, 
       height = 21, 
       units = "cm")

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
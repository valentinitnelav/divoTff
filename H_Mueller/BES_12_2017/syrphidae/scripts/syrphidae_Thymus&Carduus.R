# Thymus serpyllum aggr.
# Carduus defloratus L. s.l.

library(data.table)
library(vegan)
library(geosphere)

syrphidae <- data.table(read.csv("output/syrphidae_selected_sites_past&present.csv", 
                                 stringsAsFactors = FALSE))

# remove NA locations
syrphidae[is.na(loc_5)]
syrphidae <- syrphidae[!is.na(loc_5)]

########################################
# change here for subsetting
syrph_subst <- syrphidae[plant_sp == "Thymus serpyllum aggr."]

syrph_subst <- syrphidae[plant_sp == "Carduus defloratus L. s.l."]
########################################

commat_loc5_insects_mat <- table( syrph_subst[,.(loc_5, insect_sp)] )
# for easy visual inspection transform to data.frame object
commat_loc5_insects_df <- as.data.frame.matrix(commat_loc5_insects_mat)
jaccard_dist_insects <-  vegan::vegdist(commat_loc5_insects_df, 
                                        method = "jaccard", 
                                        binary = TRUE)

aggreg_altitude <- syrph_subst[, .(altitude_avg = mean(altitude, na.rm = TRUE)), 
                               by = loc_5]
setorder(aggreg_altitude, loc_5)
# Compute matrix of pair-wise differences in altitude
alt_dif_mat <- outer(X = aggreg_altitude$altitude_avg, 
                     Y = aggreg_altitude$altitude_avg, 
                     FUN = "-")
# row and column names
dimnames(alt_dif_mat) <- list(aggreg_altitude$loc_5, aggreg_altitude$loc_5)

# transform to class "dist"
alt_dif <- as.dist(abs(alt_dif_mat), diag = TRUE)

###
loc5_XY <- unique(syrph_subst[,.(loc_5, x_loc_5, y_loc_5)], by = "loc_5")

# Compute matrix of great circle distances between new sampled sites and old sites
dist_mat <- geosphere::distm(x = loc5_XY[,.(x_loc_5, y_loc_5)],
                             y = loc5_XY[,.(x_loc_5, y_loc_5)],
                             fun = distHaversine)
dist_mat <- dist_mat/1000
# row and column names
dimnames(dist_mat) <- list(loc5_XY$loc_5, loc5_XY$loc_5)

dist_km <- as.dist(dist_mat, diag = TRUE)

# Plot
########################################
# change here for changing graph title
my_pant <- "Thymus serpyllum aggr."

my_pant <- "Carduus defloratus L. s.l."
########################################

pdf(file = paste0("output/", "exploratory_graphs_", my_pant, ".pdf"),
    width = 15/2.54, height = 12/2.54, 
    family = "Times", pointsize = 14)

plot(x = alt_dif, y = jaccard_dist_insects,
     xlab = "Altitude differences between sites (m)",
     main = my_pant)
abline(lm(jaccard_dist_insects ~ alt_dif))

plot(x = dist_km, y = jaccard_dist_insects,
     xlab = "Distance between sites (km)",
     main = my_pant)
abline(lm(jaccard_dist_insects ~ dist_km))

# close the device
dev.off()
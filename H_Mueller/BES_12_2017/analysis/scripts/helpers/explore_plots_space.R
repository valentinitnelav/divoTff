###############################################################################
## Function to run Exploratory graphs
###############################################################################

explore_plots_space <- function(insects_dt, 
                                nmds_results,
                                site_altitude,
                                path)
{
  # -----------------------------------------------------------------------------
  # Open PDF device = plot all graphs in one PDF file
  # -----------------------------------------------------------------------------
  pdf(file = path,
      width = 15/2.54, height = 12/2.54, 
      family = "Times", pointsize = 14)
  
  # -----------------------------------------------------------------------------
  # Jaccard's dissimilarity between sites in insect community 
  # vs.
  # Distance between sites (km)
  # -----------------------------------------------------------------------------
  loc5_XY <- unique(insects_dt[,.(loc_5, x_loc_5, y_loc_5)], by = "loc_5")
  # It is very important to set order the same as in other "dist" objects:
  setorder(loc5_XY, loc_5)
  # identical(attributes(jaccard_dist_insects)$Labels, loc5_XY$loc_5) # should be TRUE
  
  # Compute matrix of great circle distances between new sampled sites and old sites
  dist_mat <- geosphere::distm(x = loc5_XY[,.(x_loc_5, y_loc_5)],
                               y = loc5_XY[,.(x_loc_5, y_loc_5)],
                               fun = distHaversine)
  dist_mat <- dist_mat/1000 # transform to km
  # row and column names
  dimnames(dist_mat) <- list(loc5_XY$loc_5, loc5_XY$loc_5)
  
  # transform to class "dist"
  dist_km <- as.dist(dist_mat, diag = TRUE)
  # diag = TRUE is just for printing reasons
  # identical(attributes(jaccard_dist_insects)$Labels,
  #           attributes(dist_km)$Labels) # should be TRUE
  
  jaccard_dist_insects <- nmds_results$jaccard_dist
  
  formula_jacc_insect_by_dist_km <- formula(jaccard_dist_insects ~ dist_km)
  
  print({  
    plot(formula_jacc_insect_by_dist_km,
         xlab = "Distance between sites (km)")
    abline(lm(formula_jacc_insect_by_dist_km))
    # plot lm summary text as graph
    gplots::textplot(object = capture.output(summary(lm(formula_jacc_insect_by_dist_km))),
                     cex = 0.4) 
  })
  
  # -----------------------------------------------------------------------------
  # Jaccard's dissimilarity between sites in insect community  
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
  # identical(attributes(jaccard_dist_insects)$Labels,
  #           attributes(latitude_dif)$Labels) # should be TRUE
  
  formula_jacc_insect_by_lat <- formula(jaccard_dist_insects ~ latitude_dif)
  
  print({ 
    plot(formula_jacc_insect_by_lat,
         xlab = "Differences in latitude between sites (degrees)")
    abline(lm(formula_jacc_insect_by_lat))
    # plot lm summary text as graph
    gplots::textplot(object = capture.output(summary(lm(formula_jacc_insect_by_lat))),
                     cex = 0.4) 
  })
  # Note that, as expected, trials with projected coordinates (Eckert IV projection) 
  # gave same results as unprojected coordinates.
  
  # -----------------------------------------------------------------------------
  # Jaccard's dissimilarity between sites in insect community  
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
  
  formula_jacc_insect_by_long <- formula(jaccard_dist_insects ~ longitude_dif)
  
  print({ 
    plot(formula_jacc_insect_by_long,
         xlab = "Differences in longitude between sites (degrees)")
    abline(lm(formula_jacc_insect_by_long))
    # plot lm summary text as graph
    gplots::textplot(object = capture.output(summary(lm(formula_jacc_insect_by_long))),
                     cex = 0.4) 
  })
  
  # -----------------------------------------------------------------------------
  # Jaccard's dissimilarity between sites in insect community  
  # vs.
  # Altitude differences between sites
  # -----------------------------------------------------------------------------
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
  
  formula_jacc_insect_by_altit <- formula(jaccard_dist_insects ~ alt_dif)
  
  print({ 
    plot(formula_jacc_insect_by_altit,
         xlab = "Altitude differences between sites (m)")
    abline(lm(formula_jacc_insect_by_altit))
    # plot lm summary text as graph
    gplots::textplot(object = capture.output(summary(lm(formula_jacc_insect_by_altit))),
                     cex = 0.4)
  })
  
  # -----------------------------------------------------------------------------
  # Jaccard's dissimilarity in plant community between sites (site's jaccard's index from plant abundances)
  # vs.
  # Altitude differences between sites
  # -----------------------------------------------------------------------------
  # Create location-by-plant-species matrix
  commat_loc5_plants_mat <- table( insects_dt[,.(loc_5, plant_sp)] )
  commat_loc5_plants_df <- as.data.frame.matrix(commat_loc5_plants_mat)
  
  # Jaccard's index from plant abundances per site matrix (above)
  jaccard_dist_plants <- vegan::vegdist(commat_loc5_plants_df, method = "jaccard", binary = TRUE)
  
  formula_jacc_plants_by_altit <- formula(jaccard_dist_plants ~ alt_dif)
  
  print({ 
    plot(formula_jacc_plants_by_altit,
         xlab = "Altitude differences between sites (m)")
    abline(lm(formula_jacc_plants_by_altit))
    # plot lm summary text as graph
    gplots::textplot(object = capture.output(summary(lm(formula_jacc_plants_by_altit))),
                     cex = 0.4) 
  })
  
  # -----------------------------------------------------------------------------
  # Jaccard's dissimilarity between sites in insect community  
  # vs.
  # Jaccard's dissimilarity in plant community between sites (site's jaccard's index from plant abundances)
  # -----------------------------------------------------------------------------
  formula_jacc_insect_by_jacc_plants <- formula(jaccard_dist_insects ~ jaccard_dist_plants)

  print({ 
    plot(jaccard_dist_insects ~ jaccard_dist_plants)
    abline(lm(jaccard_dist_insects ~ jaccard_dist_plants))
    # plot lm summary text as graph
    gplots::textplot(object = capture.output(summary(lm(jaccard_dist_insects ~ jaccard_dist_plants))),
                     cex = 0.4) 
  })
  
  # -----------------------------------------------------------------------------
  # Close the graphics device
  # -----------------------------------------------------------------------------
  dev.off()
}

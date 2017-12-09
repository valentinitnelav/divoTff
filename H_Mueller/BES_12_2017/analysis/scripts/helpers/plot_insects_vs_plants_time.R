###############################################################################
## Function to run Exploratory graphs for time analysis
###############################################################################

plot_insects_vs_plants_time <- function(insects_dt,
                                        nmds_results,
                                        path)
{
  # -----------------------------------------------------------------------------
  # Open PDF device = plot all graphs in one PDF file
  # -----------------------------------------------------------------------------
  pdf(file = path,
      width = 15/2.54, height = 12/2.54, 
      family = "Times", pointsize = 14)
  
  # -----------------------------------------------------------------------------
  # Jaccard's dissimilarity between years in insect community  
  # vs.
  # Jaccard's dissimilarity in plant community between years (year's jaccard's index from plant abundances)
  # -----------------------------------------------------------------------------
  # Create year-by-plant-species matrix
  commat_year_plants_mat <- table( insects_dt[,.(year, plant_sp)] )
  # for easy visual inspection transform to data.frame object
  commat_year_plants_df <- as.data.frame.matrix(commat_year_plants_mat)
  
  # Jaccard's index from plant abundances per year matrix (above)
  jaccard_dist_plants <- vegan::vegdist(commat_year_plants_df, 
                                        method = "jaccard", 
                                        binary = TRUE)
  
  jaccard_dist_insects <- nmds_results$jaccard_dist
  
  formula_jacc_insect_by_jacc_plants <- formula(jaccard_dist_insects ~ jaccard_dist_plants)
  
  print({ 
    plot(formula_jacc_insect_by_jacc_plants,
         main = "Years: Jaccard distances, insects vs. plants")
    abline(lm(formula_jacc_insect_by_jacc_plants))
    # plot lm summary text as graph
    gplots::textplot(object = capture.output(summary(lm(formula_jacc_insect_by_jacc_plants))),
                     cex = 0.4) 
  })
  
  # -----------------------------------------------------------------------------
  # Close the graphics device
  # -----------------------------------------------------------------------------
  dev.off()
}
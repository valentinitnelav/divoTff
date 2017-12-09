###############################################################################
## Function to prepare nmds points for plotting with helper ggplot function
## see also plot_nmds_space.R
###############################################################################

prepare_nmds_coords <- function(insects_dt,
                                nmds_pts,
                                altitude_breaks = c(-Inf, 1500, 2000, 2500, Inf))
{
  # Aggregate altitude
  aggreg_altitude <- 
    insects_dt[, 
               .(
                 altitude_avg  = mean(altitude, na.rm = TRUE),
                 altitude_avg_round_100 = round(mean(altitude, na.rm = TRUE)/100)*100,
                 altitue_range = paste0(round(range(altitude, na.rm = TRUE)/100)*100, 
                                        collapse = "-")
               ), 
               by = loc_5]
  
  # Create altitue classes
  aggreg_altitude[, altitude_gr := cut(altitude_avg_round_100,
                                       breaks = altitude_breaks,
                                       include.lowest = TRUE, 
                                       right = FALSE,
                                       dig.lab = 4)]
  
  # Merge coordinates with altitude info
  nmds_pts_altit <- merge(x = nmds_pts,
                          y = aggreg_altitude,
                          by.x = "my_varb",
                          by.y = "loc_5",
                          all.x = TRUE, 
                          sort = FALSE)
  
  nmds_pts_altit[, altitude_avg_round_100 := factor(altitude_avg_round_100)]
  
  # prepare return results
  results <- list(nmds_pts_altit, aggreg_altitude)
  names(results) <- c("nmds_pts_altit", "aggreg_altitude")
  return(results)
}
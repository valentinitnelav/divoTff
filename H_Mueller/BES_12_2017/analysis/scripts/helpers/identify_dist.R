###############################################################################
# Helper function to identify points, their values and labels after plotting.
# Working with "dist" objects
###############################################################################

identify_dist <- function(x, y, sep = '--'){
  idx <- identify(x = x, y = y)
  
  labels <- outer(X = attributes(x)$Labels,
                  Y = attributes(x)$Labels, 
                  FUN = paste, 
                  sep = paste0(' ', sep, ' '))
  
  x_vect <- as.vector(x)
  names(x_vect) <- labels[lower.tri(labels)]
  
  y_vect <- as.vector(y)
  names(y_vect) <- labels[lower.tri(labels)]
  
  return(data.frame(idx, 
                    x = x_vect[idx], 
                    y = y_vect[idx]))
}

# Example

# plot(x = alt_dif, y = jaccard_dist_insects,
#      xlab = "Altitude differences between sites (m)")
# identify_dist(x = alt_dif, y = jaccard_dist_insects)
# press ESC key after clicking the points
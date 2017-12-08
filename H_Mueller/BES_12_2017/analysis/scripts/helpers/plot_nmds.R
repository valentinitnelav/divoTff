###############################################################################
## Function to plot nmds results with ggplot
###############################################################################

plot_nmds <- function(nmds_xy,
                      label_varb,
                      fill_varb,
                      # package, dist_idx passed tofacet_wrap()
                      package = "package",
                      dist_idx = "dist_idx",
                      # pj - PositionJitter object constructed with position_jitter()
                      pj = position_jitter(width = 0.01, height = 0.01),
                      expand_x = c(0.5, 0), # passed to scale_x_continuous()
                      expand_y = c(0.5, 0), # passed to scale_y_continuous()
                      extra_features = NULL)
{
  set.seed(66)
  nmds_plot <- 
    ggplot(data = nmds_xy, 
           aes(x = MDS1, 
               y = MDS2)) +
    # pass string variable to aes
    # https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot
    geom_point(aes_string(fill = fill_varb),
               size = 2, 
               pch  = 21,
               position = pj,
               show.legend = FALSE) +
    # geom_text(aes_string(label = loc5),
    #           vjust = 1,
    #           size = 1,
    #           position = pj) +
    geom_label_repel(aes_string(label = label_varb,
                                fill  = fill_varb), # altitude_gr or altitude_avg_round_100
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
    # pass string variable to facet_wrap
    # https://stackoverflow.com/questions/21588096/pass-string-to-facet-grid-ggplot2
    facet_wrap(reformulate(package, dist_idx), 
               scales = "free", 
               labeller = label_both) +
    extra_features
  
  return(nmds_plot)
}
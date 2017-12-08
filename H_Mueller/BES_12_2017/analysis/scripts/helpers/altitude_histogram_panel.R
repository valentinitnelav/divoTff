###############################################################################
## Function to create altitude histogram multi panel plot
###############################################################################

altitude_histogram_panel <- function(data, 
                                     varb, 
                                     xintercept = 2500,
                                     wrap_varb, 
                                     extra_features = NULL)
{
  my_histos <- 
    ggplot(data = data,
           # pass string variable to aes
           # https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot
           aes_string(varb)) +
    geom_histogram() +
    geom_vline(xintercept = xintercept, 
               color = "red",
               linetype = "dashed") + 
    # pass string variable to facet_wrap
    # https://stackoverflow.com/questions/21588096/pass-string-to-facet-grid-ggplot2
    facet_wrap(reformulate(wrap_varb)) +
    theme_bw() +
    # edit strip text for each panel
    theme(strip.text = element_text(size = 8, 
                                    face = "bold")) +
    extra_features
  return(my_histos)
}
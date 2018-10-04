# /////////////////////////////////////////////////////////////////////////
#
# Script for Fig 1 - population growth rate (lambda) of a common and rare plant
# species (Lespedeza capitata and Lespedeza leptostachya).
#
# /////////////////////////////////////////////////////////////////////////

rm(list = ls(all.names = TRUE)); gc(reset = TRUE)

library(ggplot2)
library(readxl)


# Read & process data -----------------------------------------------------

# Read raw data and convert to data frame – works easier with the gsub() function
dt_raw <- as.data.frame(read_excel(path  = "data/lambda_both species.xlsx", 
                                   sheet = 1))
# Replace spaces with _ in first column
dt_raw[,1] <- gsub(pattern = "\\s", replacement = "_", x = dt_raw[,1])

# Bring data in the format required by ggplot2

dt_les_cap <- dt_raw[c(1,9,10),]
dt_les_lep <- dt_raw[c(2,5,6),]
mat_t <- rbind(t(dt_les_cap[,-1]), 
               t(dt_les_lep[,-1]))
colnames(mat_t) <- c("means", "lower_ci", "upper_ci")

dt <- data.frame(treatment = colnames(dt_les_cap[,-1]),
                 species = rep(c("les_cap", "les_lep"), each = 4))
dt <- cbind(dt, mat_t) # ignore the warning


# Barplot -----------------------------------------------------------------

# The error-bars will overlapped, so use position_dodge to dodge them horizontally
pd <- position_dodge(width = 0.5) # dodge them .05 to the left and right

lespedeza_barplot <- 
    ggplot(data = dt, 
           aes(x = treatment, 
               y = means, 
               fill = species)) +
    # add horizontal line at lambda 1
    geom_hline(yintercept = 1,
               size = 0.25,
               linetype = "longdash",
               color = "gray50") + 
    # add the bars (means)
    geom_bar(stat = "identity", 
             position = position_dodge()) +
    # set type of fill
    scale_fill_manual(name   = 'Species',
                      breaks = c("les_cap", 
                                 "les_lep"),
                      values = c("les_cap" = "gray70", 
                                 "les_lep" = "gray40"),
                      labels = c("L. capitata", 
                                 "L. leptostachya")) + 
    # plot CIs
    geom_errorbar(aes(ymax = upper_ci, 
                      ymin = lower_ci),
                  size = .2,
                  width = .1,
                  linetype = "solid",
                  position = position_dodge(.9)) +
    # set order of discrete values on OX axes
    scale_x_discrete(limits = c("no fire, no spray", 
                                "no fire, spray", 
                                "fire, no spray", 
                                "fire, spray"),
                     labels = c("control", 
                                "simulated\ngrazing", 
                                "fire", 
                                "fire and\nsimulated grazing")) +
    # adjust the distance from OX axes
    scale_y_continuous(limits = c(0, 2.4), 
                       expand = c(0, 0)) +
    
    # Final adjustments:
    # set axis labels
    labs(x = "", 
         y = "Population growth rate (lambda)") +
    theme_bw() + # eliminate default background 
    theme(
        panel.grid = element_blank(), # eliminate grids
        # adjust text in X-axis title
        axis.title.x = element_text(size = 7, face = "bold"),
        # adjust text in X-axis tick labels
        axis.text.x = element_text(size = 7, face = "bold", color = "black"),
        # adjust text in Y-axis title
        axis.title.y = element_text(size = 7, face = "bold"),
        # adjust text in Y-axis tick labels
        axis.text.y = element_text(size = 7, color = "black"),
        # adjust legend title appearance
        legend.title = element_text(size = 7, face = "bold"),
        # adjust legend label appearance
        legend.text = element_text(size = 7, face = "italic"),
        # change spacing between legend items
        legend.key.height = unit(5, "mm"),
        # don't draw legend box (check element_rect() for borders and backgrounds)
        legend.background = element_blank(),
        # Put upper-left corner of legend box in upper-left corner of graph
        # Note that the numeric position in legend.position below is relative to the entire area, 
        # including titles and labels, not just the plotting area
        legend.justification = c(0,1),
        legend.position = c(0,1),
        plot.margin = unit(c(t=0.1, r=0.75, b=-3.5, l=0.2), unit = "mm") # adjust margins
    )


# save as pdf
ggsave(filename = "output/fig-1-Lespedeza-lambda-both-species-barplot.pdf",
       device = "pdf",
       plot = lespedeza_barplot,
       width = 8, 
       height = 6, 
       units = "cm")

# save as eps
ggsave(filename = "output/fig-1-Lespedeza-lambda-both-species-barplot.eps",
       device = "eps",
       plot = lespedeza_barplot,
       width = 8, 
       height = 6, 
       units = "cm")

# save as tif
ggsave(filename = "output/fig-1-Lespedeza-lambda-both-species-barplot.tiff",
       device = "tiff",
       plot = lespedeza_barplot,
       width = 8, 
       height = 6, 
       units = "cm",
       dpi = 600)

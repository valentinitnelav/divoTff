# /////////////////////////////////////////////////////////////////////////
# Script for Fig 1 - population growth rate (lambda) of a common and rare plant
# species (Lespedeza capitata and Lespedeza leptostachya).
# /////////////////////////////////////////////////////////////////////////

library(ggplot2)

# read data
DF <- read.csv("data/archive/2017.01.17/Lespediza - lambda_both species - dataVS2.csv")
str(DF)

# ======================================
# ========== Plot (dotplot) ========== #
# ======================================
# The errorbars will overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(width = 0.5) # move them .05 to the left and right

ggplot(data=DF, aes(x = Treatment, 
                    y = means, 
                    group = Species)) +
    
    # add the points (means)
    geom_point(aes(shape=Species), size=1, position=pd) +
    # set type of point shape, see more at : http://sape.inf.usi.ch/quick-reference/ggplot2/shape
    scale_shape_manual(name   = 'Species',
                       breaks = c("les.cap", "les.lep"),
                       values = c("les.cap" = 17, 
                                  "les.lep" = 19),
                       labels = c("L. capitata", "L. leptostachya")) + 
    
    # plot CIs
    geom_errorbar(aes(ymax=upper.CI, ymin=lower.CI), size=.2, width=.1, linetype="solid", position=pd) +
    
    # set order of discrete values on OX axes and adjust the gap between the marginal points and the OY axes
    scale_x_discrete(limits = c("control", "spray", "fire", "spray and fire"),
                     labels = c("control", "spray", "fire", "spray \n and fire"),
                     expand = c(0, 0.3)) +
    scale_y_continuous(limits = c(0, 2.3)) +
    
    # Final adjustments:
    # set axis labels
    labs(x = "", 
         y = "Means") +
    theme_bw() + # eliminate default background 
    theme(panel.grid.major = element_blank(), # eliminate major grids
          panel.grid.minor = element_blank(), # eliminate minor grids
          # set font family for all text within the plot ("serif" should work as "Times New Roman")
          # note that this can be overridden with other adjustment functions below
          text = element_text(family="serif"),
          # adjust text in X-axis label
          axis.title.x = element_text(size = 10, face = "bold"),
          # adjust X-axis ticks
          axis.text.x = element_text(size = 10, face = "bold", color="black"),
          # adjust text in Y-axis label
          axis.title.y = element_text(size = 10, face = "bold"),
          # adjust legend title appearance
          legend.title = element_text(size = 8, face = "bold"),
          # adjust legend label appearance
          legend.text = element_text(size = 8, face = "italic"),
          # change spacing between legend items
          legend.key.height = unit(3, "mm"),
          # don't draw legend box (check element_rect() for borders and backgrounds)
          legend.background = element_blank(),
          # Put upper-left corner of legend box in upper-left corner of graph
          # Note that the numeric position in legend.position below is relative to the entire area, 
          # including titles and labels, not just the plotting area
          legend.justification = c(0,1),
          legend.position = c(0,1))

# save as pdf
ggsave("output/Lespediza - lambda_both species - dotplot.pdf", width=10, height=7, units="cm")
# save as png
ggsave("output/Lespediza - lambda_both species - dotplot.png", width=10, height=7, units="cm", dpi=600)

# ======================================
# ========== Plot (barplot) ========== #
# ======================================
ggplot(data=DF, aes(x = Treatment, 
                    y = means, 
                    fill = Species)) +
    
    # add the bars (means)
    geom_bar(stat="identity", position=position_dodge()) +
    # set type of fill
    scale_fill_manual(name   = 'Species',
                      breaks = c("les.cap", "les.lep"),
                      values = c("les.cap" = "gray70", 
                                 "les.lep" = "gray40"),
                      labels = c("L. capitata", "L. leptostachya")) + 
    
    # plot CIs
    geom_errorbar(aes(ymax=upper.CI, ymin=lower.CI), size=.2, width=.1, linetype="solid", position=position_dodge(.9)) +
    
    # set order of discrete values on OX axes
    scale_x_discrete(limits = c("control", "spray", "fire", "spray and fire"),
                     labels = c("control", "spray", "fire", "spray \n and fire")) +
    # adjust the distance from OX axes
    scale_y_continuous(limits = c(0, 2.4), expand = c(0, 0)) +
    
    # Final adjustments:
    # set axis labels
    labs(x = "", 
         y = "Means") +
    theme_bw() + # eliminate default background 
    theme(panel.grid.major = element_blank(), # eliminate major grids
          panel.grid.minor = element_blank(), # eliminate minor grids
          # set font family for all text within the plot ("serif" should work as "Times New Roman")
          # note that this can be overridden with other adjustment functions below
          text = element_text(family="serif"),
          # adjust text in X-axis label
          axis.title.x = element_text(size = 10, face = "bold"),
          # adjust X-axis ticks
          axis.text.x = element_text(size = 10, face = "bold", color="black"),
          # adjust text in Y-axis label
          axis.title.y = element_text(size = 10, face = "bold"),
          # adjust legend title appearance
          legend.title = element_text(size = 8, face = "bold"),
          # adjust legend label appearance
          legend.text = element_text(size = 8, face = "italic"),
          # change spacing between legend items
          legend.key.height = unit(5, "mm"),
          # don't draw legend box (check element_rect() for borders and backgrounds)
          legend.background = element_blank(),
          # Put upper-left corner of legend box in upper-left corner of graph
          # Note that the numeric position in legend.position below is relative to the entire area, 
          # including titles and labels, not just the plotting area
          legend.justification = c(0,1),
          legend.position = c(0,1))

# save as pdf
ggsave("output/Lespediza - lambda_both species - barplot.pdf", width=10, height=7, units="cm")
# save as png
ggsave("output/Lespediza - lambda_both species - barplot.png", width=10, height=7, units="cm", dpi=600)

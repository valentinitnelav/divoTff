###############################################################################
## Script for testing how Syrphidae insect species vary in time
## Running NMDS on aggregated Syrphidae data
###############################################################################

library(data.table)
library(ggplot2)
library(writexl)
library(gplots) # for plotting text

# packages for running nMDS:
library(vegan)
library(checkmate)  # was needed by smacof below
library(smacof)


# =============================================================================
# Prepare & plot nMDS results
# =============================================================================
syrphidae <- data.table(read.csv("output/syrphidae/syrphidae_selected_sites_past&present.csv", 
                                 stringsAsFactors = FALSE))

# remove 1879 & 1878
syrphidae <- syrphidae[!(year %in% c(1878, 1879))]


commat_year_insects_mat <- table( syrphidae[,.(year, insect_sp)] )
# for easy visual inspection transform to data.frame object
commat_year_insects_df <- as.data.frame.matrix(commat_year_insects_mat)

jaccard_dist_insects <- vegan::vegdist(commat_year_insects_df, 
                                       method = "jaccard", 
                                       binary = TRUE)

set.seed(2017)
nmds_jaccard_vegan <- vegan::metaMDS(comm = jaccard_dist_insects, k = 2)
nmds_jaccard_vegan # stress is given as proportion from 0 to 1 (from ?metaMDS, section Value)
# Shepard Diagram
stressplot(nmds_jaccard_vegan)

set.seed(2017)
nmds_jaccard_smacof <- smacof::mds(delta = jaccard_dist_insects, type = "ordinal")
nmds_jaccard_smacof # stress-1 is given as proportion from 0 to 1
# Shepard Diagram
plot(nmds_jaccard_smacof, plot.type = "Shepard")
cor(x = nmds_jaccard_smacof$delta, 
    y = nmds_jaccard_smacof$confdist, 
    method = "spearman")
1 - nmds_jaccard_smacof$stress^2 # R^2= 1 - S^2

# Comparing models with Procrustes rotation
plot(smacof::Procrustes(nmds_jaccard_vegan$points, nmds_jaccard_smacof$conf))
# same as above, but rotate axis
plot(smacof::Procrustes(nmds_jaccard_smacof$conf, nmds_jaccard_vegan$points))

# Prepare data for ggplot
nmds_points <- rbind(nmds_jaccard_vegan$points,
                     nmds_jaccard_smacof$conf)

nmds_points <- data.table(nmds_points,
                          id = 1:nrow(commat_year_insects_mat),
                          year = rownames(commat_year_insects_mat),
                          package = rep(c("vegan", "smacof"), each = nrow(commat_year_insects_mat)),
                          dist_idx = "Jaccard")

# -----------------------------------------------------------------------------
# Plot nMDS results
# -----------------------------------------------------------------------------
pj <- position_jitter(width = 0.05, height = 0.0025)

my_plot <- 
  ggplot(data = nmds_points, 
         aes(x = MDS1, 
             y = MDS2)) +
  geom_label(aes(label = year,
                 fill = factor(year)), 
             colour = "black", 
             fontface = "bold",
             position = pj,
             alpha = 0.6) +
  # Adjust the distance (gap) from axis
  # scale_x_continuous(expand = c(0.5, 0)) +
  # scale_y_continuous(expand = c(0.5, 0)) +
  labs(fill = 'Year') +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(package ~ dist_idx, 
             scales = "free", 
             labeller = label_both)

ggsave(filename = "output/syrphidae/syrphidae_year_NMDS_plot.pdf", 
       plot = my_plot, 
       width = 29.7, 
       height = 15, 
       units = "cm")

# =============================================================================
# Exploratory graphs
# =============================================================================
commat_year_plants_mat <- table( syrphidae[,.(year, plant_sp)] )
# for easy visual inspection transform to data.frame object
commat_year_plants_df <- as.data.frame.matrix(commat_year_plants_mat)

jaccard_dist_plants <- vegan::vegdist(commat_year_plants_df, 
                                       method = "jaccard", 
                                       binary = TRUE)

pdf(file = "output/syrphidae/syrphidae_year_Jaccard_insects_vs_plants.pdf",
    width = 15/2.54, height = 12/2.54, 
    family = "Times", pointsize = 14)

plot(jaccard_dist_insects ~ jaccard_dist_plants,
     main = "Years: Jaccard distances, insects vs. plants")
abline(lm(jaccard_dist_insects ~ jaccard_dist_plants))
gplots::textplot(object = capture.output(summary(lm(jaccard_dist_insects ~ jaccard_dist_plants))),
                 cex = 0.4) 

# close the device
dev.off()

# =============================================================================
# Aggregation by loc_5 and year
# =============================================================================
syrphidae_loc5_year_counts <- syrphidae[, .N, by = c("loc_5", "year")]
writexl::write_xlsx(syrphidae_loc5_year_counts, path = "output/syrphidae/syrphidae_loc5_year_counts.xlsx")

# write.csv(syrphidae_loc5_year_counts, 
#           file = "output/syrphidae/syrphidae_loc5_year_counts.csv", 
#           row.names= FALSE)
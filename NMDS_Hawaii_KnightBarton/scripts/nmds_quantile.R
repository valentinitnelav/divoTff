# /////////////////////////////////////////////////////////////////////////
# Script to compute nMDS coordinates on dbh-quantile-labeled sites
# /////////////////////////////////////////////////////////////////////////

library(data.table)
library(vegan)
library(smacof)
library(MASS)
library(ggplot2)


# Read & prepare data -----------------------------------------------------

dt_raw <- fread("data/KnightBarton_allstems3_KnightBarton_flat(6).csv")

# subset data
dt <- dt_raw[lifeform == "tree" & !is.na(dbh), .(island, site, species, dbh)]

# Site names need to be trnasofrmed to lowercase so that ordering/sorting happens correctly
dt[, site := tolower(site)]
setorder(dt, site, dbh)

# Create site ID-s
dt[, site_id := .GRP, by = site]
# Make site ids of character type, e.g. so that 1 is 01 (this preserved order down the road)
dt[, site_id_ch := ifelse(site_id <= 9, 
                          yes = paste0("0", site_id),
                          no = paste0(site_id))]
# Compute DBH 75% quantile for each site
dt[, qunatile_75 := quantile(dbh, probs = 0.75), by = site]
# Label sites in reference to the quantile
dt[, site_q := ifelse(dbh >= qunatile_75,
                      yes = paste0(site_id_ch, "-top"),
                      no = paste0(site_id_ch, "-low"))]

# Save intermediary results
write.csv(dt, "output/processed_data.csv", row.names = FALSE)
dt_site_unq <- unique(dt[,.(site_q, site)], by = c("site_q", "site"))
write.csv(dt_site_unq, "output/site_IDs.csv", row.names = FALSE)


# Run nMDS ----------------------------------------------------------------

# Create community-by-species matrix
com_mat <- dt[, table(site_q, species)]
com_mat <- as.data.frame.matrix(com_mat)

# Create Bray-Curtis index (distance) matrix
bray_dist <- vegan::vegdist(com_mat, method = "bray")


# Run nMDS
set.seed(2018)
nmds_bray_vegan <- vegan::metaMDS(comm = bray_dist, k = 2)

set.seed(2018)
nmds_bray_smacof <- smacof::mds(delta = bray_dist, ndim = 2, type = "ordinal")

set.seed(2018)
nmds_bray_MASS <- MASS::isoMDS(d = bray_dist, k = 2)

# Optional
# Comparing ordinations between vegan and MASS models: Procrustes rotation
# suggested by Jari Oksanen in 
# Oksanen - Multivariate Analysis of Ecological Communities in R vegan tutorial.pdf
pro <- vegan::procrustes(nmds_bray_vegan, nmds_bray_MASS)
pro
plot(pro)
plot(pro, kind = 2)


# Plot results ------------------------------------------------------------

# Prepare data for plotting in ggplot
points <- rbind(nmds_bray_vegan$points, 
                nmds_bray_smacof$conf, 
                nmds_bray_MASS$points)
points_dt <- data.table(site_q = rep(rownames(com_mat), times = 3),
                        MDS_1 = points[, 1],
                        MDS_2 = points[, 2],
                        package = rep(c("vegan", "smacof", "MASS"), 
                                      each = nrow(com_mat)))

# Join with processed data and pull island and site id (as character) for each site id
points_dt[dt, on = .(site_q), ':=' (island = island,
                                    site_id_ch = site_id_ch)]

# Make plot
nmds_plot <-
  ggplot(data = points_dt, 
         aes(x = MDS_1, 
             y = MDS_2)) +
  # Connect with line pairs of sites
  geom_line(aes(group = site_id_ch),
            color = "gray70",
            size = 0.1) +
  geom_point(aes(shape = island, 
                 colour = island),
             alpha = 0.7,
             size = 3) +
  geom_text(aes(label = site_q), 
            vjust = 1,
            size = 2.5) +
  facet_wrap(~ package, 
             scales = "free") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Save plot to PDF
ggsave(filename = "output/nMDS_plots_2018_05_23.pdf",
       plot = nmds_plot,
       width = 50,
       height = 20,
       units = "cm")

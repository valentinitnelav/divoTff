###############################################################################
## Function to run NMDS
###############################################################################

run_nmds <- function(commat_df)
{
  # -------------------------------------
  # Compute Jaccard index
  # -------------------------------------
  # "Jaccard index has identical rank order (as Bray-Curtis), but has better metric properties, 
  # and probably should be preferred." (Oksanen, J., 2009)
  jaccard_dist <-  vegan::vegdist(commat_df, 
                                  method = "jaccard", 
                                  binary = TRUE)
  # Note that if not mentioning binary = TRUE in vegdist() then "Jaccard index is computed as 2B/(1+B), 
  # where B is Bray-Curtis dissimilarity" (from ?vegdist {vegan})
  # Nevertheless, the binary argument is not mentioned in the help file of metaMDS() nor of monoMDS(), 
  # which are called from metaMDS() at run time.
  # check the bug/issue here:
  # http://stats.stackexchange.com/questions/242110/nmds-from-jaccard-and-bray-curtis-identical-is-that-a-bad-thing
  # https://github.com/joey711/phyloseq/issues/572
  # IMPORTANT: Note the arguments of the "vegan" authors at:
  # https://github.com/vegandevs/vegan/issues/153
  
  # -------------------------------------
  # Run nMDS with vegan
  # -------------------------------------
  set.seed(2017)
  nmds_jaccard_vegan <- vegan::metaMDS(comm = jaccard_dist, k = 2)
  # stress is given as proportion from 0 to 1 (from ?metaMDS, section Value)
  
  # -------------------------------------
  # Run nMDS with smacof
  # -------------------------------------
  set.seed(2017)
  nmds_jaccard_smacof <- smacof::mds(delta = jaccard_dist, type = "ordinal")
  # stress-1 is given as proportion from 0 to 1
  
  # -------------------------------------
  # NOTE about stress values:
  # -------------------------------------
  # << A large stress value does not necessarily indicate bad fit
  # The lower bound of the stress value is 0 (perfect fitt), the upper bound is nontrivial 
  # (see De Leeuw and Stoop 1984). What is a 'good' stress value then? 
  # Kruskal (1964a) gave some stress-1 benchmarks for ordinal MDS: 
  # 0.20 = poor, 0.10 = fair, 0.05 = good, 0.025 = excellent, and 0.00 = perfect. 
  # As always, such general rules of thumb are problematic since there are
  # many aspects that need to be considered when judging stress 
  # (see Borg, Groenen, and Mair 2012, for details).>> in Mair (2015).
  
  # -------------------------------------
  # Prepare data for ggplot
  # -------------------------------------
  nmds_points <- rbind(nmds_jaccard_vegan$points,
                       nmds_jaccard_smacof$conf)
  
  nmds_points <- data.table(nmds_points,
                            id = 1:nrow(commat_df),
                            my_varb = rownames(commat_df),
                            package = rep(c("vegan", "smacof"), 
                                          each = nrow(commat_df)))
  
  # -------------------------------------
  # Prepare return
  # -------------------------------------
  results <- list(jaccard_dist,
                  nmds_jaccard_vegan,
                  nmds_jaccard_smacof,
                  nmds_points)
  names(results) <- c("jaccard_dist",
                      "nmds_jaccard_vegan",
                      "nmds_jaccard_smacof",
                      "nmds_points")
  return(results)
}

# =============================================================================
# References
# =============================================================================
# Mair, P., de Leeuw, J., & Groenen, P. J. (2015). Multidimensional scaling in R: smacof. Technical report. 
#     URL: https://cran.r-project.org/web/packages/smacof/vignettes/smacof.pdf
#
# Oksanen, J. (2009). Multivariate analysis of ecological communities in R: vegan tutorial. 
#     URL: http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
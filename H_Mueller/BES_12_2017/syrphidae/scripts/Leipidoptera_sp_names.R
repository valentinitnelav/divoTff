###############################################################################
# Script to get unique values of species names for Lepidoptera order.
# Indicates also to which type of dataset it belongs:
# - old (Mueller past) or new (2016, 2017) or both
###############################################################################

library(data.table)

# read data
mueller_all <- read.csv("output/mueller_all.csv", stringsAsFactors = FALSE)
setDT(mueller_all)

# indicate old or new dataset
mueller_all[period == "mueller_past", dataset := "old"]
mueller_all[period %in% c("mueller_2016", "mueller_2017"), dataset := "new"]

# check values
sort(unique(mueller_all$insect_order))

# Process names
lepi <- mueller_all[insect_order == "Lepidoptera"]
lepi_sp <- lepi[, insect_sp, by = dataset]
lepi_sp <- unique(lepi_sp)
setorder(lepi_sp, insect_sp)
lepi_sp <- dcast(lepi_sp, insect_sp ~ dataset)

lepi_sp[, dataset := ifelse(!is.na(new) & !is.na(old), 
                            yes = "both",
                            no  = ifelse(!is.na(new) & is.na(old),
                                         yes = "new",
                                         no  = ifelse(is.na(new) & !is.na(old),
                                                      yes = "old",
                                                      no  = NA)
                                         )
                            )]

lepi_sp[, c("new", "old") := NULL]

sort(unique(lepi_sp$insect_sp))

write.csv(lepi_sp, "output/Lepidoptera_species_names_VS.csv", row.names = FALSE)
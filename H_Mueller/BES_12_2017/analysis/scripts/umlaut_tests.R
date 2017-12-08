### Run some umlaut tests
# this script was part of prepare_data script


# why duplicates in unique results ?? check bergun for axample
loc3_XY <- unique(mueller_all[,.(loc_3, loc_5, x_loc_3, y_loc_3)], by = "loc_3")

loc3_XY[loc_3 == "Bergün"]
all.equal(loc3_XY[28], loc3_XY[73])
identical(loc3_XY[28], loc3_XY[73])
loc3_XY[duplicated(loc3_XY)] # why isn't duplicate detecting them??
loc3_XY[duplicated(loc3_XY, by = "loc_3")]

sort(loc3_XY$loc_3)
sort(loc3_XY$loc_5)
loc3_XY[duplicated(loc3_XY, by = "loc_3")]
sort(unique(loc3_XY$loc_3))
all.equal(loc3_XY$loc_3, loc3_XY$loc_3)
setdiff(loc3_XY$loc_3, loc3_XY$loc_3)

loc3_XY <- loc3_XY[!duplicated(loc3_XY, by = "loc_3")] # does not do the job!

loc3_XY <- loc3_XY[1:72]

loc5_XY <- loc3_XY[, .(x_loc_5 = mean(x_loc_3, na.rm = TRUE),
                       y_loc_5 = mean(y_loc_3, na.rm = TRUE)),
                   by = loc_5]

test1_XY <- unique(mueller_all[,.(loc_5, x_loc_5, y_loc_5)], by ="loc_5")
all.equal(test1_XY, loc5_XY)
setdiff(test1_XY$x_loc_5, loc5_XY$x_loc_5)


############################ fwrite destroies umlauts
# so use combination of write.csv and read.csv

# fwrite(mueller_all, "output/mueller_all.csv")
write.csv(mueller_all, "output/mueller_all.csv", row.names = FALSE)

# encoding issue
mueller_all2 <- fread("output/mueller_all.csv", encoding = "UTF-8")
mueller_all3 <- data.table(read.csv("output/mueller_all.csv", stringsAsFactors = FALSE))
all.equal(mueller_all2, mueller_all)
all.equal(mueller_all2$loc_3, mueller_all$loc_3)
setdiff(mueller_all2$loc_3, mueller_all$loc_3)

all.equal(mueller_all3, mueller_all)

# rm(mueller_past, mueller_2016, mueller_2017)
# gc()
# make custom contrast matrix: copy and paste contrast names from "coef(summary(intmod1))" above; make sure there are the same number of columns in the matrix as there are rows in the model output
# Replace zeros with 1 for the second level in the contrast (digits/columns in matrix are in same order as model output) and use -1 when not comparing to the default level (#1) of the factor
# For more info see this blog: http://mindingthebrain.blogspot.co.uk/2013/04/multiple-pairwise-comparisons-for.html

contrast.matrix <- rbind(
  `PGM:seasfac1 vs. PGM:seasfac2` = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), # No "-1"s here as comparison is with the default level of the factor (seasfac1)
  `PGM:seasfac1 vs. PGM:seasfac3` = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), # ditto
  `PGM:seasfac1 vs. PGM:seasfac4` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), # ditto
  `PGM:seasfac2 vs. PGM:seasfac3` = c(0, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0, 0), #-1 for comparison level that replaces the default
  `PGM:seasfac2 vs. PGM:seasfac4` = c(0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 1, 0),
  `PGM:seasfac3 vs. PGM:seasfac4` = c(0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 1, 0),
  `Stranger:seasfac1 vs. Stranger:seasfac2` = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  `Stranger:seasfac1 vs. Stranger:seasfac3` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), 
  `Stranger:seasfac1 vs. Stranger:seasfac4` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
  `Stranger:seasfac2 vs. Stranger:seasfac3` = c(0, 0, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0), 
  `Stranger:seasfac2 vs. Stranger:seasfac4` = c(0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 1),
  `Stranger:seasfac3 vs. Stranger:seasfac4` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 1),  
  `Neighbour:seasfac1 vs. Neighbour:seasfac2` = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  `Neighbour:seasfac1 vs. Neighbour:seasfac3` = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), 
  `Neighbour:seasfac1 vs. Neighbour:seasfac4` = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), 
  `Neighbour:seasfac2 vs. Neighbour:seasfac3` = c(0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0), 
  `Neighbour:seasfac2 vs. Neighbour:seasfac4` = c(0, 0, 0, -1, 0, 1, 0, 0, 0, 0, 0, 0),
  `Neighbour:seasfac3 vs. Neighbour:seasfac4` = c(0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0))

comps <- glht(intmod1, contrast.matrix)
summary(comps)
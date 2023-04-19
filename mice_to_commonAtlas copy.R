library('dplyr')
library(xlsx)

path_atlas_matcher = '/Users/ali/Desktop/april23/match_human_mice/CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)


load('mice_scca_results332.rda')
dim(connectivitvals_new)

mice_common = connectivitvals_new*0

for (i in 1:dim(connectivitvals_new)[1]) {
  for (j in dim(connectivitvals_new)[2]) {
    condition1 = FALSE
    condition2 = FALSE
    indexi =which(i ==  atlas_matcher$MouseIndex)
    if (i %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexi]) ) {condition1 = TRUE}
    indexj =which(j ==  atlas_matcher$MouseIndex)
    if (j %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexj]) ) {condition2 = TRUE}
    if (condition1 & condition2)  {mice_common[i,j] = connectivitvals_new[i,j];}

  }
}



sum(abs(mice_common))










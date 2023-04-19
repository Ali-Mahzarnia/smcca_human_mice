library('dplyr')
library(xlsx)

path_atlas_matcher = '/Users/ali/Desktop/april23/match_human_mice/CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)


load('human_scca_results84.rda')
dim(connectivitvals_new)

human_cmn = connectivitvals_new*0

for (i in 1:dim(connectivitvals_new)[1]) {
  for (j in dim(connectivitvals_new)[2]) {
    if (i %in% atlas_matcher$HumanAtlasIndex & j %in% atlas_matcher$HumanAtlasIndex) {human_cmn[i,j] = connectivitvals_new[i,j];
    if (human_cmn[i,j] !=0) print(human_cmn[i,j])}
  }
}

sum(human_cmn)





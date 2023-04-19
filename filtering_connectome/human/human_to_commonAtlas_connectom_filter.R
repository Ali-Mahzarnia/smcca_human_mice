library('dplyr')
library(xlsx)

path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)



## plain

conectome_name = "connectivity_plain"
load( paste0(conectome_name , ".rda"))


human_cmn = connectivity*0

for (i in 1:dim(connectivity)[1]) {
  for (j in 1:dim(connectivity)[2]) {
    if (i %in% atlas_matcher$HumanAtlasIndex & j %in% atlas_matcher$HumanAtlasIndex) {human_cmn[i,j,] = connectivity[i,j,] }
    #if (sum(human_cmn[1,2,])==0) 
    #{cat("-",i, "- " ,j, " sum", sum(human_cmn[1,2,]) ,"fin \n")}
    
  }
}
sum(human_cmn)
temp = human_cmn[1,2,]
save(human_cmn, file = paste0("fltrd_",conectome_name , ".rda"))






#sift
rm(list = ls())

path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)

conectome_name = "connectivity_sift"
load( paste0(conectome_name , ".rda"))


human_cmn = connectivity*0

for (i in 1:dim(connectivity)[1]) {
  for (j in 1:dim(connectivity)[2]) {
    if (i %in% atlas_matcher$HumanAtlasIndex & j %in% atlas_matcher$HumanAtlasIndex) {human_cmn[i,j,] = connectivity[i,j,] }
  }
}

save(human_cmn, file = paste0("fltrd_",conectome_name , ".rda"))


#dst
rm(list = ls())

path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)

conectome_name = "connectivity_dst"
load( paste0(conectome_name , ".rda"))


human_cmn = connectivity*0

for (i in 1:dim(connectivity)[1]) {
  for (j in 1:dim(connectivity)[2]) {
    if (i %in% atlas_matcher$HumanAtlasIndex & j %in% atlas_matcher$HumanAtlasIndex) {human_cmn[i,j,] = connectivity[i,j,] }
  }
}

save(human_cmn, file = paste0("fltrd_",conectome_name , ".rda"))



#sft_node
rm(list = ls())

path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)

conectome_name = "connectivity_sift_node"
load( paste0(conectome_name , ".rda"))


human_cmn = connectivity*0

for (i in 1:dim(connectivity)[1]) {
  for (j in 1:dim(connectivity)[2]) {
    if (i %in% atlas_matcher$HumanAtlasIndex & j %in% atlas_matcher$HumanAtlasIndex) {human_cmn[i,j,] = connectivity[i,j,] }
  }
}

save(human_cmn, file = paste0("fltrd_",conectome_name , ".rda"))



#_mean_FA_index
rm(list = ls())

path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)

conectome_name = "connectivity_mean_FA_index"
load( paste0(conectome_name , ".rda"))


human_cmn = connectivity*0

for (i in 1:dim(connectivity)[1]) {
  for (j in 1:dim(connectivity)[2]) {
    if (i %in% atlas_matcher$HumanAtlasIndex & j %in% atlas_matcher$HumanAtlasIndex) {human_cmn[i,j,] = connectivity[i,j,] }
  }
}

save(human_cmn, file = paste0("fltrd_",conectome_name , ".rda"))








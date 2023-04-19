library('dplyr')
library(xlsx)

path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)



## plain

conectome_name = "connectivity_plain"
load( paste0(conectome_name , ".rda"))


mice_common = connectivity*0

for (i in 1:dim(connectivity)[1]) {
  for (j in 1:dim(connectivity)[2]) {
    
    condition1 = FALSE
    condition2 = FALSE
    indexi =which(i ==  atlas_matcher$MouseIndex)
    if (i %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexi]) ) {condition1 = TRUE}
    indexj =which(j ==  atlas_matcher$MouseIndex)
    if (j %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexj]) ) {condition2 = TRUE}
    if (condition1 & condition2)  {mice_common[i,j,] = connectivity[i,j,];}
    
  }
}
sum(mice_common)
temp = mice_common[1,2,]
save(mice_common, file = paste0("fltrd_",conectome_name , ".rda"))




# 
# 
# #sift
# rm(list = ls())
# 
# path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
# atlas_matcher = readxl::read_xlsx(path_atlas_matcher)
# 
# conectome_name = "connectivity_sift"
# load( paste0(conectome_name , ".rda"))
# 
# 
# mice_common = connectivity*0
# 
# for (i in 1:dim(connectivity)[1]) {
#   for (j in 1:dim(connectivity)[2]) {
# 
#     condition1 = FALSE
#     condition2 = FALSE
#     indexi =which(i ==  atlas_matcher$MouseIndex)
#     if (i %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexi]) ) {condition1 = TRUE}
#     indexj =which(j ==  atlas_matcher$MouseIndex)
#     if (j %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexj]) ) {condition2 = TRUE}
#     if (condition1 & condition2)  {mice_common[i,j,] = connectivity[i,j,];}
#     
#     
#       }
# }
# 
# save(mice_common, file = paste0("fltrd_",conectome_name , ".rda"))
# 
# 
# #dst
# rm(list = ls())
# 
# path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
# atlas_matcher = readxl::read_xlsx(path_atlas_matcher)
# 
# conectome_name = "connectivity_dst"
# load( paste0(conectome_name , ".rda"))
# 
# 
# mice_common = connectivity*0
# 
# for (i in 1:dim(connectivity)[1]) {
#   for (j in 1:dim(connectivity)[2]) {
#     
#     condition1 = FALSE
#     condition2 = FALSE
#     indexi =which(i ==  atlas_matcher$MouseIndex)
#     if (i %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexi]) ) {condition1 = TRUE}
#     indexj =which(j ==  atlas_matcher$MouseIndex)
#     if (j %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexj]) ) {condition2 = TRUE}
#     if (condition1 & condition2)  {mice_common[i,j,] = connectivity[i,j,];}
#     
#     
#   }
# }
# 
# save(mice_common, file = paste0("fltrd_",conectome_name , ".rda"))
# 
# 
# #sft_node
# rm(list = ls())
# 
# path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
# atlas_matcher = readxl::read_xlsx(path_atlas_matcher)
# 
# conectome_name = "connectivity_sift_node"
# load( paste0(conectome_name , ".rda"))
# 
# 
# mice_common = connectivity*0
# 
# for (i in 1:dim(connectivity)[1]) {
#   for (j in 1:dim(connectivity)[2]) {
#     
#     condition1 = FALSE
#     condition2 = FALSE
#     indexi =which(i ==  atlas_matcher$MouseIndex)
#     if (i %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexi]) ) {condition1 = TRUE}
#     indexj =which(j ==  atlas_matcher$MouseIndex)
#     if (j %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexj]) ) {condition2 = TRUE}
#     if (condition1 & condition2)  {mice_common[i,j,] = connectivity[i,j,];}
#     
#     
#   }
# }
# 
# save(mice_common, file = paste0("fltrd_",conectome_name , ".rda"))
# 
# #_mean_FA_index
# rm(list = ls())
# 
# path_atlas_matcher = 'CHASSSYMM3AtlasLegends031323_updated.xlsx'
# atlas_matcher = readxl::read_xlsx(path_atlas_matcher)
# 
# conectome_name = "connectivity_mean_FA_index"
# load( paste0(conectome_name , ".rda"))
# 
# 
# mice_common = connectivity*0
# 
# for (i in 1:dim(connectivity)[1]) {
#   for (j in 1:dim(connectivity)[2]) {
#     
#     condition1 = FALSE
#     condition2 = FALSE
#     indexi =which(i ==  atlas_matcher$MouseIndex)
#     if (i %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexi]) ) {condition1 = TRUE}
#     indexj =which(j ==  atlas_matcher$MouseIndex)
#     if (j %in% atlas_matcher$MouseIndex & !is.na(atlas_matcher$HumanAtlasIndex[indexj]) ) {condition2 = TRUE}
#     if (condition1 & condition2)  {mice_common[i,j,] = connectivity[i,j,];}
#     
#     
#   }
# }
# 
# save(mice_common, file = paste0("fltrd_",conectome_name , ".rda"))
# 
# 
# 
# 
# 
# 

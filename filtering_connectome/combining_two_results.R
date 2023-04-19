library('dplyr')
library(xlsx)

path_atlas_matcher = '/Users/ali/Desktop/april23/match_human_mice/CHASSSYMM3AtlasLegends031323_updated.xlsx'
atlas_matcher = readxl::read_xlsx(path_atlas_matcher)




atlas_matcher$common_mc_index = NA
  
for (i in 1:dim(atlas_matcher)[1]) {
  temp= min(which(atlas_matcher$HumanAtlasIndex[i]==atlas_matcher$HumanAtlasIndex ))
  if (length(temp)>0) { atlas_matcher$common_mc_index[i] = atlas_matcher$MouseIndex[temp]   }
}


common_mice_index_human =  sort(unique( na.omit(atlas_matcher$common_mc_index)))
common_human_index =  sort(unique(atlas_matcher$HumanAtlasIndex[!is.na(atlas_matcher$HumanAtlasIndex)]))
# sort(unique(atlas_matcher$MouseIndex[!is.na(atlas_matcher$HumanAtlasIndex)]))

load('human_scca_results84.rda')
dim(connectivitvals_new)
sum(connectivitvals_new)

human_cmn = connectivitvals_new

human_cmn = human_cmn [common_human_index ,common_human_index] 
colnames(human_cmn) = rownames(human_cmn) =common_human_index


dim(human_cmn)
binary_human = human_cmn 
binary_human[binary_human!=0] = 1


load('mice_scca_results332.rda')
dim(connectivitvals_new)
sum(connectivitvals_new)

mice_cmn = connectivitvals_new
mice_cmn = mice_cmn [common_mice_index_human ,common_mice_index_human] 
colnames(mice_cmn) = rownames(mice_cmn) =common_mice_index_human




dim(mice_cmn)
binary_mice =mice_cmn 
binary_mice[binary_mice!=0] = 1

sum(binary_mice)



##matching mice with human results :
binary_mice_rearanged = binary_mice*0
for (i in 1:dim(binary_mice)[1]) {
  for (j in 1:dim(binary_mice)[2]) {
    
    temp_index_i = which( atlas_matcher$MouseIndex == rownames(binary_mice)[i] )
    new_index_i = atlas_matcher$HumanAtlasIndex[temp_index_i]
    
    temp_index_j = which( atlas_matcher$MouseIndex == colnames(binary_mice)[j])
    new_index_j = atlas_matcher$HumanAtlasIndex[temp_index_j]
    
    final_index_i = which(colnames(binary_human) == new_index_i)
    final_index_j = which(colnames(binary_human) == new_index_j)
    
    binary_mice_rearanged[final_index_i, final_index_j  ] = binary_mice[i,j]
    cat(binary_mice_rearanged[29, 18  ] , " " , new_index_i, new_index_j ,"\n")
    
  }
}


sum(sum(binary_mice_rearanged))
sum(sum(binary_mice))

results = binary_human*binary_mice_rearanged
sum(sum(results!=0)) 




image(results, col = heat.colors(100), main = "heat.colors()")


 image(binary_human, col = heat.colors(100), main = "heat.colors()")
 image(binary_mice_rearanged, col = heat.colors(100), main = "heat.colors()")
 
 
 nonzero_index  = which( results!=0 , arr.ind = TRUE  )
  common_result = matrix(NA, dim(nonzero_index)[1], 4)
 
 
 for (i in 1:dim(nonzero_index)[1]) {
   
   Atlas_sheet_Hindex_i = rownames(results)[nonzero_index[i, 1]]
   Atlas_indiciator_i = min(which(Atlas_sheet_Hindex_i == atlas_matcher$HumanAtlasIndex ))
   name_human_i = atlas_matcher$HumanRegion[Atlas_indiciator_i]
   name_mice_i = atlas_matcher$MouseRegion[Atlas_indiciator_i]
   
   Atlas_sheet_Hindex_j = colnames(results)[nonzero_index[i, 2]]
   Atlas_indiciator_j = min(which(Atlas_sheet_Hindex_j == atlas_matcher$HumanAtlasIndex ))
   name_human_j = atlas_matcher$HumanRegion[Atlas_indiciator_j]
   name_mice_j = paste0(atlas_matcher$Hemisphere[Atlas_indiciator_j], " " , atlas_matcher$MouseRegion[Atlas_indiciator_j] )

   #paste0(name_human_i, "--",name_human_j )
   #paste0(name_mice_i, "--",name_mice_j )
   
   common_result[i,1] = paste0(name_human_i, "--",name_human_j )
   common_result[i,2] = paste0(  Atlas_sheet_Hindex_i, "--",Atlas_sheet_Hindex_j   )
   
   common_result[i,3] = paste0(name_mice_i, "--",name_mice_j )
   common_result[i,4] = paste0(  atlas_matcher$MouseIndex[Atlas_indiciator_i] , "--",atlas_matcher$MouseIndex[Atlas_indiciator_j]   )
   
   
 }

  write.csv(common_result , file= "common_connections.csv")
 
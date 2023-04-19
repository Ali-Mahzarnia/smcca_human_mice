library('dplyr')
library(xlsx)


path_atlas_matcher1 = '/Users/ali/Desktop/april23/match_human_mice/CHASSSYMM3AtlasLegends031323.xlsx'
atlas_matcher1 = readxl::read_xlsx(path_atlas_matcher1)


path_atlas_matcher2 = '/Users/ali/Desktop/april23/match_human_mice/Desikan1LUTALex041823.xlsx'
atlas_matcher2 = readxl::read_xlsx(path_atlas_matcher2 , sheet = "GM") %>% select ( HumanIndex, AtlasIndex)
atlas_matcher1$HumanAtlasIndex = atlas_matcher2$AtlasIndex[match(atlas_matcher1$HumanIndex , atlas_matcher2$HumanIndex)]

write.xlsx2(atlas_matcher1, "CHASSSYMM3AtlasLegends031323_updated.xlsx" )




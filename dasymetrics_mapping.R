####We want all of our variables at census block group and beat level

library(sf)
library(tidyverse)
gc()
sd_beats <- st_read("pd_beats_datasd/pd_beats_datasd.shp")

#load in San Diego geographic data
sd_block_groups <- tigris::block_groups(state = "CA", county = "San Diego County")
sd_blocks <- tigris::blocks(state = "CA", county = "San Diego County")

#load in San Diego CITY boundary, clip block groups data to the city 
sd_city_bound <- st_read("https://seshat.datasd.org/sde/city_boundary/san_diego_boundary_datasd.geojson")

st_crs(sd_block_groups) #EPSG 4269 
st_crs(sd_blocks) #EPSG 4269 
st_crs(sd_city_bound) #EPSG 4326
sd_block_groups <- st_transform(sd_block_groups, "EPSG:4326")
sd_blocks <- st_transform(sd_blocks, "EPSG:4326")
#perform the clip, then we can eliminate any Census block groups not in SD City
sd_block_groups <- st_intersection(sd_block_groups, sd_city_bound) 
sd_blocks <- st_intersection(sd_blocks, sd_city_bound) 

ggplot(sd_block_groups) + geom_sf()
ggplot(sd_beats) + geom_sf()

#want to know what overlap looks like of census block groups that are in multiple precincts
st_crs(sd_block_groups) #EPSG 4326 
st_crs(sd_blocks) #4326
st_crs(sd_beats) #EPSG 2230

sd_beats <- st_transform(sd_beats, "EPSG:4326")

sf_use_s2(FALSE)



#only need blocks that have people
sd_blocks <- sd_blocks[sd_blocks$POP20 > 0,]
#need to know area of block so we can know area of block that is split b/w beats
sd_blocks$area_block <- st_area(sd_blocks)

#extract block group number from blocks
sd_blocks$BLkGRPCE <- as.numeric(substr(sd_blocks$BLOCKCE20, 1, 1))

#intersect blocks n beats
blocks_in_beats <- st_intersection(sd_beats, sd_blocks)

#want to know area of intersection
blocks_in_beats$area_intersection <- st_area(blocks_in_beats)

#percent of block in intersection
blocks_in_beats$per_block_intersection <- as.numeric(blocks_in_beats$area_intersection/blocks_in_beats$area_block)

#breakdown of split blocks
hist(blocks_in_beats$per_block_intersection)



#weight population by how much of block is in beat
blocks_in_beats$pop_weighted <- blocks_in_beats$POP20 * blocks_in_beats$per_block_intersection

#make a census block group indicator for blocks
blocks_in_beats$tract_blockgroup <- paste(blocks_in_beats$TRACTCE20, blocks_in_beats$BLkGRPCE, sep = "_")

#make same indicator in block group data
sd_block_groups$tract_blockgroup <- paste(sd_block_groups$TRACTCE, sd_block_groups$BLKGRPCE, sep="_")

#determine the percentage of each block groups population in each beat
blocks_in_beats <- blocks_in_beats %>% group_by(tract_blockgroup) %>%
  mutate(total_blockgroup_pop = sum(POP20,na.rm=T),
         total_weighted_blockgroup_pop = sum(pop_weighted, na.rm=T))

#now determine what percent of each blockgroups weighted population is in each beat
blocks_in_beats <- blocks_in_beats %>% group_by(tract_blockgroup, beat) %>%
  mutate(total_beat_weighted_population = 
           sum(pop_weighted))  #total population from each block group in a beat

#now calculate what percent that population is of a total block groups population
blocks_in_beats$percent_blockgroup_population_inbeat <- blocks_in_beats$pop_weighted/blocks_in_beats$total_weighted_blockgroup_pop

blocks_in_beats <- blocks_in_beats %>% distinct(tract_blockgroup, beat, name, percent_blockgroup_population_inbeat, .keep_all = F)

#select just blockgroups merge variable and geoid
sd_block_groups <- sd_block_groups %>% distinct(tract_blockgroup, GEOID)

blocksgroups_in_beats <- merge(blocks_in_beats, sd_block_groups, by ='tract_blockgroup', all.x = T)

write.csv(blocksgroups_in_beats, "blockgroups_beats_dasymetric.csv", row.names = F)

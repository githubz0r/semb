### script for annotating holes (numCon, numEnds), functions not very generalized but may be useful

library(tidyverse)
library(magrittr)
pancrea_2 <- read.csv('/Users/Deathvoodoo/Documents/semb_int/table_2.csv')
pancrea_2 %<>% mutate(time_point=1:nrow(pancrea_2))
pancrea_2 <- edit(pancrea_2)
write.csv(pancrea_2, file='/Users/Deathvoodoo/Documents/semb_int/table_2.csv', row.names=F) # remember to re-read the file before writing if made changes using emacs

Maketimepoints <- function(hole.nr, time.start, missing.values=NULL){
  missing.values <- sort(missing.values)
  sub.df <- pancrea_2[pancrea_2$hole_num_rep==hole.nr, ]
  n.time.points <- nrow(sub.df)
  time.points <- time.start:(n.time.points+time.start-1)
  if (!is.null(missing.values)){
    for (value in missing.values){
      time.points[time.points>=value] <- time.points[time.points>=value]+1
    }
  }
  return(time.points)
}

nrowhole <- function(hole.nr){
  pancrea_2 %>% filter(hole_num_rep==hole.nr) %>% nrow
}

Assigntimepoints <- function(hole.nr, time.start, missing.values=NULL){
  pancrea_2[pancrea_2$hole_num_rep==hole.nr, ]$time_point <<- Maketimepoints(hole.nr=hole.nr, time.start=time.start, missing.values=missing.values)
  return(NULL)
}

assignvalsforrange <- function(hole.nr, range.begin, range.end, value, type){
  pancrea_2[pancrea_2$hole_num_rep==hole.nr, ][range.begin:range.end, ][type] <<- value
  return(NULL)
}
pancrea_2 %>% filter(data_set_name=='pos1') %>% filter(hole_num_inmovie==6) %>% nrow
pancrea_2[pancrea_2$hole_num_rep==6,]$time_point <- Maketimepoints(6, 30)

#missing_8 <- c(185, 192, 195, 200, 204, 208, 211)
Assigntimepoints(32, 1)
pancrea_2[pancrea_2$hole_num_rep==32, ]$time_point # sanity check


assignvalsforrange(31, 1, 73, 4, 'numCon')
pancrea_2[pancrea_2$hole_num_rep==31, ]

# write a full data matrix similar to original matrix
pancrea_holes_silja <- read.csv('/Users/Deathvoodoo/Documents/semb_int/TableTilLars.csv')
pancrea_holes_silja$numCon <- pancrea_2$numCon
pancrea_holes_silja$numEnds <- pancrea_2$numEnds
pancrea_holes_silja %<>% mutate(time_point=pancrea_2$time_point)
pancrea_names_2 <- c("data_set_name", "hole_num_inmovie", "hole_num_rep", "tissue_type", "dAdt", "dEdt",
                     "numCon", "numEnds", "period", "hole_area_raw", "hole_area_smooth",
                     "eccentricity", "solidity", "time_point", "numEnds_over_numCon")
write.csv(pancrea_holes_silja[pancrea_names_2], file='/Users/Deathvoodoo/Documents/semb_int/table_2_full.csv', row.names=F)

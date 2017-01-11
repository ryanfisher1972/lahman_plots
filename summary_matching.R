library(Lahman)
library(tidyr)
library(dplyr)
library(ggplot2)

data("Teams")
data("Batting")

# The Teams table only contains Hit By Pitch (HBP) and Sacrifice Fly (SF) stats
# from year 2000 onwards.
Teams %>% 
    filter(!is.na(HBP)) %>%  
    `[[`("yearID") %>% 
    unique(.)
Teams %>% 
    filter(!is.na(SF)) %>%  
    `[[`("yearID") %>% 
    unique()

# To obtain 1999 and older data, the player's HBP and SF stats from the Batting
# Table are summarized instead. The following code shows that from year 2000
# onwards, the summarized HBP and SF stats from the Batting Table match the
# stats posted in the Teams table.

df1 <- Batting %>% 
    filter(yearID >= 2000) %>% 
    group_by(yearID, lgID, teamID) %>% 
    summarise(HBP = sum(HBP, na.rm = TRUE),
              SF = sum(SF, na.rm = TRUE)) 

df2 <- Teams %>%
    filter(yearID >= 2000) %>% 
    group_by(yearID, lgID, teamID) %>% 
    select(yearID:teamID, HBP, SF) 

setequal(df1, df2) # TRUE
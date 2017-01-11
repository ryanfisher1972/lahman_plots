library(Lahman)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data from Lahman package
data("Teams")
data("Batting")

# HBP recorded from 1884, SF recorded from 1954
hbp_sf <- Batting %>% 
    group_by(yearID, lgID, teamID) %>% 
    summarise(HBP = sum(HBP, na.rm = TRUE),
              SF = sum(SF, na.rm = TRUE))

teams_batting <- Teams %>% 
    filter(yearID >= 1954) %>% 
    select(yearID:L, R:CS, name) %>%                            
    left_join(hbp_sf, by = c("yearID", "lgID", "teamID")) %>%   # insert tallied HBP and SF
    droplevels() %>%                                            # drop unused levels
    group_by(yearID, lgID, teamID) %>% 
    mutate(BA = H/AB,
           OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
           SLG = (H + X2B + 2*X3B + 3*HR)/AB)

# create a linear model and obtain the R-squared coefficient
# store the R-squared
slg_model <- lm(R ~ SLG, data = teams_batting)
slg_rsq <- substitute(r^2~~"="~~rsq, 
                      list(rsq = format(summary(slg_model)$r.squared, 
                                        digits = 4))) %>% 
    as.expression() %>% 
    as.character()

# plot Runs against On Base Percentage with R-squared coefficient
png("RunsSLG.png", width = 720, height = 720)  
ggplot(teams_batting, aes(x = SLG, y = R)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", col = "green", size = 1.5, se = FALSE) +
    annotate("text", x = quantile(teams_batting$SLG)[1], 
             y = quantile(teams_batting$R)[4], 
             hjust = 0, label = slg_rsq, parse = TRUE) +
    labs(x = "Slugging Percentage", y = "Runs Scored") +
    labs(title = "Each team's Runs Scored against Slugging Percentage every year", 
         subtitle = "Data from 1954 to 2015 (Regular Season only)", 
         x = "Slugging Percentage", y = "Runs Scored") +
    theme_bw()
dev.off()
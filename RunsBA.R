library(Lahman)
library(tidyr)
library(dplyr)
library(ggplot2)

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
ba_model <- lm(R ~ BA, data = teams_batting)
ba_rsq <- substitute(r^2~~"="~~rsq, 
                      list(rsq = format(summary(ba_model)$r.squared, digits = 4))) %>% 
    as.expression() %>% 
    as.character()

# plot Runs against Batting Average with R-squared coefficient
png("RunsBA.png", width = 720, height = 720)    
ggplot(teams_batting, aes(x = BA, y = R)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", size = 1.5, se = FALSE) +
    scale_x_continuous(expand = c(0.03,0)) +
    annotate("text", x = quantile(teams_batting$BA)[1], 
              y = quantile(teams_batting$R)[4], 
              hjust = 0, label = ba_rsq, parse = TRUE) +
    labs(title = "Each team's Runs Scored against Batting Average every year", 
         subtitle = "Data from 1954 to 2015 (Regular Season Only)", 
         x = "Batting Average", y = "Runs Scored") +
    theme_bw()
dev.off()

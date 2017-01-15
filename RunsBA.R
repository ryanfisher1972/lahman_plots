library(Lahman)
library(dplyr)
library(ggplot2)

data("Teams")
data("Batting")

# HBP recorded from 1884, SF recorded from 1954
hbp_sf <- Batting %>% 
    group_by(yearID, lgID, teamID) %>% 
    summarise(HBP = sum(HBP, na.rm = TRUE),
              SF = sum(SF, na.rm = TRUE))

# Only include team batting data from 1954 onwards
teams_0 <- Teams %>% 
    filter(yearID >= 1954) %>%
    select(yearID:L, R:CS, name) %>%                           
    left_join(hbp_sf, by = c("yearID", "lgID", "teamID")) %>%   # insert tallied HBP and SF
    droplevels() %>%                                            # drop unused factor levels
    group_by(yearID, lgID, teamID) %>% 
    # calculate triple slash stats (Batting Average, On-Base Percentage & Slugging Percentage)
    mutate(BA = H/AB,
           OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
           SLG = (H + X2B + 2*X3B + 3*HR)/AB)

# Exclude strike-shortened seasons (usually less than 150 games)
teams_batting <- subset(teams_0, G >= 153)
teams_shortened <- subset(teams_0, G < 153) # strike-shortened seasons 1981, 1994, 1995

# create a linear model and obtain the R-squared coefficient
# store the R-squared coefficient
ba_model <- lm(R ~ BA, data = teams_batting)
ba_rsq <- substitute(r^2~~"="~~rsq, 
                      list(rsq = format(summary(ba_model)$r.squared, 
                                        digits = 4))) %>% 
    as.expression() %>% 
    as.character()

# plot Runs against Batting Average with R-squared coefficient
png("RunsBA.png", width = 720, height = 720)
ggplot(teams_batting, aes(x = BA, y = R)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", size = 1.5, se = FALSE) +
    geom_point(data = teams_shortened, color = "red", alpha = 0.5) +        # strike-shortened season data
    scale_x_continuous(expand = c(0.03,0), breaks = seq(0.22,0.30,0.01)) +
    annotate("text", x = quantile(teams_batting$BA)[1], 
              y = quantile(teams_batting$R)[4], 
              hjust = 0, label = ba_rsq, parse = TRUE) +
    labs(title = "Each team's Runs Scored against Batting Average every year", 
         subtitle = "Data from 1954 to 2015 (Regular Season Only);  Strike-shortened seasonal data (red) excluded from calculations", 
         x = "Batting Average", y = "Runs Scored") +
    theme_bw()
dev.off()

# separate yearID by decade, and give appropriate labels
teams_batting$decade <- cut(teams_batting$yearID, breaks = seq(1950, 2020, by = 10))
levels(teams_batting$decade) <- c("1950s", "1960s", "1970s", "1980s", 
                                  "1990s", "2000s", "2010s")

# plot Runs against BA per decade (faceted)
png("RunsBA_decade.png", width = 720, height = 960)
ggplot(teams_batting, aes(x = BA, y = R, color = decade)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", size = 1, se = FALSE) +
    scale_x_continuous(expand = c(0.03,0), breaks = seq(0.22,0.30,0.01)) +
    facet_wrap(~ decade, ncol = 2, dir = "v") +
    labs(title = "Each team's Runs Scored against Batting Average every year", 
         subtitle = "Data from 1954 to 2015 (Regular Season Only)", 
         x = "Batting Average", y = "Runs Scored") +
    theme_bw() +
    theme(legend.position = "none")
dev.off()

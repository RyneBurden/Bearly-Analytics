if (!require("tidyverse")) {
  install.packages("tidyverse", repos = "http://cran.us.r-project.org", verbose = FALSE)
  library("tidyverse", verbose = FALSE)
}

if (!require("nflfastR")) {
  install.packages("nflfastR", repos = "http://cran.us.r-project.org", verbose = FALSE)
  library("nflfastR", verbose = FALSE)
}

if (!require("glue")) {
  install.packages("glue", repos = "http://cran.us.r-project.org", verbose = FALSE)
  library("glue", verbose = FALSE)
}

if (!require("ggrepel")) {
  install.packages("ggrepel", repos = "http://cran.us.r-project.org", verbose = FALSE)
  library("ggrepel", verbose = FALSE)
}

if (!require("magrittr")) {
  install.packages("magrittr", repos = "http://cran.us.r-project.org", verbose = FALSE)
  library("magrittr", verbose = FALSE)
}

# Usage: Rscript Gather_Graph_OPE.R <season> <week>

args <- commandArgs(trailingOnly = TRUE)
current_season <- as.integer(args[1])
current_week <- as.integer(args[2])

current_season_pbp <- nflfastR::load_pbp(current_season)

current_season_pbp <- mutate(current_season_pbp, win = if_else(home_score > away_score, if_else(posteam == home_team, TRUE, FALSE), if_else(posteam == away_team, TRUE, FALSE)))

current_ope <- current_season_pbp %>% 
  filter(season==current_season, week == current_week, pass == 1 & qb_hit == 0, interception == 0 & fumble == 0, !is.na(cpoe) & !is.na(qb_epa)) %>% 
  group_by(passer_player_id, passer_player_name) %>% 
  summarise(
    team = last(posteam), 
    Dropbacks = n(), 
    Completed = sum(success), 
    CPP = Completed / Dropbacks, 
    CPOE_Store = mean(cpoe),
    OPA = mean(qb_epa),
    AYA = mean(air_yards),
    TeamWin = last(win)
  ) %>% 
  ungroup() %>% 
  filter(Dropbacks >= 12) %>%
  left_join(nflfastR::teams_colors_logos, by = c('team' = 'team_abbr'))

# Make graph
OPE_Plot <- current_ope %>%
  ggplot(aes(x = OPA, y = CPOE_Store)) +
  geom_hline(yintercept = mean(current_ope$CPOE_Store), color = "red", linetype = "dashed", alpha=0.7) +
  geom_vline(xintercept =  mean(current_ope$OPA), color = "red", linetype = "dashed", alpha=0.7) +
  geom_point(color = if_else(current_ope$team != "LV", current_ope$team_color, current_ope$team_color2), cex = 5, alpha = if_else(current_ope$TeamWin == TRUE, .8, .2)) +
  #geom_point(color = current_ope$team_color, cex = 7, alpha = .8) +
  geom_text_repel(aes(label=passer_player_name), max.overlaps = Inf) +
  stat_smooth(geom='line', alpha=0.7, se=FALSE, method='lm')+
  labs(x = "Optimistic Passing QB_EPA Per Play (Turnovers and QB hits excluded)",
    y = "Completion Percentage Over Expected (CPOE)",
    title = glue("Optimistic Passing Efficiency ", current_season, " Week ", current_week, " (Minimum 12 attempts)"),
    caption = "Author: @BearlyAnalytics | Data: @nflfastR") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
  
ggsave(as.character(glue("S:\\Projects\\Bearly Analytics\\Graphics\\Optimistic Passing Efficiency\\", current_season, "\\OPE_", current_season, "_Week_", current_week, ".png")), OPE_Plot, device = "png", dpi = "retina", height = 6.75, width = 12)
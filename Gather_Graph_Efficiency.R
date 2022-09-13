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

if (!require("ggimage")) {
  install.packages("ggimage", repos = "http://cran.us.r-project.org", verbose = FALSE)
  library("ggimage", verbose = FALSE)
}

if (!require("ggrepel")) {
  install.packages("ggrepel", repos = "http://cran.us.r-project.org", verbose = FALSE)
  library("ggrepel", verbose = FALSE)
}

if (!require("magrittr")) {
  install.packages("magrittr", repos = "http://cran.us.r-project.org", verbose = FALSE)
  library("magrittr", verbose = FALSE)
}

# ----- OFFENSE STAT GATHER ----- #
args <- commandArgs(trailingOnly = TRUE)
current_season <-args[1]
current_week <- args[2]
# save_score_plot <- readline("Do you want to save the team efficiency plot for the week? ")
# save_competition_plot <- readline("Do you want to save the competition plot for the week? ")
save_score_plot <- "yes"
save_competition_plot <- "yes"

current_pbp <- nflfastR::load_pbp(current_season)

current_offense_numbers <- current_pbp %>% 
  filter(season==current_season, week < current_week, play_type == "run" | play_type == "pass", !is.na(epa)) %>%
  group_by(posteam) %>%
  summarise(
    Mean_EPA = mean(epa),
    Total_YPP = mean(yards_gained)
  )

# Next gather passing stats for each team
current_offense_numbers <- current_pbp %>%
  filter(season==current_season, week < current_week, play_type == "pass", !is.na(epa), !is.na(air_yards)) %>%
  group_by(posteam) %>%
  summarise(
    Pass_EPA = mean(epa),
    AAY = mean(air_yards)
  ) %>%
  cbind(current_offense_numbers)

# Finally gather all rushing stats for each offense
current_offense_numbers <- current_pbp %>%
  filter(season==current_season, week < current_week, play_type == "run", !is.na(epa)) %>%
  group_by(posteam) %>%
  summarise(
    Rush_EPA = mean(epa),
    YPC = mean(yards_gained)
  ) %>%
  cbind(current_offense_numbers)

#current_offense_numberss <- left_join(current_offense_numberss, teams_colors_logos, by = c('posteam' = 'team_abbr'))

# ----- DEFENSE STAT GATHER ----- #

current_defense_numbers <- current_pbp %>% 
  filter(season==current_season, week < current_week, play_type == "run" | play_type == "pass", !is.na(epa)) %>%
  group_by(defteam) %>%
  summarise(
    Mean_EPA = mean(epa),
    Total_YPP = mean(yards_gained)
  )

# Next gather passing stats for each team
current_defense_numbers <- current_pbp %>%
  filter(season==current_season, week < current_week, play_type == "pass", !is.na(epa), !is.na(air_yards)) %>%
  group_by(defteam) %>%
  summarise(
    Pass_EPA = mean(epa),
    AAY = mean(air_yards)
  ) %>%
  cbind(current_defense_numbers)

# Finally gather all rushing stats for each offense
current_defense_numbers <- current_pbp %>%
  filter(season==current_season, week < current_week, play_type == "run", !is.na(epa)) %>%
  group_by(defteam) %>%
  summarise(
    Rush_EPA = mean(epa),
    YPC = mean(yards_gained)
  ) %>%
  cbind(current_defense_numbers)
  
total_offense_numbers <- current_offense_numbers[ ,-c(4,7)]
total_defense_numbers <- current_defense_numbers[ ,-c(4,7)]
  
# ----- CALCULATE TEAM SCORE ----- #

Team_Scores = data.frame(matrix(ncol = 2))
#colnames(Team_Scores) <- c("Team", "Team_Score")

for (row in 1:nrow(total_offense_numbers)){
  # Calculate team scores
  def_score <- total_defense_numbers[row, 6]
  off_score <- total_offense_numbers[row, 6]
  team_score <- (off_score - def_score) 
  
  # Assign team
  team <- total_offense_numbers[row, 1]
  
  # Push Team Score
  Team_Scores <- rbind(Team_Scores, c(team, team_score))
  
}

colnames(Team_Scores) <- c("Team", "Team_Score")

# Add team information from teams_colors_logos
Team_Scores <- left_join(Team_Scores, nflfastR::teams_colors_logos, by = c('Team' = 'team_abbr'))

# Convert team score column to dbl
Team_Scores$Team_Score <- as.double(Team_Scores$Team_Score)

#Delete dummy row in Team_Scores
Team_Scores <- Team_Scores[-c(1), ]

# ----- CREATE PLOT FOR TEAM BALANCE SCORES ----- #
Team_Score_Plot <- Team_Scores %>%
  ggplot(aes(x = reorder(Team, Team_Score), y = Team_Score)) +
  #coord_flip() +
  #geom_hline(yintercept = mean(Team_Scores$Team_Score), color = "pink", linetype = "dashed", alpha = 1) +
  geom_bar(fill = if_else(Team_Scores$Team != "NYG" & Team_Scores$Team != "DAL" & Team_Scores$Team != "DEN", Team_Scores$team_color, Team_Scores$team_color2), alpha = .8, stat = "identity") + 
  geom_image(aes(image = team_logo_espn), size = 0.025, asp = 16/9, nudge_x = if_else(Team_Scores$Team_Score > 0, .05, -.05)) +
  labs(
    y = "General Effiency (Offensive EPA/Play - Defensive EPA/Play)",
    x = "Team", 
    title = glue("General Team Efficiency Through Week ", current_week - 1),
    caption = "Author: @BearlyAnalytics | Data: @nflfastR"
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9/16,
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold")
    #plot.margin = margin(.2, .1, .2, .1, "cm")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 16)) +
  scale_x_discrete(breaks = scales::pretty_breaks(n = 16))

# Show working plot
#Team_Score_Plot

#Save plot
# Values for height and width were found using a pixels to inches converter for 1024 x 512
if (save_score_plot == "yes" | save_score_plot == "y") {
  ggsave(glue("Graphics/General Efficiency/", current_season, "/GES_", current_season, "_Week_", current_week, ".png"), Team_Score_Plot, device = "png", dpi = "retina", height = 6.75, width = 12)
}

# ----- GAME COMPETITIVENESS PLOT ----- #

# Filter the NFL schedule for the given week - upcoming week is 9
Week_Games <- NFL_schedule %>% 
  filter(week == current_week, season == current_season) %>%
  group_by(game_id) ### DELETE AT END OF FILE

# Create empty tibble to use in the loop below
current_week_data <- tibble(Game_Num = numeric(), Away_Team = character(), Away_Team_EScore = numeric(), Home_Team = character(), Home_Team_EScore = numeric(), Away_Logo = character(), Home_Logo = character())

# Loop through all games and create new data
# Variables needed: home_team_score, away_team_score, home_team, away_team, team_colors_logos
for (row in 1:nrow(Week_Games)) {
  # Current away team info - uses ESPN logo
  Current_Away <- Week_Games[row, 8] %>% as.character()
  Away_Index <- which(Team_Scores$Team == Current_Away) %>% as.integer()
  Current_Away_Eff <- Team_Scores[Away_Index, 2] %>% as.double()
  Current_Away_Logo <- Team_Scores[Away_Index, 11] %>% as.character()
  Current_Away_Color <- Team_Scores[Away_Index, 6] %>% as.character()
  
  # Current home team info - uses ESPN logo
  Current_Home <- Week_Games[row, 9] %>% as.character()
  Home_Index <- which(Team_Scores$Team == Current_Home) %>% as.integer()
  Current_Home_Eff <- Team_Scores[Home_Index, 2] %>% as.double()
  Current_Home_Logo <- Team_Scores[Home_Index, 11] %>% as.character()
  Current_Home_Color <- Team_Scores[Home_Index, 6] %>% as.character()
  
  # Push Away Team
  current_week_data <- rbind(current_week_data, c("Game_Num" = row, "Away_Team" = Current_Away, "Away_Team_EScore" = Current_Away_Eff , "Home_Team" = Current_Home, "Home_Team_EScore" = Current_Home_Eff, "Away_Logo" = Current_Away_Logo, "Home_Logo" = Current_Home_Logo))
}

colnames(current_week_data) = c("Game_Num", "Away_Team", "Away_Team_EScore", "Home_Team", "Home_Team_EScore", "Away_Logo", "Home_Logo")

#current_week_data$Away_Team_EScore <- as.double(current_week_data$Away_Team_EScore)
#current_week_data$Home_Team_EScore <- as.double(current_week_data$Home_Team_EScore)

# ----- PLOT ----- #
# Make first row in tribble before loop to avoid having the column names change
# xend_list <- tribble (
#   ~Game, ~X_Start, ~X_End, Div, 
#   1, min(current_week_data[1, 3], current_week_data[1, 5]), max(current_week_data[1, 3], current_week_data[1, 5])
# )

xend_list <- data.frame(matrix(ncol = 4, nrow = 1))

# This populates our xend list for each remaining game
for (game in seq_len(nrow(current_week_data))) {
  
  away_team_div <- teams %>% filter(Team == current_week_data[game, 2]) %>% select(Division)
  home_team_div <- teams %>% filter(Team == current_week_data[game, 4]) %>% select(Division)

  if (away_team_div$Division == home_team_div$Division) {
    current_fill <- "Red"
  }
  else {
    current_fill <- "Black"
  }
  
  xend_list <- rbind(xend_list, c("Game_Num" = current_week_data[game, 1], "X_Start" = min(current_week_data[game, 3], current_week_data[game, 5]), "X_End" = max(current_week_data[game, 3], current_week_data[game, 5]), "Fill_Color" = current_fill))
  
}

# Delete the first row since it's NA
xend_list <- xend_list[-c(1),]

colnames(xend_list) <- c("Game_Num", "X_Start", "X_End", "Fill_Color")

# Modify current_week_data
current_week_data$Home_Team_EScore = as.double(current_week_data$Home_Team_EScore)
current_week_data$Away_Team_EScore = as.double(current_week_data$Away_Team_EScore)
current_week_data$Game_Num = as.integer(current_week_data$Game_Num)
current_week_data$Fill_Color = xend_list$Fill_Color

# Modify xend_list
xend_list$Game = as.integer(xend_list$Game)
xend_list$X_Start = as.double(xend_list$X_Start)
xend_list$X_End = as.double(xend_list$X_End)

Competition_Plot <- current_week_data %>%
  ggplot(aes(x = Home_Team_EScore, y = -Game_Num)) + 
  geom_segment(inherit.aes = FALSE, color = current_week_data$Fill_Color, aes(x = xend_list$X_Start, y = -xend_list$Game, xend = xend_list$X_End, yend = -xend_list$Game), size = .75)+
  geom_image(image = current_week_data$Home_Logo, size = .05, asp = 16/9) +
  geom_image(inherit.aes = FALSE, image = current_week_data$Away_Logo, size = .05, asp = 16/9, aes(x = Away_Team_EScore, y = -Game_Num)) +
  geom_vline(xintercept = mean(as.double(Team_Scores$Team_Score)), linetype = "dashed", color = "red", alpha = .6) +
  theme_bw() +
  labs(
    x = (glue(current_season, " General Efficiency Through ", current_week - 1, " Weeks (Offensive EPA/Play - Defensive EPA/Play)")),
    y = element_blank(),
    title = glue("Team Efficiency Gaps By Game NFL ", current_season, " Week ", current_week),
    caption = "Author: @BearlyAnalytics | Data: @nflfastR"
  ) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_discrete(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

if (save_competition_plot == 'yes' | save_competition_plot == 'y') {
  ggsave(glue("S:\\Projects\\Bearly Analytics\\Graphics\\Efficiency Gaps\\", current_season, "\\EG_", current_season, "_Week_", current_week, ".png"), Competition_Plot, device = "png", dpi = "retina", height = 6.75, width = 12)
}
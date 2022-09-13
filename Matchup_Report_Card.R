# Make the ratings based on league averages for offense/defense/kicking/etc
# First-down rate , Off EPA(Rush and Pass), Def EPA(Rush and Pass)

library("tidyverse")
library("nflfastR")
library("magrittr")
library("gt")
library("glue")



# Input variables for data filtering below
current_season <- readline("What season are we look at? ")
current_week <- readline(glue("What week of the ", current_season, " season? "))
current_away <- readline("Who is the away team? ")
current_home <- readline("Who is the home team? ")

current_away <- toupper(current_away)
current_home <- toupper(current_home)
current_pbp <- nflfastR::load_pbp(as.integer(current_season))

season_off_passing_stats <- current_pbp %>% 
  filter(week < current_week, play_type == "pass", !is.na(epa)) %>% 
  group_by(posteam) %>% 
  summarise(
    avg_pass_epa_gained = mean(epa),
    pass_yds_gained = sum(yards_gained),
    yards_per_pass_gained = (pass_yds_gained / n())
  )

season_off_rushing_stats <- current_pbp %>% 
  filter(week < current_week, play_type == "run", !is.na(epa)) %>% 
  group_by(posteam) %>% 
  summarise(
    avg_rush_epa_gained = mean(epa),
    rush_yds_gained = sum(yards_gained),
    yards_per_rush_gained = (rush_yds_gained / n())
  )

off_rush_fdr <- current_pbp %>% filter(week < current_week, !is.na(first_down), play_type == "run") %>% 
  group_by(posteam) %>% 
  summarise (
    avg_off_rush_fdr = sum(first_down) / n()
  )

off_pass_fdr <- current_pbp %>% filter(week < current_week, !is.na(first_down), play_type == "pass") %>% 
  group_by(posteam) %>% 
  summarise (
    avg_off_pass_fdr = sum(first_down) / n()
  )

season_off_data <- cbind(season_off_passing_stats, off_pass_fdr)
season_off_data <- cbind(season_off_data, season_off_rushing_stats)
season_off_data <- cbind(season_off_data, off_rush_fdr)
season_off_data <- season_off_data[,-c(5,7,11)]

season_def_rushing_stats <- current_pbp %>% 
  filter(week < current_week, play_type == "run", !is.na(epa)) %>% 
  group_by(defteam) %>% 
  summarise(
    avg_rush_epa_allowed = mean(epa),
    rush_yds_allowed = sum(yards_gained),
    yards_per_rush_allowed = (rush_yds_allowed) / n()
  )

season_def_passing_stats <- current_pbp %>% 
  filter(week < current_week, play_type == "pass", !is.na(epa)) %>% 
  group_by(defteam) %>% 
  summarise(
    avg_pass_epa_allowed = mean(epa),
    pass_yds_allowed = sum(yards_gained),
    yards_per_pass_allowed = (pass_yds_allowed / n())
  )

def_rush_fdr <- current_pbp %>% filter(week < current_week, !is.na(first_down), play_type == "run") %>% 
  group_by(defteam) %>% 
  summarise (
    avg_def_rush_fdr = sum(first_down) / n()
  )

def_pass_fdr <- current_pbp %>% filter(week < current_week, !is.na(first_down), play_type == "pass") %>% 
  group_by(defteam) %>% 
  summarise (
    avg_def_pass_fdr = sum(first_down) / n()
  )

season_def_data <- cbind(season_def_passing_stats, def_pass_fdr)
season_def_data <- cbind(season_def_data, season_def_rushing_stats)
season_def_data <- cbind(season_def_data, def_rush_fdr)
season_def_data <- season_def_data[,-c(5,7,11)]

season_averages <- cbind(season_off_data, season_def_data)
# season data is each teams stats through the season so far
season_averages <- season_averages[,-c(10)]

season_averages$pass_yds_gained <- season_averages$pass_yds_gained / (as.integer(current_week) - 1)
season_averages$rush_yds_gained <- season_averages$rush_yds_gained / (as.integer(current_week) - 1)
season_averages$pass_yds_allowed <- season_averages$pass_yds_allowed / (as.integer(current_week) - 1)
season_averages$rush_yds_allowed <- season_averages$rush_yds_allowed / (as.integer(current_week) - 1)

# matchup_data is the data for the bears and the team their playing this week
matchup_data_away <- season_averages %>% filter(posteam == current_away)
matchup_data_home <- season_averages %>% filter(posteam == current_home)
matchup_data_league_avg <- season_averages %>% 
  summarise(
    avg_pass_epa_gained = mean(avg_pass_epa_gained),
    pass_yds_gained = mean(pass_yds_gained),
    yards_per_pass_gained = mean(yards_per_pass_gained),
    avg_off_pass_fdr = mean(avg_off_pass_fdr),
    avg_rush_epa_gained = mean(avg_rush_epa_gained),
    rush_yds_gained = mean(rush_yds_gained),
    yards_per_rush_gained = mean(yards_per_rush_gained),
    avg_off_rush_fdr = mean(avg_off_rush_fdr),
    avg_pass_epa_allowed = mean(avg_pass_epa_allowed),
    pass_yds_allowed = mean(pass_yds_allowed),
    yards_per_pass_allowed = mean(yards_per_pass_allowed),
    avg_def_pass_fdr = mean(avg_def_pass_fdr),
    avg_rush_epa_allowed = mean(avg_rush_epa_allowed),
    rush_yds_allowed = mean(rush_yds_allowed),
    yards_per_rush_allowed = mean(yards_per_rush_allowed),
    avg_def_rush_fdr = mean(avg_def_rush_fdr)
  )

matchup_data_away <- as.data.frame(as.table(as.matrix(matchup_data_away[,-c(1)])))
matchup_data_home <- as.data.frame(as.table(as.matrix(matchup_data_home[,-c(1)])))
matchup_data_league_avg <- as.data.frame(as.table(as.matrix(matchup_data_league_avg)))

matchup_data_away <- as.data.frame(matchup_data_away[,-c(1,2)])
matchup_data_home <- as.data.frame(matchup_data_home[,-c(1,2)])
matchup_data_league_avg <- as.data.frame(matchup_data_league_avg[,-c(1,2)])

report_card_data <- data.frame(matrix(ncol = 1, nrow = 16))
report_card_data$stat <- c("Offensive EPA per pass", 
                           "Pass yards gained per game", 
                           "Yards gained per pass", 
                           "Off. Passing 1st Down Rate (%)",
                           "Offensive EPA per rush",
                           "Rushing yards gained per game",
                           "Yards gained per rush",
                           "Off. Rushing 1st Down Rate (%)",
                           "Defensive EPA per pass",
                           "Pass yards allowed per game", 
                           "Yards allowed per pass", 
                           "Def. Passing 1st Down Rate (%)",
                           "Defensive EPA per rush",
                           "Rushing yards allowed per game",
                           "Yards allowed per rush",
                           "Def. Rushing 1st Down Rate (%)")

report_card_data <- report_card_data %>% cbind(matchup_data_away)
report_card_data <- report_card_data %>% cbind(matchup_data_league_avg)
report_card_data <- report_card_data %>% cbind(matchup_data_home)
report_card_data <- report_card_data[,-c(1)]
colnames(report_card_data) <- c("STAT", current_away, "LEAGUE AVG", current_home)

report_card_data[4,2:4] <- report_card_data[4,2:4] * 100
report_card_data[8,2:4] <- report_card_data[8,2:4] * 100
report_card_data[12,2:4] <- report_card_data[12,2:4] * 100
report_card_data[16,2:4] <- report_card_data[16,2:4] * 100

current_away = nflfastR::teams_colors_logos %>% filter(team_abbr == current_away) %>% select(team_name, team_abbr)
current_home = nflfastR::teams_colors_logos %>% filter(team_abbr == current_home) %>% select(team_name, team_abbr)

color_text <- function(gtobj){
  for(i in 1:nrow(report_card_data)){
    if (i <= 8) {
      if (round(report_card_data[i,2], 3) > round(report_card_data[i,4], 3)) {
      gtobj <- gtobj %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_body(columns = 2, rows = i))
      }
      else if (round(report_card_data[i,2], 3) < round(report_card_data[i,4], 3)) {
        gtobj <- gtobj %>%
          tab_style(style = cell_text(weight = "bold"),
                    locations = cells_body(columns = 4, rows = i))
      } 
      else {
        next
      }
    }
    else {
      if (round(report_card_data[i,2], 3) < round(report_card_data[i,4], 3)) {
        gtobj <- gtobj %>%
          tab_style(style = cell_text(weight = "bold"),
                    locations = cells_body(columns = 2, rows = i))
      }
      else if (round(report_card_data[i,2], 3) > round(report_card_data[i,4], 3)) {
        gtobj <- gtobj %>%
          tab_style(style = cell_text(weight = "bold"),
                    locations = cells_body(columns = 4, rows = i))
      }
      else {
        next
      }
    }
  }
  return(gtobj)
}

report_card <- gt(report_card_data) %>%
  tab_header(
    title = md(glue("**Week ", current_week, " Matchup Report Card**")),
    subtitle = glue::glue(current_away$team_name, " @ ", current_home$team_name)
  ) %>% 
  cols_align(
    align = c("center"),
    columns = 2:4
  ) %>% 
  cols_align(
    align = c("left"),
    columns = c("STAT")
  ) %>% 
  tab_source_note(
    source_note = md("**Bold** = team advantage for any given stat<br>Made by: @BearlyAnalytics<br>Data: @nflfastR")
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#efefef")
      #cell_fill(color = "#a5f0cc")
    ),
    locations = cells_body(
      columns = c(2,4),
      rows = everything()
    )
  ) %>% 
  tab_style (
    style = list(
      cell_fill(color = "#fcfcfc")
    ),
    locations = cells_body(
      columns = c(1,3),
      rows = everything()
    )
  ) %>% 
  tab_style (
    style = list(
      cell_borders(
        sides = c("left", "right"),
        color = "#D3D3D3",
        weight = px(2)
      )
    ),
    locations = list(
      cells_body(
        columns = 2
      )
    )
  ) %>% 
  tab_style (
    style = list(
      cell_borders(
        sides = c("left", "right"),
        color = "#D3D3D3",
        weight = px(2)
      )
    ),
    locations = list(
      cells_body(
        columns = 4,
        rows = everything()
      )
    )
  ) %>% 
  tab_style (
    style = list(
      cell_borders(
        sides = c("left", "right"),
        color = "#D3D3D3",
        weight = px(2)
      )
    ),
    locations = list(
      cells_body(
        columns = 1
      )
    )
  ) %>% 
  tab_style (
    style = list(
      cell_fill(color = "#edfcf4")
    ),
    locations = cells_body(
      columns = everything(),
      rows = c(1,3,5,7)
    )
  ) %>% 
  tab_style (
    style = list(
      cell_fill(color = "#dbf9ea")
    ),
    locations = cells_body(
      columns = everything(),
      rows = c(2,4,6,8)
    )
  ) %>% 
  tab_style (
    style = list(
      cell_fill(color = "#fdf2f6")
    ),
    locations = cells_body(
      columns = everything(),
      rows = c(9,11,13,15)
    )
  ) %>% 
  tab_style (
    style = list(
      cell_fill(color = "#fbe5ed")
    ),
    locations = cells_body(
      columns = everything(),
      rows = c(10,12,14,16)
    )
  ) %>% 
  fmt_number(
    columns = c(2,3,4),
    rows = c(2,3,4,6,7,10,11,12,14,15),
    decimals = 1
  ) %>% 
  fmt_number(
    columns = c(2,3,4),
    rows = c(8,16),
    decimals = 2
  ) %>% 
  fmt_number(
    columns = c(2,3,4),
    rows = c(1,5,9,13),
    decimals = 3
  ) %>% 
  cols_width(
    2 ~ px(75),
    4 ~ px(75)
  ) %>% 
  color_text()

report_card %>%
  gtsave(
    glue("2021_Week_", current_week, "_", current_away$team_abbr, "@", current_home$team_abbr, "_matchup_report_card.png"), expand = 10,
    path = glue("S:\\Projects\\Bearly Analytics\\Graphics\\Matchup Report Cards\\", current_season, "\\Week ", current_week)
  )
# Add in conditional formatting for advantage per stat for each team

remove(report_card)
remove(current_week)
#remove(current_opponent)
remove(season_def_data)
remove(season_off_data)
remove(season_def_passing_stats)
remove(season_def_rushing_stats)
remove(season_off_passing_stats)
remove(season_off_rushing_stats)
remove(off_pass_fdr)
remove(off_rush_fdr)
remove(def_pass_fdr)
remove(def_rush_fdr)
remove(current_pbp)
remove(current_season)
remove(current_away)
remove(current_home)
remove(matchup_data_away)
remove(matchup_data_home)
remove(matchup_data_league_avg)
remove(report_card_data)
remove(season_averages)
remove(color_text)
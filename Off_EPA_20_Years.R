library('glue')
library('nflfastR')
library('magrittr')
library('tidyverse')

current_team <- readline("What team are we plotting? ")
current_season <- as.integer(readline("What year was last season?"))
pbp <- nflfastR::load_pbp(1999:current_season)
current_historical_off_epa <- pbp %>% 
  filter(play_type == "run" | play_type == "pass", !is.na(epa), posteam == current_team, season <= current_season) %>%
  group_by(season) %>% 
  summarise(
    OFF_EPA=mean(epa)
  )

current_head_coaches <- pbp %>% 
  filter(posteam == current_team, season <= current_season, home_team == current_team) %>% 
  group_by(season) %>% 
  select(
    season,
    home_coach
  ) %>% 
  unique()

# Filter through head coaches to account for coaches that were fired mid season
for (game in 1:nrow(current_head_coaches)) {
  if (game > 1) {
    if (as.character(current_head_coaches[game, 1]) == as.character(current_head_coaches[game-1, 1])) {
      print(glue(current_head_coaches[game-1,2]$home_coach, " was fired mid season, but kept for data continuity"))
      current_head_coaches = current_head_coaches[-c(game),]
    }
  }
}

team_name <- nflfastR::teams_colors_logos %>% 
  filter(team_abbr == current_team) %>% 
  select(
    team_name,
    team_abbr,
    team_color,
    team_color2,
    team_color3
  )

current_historical_off_epa$Coach <- current_head_coaches$home_coach
current_history_plot <- current_historical_off_epa %>% 
  ggplot(aes(x = season, y = OFF_EPA, color = Coach)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", alpha = 0.5) +
  geom_line() + 
  theme_bw() +
  labs(x = "Season",
       y = "Offensive EPA/Play",
       title = glue(team_name$team_name, " Offensive EPA/Play by Season (1999-", current_season, ")"),
       caption = "Author: @BearlyAnalytics") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_brewer(palette = 2, type = "qual")

ggsave(glue("Graphics/Historical/", team_name$team_name, " - 20 Year of Offense.png"), current_history_plot, device = "png", dpi = "retina", height = 6.75, width = 12)

# current_season <- as.integer(readline("What season is it? "))
# 
# current_epa_by_week <- pbp %>% 
#   filter(season == current_season, posteam == current_team, !is.na(epa), play_type == "run" | play_type == "pass", week <= 17) %>% 
#   group_by(week) %>% 
#   summarise(
#     week = week,
#     OFF_EPA = mean(epa)
#   ) %>% 
#   unique()
# 
# league_avg_epa_by_week <- pbp %>% 
#   filter(season == current_season, week <= 18, !is.na(epa), play_type == "run" | play_type == "pass") %>% 
#   group_by(week) %>% 
#   summarise(
#     week = week,
#     AVG_EPA = mean(epa)
#   ) %>% 
#   unique()
# 
# league_avg_epa_by_week <- league_avg_epa_by_week[-c(11),]
# current_epa_by_week$LEAGUE_AVG_EPA = league_avg_epa_by_week$AVG_EPA
# 
# current_off_epa_plot <- current_epa_by_week %>% 
#   ggplot(aes(x = week)) +
#   geom_point(aes(y = current_epa_by_week$OFF_EPA), color = team_name$team_color) +
#   geom_line(aes(y = current_epa_by_week$OFF_EPA, colour = glue(team_name$team_abbr ," Avg"))) +
#   geom_point(aes(y = current_epa_by_week$LEAGUE_AVG_EPA), shape = 4) +
#   geom_line(aes(y = current_epa_by_week$LEAGUE_AVG_EPA, colour = "League Avg"), linetype = "dashed") +
#   geom_hline(yintercept = 0.0, color = "red", linetype = "dashed", alpha = 0.4) +
#   scale_colour_manual("",
#                       breaks = c(glue(team_name$team_abbr," Avg"), "League Avg"),
#                       values = c(team_name$team_color, "green")) +
#   theme_bw() + 
#   labs(x = "Week (Bye Week Excluded)",
#        y = "Offensive EPA/Play",
#        title = glue(current_season, " ", team_name$team_name, " Offensive EPA/play vs League Average by Week"),
#        caption = "Data: @nflfastR") +
#   theme(
#     plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
#   ) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 18))
#   
# ggsave(glue("Graphics/Historical/", team_name$team_name, " - ",current_season ," Off EPA by Week.png"), current_off_epa_plot, device = "png", dpi = "retina", height = 6.75, width = 12)

remove(current_head_coaches)
remove(current_history_plot)
remove(current_epa_by_week)
remove(current_off_epa_plot)
#remove(current_2020_epa_by_week)
remove(current_historical_off_epa)
remove(team_name)
remove(league_avg_epa_by_week)
remove(current_team)
remove(game)
#remove(colors)
#remove(current_coach_colors)
#remove(num_coaches)
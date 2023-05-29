## CricketData Package

library(cricketdata)
library(tidyverse)
library(gt)
library(gtExtras)

##XG Boosted Model - balls_remaining = numeric, runs_scored_yet = int, wickest_lost_yet = int, run_chase = numeric


# Team Colours ------------------------------------------------------------

team_logos <- read.csv("wbb/logos.csv")
team <- c("Perth Scorchers", "Sydney Sixers", "Melbourne Renegades", "Sydney Thunder",
           "Brisbane Heat", "Hobart Hurricanes", "Adelaide Strikers", "Melbourne Stars")
team_colour_codes <- c("#FF6600", "#FF00FF","#CC0033","#33FF33", "#66FFFF", "#9933FF","#3399FF","#009966")
team_colours <- data.frame(team, team_colour_codes)
logos_and_colours <- team_logos %>%
  left_join(team_colours, by = c('team' = 'team'))


# Fetch all WBB ball-by-ball data
wbb_bbb <- fetch_cricsheet("bbb", "female", "wbb")

player_totals <- wbb_bbb %>%
  group_by(striker) %>%
  summarise(total_runs = sum(runs_off_bat),
            n = n()) %>%
  ungroup()


# Pull in Models ----------------------------------------------------------
### New Win Probability Model but using cricketdata package
## Variables neeed: For innings 1 - delivery_no (balls_remaining), runs_sum (runs_scored_yet), wickets_total (wickets_lost_yet)
# For Innings 2 - delivery_no(balls_remaining, runs_total, runs_sum(runs_scored_yet), wicket, wickets_total (wickets_lost_yet), run_chase
wp_model1_log <- readRDS("wbb/wp_model1_log.rds")
wp_model2_log <- readRDS("wbb/wp_model1_log.rds")


# Wrangle Data ready for WPA models ---------------------------------------

wbb_bbb_clean <- wbb_bbb %>%
  mutate(delivery_no = 120-balls_remaining,
         runs_gained = runs_off_bat+extras,
         run_chase = target-runs_scored_yet) %>%
  filter(season == "2022/23")

wbb_bbb_2022_innings1 <- wbb_bbb_clean %>%
  filter(innings == 1)

wbb_bbb_2022_innings2 <- wbb_bbb_clean %>%
  filter(innings == 2)

wt20_testing1 <- wbb_bbb_clean %>%
  filter(innings == 1) %>%
  dplyr::select(balls_remaining, runs_scored_yet, wickets_lost_yet) %>%
  as.matrix()

wt20_testing2 <- wbb_bbb_clean %>%
  filter(innings == 2) %>%
  dplyr::select(balls_remaining, runs_scored_yet, wickets_lost_yet, run_chase) %>%
  as.matrix()


test_1 <- predict(final_xgb_innings1_xg, wt20_testing1) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
wbb_bbb_2022_innings1$wp <- test_1$.

test_2 <- predict(final_xgb_innings2_xg, wt20_testing2) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
wbb_bbb_2022_innings2$wp <- test_2$.

wbb_bbb_2022_all <- bind_rows(wbb_bbb_2022_innings1, wbb_bbb_2022_innings2)

test_wbb <- wbb_bbb_2022_all %>%
  group_by(match_id) %>%
  mutate(wp_prev = lag(wp)) %>%
  ungroup() %>%
  mutate(wp_prev = replace_na(wp_prev, 0.49)) %>%
  mutate(bat_WPA = wp-wp_prev,
         bowl_WPA = -bat_WPA)

batters_wpa <- test_wbb %>%
  group_by(striker, batting_team) %>%
  summarise(total_bat_wpa = sum(bat_WPA, na.rm = TRUE),
            runs_for = sum(runs_off_bat, na.rm = TRUE),
            wickets_lost = sum(wicket, na.rm = TRUE),
            balls_faced= n()) %>%
  ungroup() %>%
  mutate(total_bat_wpa = round(total_bat_wpa, digits = 2))

bowlers_wpa <- test_wbb %>%
  group_by(bowler, bowling_team) %>%
  summarise(total_bowl_wpa = sum(bowl_WPA, na.rm = TRUE),
            wickets_taken = sum(wicket, na.rm = TRUE),
            runs_against = sum(runs_gained, na.rm = TRUE),
            balls_bowled= n()) %>%
  ungroup() %>%
  mutate(total_bowl_wpa = round(total_bowl_wpa, digits = 2))

wpa_bat_only <- batters_wpa %>%
  ##dplyr::select(batter, batting_team, total_bat_wpa) %>%
  rename(player = striker,
         team = batting_team)

wpa_bowl_only <- bowlers_wpa %>%
  ##dplyr::select(bowler, bowling_team,total_bowl_wpa) %>%
  rename(player = bowler,
         team = bowling_team)

wpa_both <- wpa_bat_only %>%
  full_join(wpa_bowl_only, by = c('player' = 'player', 'team' = 'team'), na_matches = "never")%>%
  mutate(total_bat_wpa = replace_na(total_bat_wpa, 0),
         total_bowl_wpa = replace_na(total_bowl_wpa, 0),
         balls_bowled = replace_na(balls_bowled, 0),
         balls_faced = replace_na(balls_faced, 0)) %>%
  mutate_if(is.integer, as.numeric) %>%
  group_by(player) %>%
  mutate(team = unique(team[!is.na(team)])) %>%
  replace(., is.na(.),0)%>%
  ungroup()%>%
  group_by(player, team) %>%
  summarise(across(c(1:8), sum)) %>%
  ungroup() %>%
  mutate(bowl_wpa_per_ball = (total_bowl_wpa/balls_bowled)*100,
         bowl_wpa_per_ball = replace_na(bowl_wpa_per_ball, 0),
         bowl_economy = runs_against/(balls_bowled/6),
         bowl_average = runs_against/wickets_taken,
         total_bowl_wpa = total_bowl_wpa*100,
         bat_wpa_per_ball = (total_bat_wpa/balls_faced)*100,
         bat_wpa_per_ball = replace_na(bat_wpa_per_ball, 0),
         strike_rate = runs_for/(balls_faced/6),
         average = runs_for/wickets_lost,
         total_bat_wpa = total_bat_wpa*100,
         total_wpa = total_bowl_wpa+total_bat_wpa,
         balls_total = balls_bowled+balls_faced,
         total_wpa_per_ball = total_wpa/balls_total) %>%
  mutate_if(is.numeric, round, 2)%>%
  left_join(logos_and_colours, by = c('team' = 'team'), na_matches = "never")



# GT tables ---------------------------------------------------------------

wpa_both %>%
  filter(balls_total >= 75) %>%
  dplyr::select(player, logo, total_wpa_per_ball, total_wpa, bat_wpa_per_ball,
                total_bat_wpa,bowl_wpa_per_ball,
                total_bowl_wpa) %>%
  arrange(desc(total_wpa)) %>%
  slice_max(total_wpa_per_ball, n = 15) %>%
  gt() %>%
  tab_header(
    title = "Women's BBL|08 Top 15 Win Probability Added per Ball",
    subtitle = "Regular Season Only"
  ) %>%
  cols_align(
    "center",
    columns = c(player, total_wpa_per_ball, total_wpa, bat_wpa_per_ball,
                total_bat_wpa,bowl_wpa_per_ball,
                total_bowl_wpa)
  ) %>% 
  cols_label(
    player = "Player",
    logo = "Team",
    total_wpa_per_ball = "WPA per Ball",
    total_wpa = "WPA",
    total_bat_wpa = "Bat WPA",
    bat_wpa_per_ball = "Bat WPA per Ball",
    total_bowl_wpa = "Bowl WPA",
    bowl_wpa_per_ball = "Bowl WPA per Ball"
  ) %>%
  tab_source_note(
    source_note = "Minimum combined balls bowled or faced >75"
  ) %>%
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) %>%
  tab_source_note(
    source_note = "Data: cricketdata"
  ) %>%
  gt_img_rows(columns = logo, height = 40) %>%
  gt_hulk_col_numeric(total_wpa_per_ball:total_bowl_wpa) %>%
  tab_options(footnotes.font.size = 12)%>%
  gtsave("wbb/WBB_Top15_WPA.png")


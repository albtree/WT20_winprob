## Women's ELO BBL
# ELO Script 
#https://cran.r-project.org/web/packages/EloRating/vignettes/EloRating_tutorial.pdf - how do custom start ratings e.g. via WPA team power ratings
library(tidyverse)
library(elo)
library(ggimage)
library(googlesheets4)
library(lubridate)
library(oddsapiR)
library(implied)
library(gt)
library(gtExtras)
library(glue)




## Columns required - match_id, date, season, team (batting_team), opponent, winner, overs completed, team_runs, team_projected runs,
#opponent_runs, margin

logos_and_colours <- readxl::read_excel("ment20/logos_mens.xlsx") %>%
  rename(team_colours = team_colour_codes)

innings <- read.csv("wbb/wbb_elo/innings_df.csv")
outcomes <- read.csv("wbb/wbb_elo/outcome_df.csv") 
dates <- read.csv("wbb/wbb_elo/dates_df.csv") 
teams <- read.csv("wbb/wbb_elo/team_df.csv")
logos_df <- read.csv("wbb/wbb_elo/logos.csv")
run_totals <- innings %>% 
  group_by(key, batting_team) %>%
  summarise(runs = sum(runs_total),
            over = max(delivery_no)) %>%
  ungroup() %>%
  left_join(dates, by = c('key' = 'key'), na_matches = "never") %>%
  left_join(outcomes, by = c('key' = 'key'), na_matches = "never") %>%
  left_join(teams, by = c('key' = 'key'), na_matches = "never") %>%
  filter(result != "no result") %>%
  mutate(proj_total = (runs/over)*20,
         proj_total = as.integer(proj_total),
         proj_total = if_else(by_type == "wickets", proj_total, runs),
         opponent = if_else(batting_team == team, "delete", team)) %>%
  filter(opponent != "delete") %>%
  group_by(key) %>%
  mutate(opp_runs1 = lag(proj_total),
         opp_runs2 = lead(proj_total)) %>%
  mutate(opp_runs1 = replace_na(opp_runs1, 0),
         opp_runs2 = replace_na(opp_runs2 , 0),
         opp_runs = case_when(opp_runs1 == 0 ~ opp_runs2,
                              opp_runs2 == 0 ~ opp_runs1)) %>%
  ungroup() %>%
  mutate(margin = proj_total - opp_runs,
         opponent = if_else(batting_team == team, "delete", team),
         season = case_when(date > '2022-01-30' ~ "2022",
                            date <= '2022-01-30' & date > '2021-02-01' ~ "2021",
                            date < '2021-01-30' & date >'2020-02-01' ~ "2020",
                            date < '2020-01-30' & date >'2019-02-01' ~ "2019",
                            date < '2019-01-30' & date >'2018-02-01' ~ "2018",
                            date < '2018-02-06' & date >'2017-02-01' ~ "2017",
                            date < '2017-01-30' & date >'2016-02-01' ~ "2016",
                            date < '2016-01-29' ~ "2015")) %>%
  rename(dont_use = team)

write.csv(run_totals, "wbb/wbb_elo/run_totals_womens.csv")


# Pre ELO Data wrangling --------------------------------------------------

womens_bbl_scores_home <- read.csv("wbb/wbb_elo/run_totals_womens.csv") %>%
  filter(method != "D/L") %>%
  dplyr::select(-bowl_out, -eliminator, -method, -proj_total, -by_innings, -result, -dont_use, -opp_runs1, -opp_runs2, -opp_runs, -margin) %>%
  group_by(key) %>%
  slice_min(X, n =1) %>%
  ungroup() %>%
  rename(bat_home = batting_team,
         runs_home = runs,
         overs_home = over)

womens_bbl_scores_bat_away <- read.csv("wbb/wbb_elo/run_totals_womens.csv") %>%
  filter(method != "D/L") %>%
  group_by(key) %>%
  slice_max(X, n =1) %>%
  ungroup() %>%
  rename(bat_away = batting_team,
         runs_away = runs,
         overs_away = over) %>%
  dplyr::select(key, bat_away, runs_away, overs_away)

womens_bbl_scores <- womens_bbl_scores_home %>%
  left_join(womens_bbl_scores_bat_away, by = c('key' = 'key'), na_matches = 'never') %>%
  separate_wider_delim(overs_home, delim = ".", names = c("overs_home", "ball_home"), too_few = "align_start") %>%
  separate_wider_delim(overs_away, delim = ".", names = c("overs_away", "ball_away"), too_few = "align_start") %>%
  mutate(overs_away = as.integer(overs_away),
         ball_away = replace_na(ball_away, "0"),
         ball_away = as.integer(ball_away),
         ball_perc_away = ball_away/6,
         team_away_overs = overs_away+ball_perc_away,
         proj_runs_away = case_when(winner == bat_away & by_type == "wickets" ~ ((runs_away/team_away_overs)*20),
                                    TRUE ~ runs_away),
         overs_home = as.integer(overs_home),
         ball_home = replace_na(ball_home, "0"),
         ball_home = as.integer(ball_home),
         ball_perc_home = ball_home/6,
         team_home_overs = overs_home+ball_perc_home,
         proj_runs_home = case_when(winner == bat_home & by_type == "wickets" ~ ((runs_home/team_home_overs)*20),
                                    TRUE ~ runs_home),
         margin = proj_runs_home-proj_runs_away) %>%
  mutate_if(is.numeric, round, 2)

# ELO creation ------------------------------------------------------------


#if team_2 and wickets, then projected runs

map_margin_to_outcome <- function(margin, marg.max = 60, marg.min = -60){
  norm <- (margin - marg.min)/(marg.max - marg.min)
  norm %>% pmin(1) %>% pmax(0)
}

# Set parameters
HGA <- 0 # home ground advantage
carryOver <- 0.6 # season carry over
k_val <- 20 # update weighting factor


elo_data <- elo.run(
  map_margin_to_outcome(proj_runs_home - proj_runs_away) ~
    adjust(bat_home, HGA) +
    bat_away +
    regress(season, 1500, carryOver) +
    group(key),
  k = k_val,
  data = womens_bbl_scores
)


elo_df <- as.data.frame(final.elos(elo_data)) %>%
  tibble::rownames_to_column("teams") %>%
  mutate(ELO = final.elos(elo_data)) %>%
  mutate_if(is.numeric, round, 0) %>%
  left_join(logos_and_colours, by = c('teams' = 'team'), na_matches = "never") %>%
  dplyr::select(teams, logo, ELO)


# Odds --------------------------------------------------------------------

bbl_odds <- toa_sports_odds(sport_key = "cricket_odi",
                            regions = "au",
                            markets = "h2h",
                            odds_format = 'decimal',
                            date_format = 'iso')
bbl_odds_df <- bbl_odds %>%
  filter(bookmaker == "SportsBet") %>%
  mutate(match_date = str_sub(commence_time,1, 10))

sportsbet_odds_home <- bbl_odds_df %>%
  group_by(id) %>%
  mutate(odds_home_away = if_else(home_team == outcomes_name, "Home", "Away")) %>%
  ungroup() %>%
  filter(odds_home_away == "Home") %>%
  dplyr::select(id, match_date, outcomes_name, outcomes_price)%>%
  rename(home_team = outcomes_name,
         home_odds = outcomes_price)

sportsbet_odds_away <- bbl_odds_df %>%
  group_by(id) %>%
  mutate(odds_home_away = if_else(home_team == outcomes_name, "Home", "Away")) %>%
  ungroup() %>%
  filter(odds_home_away == "Away") %>%
  dplyr::select(id, outcomes_name, outcomes_price)%>%
  rename(away_team = outcomes_name,
         away_odds = outcomes_price)

sportsbet_odds_both <- sportsbet_odds_home %>%
  left_join(sportsbet_odds_away, by = c('id' = 'id'), na_matches = 'never') %>%
  mutate(game_id = paste(home_team, "vs", away_team, "", match_date))
odds <- sportsbet_odds_both %>%
  dplyr::select(home_odds, away_odds) %>%
  implied_probabilities() %>%
  as.data.frame()

sportsbet_odds_both$home_implied_probability <- odds$probabilities.home_odds
sportsbet_odds_both$away_implied_probability <- odds$probabilities.away_odds



# Fixture -----------------------------------------------------------------

## Need way of matching custom game id with odds - home_team vs away_team match_date
##home_team, away_team, date, custom game_id

bbl_fixture <- read_sheet("https://docs.google.com/spreadsheets/d/1BPu30eM87HW9GJJ5jaUlsVBjsE0MZNSJBDBd7jm9LFE/edit#gid=0")%>%
  mutate(bat_home = if_else(bat_home == "Perth Scorchers", "India", bat_home),
         bat_away = if_else(bat_away == "Perth Scorchers", "India", bat_away),
         bat_home = if_else(bat_home == "Sydney Thunder", "West Indies", bat_home),
         bat_away = if_else(bat_away == "Sydney Thunder", "West Indies", bat_away))
2

bbl_fixture_df <- bbl_fixture %>%
  mutate(match_date = dmy(match_date)) %>%
  mutate(game_id = paste(bat_home, "vs", bat_away, "", match_date),
         prob_team_1 = predict(elo_data, newdata = bbl_fixture),
         prob_team_2 = 1-prob_team_1) %>%
  left_join(sportsbet_odds_both, by = c('game_id' = 'game_id'), na_matches = 'never') %>%
  rename(match_date = match_date.x) %>%
  dplyr::select(-match_date.y) %>%
  mutate_if(is.numeric, round, 2) %>%
  left_join(logos_and_colours, by = c('bat_home' = 'team')) %>%
  left_join(logos_and_colours, by = c('bat_away' = 'team')) %>%
  rename(competition = competition.x,
         logo_home = logo.x,
         team_colours_home = team_colour_codes.x,
         team_colours2_home = team_colour2.x,
         logo_away = logo.y,
         team_colours_away = team_colour_codes.y,
         team_colour2_away = team_colour2.y) %>%
  dplyr::select(-competition.y)

today_date <- with_tz(Sys.Date(), "Australia/Melbourne") %>% as.data.frame()

bbl_fixture_df %>%
  arrange((dmy(match_date))) %>%
  slice_min(match_date, n = 4) %>%
  dplyr::select(match_date, bat_home, logo_home, prob_team_1, home_odds, home_implied_probability, bat_away, logo_away,
                prob_team_2, away_odds, away_implied_probability) %>%
  gt() %>%
  tab_header(
    title = glue("Win Probabilities for Big Bash matches for next 3 matches"),
    subtitle = ""
  ) %>%
  cols_align(
    "center",
    columns = c(match_date, bat_home, prob_team_1, home_odds, home_implied_probability,
                bat_away, prob_team_2, away_odds, away_implied_probability)) %>% 
  cols_label(
    match_date = "Date",
    bat_home = "Home Team",
    logo_home = "",
    prob_team_1 = "ELO Win Prob %",
    home_odds = "Betting Odds",
    home_implied_probability = "Odds Imp. Prob",
    bat_away = "Away Team",
    logo_away = "",
    prob_team_2 = "ELO Win Prob %",
    away_odds = "Betting Odds",
    away_implied_probability = "Odds Imp. Prob") %>%
  tab_source_note(
    source_note = ""
  ) %>%
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) %>%
  tab_source_note(
    source_note = "Data: cricketdata, SportsBet via oddsapiR"
  ) %>%
  gt_img_rows(columns = logo_home, height = 40) %>%
  gt_img_rows(columns = logo_away, height = 40) %>%
  tab_options(footnotes.font.size = 12) %>%
  fmt_percent(c(prob_team_1, home_implied_probability, prob_team_2, away_implied_probability),
              decimals = 0)#%>%
gtsave("ment20/bbl/implied_elo_prob.png")



# GT ----------------------------------------------------------------------

elo_df %>%
  arrange(desc(ELO)) %>%
  gt() %>%
  tab_header(
    title = "Women's Big Bash League ELO Ratings",
    subtitle = "Final Regular Season Ratings"
  ) %>%
  cols_align(
    "left",
    columns = c(teams)
  ) %>% 
  cols_label(
    teams = "Team",
    logo = "",
    ELO = "ELO Rating (Avg 1500)",
  ) %>%
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) %>%
  tab_source_note(
    source_note = "Data: sawlachintan/cricket_data"
  ) %>%
  gt_img_rows(columns = logo, height = 40) %>%
  gt_hulk_col_numeric(ELO) %>%
  tab_options(footnotes.font.size = 12)%>%
  gtsave("wbb/wbb_elo/wbb_ELO.png")

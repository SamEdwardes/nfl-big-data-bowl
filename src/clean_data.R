library(janitor)
library(tidyverse)
options(scipen = 999)

# CLEAN DATA ----

# read data
raw <- read_csv("data/train.csv", col_types = cols())
train <- raw

# clean column names
train <- train %>%
  clean_names()

# correcting team names
train <- train %>%
  mutate(
    visitor_team_abbr = case_when(
      visitor_team_abbr == "ARI" ~ "ARZ",
      visitor_team_abbr == "BAL" ~ "BLT",
      visitor_team_abbr == "CLE" ~ "CLV",
      visitor_team_abbr == "HOU" ~ "HST",
      TRUE ~ visitor_team_abbr
    ), home_team_abbr = case_when(
      home_team_abbr == "ARI" ~ "ARZ",
      home_team_abbr == "BAL" ~ "BLT",
      home_team_abbr == "CLE" ~ "CLV",
      home_team_abbr == "HOU" ~ "HST",
      TRUE ~ home_team_abbr
    )
  )

# normalize field positioning data
train <- train %>%
  rename(
    line_of_scrimmage = yard_line,
    rushing_yards = yards,
    yards_to_first_down = distance
  ) %>%
  mutate(
    to_left = play_direction == "left",
    is_ball_carrier = nfl_id == nfl_id_rusher,
    team_on_offense = ifelse(home_team_abbr == possession_team, "home", "away"),
    is_on_offense = team == team_on_offense, # is player on offense
    yards_from_own_goal = ifelse(as.character(field_position) == possession_team,
      line_of_scrimmage, 50 + (50 - line_of_scrimmage)
    ),
    yards_from_own_goal = ifelse(line_of_scrimmage == 50, 50, yards_from_own_goal),
    yards_to_td = 100 - yards_from_own_goal,
    x_std = ifelse(to_left, 120 - x, x) - 10,
    y_std = ifelse(to_left, 160 / 3 - y, y)
  )

# CREATE FEATURES ----

# create a new data frame that has only one row for each play

train_features <- train %>%
  filter(nfl_id_rusher == nfl_id) %>%
  select(game_id, play_id, team, x_std, y_std, line_of_scrimmage, yards_to_first_down, yards_to_td, yards_from_own_goal, 
         down,
         defenders_in_the_box, defense_personnel, s, a,
         rushing_yards)


## Feature: distance from ball carrier ----

train <- train %>%
  left_join(
    select(train_features, play_id, ball_carrier_x_std = x_std, ball_carrier_y_std = y_std),
    by = c("play_id", "play_id")) %>%
  mutate(yards_from_ball_carrier = sqrt(abs(x_std - ball_carrier_x_std)^2 + abs(y_std - ball_carrier_y_std)^2))

mean_yards_from_ball_carrier_df <- train %>%
  mutate(is_on_offense = if_else(is_on_offense, "offense", "defense")) %>%
  group_by(play_id, is_on_offense) %>%
  summarise(
    yards_from_ball_carrier_mean = mean(yards_from_ball_carrier),
    yards_from_ball_carrier_min = min(yards_from_ball_carrier)
    ) %>%
  ungroup() %>%
  pivot_longer(
    names_to = "x", 
    values_to = "y",
    cols = -play_id:is_on_offense
  ) %>%
  pivot_wider(
    names_from = is_on_offense, 
    values_from = yards_from_ball_carrier_mean, yards_from_ball_carrier_min
    ) %>%
  mutate(
    yards_from_ball_carrier_mean_all = (mean_yards_from_ball_carrier_offense + mean_yards_from_ball_carrier_defense) / 2
    ) %>%
  ungroup()

train_features <- train_features %>%
  left_join(mean_yards_from_ball_carrier_df, by = c("play_id", "play_id"))



## Feature: defense personnel ----

defense_personnel_df <- train %>%
  group_by(play_id, defense_personnel) %>%
  summarise() %>%
  ungroup() %>%
  separate(defense_personnel, 
           remove = FALSE,
           into = c("count_1", "pos_1", "remove_1",
                    "count_2", "pos_2", "remove_2", 
                    "count_3", "pos_3", "remove_3",
                    "count_4", "pos_4"), # there are at most 4 positions
           # 11 columns will account for 4 positions if required
           sep = ",| ") %>%
  select(-contains("remove"), -defense_personnel) %>%
  pivot_longer(cols = -play_id, names_to = "key", values_to = "value") %>%
  mutate(key = case_when(str_detect(key, "count") ~ "number_of_players",
                         str_detect(key, "pos") ~ "position")) %>%
  drop_na(value) %>%
  mutate(position = case_when(key == "position" ~ value),
         number_of_players = case_when(key == "number_of_players" ~ as.numeric(value))) %>%
  fill(number_of_players) %>%
  select(-value, -key) %>%
  drop_na(position) %>%
  pivot_wider(names_from = position, values_from = number_of_players) %>%
  replace_na(list("DL" = 0, "LB" = 0, "DB" = 0, "OL" = 0))

train_features <- train_features %>%
  left_join(defense_personnel_df, by = c("play_id", "play_id"))
  



# EXPORT DATA ----


write_csv(train, "data/train_clean.csv")
write_csv(train_features, "data/train_features.csv")


# EXPLORE ----

View(train %>% head(100))
View(train_features %>% head(100))

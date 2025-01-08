#install.packages("hoopR")

library(hoopR)
library(tidyverse)
library(rvest)
library(corrplot)

team_box <- load_nba_team_box(2003:2023) %>% 
  filter(season_type == 2)

records <- team_box %>% 
  mutate(win = team_score > opponent_team_score) %>% 
  group_by(season, team_id) %>% 
  summarise(wins = sum(win),
            losses = n() - wins)

team_season_box <- team_box %>% 
  filter(!str_detect(team_name, "All-Stars|Team|World|USA")) %>% 
  group_by(season, team_id, team_name) %>% 
  mutate(two_point_field_goals_made = field_goals_made - three_point_field_goals_made,
         two_point_field_goals_attempted = field_goals_attempted - three_point_field_goals_attempted,
         two_point_field_goal_pct = two_point_field_goals_made / two_point_field_goals_attempted) %>% 
  summarise(across(
    .cols = c(team_score, opponent_team_score, assists, blocks, defensive_rebounds, offensive_rebounds, two_point_field_goal_pct, two_point_field_goals_attempted,
              three_point_field_goal_pct, three_point_field_goals_attempted, turnovers, free_throw_pct, free_throws_attempted), 
    .fns = \(col) mean(as.numeric(col)))) %>% 
  ungroup() %>% 
  merge(records, by = c("season", "team_id")) %>% 
  mutate(win_perc = wins / (wins + losses),
         season = season - 2002)

train <- sample(1:nrow(team_season_box), nrow(team_season_box) * .7)
test <- (1:nrow(team_season_box))[-train]

train_data <- team_season_box[train,]

model <- lm(win_perc ~ season * team_score + season * opponent_team_score + season * assists + season * blocks + season * defensive_rebounds + 
              season * offensive_rebounds + season * two_point_field_goal_pct + season * two_point_field_goals_attempted +
              season * three_point_field_goal_pct + season * three_point_field_goals_attempted + season * turnovers +
              season * free_throw_pct + season * free_throws_attempted, data = train_data)

cors <- cor(team_season_box %>% select(is.numeric))

corrplot(cors)

###########################################################################


# read data
playoff_data <- read.csv("playoff_data.csv")

# drop unimportant columns
columns_to_drop <- c('Lg', 'Series', 'X', 'X.1', 'X.2', 'W', 'W.1', 'X.3', 'Favorite', 'Underdog')
playoff_data <- playoff_data[, -which(names(playoff_data) %in% columns_to_drop)]

# combine team columns into one variable, remove duplicates 
playoff_data_long <- playoff_data %>%
  filter(Yr >= 2003) %>%
  gather(key='team', value='team_name', 2:3) %>%
  subset(select=-c(team)) %>%
  distinct()

# Remove seeds at the end of team names and shorten team names to match team_season_box
mapping <- c("Atlanta Hawks"="Hawks", "Houston Rockets"="Rockets", "Indiana Pacers"="Pacers", "Los Angeles Clippers"="Clippers",
              "Los Angeles Lakers"="Lakers", "Miami Heat"="Heat", "Milwaukee Bucks"="Bucks", "Minnesota Timberwolves"="Timberwolves",
              "New York Knicks"="Knicks", "Orlando Magic"="Magic", "Boston Celtics"="Celtics", "Philadelphia 76ers"="76ers", "Phoenix Suns"="Suns",
              "Portland Trail Blazers"="Trail Blazers", "Sacramento Kings"="Kings", "San Antonio Spurs"="Spurs", "Seattle SuperSonics"="SuperSonics",
              "Utah Jazz"="Jazz", "Washington Wizards"="Wizards", "Toronto Raptors"="Raptors", "Memphis Grizzlies"="Grizzlies",
              "Charlotte Hornets"="Hornets", "Chicago Bulls"="Bulls", "Cleveland Cavaliers"="Cavaliers", "Dallas Mavericks"="Mavericks",
              "Denver Nuggets"="Nuggets", "Detroit Pistons"="Pistons", "Golden State Warriors"="Warriors", "Charlotte Bobcats"="Bobcats",
              "Oklahoma City Thunder"="Thunder", "New Orleans Pelicans"="Pelicans", "New Jersey Nets"="Nets", "Brooklyn Nets"="Nets", "New Orleans Hornets"="Hornets")

playoff_data_long <- playoff_data_long %>%
  mutate_at(vars('team_name'), ~ gsub("\\s*\\([^\\)]+\\)", "", .)) %>%
  mutate(Yr = Yr - 2002, team_name = recode(team_name, !!!mapping))
 
# pivot data so that each year is a column and the element is a list of the playoff teams
pivot_data <- pivot_wider(playoff_data_long, names_from = Yr, values_from = team_name)

# create list of boolean values (TRUE = in the playoffs)
response_list = list()
for (i in seq(1,nrow(team_season_box))) {
    val = team_season_box[i, 'team_name'] %in% pivot_data[[as.character(team_season_box[i, 'season'])]][[1]]
    response_list <- append(response_list, val)
}

# append response list to data frame
team_season_box$response = response_list



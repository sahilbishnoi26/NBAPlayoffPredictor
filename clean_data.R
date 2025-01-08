#install.packages("hoopR")

library(hoopR)
library(tidyverse)
library(rvest)
library(corrplot)
library(magrittr)
library(janitor)

setwd("/Users/Alina/Documents/GitHub/6414-Project")

team_box <- load_nba_team_box(2003:2023) %>% 
  filter(season_type == 2)

records <- team_box %>% 
  mutate(win = team_score > opponent_team_score) %>% 
  group_by(season, team_id) %>% 
  summarise(wins = sum(win),
            losses = n() - wins)

team_season_box <- team_box %>% 
  filter(!str_detect(team_name, "All-Stars|Team|World|USA")) %>% 
  group_by(season, team_id, team_name, team_display_name, team_abbreviation) %>% 
  mutate(two_point_field_goals_made = field_goals_made - three_point_field_goals_made,
         two_point_field_goals_attempted = field_goals_attempted - three_point_field_goals_attempted,
         two_point_field_goal_pct = two_point_field_goals_made / two_point_field_goals_attempted) %>% 
  summarise(across(
    .cols = c(team_score, opponent_team_score, assists, blocks, defensive_rebounds, offensive_rebounds, two_point_field_goal_pct, two_point_field_goals_attempted,
              three_point_field_goal_pct, three_point_field_goals_attempted, turnovers, free_throw_pct, free_throws_attempted), 
    .fns = \(col) mean(as.numeric(col)))) %>% 
  ungroup() %>% 
  merge(records, by = c("season", "team_id")) %>% 
  mutate(win_perc = wins / (wins + losses))

all_stars <- data.frame()
attendance <- data.frame()

for(i in 2003:2023){
  
  raw <- paste0("https://basketball.realgm.com/nba/allstar/game/rosters/", i) %>% 
    read_html() %>% 
    html_table()
  
  print(paste0("All stars ", i, ": complete"))
  
  cur_all_stars <- rbind(raw[[length(raw)]], raw[[length(raw) - 1]]) %>% 
    mutate(season = i)
  
  all_stars <- rbind(all_stars, cur_all_stars)
  
  raw <- try(paste0("https://www.espn.com/nba/attendance/_/year/", i) %>% 
    read_html() %>% 
    html_table())

  cur_attendance <- raw %>% 
    extract2(1) %>%
    row_to_names(2) %>% 
    clean_names() %>% 
    mutate(season = i) %>% 
    select(team, season, avg) %>% 
    mutate(avg = as.numeric(str_remove(avg, ","))) %>% 
    filter(avg > 0) %>% 
    rename("avg_attendance" = avg)
  
  print(paste0("Attendance ", i, ": complete"))
  
   attendance <- rbind(attendance, cur_attendance)
  
   Sys.sleep(10)
   
}

all_stars_by_season <- all_stars %>% 
  distinct() %>% 
  group_by(Team, season) %>% 
  summarise(all_star_count = n()) %>% 
  mutate(Team = case_when(Team == "Philadelphia Sixers" ~ "Philadelphia 76ers",
                          Team == "Los Angeles Clippers" ~ "LA Clippers",
                          TRUE ~ Team))

final_attendance <- attendance %>% 
  distinct() %>% 
  mutate(team = case_when(team == "New York Knicks" ~ "Knicks",
                          team == "New Orleans Pelicans" & season <= 2013 ~ "Hornets",
                          team == "New Orleans Pelicans" ~ "Pelicans",
                          team == "Oklahoma City Thunder" & season <= 2008 ~ "SuperSonics",
                          team == "Oklahoma City Thunder" ~ "Thunder",
                          team == "New Orleans Hornets" ~ "Hornets",
                          TRUE ~ team))

extra_stats <- read_csv("playerstats_by_team_year.csv") %>% 
  mutate(season = paste0(substr(season, 1, 2), substr(season, 6, 7))) %>% 
  filter(as.numeric(season) >= 2003)

new_team_season_box <- team_season_box %>% 
  merge(all_stars_by_season, by.x = c("team_display_name", "season"), by.y = c("Team", "season"), all=T) %>% 
  merge(extra_stats, by = c("season", "team_abbreviation"), all=T) %>% 
  merge(final_attendance, by.x = c("season", "team_name"), by.y = c("season", "team"), all=T) %>% 
  mutate(all_star_count = ifelse(is.na(all_star_count), 0, all_star_count),
         two_point_fg_share = two_point_field_goals_attempted / (two_point_field_goals_attempted + three_point_field_goals_attempted))

playoff_games <- load_nba_team_box(2003:2023) %>% 
  filter(season_type == 3)

playoff_wins <- playoff_games %>% 
  group_by(team_name, season) %>% 
  summarise(playoff_wins = sum(team_score > opponent_team_score),
            playoff_games = n()) %>% 
  ungroup()

final_team_season_box <- new_team_season_box %>% 
  merge(playoff_wins, by = c("season", "team_name"), all = TRUE) %>% 
  mutate(playoff_wins = ifelse(is.na(playoff_wins), 0, playoff_wins)) %>% 
  filter(playoff_games > 0 & season > 2003) %>% 
  group_by(season) %>% 
  mutate(scaled_ppg = team_score - mean(team_score),
         scaled_oppg = opponent_team_score - mean(opponent_team_score),
         scaled_apg = assists - mean(assists),
         scaled_bpg = blocks - mean(blocks),
         scaled_dreb = defensive_rebounds - mean(defensive_rebounds),
         scaled_oreb = offensive_rebounds - mean(offensive_rebounds),
         scaled_to = turnovers - mean(turnovers)) %>% 
  ungroup() %>% 
  mutate(avg_attendance = ifelse(is.na(avg_attendance) | season == 2021, mean(avg_attendance), avg_attendance))

write.csv(final_team_season_box, "total_team_stats.csv")

set.seed(123)

train <- sample(1:nrow(final_team_season_box), nrow(final_team_season_box) * .7)
test <- (1:nrow(final_team_season_box))[-train]

train_data <- final_team_season_box[train,]
test_data <- final_team_season_box[test,]

#exploratory data analysis 
par(mfrow=c(3,3))
plot(final_team_season_box$scaled_ppg, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Scaled Points per Game')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$scaled_ppg), col='blue')
plot(final_team_season_box$scaled_oppg, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Scaled Opponents Points per Game')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$scaled_oppg), col='blue')
plot(final_team_season_box$scaled_apg, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Scaled Assists per Game')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$scaled_apg), col='blue')
plot(final_team_season_box$scaled_bpg, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Scaled Blocks per Game')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$scaled_bpg), col='blue')
plot(final_team_season_box$scaled_dreb, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Scaled Defensive Rebounds per Game')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$scaled_dreb), col='blue')
plot(final_team_season_box$scaled_oreb, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Scaled Offensive Rebounds per Game')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$scaled_oreb), col='blue')
plot(final_team_season_box$two_point_field_goal_pct, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Two Point Field Goal Percent')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$two_point_field_goal_pct), col='blue')
plot(final_team_season_box$three_point_field_goal_pct, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Three Point Field Goal Percent')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$three_point_field_goal_pct), col='blue')
plot(final_team_season_box$two_point_fg_share, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Percent of Attempts that were Two Point Field Goals')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$two_point_fg_share), col='blue')

par(mfrow=c(3,3))
plot(final_team_season_box$scaled_to, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Scaled Turnovers per Game')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$scaled_to), col='blue')
plot(final_team_season_box$free_throw_pct, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Free Throw Percentage')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$free_throw_pct), col='blue')
plot(final_team_season_box$free_throws_attempted, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Free Throw Attempts')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$free_throws_attempted), col='blue')
plot(final_team_season_box$all_star_count, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Number of All Star Players')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$all_star_count), col='blue')
plot(final_team_season_box$age, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Average Player Age')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$age), col='blue')
plot(final_team_season_box$player_height, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Average Player Height')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$player_height), col='blue')
plot(final_team_season_box$player_weight, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Average Player Weight')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$player_weight), col='blue')
plot(final_team_season_box$draft_number, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Average Player Draft Number')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$draft_number), col='blue')
plot(final_team_season_box$avg_attendance, final_team_season_box$playoff_wins, ylab = 'Playoff Wins', xlab = 'Average Number of Fans in Attendance')
abline(lm(final_team_season_box$playoff_wins ~ final_team_season_box$avg_attendance), col='blue')

model <- lm(playoff_wins ~ scaled_ppg + scaled_oppg + scaled_apg + scaled_bpg + scaled_dreb + scaled_oreb + 
               two_point_field_goal_pct + three_point_field_goal_pct + two_point_fg_share + scaled_to + free_throw_pct + 
               free_throws_attempted + all_star_count + age + player_height + player_weight + draft_number + avg_attendance,  data = final_team_season_box)

summary(model)

model <- glm(playoff_wins ~ scaled_ppg + scaled_oppg + scaled_apg + scaled_bpg + scaled_dreb + scaled_oreb + 
               two_point_field_goal_pct + three_point_field_goal_pct + scaled_to + free_throw_pct + 
               free_throws_attempted + all_star_count + age + player_height + player_weight + draft_number + avg_attendance,  data = final_team_season_box, family = "poisson")

summary(model)

#check significance
1-pchisq(model$null.deviance-model$deviance, model$df.null-model$df.residual)

#check goodness of fit 
c(deviance(model), 1-pchisq(deviance(model), model$df.residual))

pearsonres.tvalues = sum(residuals(model, type = "pearson")^2)
c(pearsonres.tvalues, 1-pchisq(pearsonres.tvalues, model$df.residual))

final_team_season_box$pred <- predict(model, final_team_season_box, type="response")
final_team_season_box$resid <- final_team_season_box$playoff_wins - final_team_season_box$pred

ggplot(final_team_season_box, aes(x = resid)) +
  geom_histogram() +
  labs(x = "Residuals",
       y = "Frequency",
       title = "Histogram of Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

qqnorm(final_team_season_box$resid,col="blue")
qqline(final_team_season_box$resid,col="red")

cors <- cor(final_team_season_box %>% dplyr::select(scaled_ppg, scaled_oppg, scaled_oppg, scaled_apg, scaled_bpg, scaled_dreb, scaled_oreb, 
                                               two_point_field_goal_pct, three_point_field_goal_pct, two_point_fg_share, scaled_to, free_throw_pct, 
                                               free_throws_attempted, all_star_count, age, player_height, player_weight, draft_number, avg_attendance),
            use = "pairwise.complete.obs")

corrplot(cors)

with(model, cbind(res.deviance = deviance, df = df.residual,p = pchisq(deviance, df.residual,lower.tail=FALSE)))

dev_residuals <- residuals(model, type = "deviance")
outliers <- which(abs(dev_residuals) > qnorm(.99995))
length(outliers)

#calculate overdispersion
wdf <- model$df.residual # n-p-1
dev <- model$deviance
overdisp <- dev/wdf

#overdisp adjusted models
require(MASS)
model_qpoisson <- glm(playoff_wins ~ scaled_ppg + scaled_oppg + scaled_oppg + scaled_apg + scaled_bpg + scaled_dreb + scaled_oreb + 
                        two_point_field_goal_pct + three_point_field_goal_pct + scaled_to + free_throw_pct + 
                        free_throws_attempted + all_star_count + age + player_height + player_weight + draft_number + avg_attendance,  
                      data = final_team_season_box, family = "quasipoisson")

model_negbin <- glm.nb(playoff_wins ~ scaled_ppg + scaled_oppg + scaled_oppg + scaled_apg + scaled_bpg + scaled_dreb + scaled_oreb + 
                         two_point_field_goal_pct + three_point_field_goal_pct + scaled_to + free_throw_pct + 
                         free_throws_attempted + all_star_count + age + player_height + player_weight + draft_number + avg_attendance,  data = final_team_season_box, link=log, init.theta = 10)
summary(model_qpoisson)
summary(model_negbin)

#check adjusted models significance 
1-pchisq(model_qpoisson$null.deviance-model_qpoisson$deviance, model_qpoisson$df.null-model_qpoisson$df.residual)
1-pchisq(model_negbin$null.deviance-model_negbin$deviance, model_negbin$df.null-model_negbin$df.residual)

wdf <- model_negbin$df.residual # n-p-1
dev <- model_negbin$deviance
overdisp <- dev/wdf

#check adjusted models goodness of fit 
c(deviance(model_qpoisson), 1-pchisq(deviance(model_qpoisson), model_qpoisson$df.residual))
c(deviance(model_negbin), 1-pchisq(deviance(model_negbin), model_negbin$df.residual))

pearsonres_qpoisson.tvalues = sum(residuals(model_qpoisson, type = "pearson")^2)
c(pearsonres_qpoisson.tvalues, 1-pchisq(pearsonres_qpoisson.tvalues, model_qpoisson$df.residual))
pearsonres_negbin.tvalues = sum(residuals(model_negbin, type = "pearson")^2)
c(pearsonres_negbin.tvalues, 1-pchisq(pearsonres_negbin.tvalues, model_negbin$df.residual))

final_team_season_box$pred_negbin <- predict(model_negbin, final_team_season_box, type="response")
final_team_season_box$resid_negbin <- final_team_season_box$playoff_wins - final_team_season_box$pred_negbin

ggplot(final_team_season_box, aes(x = resid_negbin)) +
  geom_histogram() +
  labs(x = "Residuals",
       y = "Frequency",
       title = "Histogram of Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

qqnorm(final_team_season_box$resid_negbin,col="blue")
qqline(final_team_season_box$resid_negbin,col="red")

#overdisp adjusted models gof
with(model_qpoisson, cbind(res.deviance = deviance, df = df.residual,p = pchisq(deviance, df.residual,lower.tail=FALSE)))
with(model_negbin, cbind(res.deviance = deviance, df = df.residual,p = pchisq(deviance, df.residual,lower.tail=FALSE)))

#negative binomial residual checks
hist(residuals(model_negbin),nclass=20,main="Histogram of residuals", xlab = "Residual")
qqnorm(residuals(model_negbin),col="blue")
qqline(residuals(model_negbin),col="red")

test_model$pred <- predict(model, test_model)
test_model$resid <- test_model$pred - test_model$win_perc

predictions <- final_team_season_box %>% filter(season == 2023)

predictions$pred <- predict(model, final_team_season_box %>% filter(season == 2023), type = "response")

mspe <- function(pred, dat){mean((pred-dat)^2, na.rm = T)}
mae <- function(pred, dat){mean(abs(pred-dat), na.rm = T)}
mape <- function(pred, dat){mean(abs(pred-dat)/abs(dat), na.rm = T)}
pm <- function(pred, dat){sum((pred-dat)^2, na.rm = T)/sum((dat-mean(dat, na.rm = T))^2)}

mspe(predict(model, final_team_season_box, type="response"), final_team_season_box$playoff_wins)
mspe(predict(model_negbin, final_team_season_box, type="response"), final_team_season_box$playoff_wins)

mae(predict(model, final_team_season_box, type="response"), final_team_season_box$playoff_wins)
mae(predict(model_negbin, final_team_season_box, type="response"), final_team_season_box$playoff_wins)

mape(predict(model, final_team_season_box, type="response"), final_team_season_box$playoff_wins)
mape(predict(model_negbin, final_team_season_box, type="response"), final_team_season_box$playoff_wins)

pm(predict(model, final_team_season_box, type="response"), final_team_season_box$playoff_wins)
pm(predict(model_negbin, final_team_season_box, type="response"), final_team_season_box$playoff_wins)

library(dplyr)

data <- read.csv("all_seasons.csv")
data1 <- data %>% mutate(draft_number = ifelse(draft_number == "Undrafted", NA, draft_number))
data2 <- data1 %>%
  group_by(team_abbreviation, season) %>%
  summarise(across(c(age, player_height, player_weight, draft_number), 
                   ~ifelse(all(is.na(.)), NA, mean(as.numeric(.), na.rm = TRUE))))
write.csv(data2, file = "playerstats_by_team_year.csv")

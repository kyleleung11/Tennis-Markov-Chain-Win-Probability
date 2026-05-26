library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

match_data <- read.csv("/Users/Downloads/2023-wimbledon-matches.csv")
points_data <- read.csv("/Users/Downloads/2023-wimbledon-points.csv")
wimbledon_2023 <- merge(points_data, match_data, by = "match_id")

# Data Cleaning- some faulty/incomplete matches in the dataset
wimbledon_2023 <- wimbledon_2023 %>%
  mutate(SetWinner = ifelse(
    SetNo == 5 & P2GamesWon == 7 & match_id == "2023-wimbledon-1304",
    2,
    SetWinner
  ))

wimbledon_2023 <- wimbledon_2023 %>%
  mutate(SetWinner = ifelse(
    SetNo == 5 & P2GamesWon == 7 & match_id == "2023-wimbledon-1132",
    2,
    SetWinner
  ))

wimbledon_2023 <- wimbledon_2023 %>%
  mutate(prev_SetWinner = lag(SetWinner)) %>%
  filter(!(SetWinner != 0 & prev_SetWinner != 0)) %>%
  select(-prev_SetWinner)

wimbledon_2023 <- wimbledon_2023 %>%
  filter(!(match_id %in% c("2023-wimbledon-1403", "2023-wimbledon-1104")))

wimbledon_2023 <- wimbledon_2023 %>%
  mutate(
    p1sets = ifelse(SetWinner == 1, 1, 0),
    p2sets = ifelse(SetWinner == 2, 1, 0),
  )

wimbledon_2023 <- wimbledon_2023 %>%
  group_by(match_id) %>%
  mutate(p1sets = cumsum(p1sets),
         p2sets = cumsum(p2sets)) %>%
  ungroup()
View(wimbledon_2023)
# --- Defining State
tennis_data <- wimbledon_2023 %>%
  group_by(match_id) %>%
  arrange(ElapsedTime) %>%
  mutate(
    state_pre = paste(PointServer, P1GamesWon, P2GamesWon, p1sets, p2sets, sep = "_"),
    state_post_raw = lead(state_pre),
    state_post = case_when(
      is.na(state_post_raw) & PointWinner == 1 ~ "Win_P1",
      is.na(state_post_raw) & PointWinner == 2 ~ "Win_P2",
      TRUE ~ state_post_raw
    )
  ) %>%
  ungroup()

state_counts <- tennis_data %>%
  count(state_pre) %>%
  filter(n >= 1)

tennis_data_filtered <- tennis_data %>%
  filter(state_pre %in% state_counts$state_pre)

transition_prob <- tennis_data_filtered %>%  
  filter(!is.na(state_post)) %>%
  count(state_pre, state_post) %>%
  group_by(state_pre) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

reward <- transition_prob %>%
  mutate(reward = ifelse(state_post == "Win_P1", 1, 0)) %>%
  select(state_pre, state_post, reward)

all_states <- union(transition_prob$state_pre, transition_prob$state_post)
all_states <- unique(c(all_states, "Win_P1", "Win_P2"))
value_previous <- tibble(state = all_states, value = 0) %>%
  mutate(value = case_when(
    state == "Win_P1" ~ 1,
    state == "Win_P2" ~ 0,
    TRUE ~ 0
  ))

threshold <- 1e-5
max_delta <- Inf
iter <- 0
while (max_delta > threshold && iter < 1000) {
  iter <- iter + 1
  value <- transition_prob %>%
    left_join(reward, by = c("state_pre", "state_post")) %>%
    left_join(value_previous, by = c("state_post" = "state")) %>%
    group_by(state = state_pre) %>%
    summarize(value = sum(prob * (coalesce(reward, 0) + coalesce(value, 0))), .groups = "drop")
  
  value <- bind_rows(
    value,
    tibble(state = c("Win_P1","Win_P2"),
           value = c(1,0))
  ) %>%
    distinct(state, .keep_all = TRUE)
  
  max_delta <- value_previous %>%
    inner_join(value, by = "state", suffix = c("_old","_new")) %>%
    summarize(max_delta = max(abs(value_old - value_new), na.rm = TRUE)) %>%
    pull(max_delta)
  
  value_previous <- value
}

win_prob_table <- value_previous %>% arrange(desc(value))

vals <- value_previous
tennis_with_values <- tennis_data_filtered %>%  
  left_join(vals, by = c("state_pre" = "state")) %>%
  rename(value_pre = value) %>%
  left_join(vals, by = c("state_post" = "state")) %>%
  rename(value_post = value) %>%
  mutate(delta_win_p1 = value_post - value_pre)

leaderboard <- tennis_with_values %>%
  mutate(player_responsible = ifelse(PointWinner == 1, player1, player2)) %>%
  group_by(player_responsible, match_id) %>%  # Group by match first!
  summarize(
    match_wpr = sum(ifelse(PointWinner == 1, delta_win_p1, -delta_win_p1), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(player_responsible) %>%
  summarize(
    total_wpr = sum(match_wpr),  
    n_matches = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_wpr))

top_10 <- leaderboard %>% head(10)
ggplot(top_10, aes(x = reorder(player_responsible, total_wpr), y = total_wpr)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(title = "Top Players by Win Probability Added",
       x = "Player", y = "Total Win Probability Added") +
  theme_minimal()
top_10_women <- leaderboard %>%
  filter(player_responsible == "Elina Svitolina" | player_responsible == "Marketa Vondrousova" |
         player_responsible == "Aryna Sabalenka" | player_responsible == "Elena Rybakina"
         | player_responsible == "Victoria Azarenka" | player_responsible == "Belinda Bencic"
         | player_responsible == "Caroline Garcia" | player_responsible == "Lesia Tsurenko"
         | player_responsible == "Ana Bogdan" | player_responsible == "Ekaterina Alexandrova")
ggplot(top_10_women, aes(x = reorder(player_responsible, total_wpr), y = total_wpr)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(title = "Top Female Players by Win Probability Added",
       x = "Player", y = "Total Win Probability Added") +
  theme_minimal()
top_10_men <- leaderboard %>%
  filter(player_responsible == "Novak Djokovic" | player_responsible == "Holger Rune" |
           player_responsible == "Hubert Hurkacz" | player_responsible == "Roman Safiullin"
         | player_responsible == "Daniel Elahi Galan" | player_responsible == "David Goffin"
         | player_responsible == "Tommy Paul" | player_responsible == "Grigor Dimitrov"
         | player_responsible == "Andrey Rublev" | player_responsible == "Stan Wawrinka")
ggplot(top_10_men, aes(x = reorder(player_responsible, total_wpr), y = total_wpr)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(title = "Top Male Players by Win Probability Added",
       x = "Player", y = "Total Win Probability Added") +
  theme_minimal()
head(leaderboard, 10)

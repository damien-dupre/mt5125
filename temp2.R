library(tidyverse)
library(faux)


scale_descriptives <- function(scale_items, min = min_range, max = max_range){
  low_mean <- ((max-min)/2)-((max-min)/4)
  high_mean <- ((max-min)/2)+((max-min)/4)
  scale_items %>% 
    enframe("id", "item") %>% 
    rowwise() %>% 
    mutate(
      mean = case_when(
        str_ends(item, "_r") ~ rtruncnorm(1, low_mean, 1, min, max),
        TRUE ~ rtruncnorm(1, high_mean, 1, min, max)
      ),
      sd = rtruncnorm(1, min +1, min + 0.5, min + 0.5, (max-min)/2),
      r = case_when(
        str_ends(item, "_r") ~ -runif(1, 0.5, 0.9),
        TRUE ~ runif(1, 0.5, 0.9)
      )
    ) %>% 
    select(-id) %>% 
    as_tibble()
}

rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd))
  qnorm(U, mean, sd) 
}

df_score <- sim_design(
  n = 748,
  within = list(vars = c("performance", "ti.score", "pos.score")),
  mu = list(performance = 100, ti.score = 0, pos.score = 0),
  sd = list(performance = 20, ti.score = 1, pos.score = 1),
  r = c(0.8, 0.7, 0.2), plot = FALSE
  ) %>% 
  select(-id) %>% 
  rowid_to_column("employee")

ti_score <- 
  c(
    paste("ti_", 1:4, sep = ""), 
    paste("ti_", 5, "_r", sep = "")
  ) %>% 
  scale_descriptives(1, 7) %>% 
  group_by(item) %>% 
  summarise(value = rnorm_pre(df_score$ti.score, mean, sd, r)) %>% 
  mutate(employee = row_number()) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(value = ifelse(!between(value, 1, 7), sample(1:7, 1), value) %>% round) %>% 
  pivot_wider(names_from = item, values_from = value)

pos_score <- 
  c(
    paste("pos_", c(1, 4, 6, 8), sep = ""),
    paste("pos_", c(2, 3, 5, 7), "_r", sep = "")
  ) %>% 
  scale_descriptives(1, 7) %>% 
  group_by(item) %>% 
  summarise(value = rnorm_pre(df_score$pos.score, mean, sd, r)) %>% 
  mutate(employee = row_number()) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(value = ifelse(!between(value, 0, 6), sample(0:6, 1), value) %>% round) %>% 
  pivot_wider(names_from = item, values_from = value)

df_data <- df_score %>% 
  full_join(ti_score, by = "employee") %>% 
  full_join(pos_score, by = "employee") %>% 
  mutate(performance = round(performance)) %>% 
  select(-ti.score, -pos.score)  %>% 
  mutate(
    gender = sample(c("male","female"), 748, replace = TRUE, prob = c(60, 40)),
    age = rtruncnorm(748, mean = 33, sd = 10.42, a = 18, b = 66) %>% round,
    experience = rtruncnorm(748, mean = 7.93, sd = 7.91, a = 1, b = Inf) %>% round
  )

# test <- df_data %>%
#   mutate(ti_5 = (7 + 1) - ti_5_r) %>%
#   rowwise() %>%
#   mutate(ti_score_2 = mean(c(ti_1, ti_2, ti_3, ti_4, ti_5)))
# summary(lm(performance ~ ti_score_2, data = test))
# 
# # turnover intentions ----------------------------------------------------------
# ti_1   <- rnorm_pre(df_score$ti.score, mu = 10, sd = 2, r = runif(1, 0.6, 0.9))
# ti_2   <- rnorm_pre(df_score$ti.score, mu = 10, sd = 2, r = runif(1, 0.6, 0.9))
# ti_3   <- rnorm_pre(df_score$ti.score, mu = 10, sd = 2, r = runif(1, 0.6, 0.9))
# ti_4   <- rnorm_pre(df_score$ti.score, mu = 10, sd = 2, r = runif(1, 0.6, 0.9))
# ti_5_r <- rnorm_pre(df_score$ti.score, mu = 10, sd = 2, r = -runif(1, 0.6, 0.9))
# 
# # perceived organisational support ---------------------------------------------
# pos_1   <- rnorm_pre(df_score$pos.score, mu = 10, sd = 2, r = 0.5)
# pos_2_r <- rnorm_pre(df_score$pos.score, mu = 10, sd = 2, r = 0.5)
# pos_3_r <- rnorm_pre(df_score$pos.score, mu = 10, sd = 2, r = 0.5)
# pos_4   <- rnorm_pre(df_score$pos.score, mu = 10, sd = 2, r = 0.5)
# pos_5_r <- rnorm_pre(df_score$pos.score, mu = 10, sd = 2, r = 0.5)
# pos_6   <- rnorm_pre(df_score$pos.score, mu = 10, sd = 2, r = 0.5)
# pos_7_r <- rnorm_pre(df_score$pos.score, mu = 10, sd = 2, r = 0.5)
# pos_8   <- rnorm_pre(df_score$pos.score, mu = 10, sd = 2, r = 0.5)
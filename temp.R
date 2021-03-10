library(tidyverse)
library(faux)

# function ---------------------------------------------------------------------
scale_correlation <- function(items){
  combinat::combn(items, 2) %>% 
    t() %>% 
    as_tibble() %>% 
    set_names(c("it1", "it2")) %>% 
    rowwise() %>% 
    mutate(
      value = runif(1, min = 0.6, max = 0.9),
      value = case_when(
        str_ends(it1, "_r") & !str_ends(it2, "_r") ~ -value,
        !str_ends(it1, "_r") & str_ends(it2, "_r") ~ -value,
        TRUE ~ value
      )
    ) %>%
    bind_rows(., set_names(., c("it2", "it1", "value"))) %>% 
    bind_rows(tibble(it1 = items, it2 = items, value = 1)) %>% 
    pivot_wider(names_from = it2, values_from = value) %>% 
    arrange(it1) %>% 
    column_to_rownames("it1") %>% 
    select(sort(colnames(.))) %>% 
    as.matrix()
}

scale_descriptives <- function(scale_cor, min = min_range, max = max_range){
  low_mean <- ((max-min)/2)-((max-min)/4)
  high_mean <- ((max-min)/2)+((max-min)/4)
  scale_cor %>% 
    as_tibble() %>% 
    colnames() %>% 
    enframe("id", "item") %>% 
    rowwise() %>% 
    mutate(
      mean = case_when(
        str_ends(item, "_r") ~ rtruncnorm(1, low_mean, 1, min, max),
        TRUE ~ rtruncnorm(1, high_mean, 1, min, max)
      ),
      sd = rtruncnorm(1, min +1, min + 0.5, min + 0.5, (max-min)/2)
    ) %>% 
    select(-id) %>% 
    as_tibble()
}

scale_descriptives2 <- function(scale_items, min = min_range, max = max_range){
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
      sd = rtruncnorm(1, min +1, min + 0.5, min + 0.5, (max-min)/2)
    ) %>% 
    select(-id) %>% 
    as_tibble()
}

rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd))
  qnorm(U, mean, sd) 
}

# turnover intentions ----------------------------------------------------------
ti_cor <- 
  c(
    paste("ti_", 1:4, sep = ""), 
    paste("ti_", 5, "_r", sep = "")
  ) %>% 
  scale_correlation()

ti_desc <- scale_descriptives(ti_cor, 1, 7) 

rnorm_multi(
  n = 250, 
  vars = nrow(ti_cor), 
  mu = ti_desc$mean, 
  sd = ti_desc$sd, 
  r = ti_cor,
  varnames = colnames(ti_cor)
) 

# perceived organisational support ---------------------------------------------
pos_cor <- 
  c(
    paste("pos_", c(1, 4, 6, 8), sep = ""),
    paste("pos_", c(2, 3, 5, 7), "_r", sep = "")
  ) %>% 
  scale_correlation()

pos_desc <- scale_descriptives(pos_cor, 0, 6) 

rnorm_multi(
  n = 250, 
  vars = nrow(pos_cor), 
  mu = pos_desc$mean, 
  sd = pos_desc$sd, 
  r = pos_cor,
  varnames = colnames(pos_cor)
)




df <- tibble(
  gender = sample(c("male","female"), 250, replace = TRUE, prob = c(68, 32)),
  age = rtruncnorm(250, mean = 40, sd = 15, a = 18, b = 66)
) %>% 
  bind_cols(
    rnorm_multi(
      n = 250, 
      vars = nrow(pos_cor), 
      mu = pos_desc$mean, 
      sd = pos_desc$sd, 
      r = pos_cor,
      varnames = colnames(pos_cor)
    ),
    rnorm_multi(
      n = 250, 
      vars = nrow(ti_cor), 
      mu = ti_desc$mean, 
      sd = ti_desc$sd, 
      r = ti_cor,
      varnames = colnames(ti_cor)
    ) 
  ) %>%
  rowwise() %>% 
  mutate(
    across(starts_with("pos_"), ~ ifelse(!between(.x, 0, 6), NA, .x)),
    across(starts_with("ti_"), ~ ifelse(!between(.x, 1, 7), rtruncnorm(1, mean = 3.5, sd = 1.5, a = 1, b = 7) %>% round, .x)),
    across(where(is.numeric), round)
  )

# all scales ###################################################################

items <- c(
  paste("pos_", c(1, 4, 6, 8), sep = ""), paste("pos_", c(2, 3, 5, 7), "_r", sep = ""), # perceived orgnanisational suppot
  paste("ti_", 1:4, sep = ""), paste("ti_", 5, "_r", sep = ""),
  "performance"
  )

df_correlations <- scale_correlation(items)

df_descriptive <- 
  bind_rows(
    scale_descriptives2(str_subset(items, "^pos_"), 0, 6),
    scale_descriptives2(str_subset(items, "^ti_"), 1, 7)
  ) %>% 
  add_row(item = "performance", mean = 99, sd = 33) %>% 
  arrange(item)

df_data <- 
  rnorm_multi(
    n = 250, 
    vars = nrow(df_correlations), 
    mu = df_descriptive$mean, 
    sd = df_descriptive$sd, 
    r = df_correlations,
    varnames = colnames(df_correlations)
  )

library(tidyverse)
library(tabulizer)
library(here)
library(janitor)
library(faux)
set.seed(42)

# functions --------------------------------------------------------------------
rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd))
  qnorm(U, mean, sd) 
}

# data sim ---------------------------------------------------------------------
df <- 
  rnorm_multi(
    n = 2000, 
    vars = 2, 
    mu = c(100, 2.5), 
    sd = c(20, 1.5), 
    r = c(-0.7),
    varnames = c("SR", "IP")
  ) %>% 
  mutate(
    AS = rnorm_pre(IP, mu = 0, sd = 1, r = 0.5),
    AS = if_else(AS > 0, "abusive supervision", "non-abusive supervision"),
    SR = if_else(SR < 0, 10, SR) %>% round,
    SR = if_else(row_number() %in% sample(1:2000, 100), NaN, SR),
    IP = case_when(
      IP < 0 ~ 0,
      IP > 5 ~ 5,
      TRUE ~ IP
    ) %>% round/5
  ) %>% 
  write_csv(here("doc/voodoo_data.csv"))


df <- 
  rnorm_multi(
    n = 2000, 
    vars = 2, 
    mu = c(100, 2.5), 
    sd = c(20, 1.5), 
    r = c(-0.7, ),
    varnames = c("SR", "IP")
  ) %>% 
  mutate(
    SR = if_else(SR < 0, 10, SR) %>% round,
    SR = if_else(row_number() %in% sample(1:2000, 100), NaN, SR),
    IP = case_when(
      IP < 0 ~ 0,
      IP > 5 ~ 5,
      TRUE ~ IP
    ) %>% round/5,
    AS = sample(c("abusive supervision", "non-abusive supervision"), 2000, replace = TRUE)
  )

# paper sim --------------------------------------------------------------------
df <- 
  extract_tables(
    here("doc/voodoo_paper.pdf"), 
    pages = 5, 
    output = "data.frame"
  )[[1]] %>%
  remove_empty("cols") %>% 
  clean_names() %>% 
  filter(x != "")

df_descriptive <- test %>% 
  select(x:sd) %>% 
  separate(mean_sd, c("mean", "sd"))

df_matrix <- test %>% 
  select(starts_with("x"))

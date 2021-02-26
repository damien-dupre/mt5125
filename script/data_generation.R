set.seed(123)
################################################################################
rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
    if (a > b) stop('Error: Truncation range is empty');
    U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd));
    qnorm(U, mean, sd); }
################################################################################
DV_0to10 <- c(
    rtruncnorm(50, mean = 8, sd = 3, a = 0, b = 10), 
    rtruncnorm(50, mean = 4, sd = 3, a = 0, b = 10)
    )

IV <- c(
    rep("condition_1",50), 
    rep("condition_2",50)
    )

df <- data.frame(DV_0to10,IV)

write.csv(df,"DV_continuous_IV_categorical_2.csv", row.names = FALSE)
################################################################################
DV_0to10 <- c(
    rtruncnorm(33, mean = 2, sd = 3, a = 0, b = 10), 
    rtruncnorm(33, mean = 8, sd = 3, a = 0, b = 10),
    rtruncnorm(34, mean = 5, sd = 3, a = 0, b = 10)
)

IV <- c(
    rep("condition_1",33), 
    rep("condition_2",33), 
    rep("condition_3",34)
    )

df <- data.frame(DV_0to10,IV)

write.csv(df,"DV_continuous_IV_categorical_3.csv", row.names = FALSE)
################################################################################
DV_0to10 <- c(
    rtruncnorm(25, mean = 8, sd = 3, a = 0, b = 10), 
    rtruncnorm(25, mean = 4, sd = 3, a = 0, b = 10),
    rtruncnorm(25, mean = 2, sd = 3, a = 0, b = 10), 
    rtruncnorm(25, mean = 6, sd = 3, a = 0, b = 10)
)

IV1 <- c(
    rep("IV1_condition_1",50), 
    rep("IV1_condition_2",50)
)

IV2 <- c(
    rep("IV2_condition_1",25), 
    rep("IV2_condition_2",25),
    rep("IV2_condition_1",25), 
    rep("IV2_condition_2",25)
)

df <- data.frame(DV_0to10,IV1,IV2)

write.csv(df,"DV_continuous_IV1_categorical_2_IV2_categorical_2.csv", row.names = FALSE)
################################################################################
samples = 100
r = 0.83

library('MASS')
data = mvrnorm(n=samples, mu=c(5, 50), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
DV_0to10 = data[, 1]  # standard normal (mu=0, sd=1)
IV_0to100 = data[, 2]  # standard normal (mu=0, sd=1)

df <- data.frame(DV_0to10,IV_0to100)

write.csv(df,"DV_continuous_IV_continuous.csv", row.names = FALSE)
################################################################################
samples = 100
r = 0.83

library('MASS')
data = mvrnorm(n=samples, mu=c(5, 50), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
DV_0to10 = data[, 1]  # standard normal (mu=0, sd=1)
IV1_0to100 = data[, 2]  # standard normal (mu=0, sd=1)

IV2 <- c(
    rep("IV2_condition_1",50), 
    rep("IV2_condition_2",50)
)

df <- data.frame(DV_0to10,IV1_0to100,IV2)

write.csv(df,"DV_continuous_IV1_continuous_IV2_categorical_2.csv", row.names = FALSE)
################################################################################
samples = 100
r = 0.83

library('MASS')
data = mvrnorm(n=samples, mu=c(5, 50), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
DV_0to10 = data[, 1]  # standard normal (mu=0, sd=1)
IV1_0to100 = data[, 2]  # standard normal (mu=0, sd=1)

IV2_0to100 <- rtruncnorm(100, mean = 33, sd = 33, a = 0, b = 100)

df <- data.frame(DV_0to10,IV1_0to100,IV2_0to100)

write.csv(df,"DV_continuous_IV1_continuous_IV2_continuous.csv", row.names = FALSE)
################################################################################
DV_Q1 <- sample(1:10, 100, replace=TRUE)
DV_Q2 <- sample(1:10, 100, replace=TRUE)
DV_Q3 <- sample(1:10, 100, replace=TRUE)
DV_Q4 <- sample(1:10, 100, replace=TRUE)
DV_Q5_to_rev <- sample(1:10, 100, replace=TRUE)

IV <- c(
    rep("condition_1",33), 
    rep("condition_2",33), 
    rep("condition_3",34)
)

df <- data.frame(
    DV_Q1,
    DV_Q2,
    DV_Q3,
    DV_Q4,
    DV_Q5_to_rev,
    IV)
write.csv(df,"survey_5Q.csv", row.names = FALSE)

################################################################################
DV_Q1 <- sample(1:10, 100, replace=TRUE)
DV_Q2 <- sample(1:10, 100, replace=TRUE)
DV_Q3 <- sample(1:10, 100, replace=TRUE)
DV_Q4 <- sample(1:10, 100, replace=TRUE)
DV_Q5_to_rev <- sample(1:10, 100, replace=TRUE)

IV1 <- c(
    rep("condition_1",33), 
    rep("condition_2",33), 
    rep("condition_3",34)
)

IV2_Q1 <- sample(1:10, 100, replace=TRUE)
IV2_Q2 <- sample(1:10, 100, replace=TRUE)
IV2_Q3 <- sample(1:10, 100, replace=TRUE)
IV2_Q4 <- sample(1:10, 100, replace=TRUE)
IV2_Q5_to_rev <- sample(1:10, 100, replace=TRUE)

IV3_Q1 <- sample(1:10, 100, replace=TRUE)
IV3_Q2 <- sample(1:10, 100, replace=TRUE)
IV3_Q3 <- sample(1:10, 100, replace=TRUE)
IV3_Q4 <- sample(1:10, 100, replace=TRUE)
IV3_Q5_to_rev <- sample(1:10, 100, replace=TRUE)

df <- data.frame(
    DV_Q1,
    DV_Q2,
    DV_Q3,
    DV_Q4,
    DV_Q5_to_rev,
    IV1,
    IV2_Q1,
    IV2_Q2,
    IV2_Q3,
    IV2_Q4,
    IV2_Q5_to_rev,
    IV3_Q1,
    IV3_Q2,
    IV3_Q3,
    IV3_Q4,
    IV3_Q5_to_rev
    )
write.csv(df,"survey_mediation.csv", row.names = FALSE)

################################################################################
rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  if (a > b) stop('Error: Truncation range is empty');
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd));
  qnorm(U, mean, sd); }

library(magrittr)
managers <- c(rep("senior",100), rep("junior", 100))
monthly_contract_negociated <- c(runif(100, 20, 50), runif(100, 10, 30)) %>% round(0)

job_satisfaction_Q1 <- c(
  rtruncnorm(100, mean = 8, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 4, sd = 3, a = 0, b = 10)
) %>% round(0)

job_satisfaction_Q2 <- c(
  rtruncnorm(100, mean = 7, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 5, sd = 3, a = 0, b = 10)
) %>% round(0)

job_satisfaction_Q3 <- c(
  rtruncnorm(100, mean = 6, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 6, sd = 3, a = 0, b = 10)
) %>% round(0)

job_satisfaction_Q4 <- c( # reversed
  rtruncnorm(100, mean = 2, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 8, sd = 3, a = 0, b = 10)
) %>% round(0)

job_satisfaction_Q5 <- c(
  rtruncnorm(100, mean = 7, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 5, sd = 3, a = 0, b = 10)
) %>% round(0)

stress_Q1 <- c(
  rtruncnorm(100, mean = 1, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 9, sd = 3, a = 0, b = 10)
) %>% round(0)

stress_Q2 <- c(
  rtruncnorm(100, mean = 2, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 8, sd = 3, a = 0, b = 10)
) %>% round(0)

stress_Q3 <- c(
  rtruncnorm(100, mean = 3, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 7, sd = 3, a = 0, b = 10)
) %>% round(0)

stress_Q4 <- c( # reversed
  rtruncnorm(100, mean = 8, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 2, sd = 3, a = 0, b = 10)
) %>% round(0)

turn_over_Q1 <- c(
  rtruncnorm(100, mean = 1, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 9, sd = 3, a = 0, b = 10)
) %>% round(0)

turn_over_Q2 <- c(
  rtruncnorm(100, mean = 2, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 8, sd = 3, a = 0, b = 10)
) %>% round(0)

turn_over_Q3 <- c(
  rtruncnorm(100, mean = 3, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 7, sd = 3, a = 0, b = 10)
) %>% round(0)

turn_over_Q4 <- c( # reversed
  rtruncnorm(100, mean = 7, sd = 3, a = 0, b = 10), 
  rtruncnorm(100, mean = 3, sd = 3, a = 0, b = 10)
) %>% round(0)

df <- data.frame(
  managers,
  monthly_contract_negociated,
  job_satisfaction_Q1,
  job_satisfaction_Q2,
  job_satisfaction_Q3,
  job_satisfaction_Q4,
  job_satisfaction_Q5,
  stress_Q1,
  stress_Q2,
  stress_Q3,
  stress_Q4,
  turn_over_Q1,
  turn_over_Q2,
  turn_over_Q3,
  turn_over_Q4
)
write.csv(df,"manager_satisfaction_performance.csv", row.names = FALSE)

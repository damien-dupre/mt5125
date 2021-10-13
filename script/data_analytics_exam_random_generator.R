################################################################################
#                   Data Analytics Exam Random Generator                       #
################################################################################
# libraries --------------------------------------------------------------------
library(tidyverse)
library(mailR)
library(here)
library(faux)
library(glue)

# functions --------------------------------------------------------------------
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

# test emails ------------------------------------------------------------------
list_email_students <- data.frame(
  Firstname = c("Test 1", "Test 2"),
  Email = c("test_1@gmail.com", "test_2@gmail.com")
)

# student emails ---------------------------------------------------------------
list_email_students <- file.choose() %>% 
  read_csv()

# email setup  -----------------------------------------------------------------
gmail_dcu <- config::get("gmail_dcu")

email_text <- 
  "<p>
  Dear {name}, 
  <br><br>
  Please find here attached the instructions and data regarding the MT5125 Data Analytics 
  Individual Assessment. Please follow the instructions attached and submit the 
  report by June 21st, 2021 on Loop. Can you confirm the reception of the 
  email?
  <br><br>
  These data are simulated according a specific code related to your email, 
  therefore they are unique in order to prevent any copy-pasting between students.
  <br><br>
  Let me know if you have any problem to download the documents attached.
  <br><br>
  Best regards,
  <br><br>
  Damien
  </p>"

################################################################################
for(row_student in 1:nrow(list_email_students)){
  
  student_db <- list_email_students[row_student, ]
  email <- student_db[["Email"]]
  name <- student_db[["Firstname"]]
  
  df_score <- 
    sim_design(
      n = 748,
      within = list(vars = c("performance", "ti.score", "pos.score")),
      mu = list(performance = 100, ti.score = 0, pos.score = 0),
      sd = list(performance = 30, ti.score = 1, pos.score = 1),
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
    mutate(value = ifelse(!between(value, 1, 7), NA, value) %>% round) %>% 
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
    mutate(value = ifelse(!between(value, 0, 6), NA, value) %>% round) %>% 
    pivot_wider(names_from = item, values_from = value)
  
  df_data <- df_score %>% 
    full_join(ti_score, by = "employee") %>% 
    full_join(pos_score, by = "employee") %>% 
    mutate(performance = round(performance)) %>% 
    select(-ti.score, -pos.score)  %>% 
    mutate(
      gender = sample(c("male","female"), 748, replace = TRUE, prob = c(60, 40)),
      age = rtruncnorm(748, mean = 33, sd = 10.42, a = 18, b = 66) %>% round,
      experience = rtruncnorm(748, mean = 2.93, sd = 1.91, a = 1, b = Inf) %>% round
    ) %>% 
    write_csv(here("assignment/individual_assignment_data.csv"), na = "")
  
  print(email)
  
  mailR::send.mail(
    from = gmail_dcu$email,
    to = email,
    subject = "MT5125 - Data Analytics Individual Assignment - Instructions and Data",
    body = glue::glue(email_text),
    attach.files = c(
      here("assignment/individual_assignment_data.csv"),
      here("assignment/individual_assignment_instructions.pdf"),
      here("assignment/template_1.pdf"),
      here("assignment/template_2.pdf"),
      here("assignment/template_3.pdf")
    ),
    smtp = list(
      host.name = "smtp.gmail.com", port = 465,
      user.name = gmail_dcu$email,
      passwd = gmail_dcu$passwd,
      ssl = TRUE
      ),
    authenticate = TRUE,
    send = TRUE,
    html = TRUE
    )
}

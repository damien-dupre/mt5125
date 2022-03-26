# libraries --------------------------------------------------------------------
library(blastula)
library(faux)
library(glue)
library(here)
library(janitor)
library(magrittr)
library(rmarkdown)
library(tidyverse)

# functions --------------------------------------------------------------------
trim_data <- function(x) {
  case_when(
    x > 3 ~ 3,
    x < -3 ~ -3,
    TRUE ~ round(x, 0)
  ) |> 
  add(4)
}

# variables --------------------------------------------------------------------
variable_list <- c("work_motivation", "job_satisfaction", "organizational_commitment", "leadership")
business_unit <- c("Administration/operations", "Research/development", "Marketing", "Sales", "Human resources", "Customer service", "Accounting/finance")
email_body <- "
Dear {student_db$firstname}, 

I hope you are well,

Please, find here attached the data and instructions for the MT5125 Data Analytics 
assignment. To succeed in this assignment, follow these instructions carefully 
and submit the report by Tuesday **June 21st, 2022**, 1pm on Loop. 
{student_db$firstname}, can you confirm the reception of this email containing 
data and instructions?

Note that all assignments are individual, every student has received different 
data to prevent collaboration. Any similarities between submissions will be 
considered as plagiarism.

Let me know if you have any problem to download the documents attached.

Best regards,

Damien"

# test emails ------------------------------------------------------------------
list_email_students <- 
  tibble(
    firstname = c("Damien", "Test 1", "Test 2"),
    student_no = c(42, 1234, 999),
    email = c("damien.dupre@dcu.ie", "test_1@gmail.com", "test_2@gmail.com")
  ) # check in email sent box if they have been successfully sent 

# student emails ---------------------------------------------------------------
list_email_students <- file.choose() |>  
  read_csv() |> 
  clean_names()

# for loop ---------------------------------------------------------------------
for (row_student in 1:nrow(list_email_students)) {
  
  student_db <- list_email_students[row_student, ]

  set.seed(student_db$student_no)
  
  # student variables ----------------------------------------------------------
  vari_selected <- sample(variable_list, 2)
  unit_selected <- sample(business_unit, 2)
  rev_item_pred <- paste0("predictor.", sample(1:5, 1))
  rev_item_outc <- paste0("dv.", sample(1:5, 1))
  
  # student data ---------------------------------------------------------------
  dat <- 
    sim_design(
      n = 500,
      between = list(group = c("A", "B")),
      within = list(vars = c("dv.1", "predictor.1")),
      mu = list(
        A = c(dv.1 = 0, predictor.1 = 0),
        B = c(dv.1 = 1, predictor.1 = 0)
      ),
      sd = list(
        A = c(dv.1 = 1, predictor.1 = 1),
        B = c(dv.1 = 1, predictor.1 = 1)
      ),
      r = list(A = 0.6, B = 0), plot = FALSE) %>%
    mutate(
      group = as.character(group),
      predictor.2 = rnorm_pre(predictor.1, mu = 0, sd = 1, r = 0.7),
      predictor.3 = rnorm_pre(predictor.1, mu = 0, sd = 1, r = 0.7),
      predictor.4 = rnorm_pre(predictor.1, mu = 0, sd = 1, r = 0.7),
      predictor.5 = rnorm_pre(predictor.1, mu = 0, sd = 1, r = 0.7),
      dv.2 = rnorm_pre(dv.1, mu = 0, sd = 1, r = 0.6),
      dv.3 = rnorm_pre(dv.1, mu = 0, sd = 1, r = 0.6),
      dv.4 = rnorm_pre(dv.1, mu = 0, sd = 1, r = 0.6),
      dv.5 = rnorm_pre(dv.1, mu = 0, sd = 1, r = 0.6)
    ) |> 
    mutate(across(all_of(c(rev_item_pred, rev_item_outc)), multiply_by, -1)) |> 
    mutate(across(where(is.numeric), trim_data)) |> 
    rowwise() |> 
    mutate(
      # dv = mean(dv.1:dv.5),
      # predictor = mean(predictor.1:predictor.5),
      group = case_when(
        group == "A" ~ unit_selected[1],
        group == "B" ~ unit_selected[2]
      ),
      age = runif(1, 19, 65) |> round(0)
    ) |> 
    ungroup() |> 
    select(
      employee_id = id, 
      employee_department = group,
      employee_age = age,
      starts_with("predictor"),
      starts_with("dv")
    ) |> 
    rename_with(~ str_replace(.x, "predictor", vari_selected[1])) |> 
    rename_with(~ str_replace(.x, "dv", vari_selected[2])) |> 
    write_csv("assignment/assignment_data.csv")
  
  # student instructions -------------------------------------------------------
  render(
    input = here("assignment/assignment_instructions.Rmd"),
    output_file = here("assignment/assignment_instructions.pdf"),
    params = list(
      unit_selected_1 = unit_selected[1],
      unit_selected_2 = unit_selected[2],
      vari_selected_1 = vari_selected[1],
      vari_selected_2 = vari_selected[2]
    )
  )
  
  # compose and send email -----------------------------------------------------
  email_elements <- compose_email(body = md(glue(email_body))) |> 
    add_attachment(file = here("assignment/assignment_instructions.pdf")) |> 
    add_attachment(file = here("assignment/assignment_data.csv"))
  
  ################################
  print(student_db$email)
  ################################
  
  smtp_send(
    email_elements,
    subject = "MT5125 - Important - Assignment Instructions and Data - Confirm Reception",
    from = "damien.dupre@dcu.ie",
    to = student_db$email,
    credentials = creds_key("gmail")
  )
}


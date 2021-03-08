################################################################################
#                   Data Analytics Exam Random Generator                       #
################################################################################
# libraries --------------------------------------------------------------------
library(tidyverse)
library(mailR)
library(here)

# test emails ------------------------------------------------------------------
test_student_db <- data.frame(
  Firstname = c("Test 1", "Test 2"),
  Email = c("test_1@gmail.com", "test_1@gmail.com"),
  stringsAsFactors = FALSE
) # check in email sent box if they have been successfully sent 

# student emails ---------------------------------------------------------------
list_email_students <- file.choose() %>% 
  na.omit()

# data -------------------------------------------------------------------------
# https://mockaroo.com/
df <- here("data/random_data.csv") %>% 
  read_csv()

gmail_dcu <- config::get("gmail_dcu")

email_text <- "<p>Dear {name}, <br><br>Please find here attached the instructions and data regarding the Data Analytics Individual Assessment. Please follow the instructions attached and submit the report by Thursday May 7th, 2020 on Loop. Can you confirm the reception of the email?<br><br>These data are simulated according a specific code related to your email, therefore they are unique in order to prevent any copy-pasting between students.<br><br>Let me know if you have any problem to download the documents attached.<br><br>Best regards,<br><br>Damien</p>"

location <- c("Ireland", "France", "Australia")
job_title <- c("Data Analyst", "Software Engineer", "Administrative Officer", "Web Developer", "Support Technician")

letter2number <- function(x) {utf8ToInt(x) - utf8ToInt("a") + 1L}

################################################################################
for(row_student in 1:nrow(test_student_db)){
  
  student_db <- test_student_db[row_student, ]
  email <- student_db[["Email"]]
  name <- student_db[["Firstname"]]
  
  random_number <- as.numeric(abs(sum(letter2number(email))))
  set.seed(random_number)
  
  test <- df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      employee_code = stringi::stri_rand_strings(1, 5),
      location = sample(location, 1),
      job_title = sample(job_title, 1),
      age = sample(20:60, 1),
      base_salary_2019 = case_when(
        job_title == "Data Analyst" ~ 45404,
        job_title == "Software Engineer" ~ 43926,
        job_title == "Administrative Officer" ~ 32405,
        job_title == "Web Developer" ~ 49234,
        job_title == "Support Technician" ~ 30506
      ),
      salary_2019 = case_when(
        job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019, sd = 1000),
        job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019, sd = 2000),
        job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019, sd = 500),
        job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019, sd = 1000),
        job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019, sd = 1500)
      ),
      salary_2018 = case_when(
        job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019-1000, sd = 1000),
        job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019-2000, sd = 2000),
        job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019-500, sd = 500),
        job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019-1000, sd = 1000),
        job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019-1500, sd = 1500)
      ),
      salary_2017 = case_when(
        job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019-1000-500, sd = 1000),
        job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019-2000-500, sd = 2000),
        job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019-500-500, sd = 500),
        job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019-1000-500, sd = 1000),
        job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019-1500-500, sd = 1500)
      ),
      salary_2019 = ifelse(
        gender == "Female",
        salary_2019 - (salary_2019 * (sample(0:random_number, 1, replace = TRUE))/100), 
        salary_2019
      ),
      salary_2018 = ifelse(
        gender == "Female",
        salary_2018 - (salary_2018 * (sample(0:random_number, 1, replace = TRUE))/100), 
        salary_2018
      ),
      salary_2017 = ifelse(
        gender == "Female",
        salary_2017 - (salary_2017 * (sample(0:random_number, 1, replace = TRUE))/100), 
        salary_2017
      ),
      #salary_2019 = salary_2019 + salary_2019 * (age/100),
      salary_2017_2018 = salary_2018 - salary_2017,
      salary_2018_2019 = salary_2019 - salary_2018,
      q1 = ifelse(salary_2019 - base_salary_2019 > 0, sample(2:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
      q2 = ifelse(salary_2019 - base_salary_2019 > 0, sample(3:5, 1, replace = TRUE), sample(1:4, 1, replace = TRUE)),
      q3 = ifelse(salary_2019 - base_salary_2019 > 0, sample(4:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
      q4 = ifelse(salary_2019 - base_salary_2019 < 0, sample(3:5, 1, replace = TRUE), sample(1:4, 1, replace = TRUE)),
      q5 = ifelse(salary_2019 - base_salary_2019 < 0, sample(2:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
      q6 = ifelse(salary_2019 - base_salary_2019 > 0, sample(3:5, 1, replace = TRUE), sample(1:4, 1, replace = TRUE)),
      q7 = ifelse(salary_2019 - base_salary_2019 > 0, sample(4:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
      q8 = ifelse(salary_2019 - base_salary_2019 < 0, sample(3:5, 1, replace = TRUE), sample(1:4, 1, replace = TRUE)),
      q9 = ifelse(salary_2019 - base_salary_2019 < 0, sample(2:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
      q4_r = 6-q4,
      q5_r = 6-q5,
      q8_r = 6-q8,
      q9_r = 6-q9,
      js_score = (q1 + q2 + q3 + q4_r + q5_r + q6 + q7 + q8_r + q9_r)/9
    )
  
  test %>% 
    dplyr::select(employee_code, first_name, last_name, email, gender, age, ip_address, location, job_title) %>% 
    readr::write_csv("employee_details.csv")
  
  test %>% 
    dplyr::select(employee_code, salary_2019) %>% 
    readr::write_csv("employee_salary_2019.csv")
  
  test %>% 
    dplyr::select(employee_code, salary_2018) %>% 
    sample_n(900) %>% 
    readr::write_csv("employee_salary_2018.csv")
  
  test %>% 
    dplyr::select(employee_code, salary_2017) %>% 
    sample_n(700) %>% 
    readr::write_csv("employee_salary_2017.csv")
  
  test %>% 
    dplyr::select(employee_code, q1:q9) %>% 
    sample_n(950) %>% 
    readr::write_csv("employee_satisfaction.csv")
  ################################
  
  print(email)
  
  ################################
  # mailR::send.mail(
  #   from = gmail_dcu$email,
  #   to = email,
  #   subject = "MT5125 - Data Analytics Individual Assessment - Instructions and Data",
  #   body = glue::glue(email_text),
  #   attach.files = c(
  #     "C:/Users/dupred/OneDrive/Projects/employee_details.csv", 
  #     "C:/Users/dupred/OneDrive/Projects/employee_salary_2019.csv", 
  #     "C:/Users/dupred/OneDrive/Projects/employee_salary_2018.csv", 
  #     "C:/Users/dupred/OneDrive/Projects/employee_salary_2017.csv", 
  #     "C:/Users/dupred/OneDrive/Projects/employee_satisfaction.csv",
  #     "C:/Users/dupred/OneDrive/Projects/Individual Assessment Instructions.pdf"
  #   ),
  #   smtp = list(
  #     host.name = "smtp.gmail.com", port = 465, 
  #     user.name = gmail_dcu$email,            
  #     passwd = gmail_dcu$passwd, 
  #     ssl = TRUE
  #     ),
  #   authenticate = TRUE,
  #   send = TRUE,
  #   html = TRUE
  #   )
}
################################################################################

# random_number <- as.numeric(abs(sum(letter2number("damien.dupre@dcu.ie"))))
# set.seed(random_number)
# 
# test <- df %>% 
#   dplyr::rowwise() %>% 
#   dplyr::mutate(
#     employee_code = stringi::stri_rand_strings(1, 5),
#     location = sample(location, 1),
#     job_title = sample(job_title, 1),
#     age = sample(20:60, 1),
#     base_salary_2019 = case_when(
#       job_title == "Data Analyst" ~ 45404,
#       job_title == "Software Engineer" ~ 43926,
#       job_title == "Administrative Officer" ~ 32405,
#       job_title == "Web Developer" ~ 49234,
#       job_title == "Support Technician" ~ 30506
#     ),
#     salary_2019 = case_when(
#       job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019, sd = 1000),
#       job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019, sd = 2000),
#       job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019, sd = 500),
#       job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019, sd = 1000),
#       job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019, sd = 1500)
#     ),
#     salary_2018 = case_when(
#       job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019-1000, sd = 1000),
#       job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019-2000, sd = 2000),
#       job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019-500, sd = 500),
#       job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019-1000, sd = 1000),
#       job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019-1500, sd = 1500)
#     ),
#     salary_2017 = case_when(
#       job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019-1000-500, sd = 1000),
#       job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019-2000-500, sd = 2000),
#       job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019-500-500, sd = 500),
#       job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019-1000-500, sd = 1000),
#       job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019-1500-500, sd = 1500)
#     ),
#     salary_2019 = ifelse(
#       gender == "Female",
#       salary_2019 - (salary_2019 * (sample(0:random_number, 1, replace = TRUE))/100), 
#       salary_2019
#     ),
#     salary_2018 = ifelse(
#       gender == "Female",
#       salary_2018 - (salary_2018 * (sample(0:random_number, 1, replace = TRUE))/100), 
#       salary_2018
#     ),
#     salary_2017 = ifelse(
#       gender == "Female",
#       salary_2017 - (salary_2017 * (sample(0:random_number, 1, replace = TRUE))/100), 
#       salary_2017
#     ),
#     #salary_2019 = salary_2019 + salary_2019 * (age/100),
#     salary_2017_2018 = salary_2018 - salary_2017,
#     salary_2018_2019 = salary_2019 - salary_2018,
#     q1 = ifelse(salary_2019 - base_salary_2019 > 0, sample(2:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
#     q2 = ifelse(salary_2019 - base_salary_2019 > 0, sample(3:5, 1, replace = TRUE), sample(1:4, 1, replace = TRUE)),
#     q3 = ifelse(salary_2019 - base_salary_2019 > 0, sample(4:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
#     q4 = ifelse(salary_2019 - base_salary_2019 < 0, sample(3:5, 1, replace = TRUE), sample(1:4, 1, replace = TRUE)),
#     q5 = ifelse(salary_2019 - base_salary_2019 < 0, sample(2:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
#     q6 = ifelse(salary_2019 - base_salary_2019 > 0, sample(3:5, 1, replace = TRUE), sample(1:4, 1, replace = TRUE)),
#     q7 = ifelse(salary_2019 - base_salary_2019 > 0, sample(4:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
#     q8 = ifelse(salary_2019 - base_salary_2019 < 0, sample(3:5, 1, replace = TRUE), sample(1:4, 1, replace = TRUE)),
#     q9 = ifelse(salary_2019 - base_salary_2019 < 0, sample(2:5, 1, replace = TRUE), sample(1:3, 1, replace = TRUE)),
#     q4_r = 6-q4,
#     q5_r = 6-q5,
#     q8_r = 6-q8,
#     q9_r = 6-q9,
#     js_score = (q1 + q2 + q3 + q4_r + q5_r + q6 + q7 + q8_r + q9_r)/9
#   )

# # The Indiana job satisfaction scale
# # https://search.proquest.com/docview/204749117?pq-origsite=gscholar
# 
# ## General Satisfaction
# Q1 "I feel good about this job"
# Q2 "This job is worthwhile"
# Q3 "The working conditions are good"
# Q4 (R) "I want to quit this job"
# Q5 (R) "This job is boring"
# ## Pay
# Q6 "I am happy with the amount this job pays"
# Q7 "The vacation time and other benefits on this job are okay"
# Q8 (R) "I need more money than this job pays"
# Q9 (R) "This job does not provide the medical coverage I need"

# readr::write_csv(test, "df.csv")
# 
# test %>% 
#   dplyr::select(employee_code, first_name, last_name, email, gender, age, ip_address, location, job_title) %>% 
#   readr::write_csv("employee_details.csv")
# 
# test %>% 
#   dplyr::select(employee_code, salary_2019) %>% 
#   readr::write_csv("employee_salary_2019.csv")
# 
# test %>% 
#   dplyr::select(employee_code, salary_2018) %>% 
#   sample_n(900) %>% 
#   readr::write_csv("employee_salary_2018.csv")
# 
# test %>% 
#   dplyr::select(employee_code, salary_2017) %>% 
#   sample_n(700) %>% 
#   readr::write_csv("employee_salary_2017.csv")
# 
# test %>% 
#   dplyr::select(employee_code, q1:q9) %>% 
#   sample_n(950) %>% 
#   readr::write_csv("employee_satisfaction.csv")
# 
# utils::zip(
#   zipfile = "testZip.zip", 
#   files = c("C:/Users/dupred/Desktop/employee_details.csv", "C:/Users/dupred/Desktop/employee_salary_2019.csv", "C:/Users/dupred/Desktop/employee_salary_2018.csv", "C:/Users/dupred/Desktop/employee_salary_2017.csv", "C:/Users/dupred/Desktop/employee_satisfaction.csv")
#   )
# # Email to send
# email_text <- "<p>Dear owner/manager of '{name}', <br><br>We are contacting you because we would like to organise our wedding <b>Sunday 9 of June 2019</b> and your plac would be amazing for it.<br><br>That's why we would like to know if your venue '{name}' is available <b>Sunday 9 of June 2019</b>?</b><br><br>Best regards,<br><br>YOUR NAMES</p>"
# #
# for(i in 1:nrow(email_to_send)){
#   df <- email_to_send[i,]
#   name <- as.character(df$name)
#   ################################
#   send.mail(from = gmail_wedding$email,
#             to = as.character(df$email),
#             subject = "Availability for a wedding on the 09/06/2019",
#             body = glue::glue(email_text),
#             smtp = list(host.name = "smtp.gmail.com", port = 465, 
#                         user.name = gmail_wedding$email,            
#                         passwd = gmail_wedding$passwd, ssl = TRUE),
#             authenticate = TRUE,
#             send = TRUE,
#             html = TRUE)
# }
# 
# summary(aov(lm(salary_2019 ~ gender*job_title*age, data = test)))
# 
# summary(aov(lm(js_score ~ gender*job_title*age, data = test)))
# 
# summary(aov(lm(js_score ~ salary_2019*gender*job_title*age, data = test)))

# location <- c("Ireland", "France", "Australia")
# job_title <- c("Data Analyst", "Software Engineer", "Administrative Officer", "Web Developer", "Support Technician")
# skew_center <- c(0.1, 0.2, 0.4, 0.2, 0.1)
# skew_left <- c(0.2, 0.4, 0.2, 0.1, 0.1)
# skew_right <- c(0.1, 0.1, 0.2, 0.4, 0.2)
# 
# 
# test2 <- df
# test2$employee_code <- stringi::stri_rand_strings(1000, 5)
# test2$location <- sample(location, 1000, replace = TRUE)
# test2$job_title <- sample(job_title, 1000, replace = TRUE)
# test2$age <- sample(20:60, 1000, replace = TRUE)
# 
# test2 <- test2 %>% 
#   dplyr::rowwise() %>% 
#   dplyr::mutate(
#     base_salary_2019 = case_when(
#       job_title == "Data Analyst" ~ 45404,
#       job_title == "Software Engineer" ~ 43926,
#       job_title == "Administrative Officer" ~ 32405,
#       job_title == "Web Developer" ~ 49234,
#       job_title == "Support Technician" ~ 30506
#     ),
#     salary_2019 = case_when(
#       job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019, sd = 1000),
#       job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019, sd = 2000),
#       job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019, sd = 500),
#       job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019, sd = 1000),
#       job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019, sd = 1500)
#     ),
#     salary_2019 = ifelse(
#       gender == "Female",
#       salary_2019 - (salary_2019 * (sample(0:random_number, 1, replace = TRUE))/100), 
#       salary_2019
#     ),
#     #salary_2019 = salary_2019 + salary_2019 * (age/100),
#     salary_2018 = case_when(
#       job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019-1000, sd = 1000),
#       job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019-2000, sd = 2000),
#       job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019-500, sd = 500),
#       job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019-1000, sd = 1000),
#       job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019-1500, sd = 1500)
#     ),
#     salary_2017 = case_when(
#       job_title == "Data Analyst" ~ rnorm(1, mean = base_salary_2019-1000-500, sd = 1000),
#       job_title == "Software Engineer" ~ rnorm(1, mean = base_salary_2019-2000-500, sd = 2000),
#       job_title == "Administrative Officer" ~ rnorm(1, mean = base_salary_2019-500-500, sd = 500),
#       job_title == "Web Developer" ~ rnorm(1, mean = base_salary_2019-1000-500, sd = 1000),
#       job_title == "Support Technician" ~ rnorm(1, mean = base_salary_2019-1500-500, sd = 1500)
#     ),
#     salary_2017_2018 = salary_2018 - salary_2017,
#     salary_2018_2019 = salary_2019 - salary_2018,
#     q1 = ifelse(salary_2019 - base_salary_2019 > 0, sample(1:5, 1, replace = TRUE, prob = skew_right), sample(1:5, 1, replace = TRUE, prob = skew_left)),
#     q2 = ifelse(salary_2019 - base_salary_2019 > 0, sample(1:5, 1, replace = TRUE, prob = skew_right), sample(1:5, 1, replace = TRUE, prob = skew_left)),
#     q3 = ifelse(salary_2019 - base_salary_2019 > 0, sample(1:5, 1, replace = TRUE, prob = skew_right), sample(1:5, 1, replace = TRUE, prob = skew_left)),
#     q4 = ifelse(salary_2019 - base_salary_2019 < 0, sample(1:5, 1, replace = TRUE, prob = skew_left), sample(1:5, 1, replace = TRUE, prob = skew_right)),
#     q5 = ifelse(salary_2019 - base_salary_2019 < 0, sample(1:5, 1, replace = TRUE, prob = skew_left), sample(1:5, 1, replace = TRUE, prob = skew_right)),
#     q6 = ifelse(salary_2019 - base_salary_2019 > 0, sample(1:5, 1, replace = TRUE, prob = skew_right), sample(1:5, 1, replace = TRUE, prob = skew_left)),
#     q7 = ifelse(salary_2019 - base_salary_2019 > 0, sample(1:5, 1, replace = TRUE, prob = skew_right), sample(1:5, 1, replace = TRUE, prob = skew_left)),
#     q8 = ifelse(salary_2019 - base_salary_2019 < 0, sample(1:5, 1, replace = TRUE, prob = skew_left), sample(1:5, 1, replace = TRUE, prob = skew_right)),
#     q9 = ifelse(salary_2019 - base_salary_2019 < 0, sample(1:5, 1, replace = TRUE, prob = skew_left), sample(1:5, 1, replace = TRUE, prob = skew_right)),
#     q4_r = 6-q4,
#     q5_r = 6-q5,
#     q8_r = 6-q8,
#     q9_r = 6-q9,
#     js_score = (q1 + q2 + q3 + q4_r + q5_r + q6 + q7 + q8_r + q9_r)/9
#   )
# 
# test2%>% 
#   ggplot(aes(js_score, salary_2019, color = gender)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# test2 %>% 
#   ggplot(aes(js_score, salary_2019, color = job_title)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# test2 %>% 
#   ggplot(aes(js_score, salary_2019, color = age)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# 
# test2 %>% 
#   ggplot(aes(js_score, salary_2018_2019, color = gender)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# test2 %>% 
#   ggplot(aes(js_score, age, color = gender)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# test2 %>% 
#   ggplot(aes(salary_2019, age, color = gender)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# summary(aov(lm(salary_2019 ~ gender*job_title*age, data = test2)))
# 
# summary(aov(lm(js_score ~ gender*job_title*age, data = test2)))
# 
# summary(aov(lm(js_score ~ salary_2019*gender*job_title*age, data = test2)))

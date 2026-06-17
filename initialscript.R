library(tidyverse)
library(janitor)

data <- read.csv("lboro.csv") %>% 
  clean_names()

active_users <- data %>% 
  filter(account_status == "active")

active_users <- active_users %>%
  mutate(school = case_when(
    group %in% c(
      "Aeronautical and Automotive Engineering",
      "Chemical Engineering",
      "Materials",
      "Aeronautical, Automotive, Chemical and Materials Engineering"
    ) ~ "AACME",
    
    group %in% c(
      "Mechanical, Electrical and Manufacturing Engineering"
    ) ~ "Wolfson",
    
    group %in% c(
      "Design",
      "Creative Arts"
    ) ~ "DCA",
    
    group %in% c(
      "Architecture, Building and Civil Engineering"
    ) ~ "ABCE",
    
    group %in% c(
      "Physics",
      "Mathematical Sciences",
      "Mathematics Education",
      "Computer Science",
      "Chemistry",
      "Science"
    ) ~ "Science",
    
    group %in% c(
      "Geography and Environment",
      "Criminology, Sociology and Social Policy",
      "International Relations, Politics and History",
      "Language Centre",
      "Communication and Media",
      "English",
      "Social Sciences and Humanities",
      "Loughborough Law"
    ) ~ "SSH",
    
    group == "Sport, Exercise and Health Sciences" ~ "SEHS",
    group == "Loughborough Business School"        ~ "Business School",
    group == "Loughborough University London"      ~ "Loughborough London",
    
    TRUE ~ NA_character_  # ignored groups become NA
  ))

user_counts <- active_users %>%
  filter(!is.na(school)) %>% 
  count(school, name = "n_users")


owned_counts <- active_users %>%
  filter(!is.na(school), public_items_owned_number_of_owned_items > 1) %>%
  count(school, name = "n_users") %>%
  arrange(desc(n_users))

owned_counts <- user_counts %>%
  left_join(
    active_users %>%
      filter(!is.na(school), public_items_owned_number_of_owned_items > 1) %>%
      count(school, name = "n_owned"),
    by = "school"
  ) %>%
  mutate(
    n_owned = replace_na(n_owned, 0),
    pct_owned = round(n_owned / n_users * 100, 1)
  ) %>%
  arrange(desc(pct_owned))


# Count by school
school_10plus <- active_users %>%
  filter(!is.na(school), public_items_owned_number_of_owned_items >= 10) %>%
  count(school, name = "n_users") %>%
  arrange(desc(n_users))

# Individual users
users_10plus <- active_users %>%
  filter(!is.na(school), public_items_owned_number_of_owned_items >= 10) %>%
  select(school, author_name, public_items_owned_number_of_owned_items) %>%
  arrange(school, desc(public_items_owned_number_of_owned_items))

library(shiny)
library(tidyverse)
library(janitor)
library(DT)

# ── School mapping ────────────────────────────────────────────────────────────
assign_school <- function(group) {
  case_when(
    group %in% c(
      "Aeronautical and Automotive Engineering",
      "Chemical Engineering",
      "Materials",
      "Aeronautical, Automotive, Chemical and Materials Engineering"
    ) ~ "AACME",
    
    group %in% c(
      "Mechanical, Electrical and Manufacturing Engineering"
    ) ~ "Wolfson",
    
    group %in% c("Design", "Creative Arts") ~ "DCA",
    
    group %in% c(
      "Architecture, Building and Civil Engineering"
    ) ~ "ABCE",
    
    group %in% c(
      "Physics", "Mathematical Sciences", "Mathematics Education",
      "Computer Science", "Chemistry", "Science"
    ) ~ "Science",
    
    group %in% c(
      "Geography and Environment",
      "Criminology, Sociology and Social Policy",
      "International Relations, Politics and History",
      "Language Centre", "Communication and Media",
      "English", "Social Sciences and Humanities",
      "Loughborough Law"
    ) ~ "SSH",
    
    group == "Sport, Exercise and Health Sciences" ~ "SEHS",
    group == "Loughborough Business School"        ~ "Business School",
    group == "Loughborough University London"      ~ "Loughborough London",
    
    TRUE ~ NA_character_
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Reactive: processed data ──────────────────────────────────────────────
  active_users <- reactive({
    req(input$csv)
    read.csv(input$csv$datapath) %>%
      clean_names() %>%
      filter(account_status == "active") %>%
      mutate(school = assign_school(group))
  })
  
  owned_counts <- reactive({
    req(active_users())
    user_counts <- active_users() %>%
      filter(!is.na(school)) %>%
      count(school, name = "n_users")
    
    user_counts %>%
      left_join(
        active_users() %>%
          filter(!is.na(school), public_items_owned_number_of_owned_items > 1) %>%
          count(school, name = "n_owned"),
        by = "school"
      ) %>%
      mutate(
        n_owned   = replace_na(n_owned, 0),
        pct_owned = round(n_owned / n_users * 100, 1)
      ) %>%
      arrange(desc(pct_owned))
  })
  
  school_10plus <- reactive({
    req(active_users())
    active_users() %>%
      filter(!is.na(school), public_items_owned_number_of_owned_items >= 10) %>%
      count(school, name = "n_users") %>%
      arrange(desc(n_users))
  })
  
  users_10plus <- reactive({
    req(active_users())
    active_users() %>%
      filter(!is.na(school), public_items_owned_number_of_owned_items >= 10) %>%
      select(school, author_name, public_items_owned_number_of_owned_items) %>%
      arrange(school, desc(public_items_owned_number_of_owned_items))
  })
  
  # ── Render tabs only after upload ─────────────────────────────────────────
  output$tabs_ui <- renderUI({
    req(input$csv)
    tabsetPanel(
      tabPanel("Owned items (>1)",
               br(),
               DTOutput("tbl_owned_counts")
      ),
      tabPanel("Power users (≥10 items) — by school",
               br(),
               DTOutput("tbl_school_10plus")
      ),
      tabPanel("Power users (≥10 items) — individuals",
               br(),
               DTOutput("tbl_users_10plus")
      )
    )
  })
  
  # ── Tables ────────────────────────────────────────────────────────────────
  output$tbl_owned_counts <- renderDT({
    owned_counts() %>%
      rename(School = school, `Total users` = n_users,
             `Users with >1 item` = n_owned, `% with >1 item` = pct_owned) %>%
      datatable(rownames = FALSE, options = list(pageLength = 15))
  })
  
  output$tbl_school_10plus <- renderDT({
    school_10plus() %>%
      rename(School = school, `Users with ≥10 items` = n_users) %>%
      datatable(rownames = FALSE, options = list(pageLength = 15))
  })
  
  output$tbl_users_10plus <- renderDT({
    users_10plus() %>%
      rename(School = school, Name = author_name,
             `Public items owned` = public_items_owned_number_of_owned_items) %>%
      datatable(rownames = FALSE, options = list(pageLength = 25))
  })
}
library(shiny)
library(DT)
library(bslib)

ui <- fluidPage(
  titlePanel("Repository User Analysis"),
  
  theme = bs_theme(
    version      = 5,
    bg           = "#e8eaf0",
    fg           = "#0f1117",
    primary      = "#8d9c27",
    secondary    = "#361163",
    base_font    = font_google("IBM Plex Mono"),
    heading_font = font_google("Space Mono"),
    font_scale   = 0.92
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("csv", "Upload CSV", accept = ".csv"),
      hr(),
      p("Once uploaded, use the tabs to explore the data.")
    ),
    
    mainPanel(
      uiOutput("tabs_ui")
    )
  )
)
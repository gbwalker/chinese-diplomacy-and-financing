#
# Description of app here
#

library(shiny)
library(tidyverse)
library(rsconnect)
library(DT)
library(timevis)

data <- data.frame(
  id      = 1:4,
  content = c("Item one", "Item two",
              "Ranged item", "Item four"),
  start   = c("2016-01-10", "2016-01-11",
              "2016-01-20", "2016-02-14 15:00:00"),
  end     = c(NA, NA, "2016-02-04", NA)
)

# Read in the necessary data
aiddf <- read_rds("aiddf.rds")
investdf <- read_rds("investdf.rds")
leaderdf <- read_rds("leaderdf.rds")
countries <- read_rds("countries.rds")

# script for running as a project, not as an app
# aiddf <- read_rds("data/aiddf.rds")
# investdf <- read_rds("data/investdf.rds")
# leaderdf <- read_rds("data/leaderdf.rds")
# countries_all <- read_rds("data/countries.rds")

### Selection variables

# Make selection lists for the AidData data: sector, countries, flow, flow_class, intent
# unique(aiddf$sector)
# unique(aiddf$country)
# unique(aiddf$flow)
# unique(aiddf$flow_class)
# unique(aiddf$intent)

# Make selection lists for investdf
# unique(investdf$country)
# unique(investdf$sector)
# unique(investdf$quantity_in_millions)

# Make a list of leaders for users to select
leaders <- unique(leaderdf$leader)


# Define UI for application
ui <- navbarPage("Elite Chinese Diplomacy and Financial Flows",

### First tab
  tabPanel("Investment", fluidPage(
    
    # Page title
    titlePanel("Investment"),
    
    # Sidebar
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(inputId = "country", 
                    label = "Select a country:",
                    choices = countries,
                    selected = "United States"),
        helpText("Test")
      ),
    
    # Main panel
    mainPanel(timevisOutput("engagements"),
              hr(),
              div(dataTableOutput("table1", width = "100%"), style = "font-size:80%"),
              div(dataTableOutput("table2", width = "100%"), style = "font-size:80%")
              )
    ))), # Three parentheses for tabPanel, fluidPage, and sidebarLayout
  
### Second tab
  tabPanel("Aid", fluidPage(
    # Page title
    titlePanel("Aid"),
    
    # Sidebar
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput(inputId = "country", 
                    label = "Select a country:",
                    choices = countries,
                    selected = "United States"),
        hr(),
        helpText("Test")
      ),
      
      # Main panel
      mainPanel(plotOutput("Plot"))
    ))) # Three parentheses for tabPanel, fluidPage, and sidebarLayout
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Make the timeline
  output$engagements <- renderTimevis({
    timevis(groups = data_frame(
      id = leaders,
      content = leaders
    )) %>% 
      addItems(data = data_frame(
        start = leaderdf$date,
        content = leaderdf$country,
        group = leaderdf$leader))
      
})
  
  output$table1 = renderDataTable({
    datatable(investdf, options=list(pageLength = 10)) %>% 
      formatStyle(columns = TRUE, target= "row", lineHeight="90%")
  })

  output$table2 = renderDataTable({
    datatable(leaderdf, options=list(pageLength = 10)) %>% 
      formatStyle(columns = TRUE, target= "row")  })
}

# Run the application 
shinyApp(ui = ui, server = server)


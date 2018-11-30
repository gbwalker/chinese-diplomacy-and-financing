#
# Description of app here
#

library(shiny)
library(tidyverse)
library(rsconnect)

# Read in the necessary data
aiddf <- read_rds("aiddf.rds")
investdf <- read_rds("investdf.rds")
leaderdf <- read_rds("leaderdf.rds")
countries <- read_rds("countries.rds")


# Define UI for application that draws a histogram
ui <- navbarPage("Elite Chinese Diplomacy and Financial Flows",
    
  # First tab
  tabPanel("Tab I", fluidPage(
    # Page title
    titlePanel("Tab I")
    
  )),
  
  # Second tab
  tabPanel("Tab II", fluidPage(
    # Page title
    titlePanel("Tab II")
    
  )),
   
  # Third tab
  tabPanel("Tab III", fluidPage(
    # Page title
    titlePanel("Tab III")
    
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)


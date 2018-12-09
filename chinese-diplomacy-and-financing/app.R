#
# Description of app here
#

library(shiny)
library(tidyverse)
library(rsconnect)
library(DT)
library(timevis)
library(htmlwidgets)
library(plotly)
library(stringr)
library(shinyWidgets)
library(leaflet)
library(lubridate)

##################
# Read in the data
##################

# Read in the necessary data
aiddf <- read_rds("aiddf.rds")
investdf <- read_rds("investdf.rds")
leaderdf <- read_rds("leaderdf.rds")

# Make a list of all the countries China invests in
invest_countries <- unique(investdf$country) %>%
  sort()

# Make a list of all the countries China has provided aid to, ignoring multiple country listings
aid_countries <- aiddf %>% 
  filter(! str_detect(aiddf$country, ";")) %>% 
  filter(! is.na(longitude))

aid_countries <- unique(aid_countries$country) %>% 
  sort()

# Make a selection list of all the sectors for which China has provided aid
aid_sectors <- unique(aiddf$sector)

# Make a list of all countries between the three data sets
countries <- as.list(read_rds("countries.rds"))

# Make a list of the four leaders
leaders <- unique(leaderdf$leader)

# script for running as a project, not as an app
# aiddf <- read_rds("data/aiddf.rds")
# investdf <- read_rds("data/investdf.rds")
# leaderdf <- read_rds("data/leaderdf.rds")
# countries <- read_rds("data/countries.rds")


##################
### USER INTERFACE
##################
ui <- navbarPage("Elite Chinese Diplomacy and Financial Flows", position = "fixed-top",

### First tab for aid
  tabPanel("Global Aid", 
    div(class = "outer",
    # Include custom CSS
    tags$head(includeCSS("styles.css"))
        ),
  
  # Include the map
  leafletOutput("aidmap", width = "100%", height = "650px"),
  
  # Include the selection panel
  absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = FALSE, top = 60, right = "auto",
                left = 40, bottom = "auto", height = "auto", width = 330,
                selectInput(inputId = "country2", 
                            label = HTML("<b>Select a country:</b>"),
                            choices = aid_countries,
                            selected = "Sri Lanka",
                            selectize = TRUE),
                helpText(textOutput("aid_amount")),
                helpText(textOutput("aid_number")),
                helpText(textOutput("aid_common")),
                selectInput(inputId = "sector",
                            label = HTML("<b>Choose sectors:</b>"),
                            choices = "All",
                            selected = "All",
                            multiple = FALSE),
                tags$div(title = HTML("2013- : Xi President; Li Premier\n2008-13: Xi VP; Li Vice Premier\n2003-13: Hu President; Wen Premier\n1998-03: Hu VP; Wen Vice Premier"),                
                         awesomeCheckboxGroup(inputId = "leaders2",
                                              label = HTML("<b>Choose leaders:</b>"),
                                              choices = c(leaders),
                                              selected = c("Xi Jinping", "Hu Jintao"),
                                              inline = TRUE)
                        ),
                helpText(textOutput("engagements_total2")),
                plotOutput("mini_graph", height = "230px", width = "275px")
              ),
  hr(),
  timevisOutput("engagements2", width = "100%"),
  hr(),
  h3(textOutput("aid_title")),
  div(dataTableOutput("aidtable", width = "100%"), style = "font-size:80%"),
  hr(),
  h3(textOutput("engagement_title")),
  div(dataTableOutput("leadertable2", width = "100%"), style = "font-size:80%")
  ),

### Second tab
tabPanel("Global Investments", 
         div(class = "outer",
             # Include custom CSS
             tags$head(includeCSS("styles.css"))
         ),
  
  # Page title
  # titlePanel("Global Investments (2005-2018)"),
    
  # Panel
  absolutePanel(id = "invest_controls", class = "panel panel-default", fixed = FALSE, draggable = FALSE, top = 60, right = "auto",
                left = 80, bottom = "auto", height = "auto", width = 290,
                style = "border: none;",
                selectInput(inputId = "country", 
                            label = HTML("<b>Select a country:</b>"),
                            choices = invest_countries,
                            selected = "United States",
                            selectize = TRUE),
                helpText(textOutput("invest_amount")),
                helpText(textOutput("invest_number")),
                helpText(textOutput("invest_common")),
                helpText(textOutput("invest_value")),
                tags$div(title = HTML("2013- : Xi President; Li Premier\n2008-13: Xi VP; Li Vice Premier\n2003-13: Hu President; Wen Premier\n1998-03: Hu VP; Wen Vice Premier"),                
                awesomeCheckboxGroup(inputId = "leaders",
                                     label = HTML("<b>Choose leaders:</b>"),
                                     choices = c(leaders),
                                     selected = c("Xi Jinping", "Hu Jintao"),
                                     inline = TRUE)
                        ),
                helpText(textOutput("engagements_total")),
                plotOutput("mini_graph2", height = "230px", width = "275px")
  ),
  
  plotlyOutput("investPlot", width = "95%", height = "600px", inline = TRUE),
  timevisOutput("engagements"),
  hr(),
  h3(textOutput("invest_title")),
  div(dataTableOutput("investtable", width = "100%"), style = "font-size:80%"),
  h3(textOutput("engagement_title2")),
  div(dataTableOutput("leadertable", width = "100%"), style = "font-size:80%")
  )
)

##########
### SERVER
##########

server <- function(input, output, session) {

#########
# AID TAB
#########
  
### Get some summary stats from the selected data
  
  # Find the total aid amount
  output$aid_amount <- renderText({
    amount <- aiddf %>%
      filter(str_detect(country, input$country2)) %>% 
      filter(! is.na(usd_current))
    
    # Filter by sector if applicable
    if (input$sector != "All") {
      amount <- amount %>% 
        filter(sector == input$sector)
    }
    
    amount <- sum(amount$usd_current)
    paste0("Recorded aid amount: $", formatC(amount, format = "f", digits = 0, big.mark = ","))
  })
  
  # Find the number of projects for the given country and sector
  output$aid_number <- renderText({
    
    number <- aiddf %>%
      filter(str_detect(country, input$country2))
    
    # Filter by sector if applicable
    if (input$sector != "All") {
      number <- number %>% 
        filter(sector == input$sector)
    }
  
    number <- tally(number)
    paste0("Recorded projects: ", number)
  })
  
  # Find the most common sector
  output$aid_common <- renderText({
    common <- aiddf %>%
      filter(str_detect(country, input$country2))
    
    common <- count(common, sector) %>% 
      arrange(desc(n))
    
    # Check if there are two top sectors
    ifelse(common$n[1] != common$n[2], top <- common$sector[1], top <- common$sector[1:2])
    
    # Fill in a dummy if there's only one sector
    if (length(common$sector) == 1) {
      top <- common$sector[1]
    }
    
    # Return different values if there's one or two top sectors
    ifelse(length(top) == 1, paste0("Most common sector: ", top), paste0("Most common sectors: ", top[1], " and ", top[2]))
  })
  
  # Find the total number of engagements
  output$engagements_total2 <- renderText({
    total <- leaderdf %>% 
      filter(str_detect(country, input$country2)) %>% 
      filter(leader %in% input$leaders2) %>% 
      count()
    
    paste0("Engagements: ", total)
  })
  
  
  # Draw a baby plot on the sidebar
  output$mini_graph <- renderPlot({
    
    # Make a dummy df to catch exceptions where there are no engagements
    mini_leader <- data.frame(date = NA)
    
    # Use selected country and leader(s)
    mini_leader <- leaderdf %>% 
      filter(country == input$country2) %>% 
      filter(leader %in% input$leaders2) %>% 
    
    # Group years by year ranges
      mutate(year = as.double(year)) %>% 
      mutate(bucket = case_when(
        year <= 2002 ~ "2000-02",
        year >= 2003 & year <= 2007 ~ "2003-07",
        year >= 2008 & year <= 2012 ~ "2008-12",
        year >= 2013 ~ "2013-18"
      ))

    # Do the same for aid
    mini_aid <- aiddf %>% 
      filter(country == input$country2) %>% 
    
    # Make a bucket variables
      mutate(bucket = case_when(
        year <= 2002 ~ "2000-02",
        year >= 2003 & year <= 2007 ~ "2003-07",
        year >= 2008 & year <= 2012 ~ "2008-12",
        year >= 2013 ~ "2013-18"
      ))
      
    # Make counts for both groups
    mini <- count(mini_leader, bucket)
    mini2 <- count(mini_aid, bucket)
    
    # Join the tiny tibbles to graph both together
    mini_all <- mini %>%
      full_join(mini2, by = "bucket", suffix = c(".engagements", ".aid"))
    
    # Make the NAs zeros
    mini_all[is.na(mini_all)] <- 0
    
    # Alter the data so that it records investments and engagements per YEAR
    mini_all <- mini_all %>% 
      mutate(n.engagements = case_when(
        bucket == "2000-02" ~ n.engagements / 3,
        bucket == "2003-07" ~ n.engagements / 5,
        bucket == "2008-12" ~ n.engagements / 5,
        bucket == "2013-18" ~ n.engagements / 6)
        ) %>% 
      mutate(n.aid = case_when(
        bucket == "2000-02" ~ n.aid / 3, # Aid projects start in 2000
        bucket == "2003-07" ~ n.aid / 5,
        bucket == "2008-12" ~ n.aid / 5,
        bucket == "2013-18" ~ n.aid / 2)) # Since observations only cover 2013-2104
    
    # Make the mini graph
    ggplot(mini_all, aes(x = bucket, group = 1)) + 
      geom_point(aes(y = n.engagements), size = 0, col = "#0F52BA") +
      geom_line(aes(y = n.engagements), col = "#0F52BA") +
      geom_point(aes(y = n.aid), size = 0, col = "#ED2939") + # This is red
      geom_line(aes(y = n.aid), col = "#ED2939") +
      scale_y_continuous(
        sec.axis = sec_axis(~.*1,
                            name = "Aid Projects per Year (#)",
                            breaks = waiver())) +
      labs(x = "Date", y = "Engagements per Year") +
      theme(axis.title.y = element_text(color = "#0F52BA"),
            axis.title.y.right = element_text(color = "#ED2939"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
  })
  
  
  ### Draw the map
  output$aidmap <- renderLeaflet({
    
    # Initiate the map
    leaflet(filteredMapData, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })

# Every time the user chooses a new country, redo the map view
  observe({
    map_projects <- filteredMapData() %>% 
      filter(str_detect(country, input$country2))
    
    leafletProxy("aidmap", data = map_projects) %>%
      setView(map_projects$longitude[1], map_projects$latitude[1], zoom = 7) 
  })

# Every time the user chooses a new sector, rewrite the markers
  observe({
    ifelse(input$sector == "All", 
      {map_projects <- filteredMapData()},
      {map_projects <- filteredMapData() %>% 
        filter(sector == input$sector)})
  
    leafletProxy("aidmap", data = map_projects) %>%
      clearMarkers() %>% 
      clearMarkerClusters() %>% 
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       radius = ~ifelse(is.na(usd_current), 6, log(usd_current)),
                       color = "#00A86B",
                       opacity = 0.5,
                       fillOpacity = 0.25,
                       clusterOptions = markerClusterOptions(freezeAtZoom = 12,
                                                             spiderfyOnMaxZoom = TRUE,
                                                             removeOutsideVisibleBounds = TRUE,
                                                             spiderfyDistanceMultiplier = 2,
                                                             spiderLegPolylineOptions = list(weight = .75)),
                       popup = paste0("<b>Funder: </b>", map_projects$funder, "<br>",
                                      "<b>Sector: </b>", map_projects$sector, "<br>",
                                      "<b>Description: </b>", map_projects$title, "<br>",
                                      "<b>Amount: </b>", ifelse(is.na(map_projects$usd_current),
                                                                "Unknown",
                                                                paste0("$", formatC(map_projects$usd_current,
                                                                                    format = "f",
                                                                                    digits = 0,
                                                                                    big.mark = ","))), "<br>",
                                      "<b>Intent: </b>", map_projects$intent, "<br>",
                                      "<b>Locale: </b>", str_trim(map_projects$place), ", ",
                                      map_projects$country, ", ",
                                      map_projects$year))
    
  })

  ### Make a custom list of sectors for each country
  observe({
    data1 <- filteredMapData() %>% 
      filter(str_detect(country, input$country2))
    
    updateSelectInput(session, "sector", choices = c("All", unique(data1$sector)), selected = "All")
  })
  
  # Validate the check boxes so at least one is checked
  # observe({
  #   if(is.null(input$leaders2))
  #     updateAwesomeCheckboxGroup(session, "leaders2", selected = "Xi Jinping")
  # })

  
  # Get the name of the current country for the table titles
  output$aid_title <- renderText(paste0("Aid to ", input$country2))
  output$engagement_title <- renderText(paste0("Engagements With ", input$country2))
  
  filteredMapData <- reactive({
    # Select data only for one project at a time
    map_projects <- aiddf %>% 
      filter(! is.na(latitude)) %>% 
      filter(! str_detect(country, ";"))
    
    # Remove duplicated projects so the markers don't overlap
    map_projects[! duplicated(map_projects$id), ]
  })
  
  
  ### Make the timeline
  output$engagements2 <- renderTimevis({
    
    # Select data only for the country chosen
    data1 <- leaderdf %>% 
      filter(country == input$country2)
    
    # Filter for chosen leaders. If empty, only return Xi Jinping.
    if (length(input$leaders2) == 0) {
      data1 <- data.frame(date = NA, year = NA, country = input$country2, leader = NA, text = NA)
      names <- " "
    }
    else {
      data1 <- data1 %>% 
        filter(leader %in% input$leaders2)
      names <- input$leaders2
    }
    
    # Make a boolean display category for "high" or "low" engagements to make sure the timeline adjusts the view properly
    n <- data1 %>%
      count() %>% 
      as.double()
    high <- case_when(
      n > 25 ~ TRUE,
      n <= 25 ~ FALSE)
    
    ### Timeline
    timevis(showZoom = FALSE, fit = TRUE, height = "350pt",
            
            # Set options in JS for zoom limits; zoomMax is in miliseconds
            options = list(max = "2020-01-01",
                           min = "2000-01-01",
                           zoomMax = case_when(
                             high ~ 31540000000*2,
                             high ~ 567720000000)
            ),
            groups = data_frame(id = names, content = names)) %>% 
      
      # Add the data to the timeline with a smaller font size
      # Set the default view to the most recent engagement and six months before (in miliseconds)
      addItems(data = data_frame(
        start = data1$date,
        content = paste0(substr(data1$text, 1, 100), "..."),
        group = data1$leader,
        style = "font-size: 10px",
        type = "box",
        title = paste0(data1$text, " (", data1$date, ")"))) %>% 
      
      # Set the default window differently depending on whether the number of engagements is high or low
      setWindow(ifelse(high, max(data1$date) - 100, min(data1$date) - 1500), max(data1$date) + 100)
  })

### Make the aid data table
  output$aidtable = renderDataTable({
    
    # Select data only for the country chosen
    data1 <- aiddf %>% 
      filter(str_detect(country, input$country2)) %>% 
      select(-id, -latitude, -longitude)
    
    # Filter by sector if applicable
    if (input$sector != "All") {
      data1 <- data1 %>% 
        filter(sector == input$sector)
    }
    
    # Data table
    datatable(data1, options=list(pageLength = 10),
              colnames = c("Year", "Country", "Place", "Sector", "Description", "Funder", "Flow", "Flow Class", "Intention", "Amount ($)")) %>% 
      formatCurrency(columns = "usd_current", currency = "", interval = 3, mark = ",", digits = 0)
    })
  
### Make the leader engagements data table
  output$leadertable2 = renderDataTable({
    
    # Select data only for the country chosen
    data1 <- leaderdf %>% 
      filter(country == input$country2) %>% 
      filter(leader %in% input$leaders2) %>% 
      select(-year, -country)
    
    # Data table
    datatable(data1, options=list(pageLength = 10),
              colnames = c("Date", "Leader", "Description"))
    })

  
################
# INVESTMENTS TAB
################
  
  # Find the total investment amount
  output$invest_amount <- renderText({
    amount <- investdf %>%
      filter(country == input$country) %>% 
      filter(! is.na(quantity_in_millions))
    
    amount <- sum(amount$quantity_in_millions)
    paste0("Total investment: $", formatC(amount, format = "f", digits = 0, big.mark = ","), ",000,000")
  })
  
  # Find the number of investments the given country
  output$invest_number <- renderText({
    number <- investdf %>%
      filter(country == input$country)
    
    number <- tally(number)
    paste0("Number of investments: ", number)
  })

  
  # Find the most common sector
  output$invest_common <- renderText({
    common <- investdf %>%
      filter(str_detect(country, input$country))
    
    common <- count(common, sector) %>% 
      arrange(desc(n))
    
    # Check if there are two top sectors
    ifelse(common$n[1] != common$n[2], top <- common$sector[1], top <- common$sector[1:2])
    
    # Fill in a dummy if there's only one sector
    if (length(common$sector) == 1) {
      top <- common$sector[1]
    }
    
    # Return different values if there's one or two top sectors
    ifelse(length(top) == 1, paste0("Most common sector: ", top), paste0("Most common sectors: ", top[1], " and ", top[2]))
  })
  
  # Find the most highly valued sector
  output$invest_value <- renderText({
    value <- investdf %>%
      filter(str_detect(country, input$country)) %>% 
      group_by(sector) %>% 
      tally(quantity_in_millions) %>% 
      arrange(desc(n))
    
    # Return different values if there's one or two top sectors
    paste0("Highest value sector: ", value$sector[1], " ($", formatC(value$n[1]/1000, digits = 2, format = "f"), " bil.)")
  })
  
  # Find the total number of engagements
  output$engagements_total <- renderText({
    total <- leaderdf %>% 
      filter(str_detect(country, input$country)) %>% 
      filter(leader %in% input$leaders) %>% 
      count()
    
    paste0("Engagements: ", total)
  })
  
  # Draw a baby plot on the sidebar to show investment number and engagement number
  output$mini_graph2 <- renderPlot({
    
    # Make a dummy df to catch exceptions where there are no engagements
    mini_leader <- data.frame(date = NA)
    
    # Use selected country and leader(s)
    mini_leader <- leaderdf %>% 
      filter(country == input$country) %>% 
      filter(leader %in% input$leaders) %>% 
      
      # Group years by year ranges
      mutate(year = as.double(year)) %>% 
      mutate(bucket = case_when(
        year <= 2002 ~ "2000-02",
        year >= 2003 & year <= 2007 ~ "2003-07",
        year >= 2008 & year <= 2012 ~ "2008-12",
        year >= 2013 ~ "2013-18"
      ))
    
    # Do the same for investments
    mini_invest <- investdf %>% 
      filter(country == input$country) %>% 
    
    # Make a bucket variable
      mutate(bucket = case_when(
        year(date) <= 2002 ~ "2000-02",
        year(date) >= 2003 & year(date) <= 2007 ~ "2003-07",
        year(date) >= 2008 & year(date) <= 2012 ~ "2008-12",
        year(date) >= 2013 ~ "2013-18"
      ))
    
    # Make counts for both groups
    mini <- count(mini_leader, bucket)
    mini2 <- count(mini_invest, bucket)
    
    # Join the tiny tibbles to graph both together
    mini_all <- mini %>%
      full_join(mini2, by = "bucket", suffix = c(".engagements", ".investments"))
    
    # Make the NAs zeros
    mini_all[is.na(mini_all)] <- 0
    
    # Alter the data so that it records investments and engagements per YEAR
    mini_all <- mini_all %>% 
      mutate(n.engagements = case_when(
        bucket == "2000-02" ~ n.engagements / 3, # No engagements recorded here
        bucket == "2003-07" ~ n.engagements / 5,
        bucket == "2008-12" ~ n.engagements / 5,
        bucket == "2013-18" ~ n.engagements / 6)) %>% 
      mutate(n.investments = case_when(
        bucket == "2000-02" ~ n.investments / 3,
        bucket == "2003-07" ~ n.investments / 3, # Since observations start in 2005
        bucket == "2008-12" ~ n.investments / 5,
        bucket == "2013-18" ~ n.investments / 5.5)) # Since observations stop in June 2018

    # Make the mini graph
    ggplot(mini_all, aes(x = bucket, group = 1)) + 
      geom_point(aes(y = n.engagements), size = 0, col = "#0F52BA") +
      geom_line(aes(y = n.engagements), col = "#0F52BA") +
      geom_point(aes(y = n.investments), size = 0, col = "#ED2939") + # This is red
      geom_line(aes(y = n.investments), col = "#ED2939") +
      scale_y_continuous(
                         sec.axis = sec_axis(~.*1,
                                             name = "Investments per Year (#)",
                                             breaks = waiver())) +
      labs(x = "Date", y = "Engagements per Year") +
      theme(axis.title.y = element_text(color = "#0F52BA"),
            axis.title.y.right = element_text(color = "#ED2939"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
  })
  
  ### Plot the investments
  output$investPlot <- renderPlotly({
    
    # Select data only for the country chosen
    data1 <- investdf %>% 
      filter(country == input$country)
    
    # Make a title phrase
    chart_title <- case_when(
      input$country %in% c("United Arab Emirates", "Republic of the Congo", "Democratic Republic of the Congo", "Philippines", "British Virgin Islands",
                           "United States", "Czech Republic", "Ivory Coast", "Maldives", "Netherlands", "Bahamas") ~ paste0("Chinese Investments in the ", input$country),
      TRUE ~ paste0("Chinese Investments in ", input$country)
    )
    
    # Make the plotly object
    plot_ly(data1, x = ~date, y = ~(quantity_in_millions/1000), type = "scatter",
            hoverinfo = "text",
            text = ~paste0("</br>", "<b>Amount: </b>", paste0("$", formatC(quantity_in_millions, big.mark = ",")), ",000,000",
                           "</br>", "<b>Investor: </b>", chinese_entity,
                           "</br>", ifelse(is.na(transaction_party), "", paste0("<b>Transaction Party: </b>", transaction_party)),
                           ifelse(is.na(transaction_party), paste0("<b>Date: </b>", substr(date, 1, 7)),
                                  paste0("</br>", "<b>Date: </b>", substr(date, 1, 7)))),
            color = ~sector,
            size = ~log(quantity_in_millions)*50,
            marker = list(sizeref = .25),
            mode = "markers") %>% 
      config(displayModeBar = FALSE) %>% 
      layout(
          font = list(family ="Arial"),
          title = chart_title,
          xaxis = list(title = "Date"),
          yaxis = list(title = "Amount (bil. $)"),
          margin = list(t = 150, l = 450, pad = 0),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
          )
  })
  
  # Validate the check boxes so at least one is checked
  # observe({
  #   if(is.null(input$leaders))
  #     updateAwesomeCheckboxGroup(session, "leaders", selected = "Xi Jinping")
  # })
  
  ### Make the timeline
  output$engagements <- renderTimevis({
    
    # Select data only for the country chosen
    data1 <- leaderdf %>% 
      filter(country == input$country)
    
    # Filter for chosen leaders. If empty, only return Xi Jinping.
    if (length(input$leaders) == 0) {
      data1 <- data.frame(date = NA, year = NA, country = input$country, leader = NA, text = NA)
      names <- " "
    }
    else {
      data1 <- data1 %>% 
        filter(leader %in% input$leaders)
      names <- input$leaders
    }
      
    # Make a boolean display category for "high" or "low" engagements to make sure the timeline adjusts the view properly
    n <- data1 %>%
      count() %>% 
      as.double()
    high <- case_when(
      n > 25 ~ TRUE,
      n <= 25 ~ FALSE)
    
    ### Timeline
    timevis(showZoom = FALSE, fit = TRUE, height = "350pt",
            
            # Set options in JS for zoom limits; zoomMax is in miliseconds
            options = list(max = "2020-01-01",
                           min = "2000-01-01",
                           zoomMax = case_when(
                             high ~ 31540000000*2,
                             high ~ 567720000000)
            ),
            groups = data_frame(id = names, content = names)) %>% 
      
      # Add the data to the timeline with a smaller font size
      # Set the default view to the most recent engagement and six months before (in miliseconds)
      addItems(data = data_frame(
        start = data1$date,
        content = paste0(substr(data1$text, 1, 80), "..."),
        group = data1$leader,
        style = "font-size: 10px",
        type = "box",
        title = paste0(data1$text, " (", data1$date, ")"))) %>% 
      
      # Set the default window differently depending on whether the number of engagements is high or low
      setWindow(ifelse(high, max(data1$date) - 100, min(data1$date) - 1500), max(data1$date) + 100)
  })

  # Get the name of the current country for the table titles
  output$invest_title <- renderText({
    
    title <- case_when(
      input$country %in% c("United Arab Emirates", "Republic of the Congo", "Democratic Republic of the Congo", "Philippines", "British Virgin Islands",
                           "United States", "Czech Republic", "Ivory Coast", "Maldives", "Netherlands", "Bahamas") ~ paste0("Investments in the ", input$country),
      TRUE ~ paste0("Investments in ", input$country)
    ) 
    
    title
  })
  
  output$engagement_title2 <- renderText({
    title <- case_when(
      input$country %in% c("United Arab Emirates", "Republic of the Congo", "Democratic Republic of the Congo", "Philippines", "British Virgin Islands",
                           "United States", "Czech Republic", "Ivory Coast", "Maldives", "Netherlands", "Bahamas") ~ paste0("Engagements With the ", input$country),
      TRUE ~ paste0("Engagements With ", input$country)
    ) 
    
    title
  })
  
  ### Make the investment list data table
  output$investtable = renderDataTable({
    
    # Select data only for the country chosen
    data1 <- investdf %>% 
      filter(country == input$country) %>% 
      select(-country, -region, -bri) %>% 
      
    # Mutate the date so it does not display the first of the month
      mutate(date = substr(ymd(date), 1, 7)) %>% 
    
    # Mutate the amount so that it shows up as an actual dollar amount
      mutate(quantity_in_millions = quantity_in_millions*1000000)
    
    # Data table
    datatable(data1, options=list(pageLength = 10),
              colnames = c("Date", "Chinese company", "Transaction party", "Amount ($)", "Sector", "Subsector", "Share")) %>% 
      formatStyle(columns = TRUE, target= "row", lineHeight="90%") %>% 
      formatCurrency(columns = "quantity_in_millions", currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  ### Make the leader engagements data table
  output$leadertable = renderDataTable({
    
    # Select data only for the country chosen
    data1 <- leaderdf %>% 
      filter(country == input$country) %>% 
      filter(leader %in% input$leaders) %>% 
      select(-year, -country)
    
    # Data table
    datatable(data1, options=list(pageLength = 10),
              colnames = c("Date", "Leader", "Description")) %>% 
      formatStyle(columns = TRUE, target= "row")  })

}


# Run the application 
shinyApp(ui = ui, server = server)

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
ui <- navbarPage("Elite Chinese Diplomacy and Financial Flows",
                 position = "fixed-top",

### First tab
  tabPanel("Global Aid",

        div(class = "outer",
        # Include custom CSS
        tags$head(includeCSS("styles.css"))
        ),
    
    # Include the map
    leafletOutput("aidmap", width = "100%", height = "600px"),
    
    # Include the selection panel
    absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = FALSE, top = 60, right = "auto",
                  left = 40, bottom = "auto", height = "auto", width = 330,

                  selectInput(inputId = "country2", 
                              label = "Select a country:",
                              choices = aid_countries,
                              selected = "Sri Lanka",
                              selectize = TRUE),
                  selectInput(inputId = "sector",
                              label = "Choose sectors:",
                              choices = "All",
                              selected = "All",
                              multiple = FALSE),
                  awesomeCheckboxGroup(inputId = "leaders2",
                                       label = "Choose leaders:",
                                       choices = c(leaders),
                                       selected = c("Xi Jinping", "Hu Jintao"))
    
  ),
  hr(),
  timevisOutput("engagements2"),
  hr(),
  div(dataTableOutput("aidtable", width = "100%"), style = "font-size:80%"),
  div(dataTableOutput("leadertable2", width = "100%"), style = "font-size:80%")
  ),

### Second tab
tabPanel("Global Investments", fluidPage(
  
  # Page title
  titlePanel("Global Investments (2005-2018)"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(inputId = "country", 
                  label = "Select a country:",
                  choices = invest_countries,
                  selected = "United States",
                  selectize = TRUE),
      awesomeCheckboxGroup(inputId = "leaders",
                           label = "Choose leaders:",
                           choices = c(leaders),
                           selected = c("Xi Jinping", "Hu Jintao")),
      helpText("Xi Jinping: President (2013-Present); Vice President (2008-2013)"),
      helpText("Hu Jintao: President (2003-2013); Vice President (1998-2003)"),
      helpText("Li Keqiang: Premier (2013-Present); Vice Premier (2008-2013)"),
      helpText("Wen Jiabao: Premier (2003-2013)")),
    
    # Main panel
    mainPanel(plotlyOutput("investPlot"),
              timevisOutput("engagements"),
              tabPanel("",
                       actionButton("year03", "2003"),
                       actionButton("year04", "2004"),
                       actionButton("year05", "2005"),
                       actionButton("year06", "2006"),
                       actionButton("year07", "2007"),
                       actionButton("year08", "2008"),
                       actionButton("year09", "2009"),
                       actionButton("year10", "2010"),
                       actionButton("year11", "2011"),
                       actionButton("year12", "2012"),
                       actionButton("year13", "2013"),
                       actionButton("year14", "2014"),
                       actionButton("year15", "2015"),
                       actionButton("year16", "2016"),
                       actionButton("year17", "2017"),
                       actionButton("year18", "2018")),
              hr(),
              div(dataTableOutput("investtable", width = "100%"), style = "font-size:80%"),
              div(dataTableOutput("leadertable", width = "100%"), style = "font-size:80%")
    )
  ))) # Three parentheses for tabPanel, fluidPage, and sidebarLayout
)

##########
### SERVER
##########

server <- function(input, output, session) {

#########
# AID TAB
#########

  filteredMapData <- reactive({
    # Select data only for one project at a time
    map_projects <- aiddf %>% 
      filter(! is.na(latitude)) %>% 
      # filter(str_detect(country, input$country2)) %>%
      filter(! str_detect(country, ";"))
    
    # Remove duplicated projects so the markers don't overlap
    map_projects[! duplicated(map_projects$id), ]
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

  
  ### Make the timeline
  output$engagements2 <- renderTimevis({
    
    # Select data only for the country chosen
    data1 <- leaderdf %>% 
      filter(country == input$country2) %>% 
      filter(leader == input$leaders2)
    
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
            groups = data_frame(id = input$leaders2, content = input$leaders2)) %>% 
      
      # Add the data to the timeline with a smaller font size
      # Set the default view to the most recent engagement and six months before (in miliseconds)
      addItems(data = data_frame(
        start = data1$date,
        content = paste0(substr(data1$text, 1, 60), "..."),
        group = data1$leader,
        style = "font-size: 10px",
        type = "box",
        title = paste0(data1$text, " (", data1$date, ")"))) %>% 
      
      # Set the default window differently depending on whether the number of engagements is high or low
      setWindow(ifelse(high, max(data1$date) - 100, min(data1$date) - 1500), max(data1$date) + 100)
  })
  
  # Respond to each year button
  observeEvent(input$year03.2, {
    centerTime("engagements2", "2003-06-15")})
  observeEvent(input$year04.2, {
    centerTime("engagements2", "2004-06-15")})
  observeEvent(input$year05.2, {
    centerTime("engagements2", "2005-06-15")})
  observeEvent(input$year06.2, {
    centerTime("engagements2", "2006-06-15")})
  observeEvent(input$year07.2, {
    centerTime("engagements2", "2007-06-15")})
  observeEvent(input$year08.2, {
    centerTime("engagements2", "2008-06-15")})
  observeEvent(input$year09.2, {
    centerTime("engagements2", "2009-06-15")})
  observeEvent(input$year10.2, {
    centerTime("engagements2", "2010-06-15")})
  observeEvent(input$year11.2, {
    centerTime("engagements2", "2011-06-15")})
  observeEvent(input$year12.2, {
    centerTime("engagements2", "2012-06-15")})
  observeEvent(input$year13.2, {
    centerTime("engagements2", "2013-06-15")})
  observeEvent(input$year14.2, {
    centerTime("engagements2", "2014-06-15")})
  observeEvent(input$year15.2, {
    centerTime("engagements2", "2015-06-15")})
  observeEvent(input$year16.2, {
    centerTime("engagements2", "2016-06-15")})
  observeEvent(input$year17.2, {
    centerTime("engagements2", "2017-06-15")})
  observeEvent(input$year18.2, {
    centerTime("engagements2", "2018-06-15")})

### Make the aid data table
  output$aidtable = renderDataTable({
    
    # Select data only for the country chosen
    data1 <- aiddf %>% 
      filter(str_detect(country, input$country2)) %>% 
      mutate(usd_current = case_when(
        is.na(usd_current) ~ "Unknown",
        TRUE ~ paste0(format(usd_current, big.mark = ",")))) %>% 
      select(-id, -latitude, -longitude)
    
    # Data table
    datatable(data1, options=list(pageLength = 10),
              colnames = c("Year", "Country", "Place", "Sector", "Description", "Funder", "Flow", "Flow Class", "Intention", "Amount ($)"))
    })
  
### Make the leader engagements data table
  output$leadertable2 = renderDataTable({
    
    # Select data only for the country chosen
    data1 <- leaderdf %>% 
      filter(country == input$country2) %>% 
      select(-year, -country)
    
    # Data table
    datatable(data1, options=list(pageLength = 10),
              colnames = c("Date", "Leader", "Description"))
    })

  
  ################
  # INVESTMENT TAB
  ################
  
  ### Plot the investments
  output$investPlot <- renderPlotly({
    
    # Select data only for the country chosen
    data1 <- investdf %>% 
      filter(country == input$country)
    
    # Make the plotly object
    plot_ly(data1, x = ~date, y = ~quantity_in_millions, type = "scatter",
            hoverinfo = "text",
            text = ~paste0("</br>", "<b>Amount: </b>", paste0("$", format(quantity_in_millions, big.mark = ",")), ",000,000",
                           "</br>", "<b>Investor: </b>", chinese_entity,
                           "</br>", ifelse(is.na(transaction_party), "", paste0("<b>Transaction Party: </b>", transaction_party)),
                           ifelse(is.na(transaction_party), paste0("<b>Date: </b>", substr(date, 1, 7)),
                                  paste0("</br>", "<b>Date: </b>", substr(date, 1, 7)))),
            color = ~sector,
            size = ~log(quantity_in_millions)**10,
            mode = "markers") %>% 
      config(displayModeBar = FALSE) %>% 
      layout(
        title = paste0("Chinese Investments in ", ifelse(input$country == "United States", "the United States", input$country)),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Amount (mil. $)"))
  })
  
  ### Make the timeline
  output$engagements <- renderTimevis({
    
    # Select data only for the country chosen
    data1 <- leaderdf %>% 
      filter(country == input$country) %>% 
      filter(leader %in% input$leaders)
    
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
            groups = data_frame(id = input$leaders, content = input$leaders)) %>% 
      
      # Add the data to the timeline with a smaller font size
      # Set the default view to the most recent engagement and six months before (in miliseconds)
      addItems(data = data_frame(
        start = data1$date,
        content = paste0(substr(data1$text, 1, 60), "..."),
        group = data1$leader,
        style = "font-size: 10px",
        type = "box",
        title = paste0(data1$text, " (", data1$date, ")"))) %>% 
      
      # Set the default window differently depending on whether the number of engagements is high or low
      setWindow(ifelse(high, max(data1$date) - 100, min(data1$date) - 1500), max(data1$date) + 100)
  })
  
  # Respond to each year button
  observeEvent(input$year03, {
    centerTime("engagements", "2003-06-15")})
  observeEvent(input$year04, {
    centerTime("engagements", "2004-06-15")})
  observeEvent(input$year05, {
    centerTime("engagements", "2005-06-15")})
  observeEvent(input$year06, {
    centerTime("engagements", "2006-06-15")})
  observeEvent(input$year07, {
    centerTime("engagements", "2007-06-15")})
  observeEvent(input$year08, {
    centerTime("engagements", "2008-06-15")})
  observeEvent(input$year09, {
    centerTime("engagements", "2009-06-15")})
  observeEvent(input$year10, {
    centerTime("engagements", "2010-06-15")})
  observeEvent(input$year11, {
    centerTime("engagements", "2011-06-15")})
  observeEvent(input$year12, {
    centerTime("engagements", "2012-06-15")})
  observeEvent(input$year13, {
    centerTime("engagements", "2013-06-15")})
  observeEvent(input$year14, {
    centerTime("engagements", "2014-06-15")})
  observeEvent(input$year15, {
    centerTime("engagements", "2015-06-15")})
  observeEvent(input$year16, {
    centerTime("engagements", "2016-06-15")})
  observeEvent(input$year17, {
    centerTime("engagements", "2017-06-15")})
  observeEvent(input$year18, {
    centerTime("engagements", "2018-06-15")})
  
  ### Make the investment list data table
  output$investtable = renderDataTable({
    
    # Select data only for the country chosen
    data1 <- investdf %>% 
      filter(country == input$country) %>% 
      select(-country, -region, -bri)
    
    # Data table
    datatable(data1, options=list(pageLength = 10),
              colnames = c("Date", "Chinese company", "Transaction party", "Amount (mil. $)", "Sector", "Subsector", "Share")) %>% 
      formatStyle(columns = TRUE, target= "row", lineHeight="90%")
  })
  
  ### Make the leader engagements data table
  output$leadertable = renderDataTable({
    
    # Select data only for the country chosen
    data1 <- leaderdf %>% 
      filter(country == input$country) %>% 
      select(-year, -country)
    
    # Data table
    datatable(data1, options=list(pageLength = 10),
              colnames = c("Date", "Leader", "Description")) %>% 
      formatStyle(columns = TRUE, target= "row")  })

}




# Run the application 
shinyApp(ui = ui, server = server)

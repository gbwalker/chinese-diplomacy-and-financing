#
# This tool allows you to explore trends in Chinese financial flows and the international engagement of elite Chinese officials.
# Created by Gabe Walker for Government 1005: Data (Harvard University, December 2018). 
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
library(shinyBS)

##################
# Read in the data
##################

# Read in the necessary cleaned data that results from process_data.R.
# Note that process_data.R requires china_vitae_scraper.R and aiddata_get_countries.R to be run first.
aiddf <- read_rds("aiddf.rds")
investdf <- read_rds("investdf.rds")
leaderdf <- read_rds("leaderdf.rds")

# Make lists to be used for selecting subsets of the overall data.
# This is used for the selection criteria in the Shiny UI.
# Investment countries.
invest_countries <- unique(investdf$country) %>%
  sort()

# List of unique aid countries
# AidData lists projects that have multiple country destinations with a semicolon, so this list ignores those.
aid_countries <- aiddf %>%
  filter(!str_detect(aiddf$country, ";")) %>%
  filter(!is.na(longitude))

aid_countries <- unique(aid_countries$country) %>%
  sort()

# List of all the sectors for which China has provided aid.
aid_sectors <- unique(aiddf$sector)

# List of all unique country names between the three data sets.
countries <- as.list(read_rds("countries.rds"))

# List of all the leaders.
leaders <- unique(leaderdf$leader)

##################
### USER INTERFACE
##################
ui <- navbarPage("Elite Chinese Diplomacy and Financial Flows",
  position = "fixed-top",

  ### First tab for aid.
  tabPanel(
    "Aid",

    # Startup message introduces the user to the project and gets pre-written HTML generated from the Shiny server.
    bsModal(
      id = "startupModal", title = "Elite Chinese Diplomacy and Financial Flows", trigger = "", size = "large",
      img(src = "globe.png", style = "display: block; margin-left: auto; margin-right: auto;", height = "120", width = "120"),
      htmlOutput("introduction")
    ),

    # This custom CSS comes from the SuperZip Shiny example. It widens the available field in which the user can view the map.
    # See styles.css for a link to the original file.
    div(
      class = "outer",
      # Include custom CSS
      tags$head(includeCSS("styles.css"))
    ),

    # The map is large enough to be viewed on a monitor.
    leafletOutput("aidmap", width = "100%", height = "675px"),

    # These selections allow the user to sort the displayed data.
    # Selections include country, sector, and official.
    # Based on these selections, summary statistics from the server get displayed on the sidebar.
    # The panel is fixed in place so it does not move when the user scrolls.
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = FALSE, draggable = FALSE, top = 60, right = "auto",
      left = 40, bottom = "auto", height = "auto", width = 330,
      selectInput(
        inputId = "country2",
        label = HTML("<b>Select a country:</b>"),
        choices = aid_countries,
        selected = "Sri Lanka",
        selectize = TRUE
      ),
      
      # Summary statistics based on selections.
      helpText(textOutput("aid_amount")),
      helpText(textOutput("aid_number")),
      helpText(textOutput("aid_common")),
      selectInput(
        inputId = "sector",
        label = HTML("<b>Choose sectors:</b>"),
        choices = "All",
        selected = "All",
        multiple = FALSE
      ),
      tags$div(
        title = HTML("2013- : Xi President; Li K. Premier; Wang FM\n2003-13: Hu President; Wen Premier; Yang (07-13) and Li Z. (03-07) FM"),
        awesomeCheckboxGroup(
          inputId = "leaders2",
          label = HTML("<b>Choose officials:</b>"),
          choices = c(leaders),
          selected = c("Xi Jinping", "Hu Jintao"),
          inline = TRUE
        )
      ),
      helpText(textOutput("engagements_total2")),
      
      # The mini graph shows a general relationship between number of aid projects (adjusted per year)
      # and official engagements (also adjusted per year). This, like the summary statistics, gives
      # the user a quick impression of trends.
      plotOutput("mini_graph", height = "250px", width = "290px")
    ),
    hr(),
    
    # The last items in this tab are the timeline and two tables.
    # The timeline adapts to the officials selected above. The tables
    # react to all variables selected.
    timevisOutput("engagements2", width = "100%"),
    hr(),
    h3(textOutput("aid_title")),
    div(dataTableOutput("aidtable", width = "100%"), style = "font-size:80%"),
    hr(),
    h3(textOutput("engagement_title")),
    div(dataTableOutput("leadertable2", width = "100%"), style = "font-size:80%")
  ),

  ### Second tab for investment.
  tabPanel(
    "Investments",
    
    # This tab also uses the same custom CSS from the SuperZip example
    # to make sure the two pages are aesthetically similar.
    div(
      class = "outer",
      # Include custom CSS
      tags$head(includeCSS("styles.css"))
    ),

    # As described above, this panel provides mostly the same options as for aid.
    # Users can select a country and leaders. Sector selection (nonreactive) is done
    # only through the plotly object and not through a Shiny function.
    absolutePanel(
      id = "invest_controls", class = "panel panel-default", fixed = FALSE, draggable = FALSE, top = 60, right = "auto",
      left = 80, bottom = "auto", height = "auto", width = 290,
      style = "border: none;",
      selectInput(
        inputId = "country",
        label = HTML("<b>Select a country:</b>"),
        choices = invest_countries,
        selected = "United States",
        selectize = TRUE
      ),
      
      # Summary stats give users quick first impressions.
      helpText(textOutput("invest_amount")),
      helpText(textOutput("invest_number")),
      helpText(textOutput("invest_common")),
      helpText(textOutput("invest_value")),
      tags$div(
        title = HTML("2013- : Xi President; Li K. Premier; Wang FM\n2003-13: Hu President; Wen Premier; Yang (07-13) and Li Z. (03-07) FM"),
        awesomeCheckboxGroup(
          inputId = "leaders",
          label = HTML("<b>Choose officials:</b>"),
          choices = c(leaders),
          selected = c("Xi Jinping", "Hu Jintao"),
          inline = TRUE
        )
      ),
      helpText(textOutput("engagements_total")),
      plotOutput("mini_graph2", height = "245px", width = "290px")
    ),
    
    # The plotly object fills most of the screen as the map does on the aid tab.
    # It is fully reactive to user input as with any plotly object, but interactions
    # with the chart do not affect other displayed data on the page.
    plotlyOutput("investPlot", width = "95%", height = "675px", inline = TRUE),
    hr(),
    
    # The timevis timeline and data tables are the same as in the aid tab,
    # just for investment data (for the table).
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

  ###
  # The modal popup message provides users with an introductory message that
  # has information about the app (background, how to navigate, sources, etc.).
  # It appears once on startup and never again throughout the course of the
  # session until the page is refreshed.
  ###
  toggleModal(session, "startupModal", toggle = "open")

  output$introduction <- renderText({
    HTML("<p><b>Introduction</b> </br>
         This tool allows you to explore trends in Chinese financial flows and the international engagement of elite Chinese officials. </br>
          Specifically, it focuses on the in-office diplomatic activities of current president Xi Jinping, premier Li Keqiang, and foreign minister Wang Yi, and former president Hu Jintao, premier Wen Jiabao, and foreign ministers Yang Jiechi and Li Zhaoxing.</br>
          It catalogs 5,649 individual engagements, 3,446 unique aid projects, and 2,908 investments in 186 countries between 2000 and 2018.
          </p>
         <p><b>Navigation</b> </br>
          <i>Map:</i> Drag with your mouse and zoom with the scroll wheel. Click clusters to expand groups. Click on individual projects for details.</br>
          <i>Timeline:</i> Drag with your mouse and zoom with the scrool wheel. Hover over events for details.</br>
          <i>Investment Chart:</i> Select sectors by double-clicking a name. Draw a rectangle to focus on a portion of the chart.</br>
          <i>Tables:</i> Type to search.</br>
          </p>
         <p><b>Sources</b> </br>
          All data for this project comes from free, publicly available sources. Many thanks to those who collected and provided it.<br>
          <ul>
          <li>AidData Research and Evaluation Unit. 2018. <a href=\"https://www.aiddata.org/data/geocoded-chinese-global-official-finance-dataset\">AidData's Geocoded Global Chinese Official Finance, Version 1.1.1.</a> Williamsburg, VA: AidData at William & Mary.</li>
          <li><a href=\"http://www.aei.org/china-global-investment-tracker/\">China Global Investment Tracker.</a> 2018. American Enterprise Institute.</li>
          <li><i><a href=\"http://www.chinavitae.com/index.php\">China Vitae.</a></i> 2018. Carnegie Endowment for International Peace.</li>
          </ul>
          </p>
         <p><b>Note</b> </br>
         Created by <a href=\"mailto:gabriel_walker@student.hks.harvard.edu\">Gabe Walker</a> for <i>Government 1005: Data</i> (Harvard University, December 2018). <br>
         See <a href=\"https://github.com/gbwalker/chinese-diplomacy-and-financing\">Github</a> for methodology, cleaned data, engagement scraper, and app source code.</p>")
  })

  #########
  # AID TAB
  #########

  ### 
  # The below items react to user inputs to generate summary statistics.
  # This text is then returned to the UI to be displayed on the sidebar.
  ###

  # Total aid amount.
  output$aid_amount <- renderText({
    amount <- aiddf %>%
      filter(str_detect(country, input$country2)) %>%
      filter(!is.na(usd_current))

    # Filter by sector if applicable.
    if (input$sector != "All") {
      amount <- amount %>%
        filter(sector == input$sector)
    }

    amount <- sum(amount$usd_current)
    paste0("Recorded aid: $", formatC(amount, format = "f", digits = 0, big.mark = ","))
  })

  # Number of projects for the given country and sector.
  output$aid_number <- renderText({
    number <- aiddf %>%
      filter(str_detect(country, input$country2))

    # Filter by sector if applicable.
    if (input$sector != "All") {
      number <- number %>%
        filter(sector == input$sector)
    }

    number <- tally(number)
    paste0("Recorded projects: ", number)
  })

  # The most common sector.
  # Returns two sectors if two are tied for the most common.
  output$aid_common <- renderText({
    common <- aiddf %>%
      filter(str_detect(country, input$country2))

    common <- count(common, sector) %>%
      arrange(desc(n))

    # Check if there are two top sectors.
    ifelse(common$n[1] != common$n[2], top <- common$sector[1], top <- common$sector[1:2])

    # Fill in a dummy if there's only one sector.
    if (length(common$sector) == 1) {
      top <- common$sector[1]
    }

    # Return different values if there's one or two top sectors.
    ifelse(length(top) == 1, paste0("Most common sector: ", top), paste0("Most common sectors: ", top[1], " and ", top[2]))
  })

  # Find the total number of engagements.
  output$engagements_total2 <- renderText({
    total <- leaderdf %>%
      filter(str_detect(country, input$country2)) %>%
      filter(leader %in% input$leaders2) %>%
      count()

    paste0("Engagements: ", total)
  })

  # This tiny plot on the sidebar gives users a quick
  # first impression about trends in aid amount and 
  # diplomatic engagements. It adjusts reactively based
  # on what filter criteria that users select.
  output$mini_graph <- renderPlot({

    # Make a dummy df to catch exceptions where there are no engagements,
    # which happens for some countries that China provides aid to.
    mini_leader <- data.frame(date = NA)

    # Filter the engagement data
    # by the selected country and leader(s).
    mini_leader <- leaderdf %>%
      filter(country == input$country2) %>%
      filter(leader %in% input$leaders2) %>%

      # Group years by year ranges so they are
      # more suitable for a tiny graph.
      mutate(year = as.double(year)) %>%
      mutate(bucket = case_when(
        year <= 2002 ~ "2000-02",
        year >= 2003 & year <= 2007 ~ "2003-07",
        year >= 2008 & year <= 2012 ~ "2008-12",
        year >= 2013 ~ "2013-18"
      ))

    # Filter the aid data by user selections
    # just as leader data was filtered.
    mini_aid <- aiddf %>%
      filter(country == input$country2) %>%

      # Make a bucket variable.
      mutate(bucket = case_when(
        year <= 2002 ~ "2000-02",
        year >= 2003 & year <= 2007 ~ "2003-07",
        year >= 2008 & year <= 2012 ~ "2008-12",
        year >= 2013 ~ "2013-18"
      ))

    # Filter the results if users choose a sector
    if (input$sector != "All") {
      mini_aid <- mini_aid %>% 
        filter(sector == input$sector)
    }
    
    # Count the occurences of each for 
    # each bucket value that exists,
    # again to simplify the graph as much
    # as possible for a quick impression.
    mini <- count(mini_leader, bucket)
    mini2 <- count(mini_aid, bucket)

    # Then join both together for graphing in one ggplot call.
    mini_all <- mini %>%
      full_join(mini2, by = "bucket", suffix = c(".engagements", ".aid"))

    # Make the NAs zeros to ensure
    # ggplot throws no errors.
    mini_all[is.na(mini_all)] <- 0

    # Alter the data so that it records aid and
    # engagements per YEAR, not just raw quantities.
    # This is necessary for an accurate representation on
    # the graph because the buckets contain different numbers
    # of years and the observations begin and end in different years.
    mini_all <- mini_all %>%
      mutate(n.engagements = case_when(
        bucket == "2000-02" ~ n.engagements / 3,
        bucket == "2003-07" ~ n.engagements / 5,
        bucket == "2008-12" ~ n.engagements / 5,
        bucket == "2013-18" ~ n.engagements / 6
      )) %>%
      mutate(n.aid = case_when(
        bucket == "2000-02" ~ n.aid / 3, # Aid projects start in 2000.
        bucket == "2003-07" ~ n.aid / 5,
        bucket == "2008-12" ~ n.aid / 5,
        bucket == "2013-18" ~ n.aid / 2 # Since observations only go through 2013-2014.
      )) 

    # Send the tiny graph to the sidebar with different colored lines
    # for engagements and aid (red and blue). Also eliminate the background
    # so that users are not distracted by grid lines, background color, etc.
    ggplot(mini_all, aes(x = bucket, group = 1)) +
      geom_point(aes(y = n.engagements), size = 0, col = "#0F52BA") +
      geom_line(aes(y = n.engagements), col = "#0F52BA") +
      geom_point(aes(y = n.aid), size = 0, col = "#ED2939") + # This is red
      geom_line(aes(y = n.aid), col = "#ED2939") +
      scale_y_continuous(
        sec.axis = sec_axis(~ . * 1,
          name = "Aid Projects per Year",
          breaks = waiver()
        )
      ) +
      labs(x = "Date", y = "Engagements per Year") +
      theme(
        axis.title.y = element_text(color = "#0F52BA"),
        axis.title.y.right = element_text(color = "#ED2939"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      )
  })
  
  ### 
  # The map allows users to explore projects that have coordinates.
  # It lays out base tiles and then reacts to user selections to 
  # redraw the markers and shift the overall view as necessary. 
  ###
  
  # This expression filters the aid data
  # to provide a list of projects that have
  # location data associated with them and
  # are not "multiple country" projects 
  # denoted with a ;. Other leaflet functions rely
  # on this initial sorted data.
  filteredMapData <- reactive({
    # Select data only for one project at a time
    map_projects <- aiddf %>%
      filter(!is.na(latitude)) %>%
      filter(!str_detect(country, ";"))
    
    # Remove duplicated projects so the markers don't overlap
    map_projects[!duplicated(map_projects$id), ]
  })
  
  output$aidmap <- renderLeaflet({

    # Initiate the map and lay out base tiles.
    leaflet(filteredMapData, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })

  # Every time the user chooses a new country, reorient the map view.
  # This arbitrarily focuses on the first listed project in the country.
  observe({
    map_projects <- filteredMapData() %>%
      filter(str_detect(country, input$country2))

    # Use the leafletProxy function to create a simulation of
    # the plotted map, so it does not need to refresh the entire
    # map.
    leafletProxy("aidmap", data = map_projects) %>%
      setView(map_projects$longitude[1], map_projects$latitude[1], zoom = 7)
  })

  # Every time the user chooses a new sector, redraw the markers.
  # Similarly to setting the zoom level, use leafletProxy so that
  # the map is not recreated from scratch with each new selection.
  # This improves the overall user experience. Also, cluster the
  # markers to that projects close to each other can be expanded.
  # Size of the markers depends on the log of the value of the
  # project, which is not known for every project.
  observe({
    ifelse(input$sector == "All", {
      map_projects <- filteredMapData()
    }, {
      map_projects <- filteredMapData() %>%
        filter(sector == input$sector)
    })

    leafletProxy("aidmap", data = map_projects) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~ ifelse(is.na(usd_current), 6, log(usd_current)),
        color = "#00A86B",
        opacity = 0.5,
        fillOpacity = 0.25,
        clusterOptions = markerClusterOptions(
          freezeAtZoom = 12,
          spiderfyOnMaxZoom = TRUE,
          removeOutsideVisibleBounds = TRUE,
          spiderfyDistanceMultiplier = 2,
          spiderLegPolylineOptions = list(weight = .75)
        ),
        
        # Add a popup so users can click on an individual project
        # and see details about it. The popup shows the funder,
        # sector, short description, amount if known, intent,
        # locale (usually more specific than country), and
        # year of the project.
        popup = paste0(
          "<b>Funder: </b>", map_projects$funder, "<br>",
          "<b>Sector: </b>", map_projects$sector, "<br>",
          "<b>Description: </b>", map_projects$title, "<br>",
          "<b>Amount: </b>", ifelse(is.na(map_projects$usd_current),
            "Unknown",
            paste0("$", formatC(map_projects$usd_current,
              format = "f",
              digits = 0,
              big.mark = ","
            ))
          ), "<br>",
          "<b>Intent: </b>", map_projects$intent, "<br>",
          "<b>Locale: </b>", str_trim(map_projects$place), ", ",
          map_projects$country, ", ",
          map_projects$year
        )
      )
  })

  # This observe function makes a list of custom
  # sectors for each country. That way, users
  # cannot select a sector that does not exist for
  # a given country. E.g., some countries may only
  # have received aid in Health. This would throw
  # an error.
  observe({
    data1 <- filteredMapData() %>%
      filter(str_detect(country, input$country2))

    updateSelectInput(session, "sector", choices = c("All", unique(data1$sector)), selected = "All")
  })

  # These reactive expressions adjust the table titles
  # to match the selected country. They add a "the" to
  # certain countries that are properly formatted with a "the."
  # This expression generates the title of the aid table.
  output$aid_title <- renderText({
    title <- case_when(
      input$country2 %in% c(
        "United Arab Emirates", "Republic of the Congo", "Democratic Republic of the Congo", "Philippines", "British Virgin Islands",
        "United States", "Czech Republic", "Ivory Coast", "Maldives", "Netherlands", "Bahamas"
      ) ~ paste0("Aid to the ", input$country2),
      TRUE ~ paste0("Aid to ", input$country2)
    )
    title
  })
  
  # This expression generates the title of
  # the engagement table.
  output$engagement_title <- renderText({
    title <- case_when(
      input$country2 %in% c(
        "United Arab Emirates", "Republic of the Congo", "Democratic Republic of the Congo", "Philippines", "British Virgin Islands",
        "United States", "Czech Republic", "Ivory Coast", "Maldives", "Netherlands", "Bahamas"
      ) ~ paste0("Engagements With the ", input$country2),
      TRUE ~ paste0("Engagements With ", input$country2)
    )
    title
  })

  ###
  # The timevis timeline allows users to scroll and zoom through
  # 15 years' worth of official diplomatic engagements. It uses
  # leader data selected by the user in the UI and displays a
  # popup of the specific engagement text scraped from China Vitae.
  ###
  
  output$engagements2 <- renderTimevis({

    # Select data only for the country chosen.
    data1 <- leaderdf %>%
      filter(country == input$country2)

    # Filter for chosen leaders.
    # If empty, return a blank set of names.
    if (length(input$leaders2) == 0) {
      data1 <- data.frame(date = NA, year = NA, country = input$country2, leader = NA, text = NA)
      names <- " "
    }
    else {
      data1 <- data1 %>%
        filter(leader %in% input$leaders2)
      names <- input$leaders2
    }

    # Make a boolean display category for "high" or "low" engagements
    # to make sure the timeline adjusts the view properly.
    n <- data1 %>%
      count() %>%
      as.double()
    high <- case_when(
      n > 25 ~ TRUE,
      n <= 25 ~ FALSE
    )

    # Generate the timeline.
    timevis(
      showZoom = FALSE, fit = TRUE, height = "450pt",

      # Set options for the minimum and maximum display.
      options = list(
        max = "2020-01-01",
        min = "2000-01-01"
      ),
      
      # Separate the observations by the names selected
      # by the user. If no names are selected, names is blank.
      groups = data_frame(id = names, content = names)
    ) %>%

      # Add the data to the timeline with a smaller font size
      # because there are so many observations. Include a popup
      # with the full text about the engagement and the date
      # it occurred.
      addItems(data = data_frame(
        start = data1$date,
        content = paste0(substr(data1$text, 1, 100), "..."),
        group = data1$leader,
        style = "font-size: 10px",
        type = "box",
        title = paste0(data1$text, " (", data1$date, ")")
      )) %>%

      # Set the default window differently depending on
      # whether the number of engagements is high or low.
      setWindow(ifelse(high, max(data1$date) - 100, min(data1$date) - 1500), max(data1$date) + 100)
  })

  ###
  # The aid data table displays all the data that fit
  # the criteria selected by the user, including that
  # without location data and not shown on the map.
  ###
  output$aidtable <- renderDataTable({

    # Select data only for the country chosen.
    data1 <- aiddf %>%
      filter(str_detect(country, input$country2)) %>%
      select(-id, -latitude, -longitude)

    # Filter by sector, if applicable.
    if (input$sector != "All") {
      data1 <- data1 %>%
        filter(sector == input$sector)
    }

    # Print the data table with prettier column
    # names and formatted aid amount. This allows
    # users to still sort the amounts by size even
    # when formatted nicely.
    datatable(data1,
      options = list(pageLength = 10),
      colnames = c("Year", "Country", "Place", "Sector", "Description", "Funder", "Flow", "Flow Class", "Intention", "Amount ($)")
    ) %>%
      formatCurrency(columns = "usd_current", currency = "", interval = 3, mark = ",", digits = 0)
  })

  ###
  # The engagements table displays all the data that apply
  # to the officisl selected by the user.
  ### 
  output$leadertable2 <- renderDataTable({

    # Select data only for the country chosen.
    data1 <- leaderdf %>%
      filter(country == input$country2) %>%
      filter(leader %in% input$leaders2) %>%
      select(-year, -country)

    # Print the table with prettier column names.
    datatable(data1,
      options = list(pageLength = 10),
      colnames = c("Date", "Official", "Description")
    )
  })


  ################
  # INVESTMENT TAB
  ################

  ### 
  # The below items react to user inputs to generate summary statistics.
  # This text is then returned to the UI to be displayed on the sidebar.
  ###
  
  # Total investment amount.
  output$invest_amount <- renderText({
    amount <- investdf %>%
      filter(country == input$country) %>%
      filter(!is.na(quantity_in_millions))

    amount <- sum(amount$quantity_in_millions)
    paste0("Total investment: $", formatC(amount, format = "f", digits = 0, big.mark = ","), ",000,000")
  })

  # Number of investments for a given country.
  # Sector is not included since it's incorporated
  # into the plotly graph.
  output$invest_number <- renderText({
    number <- investdf %>%
      filter(country == input$country)

    number <- tally(number)
    paste0("Number of investments: ", number)
  })

  # Most common sector.
  # Returns two sectors if two are tied for the most common.
  output$invest_common <- renderText({
    common <- investdf %>%
      filter(str_detect(country, input$country))

    common <- count(common, sector) %>%
      arrange(desc(n))

    # Check if there are two top sectors.
    ifelse(common$n[1] != common$n[2], top <- common$sector[1], top <- common$sector[1:2])

    # Fill in a dummy if there's only one sector.
    if (length(common$sector) == 1) {
      top <- common$sector[1]
    }

    # Return different values if there's one or two top sectors.
    ifelse(length(top) == 1, paste0("Most common sector: ", top), paste0("Most common sectors: ", top[1], " and ", top[2]))
  })

  # Most highly valued sector.
  output$invest_value <- renderText({
    value <- investdf %>%
      filter(str_detect(country, input$country)) %>%
      group_by(sector) %>%
      tally(quantity_in_millions) %>%
      arrange(desc(n))

    # Return different values if there's one or two top sectors.
    paste0("Highest value sector: ", value$sector[1], " ($", formatC(value$n[1] / 1000, digits = 2, format = "f"), " bil.)")
  })

  # Find the total number of engagements
  output$engagements_total <- renderText({
    total <- leaderdf %>%
      filter(str_detect(country, input$country)) %>%
      filter(leader %in% input$leaders) %>%
      count()

    paste0("Engagements: ", total)
  })

  # This tiny plot on the sidebar gives users a quick
  # first impression about trends in investment counts and 
  # diplomatic engagements. It adjusts reactively based
  # on what filter criteria that users select.
  output$mini_graph2 <- renderPlot({

    # Make a dummy df to catch exceptions where there are no engagements,
    # which happens for some countries that China provides aid to.
    mini_leader <- data.frame(date = NA)

    # Filter the engagement data
    # by the selected country and leader(s).
    mini_leader <- leaderdf %>%
      filter(country == input$country) %>%
      filter(leader %in% input$leaders) %>%

      # Group years by year ranges so they are
      # more suitable for a tiny graph.
      mutate(year = as.double(year)) %>%
      mutate(bucket = case_when(
        year <= 2002 ~ "2000-02",
        year >= 2003 & year <= 2007 ~ "2003-07",
        year >= 2008 & year <= 2012 ~ "2008-12",
        year >= 2013 ~ "2013-18"
      ))

    # Filter the investment data by user selections
    # just as leader data was filtered.
    mini_invest <- investdf %>%
      filter(country == input$country) %>%

      # Make a bucket variable.
      mutate(bucket = case_when(
        year(date) <= 2002 ~ "2000-02",
        year(date) >= 2003 & year(date) <= 2007 ~ "2003-07",
        year(date) >= 2008 & year(date) <= 2012 ~ "2008-12",
        year(date) >= 2013 ~ "2013-18"
      ))

    # Count the occurences of each for 
    # each bucket value that exists,
    # again to simplify the graph as much
    # as possible for a quick impression.
    mini <- count(mini_leader, bucket)
    mini2 <- count(mini_invest, bucket)

    # Then join both together for graphing in one ggplot call.
    mini_all <- mini %>%
      full_join(mini2, by = "bucket", suffix = c(".engagements", ".investments"))

    # Make the NAs zeros to ensure
    # ggplot throws no errors.
    mini_all[is.na(mini_all)] <- 0

    # Alter the data so that it records investment and
    # engagements per YEAR, not just raw quantities.
    # This is necessary for an accurate representation on
    # the graph because the buckets contain different numbers
    # of years and the observations begin and end in different years.
    mini_all <- mini_all %>%
      mutate(n.engagements = case_when(
        bucket == "2000-02" ~ n.engagements / 3, # No engagements recorded here
        bucket == "2003-07" ~ n.engagements / 5,
        bucket == "2008-12" ~ n.engagements / 5,
        bucket == "2013-18" ~ n.engagements / 6
      )) %>%
      mutate(n.investments = case_when(
        bucket == "2000-02" ~ n.investments / 3,
        bucket == "2003-07" ~ n.investments / 3, # Since observations start in 2005
        bucket == "2008-12" ~ n.investments / 5,
        bucket == "2013-18" ~ n.investments / 5.5 # Since observations stop in June 2018
      )) 

    # Send the tiny graph to the sidebar with different colored lines
    # for engagements and aid (red and blue). Also eliminate the background
    # so that users are not distracted by grid lines, background color, etc.
    ggplot(mini_all, aes(x = bucket, group = 1)) +
      geom_point(aes(y = n.engagements), size = 0, col = "#0F52BA") +
      geom_line(aes(y = n.engagements), col = "#0F52BA") +
      geom_point(aes(y = n.investments), size = 0, col = "#ED2939") + # This is red
      geom_line(aes(y = n.investments), col = "#ED2939") +
      scale_y_continuous(
        sec.axis = sec_axis(~ . * 1,
          name = "Investments per Year (#)",
          breaks = waiver()
        )
      ) +
      labs(x = "Date", y = "Engagements per Year") +
      theme(
        axis.title.y = element_text(color = "#0F52BA"),
        axis.title.y.right = element_text(color = "#ED2939"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      )
  })

  ### 
  # The investment chart is a plotly object that allows
  # users to browse specific projects by amount, sector,
  # and date. The usual toolbar is hidden but users can
  # still filter by sector and draw a rectangle to focus
  # on specific investments.
  ###
  output$investPlot <- renderPlotly({

    # Select data only for the country chosen.
    data1 <- investdf %>%
      filter(country == input$country)

    # Make a chart title, which adds a
    # "the" adaptively as in the data table
    # titles.
    chart_title <- case_when(
      input$country %in% c(
        "United Arab Emirates", "Republic of the Congo", "Democratic Republic of the Congo", "Philippines", "British Virgin Islands",
        "United States", "Czech Republic", "Ivory Coast", "Maldives", "Netherlands", "Bahamas"
      ) ~ paste0("Chinese Investments in the ", input$country),
      TRUE ~ paste0("Chinese Investments in ", input$country)
    )

    # Make the plotly object. Show the y axis in billions for ease of
    # reading. Popup on hover with more investment information. Color
    # by sector and adjust the size by investment amount just for the
    # aesthetic effect.
    plot_ly(data1,
      x = ~date, y = ~ (quantity_in_millions / 1000), type = "scatter",
      hoverinfo = "text",
      text = ~ paste0(
        "</br>", "<b>Amount: </b>", paste0("$", formatC(quantity_in_millions, big.mark = ",")), ",000,000",
        "</br>", "<b>Investor: </b>", chinese_entity,
        "</br>", ifelse(is.na(transaction_party), "", paste0("<b>Transaction Party: </b>", transaction_party)),
        ifelse(is.na(transaction_party), paste0("<b>Date: </b>", substr(date, 1, 7)),
          paste0("</br>", "<b>Date: </b>", substr(date, 1, 7))
        )
      ),
      color = ~sector,
      size = ~ log(quantity_in_millions) * 50,
      marker = list(sizeref = .25),
      mode = "markers"
    ) %>%
      
      # Hide the usual mode bar and change the
      # font so it matches better with Shiny's
      # default font. Also align it on the page
      # so it fits nicely with the other elements.
      config(displayModeBar = FALSE) %>%
      layout(
        font = list(family = "Arial"),
        title = chart_title,
        xaxis = list(title = "Date"),
        yaxis = list(title = "Amount (bil. $)"),
        margin = list(t = 150, l = 450, pad = 0),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  ###
  # The timevis timeline allows users to scroll and zoom through
  # 15 years' worth of official diplomatic engagements. It uses
  # leader data selected by the user in the UI and displays a
  # popup of the specific engagement text scraped from China Vitae.
  ###
  output$engagements <- renderTimevis({

    # Select data only for the country chosen.
    data1 <- leaderdf %>%
      filter(country == input$country)

    # Filter for chosen leaders.
    # If empty, return a blank set of names.
    if (length(input$leaders) == 0) {
      data1 <- data.frame(date = NA, year = NA, country = input$country, leader = NA, text = NA)
      names <- " "
    }
    else {
      data1 <- data1 %>%
        filter(leader %in% input$leaders)
      names <- input$leaders
    }

    # Make a boolean display category for "high" or "low" engagements
    # to make sure the timeline adjusts the view properly.
    n <- data1 %>%
      count() %>%
      as.double()
    high <- case_when(
      n > 25 ~ TRUE,
      n <= 25 ~ FALSE
    )

    # Generate the timeline.
    timevis(
      showZoom = FALSE, fit = TRUE, height = "500pt",

      # Set options for the minimum and maximum display.
      options = list(
        max = "2020-01-01",
        min = "2000-01-01"
      ),
      
      # Separate the observations by the names selected
      # by the user. If no names are selected, names is blank.
      groups = data_frame(id = names, content = names)
    ) %>%

      # Add the data to the timeline with a smaller font size
      # because there are so many observations. Include a popup
      # with the full text about the engagement and the date
      # it occurred.
      addItems(data = data_frame(
        start = data1$date,
        content = paste0(substr(data1$text, 1, 80), "..."),
        group = data1$leader,
        style = "font-size: 10px",
        type = "box",
        title = paste0(data1$text, " (", data1$date, ")")
      )) %>%

      # Set the default window differently depending on
      # whether the number of engagements is high or low.
      setWindow(ifelse(high, max(data1$date) - 100, min(data1$date) - 1500), max(data1$date) + 100)
  })

  # These reactive expressions adjust the table titles
  # to match the selected country. They add a "the" to
  # certain countries that are properly formatted with a "the."
  # This expression generates the title of the investment table.
  output$invest_title <- renderText({
    title <- case_when(
      input$country %in% c(
        "United Arab Emirates", "Republic of the Congo", "Democratic Republic of the Congo", "Philippines", "British Virgin Islands",
        "United States", "Czech Republic", "Ivory Coast", "Maldives", "Netherlands", "Bahamas"
      ) ~ paste0("Investments in the ", input$country),
      TRUE ~ paste0("Investments in ", input$country)
    )

    title
  })

  # This expression generates the title of the engagement table.
  output$engagement_title2 <- renderText({
    title <- case_when(
      input$country %in% c(
        "United Arab Emirates", "Republic of the Congo", "Democratic Republic of the Congo", "Philippines", "British Virgin Islands",
        "United States", "Czech Republic", "Ivory Coast", "Maldives", "Netherlands", "Bahamas"
      ) ~ paste0("Engagements With the ", input$country),
      TRUE ~ paste0("Engagements With ", input$country)
    )

    title
  })

  ###
  # The investment table displays all the data that fit
  # the criteria selected by the user.
  ###
  output$investtable <- renderDataTable({

    # Select data only for the country chosen.
    data1 <- investdf %>%
      filter(country == input$country) %>%
      select(-country, -region, -bri) %>%

      # Mutate the date so it does not display the
      # first of the month, which is an arbitrary choice
      # for the plotly object so the markers appear at
      # different points.
      mutate(date = substr(ymd(date), 1, 7)) %>%

      # Mutate the amount so that it shows up
      # as the actual dollar amount.
      mutate(quantity_in_millions = quantity_in_millions * 1000000)

    # Print the data table with prettier column
    # names and formatted investment amount. This allows
    # users to still sort the amounts by size even
    # when formatted nicely.
    datatable(data1,
      options = list(pageLength = 10),
      colnames = c("Date", "Chinese company", "Transaction party", "Amount ($)", "Sector", "Subsector", "Share")
    ) %>%
      formatStyle(columns = TRUE, target = "row", lineHeight = "90%") %>%
      formatCurrency(columns = "quantity_in_millions", currency = "", interval = 3, mark = ",", digits = 0)
  })

  ###
  # The engagements table displays all the data that apply
  # to the officisl selected by the user.
  ### 
  output$leadertable <- renderDataTable({

    # Select data only for the country chosen.
    data1 <- leaderdf %>%
      filter(country == input$country) %>%
      filter(leader %in% input$leaders) %>%
      select(-year, -country)

    # Print the table with prettier column names.
    datatable(data1,
      options = list(pageLength = 10),
      colnames = c("Date", "Official", "Description")
    ) %>%
      formatStyle(columns = TRUE, target = "row")
  })
}


# Run the application
shinyApp(ui = ui, server = server)

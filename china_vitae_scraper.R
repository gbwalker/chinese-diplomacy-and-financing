# This program scrapes China Vitae for the public activities of seven top Chinese officials: Xi Jinping, Hu Jintao,
# Li Keqiang, Wen Jiabao, Wang Yi, Yang Jiechi, and Li Zhaoxing during 2003-2018, and returns a list of their "engagements"
# with other countries. That is, any mention of a foreign nation (whether meeting with a leader, sending congratulations or condolences,
# etc.) in a description of their activities.

library(tidyverse)
library(stringr)
library(rvest)
library(countrycode)
library(fs)

# Read in a custom list of country names to scrape.
countries <- read_csv("data/country-list.csv")

# Make a list of the urls to read for the seven leaders.
urls <- c( # Xi Jinping
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2018",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2017",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2016",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2015",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2014",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=303&filter_year=2013",
  # Hu Jintao
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2013",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2012",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2011",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2010",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2009",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2008",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2007",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2006",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2005",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2004",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=19&filter_year=2003",
  # Li Keqiang
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=263&filter_year=2018",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=263&filter_year=2017",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=263&filter_year=2016",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=263&filter_year=2015",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=263&filter_year=2014",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=263&filter_year=2013",
  # Wen Jiabao
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2013",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2012",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2011",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2010",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2009",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2008",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2007",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2006",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2005",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2004",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=21&filter_year=2003",
  # Wang Yi
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=416&filter_year=2018",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=416&filter_year=2017",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=416&filter_year=2016",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=416&filter_year=2015",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=416&filter_year=2014",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=416&filter_year=2013",
  # Yang Jiechi
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=1885&filter_year=2013",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=1885&filter_year=2012",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=1885&filter_year=2011",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=1885&filter_year=2010",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=1885&filter_year=2009",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=1885&filter_year=2008",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=1885&filter_year=2007",
  # Li Zhaoxing
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=411&filter_year=2007",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=411&filter_year=2006",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=411&filter_year=2005",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=411&filter_year=2004",
  "http://www.chinavitae.com/vip/index.php?mode=events&type=cv&id=411&filter_year=2003"
)

# Make an empty df for all of the scraped data.
engagements_all <- tibble(date = NA, year = NA, country = NA, leader = NA, text = NA) %>%
  na.omit()

# Extract the text and country from
# all of the scraped data.
for (url in urls) {
  
  # Read in the HTML data.
  page <- read_html(url)

  # Get the year from the URL name.
  year <- str_extract(str_extract(url, "year=\\d\\d\\d\\d"), "\\d\\d\\d\\d")

  # Get the leader's name from the page.
  name <- html_text(html_nodes(page, ".red2"))

  # Print a status report in the console.
  print(paste0("Working on ", year, " for ", name))

  # Get text of the activity from all the tables.
  # Fortunately, all the data is formatted with
  # the same CSS on China Viate, so we can use
  # the same html_nodes.
  rank_data_html <- html_nodes(page, ".link12")
  rank_data <- html_text(rank_data_html) %>%
    as.data.frame()
  colnames(rank_data) <- "text"
  rank_data <- rank_data %>%
    mutate(text = as.character(text))

  # Make an iterator to go through
  # each entry.
  dim <- (1:length(rank_data$text))

  # Make an empty df for the engagement
  # list with a set leader and year.
  engagements <- tibble(
    date = rep(NA, length(rank_data$text)),
    year = rep(year, length(rank_data$text)),
    country = rep(NA, length(rank_data$text)),
    leader = rep(name, length(rank_data$text)),
    text = rep(NA, length(rank_data$text))
  )

  # Pull out the countries and dates.
  count <- 1
  for (i in dim) {
    for (country in countries$name) {
      if (str_detect(rank_data$text[i], country)) {
        engagements$date[count] <- rank_data$text[i - 1]
        engagements$country[count] <- country
        engagements$text[count] <- rank_data$text[i]
        count <- count + 1
      }
    }
  }

  # Get rid of NAs and China engagements.
  engagements2 <- engagements %>%
    na.omit() %>%
    filter(!country == "China") %>%

    # The below code fixes a number of edge
    # case country names that may not be 
    # captured with an initial pass.
    
    # Fix the two Koreas.
    mutate(country = case_when(
      country == "Korea" & str_detect(text, "South Korea") ~ "South Korea",
      country == "Korea" & str_detect(text, "Chairman") ~ "North Korea",
      country == "Korea" & str_detect(text, "DPRK") ~ "North Korea",
      country == "Korea" & str_detect(text, "Kim Jong Un") ~ "North Korea",
      country == "Korea" & str_detect(text, "Moon") ~ "South Korea",
      country == "Korea" & str_detect(text, "Republic of Korea") ~ "South Korea",
      country == "Korea" & str_detect(text, "ROK") ~ "South Korea",
      country == "Korea" & str_detect(text, "Korean Peninsula") ~ "Korea",
      country == "Korea" & str_detect(text, "S. Korea") ~ "South Korea",
      country == "Korea" & str_detect(text, "Korea-China") ~ "South Korea",
      TRUE ~ country
    )) %>%
    filter(!country == "Korea") %>%

    # Fix the two Congos.
    mutate(country = case_when(
      country == "Republic of Congo" | country == "Republic of the Congo" ~ "Republic of the Congo",
      TRUE ~ country
    )) %>%
    mutate(country = case_when(
      country == "Republic of the Congo" & str_detect(text, "Democratic Republic of the Congo") ~ "Democratic Republic of the Congo",
      country == "Republic of the Congo" & str_detect(text, "Democratic Republic of Congo") ~ "Democratic Republic of the Congo",
      country == "Republic of the Congo" & str_detect(text, "Brazzaville") ~ "Republic of the Congo",
      TRUE ~ country
    )) %>%

    # Fix the four Guineas.
    mutate(country = case_when(
      country == "Guinea" & str_detect(text, "Guinea Bissau") ~ "Guinea-Bissau",
      country == "Guinea" & str_detect(text, "Guinea-Bissau") ~ "Guinea-Bissau",
      country == "Guinea" & str_detect(text, "Papua New Guinea") ~ "Papua New Guinea",
      country == "Guinea" & str_detect(text, "Equatorial Guinea") ~ "Equatorial Guinea",
      TRUE ~ country
    )) %>%

    # Fix Nigeria and Niger.
    mutate(country = case_when(
      country == "Niger" & str_detect(text, "Nigeria") ~ "Nigeria",
      TRUE ~ country
    )) %>%

    # Fix Sudan and South Sudan.
    mutate(country = case_when(
      country == "Sudan" & str_detect(text, "South Sudan") ~ "South Sudan",
      TRUE ~ country
    )) %>%

    # Fix a few other countries
    # that are inconsistent.
    mutate(country = case_when(
      country == "British" | country == "Britain" | country == "United Kingdom" | country == "U.K." ~ "England",
      country == "French" ~ "France",
      country == "Papua" ~ "Papua New Guinea",
      country == "U.S." ~ "United States",
      country == "USA" ~ "United States",
      country == "Bosnia" | country == "Herzegovina" ~ "Bosnia and Herzegovina",
      country == "Czech" ~ "Czech Republic",
      country == "Solomon" ~ "Solomon Islands",
      country == "Cote D'Ivoire" ~ "Ivory Coast",
      country == "Sao Tome" ~ "Sao Tome and Principe",
      country == "Timor-Leste" ~ "East Timor",
      TRUE ~ country
    )) %>%

    # Get rid of non-date rows just in case any slip through.
    filter(str_detect(date, "\\d\\d/\\d\\d/\\d\\d")) %>%

    # Get rid of "Inner Mongolia" rows that get
    # confused with the country Mongolia.
    filter(!str_detect(text, "Inner Mongolia")) %>%

    # Get rid of duplicate engagements
    # on the same day that should not count
    # as multiple engagements (i.e., many meetings
    # in one day in one country counts as one).
    group_by(date) %>%
    distinct(country, .keep_all = TRUE) %>%
    ungroup() %>%

    # Clean text a bit, removing portions
    # and characters and leading/ending
    # bits that should not be kept in the final text.
    mutate(text = str_remove(text, "View complete entry")) %>%
    mutate(text = str_remove_all(text, "•")) %>%
    mutate(text = str_replace_all(text, "\\)O", "\\) O")) %>%
    mutate(text = str_remove_all(text, "\n\t\t\t\t\t\t")) %>%
    mutate(text = str_remove_all(text, "\n\t\t\t\t\t"))

  # Save all of the engagements in one df.
  engagements_all <- rbind(engagements_all, engagements2)
}

# Write a file that process_data.R can access.
write_rds(engagements_all, "data/all_engagements_2003-2018.rds")

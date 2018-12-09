#
# This file cleans data from three sources: AidData, AEI investment tracker,
# and scraped data from China Vitae.
#

library(tidyverse)
library(stringr)
library(countrycode)
library(fs)
library(janitor)
library(lubridate)
library(readxl)

###############################
### Manipulate the AidData data
###############################

# This is a custom function to pull out all the country names
# from the AidData data.

# First get the country guide, also custom
# for the purposes of this project. It's a serious
# challenge to make sure country names align between
# three different datasets.
cguide <- read_csv("data/country-list.csv")

# This function searches the AidData recipient_oecd_name field
# and then project_title field for country names.
get_countries <- dget("aiddata_get_countries.R")

######################
### Work with the data
######################

# Uncomment the below code to download the file automatically
# from the AidData website.

# Download the data folder and unzip
# download.file(url = "http://docs.aiddata.org/ad4/files/GeoCoded_China_Data_Merged_Files.zip",
#               destfile = "data/aiddata_china.zip",
#               quiet = TRUE,
#               mode = "wb")
# unzip("data/aiddata_china.zip", exdir = "data")

# Read the data
# aiddf <- read_csv("data/GeoCoded_China_Data_Merged_Files/all_flow_classes.csv") %>%

# Or, read the data in from the provided CSV on Github.
aiddf <- read_csv("data/all_flow_classes.csv") %>%

  # Select only variables of interest
  # out of a very large range of possibilities.
  select(
    project_id, year, recipient_condensed, recipient_oecd_name, project_title, crs_sector_name, funding_agency,
    place_name, flow, flow_class, intent, usd_current, latitude, longitude
  ) %>%

  # Clean up certain easy country names
  # right off the bat.
  mutate(country = case_when(
    recipient_condensed == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    recipient_condensed == "Congo, Rep." ~ "Republic of the Congo",
    recipient_condensed == "Central African Rep." ~ "Central African Republic",
    recipient_condensed == "Viet Nam" ~ "Vietnam",
    recipient_condensed == "Micronesia, Federated States of" ~ "Micronesia",
    recipient_condensed == "Kyrgyz Republic" ~ "Kyrgyzstan",
    recipient_condensed == "Palestinian Adm. Areas" ~ "Palestine",
    recipient_condensed == "Korea, Dem. Rep." ~ "North Korea",
    recipient_condensed == "Macedonia, FYR" ~ "Macedonia",
    TRUE ~ recipient_oecd_name
  )) %>%

  # Change some variable names for ease of typing
  # and select only those of interest.
  mutate(
    id = project_id, sector = crs_sector_name, title = project_title, funder = funding_agency,
    place = place_name
  ) %>%
  select(
    id, year, country, place, sector, title, funder, flow, flow_class, intent, usd_current, latitude,
    longitude
  )

###
# This code fixes the country names for regional cases, where
# projects are listed only as being in one region, not in a
# specific country. It uses get_countries() to do so.
###

# Make a row counter.
rows <- (1:length(aiddf$id))

# Iterate over each row in aiddf
# to find all the regional cases.
for (row in rows) {
  
  # If the country is a regional listing,
  # instead change the country to match the
  # countries found by get_countries().
  if (!is.na(str_detect(aiddf[row, ]$country, "regional"))) {
    aiddf[row, ]$country <- get_countries(aiddf[row, ]$country, aiddf[row, ]$title)
  }
}

# Manually adjust many edge cases that I found
# where names differed from each other.
aiddf <- aiddf %>%
  mutate(country = case_when(
    country == "DEL" & place %in% cguide$name ~ place,
    country == "DEL" & place == "Kinshasa" ~ "Democratic Republic of the Congo",
    country == "DEL" & place == "DR Congo" ~ "Democratic Republic of the Congo",
    country == "Republic of Congo" & place == "DR Congo" ~ "Democratic Republic of the Congo",
    country == "DEL" & str_detect(title, "DRC") ~ "Democratic Republic of the Congo",
    country == "DEL" & place == "Equatorial Guinea" ~ "Equatorial Guinea",
    country == "DEL" & place == "Papua New Guinea" ~ "Papua New Guinea",
    country == "DEL" & str_detect(title, "PNG") ~ "Papua New Guinea",
    country == "DEL" & str_detect(place, "Port Moresby") ~ "Papua New Guinea",
    country == "DEL" & str_detect(title, "Trinidad & Tobago") ~ "Trinidad and Tobago",
    country == "DEL" & str_detect(place, "Trinidad & Tobago") ~ "Trinidad and Tobago",
    country == "DEL" & place == "Bissau" ~ "Guinea-Bissau",
    country == "DEL" & place == "Palestine" ~ "Palestine",
    country == "DEL" & place == "Gaza Strip" ~ "Palestine",
    country == "DEL" & place == "Ramallah" ~ "Palestine",
    country == "DEL" & place == "Guinea-Bissau" ~ "Guinea-Bissau",
    country == "DEL" & place == "Nigeria" ~ "Nigeria",
    country == "Niger" & str_detect(title, "Nigeria") ~ "Nigeria",
    country == "Bosnia; Herzegovina" ~ "Bosnia and Herzegovina",
    country == "Cote D'Ivoire" ~ "Ivory Coast",
    country == "Timor-Leste" ~ "East Timor",
    country == "Korea" & str_detect(title, "North Korea") ~ "North Korea",
    country == "Korea" & str_detect(title, "South Korea") ~ "South Korea",
    is.na(country) & str_detect(place, "South Sudan") ~ "South Sudan",
    is.na(country) & str_detect(title, "South Sudan") ~ "South Sudan",
    TRUE ~ country
  )) %>%

  # Fix some Guinea issues (four countries
  # with the word "Guinea" in their names).
  mutate(country = case_when(
    country == "Guinea" & str_detect(place, "Equatorial Guinea") ~ "Equatorial Guinea",
    country == "Guinea" & str_detect(title, "Equatorial Guinea") ~ "Equatorial Guinea",
    country == "Guinea" & str_detect(place, "Bissau") ~ "Guinea-Bissau",
    country == "Guinea" & str_detect(title, "Bissau") ~ "Guinea-Bissau",
    country == "Guinea" & str_detect(place, "Papua New Guinea") ~ "Papua New Guinea",
    country == "Guinea" & str_detect(title, "Papua New Guinea") ~ "Papua New Guinea",
    country == "Guinea" & str_detect(place, "PNG") ~ "Papua New Guinea",
    country == "Guinea" & str_detect(title, "PNG") ~ "Papua New Guinea",
    TRUE ~ country
  )) %>%

  # Fix one multiple country name
  # for East Timor.
  mutate(country = case_when(
    str_detect(country, ";") & str_detect(country, "Timor-Leste") ~ str_replace(country, "Timor-Leste", "East Timor"),
    TRUE ~ country
  )) %>%

  # Filter out some country cases, set to DEL, that
  # do not have easily accesible country names.
  filter(!country == "DEL")

# Delete the data files if downloaded.
# file_delete(c("data/aiddata_china.zip", "data/GeoCoded_China_Data_Merged_Files", "data/__MACOSX"))

# Export the data to a data file and where the Shiny app can access it.
write_rds(aiddf, "data/aiddf.rds")
write_rds(aiddf, "chinese-diplomacy-and-financing/aiddf.rds")

####################################
### Manipulate the AEI/Heritage data
####################################

# Uncomment this code to download the file from AEI.
# download.file(url = "https://www.aei.org/data/China-Global-Investment-Tracker",
#               destfile = "data/china_investments.xlsx",
#               quiet = TRUE,
#               mode = "wb")

# Read in the data
# investdf <- read_xlsx("data/china_investments.xlsx", skip = 5) %>%

# Make a list of months to use
# for cleaning the dates.
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Read in the data from the provided CSV
# instead of downloading it.
investdf <- read_csv("data/china_investments.csv") %>%
  clean_names() %>%

  # Tweak certain country names
  # and one region, which I ended
  # up not using in the final app.
  mutate(country = case_when(
    country == "Antigua and Barbuda" ~ "Antigua",
    country == "Britain" ~ "England",
    country == "USA" ~ "United States",
    country == "UAE" ~ "United Arab Emirates",
    country == "Russian Federation" ~ "Russia",
    country == "Congo" ~ "Republic of the Congo",
    country == "Bosnia" ~ "Bosnia and Herzegovina",
    country == "Timor-Leste" ~ "East Timor",
    country == "Trinidad-Tobago" ~ "Trinidad and Tobago",
    country == "Sao Tome" ~ "Sao Tome and Principe",
    TRUE ~ country
  )) %>%
  mutate(region = case_when(
    region == "USA" ~ "United States",
    TRUE ~ region
  )) %>%

  # Turn BRI into a factor,
  # another variable I did not
  # end up using.
  mutate(bri = as.factor(bri)) %>%

  # Turn the year and month into a proper
  # date so comparisons and plotting can
  # be done later (rather than month names).
  mutate(
    month = case_when(
      month == months[1] ~ "01",
      month == months[2] ~ "02",
      month == months[3] ~ "03",
      month == months[4] ~ "04",
      month == months[5] ~ "05",
      month == months[6] ~ "06",
      month == months[7] ~ "07",
      month == months[8] ~ "08",
      month == months[9] ~ "09",
      month == months[10] ~ "10",
      month == months[11] ~ "11",
      month == months[12] ~ "12",
    ),
    date = ymd(paste(year, month, "01", sep = ""))
  ) %>%
  select(country, date, chinese_entity, transaction_party, quantity_in_millions, sector, subsector, share_size, region, bri)

# Export the data to a data file and where the Shiny app can access it.
write_rds(investdf, "data/investdf.rds")
write_rds(investdf, "chinese-diplomacy-and-financing/investdf.rds")

########################################
### Manipulate the scraped activity data
########################################

# Read in the data generated by china_vitae_scraper.R
# from the scraped data provided on Github.
leaderdf <- read_rds("data/all_engagements_2003-2018.rds") %>%

  # Change the date with
  # the lubridate package.
  mutate(date = mdy(date)) %>%

  # Filter out activity for leaders when they were not in office.
  # Xi assumed office on March 14, 2013.
  # Li K. assumed office on March 15, 2013.
  # Hu assumed office on March 15, 2003, and left March 14, 2013.
  # Wen assumed office on March 16, 2003, and left March 15, 2013.
  # Wang assumed office on March 16, 2013.
  # Yang assumed office on April 27, 2007, and left March 16, 2013.
  # Li Z. assumed office on March 17, 2003, and left April 27, 2007.
  filter(!{
    leader == "Xi Jinping" & date <= "2013-03-13"
  }) %>% 
  filter(!{
    leader == "Li Keqiang" & date <= "2013-03-14"
  }) %>% 
  filter(!{
    leader == "Hu Jintao" & date >= "2013-03-14" & date <= "2003-03-14"
  }) %>% 
  filter(!{
    leader == "Hu Jintao" & date <= "2003-03-14"
  }) %>% 
  filter(!{
    leader == "Wen Jiabao" & date >= "2013-03-15"
  }) %>% 
  filter(!{
    leader == "Wen Jiabao" & date <= "2003-03-15"
  }) %>% 
  filter(!{
    leader == "Wang Yi" & date <= "2013-03-15"
  }) %>% 
  filter(!{
    leader == "Yang Jiechi" & date >= "2013-03-16"
  }) %>% 
  filter(!{
    leader == "Yang Jiechi" & date <= "2007-04-26"
  }) %>% 
  filter(!{
    leader == "Li Zhaoxing" & date >= "2007-04-26"
  }) %>% 
  filter(!{
    leader == "Li Zhaoxing" & date <= "2003-03-16"
  })

# Export the data to a data file and where the Shiny app can access it.
write_rds(leaderdf, "data/leaderdf.rds")
write_rds(leaderdf, "chinese-diplomacy-and-financing/leaderdf.rds")

####################################################
### Make a full country list for all three datasets.
####################################################

# Make a unique country list for each
# individual dataset.
country_aiddf <- as_tibble(unique(aiddf$country))
country_investdf <- as_tibble(unique(investdf$country))
country_leaderdf <- as_tibble(unique(leaderdf$country))

# Unify the column names.
colnames(country_aiddf) <- "country"
colnames(country_investdf) <- "country"
colnames(country_leaderdf) <- "country"

# Join the country lists.
countries <- country_aiddf %>%
  full_join(country_investdf, "country") %>%
  full_join(country_leaderdf, "country") %>%

  # Remove multiple country names and alphabetize them.
  filter(!str_detect(country, ";")) %>%
  arrange(country)

# Export the data to a data file and where the Shiny app can access it.
write_rds(countries, "data/countries.rds")
write_rds(countries, "chinese-diplomacy-and-financing/countries.rds")

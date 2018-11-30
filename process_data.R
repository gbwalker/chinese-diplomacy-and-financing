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

### Custom function to pull out country names
# First get the country guide
cguide <- read_csv("data/country-list.csv")

# Function to search from the recipient_oecd_name field and then project_title field
get_countries <- dget("aiddata_get_countries.R")

######################
### Work with the data
######################

# Download the data folder and unzip
# download.file(url = "http://docs.aiddata.org/ad4/files/GeoCoded_China_Data_Merged_Files.zip",
#               destfile = "data/aiddata_china.zip",
#               quiet = TRUE,
#               mode = "wb")
# unzip("data/aiddata_china.zip", exdir = "data")

# Read the data
# aiddf <- read_csv("data/GeoCoded_China_Data_Merged_Files/all_flow_classes.csv") %>% 

# Read the data
aiddf <- read_csv("data/all_flow_classes.csv") %>%

# Select only variables of interest
  select(project_id, year, recipient_condensed, recipient_oecd_name, project_title, crs_sector_name, funding_agency,
         place_name, flow, flow_class, intent, usd_current, latitude, longitude) %>% 

# Clean up the country names
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

# Change some variable names and select only those of interest
  mutate(id = project_id, sector = crs_sector_name, title = project_title, funder = funding_agency,
         place = place_name) %>% 
  select(id, year, country, place, sector, title, funder, flow, flow_class, intent, usd_current, latitude,
         longitude)

### Fix the country names for regional cases
# Make a row counter
rows <- (1:length(aiddf$id))

# Iterate over each row in aiddf
for (row in rows){
  # If the country is a regional listing
  if (! is.na(str_detect(aiddf[row,]$country, "regional"))) {
    # Set it equal to the found countries
    aiddf[row,]$country <- get_countries(aiddf[row,]$country, aiddf[row,]$title)
  }
}

# Manually adjust some edge cases
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
    is.na(country) & str_detect(place, "South Sudan") ~ "South Sudan",
    is.na(country) & str_detect(title, "South Sudan") ~ "South Sudan",
    TRUE ~ country
  )) %>% 
  
# Fix some Guinea issues
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
  mutate(country = case_when(
    str_detect(country, ";") & str_detect(country, "Timor-Leste") ~ str_replace(country, "Timor-Leste", "East Timor"),
    TRUE ~ country
  )) %>% 

# Filter out the DELs without easily accesible country names
  filter(! country == "DEL")

# Delete the data files if downloaded
# file_delete(c("data/aiddata_china.zip", "data/GeoCoded_China_Data_Merged_Files", "data/__MACOSX"))

# Export the data
write_rds(aiddf, "data/aiddf.rds")
write_rds(aiddf, "chinese-diplomacy-and-financing/aiddf.rds")

####################################
### Manipulate the AEI/Heritage data
####################################

# Code to download the file from AEI
# download.file(url = "https://www.aei.org/data/China-Global-Investment-Tracker",
#               destfile = "data/china_investments.xlsx",
#               quiet = TRUE,
#               mode = "wb")

# Read in the data
# investdf <- read_xlsx("data/china_investments.xlsx", skip = 5) %>%

# Read in the data
investdf <- read_csv("data/china_investments.csv") %>%
  clean_names() %>% 

# Tweak country names and region
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

# Make BRI a factor
  mutate(bri = as.factor(bri))

# Export the data
write_rds(investdf, "data/investdf.rds")
write_rds(aiddf, "chinese-diplomacy-and-financing/investdf.rds")

########################################
### Manipulate the scraped activity data
########################################

# Read in the data
leaderdf <- read_rds("data/all_engagements_2003-2018.rds") %>% 

# Change the date
  mutate(date = mdy(date))

# Export the data
write_rds(leaderdf, "data/leaderdf.rds")

############################
### Make a full country list
############################

# Make a unique country list for each df
country_aiddf <- as_tibble(unique(aiddf$country))
country_investdf <- as_tibble(unique(investdf$country))
country_leaderdf <- as_tibble(unique(leaderdf$country))

# Unify the column names
colnames(country_aiddf) <- "country"
colnames(country_investdf) <- "country"
colnames(country_leaderdf) <- "country"

# Join the country lists
countries <- country_aiddf %>% 
  full_join(country_investdf, "country") %>% 
  full_join(country_leaderdf, "country") %>% 

# Take out multiple country names
  filter(! str_detect(country, ";")) %>% 

# Alphabetize
  arrange(country)

# Export the data
write_rds(countries, "data/countries.rds")
write_rds(aiddf, "chinese-diplomacy-and-financing/countries.rds")


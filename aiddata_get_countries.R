get_countries <- function(text1, text2) {
  # Split the text
  text.split <- as_tibble(str_split(text1, ";"), validate = FALSE)
  colnames(text.split) <- "word"
  
  # Make an empty df for storing country names
  countries <- as_data_frame(rep(NA, length(text.split[[1]])))
  colnames(countries) <- "country"
  
  # Clean the text and save it to the countries list
  counter = 1
  for (n in text.split$word){
    countries$country[counter] <- str_trim(n)
    counter = counter + 1
  }
  
  # Update the list to save countries that are in the country list
  countries <- countries %>% 
    mutate(country = case_when(
      country %in% cguide$name ~ country,
      TRUE ~ "DEL"
    )) %>% 
    
    # Save only country names that are in the list
    filter(! country == "DEL")
  
  # Test if there were no listed countries. If so, check the text2 (project_title)
  if (is.na(countries$country[1])){
    
    # Reset the countries df (with an arbitrary number of slots)
    countries <- as_data_frame(rep(NA, 10))
    colnames(countries) <- "country"
    
    # Find countries in text2
    counter = 1
    for (n in cguide$name){
      if (str_detect(text2, n)){
        countries$country[counter] <- n
        counter = counter + 1
      }
    }
    
    # Remove empty slots that are not country names
    countries <- countries %>% 
      na.omit()
  }
  
  # Catch exceptions where the function finds no country names
  if(is.na(countries$country[1])) {
    return("DEL")
  }
  
  # Turn the countries df into a string that can be saved
  countries <- str_flatten(countries$country, "; ")
  
  # Return
  return(countries)
}
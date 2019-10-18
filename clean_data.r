require(geojsonio)
require(leaflet)
require(dplyr)
require(tidyr)
require(rjson)
require(tidyverse)
require(reshape2)
require(readr)

## College location data
# https://ope.ed.gov/dapip/#/download-data-files
# Locations of all accredited universities in the US
institution_data = read_csv("InstitutionCampus.csv")
institution_locs = select(institution_data, LocationName, Address)

# Get the location information in the right format
institution_locs$name = str_sub(str_extract(institution_locs$Address, ", .*, .."), 2)
institution_locs = separate(institution_locs, name, into = c("city_name", "state"), sep = ", ", remove = FALSE)

# Reformat strings to be how we want them
institution_locs$city_name = str_trim(institution_locs$city_name)
institution_locs$name = str_replace(institution_locs$name, ",", "")
institution_locs$name = str_trim(institution_locs$name)

# Just ignore all the colleges that it didn't work for (weird community colleges that are part of a building)
institution_locs = filter(institution_locs, str_length(institution_locs$state) <= 2)

# Add location data to our institutions data
college_locs = left_join(institution_locs, us_cities, by = c("name", "state" = "country.etc"))
colnames(college_locs)[colnames(college_locs)=="LocationName"] <- "college_name"

## CS Ranking data
cs_rankings_data = read_csv(file.path("CSrankings", "csrankings.csv"))
acm_fellows = read_csv(file.path("CSrankings", "acm-fellows.csv"))
turing = read_csv(file.path("CSrankings", "turing.csv"))

# Rename aliased names
colnames(acm_fellows)[colnames(acm_fellows)=="year"] <- "acm_year"
colnames(turing)[colnames(turing)=="year"] <- "turing_year"

# Add in acm_fellows data and turing data
cs_rankings_data = left_join(cs_rankings_data, acm_fellows, by="name")
cs_rankings_data = left_join(cs_rankings_data, turing, by="name")

venues = read_csv(file=file.path("CSrankings", "venues.csv"))

# Articles starts as json, unfortunately
articles = fromJSON(file=file.path("CSrankings", "articles.json"))

# Fix to be csv
articles_melted = melt(t(as.data.frame(articles)))
articles_melted$Var1 = str_replace(articles_melted$Var1, "\\.[:digit:]+", "")
articles_clean = dcast(articles_melted, (seq_len(nrow(articles_melted)) - 1) %/% 11 ~ Var1)[,-1]

# Fix some names to map correctly to college location data
articles_clean$institution = str_replace_all(articles_clean$institution, "Univ\\.", "University")

# Add articles and cs_rankings_data together
cs_rankings_data = left_join(cs_rankings_data, articles_clean, by="name")

## Trim data to only US stuff
cs_rankings_data_copy = cs_rankings_data

# Fix some more names with symbols that don't exist in college location data
college_locs$college_name = str_squish(str_replace_all(college_locs$college_name, 
                                                    "[^[:alnum:]]", " "))
cs_rankings_data$institution = str_squish(str_replace_all(cs_rankings_data$institution, 
                                                    "[^[:alnum:]|]", " "))

# Filter out cities that aren't in the cs_rankings_data
college_locs = filter(college_locs, college_locs$college_name %in% cs_rankings_data$institution)

# Remove dumb whitespace
cs_rankings_data$institution = str_trim(cs_rankings_data$institution)
cs_rankings_data$affiliation = str_trim(cs_rankings_data$affiliation)

# We don't care about anything not in the US
cs_rankings_data = filter(cs_rankings_data, 
                          cs_rankings_data$institution %in% college_locs$college_name)

# Fix `area` field to be area rather than just conf again
cs_rankings_data = cs_rankings_data %>% select(-starts_with("area"))
cs_rankings_data = left_join(cs_rankings_data, venues, by = c("conf" = "alternate"))


## Write data 
write.csv(cs_rankings_data, file = file.path("shiny_app", "clean", "pubs.csv"), row.names = FALSE)
write.csv(college_locs, file = file.path("shiny_app", "clean", "college_locs.csv"), row.names = FALSE)
write.csv(venues, file = file.path("shiny_app", "clean", "venues.csv"), row.names = FALSE)
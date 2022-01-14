#-------------------------------------------------------------
# Libraries and Key
# https://bookdown.org/dereksonderegger/444/api-data-queries.html
# https://mgimond.github.io/ES218/Week03b.html
# aq2XxNmT3M9p9ez
# https://github.com/walkerke/tidycensus/issues/131
# https://www.hcd.ca.gov/community-development/building-blocks/housing-needs/overpayment-overcrowding.shtml
# 
#-------------------------------------------------------------

library(BasicRoxygen) # example import
library(tidycensus)
library(tidyverse)

# best way to looks for variables
# https://api.census.gov/data/2019/acs.html
# https://censusreporter.org/

census_api_key("6ed2c84b691cee46900c6dc2d03ed90d8d4db051", overwrite=TRUE)

#-------------------------------------------------------------
# Input Variables
#-------------------------------------------------------------

# census variables for input
census_variables <- read_csv('data/census_variables_second.csv')

# dataframe to named vectors
census_variables <- census_variables %>% pull(variable,name)

# areas for analysis
areas <- c("Santa Barbara", "Ventura")

# years for analysis
years <- c(2019)

#-------------------------------------------------------------
# Timeseries - Variables Specified

# This works, but data is lacking last two years
#-------------------------------------------------------------

la <- tibble()

for(j in years){
  print(j)
  
  # grab data as table
  temp <- get_acs(geography = "county",
                  variables = census_variables, # using named vectors for rename
                  state = "CA",
                  county = "Los Angeles",
                  year = j,
                  output = "wide",
                  geometry = FALSE)
  
  temp <- temp %>% mutate(year = j)
  
  la <- rbind(la,temp)
}

#-------------------------------------------------------------
# Timeseries - Table Specified, Single Area
#-------------------------------------------------------------

la <- tibble()

# grab data for all years
for(j in years){
  print(j)

  # grab data as table
  temp <- get_acs(geography = "county",
                  table = "B25014",
                  state = "CA",
                  county = "Los Angeles",
                  year = j,
                  output = "tidy",
                  geometry = FALSE)

  temp <- temp %>% mutate(year = j)

  la <- rbind(la,temp)
}

# grab names of variables to join 
var_names <- load_variables(2019, 'acs5', cache = TRUE)

# merge names
la <- merge(la,var_names, by.x = "variable", by.y = "name")

# remove unneccesary variables
la_wide <- la %>% select(-1, -5, -8)

la_wide <- la_wide %>%  pivot_wider(names_from = label, values_from = estimate) 


#-------------------------------------------------------------
# Timeseries - Table Specified, All Areas
#-------------------------------------------------------------

study_area <- tibble()

# grab data for all years
for(j in years){
  print(j)
  
  # grab data as table
  temp <- get_acs(geography = "county",
                  table = "B25014",
                  state = "CA",
                  county = c("Los Angeles", "Orange"),
                  year = j,
                  output = "tidy",
                  geometry = FALSE)
  
  temp <- temp %>% mutate(year = j)
  
  study_area <- rbind(study_area,temp)
}

# grab names of variables to join 
var_names <- load_variables(2019, 'acs5', cache = TRUE)

# merge names
study_area <- merge(study_area,var_names, by.x = "variable", by.y = "name")

# remove unneccesary variables
study_area_wide <- study_area %>% select(-1, -5, -8)

study_area_wide <- study_area_wide %>%  pivot_wider(names_from = label, values_from = estimate) 

# TURN THIS INTO FUNCTION

#-------------------------------------------------------------
# TS Function
#-------------------------------------------------------------

#' @title
#' Census Table Time Series
#'
#' @description
#' This function returns a dataframe of specified census table within designated areas and years 
#' 
#' @param census_table character strings of census table id
#' 
#' @param geography character string of geography that the data is available at (tract, county, zcta, etc...)
#' 
#' @param state character string of state study areas are in (without this, downloads all counties in US)
#' 
#' @param study_areas vector of character strings of areas that the table allows for
#' 
#' @param years vector of numeric type for each year desired
#'
#' @return df census table dataframe
#' 
#' @export
#'
#' @examples
#' census_table_timeseries("B25014", "county", ("Los Angeles", "Orange"), (2019,2018,2017))



same_state_census_table_timeseries <- function(census_table, geography, state, study_areas, years) {
  
  df <- tibble()
  
  for(i in years){

    print(paste("Downloading data for year:", i))
    
    temp <- get_acs(geography = geography,
                    table = census_table,
                    state = state,
                    county = study_areas,
                    year = i,
                    output = "tidy",
                    geometry = FALSE)
    
    temp <- temp %>% mutate(year = i)
    
    df <- rbind(df,temp)
  }

  # grab names of variables to join
  var_names <- load_variables(years[1], 'acs5', cache = TRUE)

  # merge names
  df <- merge(df,var_names, by.x = "variable", by.y = "name")

  # remove unnecessary variables
  df_wide <- df %>% select(-1, -5, -8)

  df_wide <- df_wide %>%  pivot_wider(names_from = label, values_from = estimate)
  
  return(df_wide)
}

df <- same_state_census_table_timeseries("B25014", "county", "CA", c("Los Angeles", "Orange"), c(2019,2018,2017))

# TURN THIS INTO ROXYGEN FUNCTION

# HETEROGENEITY / MAJORITYCENSUS TRACT SUMMARY FUNCTIONS

# TURN INTO FUNCTION

# TURN INTO ROXYGEN FUNCTION

# MAP MAP MAP
























########################################################[IGNORE]#################################################

########################################################[IGNORE]#################################################

########################################################[IGNORE]#################################################


#-------------------------------------------------------------
# Long/Tidy Dataset
#-------------------------------------------------------------

temp_tidy <- get_acs(geography = "county",
                table = "B25014",
                state = "CA",
                county = "Los Angeles",
                year = j,
                output = "tidy",
                geometry = FALSE)

var_names <- load_variables(j, 'acs5', cache = TRUE)

temp_tidy <- merge(temp_tidy,var_names, by.x = "variable", by.y = "name")

temp_tidy <- temp_tidy %>% tidyr::separate(label, remove=FALSE, c('estimate_drop','total_drop','occupant', 'occupied per room'), sep ='!!')

temp_tidy <- temp_tidy %>% mutate(year = j)

#-------------------------------------------------------------
# Wide Dataset
#-------------------------------------------------------------

temp_wide <- get_acs(geography = "county",
                     table = "B25014",
                     state = "CA",
                     county = "Los Angeles",
                     year = j,
                     output = "wide",
                     geometry = FALSE)


#-------------------------------------------------------------
# Long/Tidy to Wide Dataset
#-------------------------------------------------------------

tidy_to_wide <- temp_tidy %>% select(-1, -5, -7:-11)

tidy_to_wide <- tidy_to_wide %>%  pivot_wider(names_from = label, values_from = estimate) 

#-------------------------------------------------------------
# Wide to Long/Tidy  Dataset
#-------------------------------------------------------------

wide_to_tidy <- temp_wide %>%  pivot_longer(cols=3:28, names_to = "variable", values_to = "estimate")


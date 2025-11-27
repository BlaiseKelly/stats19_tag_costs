library(stats19)
library(reshape2) # officially depricated but melt is really concise
library(dplyr)
library(utils)
library(readODS)


get_ons_cost_data <- function(
    sheet2get = "Average_value_road_type"
) {
  
  url = "https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods"
  
  tmpfile <- tempfile(fileext = ".ods")
  
  utils::download.file(url, destfile = tmpfile, mode = "wb")
  
  if(sheet2get == "Average_value"){
    ons_cost <- readODS::read_ods(tmpfile, sheet = "Average_value", skip = 3)
    
    # drop first row and keep first 5 columns
    ons_cost_form <- ons_cost[-1, 1:5]
    
    names(ons_cost_form) <- c(
      "collision_data_year", "price_year", "severity",
      "cost_per_casualty", "cost_per_collision"
    )
    
    ons_cost_form = ons_cost_form |> 
      mutate(cost_per_casualty = as.numeric(cost_per_casualty),
             cost_per_collision = as.numeric(cost_per_collision))
    
  }
  
  if(sheet2get == "Average_value_road_type"){
    
    ons_cost <- readODS::read_ods(tmpfile, sheet = "Average_value_road_type", skip = 3)
    
    ons_cost_form <- ons_cost[,2:6]
    
    names(ons_cost_form) <- c("collision_year", "collision_severity", "Urban", "Rural", "Motorway")
    
  }
  
  if(sheet2get == "Total_value"){
    
    ons_cost_form <- readODS::read_ods(tmpfile, sheet = "Total_value", skip = 3)
    
    
    
  }
  
  if(sheet2get == "Total_value_road_type"){
    
    ons_cost <- readODS::read_ods(tmpfile, sheet = "Total_value_road_type", skip = 3)
    
    # drop first row and keep first 5 columns
    ons_cost_form <- ons_cost[-1, 2:7]
    
    names(ons_cost_form) <- c("collision_year",	"collision_severity",	"Urban",	"Rural",	"Motorways",	"All_roads")
    
    ons_cost_form = ons_cost_form |> 
      mutate(Urban= as.numeric(Urban),
             Rural = as.numeric(Rural),
             Motorways = as.numeric(Motorways),
             All_roads = as.numeric(All_roads))
    
  }
  
  
  return(ons_cost_form)
}

# use function to load in average value road type
ons_tag <- get_ons_cost_data(sheet2get = "Average_value_road_type") |> 
  filter(collision_year == 2024 & collision_severity %in% c("Fatal", "Serious", "Slight")) |> 
  melt(c("collision_year", "collision_severity"), variable.name = "ons_road", value.name = "cost")

# get casualty data for 2024 and add column for fatal
cas_adj_2024 <- get_stats19("2024", type = "casualty") |> 
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) # serious and slight have a column so add one for fatal to give consistency

# get casualty data and process as non adjusted
cas_non_adj_2024 <- get_stats19("2024", type = "casualty") |> 
  mutate(Fatal = if_else(casualty_severity == "Fatal", 1, 0),
         Serious = if_else(casualty_severity == "Serious", 1, 0),
         Slight = if_else(casualty_severity == "Slight", 1, 0)) # serious and slight have a column so add one for fatal to give consistency

# get crash data for 2024 which includes columns for road types
cra_2024 <- get_stats19("2024", type = "collision")

# get ons/TAG cost data sheet RAS4001
ons_tag <- get_ons_cost_data(sheet2get = "Average_value_road_type") |> 
  filter(collision_year == 2024 & collision_severity %in% c("Fatal", "Serious", "Slight")) |> 
  melt(c("collision_year", "collision_severity"), variable.name = "ons_road", value.name = "cost")

# calculate costs with non adjusted
non_adj_tag_check <- cas_non_adj_2024 |> 
  inner_join(cra_2024) |> 
  select(collision_index,collision_year, Fatal, Serious, Slight, first_road_class, urban_or_rural_area) |> 
  mutate(ons_road = if_else(first_road_class == "Motorway", "Motorway", if_else(urban_or_rural_area == "Urban", "Urban", "Rural"))) |> 
  select(collision_year,Fatal,Serious,Slight,ons_road) |> 
  melt(c("collision_year", "ons_road"),variable.name = "collision_severity",value.name = "casualties") |> 
  left_join(ons_tag, by = c("collision_year", "collision_severity", "ons_road")) |> 
  filter(casualties > 0) |> 
  mutate(total_cost = casualties*cost)


# load in official casualty figures to check
url = "https://assets.publishing.service.gov.uk/media/68d3cdeeca266424b221b253/ras0101.ods"

tmpfile <- tempfile(fileext = ".ods")

utils::download.file(url, destfile = tmpfile, mode = "wb")

gov_totals <- readODS::read_ods(tmpfile, sheet = "Casualties", skip = 6, col_names = TRUE)

official_fatal_2024 <- gov_totals$`All road users (killed)`[99]
official_all_cas_2024 <- gov_totals$`All road users (all severities) [note 6]`[99]

# get totals we have calculated
non_adj_fat_cas_check <- sum(non_adj_tag_check[non_adj_tag_check$collision_severity == "Fatal",]$casualties)
non_adj_all_cas_check <- sum(non_adj_tag_check$casualties)

ons_tag_totals <- get_ons_cost_data(sheet2get = "Total_value_road_type") |> 
  filter(collision_year == 2024 & collision_severity %in% c("Fatal", "Serious", "Slight"))

ons_total_mway <- sum(ons_tag_totals$Motorways)
ons_total_all <- sum(ons_tag_totals$All_roads)

non_adj_motorway_costs <- sum(non_adj_tag_check[non_adj_tag_check$ons_road == "Motorway",]$cost,na.rm = TRUE)/1e6
non_adj_total_cost <- sum(non_adj_tag_check$total_cost)/1e6

non_adj_mway_diff <- ons_total_mway-non_adj_motorway_costs
non_adj_total_diff <- ons_total_all-non_adj_total_cost

adj_tag_check <- cas_adj_2024 |> 
  inner_join(cra_2024) |> 
  select(collision_index,collision_year, Fatal = fatal_count, Serious = casualty_adjusted_severity_serious, Slight = casualty_adjusted_severity_slight, first_road_class, urban_or_rural_area) |> 
  mutate(ons_road = if_else(first_road_class == "Motorway", "Motorway", if_else(urban_or_rural_area == "Urban", "Urban", "Rural"))) |> 
  select(collision_year,Fatal,Serious,Slight,ons_road) |> 
  melt(c("collision_year", "ons_road"),variable.name = "collision_severity",value.name = "casualties") |> 
  left_join(ons_tag, by = c("collision_year", "collision_severity", "ons_road")) |> 
  filter(casualties > 0) |> 
  mutate(total_cost = casualties*cost)

adj_fat_cas_check <- sum(adj_tag_check[adj_tag_check$collision_severity == "Fatal",]$casualties)
adj_all_cas_check <- sum(adj_tag_check$casualties)
adj_motorway_costs <- sum(adj_tag_check[adj_tag_check$ons_road == "Motorway",]$cost,na.rm = TRUE)/1e6
adj_total_cost <- sum(adj_tag_check$total_cost)/1e6

adj_mway_diff <- ons_total_mway-adj_motorway_costs
adj_total_diff <- ons_total_all-adj_total_cost

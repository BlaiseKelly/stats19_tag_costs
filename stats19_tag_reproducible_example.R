library(stats19)
library(reshape2) # officially deprecated but melt and dcast are v concise
library(dplyr)
library(utils)
library(readODS)

# get ras4001
url = "https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods"
  
tmpfile <- tempfile(fileext = ".ods")
  
utils::download.file(url, destfile = tmpfile, mode = "wb")

# get table average_value
average_value <- readODS::read_ods(tmpfile, sheet = "Average_value", skip = 5, col_names = FALSE) |> 
  transmute("collision_year" = ...1,
            "collision_severity" = ...3,
            "cost_per_casualty" = as.numeric(...4),
            "cost_per_collision" = as.numeric(...5))

# join with collision data
tag_sev <- cra_2024 |> 
  select(collision_year, collision_severity) |> 
  left_join(average_value, by = c("collision_year", "collision_severity")) |> 
  group_by(collision_severity) |> 
  summarise(casualty_cost_millions = round(sum(cost_per_casualty)/1e6),
            collision_cost_millions = round(sum(cost_per_collision)/1e6))

# get table average_value_road_type
average_value_road_type <- readODS::read_ods(tmpfile, sheet = "Average_value_road_type", skip = 3) |> 
  transmute(collision_year = `Collision data year`,
            collision_severity = Severity,
            built_up = `Built-up roads (£) [note 3]`,
            not_built_up = `Non built-up roads (£) [note 3]`,
            Motorway = `Motorways (£) [note 3]`) |> 
  filter(collision_year == 2024 & collision_severity %in% c("Fatal", "Serious", "Slight")) |> 
  melt(c("collision_year", "collision_severity"), variable.name = "ons_road", value.name = "cost")

# get collision data for 2024 which includes columns for road types
cra_2024 <- get_stats19("2024", type = "collision")

# define road category, first by motorway or not, then speed limit and 3 collisions had no speed data but did have urban or rural, so that also used.
tag_sev_road_type <- cra_2024 |> 
  select(collision_year, collision_severity, first_road_class, speed_limit, urban_or_rural_area) |> 
  mutate(speed_limit = as.numeric(speed_limit)) |> 
  mutate(ons_road = if_else(first_road_class == "Motorway", "Motorway", if_else(speed_limit <= "40", "built_up", "not_built_up"))) |> 
 mutate(ons_road = if_else(is.na(speed_limit) & urban_or_rural_area == "Urban", "built_up",ons_road)) |> 
  mutate(ons_road = if_else(is.na(speed_limit) & urban_or_rural_area == "Rural", "not_built_up",ons_road)) |> 
  left_join(average_value_road_type, by = c("collision_year", "collision_severity", "ons_road")) |> 
  group_by(collision_severity, ons_road) |> 
  summarise(costs_millions = round(sum(cost)/1e6)) |> 
  dcast(collision_severity~ons_road)






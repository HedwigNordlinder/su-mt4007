library(swemaps2)
library(sf)
generate_labour_market_geometry <- function(labour_markets) {

  municipality <- st_transform(municipality, 3006)
  
  
  merged_labour_markets <- merge(municipality, labour_markets, by.x = "kn_kod", by.y = "kod")
  
  
  labour_market_codes <- unique(merged_labour_markets$Kod)
  labour_market_geometries <- c()
  
  for(i in 1:length(labour_market_codes)) {
    constituent_polygons <- merged_labour_markets$geometry[merged_labour_markets$Kod == labour_market_codes[i]]
    labour_market_geometry <- st_union(constituent_polygons)
    labour_market_geometries <- c(labour_market_geometries, labour_market_geometry)
  }
  
  geometry <- st_sfc(labour_market_geometries, crs = 3006)
  
  return(st_sf(data.frame(geometry=geometry, name=labour_market_codes)))
}

generate_adjacency_dataframe <- function(sf_object) {
  
  adjacency_matrix <- st_touches(sf_object, sparse = FALSE)
  
  adjacency_frame <- as.data.frame(adjacency_matrix)
  adjacency_frame$name <- sf_object$name
  return(adjacency_frame)
}



# We tag municipalities by both their labour markets and their regions

tag_county <- function(time_series_frame) {
  
  tagged_time_series_frame <- time_series_frame %>% mutate(county = substr(municipal_code,1,2))
  return(tagged_time_series_frame)
}

tag_municipalities_years <- function(tagging_sheet_path, time_series_frame, time_to_sheet_index) {
  tagged_time_series_frame <- data.frame(matrix(ncol=ncol(time_series_frame) + 1))
  colnames(tagged_time_series_frame) <- c(colnames(time_series_frame),"Kod")  
  times <- unique(time_series_frame$time)
  
  for(i in 1:length(times)) {
    
    current_tagging_frame <- read.xlsx(tagging_sheet_path, sheet= time_to_sheet_index(times[i])) 
    current_tagging_frame <- current_tagging_frame %>% select(kod, Kod)
    current_time_series_frame <- time_series_frame %>% filter(time == times[i])
    tagged_current_time_series_frame <- merge(current_time_series_frame, current_tagging_frame, by.x = "municipal_code", by.y = "kod")
    tagged_time_series_frame <- rbind(tagged_time_series_frame, tagged_current_time_series_frame)
  }
  return(tagged_time_series_frame[2:nrow(tagged_time_series_frame),])
}

municipality_to_la <- function(labour_markets, single_week_observation) {
  
  tagged_observations <- merge(single_week_observation, labour_markets, by.x="municipal_code",by.y="kod")
  
  converted_frame <- tagged_observations %>% group_by(Kod) %>% summarise(cases = sum(cases))
  return(converted_frame)
}

get_cases_adjacent <- function(single_week_observation, sf_object) {
  
  adjacency_frame <- generate_adjacency_dataframe(sf_object)
  
  
  adjacent_cases <- rep(0,nrow(single_week_observation))
  
  
  
  
}

adjacent_cases_all_weeks <- function(cases_frame, labour_market_file, time_to_labour_market_function) {
  
  times = unique(cases_frame$time)
  compound_frame <- data.frame(Kod = c(), cases = c(), adjacent_cases = c(), time = c())
  current_labour_market_index <- time_to_labour_market_function(times[i])
  current_labour_market <- read.xlsx(labour_market_file, sheet = current_labour_market_index)
  
  for(i in 1:length(times)) {
    print(paste0(i,"/",length(times)))
    current_week <- cases_frame %>% filter(time == times[i]) %>% mutate(cases = value) %>% select(-value)
    if(current_labour_market_index != time_to_labour_market_function(times[i])) {
      current_labour_market_index = time_to_labour_market_function(times[i])
      current_labour_market = read.xlsx(labour_market_file, sheet = current_labour_market_index)
    }
    current_week_la <- municipality_to_la(current_labour_market, current_week)
    current_week_la_adjacent_cases <- get_cases_adjacent(current_week_la, generate_labour_market_geometry(current_labour_market))
    current_week_la_adjacent_cases$time <- rep(times[i],nrow(current_week_la_adjacent_cases))
    compound_frame <- rbind(compound_frame, current_week_la_adjacent_cases)
  }
  return(compound_frame)
}

# Problem: We lost two labour market regions
week_on_week_case_ratios <- function(time_series_frame) {
  times <- unique(time_series_frame$time)
  

  first_period_rows <- nrow(time_series_frame[time_series_frame$time == times[1],])
  case_ratios <- rep(1, first_period_rows) 
  adjacent_case_ratios <- rep(1, first_period_rows)
  for(i in 2:length(times)) {
    previous_period <- time_series_frame[time_series_frame$time == times[i-1],]
    current_period <- time_series_frame[time_series_frame$time == times[i],]
    case_ratio <- (previous_period$cases + 1) / (current_period$cases + 1)
    adjacent_case_ratio <- (previous_period$adjacent_cases + 1) / (current_period$adjacent_cases + 1)
    case_ratios <- c(case_ratios, case_ratio)
    adjacent_case_ratios <- c(adjacent_case_ratios, adjacent_case_ratio)
  }
  
  time_series_frame_with_changes <- time_series_frame
  time_series_frame_with_changes$case_ratios <- case_ratios
  time_series_frame_with_changes$adjacent_case_ratios <- adjacent_case_ratios
  return(time_series_frame_with_changes)
}

# Näst på dagordningen: Arbetsmarknadsregionerna / regionerna är dina fixed effects, därefter jämför du prediktionsförmågan mellan dem på Bayesianskt vis

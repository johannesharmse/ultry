# Data Transformer Functions ===================================================

get_distance_from_start <- function(sf_points) {
  
  # Get distance from start for each geometric point
  
  distance <- st_distance(sf_points, lag(sf_points, n = 1), by_element = T)
  distance[1] <- 0
  
  return(cumsum(distance))
  
}


set_distance_units <- function(data) {
  
  # Convert numeric "distance" column to units format, 
  # based on units specified in "units" column
  
  assert_that(all(c("distance", "units") %in% colnames(data)))
  
  data %>% 
    group_by(units) %>% 
    mutate(distance = set_units(distance, 
                                as.character(first(units)), 
                                mode = "standard")) %>% 
    ungroup() %>% 
    select(-units) %>% 
    arrange(distance)
  
}


set_checkpoint_coords <- function(data, route) {
  
  # Find coordinates of each checkpoint in `data` 
  # based off of its specified "distance" value
  
  assert_that(c("distance") %in% colnames(data))
  assert_that(all(c("distance", "geometry") %in% colnames(route)))
  
  data <- data %>%
    rowwise() %>% 
    mutate(route_point_id = which.min(abs(distance - route$distance)),
           geometry = route$geometry[route_point_id]) %>% 
    ungroup()
  st_geometry(data) <- "geometry"
  
  return(data)
  
}


set_leg_segments <- function(data, route_points) {
  
  # Get geometric line string segments of each leg 
  # on route by using the coordinates of each checkpoint
  
  assert_that(all(c("route_point_id", "geometry") %in% colnames(data)))
  assert_that("geometry" %in% colnames(route_points))
  
  data %>% 
    rename(end_point_idx = route_point_id) %>% 
    mutate(start_point_idx = coalesce(lag(end_point_idx, 1), 1)) %>% 
    rowwise() %>% 
    mutate(
      geometry_segment = route_points[start_point_idx:end_point_idx, "geometry"] %>% 
        summarise(do_union = F) %>% 
        st_cast("LINESTRING") %>% 
        st_geometry()
    ) %>% 
    ungroup()
  
}


get_route_leg_stats <- function(data) {
  
  # Calculate statistics for each course leg. 
  # Statistics include leg number, distance, total ascent 
  # and total descent
  
  assert_that(all(c("ele", "leg_number", "distance") %in% colnames(data)))
  
  ZERO_UNIT <- set_units(0, deparse_unit(data$ele), mode = "standard")
  
  data %>% 
    as_tibble() %>% 
    mutate(id = paste("Leg", leg_number, sep = " "), 
           ele_change = coalesce(ele - lag(ele, 1), ZERO_UNIT)) %>% 
    group_by(id) %>% 
    summarise(
      leg_number = first(leg_number),
      distance = max(distance),
      total_ascent = sum(ele_change[ele_change > ZERO_UNIT]),
      total_descent = abs(sum(ele_change[ele_change < ZERO_UNIT]))
    ) %>% 
    arrange(leg_number) %>% 
    mutate(distance = distance - coalesce(lag(distance, 1), ZERO_UNIT))
  
}


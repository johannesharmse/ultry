# Global App Data ==============================================================


# Functions - Data Import and Transform ----------------------------------------

source("utils/file-read.R", local = TRUE)
source("utils/data-transform.R", local = TRUE)


# Import -----------------------------------------------------------------------

# expected file columns and their formats
EXPECTED_COLS <- list(
  times = list(
    id          = list(type = "number"),
    name        = list(type = "text"),
    category    = list(type = "category"),
    category2   = list(type = "category"),
    time        = list(type = "hms"),
    checkpoint  = list(type = "category")
  ),
  checkpoints = list(
    checkpoint  = list(type = "category"),
    distance    = list(type = "number"),
    units       = list(type = "category", acceptedValues = c("km", "mi"))
  )
)
# import data files
# historic times across checkpoints
TIMES <- import_table("data/times.csv", format = EXPECTED_COLS$times)
# checkpoint locations by race distance
CHECKPOINTS <- import_table("data/checkpoints.csv", format = EXPECTED_COLS$checkpoints)
# race course points
MAP_POINTS <- import_gpx("data/map.gpx")


# Data Transformation ----------------------------------------------------------

# create course line string from points
MAP_ROUTE <- MAP_POINTS %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

MAP_POINTS <- MAP_POINTS %>% 
  mutate(
    # calculate distance from start for each map point
    distance = get_distance_from_start(st_geometry(.)),
    # set elevation units
    ele = set_units(ele, deparse_unit(distance), mode = "standard")
  )

CHECKPOINTS <- CHECKPOINTS %>% 
  set_distance_units() %>% 
  # get coordinates for each checkpoint
  set_checkpoint_coords(route = MAP_POINTS) %>% 
  mutate(station_number = row_number()) %>% 
  rename(
    distance_cum = distance
  )

# calibrate last checkpoint to be at finish line / last map point
mask <- CHECKPOINTS$station_number == max(CHECKPOINTS$station_number)
last_route_point_idx <- nrow(MAP_POINTS)
CHECKPOINTS[mask, "route_point_id"] <- MAP_POINTS$track_seg_point_id[last_route_point_idx]
CHECKPOINTS$geometry[mask] <- MAP_POINTS$geometry[last_route_point_idx]

# categorize course points by leg
MAP_POINTS <- MAP_POINTS %>% 
  left_join(
    CHECKPOINTS %>% 
      as_tibble() %>% 
      select(
        route_point_id, 
        station_number
      ),
    by = c("track_seg_point_id" = "route_point_id")
  ) %>% 
  rename(leg_number = station_number) %>% 
  arrange(track_seg_point_id) %>% 
  fill(leg_number, .direction = "updown")

# get line string for each course leg
CHECKPOINTS <- set_leg_segments(CHECKPOINTS, MAP_POINTS) %>% 
  arrange(station_number) %>% 
  mutate(
    start_checkpoint = lag(as.character(checkpoint)),
    start_distance = lag(distance_cum)
  )
CHECKPOINTS$start_checkpoint[1] <- "Start"
CHECKPOINTS$start_distance[1] <- set_units(0, 
                                           deparse_unit(CHECKPOINTS$start_distance), 
                                           mode = "standard")

# get leg stats - distance, elevation gain, elevation loss
CHECKPOINTS <- CHECKPOINTS %>%
  left_join(
    get_route_leg_stats(MAP_POINTS) %>% 
      mutate(distance = set_units(distance, km)),
    by = c("station_number" = "leg_number")
  )

# fill missing time data for runners
TIMES <- TIMES %>% 
  right_join(
    TIMES %>% 
      expand(id, checkpoint),
    by = c("id", "checkpoint")
  ) %>% 
  group_by(id) %>% 
  fill(-time, .direction = "downup") %>% 
  left_join(
    CHECKPOINTS %>% 
      as_tibble() %>% 
      select(checkpoint, station_number, distance_cum, distance),
    by = "checkpoint"
  ) %>% 
  rename(race_distance = distance_cum) %>% 
  group_by(id) %>% 
  arrange(station_number) %>% 
  # calculate leg statistics for each runner
  mutate(
    leg_time_start = lag(time),
    leg_time = time - lag(time),
    leg_time_start = if_else(station_number == 1 & !is.na(time), period(0), leg_time_start),
    leg_time = if_else(station_number == 1 & !is.na(time), time, leg_time),
    pace = set_units(period_to_seconds(leg_time), s) / distance
  ) %>% 
  ungroup() %>% 
  rename(leg_time_end = time)
# set pace units
units(TIMES$pace) <- make_units(min/km)
# format leg statistics
TIMES <- TIMES %>%
  filter(
    !is.na(leg_time) & 
      leg_time > 0
  ) %>% 
  mutate(
    race_distance = as.numeric(race_distance),
    leg_time_start = period_to_seconds(leg_time_start), 
    leg_time = period_to_seconds(leg_time),
    race_time = period_to_seconds(leg_time_end),
    leg_time_str = paste(leg_time %/% 3600, "H", 
                         leg_time %% 3600 %/% 60, "m", 
                         sep = ""),
    leg_pace_str = sprintf("%.0f:%02.0f", 
                           as.numeric(pace) %/% 1,
                           round(as.numeric(pace) %% 1 * 60)),
    leg_distance_str = sprintf("%.1fkm",
                               as.numeric(set_units(distance, km))),
    race_time_str = sprintf("%1.0fH%1.0fm", 
                            race_time %/% 3600, 
                            race_time %% 3600 %/% 60),
    race_distance_str = sprintf("%.1fkm", race_distance)
  )
# ensure single checkpoint time per runner
TIMES <- TIMES %>% 
  group_by(station_number, id) %>% 
  filter(leg_time == max(leg_time)) %>% 
  ungroup()

TIMES <- TIMES %>% 
  # get runner finish status
  group_by(id) %>% 
  mutate(
    finished = max(TIMES$station_number) %in% station_number
  ) %>% 
  # get runner position per checkpoint
  group_by(checkpoint) %>%
  mutate(
    position = rank(race_time)
  ) %>% 
  ungroup()

# calculate number of finishers
N_FINISHERS <- TIMES %>%
  filter(station_number == max(station_number) & finished) %>%
  nrow()


# Data Expose ------------------------------------------------------------------

# specify global variables to keep
GLOBAL <- list(
  TIMES = TIMES,
  N_FINISHERS = N_FINISHERS,
  CHECKPOINTS = CHECKPOINTS,
  MAP_ROUTE = MAP_ROUTE,
  MAP_POINTS = MAP_POINTS
)

# clean up
rm(
  EXPECTED_COLS,
  TIMES,
  CHECKPOINTS,
  MAP_POINTS,
  MAP_ROUTE,
  mask,
  last_route_point_idx,
  N_FINISHERS
)

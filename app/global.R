# App Entry Point ==============================================================


# Libraries --------------------------------------------------------------------

library(assertthat)
library(bsicons)
library(bslib)
library(dplyr)
library(echarts4r)
library(lubridate)
library(mapboxer)
library(purrr)
library(reactable)
library(sass)
library(sf)
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(sparklines)
library(tidyr)
library(units)


# Config Variables -------------------------------------------------------------

MAPBOX_TOKEN <- Sys.getenv("MAPBOX_TOKEN")


# Theme ------------------------------------------------------------------------

# colors
PALETTE <- list(
  PINK        = "#fa0050",
  RED         = "#FF0B0B",
  LIGHT_RED   = "#c47e6c",
  DARK_BLUE   = "#051c2c",
  LIGHT_BLUE  = "#1AA1DC",
  WHITE       = "#ffffff",
  GREY        = "#AEB5BC",
  BLACK       = "#000000"
)

# Tables - Leg stats & Results
options(
  reactable.theme = reactableTheme(
    color = PALETTE$WHITE,
    backgroundColor = PALETTE$DARK_BLUE,
    borderColor = PALETTE$GREY,
    highlightColor = PALETTE$PINK,
    inputStyle = list(backgroundColor = PALETTE$DARK_BLUE),
    selectStyle = list(backgroundColor = PALETTE$PINK),
    pageButtonHoverStyle = list(backgroundColor = PALETTE$LIGHT_BLUE),
    pageButtonActiveStyle = list(backgroundColor = PALETTE$GREY),
    searchInputStyle = list("&:focus" = list(borderColor = PALETTE$PINK), 
                            borderColor = PALETTE$WHITE)
  )
)


# Functions - Utility ----------------------------------------------------------

source("utils/helpers-units.R")
source("utils/ui-components.R")


# Modules ----------------------------------------------------------------------

# load default race data
GLOBAL_FILE_PATH <- "data/GLOBAL.RDS"
if (file.exists(GLOBAL_FILE_PATH)) {
  
  message("Loading default race data...")
  GLOBAL <- readRDS(GLOBAL_FILE_PATH)
  assert_that(
    all(
      c("TIMES", "N_FINISHERS", "CHECKPOINTS", "MAP_ROUTE", "MAP_POINTS") %in% 
        names(GLOBAL)
    )
  )
  
} else {
  
  source("modules/global_data.R")
  
}

source("modules/mod_above_the_fold.R")
source("modules/mod_route_times.R")
source("modules/mod_route_elevation.R")
source("modules/mod_route_map.R")
source("modules/mod_leg_info.R")
source("modules/mod_leg_info_goal.R")
source("modules/mod_leg_table.R")
source("modules/mod_results_table.R")


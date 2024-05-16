# Module - Leg Cards in Course Details =========================================


# UI ---------------------------------------------------------------------------

mod_leg_info_ui <- function(id) {
  
  ns <- NS(id)
  
  
  # Containers - Cards ---------------------------------------------------------
  
  div(
    class = "d-flex flex-wrap gap-4", 
    # Leg Name
    leg_info_box(
      uiOutput(ns("stat_leg_name")),
      "Leg",
      "map"
    ),
    # Leg Distance
    leg_info_box(
      uiOutput(ns("stat_distance")),
      "Distance",
      "arrows"
    ),
    # Leg Elevation Gain
    leg_info_box(
      uiOutput(ns("stat_ascent")),
      "Elevation Gain",
      "graph-up"
    ),
    # Leg Elevation Loss
    leg_info_box(
      uiOutput(ns("stat_descent")),
      "Elevation Loss",
      "graph-down"
    )
  )
  
}



# Server ------------------------------------------------------------

mod_leg_info <- function(id, state, GLOBAL) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    
    
    # Constants ----------------------------------------------------------------
    
    # checkpoint data formatted
    data <- GLOBAL$CHECKPOINTS
    data <- data %>% 
      mutate(
        distance = paste(round(as.numeric(distance)), "km", sep = ""),
        total_ascent = paste(round(as.numeric(total_ascent)), "m", sep = ""),
        total_descent = paste(round(as.numeric(total_descent)), "m", sep = "")
      ) %>%
      # add route stats
      bind_rows(
        data %>%
          summarise(
            station_number = -1,
            checkpoint = "Full Course",
            distance = paste(round(sum(as.numeric(distance))), "km", sep = ""),
            total_ascent = paste(round(sum(as.numeric(total_ascent))), "m", sep = ""),
            total_descent = paste(round(sum(as.numeric(total_descent))), "m", sep = "")
          )
      )
    
    
    # Reactive Values ----------------------------------------------------------
    
    leg_name <- reactiveVal()
    distance <- reactiveVal()
    ascent <- reactiveVal()
    descent <- reactiveVal()
    
    
    # Observers ----------------------------------------------------------------
    
    # update card values based on selected leg
    observeEvent(state$selected_segment_idx, {
      
      mask <- data$station_number == coalesce(state$selected_segment_idx, -1)
      leg_name(data$checkpoint[mask])
      distance(data$distance[mask])
      ascent(data$total_ascent[mask])
      descent(data$total_descent[mask])
      
    }, ignoreNULL = F)
    
    
    # Update card values -------------------------------------------------------
    
    output$stat_leg_name <- renderUI({ render_leg_stat(leg_name()) })
    output$stat_distance <- renderUI({ render_leg_stat(distance()) })
    output$stat_ascent <- renderUI({ render_leg_stat(ascent()) })
    output$stat_descent <- renderUI({ render_leg_stat(descent()) })
    
  }
  
  # return module server
  moduleServer(id, server)
  
}



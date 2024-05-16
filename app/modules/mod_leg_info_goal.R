# Module - Goal Cards in Race Projection =======================================


# UI ---------------------------------------------------------------------------

mod_leg_info_goal_ui <- function(id) {
  
  ns <- NS(id)
  
  
  # Container - Cards ----------------------------------------------------------
  
  div(
    class = "d-flex flex-wrap gap-4",
    # Race Goal Time
    div(
      leg_info_box(
        uiOutput(ns("goal_time")),
        "Goal Time",
        "stopwatch"
      )
    ),
    # Race Goal Pace
    div(
      leg_info_box(
        uiOutput(ns("goal_pace")),
        "Goal Pace",
        "speedometer"
      )
    )
  )
  
}


# Server -----------------------------------------------------------------------

mod_leg_info_goal <- function(id, state, GLOBAL) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    
    
    # Reactive Values ----------------------------------------------------------
    
    goal_time <- reactiveVal()
    goal_pace <- reactiveVal()
    
    
    # Observers ----------------------------------------------------------------
    
    # update card values when leg is selected or goal input is updated
    observeEvent(c(state$selected_segment_idx, state$leg_quantile_durations) , {
      
      req(state$leg_quantile_durations)
      
      # get course values or selected leg values
      mask <- if (is.null(state$selected_segment_idx)) {
        state$leg_quantile_durations$station_number == -1
      } else {
        state$leg_quantile_durations$station_number == state$selected_segment_idx
      }
      goal_time(state$leg_quantile_durations$leg_time_str[mask])
      goal_pace(state$leg_quantile_durations$leg_pace_str[mask])
      
    }, ignoreNULL = F)
    
    
    # Render card values -------------------------------------------------------
    
    output$goal_time <- renderUI({ render_leg_stat(goal_time()) })
    output$goal_pace <- renderUI({ render_leg_stat(goal_pace()) })
    
  }
  
  # return module server
  moduleServer(id, server)
  
}



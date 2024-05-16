# Module - Above the Fold ======================================================


# UI ---------------------------------------------------------------------------

mod_above_the_fold_ui <- function(id) {
  
  ns <- NS(id)
  
  
  # Container ------------------------------------------------------------------
  
  tags$section(
    class = "container-fluid d-flex align-items-center justify-content-center",
    style = "min-height: 80vh; background: url(assets/images/hero-cover.jpg);",
    
    
    # Cards - Goal Time & Goal Pace --------------------------------------------
    
    div(
      class = "d-flex flex-wrap gap-3 justify-content-center",
      # Goal Time
      div(
        leg_info_box(
          uiOutput(ns("goal_time")),
          "Goal Time",
          "stopwatch",
          edit = TRUE,
          edit_button = ns("edit_goal_time_button"),
          confirm_button = ns("confirm_goal_time_button"),
          edit_input = ns("goal_time_input")
        )
      ),
      # Goal Pace
      div(
        leg_info_box(
          uiOutput(ns("goal_pace")),
          "Goal Pace",
          "speedometer",
          edit = TRUE,
          edit_button = ns("edit_goal_pace_button"),
          confirm_button = ns("confirm_goal_pace_button"),
          edit_input = ns("goal_pace_input")
        )
      )
    )
  )
  
}


# Server -----------------------------------------------------------------------

mod_above_the_fold <- function(id, state, GLOBAL) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    
    
    # Constants ----------------------------------------------------------------
    
    # race time distribution function
    finisher_mask <- GLOBAL$TIMES$station_number == max(GLOBAL$TIMES$station_number) & 
                      GLOBAL$TIMES$finished & 
                      GLOBAL$TIMES$leg_time > 0
    race_time_distribution <- ecdf(GLOBAL$TIMES$race_time[finisher_mask])
    # course total distance
    race_distance <- max(GLOBAL$TIMES$race_distance)
    
    
    # Reactive Values ----------------------------------------------------------
    
    # non-interactive card values
    goal_time <- reactiveVal()
    goal_pace <- reactiveVal()
    
    
    # Observers ----------------------------------------------------------------
    
    # press edit button on goal time card
    observeEvent(input$edit_goal_time_button, {
      
      shinyjs::hide("goal_time")
      shinyjs::show("goal_time_input")
      shinyjs::hide("edit_goal_time_button")
      shinyjs::show("confirm_goal_time_button")
      
    }, ignoreInit = TRUE)
    
    # press edit button on goal pace card
    observeEvent(input$edit_goal_pace_button, {
      
      shinyjs::hide("goal_pace")
      shinyjs::show("goal_pace_input")
      shinyjs::hide("edit_goal_pace_button")
      shinyjs::show("confirm_goal_pace_button")
      
    }, ignoreInit = TRUE)
    
    # press confirm button on goal time card
    observeEvent(input$confirm_goal_time_button, {
      
      shinyjs::hide("goal_time_input")
      shinyjs::show("goal_time")
      shinyjs::hide("confirm_goal_time_button")
      shinyjs::show("edit_goal_time_button")
      
      state$percentile_input <- race_time_to_percentile(input$goal_time_input, 
                                                        race_time_distribution)
      
    }, ignoreInit = TRUE)
    
    # press confirm button on goal pace card
    observeEvent(input$confirm_goal_pace_button, {
      
      shinyjs::hide("goal_pace_input")
      shinyjs::show("goal_pace")
      shinyjs::hide("confirm_goal_pace_button")
      shinyjs::show("edit_goal_pace_button")
      
      state$percentile_input <- race_pace_to_percentile(input$goal_pace_input, 
                                                        race_distance, 
                                                        race_time_distribution)
      
    }, ignoreInit = TRUE)
    
    # calculate goal time and pace for each leg
    observeEvent(state$percentile_input, {
      
      # calculate goal stats for each leg
      state$leg_quantile_durations <- GLOBAL$TIMES %>% 
        # only use finishers to determine goal time/pace
        filter(
          leg_time > 0 & 
            finished
        ) %>% 
        group_by(station_number) %>% 
        # only calculate leg goal stats where significant # of observations
        filter(
          n() >= GLOBAL$N_FINISHERS * 0.7
        ) %>% 
        summarise(
          checkpoint = first(checkpoint),
          race_distance_str = first(race_distance_str),
          distance = first(distance),
          race_distance = first(race_distance),
          quantile_cum = quantile(race_time, state$percentile_input, na.rm = T),
          quantile = quantile(leg_time, state$percentile_input, na.rm = T)
        ) %>% 
        mutate(
          pace = set_units(set_units(quantile, s) / distance, min/km),
          leg_time_str = paste(quantile %/% 3600, "H", 
                               quantile %% 3600 %/% 60, "m", 
                               sep = ""),
          leg_pace_str = sprintf("%.0f:%02.0f", 
                                 as.numeric(pace) %/% 1,
                                 round(as.numeric(pace) %% 1 * 60)),
          leg_distance_str = sprintf("%.1fkm",
                                     as.numeric(set_units(distance, km))),
          race_time_str = paste(quantile_cum %/% 3600, "H", 
                                quantile_cum %% 3600 %/% 60, "m", 
                                sep = "")
        )
      
      # calculate goal stats for course as whole
      state$leg_quantile_durations <- state$leg_quantile_durations %>% 
        bind_rows(
          # route stats
          state$leg_quantile_durations %>% 
            filter(station_number == max(station_number)) %>% 
            mutate(
              station_number = -1,
              checkpoint = "Full Course",
              distance = set_units(race_distance, km),
              quantile = quantile_cum,
              pace = set_units(set_units(quantile, s) / distance, min/km),
              leg_time_str = paste(quantile %/% 3600, "H", 
                                   quantile %% 3600 %/% 60, "m", 
                                   sep = ""),
              leg_pace_str = sprintf("%.0f:%02.0f", 
                                     as.numeric(pace) %/% 1,
                                     round(as.numeric(pace) %% 1 * 60)),
              leg_distance_str = sprintf("%.1fkm",
                                         as.numeric(set_units(distance, km))),
              race_time_str = paste(quantile_cum %/% 3600, "H", 
                                    quantile_cum %% 3600 %/% 60, "m", 
                                    sep = "")
            )
        )
      
      
      # update card input box values for goal race time and race pace
      race_row <- state$leg_quantile_durations[state$leg_quantile_durations$station_number == -1, ]
      # goal race time
      updateTextInput(
        session,
        "goal_time_input",
        value = sprintf("%1.0f:%1.0f", 
                        race_row$quantile %/% 3600,
                        race_row$quantile %% 3600 %/% 60)
      )
      # goal race pace
      updateTextInput(
        session,
        "goal_pace_input",
        value = race_row$leg_pace_str
      )
      
    })
    
    
    # Render Card Non-Input Values - Goal Time & Goal Pace ---------------------
    
    # goal race time
    output$goal_time <- renderUI({ 
      render_leg_stat(state$leg_quantile_durations$leg_time_str[
          state$leg_quantile_durations$station_number == -1
        ]) 
      })
    # goal race pace
    output$goal_pace <- renderUI({ 
      render_leg_stat(state$leg_quantile_durations$leg_pace_str[
        state$leg_quantile_durations$station_number == -1
        ])
      })
    
  }
  
  # return module server
  moduleServer(id, server)
  
}

# App Server ===================================================================

server <- function(input, output, session) {
  
  
  # Initial State Values -------------------------------------------------------
  
  state <- reactiveValues(
    selected_segment_idx = NULL,
    leg_quantile_durations = NULL, 
    percentile_input = 0.5
  )
  
  
  # Modules --------------------------------------------------------------------
  
  mod_above_the_fold("above_the_fold", state, GLOBAL)
  mod_route_times("times", state, GLOBAL)
  mod_route_map("map", state, GLOBAL)
  mod_route_elevation("elevation", state, GLOBAL)
  mod_leg_info("leg_info", state, GLOBAL)
  mod_leg_info_goal("leg_info_goal", state, GLOBAL)
  mod_leg_table("leg_table", state, GLOBAL)
  mod_results_table("results_table", state, GLOBAL)
  
  
  # Zoom Buttons Interaction ---------------------------------------------------
  
  # reset zoom/selection when course leg is selected
  observeEvent(c(input$reset_zoom_course, input$reset_zoom_results), {
    state$selected_segment_idx <- NULL
  })
  # toggle zoom buttons' disabled state
  observeEvent(is.null(state$selected_segment_idx), {
    toggleState(id = "reset_zoom_course", condition = !is.null(state$selected_segment_idx))
    toggleState(id = "reset_zoom_results", condition = !is.null(state$selected_segment_idx))
  })
  
}
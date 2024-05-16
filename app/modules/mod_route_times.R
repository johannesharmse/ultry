# Module - Historic Times Chart ================================================


# UI ---------------------------------------------------------------------------

mod_route_times_ui <- function(id) {
  
  ns <- NS(id)
  
  # Times Line Chart -----------------------------------------------------------
  echarts4rOutput(ns("times"), width = "100%", height = "100%")
  
}


# Server -----------------------------------------------------------------------

mod_route_times <- function(id, state, GLOBAL) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    
    
    # Constants ----------------------------------------------------------------
    
    df <- GLOBAL$TIMES %>% 
      rowwise() %>% 
      mutate(tooltip = as.character(
        route_time_dp_tooltip(
          leg_name = checkpoint, 
          race_distance = race_distance_str, 
          leg_distance = leg_distance_str,
          leg_pace = leg_pace_str, 
          leg_duration = leg_time_str, 
          race_duration = race_time_str
          )
        )) %>% 
      group_by(id) %>% 
      mutate(color = if_else(finished, "#0000ff", "#ff000050"))
    
    
    # Observers ----------------------------------------------------------------
    
    # zoom in/out on times chart when leg is selected elsewhere
    observeEvent(state$selected_segment_idx, {
      
      if (is.null(state$selected_segment_idx)) {
        
        # zoom out for full race times
        echarts4rProxy(ns("times")) %>%
          e_dispatch_action_p("dataZoom", 
                              dataZoomIndex = 0, 
                              startValue = 0, 
                              endValue = max(GLOBAL$CHECKPOINTS$distance_cum))
        echarts4rProxy(ns("times")) %>% 
          e_dispatch_action_p("dataZoom", 
                              dataZoomIndex = 1, 
                              startValue = 0, 
                              endValue = max(df$race_time))
        
      } else {
        
        # zoom in on leg selected
        # y-axis zoom
        series_length <- GLOBAL$CHECKPOINTS %>% 
          filter(station_number %in% c(state$selected_segment_idx - 1, 
                                       state$selected_segment_idx)) %>% 
          as_tibble() %>% 
          select(distance_cum) %>% 
          unlist() %>% 
          { c(min(.), max(.)) }
        padding <- (series_length[2] - series_length[1]) * 0.15
        series_length[1] <- series_length[1] - padding
        series_length[2] <- series_length[2] + padding
        # x-axis zoom
        series_length_2 <- df %>% 
          filter(station_number %in% c(state$selected_segment_idx - 1, 
                                       state$selected_segment_idx)) %>% 
          as_tibble() %>% 
          select(race_time) %>% 
          unlist() %>% 
          { c(min(.), max(.)) }
        padding <- (series_length_2[2] - series_length_2[1]) * 0.15
        series_length_2[1] <- series_length_2[1] - padding
        series_length_2[2] <- series_length_2[2] + padding
        
        echarts4rProxy(ns("times")) %>% 
          e_dispatch_action_p("dataZoom", 
                              dataZoomIndex = 0, 
                              startValue = series_length[1], 
                              endValue = series_length[2])
        echarts4rProxy(ns("times")) %>% 
          e_dispatch_action_p("dataZoom", 
                              dataZoomIndex = 1, 
                              startValue = series_length_2[1], 
                              endValue = series_length_2[2])
        
      }
      
    }, ignoreNULL = FALSE)
    
    
    # Render Times Line Chart --------------------------------------------------
    
    output$times <- renderEcharts4r({
      
      req(state$leg_quantile_durations)
      
      # finishers - blue lines
      df %>% 
        filter(finished) %>% 
        e_chart(race_time, dispose = FALSE) %>% 
        e_line(
          race_distance,
          bind = tooltip,
          legend = TRUE,
          name = "Finished",
          symbol = "none",
          emphasis = list(disabled = TRUE),
          z = 2,
          lineStyle = list(color = PALETTE$LIGHT_BLUE, 
                           opacity = 1, 
                           width = 0.35),
          tooltip = list(trigger = "item", 
                         formatter = htmlwidgets::JS('function (params) {
                                                        return params.name;
                                                     }'))
        ) %>% 
        # DNFs - red lines
        e_data(df %>% filter(!finished), race_time) %>% 
        e_line(
          race_distance,
          bind = tooltip,
          legend = TRUE,
          name = "DNF",
          symbol = "none",
          z = 3,
          lineStyle = list(color = PALETTE$RED, width = 0.35),
          itemStyle = list(color = PALETTE$RED),
          tooltip = list(trigger = "item", 
                         formatter = htmlwidgets::JS('function (params) {
                                                       return params.name;
                                                      }'))
        ) %>% 
        # Goal Time - thick, light blue line
        e_data(
          state$leg_quantile_durations %>%
            arrange(station_number) %>% 
            mutate(
              race_time = quantile_cum
            ) %>% 
            rowwise() %>% 
            mutate(tooltip = as.character(
              route_time_dp_tooltip(
                leg_name = checkpoint, 
                race_distance = race_distance_str, 
                leg_distance = leg_distance_str,
                leg_pace = leg_pace_str, 
                leg_duration = leg_time_str, 
                race_duration = race_time_str
              )
            )), 
          race_time
        ) %>% 
        e_line(
          race_distance,
          bind = tooltip,
          symbol = "emptyCircle",
          symbolSize = 6,
          z = 4,
          legend = TRUE,
          name = "Goal",
          itemStyle = list(color = PALETTE$LIGHT_BLUE),
          lineStyle = list(color = PALETTE$WHITE),
          tooltip = list(trigger = "item", 
                         formatter = htmlwidgets::JS('function (params) {
                                                        return params.name;
                                                     }'))
        ) %>% 
        e_x_axis(
          axisLabel = list(
                    formatter = htmlwidgets::JS(
                      'function (value) {
                        if ( value.componentType== "markLine" ) value = value.value;
                        return Math.floor(value / 3600).toString() + "H";
                      }'
                    )
                  ),
                 splitLine = list(show = FALSE)
        ) %>% 
        e_y_axis(
          axisLabel = list(showMinLabel = FALSE, 
                           showMaxLabel = TRUE, 
                           formatter = toKmString),
          splitLine = list(show = FALSE)
        ) %>% 
        e_tooltip() %>% 
        e_legend(textStyle = list(color = PALETTE$GREY)) %>% 
        e_datazoom(y_index = c(0,1), filterMode = "none", toolbox = FALSE, show = FALSE) %>% 
        e_datazoom(x_index = c(0,1), filterMode = "none", toolbox = FALSE, show = FALSE)
      
    })
    
  }
  
  # return module server
  moduleServer(id, server)
  
}



# Module - Course Elevation Area Chart =========================================


# UI ---------------------------------------------------------------------------

mod_route_elevation_ui <- function(id) {
  
  ns <- NS(id)
  
  
  # Container - Course Elevation Area Chart ------------------------------------
  
  div(
    style = "position: relative;",
    echarts4rOutput(ns("elevation"), width = "100%", height = "240px")
  )
  
}


# Server -----------------------------------------------------------------------

mod_route_elevation <- function(id, state, GLOBAL) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    
    
    # Constants ----------------------------------------------------------------
    
    # chart data
    df <- GLOBAL$MAP_POINTS %>% 
      select(leg_number, distance, ele) %>% 
      mutate(
        distance = as.numeric(distance) * 1e-3,
        opacities = 0.1
      )
    
    
    # Observers ----------------------------------------------------------------
    
    # set state when leg is selected on elevation chart
    observeEvent(input$selected_segment_idx, {
      
      state$selected_segment_idx <- input$selected_segment_idx
      
    })
    
    # zoom in/out on elevation chart when leg is selected elsewhere
    observeEvent(state$selected_segment_idx, {
      
      if (is.null(state$selected_segment_idx)) {
        
        # zoom out for full course
        echarts4rProxy(ns("elevation")) %>% 
          e_dispatch_action_p("dataZoom", 
                              dataZoomIndex = 0, 
                              startValue = 0, 
                              endValue = max(df$distance))
        
      } else {
        
        # zoom in to highlight leg
        series_length <- df %>% 
          filter(leg_number == state$selected_segment_idx) %>% 
          as_tibble() %>% 
          select(distance) %>% 
          unlist() %>% 
          as.numeric() %>% 
          { c(min(.), max(.)) }
        
        echarts4rProxy(ns("elevation")) %>% 
          e_dispatch_action_p("dataZoom", 
                              dataZoomIndex = 0, 
                              startValue = series_length[1], 
                              endValue = series_length[2])
        
      }
      
    }, ignoreNULL = FALSE)
    
    
    # Render Elevation Chart ---------------------------------------------------
    
    output$elevation <- renderEcharts4r({
      
      df %>% 
        group_by(leg_number) %>%  
        e_chart(distance, dispose = FALSE) %>% 
        e_line(
          ele, 
          legend = FALSE,
          symbol = "none",
          areaStyle = list(opacity = 0.1, color = PALETTE$PINK),
          triggerLineEvent = TRUE,
          emphasis = list(disabled = FALSE, focus = "series"),
          lineStyle = list(color = PALETTE$PINK)
        ) %>%
        e_datazoom(filterMode = "none", toolbox = FALSE, show = FALSE) %>%
        e_y_axis(
          show = TRUE, 
          min = 0, 
          max = "dataMax",
          splitLine = list(show = FALSE),
          axisLabel = list(showMinLabel = FALSE, 
                           showMaxLabel = TRUE, 
                           formatter = toMString)
        ) %>%
        e_x_axis(
          min = 0, 
          max = "dataMax",
          splitLine = list(show = FALSE),
          axisLabel = list(showMinLabel = FALSE, 
                           showMaxLabel = TRUE, 
                           formatter = toKmString)
        ) %>%
        e_on(
          list(seriesType = "line"),
          sprintf(
            "function(e){ 
              Shiny.setInputValue('%s', e.seriesIndex + 1, {priority: 'event'});
            }",
            ns('selected_segment_idx')
          ),
          event = "click"
        )
      
    })
    
  }
  
  # return module server
  moduleServer(id, server)
  
}



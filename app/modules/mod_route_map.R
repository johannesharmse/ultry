# Module - Course Map ==========================================================


# UI ---------------------------------------------------------------------------

mod_route_map_ui <- function(id) {
  
  ns <- NS(id)
  
  
  # Map ------------------------------------------------------------------------
  
  mapboxerOutput(ns("map"), width = "100%", height = "100%")
  
}


# Server -----------------------------------------------------------------------

mod_route_map <- function(id, state, GLOBAL) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    
    
    # Constants ----------------------------------------------------------------
    
    tooltip_content <- "<div><div><b>Leg {{station_number}}</b></div></br>
                        <div>From:</div><div>{{start_checkpoint}} @ {{start_distance}} km</div>
                        <div>To:</div><div>{{checkpoint}} @ {{distance_cum}} km</div></div>"
    base_map_bbox <- GLOBAL$MAP_ROUTE %>% st_bbox()
    padding <- list(top = 25, bottom = 25, left = 25, right = 25)
    data <- GLOBAL$CHECKPOINTS %>% 
      st_set_geometry("geometry_segment") %>% 
      mutate(
        color = PALETTE$PINK, 
        opacity = 1,
        geometry = NULL
      )
    
    
    # Observers ----------------------------------------------------------------
    
    # set state when leg is selected on map
    observeEvent(input$map_onclick, {
      
      state$selected_segment_idx <- input$map_onclick$props$station_number
      
    })
    
    # zoom in/out on map when leg is selected elsewhere
    observeEvent(state$selected_segment_idx, {
      
      if (is.null(state$selected_segment_idx)) {
        
        # zoom out for full course
        map <- mapboxer_proxy(ns("map")) %>% 
          set_data(data = data, source_id = "route") %>% 
          fit_bounds(bounds = base_map_bbox, padding = padding)
        
      } else {
        
        # zoom in to highlight leg
        bbox <- GLOBAL$MAP_POINTS %>% 
          filter(leg_number == state$selected_segment_idx) %>% 
          st_bbox()
        map <- mapboxer_proxy(ns("map")) %>% 
          set_data(data = data %>% 
                     mutate(color = if_else(
                       station_number == state$selected_segment_idx,
                       PALETTE$PINK,
                       PALETTE$GREY)), 
                   source_id = "route") %>% 
          fit_bounds(bounds = bbox, padding = padding)
        
      }
      
      # update map
      map %>% 
        update_mapboxer()
      
    }, ignoreNULL = FALSE)
    
    
    # Render Map ---------------------------------------------------------------
    
    output$map <- renderMapboxer({
      
      mapboxer(style = "mapbox://styles/mapbox/satellite-v9", 
               token = MAPBOX_TOKEN) %>% 
        add_line_layer(
          id = "route",
          line_color = c("get", "color"),
          source = as_mapbox_source(data),
          line_sort_key = 1,
          line_opacity = c("get", "opacity"),
          line_width = 3
        ) %>% 
        add_tooltips(layer_id = "route", tooltip = tooltip_content) %>% 
        add_circle_layer(
          id = "aid_stations",
          circle_color = PALETTE$WHITE,
          source = as_mapbox_source(GLOBAL$CHECKPOINTS %>% 
                                      st_set_geometry("geometry") %>% 
                                      mutate(geometry_segment = NULL))
        ) %>% 
        fit_bounds(bounds = base_map_bbox, padding = padding)
      
    })
    
  }
  
  # return module server
  moduleServer(id, server)
  
}



# Module - Leg Stats Table in Course Details Card ==============================


# UI ---------------------------------------------------------------------------

mod_leg_table_ui <- function(id) {
  
  ns <- NS(id)
  
  
  # Table - Leg Stats ----------------------------------------------------------
  
  reactableOutput(ns("table"))
  
}


# Server -----------------------------------------------------------------------

mod_leg_table <- function(id, state, GLOBAL) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    
    
    # Observers ----------------------------------------------------------------
    
    # highlight leg row in table if selected elsewhere
    observeEvent(state$selected_segment_idx, {
      
      updateReactable(
        "table",
        selected = coalesce(state$selected_segment_idx, NA),
        session = session
      )
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # set state when table row is selected
    observeEvent(getReactableState("table", "selected"), {
      
      state$selected_segment_idx <- getReactableState("table", "selected")
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    
    # Render Table -------------------------------------------------------------
    
    output$table <- renderReactable({
      
      reactable(
        GLOBAL$CHECKPOINTS %>% 
          as_tibble() %>% 
          select(
            checkpoint,
            distance,
            total_ascent,
            total_descent
          ),
        columns = list(
          checkpoint = colDef(
            name = "Checkpoint"
          ),
          distance = colDef(
            name = "Distance",
            format = colFormat(suffix = "km", digits = 1)
          ),
          total_ascent = colDef(
            name = "Elevation Gain",
            format = colFormat(suffix = "m", digits = 0)
          ),
          total_descent = colDef(
            name = "Elevation Loss",
            format = colFormat(suffix = "m", digits = 0)
          ),
          .selection = colDef(
            show = FALSE
          )
        ),
        highlight = TRUE,
        selection = "single",
        onClick = "select",
        rowStyle = JS(sprintf("function(rowInfo) {
          if (rowInfo && rowInfo.selected) {
            return { backgroundColor: '%s', boxShadow: 'inset 2px 0 0 0 %s' }
          }
        }", PALETTE$PINK, PALETTE$LIGHT_BLUE)),
        defaultPageSize = 13
      )
      
    })
    
  }
  
  # return module server
  moduleServer(id, server)
  
}



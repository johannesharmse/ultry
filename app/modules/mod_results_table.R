# Module - Results Table =======================================================


# UI ---------------------------------------------------------------------------

mod_results_table_ui <- function(id) {
  
  ns <- NS(id)
  
  # Container - Dropdown List & Table ------------------------------------------
  div(
    # Dropdown List - select race time, leg time or pace
    div(
      selectInput(
        ns("table_data_type"), 
        NULL,
        list(`Race Time` = "race_time", `Leg Time` = "leg_time", Pace = "pace")
      )
    ),
    # Table - Results
    reactableOutput(ns("table"))
  )
  
}


# Server -----------------------------------------------------------------------

mod_results_table <- function(id, state, GLOBAL) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    
    
    # Constants ----------------------------------------------------------------
    
    
    # dropdown column patterns - which columns to show
    data_type_col_patterns <- list(
        race_time     = c("race_time", "race_time_str"),
        leg_time      = c("leg_time", "leg_time_str"),
        pace          = c("pace", "leg_pace_str")
      )
    
    # table data
    data <- GLOBAL$TIMES %>%
      filter(leg_time > 0) %>% 
      mutate(across(c(pace, leg_time, race_time), as.numeric)) %>% 
      complete(name, checkpoint) %>% 
      group_by(name) %>% 
      summarise(
        final_position = first(position[station_number == max(GLOBAL$TIMES$station_number, na.rm = T)], 
                               na_rm = T),
        category = first(category, na_rm = TRUE),
        finished = first(finished, na_rm = TRUE),
        pace = list(set_names(pace, checkpoint)), 
        leg_pace_str = list(set_names(leg_pace_str, checkpoint)), 
        leg_time = list(set_names(leg_time, checkpoint)), 
        leg_time_str = list(set_names(leg_time_str, checkpoint)), 
        race_time = list(set_names(race_time, checkpoint)), 
        race_time_str = list(set_names(race_time_str, checkpoint))
      )
    
    
    # Reactive Values ----------------------------------------------------------
    
    data_selected <- reactiveVal()
    table_values <- reactiveVal()
    # table columns - values & props
    col_defs <- sapply(
      GLOBAL$CHECKPOINTS$checkpoint,
      function(x, y) {
        colDef(
          cell = function(value, index, name) { ifelse(!is.na(value), table_values()[[name]][index], "") }
        )
      },
      simplify = FALSE, USE.NAMES = TRUE
    )
    names(col_defs) <- GLOBAL$CHECKPOINTS$checkpoint
    # final position
    col_defs$final_position <- colDef(name = "", 
                                      width = 60, 
                                      na = "DNF", 
                                      style = function(value, index) { 
                                        list(
                                          color = if (data$finished[index]) NULL else PALETTE$RED
                                        )
                                      })
    # participant name
    col_defs$name <- colDef(name = "Name", width = 200)
    # participant category
    col_defs$category <- colDef(name = "Category", width = 150)
    # sparkline
    col_defs$sparkline <- colDef(name = "", 
                                 cell = function(value, index) { 
                                   sparkline(
                                     unname(data_selected()$sparkline[[index]]), 
                                     "bar",
                                     config = list(
                                       barColor = PALETTE$LIGHT_BLUE,
                                       disableInteraction = TRUE
                                     )
                                   ) 
                                 })
    
    
    # Observers ----------------------------------------------------------------
    
    # update table content based on dropdown value
    observeEvent(input$table_data_type, {
      
      selected_col_patterns <- data_type_col_patterns[[input$table_data_type]]
      data_selected(
        data %>% 
          select(final_position, name, category, all_of(selected_col_patterns[1])) %>% 
          mutate(sparkline = .data[[selected_col_patterns[1]]]) %>% 
          unnest_wider(all_of(selected_col_patterns[1])) %>% 
          relocate(GLOBAL$CHECKPOINTS$checkpoint, .after = last_col())
      )
      table_values(
        data %>% 
          select(all_of(selected_col_patterns[2])) %>% 
          unnest_wider(all_of(selected_col_patterns[2]))
      )
      
    })
    
    
    # Render Table -------------------------------------------------------------
    
    output$table <- renderReactable({
      reactable(
        data_selected(),
        columns = col_defs,
        highlight = FALSE,
        searchable = TRUE,
        defaultColDef = colDef(sortNALast = TRUE),
        defaultSorted = list(final_position = "asc"),
        defaultPageSize = 12
      )
    })
    
  }
  
  # return module server
  moduleServer(id, server)
  
}



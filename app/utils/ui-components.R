# UI Components ----------------------------------------------------------------

render_leg_stat <- function(stat) {
  
  # Key text on card
  
  p(
    class = "fs-6 fw-bold",
    stat
  )
  
}


leg_info_box <- function(value, 
                         sub_title, 
                         icon_name, 
                         edit = FALSE, 
                         edit_button = NULL, 
                         confirm_button = NULL, 
                         edit_input = NULL) {
  
  # Construct info card used to display course leg 
  # statistics, goal pace and goal time
  
  if (edit) {
    
    value <- div(
      value,
      hidden(
        textInput(edit_input, label = NULL, value = "", width = "100%")
      )
    )
    
  }
  
  body <- div(
    class = "d-flex align-items-center",
    div(
      value,
      p(sub_title, style = "font-size: 0.75rem")
    ),
    div(
      class="d-flex ms-3",
      bs_icon(icon_name),
    )
  )
  
  card <- if (edit) {
    
    card(
      class = "m-0",
      style = "width: 10rem; height: 10rem;",
      div(
        class = "m-3 p-0",
        style = "position: absolute; top: 0px; right: 0px;",
        actionLink(edit_button, label = NULL, icon = icon("pencil")),
        hidden(actionLink(confirm_button, label = NULL, icon = icon("check")))
      ),
      card_body(
        class = "d-flex justify-content-center h-100",
        body
      )
    )
    
  } else {
    
    card(
      class = "m-0",
      style = "width: 8rem; height: 8rem;",
      card_body(
        class = "d-flex justify-content-center",
        body
      )
    )
    
  }
  
  return(card)
  
}


route_time_dp_tooltip <- function(leg_name, 
                                  race_distance, 
                                  leg_distance, 
                                  leg_pace, 
                                  leg_duration, 
                                  race_duration) {
  
  # Construct tooltip card used on Echarts race time 
  # line chart. Tooltip displays goal statistics for a 
  # given course leg.
  
  div(
    class = "card-body",
    # leg name @ 0.0km
    div(
      class = "mb-2",
      style = "font-size: 0.75rem",
      p(
        class = "card-title fw-bold m-0",
        sprintf("%s @ %s", leg_name, race_distance)
      ),
    ),
    div(
      style = "font-size: 0.7rem",
      # leg distance
      p(
        class = "card-text m-0 lh-sm fw-light",
        span(
          class = "fw-normal",
          "Leg Distance: "
        ),
        leg_distance
      ),
      # leg goal pace
      p(
        class = "card-text m-0 lh-sm fw-light",
        span(
          class = "fw-normal",
          "Leg Pace: "
        ),
        sprintf(
          "%s min/km",
          leg_pace
        )
      ),
      # leg goal time
      p(
        class = "card-text m-0 lh-sm fw-light",
        span(
          class = "fw-normal",
          "Leg Time: "
        ),
        leg_duration
      ),
      # race goal time
      p(
        class = "card-text m-0 lh-sm fw-light",
        span(
          class = "fw-normal",
          "Race Time: "
        ),
        race_duration
      )
    )
  )
  
}

# File Import Functions ========================================================

import_table <- function(file, standardize_names=TRUE, format=list()) {
  
  # Read CSV file, check for expected column names and formats, and 
  # standardize column name
  
  assert_that(is.readable(file))
  assert_that(is.flag(standardize_names))
  assert_that(is.list(format))
  
  data <- read.csv(file, check.names = F)
  
  # standardize column names
  #   -lowercase
  #   -remove white spaces
  if (standardize_names) {
    colnames(data) <- gsub("\\s", "", tolower(colnames(data)))
  }
  
  if(length(format)) {
    
    assert_that(all(names(format) %in% colnames(data)))
    
    # format columns
    colTypes <- sapply(format, function(x) x$type)
    
    if (length(colTypes)) {
      
      colTypesDistinct <- unique(colTypes)
      
      assert_that(all(colTypesDistinct %in% c("number", "text", "category", "hms")))
      
      data <- data %>% 
        mutate(across(all_of(names(colTypes)[colTypes == "number"]), as.numeric)) %>% 
        mutate(across(all_of(names(colTypes)[colTypes == "text"]), as.character)) %>% 
        # keep factor level order
        mutate(across(all_of(names(colTypes)[colTypes == "category"]), 
                      ~factor(.x, levels = unique(.x)))) %>% 
        mutate(across(all_of(names(colTypes)[colTypes == "hms"]), hms))
      
    }
    
    # check values
    acceptedValues <- sapply(format, function(x) x$acceptedValues)
    acceptedValues <- acceptedValues[sapply(acceptedValues, function(x) !is.null(x))]
    if (length(acceptedValues)) {
      sapply(names(acceptedValues), function(x)
        assert_that(all(unique(data[, x]) %in% acceptedValues[[x]])))
    }
    
  }
  
  return(data)
  
}


import_gpx <- function(file, point_layer="track_points") {
  
  # Read GPX file and check that expected columns are present
  
  data <- st_read(file, layer = point_layer)
  
  assert_that(all(c("track_seg_point_id", "ele", "geometry") %in% colnames(data)))
  
  return (data)
  
}

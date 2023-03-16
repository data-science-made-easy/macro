rgb2hex <- function(r, g, b) rgb(r / 255, g / 255, b / 255)

get_impulse_matrix <- function(input) {
  impulse_matrix <- get_zero_impulse_matrix()
  for (i in 1:n_inputs) for (j in 1:n_periods) {
    impulse_matrix[i, j] <- as.numeric(input[[paste0("year_", j, "_", i)]]) # order i j counterintuitive
  }
  
  impulse_matrix
}

sum_effects <- function(impulse_matrix, quarter = FALSE, column_selection = NULL) {
  lst       <- if (quarter) impulse_effect_list$quarter else impulse_effect_list$year
  mat       <- lst[[1]]
  mat[]     <- 0
  n_horizon <- ncol(mat) # maximum period on which we observe impact
  
  for (i in 1:length(lst)) { # effect on output series for each input parameter
    effect <- lst[[i]] # effect on output series for input parameter i
    for (j in seq_along(period)) { # 4 years
      if (0 != impulse_matrix[i, j]) { # do st
        if (quarter) { # quarter data
          index_first_data_point <- 1 + 4 * (j - 1)
        } else { # year data
          index_first_data_point <- j
        }
        n_data_points                           <- 1 + n_horizon - index_first_data_point
        mat[, index_first_data_point:n_horizon] <- mat[, index_first_data_point:n_horizon] + effect[, 1:n_data_points] * impulse_matrix[i, j]
      }
    }
  }
  
  if (is.null(column_selection)) {
    return(mat)
  } else {
    return(mat[, column_selection])
  }  
}

# get_zero_effects_matrix <- function() sum_effects(get_zero_impulse_matrix())

get_file_name <- function(mat = input_impulse_matrix, index_impulse_series = NULL, index_result_series = NULL) { # , bar = TRUE
  if (is.null(index_impulse_series)) return("empty-plot")
  
  # Add all impulses  
  file_name <- NULL
  for (i in index_impulse_series) file_name <- c(file_name, input_var$name[i], mat[i, ])
  
  # Add all result series (index only)
  file_name <- c(file_name, "show", index_result_series)
  
  # # Add line type
  # file_name <- c(file_name, if (bar) "bar" else "line")
  
  # Replace
  file_name <- gsub("\\.", "dot", file_name) # . by dot
  file_name <- gsub('-','m', file_name) # minus sign
  file_name <- paste(file_name, collapse = "-") # glue with -
  file_name <- gsub(' ','-', file_name) # spaces
  
  file_name
}

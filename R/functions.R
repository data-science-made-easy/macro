rgb2hex <- function(r, g, b) rgb(r / 255, g / 255, b / 255)

get_impulse_matrix <- function(input) {
  impulse_matrix <- get_zero_impulse_matrix()
  for (i in 1:n_inputs) for (j in 1:n_periods) {
    # print(c(i,j, input[[paste0("jaar_", , "_", j)]]))
    impulse_matrix[i, j] <- as.numeric(input[[paste0("year_", j, "_", i)]]) # order i j counterintuitive
  }
  
  impulse_matrix
}

sum_effects <- function(impulse_matrix) {
  mat       <- impulse_effect_list[[1]]
  mat[]     <- 0
  n_horizon <- ncol(mat) # maximum period on which we observe impact (here 10)
  
  for (i in 1:length(impulse_effect_list)) { # effect on output series for each input parameter
    effect <- impulse_effect_list[[i]] # effect on output series for input parameter i
    for (j in seq_along(period)) { # 4 years
      if (0 != impulse_matrix[i, j]) { # do st
        n                  <- 1 + n_horizon - j # 10, 9, 8, 7
        mat[, j:n_horizon] <- mat[, j:n_horizon] + effect[, 1:n] * impulse_matrix[i, j]
      }
    }
  }
  
  # to_year_on_year(mat)
  mat
}

to_year_on_year <- function(mat) {
  result <- cbind(mat[,1],t(diff(t(mat))))
  colnames(result) <- colnames(mat)
  result
}

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
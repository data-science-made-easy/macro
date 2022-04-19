generate_inputs <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  
  inputs
}

input_var_seq <- function(i) {
  # Generate input sequence for parameter i
  seq(from = -input_var$max[i], to = input_var$max[i], by = input_var$step[i])
}

dropdown_data_frame <- function(input_var) {  
  df <- data.frame(stringsAsFactors = FALSE, row.names = input_var_name_with_unit)

  for (i in 1:n_inputs) for (j in 1:n_periods) {
    id       <- paste0("year_", j, "_", i) # [year]_[i_parameter] 1_1 ... 4_7
    df[i, j] <- as.character(selectInput(inputId = id, label = NULL, choices = input_var_seq(i), selected = 0))
  }
  #
  # for (i in 1:n_periods) {
  #   df <- cbind(df, generate_inputs(selectInput, n_inputs, paste0('year_', i, '_'), label = NULL, choices = 10:-10, selected = 0))
  # }
  colnames(df)[1:4] <- paste("vanaf jaar", 1:4)
  
  df
}

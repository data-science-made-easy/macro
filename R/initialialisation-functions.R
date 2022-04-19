read_impulse_effect_list <- function(saffier_file_name, var_code_file_name) {
  # Load Saffier impuls effects
  impulse_effect_list    <- list()
  
  for (i in seq_along(input_var$name)) {
    this_impulse_effect_list <- openxlsx::read.xlsx(saffier_file_name, sheet = input_var$name[i], rowNames = T)#, sep.names = " ")
    impulse_effect_list[[i]] <- this_impulse_effect_list # Convert to %
  }
  names(impulse_effect_list) <- input_var$name

  # Replace variable codes by variable names
  var_code <- openxlsx::read.xlsx(var_code_file_name, colNames = T, sheet = "output")
  for (i in seq_along(impulse_effect_list)) {
    # check consistency of naming
    stopifnot(all(var_code[, 1] == rownames(impulse_effect_list[[i]])))
    rownames(impulse_effect_list[[i]]) <- var_code[, 2]
  }
  
  impulse_effect_list
}

get_zero_impulse_matrix <- function() {
  impulse_matrix <- matrix(0, nrow = n_inputs, ncol = n_periods)
  rownames(impulse_matrix) <- input_var$name#paste(input_var$name, "(%-punt)")
  colnames(impulse_matrix) <- period
  impulse_matrix
}
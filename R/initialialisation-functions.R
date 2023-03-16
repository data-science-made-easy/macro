quarter_to_year <- function(impulse_effect_list_quarter) {
  impulse_effect_list_year <- list()
  n_quarter                <- ncol(impulse_effect_list_quarter[[1]])
  n_year                   <- n_quarter / 4
  effect_template          <- impulse_effect_list_quarter[[1]][, 1:n_year]
  effect_template[]        <- NA

  for (i in seq_along(impulse_effect_list_quarter)) {
    this_effect_q <- impulse_effect_list_quarter[[i]]
    this_effect <- effect_template

    # option (a)
    # for (j in 1:n_year - 1) this_effect[, 1 + j] <- this_effect_q[, 4 + 4 * j] # value 'end of year'
    
    # option (b)
    # year = 'mean of quarters' is also used in publication as proxy
    for (k in 1:nrow(this_effect)) {
      for (j in 1:n_year - 1) {
        this_effect[k, 1 + j] <- mean(as.numeric(this_effect_q[k, 1:4 + 4 * j]))
      }
    }

    impulse_effect_list_year[[i]] <- this_effect
  }

  names(impulse_effect_list_year) <- names(impulse_effect_list_quarter)
  impulse_effect_list_year
}

read_impulse_effect_list <- function(spoorboekje_file_name, var_code_file_name) {
  # Load Saffier impuls effects
  impulse_effect_list_quarter    <- list()
  
  for (i in seq_along(input_var$name)) {
    this_impulse_effect_list_quarter <- openxlsx::read.xlsx(spoorboekje_file_name, sheet = input_var$name[i], rowNames = T)#, sep.names = " ")
    impulse_effect_list_quarter[[i]] <- this_impulse_effect_list_quarter # Convert to %
  }
  names(impulse_effect_list_quarter) <- input_var$name

  # Replace variable codes by variable names
  var_code <- openxlsx::read.xlsx(var_code_file_name, colNames = T, sheet = "output")
  for (i in seq_along(impulse_effect_list_quarter)) {
    # check consistency of naming
    stopifnot(all(var_code[, 1] == rownames(impulse_effect_list_quarter[[i]])))
    rownames(impulse_effect_list_quarter[[i]]) <- var_code[, 2]
  }

  # create year data (i.e. average over quarters of each year)
  impulse_effect_list_y <- quarter_to_year(impulse_effect_list_quarter)

  # create quarter data (i.e. rename columns by dividing them by 4)
  impulse_effect_list_q <- impulse_effect_list_quarter
  for (i in seq_along(impulse_effect_list_q)) colnames(impulse_effect_list_q[[i]]) <- as.numeric(colnames(impulse_effect_list_q[[i]])) / 4

  return(list(year = impulse_effect_list_y, quarter = impulse_effect_list_q))
}

get_zero_impulse_matrix <- function() {
  impulse_matrix <- matrix(0, nrow = n_inputs, ncol = n_periods)
  rownames(impulse_matrix) <- input_var$name#paste(input_var$name, "(%-punt)")
  colnames(impulse_matrix) <- period
  impulse_matrix
}
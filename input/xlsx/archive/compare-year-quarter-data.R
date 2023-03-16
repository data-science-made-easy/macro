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

year = read_impulse_effect_list("v16a.xlsx", "input-output-vars-v16a.xlsx")
quar = read_impulse_effect_list("v16a-q.xlsx", "input-output-vars-v16a.xlsx")

delta <- NULL
for (i in 1:7) {
  print(names(year)[i])
  y <- year[[i]]
  for (x in 1:nrow(y)) for (j in 1:ncol(y)) {
    val <- y[x, j]
    vec <- unlist(quar[[i]][x, 4 * (j - 1) + 1:4])
    delta <- c(delta, val - mean(vec))
  }
}

hist(delta)
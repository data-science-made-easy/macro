###
### Definition of global variables
###

# load operator settings
source("input/app-settings/operator-settings.R")

# Subdirectories
snippet_path <- file.path("input", "snippet")

# Load Saffier impuls effects
input_var             <- openxlsx::read.xlsx(var_code_file_name, colNames = T, sheet = "input")
impulse_effect_list   <- read_impulse_effect_list(spoorboekje_file_name, var_code_file_name)
n_inputs              <- nrow(input_var)
n_periods             <- length(period) # for impulses
result_param          <- openxlsx::read.xlsx(var_code_file_name, colNames = T, sheet = "output")
n_outputs             <- nrow(result_param)

# prepare the layout of impulse table
df <- dropdown_data_frame(input_var)

# Generate list of inputs to observe for thermometer
year_inputs   <- sapply(1:n_periods, function(x) paste0("year_", x))
year_input_id <- unlist(lapply(year_inputs, function(inp) paste0(inp, "_", 1:7)))

# Colors
col_anakiwa <- "#8FCAE7" # report download button
col_rose    <- "#e6006e" # spinner, warning, traffic light
col_apple   <- "#43B546" # traffic light
light_green <- "#81CB7A" # color of dropdowns for valid non-zero value

# Dutch months
month_dutch <- c("januari", "februari", "maart", "april", "mei", "juni", "juli", "augustus", "september", "oktober", "november", "december")

# Pandoc is needed for rmarkdown
set_pandoc_path(pandoc_executable = "/opt/shiny-server/ext/pandoc")
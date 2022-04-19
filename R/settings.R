# UI
ui_offset <- 0
ui_width  <- 8

# Input xlsx files
saffier_file_name  <- "xlsx/v14a_02.xlsx"
var_code_file_name <- "xlsx/input-output-vars.xlsx"

# Report template
report_template        <- "report/saffier-report.Rmd"
report_header_tex_file <- "report/header.tex"

# Impulse time frame
period             <- paste("jaar", 1:4)

# Colors
col_endeavour  <- "#01689B" # rgb(  1 / 255, 104 / 255, 155 / 255)
col_anakiwa    <- "#8FCAE7" #rgb2hex(143, 202, 231) #8FCAE7
col_rose       <- "#e6006e" #rgb(202 / 255,   0 / 255,  93 / 255)
col_mauvelous  <- "#EA99BE" #rgb(234 / 255, 153 / 255, 190 / 255) #EA99BE
col_sun        <- "#FBAD1D"
col_apple      <- "#43B546"
light_green    <- "#81CB7A"#
col_wewak      <- "#F099C1"
col_paleorange <- "#E5BA3A"

# Load Saffier impuls effects
input_var            <- openxlsx::read.xlsx(var_code_file_name, colNames = T, sheet = "input")
impulse_effect_list  <- read_impulse_effect_list(saffier_file_name, var_code_file_name)
n_inputs             <- nrow(input_var)
n_periods            <- length(period)
result_param         <- openxlsx::read.xlsx(var_code_file_name, colNames = T, sheet = "output")

# Set unit for inputs
input_var_name_with_unit <- paste0(input_var$name, " (", input_var$unit, ")")

df <- dropdown_data_frame(input_var)

# Generate list of inputs to observe for thermometer
year_inputs <- sapply(1:n_periods, function(x) paste0("year_", x))
year_input_id <- unlist(lapply(year_inputs, function(inp) paste0(inp, "_", 1:7)))

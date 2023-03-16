# UI
ui_offset              <- 0  # space left side of app
ui_width               <- 12 # width of app

# Impulse table
period                 <- paste("jaar", 1:4) # impulse table header

# Result table
effect_decimal_places  <- 2
result_col_selection   <- c(1, 2, 4, 7, 10) # show these columns only

# Figure
fig_title              <- "Verwacht effect t.o.v. basispad" # title
x_title                <- "tijd (jaar)"                     # title of x-axis
y_r_n_decimals         <- 2                                 # number of decimals on right y-axis

# Figure path
fig_file_path          <- "./www/generated"
fig_file_hash          <- "./www/generated/cache"

# Input xlsx file path
spoorboekje_file_name  <- "input/xlsx/input-spoorboekje-v16a.xlsx"
var_code_file_name     <- "input/xlsx/input-output-vars-v16a.xlsx"

# Report template path
report_template        <- "input/report/saffier-report-template.Rmd"

# Rijkshuisstijl font
path_fonts             <- "www/fonts"

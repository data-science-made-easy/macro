server <- function(input, output, session) {
  # options(DT.options = list(pageLength = 7, filter = FALSE, autoWidth = FALSE))
  options(DT.options = list(dom = 't', ordering = F,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#8FCAE7', 'color': '#fff'});",
    "}")
  ))

  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Disclaimer",
      "Deze tool dient slechts educatieve doeleinden. Het doel is de gebruiker inzicht te geven in de werking van de Nederlandse economie. De resultaten zijn een benadering van de verwachte gevolgen van de gegeven impulsen. Het proces om tot deze benadering te komen kan fouten bevatten. Aan de resultaten kan dan ook geen enkel recht worden ontleend.", style = "text-align: justify",
      easyClose = TRUE,
      footer = tagList(modalButton("Sluiten"))
    ))
  })
  
  session$userData$impulse_matrix  <- get_zero_impulse_matrix() # put in session to detect changes implying work to do
  session$userData$selected_series <- 1 # user wants to see this series in figure

  update_traffic_light <- function(violation = F) {
    output$thermometer <- renderPlot({
      # plot_thermometer(fraction)
      plot_traffic_light(violation)
    }, width = 25, height = 50)
  }
  
  # Put impulse matrix in table v2
  output$struct_table <- DT::renderDataTable(
    df, server = FALSE, escape = FALSE, selection = 'none',
    callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-slider-input');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());"
    )
  )
  
  observeEvent(lapply(year_input_id, function(val) input[[val]]), {
    abs_impulses <- abs(get_impulse_matrix(input))

    violation       <- FALSE
    violation_index <- NA
    warning_prefix  <- "We tonen geen resultaten als "
    warning_txt     <- ""
    
    # Detect too many impulses
    n_impulses <- length(which(apply(abs_impulses, 1, function(vec) any(0 != vec))))
    if (2 < n_impulses) {
      violation   <- TRUE
      violation_index <- 0
      warning_txt <- paste0(warning_prefix, "je aan meer dan twee variabelen impulsen toedient.")
    }
        
    # Detect too large impulses
    abs_impuls_per_param <- apply(abs_impulses, 1, sum)
    input_too_large      <- input_var$max < abs_impuls_per_param
    for (i in 1:n_inputs) if (!violation & input_var$max[i] < sum(abs_impulses[i, ])) {
      violation       <- TRUE
      violation_index <- i
      warning_txt     <- paste0(warning_prefix, "de som van de absolute impulsen die je toedient aan variabele '", input_var$name[i], "' groter is dan ", input_var$max[i], " ", input_var$unit[i], ".")
    }

    output$warning <- renderText({warning_txt})
    if (violation) { # impulses too big or too many
      shinyjs::js$disableTab("Resultaattabel")
      shinyjs::js$disableTab("Figuur")
      shinyjs::disable("report")
      
      for (i in 1:n_periods) for (j in 1:n_inputs) {
        val <- as.numeric(input[[paste0("year_", i, "_", j)]])
        this_col <- if (0 != val & (0 == violation_index | j == violation_index)) col_rose else "#FFFFFF"
        shinyjs::runjs(paste0("document.getElementById('year_", i, "_", j, "').style.backgroundColor ='", this_col ,"'"))
      }
      
    } else { # impulses within limit
      shinyjs::js$enableTab("Resultaattabel")
      shinyjs::js$enableTab("Figuur")
      shinyjs::enable("report")
      
      # reset background color dropdown
      for (i in 1:n_periods) for (j in 1:n_inputs) {
        val <- as.numeric(input[[paste0("year_", i, "_", j)]])
        this_col <- if (0 != val) light_green else "#FFFFFF"
        shinyjs::runjs(paste0("document.getElementById('year_", i, "_", j, "').style.backgroundColor ='", this_col ,"'"))
      }
    }

    update_traffic_light(violation)
  }, ignoreInit = TRUE)

  update_result_table <- function(initialize = FALSE) {
    impulse_matrix <- if (initialize) get_zero_impulse_matrix() else get_impulse_matrix(input)
    effect <- sum_effects(impulse_matrix) #session$userData$input_impulse_matrix
    effect_rounded <- round(effect, 2) # round
    rownames(effect_rounded) <- paste0(rownames(effect_rounded), " (", result_param$unit, ")") # add unit
    output$effect_table <- DT::renderDataTable(effect_rounded, server = FALSE, options = list(paging = FALSE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#8FCAE7', 'color': '#fff'});",
        "}")
    ), selection = list('multiple', selected = session$userData$selected_series))
  }
  
  show_figure <- function(index = NULL, initialize = FALSE) {
    if (!is.null(index)) {
      file_path <- "./www/generated"
      hash_path <- "./www/generated/cache"
      
      # get impulses
      impulse_matrix <- if (initialize) get_zero_impulse_matrix() else get_impulse_matrix(input)
      index_impulse_series <- which(apply(impulse_matrix, 1, function(vec) any(0 != vec)))
      file_name <- get_file_name(impulse_matrix, index_impulse_series, index_result_series = index) # 
      
      if (0 == length(index_impulse_series)) {
        footnote_text <- "Geen impulsen"
      } else {
        too_many <- 2 < length(index_impulse_series)
        if (too_many) index_impulse_series <- index_impulse_series[1:2]
        
        footnote_text <- paste0("Impuls(en): ", paste0(input_var$name[index_impulse_series], collapse = ", "))
        if (too_many) footnote_text <- paste(footnote_text, "en meer")
      }
    
      # get series type
      # series_type <- if (input$barLineToggle) "bar--" else "line"
    
      index_order <- sort(index, index.return = T)$ix
      unit        <- result_param$unit[index][index_order]
      n_units     <- length(unique(unit))
      unit_1      <- unique(unit)[1]
      y_l_title   <- paste0("verandering (", unit_1, ")")
      unit_2      <- NULL # so we can see whether 2th y-axis exists
      if (1 == n_units) {
        # one y-axis
        y_axis <- "l"
      } else if (2 == n_units) {
        unit_2 <- unique(unit)[2]
        y_axis <- unit
        y_axis[y_axis == unit_1] <- "l"
        y_axis[y_axis == unit_2] <- "r"
      } else {
        stop(paste("Max. two units allowed. Current units:", unique(unit)))
      }
      y_r_title <- if (is.null(unit_2)) NULL else paste0("verandering (", unit_2, ")")
    
      # Calculate results we want to show
      mat <- t(sum_effects(impulse_matrix))[, sort(index), drop = FALSE]

      # fix series names that show-up on right axis
      if (!is.null(unit_2)) {
        index_y_r <- which("r" == y_axis)
        colnames(mat)[index_y_r] <- paste(colnames(mat)[index_y_r], "(r-as)")
      }

      # add x-axis
      mat <- cbind(as.numeric(rownames(mat)), mat) # add years
      mat[, 1] <- 0.5 + mat[, 1] # fix x-axis so years are periods and not points


      # put path in session so we can return path
      shinybusy::show_spinner(spin_id = "spin_fig")
      session$userData$figure_file_path <- nicerplot::nplot(mat, lock = FALSE, y_force_include_zero = TRUE, palette = "cpb, extra", title = "Verwacht effect t.o.v. basispad", x_title = "tijd (jaar)", y_title = y_l_title, footnote = footnote_text, destination_path = file_path, file = file_name, legend_n_per_column = if (ncol(mat) < 10) 3 else 4, x_lim = c(1,11), x_ticks = 1:11, x_at = 0.5 + 1:11, x_lab = 1:10, hash_dir = hash_path, decimal_mark = ".", big_mark = ",", y_lim = if (all(0 == mat[, -1])) c(-1, 1) else NA, y_axis = y_axis, y_r_n_decimals = 2, y_r_title = y_r_title, x_axis_bold_if_zero = is.null(unit_2)) #, type = series_type
      shinybusy::hide_spinner(spin_id = "spin_fig")

      output$plot <- renderImage({ # start here so spinner shows
        # return img
        list(src = session$userData$figure_file_path, width = 500, contentType = 'image/png', alt = "TODO")
      }, deleteFile = FALSE)

      # If output$plot has not been shown yet, then the create plot above will be skipped (to prevent unnecessary work) but we do need da figa for the report, so:
      # if (is.null(session$userData$figure_file_path)) session$userData$figure_file_path <- paste0(file.path(file_path, file_name), ".png")
        
      return(session$userData$figure_file_path) # return figure file path so we can show it in report or on website

    } else return(NULL)
  }
  
  # Only create figure when shown
  observeEvent(input$tabs, {
    if ("Resultaattabel" == input$tabs) {
      impulse_matrix <- get_impulse_matrix(input)
      if (all(session$userData$impulse_matrix == impulse_matrix)) {
        # clean
      } else {
        # dirty
        update_result_table()
        session$userData$impulse_matrix <- impulse_matrix
      }
    } else if ("Figuur" == input$tabs) {
      index <- input$effect_table_rows_selected
      if (0 == length(index)) index <- 1
      session$userData$selected_series <- index # put in session to remember what user selected when showing new result table
      show_figure(index = index)
    }
  })
  
  # # if user toggles between bar-- and line
  # observeEvent(input$barLineToggle, {
  #   if ("Figuur" == input$tabs) {
  #     index <- input$effect_table_rows_selected
  #     if (0 == length(index)) index <- 1
  #     show_figure(index = index)
  #   }
  # })
  
  observeEvent(input$reset, {
    # update impulses
    for (j in 1:n_inputs) for (i in 1:n_periods) {
      this_input_var_seq <- input_var_seq(j)
      updateSelectInput(session = session, inputId = paste0("year_", i, "_", j), choices = this_input_var_seq, selected = 0)
    }
    
    # update tab we watch
    if ("Resultaattabel" == input$tabs) {
      impulse_matrix <- get_zero_impulse_matrix()
      update_result_table(initialize = TRUE)
      session$userData$impulse_matrix <- impulse_matrix
    } else if ("Figuur" == input$tabs) {
      index <- input$effect_table_rows_selected
      if (0 == length(index)) index <- 1
      session$userData$selected_series <- index # put in session to remember what user selected when showing new result table
      show_figure(index = index, initialize = TRUE)
    }
  })
  
  output$report <- downloadHandler(
    filename = paste0("saffier-rapport-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".pdf"),
    content = function(file) {

      # Create figure
      index <- input$effect_table_rows_selected
      if (0 == length(index)) index <- 1

      figure_path <- show_figure(index = index)
      shinybusy::show_spinner(spin_id = "spin_rep")
      tmp_path <- tempdir()
      tmp_path_www <- file.path(tmp_path, "www")
      tmp_path_www_generated <- file.path(tmp_path_www, "generated")
      dir.create(tmp_path_www_generated, recursive = TRUE, showWarnings = FALSE)
            
      file.copy(report_header_tex_file, tmp_path, overwrite = TRUE)
      file.copy("www/RO_CP_Logo_Homepage_nl.png", tmp_path_www)
      file.copy(figure_path, tmp_path_www_generated)
      
      tempReport <- file.path(tmp_path, basename(report_template))
      file.copy(report_template, tempReport, overwrite = TRUE)

      knitr::opts_chunk$set(echo = FALSE)
      param <- list(input_impulse_matrix = get_impulse_matrix(input), path_to_figure = figure_path)
      rmarkdown::render(tempReport, output_file = file, params = param)#, envir = new.env(parent = globalenv()))
      shinybusy::hide_spinner(spin_id = "spin_rep")
    }
  )

  update_traffic_light(violation = FALSE)
  update_result_table(initialize = TRUE)
}
































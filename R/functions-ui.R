create_impulse_row_names <- function(input_var, input_data = NULL, link = TRUE) {
  vec       <- NULL
  link_post <- if (link) "</a>" else ""
  for (i in 1:nrow(input_var)) {
    # if input data is given and variable is set (i.e. nonzero) then make that row name bold
    bold  <- ""
    if (!is.null(input_data)) if (any(0 != input_data[i, ])) bold <- "**"
    link_pre <- if (link) paste0("<a id = 'popup_var_impulse_id_", i, "' href = '#' class = 'action-button'>") else ""
    vec[i]   <- paste0(bold, link_pre, stringr::str_to_sentence(input_var$name[i]), link_post, " (", input_var$unit[i], ")", bold)
  }
    
  vec
}

add_row_names <- function(effect_rounded) {
  row_name <- rownames(effect_rounded)
  
  for(i in 1:nrow(effect_rounded)){
    this_tooltip <- create_if_non_existing(get_snippet_file_name(row_name[i]))
    # this_tooltip <- shiny::includeMarkdown(this_tooltip)
    html <- markdown::markdownToHTML(this_tooltip, fragment.only = TRUE, encoding = "UTF-8")
    # Encoding(html) <- "UTF-8"
    this_tooltip <- HTML(html)
    this_tooltip <- as.character(this_tooltip)
    this_tooltip <- stringr::str_sub(this_tooltip, end = -2) # remove last newline because of layout

    # following is HACK
    # remove <p> for layout purposes (<p>one line</p> results in tooltip ending with empty line below text)
    if ("<p>" == stringr::str_sub(this_tooltip, 1, 3) & "</p>" == stringr::str_sub(this_tooltip, -4)) { # remove p because of layout
      this_tooltip <- stringr::str_sub(this_tooltip, start = 4)
      this_tooltip <- stringr::str_sub(this_tooltip, end = -5)
    }    
    this_tooltip <- stringr::str_replace_all(this_tooltip, "\n", "<BR/>")
    
    rownames(effect_rounded)[i] <- as.character(shinyBS::tipify(shiny::actionLink(inputId = paste("ttip_", i, sep = ""), label = row_name[i]), title = this_tooltip, placement = "right", trigger = "hover", options = NULL))
  }
  
  rownames(effect_rounded) <- paste0(rownames(effect_rounded), " (", result_param$unit, ")") # add unit
  
  effect_rounded
}
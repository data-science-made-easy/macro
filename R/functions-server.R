# replace: ' ' -> '-', 'ë' -> 'e'
get_snippet_file_name <- function(subject) paste0(file.path(snippet_path, stringr::str_replace_all(stringr::str_replace_all(subject, " ", "-"), "ë", "e")), ".md")

create_if_non_existing <- function(file_name) {
  if (!file.exists(file_name)) {
    txt <- paste0("Bestand '", file_name, "' bestaat niet.")
    txt <- c(txt, "Zie m:/p_jamesgebruiker/saffier.")
    writeLines(txt, file_name)
  }
  
  file_name
}

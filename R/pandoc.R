set_pandoc_path <- function(pandoc_executable) {
  if ("" == Sys.getenv("RSTUDIO_PANDOC")) {
    if (file.exists(pandoc_executable)) {
      Sys.setenv(RSTUDIO_PANDOC = pandoc_executable)
    } else {
      cat(paste0("Warning [MD]: pandoc NOT FOUND, expected at '", pandoc_executable, "'. Please set parameter 'pandoc_executable' in file 'R/settings.R'."))
    }
  }
}
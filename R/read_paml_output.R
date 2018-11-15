read_paml_output <- function(file_path) {
  my_col_names <- c("seq_1", "seq_2", "S", "N", "t", "kappa", "omega", "dN", "SE_dN", "dS", "SE_dS", "PITG")
  
  lines <- readr::read_lines(file_path)
  if (length(lines) > 0) {
    start_index <- which(grepl(lines, pattern = "^seq\\."))
    raw_data <- paste0(lines[c(start_index, start_index + 2)], collapse = "\n")
    raw_data <- gsub(raw_data, pattern = " +- ", replacement = "  ", fixed = TRUE)
    output <- read.table(text = raw_data, header = TRUE)
    output$pitg <- stringr::str_match(basename(file_path), "^Out_(.+)$")[,2]
    colnames(output) <- my_col_names
  } else {
    output <- as.data.frame(matrix(ncol = length(my_col_names), nrow = 0, dimnames = list(character(0), my_col_names)))
  }
  return(output)
}
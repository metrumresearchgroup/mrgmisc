#' Function to read in all SAS files in a given directory
#' 
#' @param .dir directory containing SAS files
#' 
#' @author Eric Anderson
#' 
#' @export
read_sas_folder <- function(.dir){
  src_dir <- here::here("data", "source", .dir)
  src_files <- list.files(src_dir, full.names = TRUE, pattern = "sas7bdat")
  src_data <- purrr::map(src_files, ~ haven::read_sas(.x))
  src_data <- setNames(src_data, basename(src_files))
  # Include the full source name as a column
  # Trim off the path above the project
  src_files_trimmed <- gsub(here::here(), "", src_files, fixed = TRUE)
  for (src.i in names(src_data)) {
    src_data[[src.i]]$SOURCE <- src_files_trimmed[basename(src_files_trimmed) == src.i]
  }
  src_data
}
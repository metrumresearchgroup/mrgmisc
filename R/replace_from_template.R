#' replace values from a template file
#' @param file template file
#' @param patterns patterns to match
#' @param replacement_df dataframe of replacements
#' @param output_dir set directory if not current directory
#' @param file_name column in replacement containing requested output file name
#' @param ... additional arguments for readLines
#' @details 
#' the pattern will first check for names, and if matching names in replacement df will follow, however
#' if no names are detected, then will match by position
replace_from_template <- function(file, patterns, replacement_df, output_dir = NULL, file_name = NULL, ...) {
  file_lines <- readLines(file, ...)
  for (i in 1:nrow(replacement_df)) {
  new_file_lines <- str_replace_all(file_lines,
                                    setNames(unlist(replacement_df[i, names(patterns)]), patterns))
  writeLines(new_file_lines, unlist(replacement_df[i, "file_name"]))
                    
  }
  return(TRUE)
}
#
## pattern name must currently match the replacement df column name 
#patterns = c(CL = "<cl> ; omega_cl", V = "<v> ; omega_V")
#
#replacements = data.frame(expand.grid(CLnum = seq(0.1, 0.6, 0.1), Vnum = seq(0.1, 0.6, 0.1)))
#
## transmute will drop unused columns so in this case the placeholder CLnum and Vnum
#replacement_df <- replacements %>% dplyr::transmute(CL = paste0(CLnum, " ; omega_cl"), 
#                        V = paste0(Vnum, " ; omega_V"),
#                        file_name = paste0(CLnum,"CL-",Vnum,"V", ".mod"))
#
#
## will write out mod files for all replacements
#replace_from_template("replacement.mod", patterns, replacement_df)
#
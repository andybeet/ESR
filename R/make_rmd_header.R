#' make an header for the md file
#'
#' Need to include title and tool tip for latex -> pdf conversion
#'
#'@param filenameOut Name of the output file (.md)
#'
#'@return Writes content to an md file

make_rmd_header <- function(filenameOut) {
  # add a title

  write("---",here::here(filenameOut),append=T)
  write("title: \"Ecosystem Status Report (2015) for the Northeast Large Marine Ecosystem\"",here::here(filenameOut),append=T)
  write("output: ",here::here(filenameOut),append=T)
  write("  word_document: ",here::here(filenameOut),append=T)
  write("    reference_docx: ESR_template.docx",here::here(filenameOut),append=T)
  write("---",here::here(filenameOut),append=T)
  write("",here::here(filenameOut),append=T)    
}


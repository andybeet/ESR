#' make an header for the md file
#'
#' Need to include title and tool tip for latex -> pdf conversion
#'
#'@param filenameOut Name of the output file (.md)
#'
#'@return Writes content to an md file

make_md_header <- function(filenameOut) {
  # add a title
  write("---",here::here(filenameOut),append=T)
  write("title: \"Ecosystem Status Report (2015) for the Northeast Large Marine Ecosystem\"",here::here(filenameOut),append=T)
  write("output: ",here::here(filenameOut),append=T)
  write("  pdf_document: ",here::here(filenameOut),append=T)
  write("    keep_tex: yes",here::here(filenameOut),append=T)
  write("header-includes: ",here::here(filenameOut),append=T)
  write("   - \\RequirePackage{accsupp}",here::here(filenameOut),append=T)
  write("   - \\RequirePackage{pdfcomment} ",here::here(filenameOut),append=T)
  write("   - \\newcommand{\\AccTool}[2]{\\BeginAccSupp{method=pdfstringdef,unicode,Alt={{#1}}}\\pdftooltip{{#2}}{{#1}}\\EndAccSupp{}} ",here::here(filenameOut),append=T)
  write("---",here::here(filenameOut),append=T)
  write("",here::here(filenameOut),append=T)
  
    
}


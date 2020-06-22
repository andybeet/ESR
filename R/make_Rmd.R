#' make an rmd file from extracted web data
#'
#' Parses data in txt file into a rmarkdown file
#'
#'@param filename Name of the file to be parsed (.txt)
#'@param filenameOut Name of the output file (.md)
#'
#'@return Writes content to an md file

make_rmd <- function(siteurl,filename,filenameOut) {
  file.remove(filenameOut)
  fileContent <- readLines(filename)
  # replace arows with latex equivalent
  #fileContent <- gsub("&#8599;","\\$\\\\nearrow\\$",fileContent)
  #fileContent <- gsub("&#8600;","\\$\\\\searrow\\$",fileContent)
  #fileContent <- gsub("&harr;","\\$\\\\leftrightarrow\\$",fileContent)
  figProperties <- list()
  make_rmd_header(filenameOut)
  # look for <h tags format with hashes
  iline <- 0
  figs <- NULL
  # line by line
  while (iline < length(fileContent)) {
    iline <- iline + 1
    if (iline == 1) next
    aline <- fileContent[iline]

    # find all lines beginning with a header <h and print out markdown
    if(grepl("<h\\d{1}",aline)) {
      aline <- gsub("<em>","",aline)
      htag <- stringr::str_match(aline,"<h\\d{1}")
      split1 <- unlist(strsplit(htag,"<h"))
      headerSize <- as.numeric(split1[2])

      if((headerSize == 1) & (iline != 1)){ # same h1 on every web page. ignore all after first instance
        next
      }
      #Extract header title and format based on hsize
      split2 <- unlist(strsplit(aline,htag))[2]
      split3 <- unlist(strsplit(split2,">"))[2]
      headerText <- unlist(strsplit(split3,"<"))[1]
      # removes all period in title then add a period after every number
      newH <- stringr::str_replace_all(headerText,"\\."," ")
      headerText <- stringr::str_replace_all(newH,"([0-9]+)","\\1\\.")
      dotsinHeader <- stringr::str_count(headerText,"\\.")
      if (dotsinHeader > 0) {
        headerSize <- dotsinHeader
      }
      if (iline<=104) { # before chapter numbering
        headerSize <- 2
      }
      if (grepl("Main Findings",headerText))
        headerSize <-  1
      if (headerText == "Coastal Condition Reports") # ignore. A HACK
        next

      header <- paste(paste0(rep("#",headerSize),collapse = "") ,headerText)
      print(header)
      write(header,here::here(filenameOut),append=T)
      next
    }

    # look for headings in collapsiblepaneltabs
    # original website had titles in dynamic content
    if(grepl("CollapsiblePanelTab",aline)) {
      split1 <- unlist(strsplit(aline,">"))[2]
      headerText <- unlist(strsplit(split1,"<"))[1]
      nPeriods <- stringr::str_count(headerText,"\\.")
      nPeriods <- max(1,nPeriods)
      header <- paste(paste0(rep("#",nPeriods),collapse = "") ,headerText)
      print(header)
      write(header,here::here(filenameOut),append=T)
      next
    }
    

    # some <p> have divs inside that just have an anchor.
    # We just want the anchor
    if (grepl("<div class=\"figure\"",aline)){
      figList <- get_anchor_info(aline,figs)
      if (!is.null(figList)){
        figs <- figList$figs
        # write anchor content      
        write(paste0("```{r,fig.cap=\"",figList$figCaption,"\",echo=F}"),here::here(filenameOut),append=T)
        write(paste0("knitr::include_graphics(\"",figList$fileUrl,"\")"),here::here(filenameOut),append=T)
        write(paste0("```"),here::here(filenameOut),append=T)
        write("",here::here(filenameOut),append=T)
      }
      
      next
    }
    
    # now look for <p
    if(grepl("<p>",aline)) {
      # badly formatted html code needs a fix for no \n
      aline <- unlist(strsplit(aline,"*<p"))
      aline <- paste0("<p",aline[2])
      # remove blank space prior to <p
      aline <- gsub("^\\s*<p>","",aline)
      aline <- gsub("</p>","",aline)
      # remove further leading blank space, prevents indentation
      aline <- sub("^\\s*","",aline)
      
      # within each paragraph there are links to figures. Get the title and the url.
      # Check to see if it has been found before. Match all anchors
      figList <- get_anchor_info(aline,figs)
      
      # remove all anchor reference. Revoves hyperlinks in output md
      aline <- stringr::str_remove_all(aline, "</?a[^>]*>")

      # write paragraph content then figure
      write(aline,here::here(filenameOut),append=T)
      write("",here::here(filenameOut),append=T)
      
      if (!is.null(figList)) {
        figs <- figList$figs
        # write anchor content
        write(paste0("```{r,fig.cap=\"",figList$figCaption,"\",echo=F}"),here::here(filenameOut),append=T)
        write(paste0("knitr::include_graphics(\"",figList$fileUrl,"\")"),here::here(filenameOut),append=T)
        write(paste0("```"),here::here(filenameOut),append=T)
        write("",here::here(filenameOut),append=T)
      }
      next
    }

    # look for list items
    if (grepl("<li>",aline)) {
      aline <- sub("<li>","",aline)
      aline <- sub("</li>","",aline)
      aline <- sub("^\\s*","",aline)

      bulletFormat <- paste("*",aline)
      write(bulletFormat,here::here(filenameOut),append=T)


    }


  }
  
  return(figs)
}


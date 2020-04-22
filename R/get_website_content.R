#' Gets website content
#' 
#' Recursive script that parses data webpages from
#' "net.nefsc.noaa.gov/www/ecosys/ecosystem-status-report"
#'
#'@param filename Name of the first html file to be parsed (oindex.html
#'@param filenameOut Name of the output file (.txt)
#'
#'@section Note:
#' You will need to be connected to the internal server
#'
#'@return Writes content to an txt file

get_website_content <- function(siteurl,filenameIn,filenameOut="ecosys-website.txt") {
  #url where all data is stored
  pageurl <- paste0(siteurl,filenameIn)

  message(paste0("Processing: ",pageurl))

  # read in html page
  pageContent <- readLines(pageurl)
  pageContent <- stringi::stri_trans_general(pageContent, "Latin-ASCII")

  # look for <h tags, <p and /p> tags
  # look for class="next" for next page
  newPage <- NULL
  iline <- 0
  # line by line
  while (iline < length(pageContent)) {
    iline <- iline + 1
    aline <- pageContent[iline]
    
    # find all lines beginning with a header <h
    if(grepl("<h\\d{1}",aline)) {
      newPage <- rbind(newPage,aline)
      next
    }

    # look for headings in collapsiblepaneltabs capture all between divs
    if(grepl("class=\"CollapsiblePanelTab\"",aline)) {
      if (grepl("<div",aline) & grepl("</div",aline)) { # if all on one line
        newPage <- rbind(newPage,aline)
      } else { # else read in multiple lines and paste as one line
        grabline <- aline
        while (!grepl("</div",aline)) {
          iline <- iline + 1
          aline <- pageContent[iline]
          aline <- sub("^\\s*","",aline)
          aline <- sub("\\s*$","",aline)
          grabline <- paste0(grabline,aline)
        }
       # print(grabline)
        newPage <- rbind(newPage,grabline)
      }
      next
    }
    
    # pick out stand alone figures inside <div>s but outside <p>
    if(grepl("<div class=\"figure\"",aline)) {
      #aline <- sub("^\\s* <div class=\"figure\">","",aline)
      grabline <- aline
      while (!grepl("</div",aline)) {
        iline <- iline + 1
        aline <- pageContent[iline]
        aline <- sub("^\\s*","",aline)
        aline <- sub("\\s*$","",aline)
        grabline <- paste0(grabline,aline)
      }
      #grabline <- sub("</div>","",aline)
      newPage <- rbind(newPage,grabline)
      next
    }
        
    # find all lines between either <p or /p>
    if(grepl("<p>",aline)) {
      while(!grepl("/p>",aline)){
        iline <- iline + 1
        aline <- pageContent[iline]
        newPage <- rbind(newPage,aline)
      }
      newPage <- rbind(newPage,aline)
      next
    }
    
    # look for list items
    if (grepl("<li>",aline)) {
      newPage <- rbind(newPage,aline)
      next
    }
    

    # look for next page, parse it
    if (grepl("class=\"next",aline)) {
      split1 <- strsplit(aline,"href=\"")
      split2 <- strsplit(unlist(split1)[2],"\"")
      nextPage <- unlist(split2)[1]

      #write current page grab to a file
      write(newPage,here::here(filenameOut),append=T)
      # process the next page
      if (nextPage == "sitemap.html") return()
      get_website_content(nextPage)

    }



  }

}


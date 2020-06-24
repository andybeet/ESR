#' Parse a line to extract info inside <a> tags
#' 
#' Only looks for linked png files. The href link, alt text, href Caption are captured
#'
#'@param aline Character string. Line from html file
#'@param figs Character vector. unique list of previously found href urls
#'
#'@return List of href objects
#'
#'\item{figs}{updated character vector of href links}
#'\item{figUrl}{The urls of the figures found}
#'\item{figCaption}{The Caption (The hyperlinked text)}
#'\item{figText}{The alt text}
#'


get_anchor_info <- function(aline,figs) {
  
  hrefs <- stringr::str_match_all(aline,"<a[^>]+href=\"(.*?)\"[^>]*>(.*?)</a>")
  hrefs <- hrefs[[1]] # unlist
  if (nrow(hrefs) == 0) return() # no anchors found
  if (ncol(hrefs) < 3) return() # not a real anchor

  newFigs <- list()
  for (imatch in 1:nrow(hrefs)) {
    titleText <- hrefs[imatch,1]
    fileUrl <- hrefs[imatch,2]
    figText <- hrefs[imatch,3]

    if (!is.na(fileUrl)) { # add figure to markdown
      if(!grepl("\\.png",fileUrl)) return() # skips references 
      if (any(fileUrl==figs)) return() # duplicate refs to figures

      title <- stringr::str_match(titleText,"title=\"(.*?)\"")[2]
      
      # write("<figure>",here::here(filenameOut),append=T)
      # write(paste0("<img src=",fileUrl," alt=",title,"/>"),here::here(filenameOut),append=T)
      # write(paste0("<figcaption>",title,"</figcaption>"),here::here(filenameOut),append= T)
      # write("</figure>",here::here(filenameOut),append=T)
      # write("",here::here(filenameOut),append=T)
      
      
      newFigs$fileUrl[imatch] <- fileUrl 
      newFigs$figCaption[imatch] <- title
      newFigs$figText[imatch] <- figText 
      
      figs <- c(figs,fileUrl)
    }
  }
  

  return(list(figs=figs,newFigs=newFigs))
  
  #return(list(figs=figs,fileUrl=fileUrl,figCaption=title,figText=figText))
  
}
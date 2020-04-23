#' Main file to run
#'
#' ############## You will need to be connected to the internal server ######
#'
#' Read in old website files
#' parse them
#' write them to a text file
#' make an md file from text file
#' convert md into pdf
#' 

source("R/get_website_content.R")
source("R/make_md.R")
source("R/check_valid_UTF8.R")
source("R/get_anchor_info.R")

# parses all web files and picks out content we need.
# writes to a text file
siteurl <- "//net.nefsc.noaa.gov/www/ecosys/ecosystem-status-report/"
get_website_content(siteurl,"index.html",filenameOut="ecosys-website.txt")

# converts the txt file to to an md file

figs <- make_md(filename="ecosys-website.txt",filenameOut = "ESR_2015.md")

# check to make sure all figures are downloaded
get_figs(siteurl,figs)


# check for validUTf-8 encoding
check_valid_UTF8("ESR_2015.md")

# use virtual studio code to convert to pdf

# convert md to pdf. This doesn't work
#rmarkdown::pandoc_convert(here::here("ecosys-website.md"),output="ESR_2014.pdf")

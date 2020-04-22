#' pull figures needed for the report to the local folder
#'
#'
#'

get_figs <- function(siteurl,figs) {
  if (!dir.exists(here::here("figures"))) {
    dir.create(here::here("figures"))
  }
  flag <- file.copy(paste0(siteurl,figs),here::here(figs))
  if (!any(flag)) {
    message("some files were not copied from server. Maybe they already exist!")
    print(figs[!flag])
  }
}
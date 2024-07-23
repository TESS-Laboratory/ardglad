#' Generate URLs for GLAD ARD data
#' @param aoi an sf or sfc object representing the area of interest.
#' @param start_date a character string representing the start date.
#' @param end_date a character string representing the end date. if NULL, only
#' the start date is used and tiles for a single 16-day interval are returned.
#' @param rerpoj a logical value indicating whether to reproject the input if
#' it is not in the WGS84 CRS.
#' @param src a character string indicating the source of the data. Options are
#' "umd" or "aws".
#' @return a named list of character vectors containing the URLs for the
#' requested data. names correspond to the start day of the 16-day interval.
#' @details
#' Each item in the returned list comprises the tile(s) for a given 16-day
#' interval. thelist items are named according to the start date of the
#' interval.
#' @export
ard_glad_urls <- function(
    aoi, start_date, end_date = NULL,
    rerpoj = TRUE, src = c("umd", "aws")) {
  src <- rlang::arg_match(src)

  src <- switch(src,
    umd = "https://glad.umd.edu/dataset/glad_ard2/",
    aws = "/vsis3/glad.landsat.ard/data/tiles/"
  )

  tiles <- tile_name_and_lat(aoi, reproj)
  lats <- sub(".*_", "", tiles)
  time_ints <- time_int_range(start_date, end_date)

  urls <- lapply(time_ints, function(x) {
    paste0(
      src,
      lats, "/", tiles, "/", x, ".tif"
    )
  })

  return(urls)
}

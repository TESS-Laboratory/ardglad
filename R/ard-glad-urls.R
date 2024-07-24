#' Generate URLs for GLAD ARD data
#' @param aoi an sf or sfc object representing the area of interest.
#' @param start_date a character string representing the start date.
#' @param end_date a character string representing the end date. if NULL, only
#' the start date is used and tiles for a single 16-day interval are returned.
#' @param rerpoj a logical value indicating whether to reproject the input if
#' it is not in the WGS84 CRS.
#' @param src a character string indicating the source of the data. Options are
#' "umd" (University of Maryland) and "aws" (Amazon Web Services).
#' @return An ard_glad_urls object (an s3 class), essentially a simple named
#' list of character vectors containing the URLs for the requested data.
#' Names correspond to the 16-day interval range.
#' @rdname ard-glad-urls
#' @family GLAD ARD urls
#' @details
#' Each item in the returned list comprises the tile(s) for a given 16-day
#' interval.
#' @export
ard_glad_urls <- function(
    aoi, start_date, end_date = NULL,
    rerpoj = TRUE, src = c("umd", "aws")) {
  src <- rlang::arg_match(src)

  src_base <- switch(src,
    umd = "https://glad.umd.edu/dataset/glad_ard2/",
    aws = "/vsis3/glad.landsat.ard/data/tiles/"
  )

  tiles <- tile_name_and_lat(aoi, reproj)
  lats <- sub(".*_", "", tiles)
  time_ints <- time_int_range(start_date, end_date)

  urls <- lapply(time_ints, function(x) {
    paste0(
      src_base,
      lats, "/", tiles, "/", x, ".tif"
    )
  })

  class(urls) <- c(glue::glue("ard_glad_urls_{src}"), class(urls))

  return(urls)
}

#' print method for ard_glad_urls objects
#' @rdname ard-glad-urls
#' @family GLAD ARD urls
#' @param x an ard_glad_urls object
#' @param ... not used
#' @export
print.ard_glad_urls_umd <- function(x, ...) {
  glad_print_method(x)
}

#' print method for ard_glad_urls objects
#' @rdname ard-glad-urls
#' @family GLAD ARD urls
#' @param x an ard_glad_urls object
#' @param ... not used
#' @export
print.ard_glad_urls_aws <- function(x, ...) {
  glad_print_method(x)
}

#' generic printing function for ard_glad_urls objects
#' @param x an ard_glad_urls object
#' @noRd
#' @keywords internal
glad_print_method <- function(x) {
  cli::cli_h3(cli::col_br_magenta("< ARD GLAD urls >"))
  pretty_p <- function(i) {
    cli::cli_h1(paste0("[[{which(names(x) == i)}]] ", i))
    y <- x[[i]]
    cat(
      cli::col_green(
        cli::style_italic(
          paste0(
            paste(y, collapse = "\n"), "\n"
          )
        )
      )
    )
  }
  names(x) |>
    lapply(pretty_p)
  invisible()
}

#' generic to coerce ard_glad_urls objects to a list
#' @export
#' @noRd
#' @keywords internal
as.list.ard_glad_urls_umd <- function(x, ...) {
  class(x) <- "list"
  return(x)
}

#' generic to coerce ard_glad_urls objects to a list
#' @export
#' @noRd
#' @keywords internal
as.list.ard_glad_urls_aws <- function(x, ...) {
  class(x) <- "list"
  return(x)
}

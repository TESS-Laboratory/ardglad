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
    reproj = TRUE, src = c("umd", "aws")) {
  # arg checks
  src <- rlang::arg_match(src)

  # TODO: let's write our own assertions remove the dep and make better messages
  assertthat::assert_that(length(end_date) <= 1)
  assertthat::assert_that(is.logical(reproj))

  if (length(start_date) > 1) {
    return(ard_glad_urls_multi(aoi, start_date, reproj, src))
  }

  # get the intersecting tile and lat names
  tiles <- tile_name_and_lat(aoi, reproj)
  lats <- sub(".*_", "", tiles)
  # get the time interval ids
  time_ints <- time_int_range(start_date, end_date)

  src_base <- switch(src,
    umd = "https://glad.umd.edu/dataset/glad_ard2/",
    aws = "/vsis3/glad.landsat.ard/data/tiles/"
  )

  urls <- lapply(time_ints, function(x) {
    paste0(
      src_base,
      lats, "/", tiles, "/", x, ".tif"
    )
  })

  class(urls) <- c(glue::glue("ard_glad_urls_{src}"), class(urls))

  return(urls)
}


ard_glad_urls_multi <- function(aoi, start_date, reproj, src) {
  urls_multi <- lapply(start_date, function(x) {
    ard_glad_urls(aoi, x, reproj = reproj, src = src)
  })
  urls_multi <- unlist(lapply(urls_multi, as.list), recursive = FALSE)
  class(urls_multi) <- c(glue::glue("ard_glad_urls_{src}"), class(urls_multi))
  return(urls_multi)
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

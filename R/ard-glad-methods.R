#' print method for ard_glad objects
#' @param x an ard_glad_src object
#' @param ... not used
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
print.ard_glad <- function(x, ...) {
  cli::cli_h3(
    cli::col_br_magenta("< {length(x)} ARD GLAD mosaic{?s} >")
  )
  pretty_p <- function(i) {
    cli::cli_h1(paste0("[[{which(names(x) == i)}]] ", i))
    y <- x[[i]]
    cli::cli_inform(paste(cli::col_br_blue("source      :"), y$src))
    cli::cli_inform(paste(
      cli::col_br_blue("dimensions  :"),
      paste(y$dim, collapse = ", "), "(nrow, ncol, nlyr)"
    ))
    cli::cli_inform(paste(
      cli::col_br_blue("resolution  :"),
      paste(round(y$res, 7), collapse = ", "), "(x, y)"
    ))
    cli::cli_inform(paste(
      cli::col_br_blue("extent      :"),
      paste(round(y$ext, 7), collapse = ", "),
      "(xmin, xmax, ymin, ymax)"
    ))
    crsinfo <- y$crs
    cli::cli_inform(paste(
      cli::col_br_blue("coord. ref. :"),
      ifelse(
        y$islonlat,
        "lon/lat",
        NULL
      ),
      ifelse(
        is.na(crsinfo$name),
        NULL,
        crsinfo$name
      ),
      ifelse(
        all(!is.na(c(crsinfo$authority, crsinfo$code))),
        glue::glue("({crsinfo$authority}:{crsinfo$code})"),
        NULL
      )
    ))
    cli::cli_inform(paste(
      cli::col_br_blue("band names   :"),
      paste(y$names, collapse = ", ")
    ))
  }
  lapply(names(x), pretty_p)
  invisible()
}

#' plot method for ard_glad objects
#' @param x a ard_glad object
#' @inheritParams terra::plotRGB
#' @param ... additional arguments passed to `terra::plotRGB`
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
plot.ard_glad <- function(
    x,
    r = 3, g = 2, b = 1, zlim = c(0, 5000),
    stretch = "lin", colNA = "#acacac", ...) {
  mapply(
    \(y, z) {
      terra::plotRGB(y,
        r = r, g = g, b = b,
        zlim = zlim,
        zcol = TRUE,
        stretch = "lin",
        axes = TRUE,
        mar = c(2, 2, 2, 1),
        main = z,
        colNA = colNA,
        ...
      )
    },
    as.list(x),
    names(x)
  )
  invisible()
}

#' generic to coerce ard_glad_urls objects to a list
#' @param x an `ard_glad` object
#' @param ... passed to `terra::rast`
#' @return a list of SpatRaster objects
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
as.list.ard_glad <- function(x, ...) {
  class(x) <- "list"
  lapply(x, \(r) terra::rast(r$src, ...))
}


#' function to construct the ard_glad object
#' @param x a list of SpatRaster objects
#' @return an ard_glad object
#' @noRd
#' @keywords internal
build_ard_glad <- function(x) {
  bag <- lapply(
    x,
    function(r) {
      if (!inherits(r, "SpatRaster")) {
        r <- terra::rast(r)
      }
      list(
        src = terra::sources(r),
        dim = dim(r),
        res = terra::res(r),
        ext = terra::ext(r),
        crs = terra::crs(r, describe = TRUE),
        islonlat = terra::is.lonlat(r),
        names = terra::names(r)
      )
    }
  )

  class(bag) <- c("ard_glad", class(bag))

  return(bag)
}


#' get sources for 'ard_glad' objects
#' @param x an 'ard_glad' object
#' @param ... not used
#' @return a list of sources
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
ard_glad_sources <- function(x, ...) {
  UseMethod("ard_glad_sources")
}

#' method for default sources - not supported
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
ard_glad_sources.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "x" = "A class of {class(x)} is not supported for the `ard_glad_sources` function",
      "i" = "Please use `ard_glad_download` to produce a supported class"
    )
  )
}

#' get sources for 'ard_glad' objects
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
ard_glad_sources.ard_glad <- function(x, ...) {
  class(x) <- "list"
  lapply(x, \(x) x$src)
}


#' function to construct new GeoTiff filenames for ard_glad objects
#' @param x an ard_glad object
#' @param label a character string to append to the filename
#' @param ... not used
#' @return a character vector of new filenames
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
#' @details This function is used for creating new filenmaes for providing
#' save locations for functions such as `ard_glad_mask`
new_source_names <- function(x, ...) {
  UseMethod("new_source_names")
}

#' method for default sources - not supported
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
new_source_names.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "x" = "A class of {class(x)} is not supported for the `new_source_names`
        function",
      "i" = "Please use `ard_glad_download` to produce a supported class"
    )
  )
}

#' method for ard_glad sources
#' @rdname ard-glad-utility
#' @family GLAD ARD utility
#' @export
new_source_names.ard_glad <- function(x, label, ...) {
  class(x) <- "list"
  sapply(x, \(x) {
    dn <- dirname(x$src)
    bn <- tools::file_path_sans_ext(basename(x$src))
    file.path(dn, paste0(bn, "_", label, ".tif"))
  })
}

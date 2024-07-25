#' Download GLAD ARD data
#' @param glad_urls a list of URLs to download from `ard_glad_urls`
#' @param dir a character string of the directory to save the files
#' @param prefix a character string to prefix the subdirectory/filenames
#' @param return_filename a logical value indicating whether to return the
#' filename or the SpatRaster object
#' @return a list of SpatRaster objects or filenames if `return_filename` is
#' TRUE
#' @export
#' @rdname ard-glad-download
#' @family GLAD ARD download
#'
ard_glad_download <- function(
    glad_urls, dir,
    prefix = dir,
    return_filename = FALSE) {
  UseMethod("ard_glad_download")
}

#' method for default sources - not supported
#' @export
ard_glad_download.default <- function(glad_urls, ...) {
  cli::cli_abort(
    c(
      "x" = "A class of {class(glad_urls)} is not supported for the `ard_glad_download` function",
      "i" = "Please use `ard_glad_urls` to produce a supported class"
    )
  )
}

#' method for AWS sources - not supported
#' @export
ard_glad_download.ard_glad_urls_aws <- function(glad_urls, ...) {
  cli::cli_abort(
    c(
      "x" = "The AWS source is not yet supported",
      "i" = "Please use the umd source for now"
    )
  )
}

#' method for UMD sources
#' @export
ard_glad_download.ard_glad_urls_umd <- function(
    glad_urls, dir, prefix = dir, return_filename = FALSE) {
  glad <- mapply(
    \(x, y){
      ard_glad_download_ts(x, y, dir, prefix, return_filename)
    },
    as.list(glad_urls),
    as.list(names(glad_urls))
  )

  # TODO: NEED TO BETTER HANDLE RETURN FILENAME - new class or attribute?
  class(glad) <- c("ard_glad", class(glad))

  return(glad)
}

#' generic to coerce ard_glad_urls objects to a list
#' @export
#' @noRd
#' @keywords internal
as.list.ard_glad <- function(x, ...) {
  class(x) <- "list"
  return(x)
}


ard_glad_download_ts <- function(x, name, dir, prefix, return_filename) {
  name_split <- strsplit(x, "/")

  mk_name <- paste(
    dir,
    gsub("[:\\-]", "_", name),
    sep = "_"
  )

  nested_dir <- file.path(dir, mk_name)

  if (!file.exists(nested_dir)) {
    dir.create(nested_dir, recursive = TRUE)
  }

  vrt_name <- file.path(nested_dir, paste0(mk_name, ".vrt"))

  out_files <- file.path(
    nested_dir,
    lapply(name_split, \(x) {
      x <- tail(x, 3)
      paste0(c(x[2], x[3]), collapse = "_")
    })
  )

  cli::cli_alert_info("Downloading files for time period: {name}")

  down_df <- curl::multi_download(x,
    destfiles = out_files,
    userpwd = "glad:ardpas",
    resume = TRUE
  )

  check_status_codes(down_df)

  glad_ard_vrt <- build_vrt(out_files, vrt_name)

  if (return_filename) {
    return(vrt_name)
  }

  return(glad_ard_vrt)
}

build_vrt <- function(files, vrtname) {
  glad_vrt <- terra::vrt(
    files,
    filename = vrtname,
    overwrite = TRUE
  )
  names(glad_vrt) <- c("B", "G", "R", "N", "S1", "S2", "T", "QA")
  terra::update(glad_vrt, names = TRUE)
  return(glad_vrt)
}

#' Check status codes of downloads - fail if there are errors.
#' @param x a data frame of download from curl::multi_download
#' @noRd
#' @keywords internal
#' @return NULL
#'
check_status_codes <- function(x) {
  if (any(isFALSE(x$success))) {
    cli::cli_abort(
      c(
        "x" = "Some files have failed to download...",
        "i" = "This may be simply be a network error -
        try again to attempt resuming the download."
      )
    )
  }
}

#' print method for ard_glad objects
#' @rdname ard-glad-download
#' @family GLAD ARD download
#' @param x an ard_glad object
#' @param ... not used
#' @export
print.ard_glad <- function(x, ...) {
  cli::cli_h3(cli::col_br_magenta("< ARD GLAD rasters>"))
  pretty_p <- function(i) {
    cli::cli_h1(paste0("[[{which(names(x) == i)}]] ", i))
    y <- x[[i]]
    print(y)
  }
  lapply(names(x), pretty_p)
  invisible()
}

#' plot method for ard_glad objects
#' @rdname ard-glad-download
#' @family GLAD ARD download
#' @param x an ard_glad object
#' @param ... not used
#' @export
plot.ard_glad <- function(x) {
  mapply(
    \(y, z) {
      terra::plotRGB(y,
        r = 3, g = 2, b = 1,
        zlim = c(0, 5000),
        zcol = TRUE,
        stretch = "lin",
        axes = TRUE,
        mar = c(2, 2, 2, 1),
        main = z
      )
    },
    x,
    names(x)
  )
  invisible()
}

#' Download GLAD ARD data
#' @param glad_urls a list of URLs to download from `ard_glad_urls`
#' @param dir a character string of the directory to save the files
#' @param prefix a character string to prefix the subdirectory/filenames
#' @param quiet a logical to suppress messages
#' @param ... additional arguments not used
#' @return a list of SpatRaster objects or filenames if `return_filename` is
#' TRUE
#' @export
#' @rdname ard-glad-download
#' @family GLAD ARD download
#'
ard_glad_download <- function(
    glad_urls, dir,
    prefix = dir,
    quiet = TRUE, ...) {
  UseMethod("ard_glad_download")
}

#' method for default sources - not supported
#' @rdname ard-glad-download
#' @family GLAD ARD download
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
#' @rdname ard-glad-download
#' @family GLAD ARD download
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
#' @rdname ard-glad-download
#' @family GLAD ARD download
#' @export
ard_glad_download.ard_glad_urls_umd <- function(
    glad_urls, dir, prefix = dir, quiet = TRUE, ...) {
  assertthat::assert_that(is.character(dir))
  assertthat::assert_that(is.character(prefix))
  assertthat::assert_that(is.logical(quiet))

  glad <- future.apply::future_mapply(
    \(x, y) {
      ard_glad_download_ts(x, y, dir, prefix, quiet, return_filename = TRUE)
    },
    as.list(glad_urls),
    as.list(names(glad_urls)),
    future.seed = TRUE
  )

  return(build_ard_glad(glad))
}


ard_glad_download_ts <- function(x, name, dir, prefix, quiet, return_filename) {
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

  if (!quiet) cli::cli_alert_info("Downloading files for time period: {name}")

  down_df <- curl::multi_download(x,
    destfiles = out_files,
    userpwd = "glad:ardpas",
    resume = TRUE,
    multiplex = TRUE,
    progress = !quiet
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

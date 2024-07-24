#' Download GLAD ARD data
#' @param glad_urls a list of URLs to download from `ard_glad_urls`
#' @param dir a character string of the directory to save the files
#' @param prefix a character string to prefix the subdirectory/filenames
#' @param return_filenmae a logical value indicating whether to return the
#' filename or the SpatRaster object
#' @return a list of SpatRaster objects or filenames if `return_filenmae` is
#' TRUE
#' @export
#'
ard_glad_download <- function(
    glad_urls, dir, prefix = dir, return_filenmae = FALSE) {
  mapply(
    \(x, y){
      ard_glad_download_ts(x, y, dir, prefix, return_filenmae)
    },
    as.list(glad_urls),
    as.list(names(glad_urls))
  )
}


ard_glad_download_ts <- function(x, name, dir, prefix, return_filenmae) {
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

  if (return_filenmae) {
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

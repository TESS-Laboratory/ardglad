ard_glad_download <- function(x, name, dir, return_filenmae = FALSE) {
  browser()
  if (!file.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  name_split <- strsplit(x, "/")
  # dttm_id <- gsub(".tif", "", tail(name_split[[1]], 1))

  mk_name <- paste(
    dir,
    gsub("[:\\-]", "_", name),
    sep = "_"
  )

  nested_dir <- file.path(dir, mk_name)

  vrt_name <- file.path(nested_dir, paste0(mk_name, ".vrt"))

  out_files <- file.path(
    nested_dir,
    lapply(name_split, \(x) {
      x <- tail(x, 3)
      paste0(c(x[2], x[3]), collapse = "_")
    })
  )

  down_df <- curl::multi_download(x,
    destfiles = out_files,
    userpwd = "glad:ardpas"
  )

  check_status_codes(down_df)

  glad_vrt <- terra::vrt(
    out_files,
    filename = vrt_name
  )
  names(glad_vrt) <- c("B", "G", "R", "N", "S1", "S2", "T", "QA")
  terra::update(glad_vrt, names = TRUE)

  if (return_filenmae) {
    return(vrt_name)
  }

  return(glad_vrt)
}


check_status_codes <- function(x) {
  if (any(x$status_code %in% c(200, 206, 416)) || any(isFALSE(x$success))) {
    cli::cli_abort(
      c(
        "x" = "Some files have failed to download...",
        "i" = "This may be simply be a network error -
        try again to attempt resuming the download."
      )
    )
  }
}

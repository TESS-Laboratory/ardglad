#' Mask GLAD ARD data for shadows and clouds
#' @param glad a list of SpatRaster objects
#' @param mask_band a character string representing the band to use as the mask
#' @param keep_bits a numeric vector of bits to keep see details
#' @return an object of class `ard_glad` simply a list of SpatRaster objects
#' with a convenient class for GLAD specific operations.
#' @details The `keep_bits` argument is used to create a binary mask from the
#' `mask_band`. The description of each bit are:
#' 0 - No data; 1 - Land; 2 - Water; 3 - Cloud; 4 - Cloud shadow;
#' 5 - Topographic Shadow; 6 - Snow/Ice; 7 - Haze; 8 - Cloud proximity;
#' 9 - Shadow proximity; 10 - Other shadows;
#' 11 - Additional cloud proximity over land;
#' 12 - Additional cloud proximity over water;
#' 14 - Additional shadow proximity over land; 15 - Same as code 1. Land;
#' 16 - Same as code 11. Additional cloud proximity over land;
#' 17 - Same as code 14. Additional shadow proximity over land.
#'
#' see Potapov, et. al (2020) for more details (https://doi.org/10.3390/rs12030426)
#' @export
ard_glad_mask <- function(
    glad, mask_band = "QA",
    keep_bits = c(1, 2, 15)) {
  UseMethod("ard_glad_mask")
}

#' method for default sources - not supported
#' @export
ard_glad_mask.default <- function(glad, ...) {
  cli::cli_abort(
    c(
      "x" = "A class of {class(glad)} is not supported for the `ard_glad_mask` function",
      "i" = "Please use `ard_glad_download` to produce a supported class"
    )
  )
}

#' method for ard_glad objects
#' @export
ard_glad_mask.ard_glad <- function(
    glad, mask_band = "QA",
    keep_bits = c(1, 2, 15)) {
  apply_mask <- function(r) {
    # Extract the mask band
    mask <- r[[mask_band]]
    # Create a binary mask
    binary_mask <- terra::app(mask, \(x) ifelse(!x %in% keep_bits, NA, 1))
    # Apply the mask to the raster
    masked_raster <- terra::mask(r, binary_mask)
    return(masked_raster)
  }

  glad <- lapply(glad, apply_mask)

  class(glad) <- c("ard_glad", class(glad))

  return(glad)
}

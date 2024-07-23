#' function to load the global ARD tiles
#' @export
#' @keywords internal
#' @details
#' This tile dataset is from
#' https://glad.umd.edu/users/Potapov/ARD/Global_ARD_tiles.zip
gladtiles <- function() {
  sf::read_sf(
    system.file("global_ard_tiles/Global_ARD_tiles.fgb", package = "ardglad")
  )
}

#' Get glad tile names that intersect an aoi.
#' @param x an sf object representing the area of interest
#' @param reproj a logical value indicating whether to reproject the input.
#' @return a character vector of tile names
#' @noRd
#' @keywords internal
#' @details
#' If reproj is FALSE, an error is thrown when the input object is not in a
#' geographic coordinate system.
tile_name_and_lat <- function(x, reproj = TRUE) {
  class_assert(x, c("sf", "sfc", "sfg"))
  x <- assert_wgs84(x, reproj)
  tiles_intersects <- gladtiles() |>
    sf::st_filter(x)

  return(tiles_intersects$TILE)
}


assert_wgs84 <- function(x, reproj) {
  if (!sf::st_crs(x)$epsg == 4326) {
    if (reproj) {
      x <- sf::st_transform(x, 4326)
      cli::cli_warn(
        c("!" = "The input area of interest was reprojected to 'EPSG:4326' in
        order to derive determine tiles. ")
      )
    } else {
      cli::cli_abort(
        c(
          "x" = "The input object must use the WGS84 (EPSG:4326) CRS"
        )
      )
    }
  }
  return(x)
}

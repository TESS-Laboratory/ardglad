## code to prepare `global_ard_tiles` dataset goes here

if (!dir.exists("inst/global_ard_tiles")) {
  dir.create("inst/global_ard_tiles", recursive = TRUE)
}

glad_tiles_path <- "inst/global_ard_tiles/Global_ARD_tiles.fgb"
glad_tiles_zip <- "inst/global_ard_tiles/Global_ARD_tiles.zip"
# get the glad tiles
glad_tiles <- sf::read_sf(
  "/vsizip/vsicurl/https://glad.umd.edu/users/Potapov/ARD/Global_ARD_tiles.zip"
)

sf::write_sf(glad_tiles, glad_tiles_path, delete_dsn = TRUE)

utils::zip(glad_tiles_zip, glad_tiles_path)
file.remove(glad_tiles_path)

glad_tiles <- sf::read_sf(paste0("/vsizip/", glad_tiles_zip))

glad_tiles

# check if you want
# mapview::mapview(glad_tiles)

# we dont want to save as an R object so don't do this
# usethis::use_data(glad_tiles, overwrite = TRUE)

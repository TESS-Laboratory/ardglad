test_that("ard_glad_urls lengths are good", {
  tloc <- sf::st_point(c(117.371, 5.114)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_sf() |>
    sf::st_buffer(100000)

  url_list1 <- ard_glad_urls(tloc, "2022-03-03")
  expect_length(url_list1, 1)
  expect_length(url_list1[[1]], 7)

  url_list2 <- ard_glad_urls(tloc, "2022-04-01", "2024-04-30")
  expect_length(url_list2, 49)

  lapply(url_list2, function(x) {
    expect_length(x, 7)
  })
})

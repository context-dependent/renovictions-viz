library(sf)
library(mgcv)
library(dplyr)
library(rayshader)

triangle <- st_sfc(st_polygon(list(rbind(c(0, 0), c(3, 0), c(0, 4), c(0, 0)))))

triangle |>
    st_cast("LINESTRING") |>
    st_line_sample(density = 10, type = "regular") |>
    plot()
triangle |>
    st_cast("LINESTRING") |>
    st_length()


generate_circle <- function(radius, point_density = 3) {
    circle <- st_sfc(st_point(c(0, 0))) |> st_buffer(radius)
    circle |>
        st_cast("LINESTRING") |>
        st_line_sample(density = point_density, type = "regular")
}

circle <- generate_circle(10)

circle

hump <- tibble(
    elevation = c(0, 1, 2, 3),
    radius = c(16, 8, 4, 2)
)

d_hump <- hump |>
    mutate(
        contour = purrr::map(radius, generate_circle) |> purrr::reduce(c)
    ) |>
    st_as_sf() |>
    st_cast("POINT") |>
    mutate(
        x = st_coordinates(contour)[, 1],
        y = st_coordinates(contour)[, 2]
    )


mod <- mgcv::gam(elevation ~ s(x, y, bs = "tp", k = 10), data = d_hump)

grid <- tidyr::expand_grid(
    x = seq(-20, 20, length.out = 100),
    y = seq(-20, 20, length.out = 100)
)

grid <- grid |>
    mutate(
        z = predict(mod, newdata = grid)
    )

library(ggplot2)

grid |>
    ggplot() +
    geom_raster(aes(x, y, fill = z))

matrix_hump <- grid$z |> matrix(ncol = 100, byrow = TRUE)

matrix_hump |>
    sphere_shade(texture = "desert") |>
    add_shadow(ray_shade(matrix_hump, zscale = 3), 0.5) |>
    add_shadow(ambient_shade(matrix_hump), 0) |>
    plot_3d(matrix_hump, z_scale = 1000, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

render_snapshot()

t <- tempfile()
download.file("https://tylermw.com/data/dem_01.tif.zip", t)
tif <- raster::raster(unzip(t, "dem_01.tif"))

m <- raster_to_matrix(tif)

m[1:10, 1:10]

m |>
    sphere_shade(texture = "desert") |>
    plot_map()


install.packages("terra")
library(terra)
tester_640483 <- rast("data-raw/SW/dm_640483.tif")
tester_640485 <- rast("data-raw/SW/dm_640485.tif")

terra::plot(terra::merge(tester_640483, tester_640485))

ingest_dem <- function(dir_path) {
    dir_path |>
        fs::dir_ls(regexp = "tif$") |>
        purrr::map(terra::rast) |>
        terra::sprc()
}

d <- ingest_dem("data-raw/SW/")
terra::merge(d, filename = "data/dem_gta-sw.tif", overwrite = TRUE)
d <- terra::merge(tester_640483, tester_640485, filename = "data/dem_gta-sw.tif", overwrite = TRUE)
d

library(sf)

gta_dem_tiles <- st_read("data-raw/GTAORTHODEM.shp")
gta_dem_tiles <- gta_dem_tiles |>
    filter(Region == "SW") |>
    mutate(
        id = stringr::str_extract(Thumbnail, r"(\d+(?=\.jpg$))"), 
        dem = purrr::map(id, ~ terra::rast(glue::glue("data-raw/SW/dm_{.x}.tif")))
    )

dem_crs <- st_crs(gta_dem_tiles)

hoods <- st_read("data-raw/boundaries.geojson") |>
    st_transform(dem_crs)

to_proper <- hoods$geometry |> st_union()

d <- gta_dem_tiles |>
    filter(unclass(st_intersects(geometry, to_proper, sparse = FALSE)))  |>
    pull(dem) |>
    terra::sprc() |>
    terra::merge(filename = "dem_to-proper.tif", overwrite = TRUE) |>
    terra::crop(to_proper)

terra::plot(d)

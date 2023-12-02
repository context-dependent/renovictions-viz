library(tidyverse)
library(sf)
library(opendatatoronto)
library(rayshader)

# Neighbourhood Census Profiles
odt_id <- opendatatoronto::search_packages("neighbourhood-profiles")$id

odt_res <- odt_id |> list_package_resources()

profiles <- odt_res |>
    slice(1) |>
    get_resource()

# We've got to do some stuff to make them usable
neighbourhood_profiles <- profiles$hd2021_census_profile |>
    rename(census_vector = `Neighbourhood Name`) |>
    filter(
        census_vector |>
            str_detect("^Total - Age groups|Neighbourhood Number|Renter$|Owner$|Prevalence.+LIM-AT|^Total.+shelter-cost-to-income ratio|^Spending 30.+more of income on shelter costs$")
    ) |>
    pivot_longer(
        cols = -census_vector,
        names_to = "neighbourhood_name",
        values_to = "value"
    ) |>
    pivot_wider(
        names_from = census_vector,
        values_from = value
    ) |>
    set_names(
        c(
            "name",
            "id", "population", "low_income_pct",
            "dwell_owned", "dwell_rented",
            "dwell_total",
            "dwell_private_unaffordable",
            "dwell_private_total"
        )
    ) |>
    transmute(
        id = as.integer(id), name,
        population = as.integer(population),
        n_rentals = as.integer(dwell_rented),
        p_low_income = as.double(low_income_pct) / 100,
        p_unaffordable_dwelling = as.double(dwell_private_unaffordable) /
            as.double(dwell_private_total)
    )


# Neighbourhood boundaries (also from opendatato)
neighbourhood_boundaries <- st_read("data-raw/boundaries.geojson") |>
    transmute(
        id = AREA_SHORT_CODE |> as.integer(),
        name_odto = AREA_NAME,
        nia = CLASSIFICATION
    )

# Combining the boundaries and the profiles
neighbourhood_data <- neighbourhood_boundaries |>
    left_join(neighbourhood_profiles, by = "id") |>
    arrange(id) |>
    mutate(
        area_km2 = as.double(st_area(geometry) / 1e6),
        population_density = population / area_km2
    )

neighbourhood_data


# Spatial points of renovictions (and agis, own use)
renovictions <- read_csv("data-raw/renovictions.csv") |>
    transmute(
        year = as.integer(str_sub(date_initiated, 1, 4)),
        type = type_agi_r,
        lat = latitude,
        lon = longitude
    ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)


# 1. Renovictions over Neighbourhoods
neighbourhood_data |>
    ggplot() +
    geom_sf() +
    geom_sf(
        aes(fill = type),
        data = renovictions,
        size = 1,
        shape = 21,
        alpha = .8
    ) +
    theme_void()

# 2. Kind of reflect population density
neighbourhood_data |>
    ggplot() +
    geom_sf(aes(fill = population_density)) +
    geom_sf(data = renovictions, size = 1, shape = 21, alpha = .8) +
    scale_fill_viridis_c(trans = "log") +
    theme_void()

# 3. Combining points and areas
bare_hoods <- neighbourhood_data |>
    select(id) |>
    st_transform(3347)

renovictions_by_hood <- renovictions |>
    st_transform(3347) |>
    st_join(bare_hoods) |>
    st_set_geometry(NULL) |>
    filter(type %in% c("own use", "renoviction")) |>
    group_by(id, type) |>
    summarize(n_discretionary_evictions = n())

neighbourhood_data_plus <- neighbourhood_data |>
    left_join(renovictions_by_hood) |>
    mutate(
        discretionary_evictions_per_k = case_when(
            !is.na(n_discretionary_evictions) ~ n_discretionary_evictions / n_rentals * 1000,
            TRUE ~ 0
        ),
        depk_plus = ntile(discretionary_evictions_per_k, 100)
    )

neighbourhood_data_plus |>
    ggplot(aes(fill = discretionary_evictions_per_k)) +
    geom_sf() +
    scale_fill_viridis_c() +
    theme_void()


neighbourhood_data_plus |>
    ggplot(aes(fill = depk_plus)) +
    geom_sf() +
    scale_fill_viridis_c() +
    theme_void()


# 4. Low income and depk+
neighbourhood_data_plus |>
    ggplot(aes(p_low_income, discretionary_evictions_per_k)) +
    geom_point() +
    geom_smooth(method = "lm")

# 5. Unaffordable housing and depk+
neighbourhood_data_plus |>
    ggplot(aes(p_unaffordable_dwelling, discretionary_evictions_per_k)) +
    geom_point() +
    geom_point(
        shape = 21, colour = "tomato", size = 3, stroke = 1,
        data = function(d) {
            d |>
                arrange(desc(depk_plus)) |>
                slice(1:10)
        }
    ) +
    geom_smooth(method = "lm")

# 6. Top 10 neighbourhoods by depk+
neighbourhood_data_plus |>
    arrange(desc(discretionary_evictions_per_k)) |>
    slice(1:10) |>
    mutate(name = fct_inorder(name) |> fct_rev()) |>
    ggplot(aes(discretionary_evictions_per_k, name)) +
    geom_col()

# 7. NIAs and depk+
#    [^emerging-neighbourhoods]:
#        emerging neighbourhood is a designation given to neighbourhoods that were identified as
#        "Priority Investment Areas" (PIAs) in 2009, but which were not identified as "Neighbourhood Improvement Areas" (NIAs) in 2014.
#        See [this map](https://www.toronto.ca/wp-content/uploads/2017/11/8f72-priority_areas_with_neighbourhoods.pdf) for historical PIA designations,
#        and [this report](https://www.toronto.ca/legdocs/mmis/2014/cd/bgrd/backgroundfile-67382.pdf) detailing the methodology for identifying NIAs.

neighbourhood_data_plus |>
    ggplot() +
    geom_sf(aes(fill = nia))

neighbourhood_data_plus |>
    ggplot(aes(depk_plus, nia)) +
    ggbeeswarm::geom_quasirandom()

neighbourhood_data_plus |>
    st_set_geometry(NULL) |>
    group_by(nia) |>
    summarize(
        mean_depk = mean(discretionary_evictions_per_k, na.rm = TRUE),
        mean_p_low_income = mean(p_low_income, na.rm = TRUE),
        mean_p_unaffordable_dwelling = mean(
            p_unaffordable_dwelling,
            na.rm = TRUE
        )
    )

st_crs(neighbourhood_data_plus)

library(leaflet)

pal_num <- colorNumeric(
    palette = "magma",
    domain = neighbourhood_data_plus$discretionary_evictions_per_k
)

neighbourhood_data_plus |>
    leaflet() |>
    addProviderTiles(
        providers$CartoDB.Positron
    ) |>
    addPolygons(
        fillColor = ~ pal_num(discretionary_evictions_per_k),
        fillOpacity = .5,
        stroke = FALSE,
        smoothFactor = 0.05,
        label = ~name
    ) |>
    addLegend(
        title = "Discretionary Evictions<br>per 1000 Private Dwellings",
        pal = pal_num,
        values = ~discretionary_evictions_per_k,
        opacity = 1
    )


# 8. Using the third dimension?
tiles <- st_read("data-raw/GTAORTHODEM.shp")
dem_crs <- st_crs(tiles)
mercator_hoods <- neighbourhood_data_plus |>
    st_transform(dem_crs)
toronto_footprint <- st_bbox(mercator_hoods) |>
    st_as_sfc()

toronto_tiles <- tiles |>
    mutate(in_bbox = purrr::map_lgl(geometry, st_intersects, toronto_footprint, sparse = FALSE))

toronto_tiles |>
    filter(in_bbox)

toronto_tiles |>
    ggplot() +
    geom_sf(aes(fill = in_bbox)) +
    geom_sf(data = mercator_hoods)



toronto_tiles <- toronto_tiles |>
    filter(in_bbox) |>
    mutate(
        id = stringr::str_extract(Thumbnail, r"(\d+(?=\.jpg$))")
    )

test_tile <- glue::glue("data-raw/SW/dm_{toronto_tiles$id[[2]]}.tif") |>
    raster::raster()

summary(test_tile)
plot(test_tile)

test_full <- raster::raster("data-raw/SW/dm_640483.tif")

raster::crs(test_full) <- dem_crs$wkt
str(test_full)
raster::getValues(test_full)
test_full |> raster_to_matrix() |> summary()
dem_crs$proj4string
test_matrix <- test_full |> raster_to_matrix()
test_matrix[1:10, 1:10]
dim(test_matrix)

test_matrix <- raster_to_matrix(test_full)
test_zip <- raster::raster(unzip("data-raw/GTA2002_DEM-SW.zip"))

test_matrix |>
    sphere_shade(texture = "desert") |>
    add_shadow(ray_shade(test_tile, zscale = 3), 0.5) |>
    plot_3d(test_matrix, zscale = 3)
install.packages("stars")
library(stars)

read_stars(unzip("data-raw/GTA2002_DEM-SW.zip"))

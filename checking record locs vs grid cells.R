# unique values
grid_tiles <- unique(grid_lizard$tile_name)
obs_tiles  <- unique(obs_sf$gridref)

# intersection
matching_tiles <- intersect(grid_tiles, obs_tiles)

length(matching_tiles)

setdiff(obs_tiles, grid_tiles)
setdiff(grid_tiles, obs_tiles)

obs_outside <- obs_sf %>%
  filter(!(gridref %in% grid_lizard$tile_name))

library(ggplot2)

ggplot() +
  geom_sf(data = grid_lizard, fill = NA, colour = "grey70") +
  geom_sf(data = obs_outside, colour = "red", size = 1) +
  theme_minimal() +
  labs(
    title = "Records not matching GIS grid tiles",
    subtitle = "Red points fall outside grid tile naming system"
  )

nrow(obs_outside)

sort(table(obs_outside$taxon.name))

library(sf)

obs_sf_ll <- st_transform(obs_sf, 4326)
grid_lizard_ll <- st_transform(grid_lizard, 4326)

obs_outside <- obs_sf_ll %>%
  filter(!(gridref %in% grid_lizard$tile_name))

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = grid_lizard_ll,
    color = "grey",
    weight = 1,
    fill = FALSE
  ) %>%
  addCircleMarkers(
    data = obs_outside,
    radius = 4,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>Species:</b> ", taxon.name, "<br>",
      "<b>GridRef:</b> ", gridref, "<br>",
      "<b>Obs ID:</b> ", id
    )
  )

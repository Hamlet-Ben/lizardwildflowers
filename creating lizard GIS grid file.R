library(sf)
library(dplyr)

UK_regions <- st_read("GIS Data/UK Regions UK grid.shp")

library(sf)

grid <- st_read("GIS Data/South Cornwall 1km grids.shp")

bbox <- st_bbox(c(
  xmin = -5.32,
  ymin = 49.95,
  xmax = -5.00,
  ymax = 50.10
), crs = 4326)

bbox_sf <- st_as_sfc(bbox) %>%
  st_transform(27700)

lizard_area <- st_intersection(UK_regions, bbox_sf)

grid_lizard <- st_intersection(grid, lizard_area)

# fast reload for analysis
saveRDS(grid_lizard, "GIS Data/grid_lizard.rds")

# portable spatial file
st_write(grid_lizard, "GIS Data/grid_lizard.gpkg", delete_dsn = TRUE)

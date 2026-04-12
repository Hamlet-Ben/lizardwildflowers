# ============================================================
# 1. DOWNLOAD iNaturalist DATA (ANGIOSPERMS, LIZARD BBOX)
# ============================================================

# Pull observations from iNaturalist API using:
# - taxon_id = 47125 → Angiosperms (flowering plants)
# - bounding box → Lizard Peninsula region
# - pagination handled in your custom function

source("lizard helper functions.R")

obs <- get_inat_obs_project_v2(
  taxon_id = 47125,
  swlat = 49.95,
  swlng = -5.32,
  nelat = 50.10,
  nelng = -5.00,
  max_pages = 50
)

# Result: flattened data frame of iNat observations


# ============================================================
# 2. LOAD REQUIRED PACKAGES
# ============================================================

library(sf)      # spatial data handling
library(dplyr)   # data manipulation
library(purrr)   # list-column handling (for coordinates)


# ============================================================
# 3. EXTRACT COORDINATES + CONVERT TO sf OBJECT
# ============================================================

obs_sf <- obs %>%
  # Keep only records with coordinates
  filter(!is.na(geojson.coordinates)) %>%
  
  # Extract lon/lat from list column
  mutate(
    lon = map_dbl(geojson.coordinates, 1),
    lat = map_dbl(geojson.coordinates, 2)
  ) %>%
  
  # Convert to spatial object (WGS84)
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Result: sf object with point geometry in lat/lon


# ============================================================
# 4. TRANSFORM TO BRITISH NATIONAL GRID (EPSG:27700)
# ============================================================

# Required for:
# - 1 km grid alignment
# - UK-standard spatial analysis

obs_sf <- st_transform(obs_sf, 27700)


# ============================================================
# 5. FILTER DATA FOR QUALITY + RELEVANCE
# ============================================================

obs_sf <- obs_sf %>%
  filter(
    quality_grade == "research",  # verified observations only
    !is.na(taxon.id),             # ensure taxon is defined
    !is.na(user.login),           # ensure observer is known
    captive == FALSE              # exclude garden/captive records
  )

# Result: clean, high-quality wild plant observations


# ============================================================
# 6. EXTRACT PROJECTED COORDINATES (EASTING/NORTHING)
# ============================================================

coords <- st_coordinates(obs_sf)

obs_sf <- obs_sf %>%
  mutate(
    easting = coords[,1],   # X coordinate (metres)
    northing = coords[,2]   # Y coordinate (metres)
  )


# ============================================================
# 7. ASSIGN 1 KM GRID CELLS
# ============================================================

# Snap each observation to a 1 km grid cell by flooring coordinates

obs_sf <- obs_sf %>%
  mutate(
    cell_e = floor(easting / 1000) * 1000,
    cell_n = floor(northing / 1000) * 1000,
    cell_id = paste(cell_e, cell_n, sep = "_")
  )

# Result:
# - Each observation linked to a unique 1 km grid cell
# - cell_id used for grouping and summarisation


# ============================================================
# 8. CONVERT TO BRITISH NATIONAL GRID REFERENCES
# ============================================================

# Convert coordinates into human-readable grid references
# (e.g. "SW7145")

obs_sf <- obs_sf %>%
  mutate(
    gridref = bng_to_gridref(easting, northing, digits = 4)
  )

# Note:
# - digits = 4 → 1 km resolution
# - useful for reporting and field interpretation


# ============================================================
# 9. SAVE PROCESSED DATA
# ============================================================

# Save as RData for fast reload in future sessions

save(obs_sf, file = "obs_sf.RData")

# Alternative (often better):
# saveRDS(obs_sf, "obs_sf.rds")

# Result:
# - fully processed spatial dataset
# - ready for analysis, mapping, and reporting
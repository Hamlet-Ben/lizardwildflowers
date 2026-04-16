# ============================================================
# INCREMENTAL UPDATE HELPERS
# ============================================================

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(sf)

# ------------------------------------------------------------
# Download iNat observations with support for updated-since queries
# ------------------------------------------------------------
get_inat_obs_project_v3 <- function(project_slug = NULL,
                                    per_page = 200,
                                    max_pages = 100,
                                    quality_grade = NULL,
                                    place_id = NULL,
                                    taxon_id = NULL,
                                    d1 = NULL, d2 = NULL,
                                    updated_since = NULL,
                                    verbose = TRUE,
                                    ...) {
  all_results <- list()
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  for (page in seq_len(max_pages)) {
    if (verbose) cat("Fetching page", page, "...\n")
    
    query <- list(
      per_page = per_page,
      page = page,
      quality_grade = quality_grade,
      place_id = place_id,
      taxon_id = taxon_id,
      d1 = d1,
      d2 = d2,
      updated_since = updated_since,
      ...
    )
    
    if (!is.null(project_slug)) {
      query$project_id <- project_slug
    }
    
    query <- query[!vapply(query, is.null, logical(1))]
    
    res <- GET(url = base_url, query = query)
    
    if (status_code(res) != 200) {
      warning("API request failed on page ", page)
      break
    }
    
    content_json <- fromJSON(
      content(res, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    
    if (length(content_json$results) == 0) break
    
    all_results[[page]] <- content_json$results
    
    # Stop once we've fetched everything available
    if (!is.null(content_json$total_results) &&
        page * per_page >= content_json$total_results) {
      break
    }
  }
  
  bind_rows(all_results)
}


# ------------------------------------------------------------
# Find most recent updated_at timestamp in an existing dataset
# ------------------------------------------------------------
get_latest_updated_at <- function(obs_df) {
  if (!"updated_at" %in% names(obs_df)) {
    stop("Column 'updated_at' not found in existing dataset.")
  }
  
  latest_ts <- max(as.POSIXct(obs_df$updated_at, tz = "UTC"), na.rm = TRUE)
  
  if (!is.finite(latest_ts)) {
    stop("Could not determine a valid latest 'updated_at' timestamp.")
  }
  
  latest_ts
}


# ------------------------------------------------------------
# Turn raw iNat download into your cleaned sf object
# ------------------------------------------------------------
process_inat_obs_to_sf <- function(obs_raw) {
  obs_sf <- obs_raw %>%
    filter(!is.na(geojson.coordinates)) %>%
    mutate(
      lon = map_dbl(geojson.coordinates, 1),
      lat = map_dbl(geojson.coordinates, 2)
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(27700) %>%
    filter(
      quality_grade == "research",
      !is.na(taxon.id),
      !is.na(user.login),
      captive == FALSE
    )
  
  coords <- st_coordinates(obs_sf)
  
  obs_sf %>%
    mutate(
      easting = coords[, 1],
      northing = coords[, 2],
      cell_e = floor(easting / 1000) * 1000,
      cell_n = floor(northing / 1000) * 1000,
      cell_id = paste(cell_e, cell_n, sep = "_"),
      gridref = bng_to_gridref(cell_e, cell_n, digits = 4)
    )
}


# ------------------------------------------------------------
# Incrementally update an existing dataset
# ------------------------------------------------------------
update_inat_dataset <- function(existing_obs_sf,
                                project_slug = NULL,
                                taxon_id = 47125,
                                swlat = 49.95,
                                swlng = -5.32,
                                nelat = 50.10,
                                nelng = -5.00,
                                max_pages = 100,
                                verbose = TRUE) {
  
  # Work out the most recent update timestamp in the existing data
  latest_updated <- get_latest_updated_at(existing_obs_sf)
  
  if (verbose) {
    cat("Latest existing updated_at:", format(latest_updated, tz = "UTC"), "\n")
  }
  
  # iNat expects ISO-ish datetime string
  updated_since_str <- format(latest_updated, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # Pull records updated since that timestamp
  new_raw <- get_inat_obs_project_v3(
    project_slug = project_slug,
    taxon_id = taxon_id,
    updated_since = updated_since_str,
    swlat = swlat,
    swlng = swlng,
    nelat = nelat,
    nelng = nelng,
    max_pages = max_pages,
    verbose = verbose
  )
  
  if (nrow(new_raw) == 0) {
    if (verbose) cat("No updated records found.\n")
    return(existing_obs_sf)
  }
  
  if (verbose) cat("Downloaded", nrow(new_raw), "updated/new records.\n")
  
  # Process the new raw records using the same pipeline
  new_obs_sf <- process_inat_obs_to_sf(new_raw)
  
  # Combine with old data, then keep the most recent version of each observation
  combined <- bind_rows(existing_obs_sf, new_obs_sf) %>%
    mutate(updated_at_parsed = as.POSIXct(updated_at, tz = "UTC")) %>%
    arrange(id, desc(updated_at_parsed)) %>%
    distinct(id, .keep_all = TRUE) %>%
    select(-updated_at_parsed)
  
  #remove any records that are no longer research grade and return
  subset(combined, quality_grade == "research")
  
}


add_higher_taxo <- function(obs_sf, all_taxa_inat_info){
  
  family_lookup <- all_taxa_inat_info %>%
    filter(rank == "family") %>%
    select(
      taxon_id = id,
      family = name,
      family_common = preferred_common_name
    )
  
  order_lookup <- all_taxa_inat_info %>%
    filter(rank == "order") %>%
    select(
      taxon_id = id,
      order = name,
      order_common = preferred_common_name
    )
  
  class_lookup <- all_taxa_inat_info %>%
    filter(rank == "class") %>%
    select(
      taxon_id = id,
      class = name,
      class_common = preferred_common_name
    )
  
  library(tidyr)
  library(stringr)
  
  obs_long <- obs_sf %>%
    st_drop_geometry() %>%
    select(id, taxon.ancestry) %>%
    mutate(ancestry_list = strsplit(taxon.ancestry, "/")) %>%
    unnest_longer(ancestry_list) %>%
    mutate(ancestry_list = as.numeric(ancestry_list))
  
  obs_taxonomy <- obs_long %>%
    left_join(family_lookup, by = c("ancestry_list" = "taxon_id")) %>%
    left_join(order_lookup,  by = c("ancestry_list" = "taxon_id")) %>%
    left_join(class_lookup,  by = c("ancestry_list" = "taxon_id"))
  
  obs_taxonomy <- obs_taxonomy %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      family = dplyr::first(stats::na.omit(family)),
      family_common = dplyr::first(stats::na.omit(family_common)),
      order = dplyr::first(stats::na.omit(order)),
      order_common = dplyr::first(stats::na.omit(order_common)),
      class = dplyr::first(stats::na.omit(class)),
      class_common = dplyr::first(stats::na.omit(class_common)),
      .groups = "drop"
    )
  
  obs_sf <- obs_sf %>%
    left_join(obs_taxonomy, by = "id")
  
  return(obs_sf)
}

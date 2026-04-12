# ============================================================
# HELPER FUNCTIONS
# ============================================================


# ============================================================
# FUNCTION: get_inat_obs_project_v2
# PURPOSE: Download iNaturalist observations via API
# ============================================================

get_inat_obs_project_v2 <- function(project_slug = NULL,
                                    per_page = 200,
                                    max_pages = 20,
                                    quality_grade = NULL,
                                    place_id = NULL,
                                    taxon_id = NULL,
                                    d1 = NULL, d2 = NULL,
                                    verbose = TRUE,
                                    ...) {
  
  # Load required packages (inside function for portability)
  library(httr)      # API requests
  library(jsonlite)  # JSON parsing
  library(dplyr)     # data manipulation
  
  # Initialise list to store paginated results
  all_results <- list()
  
  # iNaturalist observations endpoint
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  # Loop through pages of results
  for (page in 1:max_pages) {
    
    # Optional progress message
    if (verbose) cat("Fetching page", page, "...\n")
    
    # Build query parameters
    query <- list(
      per_page = per_page,       # max records per page (up to 200)
      page = page,               # pagination index
      quality_grade = quality_grade,  # e.g. "research"
      place_id = place_id,       # optional iNat place filter
      taxon_id = taxon_id,       # taxonomic filter (e.g. angiosperms)
      d1 = d1,                   # start date (YYYY-MM-DD)
      d2 = d2,                   # end date (YYYY-MM-DD)
      ...                        # additional parameters (e.g. bbox, has=geo)
    )
    
    # Only include project filter if provided
    if (!is.null(project_slug)) {
      query$project_id <- project_slug
    }
    
    # Remove NULL values to avoid malformed API queries
    query <- query[!sapply(query, is.null)]
    
    # Send GET request to iNaturalist API
    res <- GET(url = base_url, query = query)
    
    # Check for failed request
    if (status_code(res) != 200) {
      warning("API request failed on page ", page)
      break
    }
    
    # Parse JSON response into R list/data frame
    content_json <- fromJSON(
      content(res, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    
    # Stop if no more results returned
    if (length(content_json$results) == 0) break
    
    # Store results for this page
    all_results[[page]] <- content_json$results
  }
  
  # Combine all pages into a single data frame
  df <- bind_rows(all_results)
  
  # Return full dataset
  return(df)
}


# ============================================================
# FUNCTION: bng_to_gridref
# PURPOSE: Convert BNG coordinates to UK grid reference
# ============================================================

bng_to_gridref <- function(easting, northing, digits = 4) {
  
  # ------------------------------------------------------------
  # Step 1: Identify 100 km grid square
  # ------------------------------------------------------------
  
  # Calculate 100 km grid indices
  e100k <- floor(easting / 100000)
  n100k <- floor(northing / 100000)
  
  # Lookup table for OS grid letters (GB only)
  letters <- c(
    "SV","SW","SX","SY","SZ","TV","TW",
    "SQ","SR","SS","ST","SU","TQ","TR",
    "SL","SM","SN","SO","SP","TL","TM",
    "SF","SG","SH","SJ","SK","TF","TG",
    "SA","SB","SC","SD","SE","TA","TB",
    "NV","NW","NX","NY","NZ","OV","OW",
    "NQ","NR","NS","NT","NU","OQ","OR",
    "NL","NM","NN","NO","NP","OL","OM",
    "NF","NG","NH","NJ","NK","OF","OG",
    "NA","NB","NC","ND","NE","OA","OB",
    "HV","HW","HX","HY","HZ","JV","JW"
  )
  
  # Convert grid indices into letter pair
  index <- n100k * 7 + e100k + 1
  grid_letters <- letters[index]
  
  
  # ------------------------------------------------------------
  # Step 2: Calculate position within 100 km square
  # ------------------------------------------------------------
  
  # Extract remainder coordinates within 100 km square
  e_remainder <- floor((easting %% 100000) / 10^(5 - digits/2))
  n_remainder <- floor((northing %% 100000) / 10^(5 - digits/2))
  
  
  # ------------------------------------------------------------
  # Step 3: Format grid reference string
  # ------------------------------------------------------------
  
  # Combine letters + numeric components
  # digits = 4 → 1 km resolution (2 digits each for easting/northing)
  
  sprintf(
    "%s%0*d%0*d",
    grid_letters,
    digits/2, e_remainder,
    digits/2, n_remainder
  )
}